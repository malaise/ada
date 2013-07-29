with Con_Io, Afpx, As.U, Str_Util;
with Afpx_Xref, Utils, Communication, Fleet;
package body Battle is

  -- A cell
  type Cell_State is record
    Used : Boolean := False;
    Shot : Boolean := False;
  end record;

  -- A grid
  type Grit_Matrix is array (Utils.Row_Range, Utils.Col_Range) of Cell_State;
  -- Default content of a grid
  Empty_Grid : constant Grit_Matrix := (others => (others => (False, False)));

  -- Local and opponent grids
  My_Grid, Op_Grid : Grit_Matrix;

  -- Colors
  Black, Blue, Red : Con_Io.Effective_Colors;

  -- Center text in a field
  procedure Center (Fld : in Afpx.Field_Range;
                    Str : in String) is
  begin
    Afpx.Clear_Field (Fld);
    Afpx.Encode_Field (Fld, (0, 0),
         Str_Util.Center (Str, Afpx.Get_Field_Width (Fld)));
  end Center;

  -- Decode a message as text
  function Decode (Msg : String) return String is
    Lmsg : constant String := Msg;
    Result : As.U.Asu_Us;
  begin
    if Utils.Debug_Play then
      Utils.Debug ("Decoding: >" & Msg & "<");
    end if;
    -- Row and Col
    Result := As.U.Tus (Lmsg(2));
    if Lmsg(3) = 'a' then
      Result.Append ("10");
    else
      Result.Append (Lmsg(3));
  end if;
    Result.Append (' ');
    case Lmsg(4) is
      when 'M' =>
        Result.Append ("missed.");
      when 'H' =>
        Result.Append ("hit.");
      when 'S' | 'E' =>
        Result.Append (Lmsg(5 .. Lmsg'Last) & " sunk.");
      when others =>
        raise Protocol_Error;
    end case;
    return Result.Image;
  end Decode;

  -- Check result of a shoot, return corresponding end of message
  --  result char (H, S or E) and ship name
  function Check_Shoot (Cell : Utils.Coord) return String is
    Shot : Boolean;
    Sunk : Boolean;
    Nb_Sunk : Natural;
    Result : As.U.Asu_Us;
    use type Utils.Coord, As.U.Asu_Us;
  begin
    Result := As.U.Tus ('H');
    Nb_Sunk := 0;
    -- Check each ship
    for S in Fleet.Ship_List loop
      -- Is this ship the one just shot
      Shot := False;
      -- Is this ship sunk
      Sunk := True;
      for C in 1 .. Fleet.Length (S) loop
        if Fleet.My_Ships(S)(C) = Cell then
          Shot := True;
          My_Grid(Cell.Row, Cell.Col).Shot := True;
        end if;
        if not My_Grid(Fleet.My_Ships(S)(C).Row,
                       Fleet.My_Ships(S)(C).Col).Shot then
          Sunk := False;
        end if;
      end loop;
      if Shot and then Sunk then
        -- This ship has just been sunk
        Result := As.U.Tus ('S') & Fleet.Ship_Names(S);
      end if;
      if Sunk then
        -- This ship is sunk
        Nb_Sunk := Nb_Sunk + 1;
      end if;
    end loop;
    if Nb_Sunk = Fleet.Ship_List'Pos(Fleet.Ship_List'Last)
               - Fleet.Ship_List'Pos(Fleet.Ship_List'First) + 1 then
      -- All ships are sunk
      Result.Replace_Element (1, 'E');
    end if;
    return Result.Image;
  end Check_Shoot;

  -- Show an opponent ship as sunk
  procedure Show_Sunk (Ref : Utils.Coord) is
    procedure Make_Red (C : Utils.Coord) is
    begin
      Afpx.Set_Field_Colors (Utils.Coord2Fld (Afpx_Xref.Play.Grid2, C),
                             Background => Red);
    end Make_Red;
    C : Utils.Coord;
  begin
    -- Mark in Red all adjacent cells as long as valid and used
    C := Ref;
    while Utils.In_Grid (C, -1, 0) loop
      C.Row := Utils.Row_Range'Pred (C.Row);
      exit when not Op_Grid (C.Row, C.Col).Used;
      Make_Red (C);
    end loop;
    C := Ref;
    while Utils.In_Grid (C, 1, 0) loop
      C.Row := Utils.Row_Range'Succ (C.Row);
      exit when not Op_Grid (C.Row, C.Col).Used;
      Make_Red (C);
    end loop;
    C := Ref;
    while Utils.In_Grid (C, 0, -1) loop
      C.Col := Utils.Col_Range'Pred (C.Col);
      exit when not Op_Grid (C.Row, C.Col).Used;
      Make_Red (C);
    end loop;
    C := Ref;
    while Utils.In_Grid (C, 0, 1) loop
      C.Col := Utils.Col_Range'Succ (C.Col);
      exit when not Op_Grid (C.Row, C.Col).Used;
      Make_Red (C);
    end loop;
  end Show_Sunk;

  -- Variables shared by Play and Receive
  Abort_Game : Boolean;
  Shoot : Boolean;
  Shot : Boolean;
  Done : Boolean;
  Reset : Boolean;

  -- Reception of a message from partner
  procedure Receive (Msg : in String) is
    C : Utils.Coord;
    Cell : Cell_State;
    Reply : As.U.Asu_Us;
    Message : constant String (1 .. Msg'Length) := Msg;
  begin
    if Utils.Debug_Play then
      Utils.Debug ("Handling message >" & Message & "<");
      if Shoot then
        Utils.Debug ("while shooting");
      else
        Utils.Debug ("while waiting");
      end if;
    end if;
    if Message = "E" then
      -- Aborted by partner
      if Utils.Debug_Play then
        Utils.Debug ("Received message: " & Message);
      end if;
      if Utils.Debug_Play then
        Utils.Debug ("Partner has aborted game");
      end if;
      Abort_Game := True;
      return;
    end if;

    if Message(1) = 'S' then
      -- Received a shoot request
      if Shoot or else Message'Length /= 3 then
        raise Protocol_Error;
      end if;
      -- Clear message of last reply received
      Afpx.Clear_Field (Afpx_Xref.Play.Op_Msg);
      -- Evaluate result
      begin
        C := Utils.Value (Message(2 .. 3));
      exception
        when others =>
          if Utils.Debug_Play then
            Utils.Debug ("Error decoding shoot request");
          end if;
          raise Protocol_Error;
      end;
      Cell := My_Grid(C.Row, C.Col);
      Reply := As.U.Tus ("R" & Message(2 .. 3));
      if Cell.Shot then
        -- Cell already shot
        Reply.Append ("M");
      else
        if Cell.Used then
          -- Hit
          Afpx.Set_Field_Colors (Utils.Coord2Fld (Afpx_Xref.Play.Grid1, C),
                                 Background => Red);
          Reply.Append (Check_Shoot (C));
        else
          -- Miss
          Afpx.Set_Field_Colors (Utils.Coord2Fld (Afpx_Xref.Play.Grid1, C),
                                 Background => Blue);
          Reply.Append ("M");
          My_Grid(C.Row, C.Col).Shot := True;
        end if;
      end if;
      -- Update message
      Center (Afpx_Xref.Play.My_Msg, Decode (Reply.Image));
      if Reply.Element (4) = 'E' then
        -- Lost
        Afpx.Set_Field_Activation (Afpx_Xref.Play.Loose, True);
        Done := True;
      end if;
      -- Reply
      Communication.Send (Reply.Image);
      Shoot := True;
      Shot := False;

    elsif Message(1) = 'R' then
      -- Received a shoot reply
      if not Shoot or else not Shot then
        raise Protocol_Error;
      end if;
      -- Evaluate result
      begin
        C := Utils.Value (Message(2 .. 3));
      exception
        when others =>
          if Utils.Debug_Play then
            Utils.Debug ("Error decoding shoot reply");
          end if;
          raise Protocol_Error;
      end;
      Cell := My_Grid(C.Row, C.Col);
      -- Show result if new shot
     if not Op_Grid (C.Row, C.Col).Shot then
       Op_Grid (C.Row, C.Col).Shot := True;
       if Message(4) = 'M' then
         -- Miss
         Afpx.Set_Field_Colors (Utils.Coord2Fld (Afpx_Xref.Play.Grid2, C),
                                 Background => Blue);
       elsif  Message(4) = 'H' then
         -- Hit
         Afpx.Set_Field_Colors (Utils.Coord2Fld (Afpx_Xref.Play.Grid2, C),
                                 Background => Black);
         Op_Grid (C.Row, C.Col).Used := True;
       else
         -- Sink
         Afpx.Set_Field_Colors (Utils.Coord2Fld (Afpx_Xref.Play.Grid2, C),
                                Background => Red);
         Op_Grid (C.Row, C.Col).Used := True;
         Show_Sunk (C);
       end if;
     end if;
      -- Update message
      Center (Afpx_Xref.Play.Op_Msg, Decode (Message));
      if Message(4) = 'E' then
        -- Lost
        Afpx.Set_Field_Activation (Afpx_Xref.Play.Win, True);
        Done := True;
      end if;
      Shoot := False;

    elsif Message(1) = 'Z' then
      -- Receive a reset
      if not Done then
        raise Protocol_Error;
      end if;
      Reset := True;
    else
      raise Protocol_Error;
    end if;
    -- Done
  end Receive;

  -- Return true as long as play a new game
  function Play (Server, Start : Boolean) return Boolean is
    Cursor_Field : Afpx.Field_Range;
    Cursor_Col   : Con_Io.Col_Range;
    Insert       : Boolean;
    Redisplay    : Boolean;
    Result       : Afpx.Result_Rec;
    Target : Utils.Coord;

    use type Afpx.Keyboard_Key_List, Afpx.Field_Range;
  begin
    if Utils.Debug_Play then
      Utils.Debug ("Start of play");
    end if;

    -- Init Afpx and colors
    Afpx.Use_Descriptor (Afpx_Xref.Play.Dscr_Num);
    Black := Con_Io.Color_Of ("Black");
    Blue := Con_Io.Color_Of ("Blue");
    Red := Con_Io.Color_Of ("Red");
    Afpx.Set_Field_Activation (Afpx_Xref.Play.Win, False);
    Afpx.Set_Field_Activation (Afpx_Xref.Play.Loose, False);

    -- Init for Afpx Ptg
    Cursor_Field := 1;
    Cursor_Col := 0;
    Insert := False;
    Redisplay := False;

    -- Init my grid and screen from Fleet
    My_Grid := Empty_Grid;
    Op_Grid := Empty_Grid;
    for Ship in Fleet.Ship_List loop
      for C in 1 .. Fleet.Length (Ship) loop
        My_Grid(Fleet.My_Ships(Ship)(C).Row,
                Fleet.My_Ships(Ship)(C).Col).Used := True;
        Afpx.Set_Field_Colors (
            Utils.Coord2Fld (Afpx_Xref.Play.Grid1, Fleet.My_Ships(Ship)(C)),
            Background => Black);
      end loop;
    end loop;

    -- Init reception callback
    Shoot := Start;
    Shot := False;
    Done := False;
    Reset := False;
    Abort_Game := False;
    Communication.Set_Callback (Receive'Access);

    loop
      -- Screen title and scales colors
      Afpx.Reset_Field (Afpx_Xref.Play.Scale1, Reset_String => False);
      Afpx.Reset_Field (Afpx_Xref.Play.Scale1 + 1, Reset_String => False);
      Afpx.Reset_Field (Afpx_Xref.Play.Scale2, Reset_String => False);
      Afpx.Reset_Field (Afpx_Xref.Play.Scale2 + 1, Reset_String => False);
      Afpx.Clear_Field (Afpx_Xref.Play.Title);
      if Done then
        Afpx.Encode_Field (Afpx_Xref.Play.Title, (0, 2), "End");
      elsif Shoot then
        Afpx.Reset_Field (Afpx_Xref.Play.Title, Reset_Colors => False);
        Afpx.Set_Field_Colors (Afpx_Xref.Play.Scale2, Foreground => Blue);
        Afpx.Set_Field_Colors (Afpx_Xref.Play.Scale2 + 1, Foreground => Blue);
      else
        Afpx.Encode_Field (Afpx_Xref.Play.Title, (0, 0), "Waiting");
        Afpx.Set_Field_Colors (Afpx_Xref.Play.Scale1, Foreground => Blue);
        Afpx.Set_Field_Colors (Afpx_Xref.Play.Scale1 + 1, Foreground => Blue);
      end if;

      -- Buttons at end of game
      Afpx.Set_Field_Activation (Afpx_Xref.Play.Restart,  Server and then Done);
      Afpx.Set_Field_Activation (Afpx_Xref.Play.Exitgame, Server and then Done);

      -- Get action
      Afpx.Put_Then_Get (Cursor_Field, Cursor_Col, Insert, Redisplay, Result);
      case Result.Event is
        when Afpx.Signal_Event =>
          -- Aborted by signal
          if Communication.Sig_Received then
            raise Utils.Abort_Game;
          end if;
        when Afpx.Keyboard =>
          if Result.Keyboard_Key = Afpx.Break_Key then
            -- Aborted by Ctrl C
            raise Utils.Abort_Game;
          end if;
        when Afpx.Fd_Event =>
          null;
        when Afpx.Mouse_Button =>
          -- Shoot or end of game choice
          if Result.Field_No = Afpx_Xref.Play.Exitgame then
            Communication.Send ("E");
            if Utils.Debug_Play then
              Utils.Debug ("End of play");
            end if;
            return False;
          elsif Result.Field_No = Afpx_Xref.Play.Restart then
            Communication.Send ("Z");
            if Utils.Debug_Play then
              Utils.Debug ("End of play");
            end if;
            return True;
          end if;
          -- Shoot?
          if Shoot
          and then not Shot
          and then not Done
          and then Result.Field_No in Afpx_Xref.Play.Grid2
                                   .. Afpx_Xref.Play.Grid2 + 99 then
            Target := Utils.Fld2Coord (Afpx_Xref.Play.Grid2, Result.Field_No);
            if Utils.Debug_Play then
              Utils.Debug ("Shooting at " & Utils.Image (Target));
            end if;
            -- Clear message of last reply sent
            Afpx.Clear_Field (Afpx_Xref.Play.My_Msg);
            -- Send shoot request
            Communication.Send ("S" & Utils.Image (Target));
            Shot := True;
          end if;
        when Afpx.Refresh =>
          Redisplay := True;
        when others =>
          -- Other event
          null;
      end case;
      -- Received a shoot or reply or end message
      if Abort_Game then
        if Done then
          if Utils.Debug_Play then
            Utils.Debug ("End of play");
          end if;
          return False;
        else
          raise Utils.Abort_Game;
        end if;
      end if;
      if Reset then
        -- Received a reset
        return True;
      end if;
    end loop;

  end Play;

end Battle;

