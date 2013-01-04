with Con_Io, Afpx, As.U;
with Afpx_Xref, Communication, Utils, Fleet;
package body Setup is

  function Init (Addr : String; Server : Boolean) return Boolean is
    Cursor_Field : Afpx.Field_Range;
    Cursor_Col   : Con_Io.Col_Range;
    Insert       : Boolean;
    Result       : Afpx.Result_Rec;
    Redisplay    : Boolean;
    use type Afpx.Field_Range, Afpx.Keyboard_Key_List;
  begin
    -- Init Afpx: deactivate grid and actions
    Afpx.Use_Descriptor (Afpx_Xref.Setup.Dscr_Num);
    for Fld in Afpx_Xref.Setup.Start .. Afpx_Xref.Setup.Delete loop
      Afpx.Set_Field_Activation (Fld, False);
    end loop;
    Afpx.Set_Field_Activation (Afpx_Xref.Setup.Done, False);

    -- Initiate connection
    Communication.Connect (Addr, Server);

    -- Init for Afpx Ptg
    Cursor_Field := 1;
    Cursor_Col := 0;
    Insert := False;
    Redisplay := False;

    loop
      Afpx.Put_Then_Get (Cursor_Field, Cursor_Col, Insert, Result, Redisplay);
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
          -- Connected
          exit when Communication.Is_Connected;
        when Afpx.Mouse_Button =>
          if Result.Field_No = Afpx_Xref.Setup.Cancel then
            -- Cancelled by user
            return False;
          end if;
        when Afpx.Refresh =>
          Redisplay := True;
        when others =>
          -- Other event
          null;
      end case;
    end loop;
    return True;
  end Init;

  -- Reception of a message from partner
  Partner_Done : Boolean;
  Abort_Game : Boolean;
  procedure Receive (Msg : in String) is
  begin

    if Msg = "D" then
      -- Partner has completed its setup
      if Utils.Debug_Setup then
        Utils.Debug ("Partner has completed setup");
      end if;
      Partner_Done := True;
    elsif Msg = "E" then
      -- Aborted by partner
      if Utils.Debug_Setup then
        Utils.Debug ("Partner has aborted setup");
      end if;
      Abort_Game := True;
    end if;
  end Receive;

  -- Status during Setup (some are needed for field activation)
  -- Current action
  type Action_List is (Idle, Setting, Positionning, Deleting, Done);
  Action : Action_List;

  -- Which ships are set
  type Ship_State is array (Fleet.Ship_List) of Boolean;
  No_Ship : constant Ship_State := (others => False);
  All_Ships : constant Ship_State := (others => True);
  Ships : Ship_State := No_Ship;

  -- Kind of ship currently being set
  subtype Curr_Ship_List is Fleet.Ship_List range Fleet.Carrier .. Fleet.Sub1;
  Curr_Ship : Curr_Ship_List;

  -- Squares used
  Grid : array (Utils.Row_Range, Utils.Col_Range) of Boolean
       := (others => (others => False));

  -- Valid extremities
  subtype Valid_Number is Natural range 0 .. 4;
  subtype Valid_Range is Valid_Number range 1 .. Valid_Number'Last;
  Valid_Nb : Valid_Number;
  Valids : array (Valid_Range) of Utils.Coord;

  -- Handle user action during setup, return True when Done
  function Handle_Click (Fld: Afpx.Field_Range) return Boolean;

  -- Define Setup
  procedure Define is
    Cursor_Field : Afpx.Field_Range;
    Cursor_Col   : Con_Io.Col_Range;
    Insert       : Boolean;
    Result       : Afpx.Result_Rec;
    Redisplay    : Boolean;
    Ship_Fld     : Afpx.Absolute_Field_Range;
    use type Afpx.Keyboard_Key_List, Afpx.Field_Range;
  begin
    -- Reset Afpx descriptor
    Afpx.Use_Descriptor (Afpx_Xref.Setup.Dscr_Num);
    Afpx.Clear_Field (Afpx_Xref.Setup.Title);
    Afpx.Encode_Field (Afpx_Xref.Setup.Title, (0, 3), "Setup");

    -- Init reception of message from partner
    Partner_Done := False;
    Abort_Game := False;
    Communication.Set_Callback (Receive'Access);

    -- Init for Afpx Ptg
    Cursor_Field := 1;
    Cursor_Col := 0;
    Insert := False;
    Redisplay := False;

    -- Init ship names
    Curr_Ship := Fleet.Ship_List'First;
    for I in Afpx_Xref.Setup.Aircraftcarrier
              .. Afpx_Xref.Setup.Submarines + 1 loop
      if Afpx.Is_Put_Kind (I) then
        Fleet.Ship_Names(Curr_Ship) := As.U.Tus (Afpx.Decode_Field (I, 0));
      end if;
      Curr_Ship := Fleet.Ship_List'Succ (Curr_Ship);
    end loop;
    -- Copy to second submarine
    Fleet.Ship_Names(Curr_Ship) :=
          Fleet.Ship_Names(Fleet.Ship_List'Pred (Curr_Ship));

    -- Init for setup
    Action := Idle;

    loop
      -- Cancel is active if setting or deleting
      Afpx.Set_Field_Activation (Afpx_Xref.Setup.Cancel,
                                 Action /= Idle and then Action /= Done);
      -- Delete is active only in idle if at least one ship is set
      Afpx.Set_Field_Activation (Afpx_Xref.Setup.Delete,
                                 Action = Idle and then Ships /= No_Ship);
      -- Done is Active in Idle if all ships are set
      Afpx.Set_Field_Activation (Afpx_Xref.Setup.Done,
                                 Action = Idle and then Ships = All_Ships);
      -- Activate ships
      Afpx.Set_Field_Activation (Afpx_Xref.Setup.Aircraftcarrier,
                                 Action = Idle
                                 and then not Ships(Fleet.Carrier));
      Afpx.Set_Field_Activation (Afpx_Xref.Setup.Battleship,
                                 Action = Idle
                                 and then not Ships(Fleet.Battleship));
      Afpx.Set_Field_Activation (Afpx_Xref.Setup.Cruiser,
                                 Action = Idle
                                 and then not Ships(Fleet.Cruiser));
      Afpx.Set_Field_Activation (Afpx_Xref.Setup.Submarines,
                                 Action = Idle and then
                                 ( not Ships(Fleet.Sub1)
                                   or else not Ships(Fleet.Sub2))) ;
      if Action = Setting or else Action = Positionning then
         -- Activate and protect ship being set
         Ship_Fld := Afpx_Xref.Setup.Aircraftcarrier
            + (Afpx.Absolute_Field_Range (
                Fleet.Ship_List'Pos(Curr_Ship)
              - Fleet.Ship_List'Pos(Fleet.Ship_List'First)) * 2);
         Afpx.Set_Field_Activation (Ship_Fld, True);
         Afpx.Set_Field_Protection (Ship_Fld, True);
      elsif Action = Idle then
        -- Unprotect ships
        for I in Afpx_Xref.Setup.Aircraftcarrier
              .. Afpx_Xref.Setup.Submarines loop
          if Afpx.Is_Button_Kind (I) then
            Afpx.Set_Field_Protection (I, False);
          end if;
        end loop;
      end if;

      Afpx.Put_Then_Get (Cursor_Field, Cursor_Col, Insert, Result, Redisplay);
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
          -- Receive a completion or end message
          if Abort_Game then
            raise Utils.Abort_Game;
          end if;
          exit when Action = Done and then Partner_Done;
        when Afpx.Mouse_Button =>
          if Handle_Click (Result.Field_No) then
            -- Setup is just done locally
            Communication.Send ("D");
            exit when Action = Done and then Partner_Done;
            -- Waiting for partner completion of setup
            Afpx.Encode_Field (Afpx_Xref.Setup.Title, (0, 2), "Waiting");
          end if;
        when Afpx.Refresh =>
          Redisplay := True;
        when others =>
          -- Other event
          null;
      end case;
    end loop;
  end Define;

  -- Is a cell allowed: no ship in Cell nor in in adjacent cells
  --  but touching corner is allowed
  function Allowed (R : Utils.Row_Range; C : Utils.Col_Range) return Boolean is
  begin
    if Grid (R, C) then
      -- Cell is already used
      return False;
    end if;
    if Utils.In_Grid ((R, C), -1, 0)
    and then Grid (Utils.Row_Range'Pred(R), C) then
      -- One ship above
      return False;
    end if;
    if Utils.In_Grid ((R, C), 1, 0)
    and then Grid (Utils.Row_Range'Succ(R), C) then
      -- One ship below
      return False;
    end if;
    if Utils.In_Grid ((R, C), 0, -1)
    and then Grid (R, Utils.Col_Range'Pred(C)) then
      -- One ship on left
      return False;
    end if;
    if Utils.In_Grid ((R, C), 0, 1)
    and then Grid (R, Utils.Col_Range'Succ(C)) then
      -- One ship on right
      return False;
    end if;
    return True;
  end Allowed;

  -- Check if a direction is valid for a ship
  type Dir_List is (Up, Down, Left, Right);
  function Get_Valid (Ship : Curr_Ship_List;
                      Start : Utils.Coord;
                      Dir : Dir_List) return Utils.Coord is
    use type Utils.Row_Range, Utils.Col_Range;
    Erow : Utils.Row_Range;
    Ecol : Utils.Col_Range;
    Len : Positive;
  begin
    -- Length of ship
    Len := Fleet.Length (Ship) - 1;
    case Dir is
      when Up =>
        -- From Start, Len rows up
        if not Utils.In_Grid (Start, -Len, 0) then
          return Start;
        end if;
        Erow := Utils.Row_Range'Val(Utils.Row_Range'Pos(Start.Row) - Len);
        for Row in Erow .. Start.Row loop
          if not Allowed (Row, Start.Col) then
            return Start;
          end if;
        end loop;
        if Utils.Debug_Setup then
          Utils.Debug ("Up is valid");
        end if;
        return (Erow, Start.Col);
      when Down =>
        -- From Start, Len rows down
        if not Utils.In_Grid (Start, Len, 0) then
          return Start;
        end if;
        Erow := Utils.Row_Range'Val(Utils.Row_Range'Pos(Start.Row) + Len);
        for Row in Start.Row .. Erow loop
          if not Allowed (Row, Start.Col) then
            return Start;
          end if;
        end loop;
        if Utils.Debug_Setup then
          Utils.Debug ("Down is valid");
        end if;
        return (Erow, Start.Col);
      when Left =>
        -- From Start, Len cols left
        if not Utils.In_Grid (Start, 0, -Len) then
          return Start;
        end if;
        Ecol := Start.Col - Utils.Col_Range(Len);
        for Col in Ecol .. Start.Col loop
          if not Allowed (Start.Row, Col) then
            return Start;
          end if;
        end loop;
        if Utils.Debug_Setup then
          Utils.Debug ("Left is valid");
        end if;
        return (Start.Row, Ecol);
      when Right =>
        -- From Start, Len cols right
        if not Utils.In_Grid (Start, 0, Len) then
          return Start;
        end if;
        Ecol := Start.Col + Utils.Col_Range(Len);
        for Col in Start.Col .. Ecol loop
          if not Allowed (Start.Row, Col) then
            return Start;
          end if;
        end loop;
        if Utils.Debug_Setup then
          Utils.Debug ("Right is valid");
        end if;
        return (Start.Row, Ecol);
    end case;
  end Get_Valid;

  -- Store Ship position
  procedure Store_Ship (Ship : Curr_Ship_List; Start, Stop : in Utils.Coord) is
    I : Positive;
    use type Utils.Row_Range, Utils.Col_Range;
  begin
    Ships(Ship) := True;
    I := 1;
    if Start.Row = Stop.Row then
      if Start.Col < Stop.Col then
        for Col in Start.Col .. Stop.Col loop
          Fleet.My_Ships(Ship)(I) := (Start.Row, Col);
          Grid (Start.Row, Col) := True;
          I := I + 1;
        end loop;
      else
        for Col in Stop.Col .. Start.Col loop
          Fleet.My_Ships(Ship)(I) := (Start.Row, Col);
          Grid (Start.Row, Col) := True;
          I := I + 1;
        end loop;
      end if;
    else
      if Start.Row < Stop.Row then
        for Row in Start.Row .. Stop.Row loop
          Fleet.My_Ships(Ship)(I) := (Row, Start.Col);
          Grid (Row, Start.Col) := True;
          I := I + 1;
        end loop;
      else
        for Row in Stop.Row .. Start.Row loop
          Fleet.My_Ships(Ship)(I) := (Row, Start.Col);
          Grid (Row, Start.Col) := True;
          I := I + 1;
        end loop;
      end if;
    end if;
  end Store_Ship;

  -- Handle user action during setup
  function Handle_Click (Fld : Afpx.Field_Range) return Boolean is
    Start, Stop : Utils.Coord;
    Found : Boolean;
    Del_Ship : Fleet.Ship_List;
    use type Afpx.Field_Range, Utils.Coord, Fleet.Ship_List;
  begin
    case Fld is
      -- Set a ship
      when Afpx_Xref.Setup.Aircraftcarrier =>
        Action := Setting;
        Curr_Ship := Fleet.Carrier;
      when Afpx_Xref.Setup.Battleship =>
        Action := Setting;
        Curr_Ship := Fleet.Battleship;
      when Afpx_Xref.Setup.Cruiser =>
        Action := Setting;
        Curr_Ship := Fleet.Cruiser;
      when Afpx_Xref.Setup.Submarines =>
        Action := Setting;
        Curr_Ship := Fleet.Sub1;
      when Afpx_Xref.Setup.Grid .. Afpx_Xref.Setup.Grid + 99 =>
        case Action is
          when Idle | Done =>
            null;
          when Setting =>
            -- Store Coordinate
            Start := Utils.Fld2Coord (Afpx_Xref.Setup.Grid, Fld);
            Fleet.My_Ships(Curr_Ship)(1) := Start;
            if Utils.Debug_Setup then
              Utils.Debug ("Selected Cell is " & Utils.Image (Start));
            end if;
            -- Propose valid extremities
            Valid_Nb := 0;
            for Dir in Dir_List loop
              Stop := Get_Valid (Curr_Ship, Start, Dir);
              if Stop /= Start then
                if Utils.Debug_Setup then
                  Utils.Debug ("Got " & Utils.Image (Stop));
                end if;
                Valid_Nb := Valid_Nb + 1;
                Valids(Valid_Nb) := Stop;
              end if;
            end loop;
            if Valid_Nb /= 0 then
              Afpx.Set_Field_Colors (
                Utils.Coord2Fld (Afpx_Xref.Setup.Grid, Start),
                Background => Con_Io.Color_Of ("Black"));
              for I in 1 .. Valid_Nb loop
                Afpx.Set_Field_Colors (
                  Utils.Coord2Fld (Afpx_Xref.Setup.Grid, Valids(I)),
                  Background => Con_Io.Color_Of ("Lime_Green"));
              end loop;
              Action := Positionning;
            end if;
          when Positionning =>
            -- Check if this is a valid extremity
            Found := False;
            Start := Fleet.My_Ships(Curr_Ship)(1);
            Stop := Utils.Fld2Coord (Afpx_Xref.Setup.Grid, Fld);
            for I in 1 .. Valid_Nb loop
              if Valids(I) = Stop then
                Found := True;
              end if;
            end loop;
            if Found then
              -- Cancel proposed cells
              for I in 1 .. Valid_Nb loop
                Afpx.Reset_Field (
                  Utils.Coord2Fld (Afpx_Xref.Setup.Grid, Valids(I)),
                  Reset_String => False);
              end loop;
              -- Store ship and update grid
              Store_Ship (Curr_Ship, Start, Stop);
              if Utils.Debug_Setup then
                for I in 1 .. Fleet.Length (Curr_Ship) loop
                  Utils.Debug ("Ship is in "
                              & Utils.Image (Fleet.My_Ships(Curr_Ship)(I)));
                end loop;
              end if;
              -- Update screen
              for I in 1 .. Fleet.Length (Curr_Ship) loop
                Afpx.Set_Field_Colors (
                    Utils.Coord2Fld (Afpx_Xref.Setup.Grid,
                                     Fleet.My_Ships(Curr_Ship)(I)),
                    Background => Con_Io.Color_Of ("Black"));
              end loop;
              -- Move Sub1 as Sub2 if first submarine
              if Curr_Ship = Fleet.Sub1 and then not Ships(Fleet.Sub2) then
                Fleet.My_Ships(Fleet.Sub2) := Fleet.My_Ships(Fleet.Sub1);
                Ships(Fleet.Sub2) := True;
                Ships(Fleet.Sub1) := False;
              end if;
              Action := Idle;
            end if;
          when Deleting =>
            -- Check there is a ship and delete it
            Start := Utils.Fld2Coord (Afpx_Xref.Setup.Grid, Fld);
            if Grid(Start.Row, Start.Col) then
              -- There is a ship here, find it
              Search:
              for S in Fleet.Ship_List loop
                if Ships(S) then
                  for I in 1 .. Fleet.Length (S) loop
                    if Fleet.My_Ships(S)(I) = Start then
                      -- Found it
                      Del_Ship := S;
                      exit Search;
                    end if;
                  end loop;
                end if;
              end loop Search;
              -- Delete this ship
              for I in 1 .. Fleet.Length (Del_Ship) loop
                Grid(Fleet.My_Ships(Del_Ship)(I).Row,
                     Fleet.My_Ships(Del_Ship)(I).Col) := False;
                Afpx.Reset_Field (
                    Utils.Coord2Fld (Afpx_Xref.Setup.Grid,
                                     Fleet.My_Ships(Del_Ship)(I)),
                    Reset_String => False);
                if Utils.Debug_Setup then
                  Utils.Debug ("Deleting ship in "
                              & Utils.Image (Fleet.My_Ships(Del_Ship)(I)));
                end if;
              end loop;
              Ships(Del_Ship) := False;
              -- Move Sub1 as Sub2 if first submarine
              if Del_Ship = Fleet.Sub2 and then Ships(Fleet.Sub1) then
                Fleet.My_Ships(Fleet.Sub2) := Fleet.My_Ships(Fleet.Sub1);
                Ships(Fleet.Sub2) := True;
                Ships(Fleet.Sub1) := False;
              end if;
            end if;
            Action := Idle;
        end case;
      when Afpx_Xref.Setup.Delete =>
        Action := Deleting;
      when Afpx_Xref.Setup.Cancel =>
        -- Cancel proposed cells
        if Action = Positionning then
          if Utils.Debug_Setup then
            Utils.Debug ("Cancelling "
                       & Utils.Image (Fleet.My_Ships(Curr_Ship)(1)));
          end if;
          Afpx.Reset_Field (
            Utils.Coord2Fld (Afpx_Xref.Setup.Grid,
                             Fleet.My_Ships(Curr_Ship)(1)),
            Reset_String => False);
          for I in 1 .. Valid_Nb loop
            if Utils.Debug_Setup then
              Utils.Debug ("Cancelling " & Utils.Image (Valids(I)));
            end if;
            Afpx.Reset_Field (
              Utils.Coord2Fld (Afpx_Xref.Setup.Grid, Valids(I)),
              Reset_String => False);
          end loop;
        end if;
        Action := Idle;
      when Afpx_Xref.Setup.Done =>
        -- Store setup
        -- Done
        Action := Done;
        return True;
      when others =>
        null;
    end case;
    return False;
  end Handle_Click;

end Setup;

