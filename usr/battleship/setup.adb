with Con_Io, Afpx;
with Afpx_Xref, Communication, Utils;
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
  Partner_Done : Boolean := False;
  Abort_Game : Boolean := False;
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
  type Action_List is (Idle, Setting, Positionning, Deleting);
  Action : Action_List;

  -- Which ships are set
  type Ship_List is (Carrier, Battleship, Cruiser, Sub1, Sub2);
  type Ship_State is array (Ship_List) of Boolean;
  No_Ship : constant Ship_State := (others => False);
  All_Ships : constant Ship_State := (others => True);
  Ships : Ship_State := No_Ship;

  -- Kind of ship currently being set
  subtype Curr_Ship_List is Ship_List range Carrier .. Sub1;
  Curr_Ship : Curr_Ship_List;

  -- Where are ships set and squares used
  subtype Ship_Len_Range is Positive range 1 .. 5;
  type Pos_List is array (Ship_Len_Range) of Utils.Coord;
  Ships_Pos : array (Ship_List) of Pos_List;
  Grid : array (Utils.Row_Range, Utils.Col_Range) of Boolean
       := (others => (others => False));

  -- Valid extremities
  subtype Valid_Number is Natural range 0 .. 4;
  subtype Valid_Range is Valid_Number range 1 .. Valid_Number'Last;
  Valid_Nb : Valid_Number;
  Valids : array (Valid_Range) of Utils.Coord;

  -- Handle user action during setup
  function Handle_Click (Fld: Afpx.Field_Range) return Boolean;

  -- Define Setup
  procedure Define is
    Cursor_Field : Afpx.Field_Range;
    Cursor_Col   : Con_Io.Col_Range;
    Insert       : Boolean;
    Result       : Afpx.Result_Rec;
    Redisplay    : Boolean;
    Completed    : Boolean;
    Ship_Fld     : Afpx.Absolute_Field_Range;
    use type Afpx.Keyboard_Key_List, Afpx.Field_Range;
  begin
    -- Reset Afpx descriptor
    Afpx.Use_Descriptor (Afpx_Xref.Setup.Dscr_Num);
    Afpx.Clear_Field (Afpx_Xref.Setup.Title);
    Afpx.Encode_Field (Afpx_Xref.Setup.Title, (0, 0), "Setup");

    -- Init reception of message from partner
    Communication.Set_Callback (Receive'Access);

    -- Init for Afpx Ptg
    Cursor_Field := 1;
    Cursor_Col := 0;
    Insert := False;
    Redisplay := False;

    -- Init for setup
    Completed := False;

    loop
      -- Cancel is active if setting or deleting
      Afpx.Set_Field_Activation (Afpx_Xref.Setup.Cancel, Action /= Idle);
      -- Delete is active only in idle if at least one ship is set
      Afpx.Set_Field_Activation (Afpx_Xref.Setup.Delete,
                                 Action = Idle and then Ships /= No_Ship);
      -- Done is Active in Idle if all ships are set
      Afpx.Set_Field_Activation (Afpx_Xref.Setup.Done,
                                 Action = Idle and then Ships = All_Ships);
      -- Activate ships
      Afpx.Set_Field_Activation (Afpx_Xref.Setup.Aircraftcarrier,
                                 Action = Idle and then not Ships(Carrier));
      Afpx.Set_Field_Activation (Afpx_Xref.Setup.Battleship,
                                 Action = Idle and then not Ships(Battleship));
      Afpx.Set_Field_Activation (Afpx_Xref.Setup.Cruiser,
                                 Action = Idle and then not Ships(Cruiser));
      Afpx.Set_Field_Activation (Afpx_Xref.Setup.Submarines,
                                 Action = Idle and then
                                 ( not Ships(Sub1) or else not Ships(Sub2))) ;
      if Action = Setting or else Action = Positionning then
         -- Activate and protect ship being set
         Ship_Fld := Afpx_Xref.Setup.Aircraftcarrier
            + (Afpx.Absolute_Field_Range (
                Ship_List'Pos(Curr_Ship) - Ship_List'Pos(Ship_List'First)) * 2);
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
          exit when Completed and then Partner_Done;
        when Afpx.Mouse_Button =>
          Completed := Handle_Click (Result.Field_No);
          exit when Completed and then Partner_Done;
        when Afpx.Refresh =>
          Redisplay := True;
        when others =>
          -- Other event
          null;
      end case;
    end loop;
  end Define;

  -- Length of a ship
  function Length (Ship : Curr_Ship_List) return Ship_Len_Range is
  begin
    case Ship is
      when Carrier => return 5;
      when Battleship => return 4;
      when Cruiser => return 3;
      when Sub1 => return 2;
    end case;
  end Length;

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
    Len := Length (Ship);
    case Dir is
      when Up =>
        -- From Start, Len rows up
        if Start.Row < Utils.Row_Range'Val(Len - 1) then
          return Start;
        end if;
        Erow := Utils.Row_Range'Val(Utils.Row_Range'Pos(Start.Row) - Len + 1);
        for Row in Erow .. Start.Row loop
          if Grid (Row, Start.Col) then
            return Start;
          end if;
        end loop;
        if Utils.Debug_Setup then
          Utils.Debug ("Up is valid");
        end if;
        return (Erow, Start.Col);
      when Down =>
        -- From Start, Len rows down
        if Start.Row > Utils.Row_Range'Val(
              Utils.Row_Range'Pos(Utils.Row_Range'Last) - Len + 1)
        then
          return Start;
        end if;
        Erow := Utils.Row_Range'Val(Utils.Row_Range'Pos(Start.Row) + Len - 1);
        for Row in Start.Row .. Erow loop
          if Grid (Row, Start.Col) then
            return Start;
          end if;
        end loop;
        if Utils.Debug_Setup then
          Utils.Debug ("Down is valid");
        end if;
        return (Erow, Start.Col);
      when Left =>
        -- From Start, Len cols left
        if Start.Col < Utils.Col_Range(Len) then
          return Start;
        end if;
        Ecol := Start.Col - Utils.Col_Range(Len - 1);
        for Col in Ecol .. Start.Col loop
          if Grid (Start.Row, Col) then
            return Start;
          end if;
        end loop;
        if Utils.Debug_Setup then
          Utils.Debug ("Left is valid");
        end if;
        return (Start.Row, Ecol);
      when Right =>
        -- From Start, Len cols right
        if Start.Col > Utils.Col_Range'Last - Utils.Col_Range(Len - 1) then
          return Start;
        end if;
        Ecol := Start.Col + Utils.Col_Range(Len - 1);
        for Col in Start.Col .. Ecol loop
          if Grid (Start.Row, Col) then
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
          Ships_Pos(Ship)(I) := (Start.Row, Col);
          Grid (Start.Row, Col) := True;
          I := I + 1;
        end loop;
      else
        for Col in Stop.Col .. Start.Col loop
          Ships_Pos(Ship)(I) := (Start.Row, Col);
          Grid (Start.Row, Col) := True;
          I := I + 1;
        end loop;
      end if;
    else
      if Start.Row < Stop.Row then
        for Row in Start.Row .. Stop.Row loop
          Ships_Pos(Ship)(I) := (Row, Start.Col);
          Grid (Row, Start.Col) := True;
          I := I + 1;
        end loop;
      else
        for Row in Stop.Row .. Start.Row loop
          Ships_Pos(Ship)(I) := (Row, Start.Col);
          Grid (Row, Start.Col) := True;
          I := I + 1;
        end loop;
      end if;
    end if;
  end Store_Ship;

  -- Handle user action during setup
  function Handle_Click (Fld : Afpx.Field_Range) return Boolean is
    use type Afpx.Field_Range, Utils.Coord;
    Start, Stop : Utils.Coord;
    Found : Boolean;
  begin
    case Fld is
      -- Set a ship
      when Afpx_Xref.Setup.Aircraftcarrier =>
        Action := Setting;
        Curr_Ship := Carrier;
      when Afpx_Xref.Setup.Battleship =>
        Action := Setting;
        Curr_Ship := Battleship;
      when Afpx_Xref.Setup.Cruiser =>
        Action := Setting;
        Curr_Ship := Cruiser;
      when Afpx_Xref.Setup.Submarines =>
        Action := Setting;
        Curr_Ship := Sub1;
      when Afpx_Xref.Setup.Grid .. Afpx_Xref.Setup.Grid + 99 =>
        case Action is
          when Idle =>
            null;
          when Setting =>
            -- Store Coordinate
            Start := Utils.Fld2Coord (Afpx_Xref.Setup.Grid, Fld);
            Ships_Pos(Curr_Ship)(1) := Start;
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
            -- Cancel if none
            if Valid_Nb = 0 then
              Action := Idle;
            else
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
            Start := Ships_Pos(Curr_Ship)(1);
            Stop := Utils.Fld2Coord (Afpx_Xref.Setup.Grid, Fld);
            -- Cancel proposed cells
            for I in 1 .. Valid_Nb loop
              if Valids(I) = Stop then
                 Found := True;
              end if;
              Afpx.Reset_Field (
                Utils.Coord2Fld (Afpx_Xref.Setup.Grid, Valids(I)),
                Reset_String => False);
            end loop;
            if Found then
              -- Store ship and update grid
              Store_Ship (Curr_Ship, Start, Stop);
              if Utils.Debug_Setup then
                for I in 1 .. Length (Curr_Ship) loop
                  Utils.Debug ("Ship is in "
                              & Utils.Image (Ships_Pos(Curr_Ship)(I)));
                end loop;
              end if;
              -- Update screen
              for I in 1 .. Length (Curr_Ship) loop
                Afpx.Set_Field_Colors (
                    Utils.Coord2Fld (Afpx_Xref.Setup.Grid,
                                     Ships_Pos(Curr_Ship)(I)),
                    Background => Con_Io.Color_Of ("Black"));
              end loop;
              -- Move Sub1 as Sub2 is first submarine
              if Curr_Ship = Sub1 and then not Ships(Sub2) then
                Ships_Pos(Sub2) := Ships_Pos(Sub1);
                Ships(Sub2) := True;
                Ships(Sub1) := False;
              end if;
              Action := Idle;
            end if;
          when Deleting =>
            -- Check there is a ship and delete it
            null;
        end case;
      when Afpx_Xref.Setup.Delete =>
        Action := Deleting;
      when Afpx_Xref.Setup.Cancel =>
        -- Cancel proposed cells
        if Action = Positionning then
          Afpx.Reset_Field (
            Utils.Coord2Fld (Afpx_Xref.Setup.Grid, Ships_Pos(Curr_Ship)(1)),
            Reset_String => False);
          for I in 1 .. Valid_Nb loop
            Afpx.Reset_Field (
              Utils.Coord2Fld (Afpx_Xref.Setup.Grid, Valids(I)),
              Reset_String => False);
          end loop;
        end if;
        Action := Idle;
      when Afpx_Xref.Setup.Done =>
        -- Store setup
        -- Done
        return True;
      when others =>
        null;
    end case;
    return False;
  end Handle_Click;

end Setup;

