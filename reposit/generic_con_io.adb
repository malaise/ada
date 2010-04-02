with Ada.Calendar, Ada.Characters.Latin_1;
with Argument, Dyn_Data, Environ, Language, Lower_Str, Event_Mng, String_Mng;
package body Generic_Con_Io is

  X_Init_Done : Boolean := False;

  The_Color_Names : Colors_Definition := Default_Colors;

  Lf : Character renames Ada.Characters.Latin_1.Lf;
  Lfs : constant String := Lf & "";

  -- Can be called to initialise con_ios
  -- If not called, this init will be called together with first con_io
  --  initialisation and with default colors
  procedure Initialise is
    X_Colors : X_Mng.Color_Definition;
    Xi : X_Mng.Color;
  begin
    if not X_Init_Done then
      for I in The_Color_Names'Range loop
         Xi := Colors'Pos(I) - Colors'Pos(Color01);
         X_Colors(Xi) := The_Color_Names(I);
         -- Lower case and '_' -> ' '
         X_Colors(Xi) := Asu_Tus (Lower_Str (Asu_Ts (X_Colors(Xi))));
         X_Colors(Xi) := Asu_Tus (String_Mng.Replace (Asu_Ts (X_Colors(Xi)),
                                                       "_", " "));
      end loop;
      X_Mng.X_Initialise ("", X_Colors);
      -- Because we handle Ctrl-C in (Get_Key_Time) we shall handle signals
      Event_Mng.Activate_Signal_Handling;
      X_Init_Done := True;
    end if;
  end Initialise;

  procedure Set_Colors (Color_Names : in Colors_Definition) is
  begin
    if X_Init_Done then
      raise Already_Init;
    end if;
    The_Color_Names := Color_Names;
  end Set_Colors;

  function Color_Of (Name : String) return Effective_Colors is
    use type Asu_Us;
  begin
    for I in The_Color_Names'Range loop
      if Asu_Ts (The_Color_Names(I)) = Name then
        return I;
      end if;
    end loop;
    raise Unknown_Color;
  end Color_Of;

  function Color_Name_Of (Color : Effective_Colors) return String is
  begin
    return Asu_Ts (The_Color_Names(Color));
  end Color_Name_Of;

  package body One_Con_Io is

    Font_Env_Name : constant String := "CON_IO_FONT";
    Font_Env_Small : constant String := "small";
    Font_Env_Large : constant String := "large";

    Id : X_Mng.Line;
    Line_Def : constant X_Mng.Line_Definition_Rec := (
      Screen_Id => 0,
      Row => 1,
      Column => 1,
      Height => Row_Range_Last - Row_Range_First + 1,
      Width => Col_Range_Last - Col_Range_First + 1,
      Background => 0,
      Border => 0,
      No_Font => Font_No);

    -- Discard or Tid_xxx
    Mouse_Discard : constant X_Mng.Event_Kind := X_Mng.Keyboard;
    Mouse_Status : X_Mng.Event_Kind := X_Mng.No_Event;
    Motion_Enabling : Boolean := False;

    Line_Foreground : Effective_Colors := Default_Foreground;
    Line_Background : Effective_Colors := Default_Background;
    Line_Blink_Stat : Effective_Blink_Stats := Default_Blink_Stat;
    Line_Xor_Mode   : Effective_Xor_Modes := Default_Xor_Mode;

    X_Max : Graphics.X_Range;
    Y_Max : Graphics.Y_Range;
    Font_Width  : Natural;
    Font_Height : Natural;
    Font_Offset : Natural;

    -- Square Convertion
    function Full2Con (Position : Full_Square) return Square is
    begin
      return (Position.Row, Position.Col);
    end Full2Con;
    function Con2Full (Position : Square) return Full_Square is
    begin
      return (Position.Row, Position.Col);
    end Con2Full;

    function Color_Of (Name : String) return Effective_Colors is
    begin
      return Effective_Colors(Generic_Con_Io.Color_Of(Name));
    end Color_Of;
    function Color_Name_Of (Color : Effective_Colors) return String is
    begin
      return Generic_Con_Io.Color_Name_Of (
                  Generic_Con_Io.Effective_Colors(Color));
    end Color_Name_Of;

    -- Dynamic alloc/desaloc of windows
    package Dyn_Win is new Dyn_Data(Window_Data, Window);

    procedure Set_Attributes (Foreground : in Effective_Colors;
                              Blink_Stat : in Effective_Blink_Stats;
                              Background : in Effective_Colors;
                              Xor_Mode   : in Effective_Xor_Modes;
                              Forced     : in Boolean := False);

    Init_Done : Boolean := False;
    procedure Init is
      Line : X_Mng.Line_Definition_Rec := Line_Def;
      Env_Str : String (1 .. Font_Env_Small'Length);
      Env_Len : Natural;
    begin
      if Init_Done then
        return;
      end if;
      Initialise;
      Env_Str := (others => '-');
      Env_Len := Env_Str'Length;
      Environ.Get_Str (Font_Env_Name, Env_Str, Env_Len);
      if Lower_Str (Env_Str (1 .. Env_Len)) = Font_Env_Small
      and then Font_No /= Font_No_Range'First then
        Line.No_Font := Font_No - 1;
      elsif Lower_Str (Env_Str (1 .. Env_Len)) = Font_Env_Large
      and then Font_No /= Font_No_Range'Last then
        Line.No_Font := Font_No + 1;
      end if;
      X_Mng.X_Open_Line (Line, Id);
      X_Mng.X_Set_Line_Name (Id, Argument.Get_Program_Name);
      Mouse_Status := Mouse_Discard;
      Motion_Enabling := False;
      Init_Done := True;
      X_Mng.X_Get_Graphic_Characteristics(Id, X_Max, Y_Max,
            Font_Width, Font_Height, Font_Offset);
      -- Max is width - 1 so that range is 0 .. max
      X_Max := X_Max - 1;
      Y_Max := Y_Max - 1;
      Set_Attributes (Default_Foreground, Default_Blink_Stat, Default_Background,
                      Default_Xor_Mode, Forced => True);
      Flush;
    exception
      when others =>
        raise Init_Failure;
    end Init;

    procedure Destroy is
    begin
      if not Init_Done then
        raise Not_Init;
      end if;
      X_Mng.X_Close_Line(Id);
      Init_Done := False;
    end Destroy;

    -- Suspend and resume con_io
    procedure Suspend is
    begin
      -- Clear window and suspend
      Reset_Term;
      X_Mng.X_Suspend (Id);
    end Suspend;

    procedure Resume is
    begin
      if not Init_Done then
        raise Not_Init;
      end if;
      -- Resume
      X_Mng.X_Resume (Id);
      -- Force reset of attributes
      Line_Blink_Stat := Default_Blink_Stat;
    end Resume;


    -- Screen characteristics
    function Screen return Window is
    begin
      return Screen_Window;
    end Screen;

    -- Reset screen, windows and keyboard
    procedure Reset_Term is
    begin
      if not Init_Done then
        raise Not_Init;
      end if;
      X_Mng.X_Clear_Line (Id);
      -- Set current attributes in cache
      Set_Attributes (Default_Foreground, Default_Blink_Stat, Default_Background,
                      Default_Xor_Mode, Forced => True);
    end Reset_Term;

    -- Flushes X
    procedure Flush is
    begin
      if not Init_Done then
        raise Not_Init;
      end if;
      X_Mng.X_Flush (Id);
    end Flush;

    -- Set / get colors
    procedure Set_Foreground (Foreground : in Colors      := Current;
                              Blink_Stat : in Blink_Stats := Current;
                              Name       : in Window      := Screen) is
    begin
      if Name = null then
        raise Window_Not_Open;
      end if;
      if Foreground /= Current then
        Name.Current_Foreground := Foreground;
      end if;
      if Blink_Stat /= Current then
        Name.Current_Blink_Stat := Blink_Stat;
      end if;
    end Set_Foreground;

    procedure Set_Background (Background : in Colors := Current;
                              Name       : in Window       := Screen) is
    begin
      if Name = null then
        raise Window_Not_Open;
      end if;
      if Background /= Current then
        Name.Current_Background := Background;
      end if;
    end Set_Background;

    function Get_Foreground (Name : Window := Screen)
      return Effective_Colors is
    begin
      if Name = null then
        raise Window_Not_Open;
      end if;
      return Name.Current_Foreground;
    end Get_Foreground;

    function Get_Background (Name : Window := Screen)
      return Effective_Colors is
    begin
      if Name = null then
        raise Window_Not_Open;
      end if;
      return Name.Current_Background;
    end Get_Background;

    function Get_Blink_Stat (Name : Window := Screen)
      return Effective_Blink_Stats is
    begin
      if Name = null then
        raise Window_Not_Open;
      end if;
      return Name.Current_Blink_Stat;
    end Get_Blink_Stat;

    procedure Set_Xor_Mode(Xor_Mode : in Xor_Modes := Current;
                           Name : in Window := Screen) is
    begin
      if Name = null then
        raise Window_Not_Open;
      end if;
      if Xor_Mode /= Current then
        Name.Current_Xor_Mode := Xor_Mode;
      end if;
    end Set_Xor_Mode;

    function Get_Xor_Mode(Name : Window := Screen) return Effective_Xor_Modes is
    begin
      if Name = null then
        raise Window_Not_Open;
      end if;
      return Name.Current_Xor_Mode;
    end Get_Xor_Mode;


    -- Get Upper_Left / Lower_Right absolute coordinates of a window
    function Get_Absolute_Upper_Left (Name : Window) return Square is
    begin
      if Name = null then
        raise Window_Not_Open;
      end if;
      return Name.Upper_Left;
    end Get_Absolute_Upper_Left;

    function Get_Absolute_Lower_Right (Name : Window) return Square is
    begin
      if Name = null then
        raise Window_Not_Open;
      end if;
      return Name.Lower_Right;
    end Get_Absolute_Lower_Right;

    -- Get Lower_Right relative coordinates of a window (Upper_Left is (0, 0)).
    function Get_Relative_Lower_Right (Name : Window) return Square is
    begin
      if Name = null then
        raise Window_Not_Open;
      end if;
      return (Name.Lower_Right.Row - Name.Upper_Left.Row,
              Name.Lower_Right.Col - Name.Upper_Left.Col);
    end Get_Relative_Lower_Right;

    -- Open a window
    procedure Open (Name                    : in out Window;
                    Upper_Left, Lower_Right : in Square) is
    begin
      if Name /= null then
        raise Window_Already_Open;
      end if;
      if Upper_Left.Row > Lower_Right.Row or else
         Upper_Left.Col > Lower_Right.Col then
        raise Invalid_Square;
      end if;
      begin
        Name := Dyn_Win.Allocate (Screen_Data);
      exception
        when Storage_Error =>
          raise Open_Failure;
      end;
      Name.Upper_Left := Upper_Left;
      Name.Lower_Right := Lower_Right;
      Name.Current_Pos := Home;
      Name.Current_Foreground := Default_Foreground;
      Name.Current_Background := Default_Background;
      Name.Current_Blink_Stat := Default_Blink_Stat;
      Name.Current_Xor_Mode   := Default_Xor_Mode;
    exception
      when Constraint_Error =>
        raise Invalid_Square;
    end Open;

    function Is_Open (Name : Window) return Boolean is
    begin
      if not Init_Done then
        raise Not_Init;
      end if;
      return Name /= null;
    end Is_Open;

    -- True if the absolute square (relative to screen) is in the window.
    -- False otherwise
    function In_Window (Absolute_Square : Square;
                        Name            : Window) return Boolean is
    begin
      if Name = null then
        raise Window_Not_Open;
      end if;
      return   Absolute_Square.Row >= Name.Upper_Left.Row
      and then Absolute_Square.Row <= Name.Lower_Right.Row
      and then Absolute_Square.Col >= Name.Upper_Left.Col
      and then Absolute_Square.Col <= Name.Lower_Right.Col;
    end In_Window;

    -- Returns the relative square (relative to window), being the same
    --  physical position as the absolute square (relative to screen).
    -- May raise Invalid_Square if the absolute position is not in window.
    function To_Relative (Absolute_Square : Square;
                          Name            : Window) return Square is
    begin
      if not In_Window(Absolute_Square, Name) then
        raise Invalid_Square;
      end if;
      return (Row => Absolute_Square.Row - Name.Upper_Left.Row,
              Col => Absolute_Square.Col - Name.Upper_Left.Col);
    end To_Relative;

    -- Returns the absolute square (in screen) corresponding to the relative
    --  square in the window
    -- May raise Invalid_Square if the relative square is not in window
    function To_Absolute (Relative_Square : Square;
                          Name            : Window) return Square is
    begin
      if Name = null then
        raise Window_Not_Open;
      end if;
      if (Relative_Square.Row > Name.Lower_Right.Row-Name.Upper_Left.Row) or else
         (Relative_Square.Col > Name.Lower_Right.Col-Name.Upper_Left.Col) then
        raise Invalid_Square;
      end if;
      return (Row => Relative_Square.Row + Name.Upper_Left.Row, Col =>
        Relative_Square.Col + Name.Upper_Left.Col);
    end To_Absolute;

    procedure Set_Attributes (Foreground : in Effective_Colors;
                              Blink_Stat : in Effective_Blink_Stats;
                              Background : in Effective_Colors;
                              Xor_Mode   : in Effective_Xor_Modes;
                              Forced     : in Boolean := False) is
    begin
      -- Reset can be forced explicitely by the Forced argument
      --  or by resetting Line_Blink_Stat to Default
      if Forced or else Line_Blink_Stat = Default_Blink_Stat
                or else Foreground /= Line_Foreground
                or else Blink_Stat /= Line_Blink_Stat
                or else Background /= Line_Background then
        X_Mng.X_Set_Attributes (Id, Colors'Pos(Background) - 1,
                                    Colors'Pos(Foreground) - 1,
                                    Blink => Blink_Stat = Blink,
                                    Superbright => True);
        Line_Foreground := Foreground;
        Line_Blink_Stat := Blink_Stat;
        Line_Background := Background;
      end if;
      if Forced or else Xor_Mode /= Line_Xor_Mode then
        X_Mng.X_Set_Xor_Mode (Id, Xor_Mode = Xor_On);
        Line_Xor_Mode := Xor_Mode;
      end if;
    end Set_Attributes;

    -- Make window re-usable (have to re_open it)
    procedure Close (Name : in out Window) is
    begin
      if Name = null then
        raise Window_Not_Open;
      end if;
      Dyn_Win.Free(Name);
    end Close;

    -- Move cursor for use with put or get
    procedure Move (Position : in Square := Home;
                    Name     : in Window := Screen) is
    begin
      Move(Position.Row, Position.Col, Name);
    end Move;

    procedure Clear (Name : in Window := Screen) is
    begin
      if Name = null then
        raise Window_Not_Open;
      end if;
      -- Upper left and lower right, set foreground as our background
      Set_Attributes (Name.Current_Background,
                      Not_Blink,
                      Name.Current_Background, Xor_Off);
      X_Mng.X_Draw_Area (Id, Name.Lower_Right.Col - Name.Upper_Left.Col + 1,
                             Name.Lower_Right.Row - Name.Upper_Left.Row + 1,
                             Name.Upper_Left.Row, Name.Upper_Left.Col);
      Set_Attributes (Name.Current_Foreground,
                      Name.Current_Blink_Stat,
                      Name.Current_Background,
                      Name.Current_Xor_Mode);
      Move (Name => Name);
    end Clear;

    procedure Move (Row  : in Row_Range;
                    Col  : in Col_Range;
                    Name : in Window := Screen) is
    begin
      if Name = null then
        raise Window_Not_Open;
      end if;
      if (Row > Name.Lower_Right.Row - Name.Upper_Left.Row) or else
         (Col > Name.Lower_Right.Col - Name.Upper_Left.Col) then
        raise Invalid_Square;
      end if;
      Name.Current_Pos := (Row, Col);
    end Move;

    function Position (Name : Window := Screen) return Square is
    begin
      if Name = null then
        raise Window_Not_Open;
      end if;
      return Name.Current_Pos;
    end Position;

    procedure Bell (Repeat : in Positive := 1) is
    begin
      if not Init_Done then
        raise Not_Init;
      end if;
      if Repeat in X_Mng.Bell_Repeat then
        X_Mng.X_Bell (Id, Repeat);
      else
        X_Mng.X_Bell (Id, X_Mng.Bell_Repeat'Last);
      end if;
    end Bell;

    -- Get window attributes when current, and set the whole
    procedure Set_Attributes_From_Window (
                       Name       : in Window;
                       Foreground : in Colors;
                       Blink_Stat : in Blink_Stats;
                       Background : in Colors) is
      Fg : Effective_Colors;
      Bl : Effective_Blink_Stats;
      Bg : Effective_Colors;
    begin
      if Foreground = Current then
        Fg := Name.Current_Foreground;
      else
        Fg := Foreground;
      end if;
      if Blink_Stat = Current then
        Bl := Name.Current_Blink_Stat;
      else
        Bl := Blink_Stat;
      end if;
      if Background = Current then
        Bg := Name.Current_Background;
      else
        Bg := Background;
      end if;
      Set_Attributes (Fg, Bl, Bg, Name.Current_Xor_Mode);
    end Set_Attributes_From_Window;

    -- Raw conversion, no exception but Wide_Def_Char
    function Wide_To_Char (W : Wide_Character) return Character is
    begin
      if Language.Is_Char (W) then
        return Language.Wide_To_Char (W);
      else
        return Wide_Def_Char;
      end if;
    end Wide_To_Char;
    function Wide_To_String (Str : Wide_String) return String is
      Res : String (1 .. Str'Length);
      Index : Positive;
    begin
      Index := Res'First;
      for I in Str'Range loop
        Res(Index) := Wide_To_Char (Str(I));
        Index := Index + 1;
      end loop;
      return Res;
    end Wide_To_String;

    function "&" (Left : String; Right : Wide_String) return String is
    begin
      return Left & Wide_To_String (Right);
    end "&";

    function "&" (Left : Wide_String; Right : String) return String is
    begin
      return Wide_To_String (Left) & Right;
    end "&";

    -- Increment col by one or row by one...
    procedure Move_1 (Name : in Window := Screen) is
    begin
      if Name.Current_Pos.Col /= Name.Lower_Right.Col - Name.Upper_Left.Col then
        -- Next col
        Name.Current_Pos.Col := Col_Range'Succ(Name.Current_Pos.Col);
      else
        -- 1st col
        Name.Current_Pos.Col := Col_Range'First;
        if Name.Current_Pos.Row /=
           Name.Lower_Right.Row  - Name.Upper_Left.Row then
          -- Next line
          Name.Current_Pos.Row := Row_Range'Succ(Name.Current_Pos.Row);
        else
          -- No scroll :-( first row
          Name.Current_Pos.Row := Row_Range'First;
        end if;
      end if;
    end Move_1;

    -- Internal write of the string of ONE character at the current cursor
    --  position and with attributes.
    -- Lf only is interpreted
    procedure Put_1_Char (C          : in String;
                          Name       : in Window := Screen;
                          Foreground : in Colors := Current;
                          Blink_Stat : in Blink_Stats := Current;
                          Background : in Colors := Current;
                          Move       : in Boolean := True) is
    begin
      if Name = null then
        raise Window_Not_Open;
      end if;
      if Language.Put_Length (C) /= 1 then
        -- Internal error put a "Warning" character
        X_Mng.X_Put_String (Id, "#",
                            Name.Upper_Left.Row + Name.Current_Pos.Row,
                            Name.Upper_Left.Col + Name.Current_Pos.Col);
      elsif C /= Lfs then
        Set_Attributes_From_Window (Name, Foreground, Blink_Stat, Background);
        -- Put character
        X_Mng.X_Put_String (Id, C,
                            Name.Upper_Left.Row + Name.Current_Pos.Row,
                            Name.Upper_Left.Col + Name.Current_Pos.Col);
      end if;
      if Move then
        if C = Lfs then
          -- End of current row
          Name.Current_Pos.Col := Name.Lower_Right.Col - Name.Upper_Left.Col;
        end if;
        Move_1 (Name);
      end if;
    end Put_1_Char;


    -- Writes a character at the current cursor position and with the
    --  curent attributes. Position can be set by using move.
    -- Lf is the only special Ascii character which is interpreted.
    -- If not Move, the cursor position is not updated
    --  (Lf would be ignored then)
    procedure Put (C          : in Character;
                   Name       : in Window := Screen;
                   Foreground : in Colors := Current;
                   Blink_Stat : in Blink_Stats := Current;
                   Background : in Colors := Current;
                   Move       : in Boolean := True) is
    begin
      Put_1_Char (C & "", Name, Foreground, Blink_Stat, Background, Move);
    end Put;

    -- Idem with a string
    procedure Put (S          : in String;
                   Name       : in Window := Screen;
                   Foreground : in Colors := Current;
                   Blink_Stat : in Blink_Stats := Current;
                   Background : in Colors := Current;
                   Move       : in Boolean := True) is
      Ifirst, Ilast : Natural;
      Last : Natural;
      Saved_Pos : constant Square := Name.Current_Pos;
      Win_Last_Col : constant Col_Range
                   := Name.Lower_Right.Col - Name.Upper_Left.Col;
      Indexes : constant Language.Index_Array
              := Language.All_Indexes_Of (S);
      Plf : Boolean;

      procedure X_Put (Str : in String) is
      begin
        if Str'Length /= 0 then
          X_Mng.X_Put_String (Id, Str,
                  Name.Upper_Left.Row + Name.Current_Pos.Row,
                  Name.Upper_Left.Col + Name.Current_Pos.Col);
        end if;
      end X_Put;

    begin
      if Name = null then
        raise Window_Not_Open;
      end if;
      -- Check empty string
      if S = "" then
        return;
      end if;
      Set_Attributes_From_Window (Name, Foreground, Blink_Stat, Background);
      -- Put chunks of string due to Lfs or too long slices
      Ifirst := Indexes'First;
      loop
        Ilast := Ifirst;
        Plf := False;
        -- Look for Lf or end of string
        while Ilast /= Indexes'Last and then S(Indexes(Ilast)) /= Lf loop
          Ilast := Ilast + 1;
        end loop;
        -- Skip Lf
        if S(Indexes(Ilast)) = Lf then
          Ilast := Ilast - 1;
          Plf := True;
        end if;
        -- Truncate to fit window
        -- Last - first <= Win_last_col - Pos
        if Name.Current_Pos.Col + Ilast - Ifirst  > Win_Last_Col then
           Ilast := Ifirst + Win_Last_Col - Name.Current_Pos.Col;
        end if;
        -- Set Last to last char to put
        if Ilast /= Indexes'Last then
          Last := Indexes(Ilast + 1) - 1;
        else
          Last := S'Last;
        end if;
        -- Put the chunk
        X_Put (S(Indexes(Ifirst) .. Last));
        -- Update position : last character + one
        One_Con_Io.Move (Name.Current_Pos.Row,
                         Name.Current_Pos.Col + Ilast - Ifirst,
                         Name);
        Move_1 (Name);
        -- Issue Lf
        if Plf then
          Put_1_Char (Lfs, Name);
          Ilast := Ilast + 1;
        end if;
        -- Move to next chunk
        exit when Ilast = Indexes'Last;
        Ifirst := Ilast + 1;
      end loop;

      -- Restore pos
      if not Move then
        One_Con_Io.Move (Saved_Pos, Name);
      end if;

    end Put;

    -- Idem but appends a CR
    procedure Put_Line (S          : in String;
                        Name       : in Window := Screen;
                        Foreground : in Colors := Current;
                        Blink_Stat : in Blink_Stats := Current;
                        Background : in Colors := Current) is
    begin
      if Name = null then
        raise Window_Not_Open;
      end if;
      -- Puts the string
      Put(S, Name, Foreground, Blink_Stat, Background);
      -- New line
      New_Line(Name);
    end Put_Line;

    -- Idem with a wide character
    procedure Putw (W          : in Wide_Character;
                    Name       : in Window := Screen;
                    Foreground : in Colors := Current;
                    Blink_Stat : in Blink_Stats := Current;
                    Background : in Colors := Current;
                    Move       : in Boolean := True) is
    begin
      Put (Language.Wide_To_String (W & ""),
           Name, Foreground, Blink_Stat, Background, Move);
    end Putw;

    -- Idem with a wide string
    procedure Putw (S          : in Wide_String;
                    Name       : in Window := Screen;
                    Foreground : in Colors := Current;
                    Blink_Stat : in Blink_Stats := Current;
                    Background : in Colors := Current;
                    Move       : in Boolean := True) is
    begin
      Put (Language.Wide_To_String (S),
           Name, Foreground, Blink_Stat, Background, Move);
    end Putw;

    -- Idem but appends a Lf
    procedure Putw_Line (S          : in Wide_String;
                         Name       : in Window := Screen;
                         Foreground : in Colors := Current;
                         Blink_Stat : in Blink_Stats := Current;
                         Background : in Colors := Current) is
    begin
      Put_Line (Language.Wide_To_String (S),
           Name, Foreground, Blink_Stat, Background);
    end Putw_Line;


    -- Puts CR
    procedure New_Line (Name   : in Window := Screen;
                        Number : in Positive := 1) is
    begin
      if Name = null then
        raise Window_Not_Open;
      end if;
      for I in 1 .. Number loop
        Put (Lf, Name);
      end loop;
    end New_Line;

    -- Selection (in/out) management
    -- Set/reset the selection to be transfered to other applications
    procedure Set_Selection (Selection : in String) is
    begin
      if not Init_Done then
        raise Not_Init;
      end if;
      if Selection = "" then
        X_Mng.X_Reset_Selection (Id);
      else
        X_Mng.X_Set_Selection (Id, Selection);
      end if;
    end Set_Selection;

    -- Request selection from other applications. An event (Curs_Mvt) of
    --  kind Selection will be received, then Get_Selection shall be called
    procedure Request_Selection is
    begin
      if not Init_Done then
        raise Not_Init;
      end if;
      X_Mng.X_Request_Selection (Id);
    end Request_Selection;

    -- Get the requested selection
    function Get_Selection (Max_Len : Natural) return String is
    begin
      if not Init_Done then
        raise Not_Init;
      end if;
      return X_Mng.X_Get_Selection (Id, Max_Len);
    exception
      when X_Mng.X_Failure =>
        -- Unable to get selection
        return "";
    end Get_Selection;


    -- X event management
    procedure Next_X_Event (Timeout : in Timers.Delay_Rec;
                            X_Event : out X_Mng.Event_Kind) is
    -- In out for X_Mng
    X_Timeout : Timers.Delay_Rec := Timeout;
    begin
      -- Wait
      X_Mng.X_Wait_Event (Id, X_Timeout, X_Event);
    end Next_X_Event;

    -- Maps some function keys (16#FF# 16#xx#) back into a normal key
    --  or to another function key
    procedure Translate_X_Key (Kbd_Tab : in out X_Mng.Kbd_Tab_Code;
                               Is_Code : in out Boolean) is
      Key : X_Mng.Byte := Kbd_Tab.Tab(2);
      Translated : Boolean;
      use type X_Mng.Byte;
    begin
      -- No translation of chars, only function keys
      if not Is_Code or else Kbd_Tab.Tab(1) /= 16#FF# then
        return;
      end if;
      Translated := True;
      case Key is
        when 16#8D# =>
          -- Enter
          Key := 16#0D#;
        when  16#AA# .. 16#B9# =>
          -- Oper or Num
          Is_Code := False;
          Key := Key - 16#80#;
        when 16#95# .. 16#9C# =>
          -- Key movement
          Key := Key - 16#45#;
        when 16#9D# =>
          -- 5 not num : discard
          Key := 16#00#;
        when 16#9E# =>
          -- Insert
          Key := 16#63#;
        when 16#9F# =>
          -- Suppre
          Key := 16#FF#;
        when others =>
          -- No translation
          Translated := False;
      end case;
      if Translated then
        if Is_Code then
          Kbd_Tab.Tab(2) := Key;
        else
          Kbd_Tab.Tab(1) := Key;
          Kbd_Tab.Nbre := 1;
        end if;
      end if;
    end Translate_X_Key;

    -- Check if a key is available until a certain time.
    -- Returns if key pressed (Esc event), then Ctrl..Kbd_Tab are significant
    -- otherwise mouse action, refresh, timeout...
    subtype Event_List is Curs_Mvt range Esc .. Refresh;
    procedure Get_Key_Time (Event       : out Event_List;
                            Ctrl        : out Boolean;
                            Shift       : out Boolean;
                            Code        : out Boolean;
                            Kbd_Tab     : out X_Mng.Kbd_Tab_Code;
                            Time_Out    : in Delay_Rec := Infinite_Delay) is

      X_Event : X_Mng.Event_Kind;
      use X_Mng, Ada.Calendar;
      use type Timers.Delay_Rec, Timers.Delay_List;
    begin
      if not Init_Done then
        raise Not_Init;
      end if;

      Event := Timeout;
      Ctrl := False;
      Shift := False;
      Code := False;
      Next_X_Event (Time_Out, X_Event);
      case X_Event is
        when X_Mng.Fd_Event =>
          -- Fd event
          Event := Fd_Event;
          return;
        when X_Mng.Timer_Event =>
          -- Timer event
          Event := Timer_Event;
          return;
        when X_Mng.Signal_Event =>
          -- Signal event
          Event := Signal_Event;
          return;
        when X_Mng.Refresh =>
          -- Refresh
          Event := Refresh;
          return;
        when X_Mng.No_Event =>
          -- Timeout
          Event := Timeout;
          return;
        when X_Mng.Tid_Press | X_Mng.Tid_Release | X_Mng.Tid_Motion =>
          Event := Mouse_Button;
          Mouse_Status := X_Event;
          return;
        when X_Mng.Selection =>
          -- Selection to get
          Event := Selection;
          return;
        when X_Mng.Exit_Request =>
          -- X Exit request from window manager
          Event := Break;
          return;
        when X_Mng.Keyboard =>
          X_Mng.X_Read_Key(Id, Ctrl, Shift, Code, Kbd_Tab);
          Translate_X_Key (Kbd_Tab, Code);
          -- Check break
          if not Code
          and then Ctrl
          and then Kbd_Tab.Nbre = 1
          and then (Kbd_Tab.Tab(1) = 3
            or else Kbd_Tab.Tab(1) = Character'Pos('c')) then
            -- Ctrl c
            Event := Break;
            return;
          end if;
          -- Escape for any other keyboard key
          Event := Esc;
          return;
      end case;

    end Get_Key_Time;

    -- Idem but the get is initialised with the initial content of the string
    --  and cursor's initial location can be set
    procedure Put_Then_Get (Str        : in out Wide_String;
                            Last       : out Natural;
                            Stat       : out Curs_Mvt;
                            Pos        : in out Positive;
                            Insert     : in out Boolean;
                            Name       : in Window := Screen;
                            Foreground : in Colors := Current;
                            Blink_Stat : in Blink_Stats := Current;
                            Background : in Colors := Current;
                            Time_Out   : in Delay_Rec :=  Infinite_Delay;
                            Echo       : in Boolean := True) is
      -- Local string for working on
      Lstr        : Asu_Us := Asu_Tus (Language.Wide_To_String (Str));
      -- Indexes in Lstr of put positions
      Indexes     : Language.Index_Array
                  := Language.All_Indexes_Of (Asu_Ts(Lstr));
      -- Constant put width
      Width       : constant Natural
                  := Indexes'Length;
      -- Got event
      Event       : Event_List;
      -- Got key & info
      Kbd_Tab     : X_Mng.Kbd_Tab_Code;
      Code        : Boolean;
      Ctrl, Shift : Boolean;

      -- Key of function key (2nd byte)
      Key         : Natural;
      -- Done?
      Done        :  Boolean;
      Redraw      : Boolean;
      First_Pos   : constant Square := Name.Current_Pos;
      Last_Time   : Delay_Rec;

      -- Return index in str of last char of a position
      function End_Index_Of (Position : Natural) return Natural is
        -- Nb of chars for last position
        Last_Char_Nb : Positive;
      begin
        if Position > Indexes'Last then
          return 0;
        end if;
        Last_Char_Nb := Language.Nb_Chars (
                             Asu.Element (Lstr, Indexes(Position)));
        return Indexes(Position) + Last_Char_Nb - 1;
      end End_Index_Of;
      -- Return index of last significant char of Str
      function Parse return Natural is
      begin
        for I in reverse 1 .. Width loop
          if Asu.Element (Lstr, Indexes(I)) /= ' ' then
            -- This character is the last meaningfull
            return End_Index_Of (I);
          end if;
        end loop;
        -- All is spaces
        return 0;
      end Parse;

      -- Return slice of Str between 2 positions
      function Slice (First_Pos : Positive;
                      Last_Pos  : Natural) return String is
      begin
        if Last_Pos < First_Pos then
          return "";
        else
          return Asu.Slice (Lstr, Indexes(First_Pos), End_Index_Of (Last_Pos));
        end if;
      end Slice;

      -- Overwrite a slice of Str by a string
      procedure Overwrite (At_Pos : Positive; By : String) is
        Ifirst, Ilast : Positive;
        Len : constant Natural := Language.Put_Length (By);
      begin
        if At_Pos + Len - 1 > Width then
          raise Constraint_Error;
        end if;
        if Len = 0 then
          return;
        end if;
        Ifirst := Indexes (At_Pos);
        Ilast := End_Index_Of (At_Pos + Len - 1);
        Asu.Replace_Slice (Lstr, Ifirst, Ilast, By);
        Indexes := Language.All_Indexes_Of (Asu_Ts(Lstr));
      end Overwrite;

      procedure Cursor (Show : in Boolean) is
        Absolute_Pos : Square;
      begin
        Move(First_Pos.Row, First_Pos.Col + Pos - 1, Name);
        Absolute_Pos := To_Absolute (Name.Current_Pos, Name);
        if Show then
          if Insert then
            X_Mng.X_Overwrite_Char (Id, 16#5E#,
                  Absolute_Pos.Row, Absolute_Pos.Col);
          else
            X_Mng.X_Overwrite_Char (Id, 16#5F#,
                  Absolute_Pos.Row, Absolute_Pos.Col);
          end if;
        else
          X_Mng.X_Put_String (Id, Slice(Pos, Pos),
                Absolute_Pos.Row, Absolute_Pos.Col);
        end if;
      end Cursor;

      use type Timers.Delay_Rec, Timers.Delay_List;
      use type X_Mng.Byte;
      use type Asu_Us;
    begin
      -- Time at which the get ends
      if Time_Out = Timers.Infinite_Delay
      or else Time_Out.Delay_Kind = Timers.Delay_Exp then
        Last_Time := Time_Out;
      else
        Last_Time := (Delay_Kind => Timers.Delay_Exp,
                      Clock      => null,
                      Period     => Timers.No_Period,
                      Expiration_Time => Ada.Calendar."+"
                         (Ada.Calendar.Clock, Time_Out.Delay_Seconds) );
      end if;

      -- Emtpy string
      if Width = 0 then
        Last := 0;

        loop
          Get_Key_Time (Event, Ctrl, Shift, Code, Kbd_Tab, Last_Time);
          if Event /= Esc then
            -- No key ==> mouse, time out, refresh, fd...
            Stat := Event;
            return;
          elsif Code and then Kbd_Tab.Tab(1) = 16#FF# then
            Key := Natural (Kbd_Tab.Tab(2));
            -- Function key
            case Key is
              when 16#0D# =>
                -- Return
                Stat := Ret;
                return;
              when 16#1B# =>
                -- Escape
                Stat := Esc;
                return;
              when 16#09# =>
                if Shift then
                  -- Shift Tab
                  Stat := Stab;
                else
                  -- Tab
                  Stat := Tab;
                end if;
                return;
              when 16#08# =>
                -- Backspace
                null;
              when 16#50# =>
                -- Home
                Stat := Left;
                return;
              when 16#57# =>
                -- End
                Stat := Right;
                return;
              when 16#51# =>
                -- <--
                if not Ctrl then
                  Stat := Left;
                else
                  Stat := Ctrl_Left;
                end if;
                return;
              when 16#53# =>
                -- -->
                if not Ctrl then
                  Stat := Right;
                else
                  Stat := Ctrl_Right;
                end if;
                return;
              when 16#52# =>
                -- Up
                if not Ctrl then
                  Stat := Up;
                else
                  Stat := Ctrl_Up;
                end if;
                return;
              when 16#54# =>
                -- Down
                if not Ctrl then
                  Stat := Down;
                else
                  Stat := Ctrl_Down;
                end if;
                return;
              when 16#55# =>
                -- Page Up
                if not Ctrl then
                  Stat := Pgup;
                else
                  Stat := Ctrl_Pgup;
                end if;
                return;
              when 16#56# =>
                -- Page Down
                if not Ctrl then
                  Stat := Pgdown;
                else
                  Stat := Ctrl_Pgdown;
                end if;
                return;
              when 16#63# =>
                -- Insert
                Insert := not Insert;
              when others =>
                null;
            end case;
          elsif not Code then
            -- Every other valid char
            Stat := Full;
            return;
          end if;  -- Function key or normal key
        end loop;  -- Discard any unaccepted key
      end if;  -- Width = 0

      -- Check width and current_pos / window's width
      if Width > Name.Lower_Right.Col - Name.Upper_Left.Col  + 1 then
        raise String_Too_Long;
      end if;

      -- Put the string
      Move(First_Pos, Name);
      if Echo then
        Put(Asu_Ts(Lstr), Name,
            Foreground, Blink_Stat, Background, Move => False);
      end if;

      Done := False;
      loop
        -- Show cursor
        if Echo then
          Cursor (True);
        end if;
        Redraw := False;
        -- Try to get a key
        Get_Key_Time (Event, Ctrl, Shift, Code, Kbd_Tab, Last_Time);
        -- Hide cursor
        if Echo then
          Cursor (False);
        end if;
        if Event /= Esc then
          -- No key ==> mouse, time out, refresh, fd...
          Last := Parse;
          Stat := Event;
          Done := True;
        elsif Code and then Kbd_Tab.Tab(1) = 16#FF# then
          Key := Natural (Kbd_Tab.Tab(2));
          case Key is
            when 16#0D# =>
              -- Return
              Last := Parse;
              Stat := Ret;
              Done := True;
            when 16#1B# =>
              -- Escape
              Last := Parse;
              Stat := Esc;
              Done := True;
            when 16#09# =>
              Last := Parse;
              if Shift then
                -- Shift Tab
                Stat := Stab;
              else
                -- Tab
                Stat := Tab;
              end if;
              Done := True;
            when 16#08# =>
              -- Backspace
              if Pos /= 1 then
                Pos := Pos - 1;
                Overwrite (Pos, Slice (Pos + 1, Width) & String'(" "));
                Redraw := True;
              end if;
            when 16#50# =>
              -- Home
              Pos := 1;
            when 16#57# =>
              -- End
              Pos := Width;
            when 16#51# =>
              -- <--
              if not Ctrl and then Pos /= 1 then
                Pos := Pos - 1;
              else
                Last := Parse;
                if Ctrl then
                  Stat := Ctrl_Left;
                else
                  Stat := Left;
                end if;
                Done := True;
              end if;
            when 16#53# =>
              -- -->
              if not Ctrl and then Pos /= Width then
                Pos := Pos + 1;
              else
                Last := Parse;
                if Ctrl then
                  Stat := Ctrl_Right;
                else
                  Stat := Right;
                end if;
                Done := True;
              end if;
            when 16#52# =>
              -- Up
              Last := Parse;
              if Ctrl then
                Stat := Ctrl_Up;
              else
                Stat := Up;
              end if;
              Done := True;
            when 16#54# =>
              -- Down
              Last := Parse;
              if Ctrl then
                Stat := Ctrl_Down;
              else
                Stat := Down;
              end if;
              Done := True;
            when 16#55# =>
              -- Page Up
              Last := Parse;
              if not Ctrl then
                Stat := Pgup;
              else
                Stat := Ctrl_Pgup;
              end if;
              Done := True;
            when 16#56# =>
              -- Page Down
              Last := Parse;
              if not Ctrl then
                Stat := Pgdown;
              else
                Stat := Ctrl_Pgdown;
              end if;
              Done := True;
            when 16#63# =>
              -- Insert
              Insert := not Insert;
            when 16#FF# =>
              if not Ctrl then
                -- Suppr
                Overwrite (Pos, Slice (Pos + 1, Width) & String'(" "));
                Redraw := True;
              else
                -- Ctrl Suppr : clear field + home
                Pos := 1;
                Lstr := Width * ' ';
                Indexes := Language.All_Indexes_Of (Asu_Ts(Lstr));
                Redraw := True;
              end if;
            when others =>
              null;
          end case;
        elsif not Code then
          -- All other valid chars
          declare
            Got_Str : String (1 .. Kbd_Tab.Nbre);
          begin
            for I in Got_Str'Range loop
              Got_Str(I) := Character'Val(Kbd_Tab.Tab(I));
            end loop;
            if Language.Put_Length (Got_Str) /= 1 then
              raise Constraint_Error;
            end if;
            if Insert and then Pos < Width then
              -- Insert => Shift right
              Overwrite (Pos, Got_Str & Slice (Pos, Width - 1));
            else
              Overwrite (Pos, Got_Str);
            end if;
          end;
          Redraw := True;
          if Pos /= Width then
            Pos := Pos + 1;
          else
            Last := Parse;
            Stat := Full;
            Done := True;
          end if;
        end if;  -- Is_char

        -- Redraw if necessary
        if Redraw and then Echo then
          Move(First_Pos, Name);
          Put(Asu_Ts(Lstr), Name,
              Foreground, Blink_Stat, Background, Move => False);
       end if;
       exit when Done;
      end loop;
      Str := Language.String_To_Wide (Asu_Ts (Lstr));
    end Put_Then_Get;

    -- Gets a string of at most width characters
    procedure Get (Str        : out Wide_String;
                   Last       : out Natural;
                   Stat       : out Curs_Mvt;
                   Pos        : out Positive;
                   Insert     : out Boolean;
                   Name       : in Window := Screen;
                   Foreground : in Colors := Current;
                   Blink_Stat : in Blink_Stats := Current;
                   Background : in Colors := Current;
                   Time_Out   : in Delay_Rec :=  Infinite_Delay;
                   Echo       : in Boolean := True) is
      Lstr : Wide_String(Str'Range ) := (others => ' ');
      Lpos : Positive;
      Lins : Boolean;
    begin
      Lpos := 1;
      Lins := False;
      -- Init empty
      Put_Then_Get(Lstr, Last, Stat, Lpos, Lins, Name,
          Foreground, Blink_Stat, Background, Time_Out, Echo);
      Str := Lstr;
      Pos := Lpos;
      Insert := Lins;
    end Get;

    function Get (Name     : in Window := Screen;
                  Time_Out : in Delay_Rec := Infinite_Delay)
             return Get_Result is
      Str    : Wide_String (1 .. 1);
      Last   : Natural;
      Stat   : Curs_Mvt;
      Pos    : Positive;
      Insert : Boolean;
    begin
      Get (Str, Last, Stat, Pos, Insert, Name,
           Time_Out => Time_Out, Echo => False);
      case Stat is
        when Up =>
          return (Mvt => Up);
        when Down =>
          return (Mvt => Down);
        when Ctrl_Up =>
          return (Mvt => Ctrl_Up);
        when Ctrl_Down =>
          return (Mvt => Ctrl_Down);
        when Pgup =>
          return (Mvt => Pgup);
        when Pgdown =>
          return (Mvt => Pgdown);
        when Ctrl_Pgup =>
          return (Mvt => Ctrl_Pgup);
        when Ctrl_Pgdown =>
          return (Mvt => Ctrl_Pgdown);
        when Left =>
          return (Mvt => Left);
        when Right =>
          return (Mvt => Right);
        when Ctrl_Left =>
          return (Mvt => Ctrl_Left);
        when Ctrl_Right =>
          return (Mvt => Ctrl_Right);
        when Full =>
          -- Character input
          return (Mvt => Full, Char => Str(1));
        when Tab =>
          return (Mvt => Tab);
        when Stab =>
          return (Mvt => Stab);
        when Ret =>
          return (Mvt => Ret);
        when Esc =>
          return (Mvt => Esc);
        when Break =>
          return (Mvt => Break);
        when Selection =>
          return (Mvt => Selection);
        when Fd_Event =>
          return (Mvt => Fd_Event);
        when Timer_Event =>
          return (Mvt => Timer_Event);
        when Signal_Event =>
          return (Mvt => Signal_Event);
        when Refresh =>
          return (Mvt => Refresh);
        when Mouse_Button =>
          return (Mvt => Mouse_Button);
        when Timeout =>
          return (Mvt => Timeout);
      end case;
    end Get;

    -- Take first character of keyboard buffer (no echo) or refresh event
    procedure Pause is
      Str  : Wide_String (1 .. 0);
      Last : Natural;
      Stat : Curs_Mvt;
      Pos  : Positive;
      Ins  : Boolean;
    begin
      loop
        -- Str is empty so no echo at all
        Get (Str, Last, Stat, Pos, Ins);
        exit when Stat /= Mouse_Button;
      end loop;
    end Pause;


    procedure Enable_Motion_Events (Motion_Enabled : in Boolean) is
    begin
      if Motion_Enabled /= Motion_Enabling then
        X_Mng.X_Enable_Motion_Events(Id, Motion_Enabled);
        Motion_Enabling := Motion_Enabled;
      end if;
    end Enable_Motion_Events;

    package body Graphics is

      function X_Max return X_Range is
      begin
        if not Init_Done then
          raise Not_Init;
        end if;
        return One_Con_Io.X_Max;
      end X_Max;

      function Y_Max return Y_Range is
      begin
        if not Init_Done then
          raise Not_Init;
        end if;
        return One_Con_Io.Y_Max;
      end Y_Max;

      -- Font characteristics
      function Font_Width  return Natural is
      begin
        if not Init_Done then
          raise Not_Init;
        end if;
        return One_Con_Io.Font_Width;
      end Font_Width;

      function Font_Height return Natural is
      begin
        if not Init_Done then
          raise Not_Init;
        end if;
        return One_Con_Io.Font_Height;
      end Font_Height;

      function Font_Offset return Natural is
      begin
        if not Init_Done then
          raise Not_Init;
        end if;
        return One_Con_Io.Font_Offset;
      end Font_Offset;


      procedure Set_Screen_Attributes is
      begin
        Set_Attributes (Screen.Current_Foreground,
                        Screen.Current_Blink_Stat,
                        Screen.Current_Background,
                        Screen.Current_Xor_Mode);
      end Set_Screen_Attributes;

      procedure Put (C : in Character;
                     X : in X_Range;
                     Y : in Y_Range) is
      begin
        if not Init_Done then
          raise Not_Init;
        end if;
        Set_Screen_Attributes;
        X_Mng.X_Put_Char_Pixels(Id, X_Mng.Byte(Character'Pos(C)),
                                X, One_Con_Io.Y_Max - Y);
      end Put;

      procedure Put (S : in String;
                     X : in X_Range;
                     Y : in Y_Range) is
        Lx : X_Range;
        Ly : Y_Range;
      begin
        if not Init_Done then
          raise Not_Init;
        end if;
        Set_Screen_Attributes;
        Lx := X;
        Ly := One_Con_Io.Y_Max - Y;
        for I in S'Range loop
          X_Mng.X_Put_Char_Pixels(Id, X_Mng.Byte(Character'Pos(S(I))),
                                  Lx, Ly);
          Lx := Lx + Font_Width;
        end loop;
      end Put;

      procedure Draw_Point (X : in X_Range;
                            Y : in Y_Range) is
      begin
        if not Init_Done then
          raise Not_Init;
        end if;
        Set_Screen_Attributes;
        X_Mng.X_Draw_Point(Id, X, One_Con_Io.Y_Max - Y);
      end Draw_Point;

      procedure Draw_Line (X1 : in X_Range;
                           Y1 : in Y_Range;
                           X2 : in X_Range;
                           Y2 : in Y_Range) is
      begin
        if not Init_Done then
          raise Not_Init;
        end if;
        Set_Screen_Attributes;
        X_Mng.X_Draw_Line(Id, X1, One_Con_Io.Y_Max - Y1, X2, One_Con_Io.Y_Max - Y2);
      end Draw_Line;


      procedure Draw_Rectangle (X1 : in X_Range;
                                Y1 : in Y_Range;
                                X2 : in X_Range;
                                Y2 : in Y_Range) is
      begin
        if not Init_Done then
          raise Not_Init;
        end if;
        Set_Screen_Attributes;
        X_Mng.X_Draw_Rectangle(Id, X1, One_Con_Io.Y_Max - Y1, X2, One_Con_Io.Y_Max - Y2);
      end Draw_Rectangle;

      procedure Fill_Rectangle (X1 : in X_Range;
                                Y1 : in Y_Range;
                                X2 : in X_Range;
                                Y2 : in Y_Range) is
      begin
        if not Init_Done then
          raise Not_Init;
        end if;
        Set_Screen_Attributes;
        X_Mng.X_Fill_Rectangle(Id, X1, One_Con_Io.Y_Max - Y1, X2, One_Con_Io.Y_Max - Y2);
      end Fill_Rectangle;

      procedure Draw_Points(X, Y          : in Natural;
                            Width, Height : in Natural;
                            Points        : in Byte_Array) is
      begin
        if not Init_Done then
          raise Not_Init;
        end if;
        Set_Screen_Attributes;
        X_Mng.X_Draw_Points(Id, X, One_Con_Io.Y_Max - Y, Width, Height, Points);
      end Draw_Points;

      procedure Fill_Area(Xys : in Natural_Array) is
        Loc_Xys : Natural_Array (Xys'Range) := Xys;
        Y : Boolean;
      begin
        if not Init_Done then
          raise Not_Init;
        end if;
        -- Fix the Ys, each second index of Xys
        Y := False;
        for I in Loc_Xys'Range loop
          if Y then
            Loc_Xys(I) := One_Con_Io.Y_Max - Loc_Xys(I);
          end if;
          Y := not Y;
        end loop;
        Set_Screen_Attributes;
        X_Mng.X_Fill_Area(Id, Loc_Xys);
      end Fill_Area;

      procedure Get_Current_Pointer_Pos (Valid : out Boolean;
                                         X     : out X_Range;
                                         Y     : out Y_Range) is
        Lx, Ly : Integer;
      begin
        Valid := False;
        if not Init_Done then
          raise Not_Init;
        end if;
        X_Mng.X_Get_Current_Pointer_Position(Id, Lx, Ly);
        -- In screen? (avoiding function call for X/Y_Max)
        if       Lx in Graphics.X_Range and then Lx <= One_Con_Io.X_Max
        and then Ly in Graphics.Y_Range and then Ly <= One_Con_Io.Y_Max then
          X := Lx;
          Y := One_Con_Io.Y_Max - Ly;
          Valid := True;
        end if;
      end Get_Current_Pointer_Pos;


    end Graphics;

    -- Set pointer shape
    procedure Set_Pointer_Shape (Pointer_Shape : in Pointer_Shape_List;
                                 Grab : in Boolean) is
    begin
      if Pointer_Shape = None then
        X_Mng.X_Hide_Graphic_Pointer(Id, Grab);
      else
        X_Mng.X_Set_Graphic_Pointer(Id, Pointer_Shape=Cross, Grab);
      end if;
    end Set_Pointer_Shape;


    -- Get a mouse event. If valid is False, it means that a release
    -- has occured outside the screen, then only Button and status
    -- are significant
    procedure Get_Mouse_Event (Mouse_Event : out Mouse_Event_Rec;
                      Coordinate_Mode : in Coordinate_Mode_List := Row_Col) is
      Loc_Event : Mouse_Event_Rec(Coordinate_Mode);
      Button : X_Mng.Button_List;
      Row, Col : Integer;
      use X_Mng;
    begin
      if not Init_Done then
        raise Not_Init;
      end if;
      -- Init result : Press not valid
      Loc_Event.Valid := False;
      Loc_Event.Status := Pressed;
      Loc_Event.Button := Mouse_Button_List'First;
      if Coordinate_Mode = Row_Col then
        Loc_Event.Row := Row_Range'First;
        Loc_Event.Col := Col_Range'First;
      else
        Loc_Event.X := Graphics.X_Range'First;
        Loc_Event.Y := Graphics.Y_Range'First;
      end if;
      Mouse_Event := Loc_Event;

      -- Mouse event pending?
      if Mouse_Status = Mouse_Discard then
        return;
      end if;

      -- Get button and pos
      X_Mng.X_Read_Tid (Id, Coordinate_Mode = Row_Col, Button, Row, Col);

      -- Event was a press release or motion?
      case Mouse_Status is
        when X_Mng.Tid_Press | X_Mng.Tid_Release =>

          case Button is
            when X_Mng.None =>
              return;
            when X_Mng.Left =>
              Loc_Event.Button := Left;
            when X_Mng.Middle =>
              Loc_Event.Button := Middle;
            when X_Mng.Right =>
              Loc_Event.Button := Right;
            when X_Mng.Up =>
              Loc_Event.Button := Up;
            when X_Mng.Down =>
              Loc_Event.Button := Down;
            when X_Mng.Shift_Up =>
              Loc_Event.Button := Shift_Up;
            when X_Mng.Shift_Down =>
              Loc_Event.Button := Shift_Down;
            when X_Mng.Ctrl_Up =>
              Loc_Event.Button := Ctrl_Up;
            when X_Mng.Ctrl_Down =>
              Loc_Event.Button := Ctrl_Down;
          end case;
          if Mouse_Status = X_Mng.Tid_Press then
            Loc_Event.Status := Pressed;
          else
            Loc_Event.Status := Released;
          end if;
        when X_Mng.Tid_Motion =>
          if Button /= X_Mng.None or else not Motion_Enabling then
            return;
          end if;
          Loc_Event.Status := Motion;
          Loc_Event.Button := Motion;
        when others =>
          return;
      end case;

      -- Coordinates
      if Coordinate_Mode = Row_Col then
        if Row - 1 in Row_Range and then Col - 1 in Col_Range then
          -- In screen
          Loc_Event.Valid := True;
          Loc_Event.Row := Row - 1;
          Loc_Event.Col := Col - 1;
        else
          -- Out of screen : set to boundaries
          Loc_Event.Valid := False;
          if Row - 1 in Row_Range then
            Loc_Event.Row := Row - 1;
          elsif Row - 1 < Row_Range'First then
            Loc_Event.Row := Row_Range'First;
          elsif Row - 1 > Row_Range'Last then
            Loc_Event.Row := Row_Range'Last;
          end if;
          if Col - 1 in Col_Range then
            Loc_Event.Col := Col - 1;
          elsif Col - 1 < Col_Range'First then
            Loc_Event.Col := Col_Range'First;
          elsif Col - 1 > Col_Range'Last then
            Loc_Event.Col := Col_Range'Last;
          end if;
        end if;
      else
        if       Row in Graphics.X_Range and then Row <= One_Con_Io.X_Max
        and then Col in Graphics.Y_Range and then Col <= One_Con_Io.Y_Max then
          Loc_Event.Valid := True;
          Loc_Event.X := Row;
          Loc_Event.Y := One_Con_Io.Y_Max - Col;
        else
          Loc_Event.Valid := False;
          if Row in Graphics.X_Range and then Row <= One_Con_Io.X_Max then
            Loc_Event.X := Row;
          elsif Row < Graphics.X_Range'First then
            Loc_Event.X := Graphics.X_Range'First;
          elsif Row > X_Max then
            Loc_Event.X := X_Max;
          end if;
          if Col in Graphics.Y_Range and then Col <= One_Con_Io.Y_Max then
            Loc_Event.Y := Y_Max - Col;
          elsif Col < Graphics.Y_Range'First then
            Loc_Event.Y := Y_Max;
          elsif Col > Y_Max then
            Loc_Event.Y := Graphics.Y_Range'First;
          end if;
        end if;
      end if;
      Mouse_Event := Loc_Event;
      Mouse_Status := Mouse_Discard;
    exception
      when X_Mng.X_Failure =>
        null;
    end Get_Mouse_Event;

  end One_Con_Io;

end Generic_Con_Io;

