with Ada.Calendar;
with Argument, Environ, Lower_Str, Event_Mng, String_Mng, Dynamic_List;
package body Consoles is

  package Window_Dyn_List_Mng is new Dynamic_List (Window);
  package Window_List_Mng renames Window_Dyn_List_Mng.Dyn_List;
  Windows : Window_List_Mng.List_Type;


  procedure Set (Dest : in out Console_Data; Val : in Console_Data) is
  begin
    Dest := Val;
  end Set;
  procedure Finalize (Con : in Console_Data) is
    Id : X_Mng.Line := Con.Id;
    W : Window;
    Moved : Boolean;
  begin
    if not Con.Initialised then
      return;
    end if;
    X_Mng.X_Close_Line(Id);
    -- Del & dealloc all windows which refer to this console
    if Windows.Is_Empty then
      return;
    end if;
    Windows.Rewind;
    loop
      Windows.Read (W, Window_List_Mng.Current);
      if W.Get_Access.Con.Get_Access = Con'Unrestricted_Access then
        -- This is a window of current console
        W.Get_Access.Open := False;
        Windows.Deallocate (Moved => Moved);
        exit when not Moved;
      else
        exit when not Windows.Check_Move;
      end if;
    end loop;
  end Finalize;

  procedure Set (Dest : in out Window_Data; Val : in Window_Data) is
  begin
    Dest := Val;
  end Set;
  procedure Finalize (Win : in Window_Data) is
  begin
    null;
  end Finalize;

  -- Global X11 initialisation info
  X_Init_Done : Boolean := False;
  The_Color_Names : Colors_Definition := Default_Colors;

  Lf : Character renames Ada.Characters.Latin_1.Lf;
  Lfs : constant String := Lf & "";

  -- Can be called to initialise consoles
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
         X_Colors(Xi) := As.U.Tus (Lower_Str (X_Colors(Xi).Image));
         X_Colors(Xi) := As.U.Tus (String_Mng.Replace (
             X_Colors(Xi).Image, "_", " "));
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
  begin
    for I in The_Color_Names'Range loop
      if The_Color_Names(I).Image = Name then
        return I;
      end if;
    end loop;
    raise Unknown_Color;
  end Color_Of;

  function Color_Name_Of (Color : Effective_Colors) return String is
  begin
    return The_Color_Names(Color).Image;
  end Color_Name_Of;


  -- Some constants
  Font_Env_Name : constant String := "CON_IO_FONT";
  Font_Env_Small : constant String := "small";
  Font_Env_Large : constant String := "large";
  Line_Def : constant X_Mng.Line_Definition_Rec := (
    Screen_Id => 0,
    Row => 1,
    Column => 1,
    Height => 0,
    Width => 0,
    Background => 0,
    Border => 0,
    No_Font => 0);
  Mouse_Discard : constant X_Mng.Event_Kind := X_Mng.Keyboard;

  -- Internal
  procedure Set_Attributes (Con : in Console;
                            Foreground : in Effective_Colors;
                            Background : in Effective_Colors;
                            Xor_Mode   : in Effective_Xor_Modes;
                            Forced     : in Boolean := False) is
    C : constant access Console_Data := Con.Get_Access;
  begin
    -- Reset can be forced explicitely by the Forced argument
    if Forced or else Foreground /= C.Line_Foreground
              or else Background /= C.Line_Background then
      X_Mng.X_Set_Attributes (C.Id, Colors'Pos(Background) - 1,
                                  Colors'Pos(Foreground) - 1,
                                  Superbright => True);
      C.Line_Foreground := Foreground;
      C.Line_Background := Background;
    end if;
    if Forced or else Xor_Mode /= C.Line_Xor_Mode then
      X_Mng.X_Set_Xor_Mode (C.Id, Xor_Mode = Xor_On);
      C.Line_Xor_Mode := Xor_Mode;
    end if;
  end Set_Attributes;

  function Create (Font_No  : in Font_No_Range;
                   Row_Last : in Row_Range := Def_Row_Last;
                   Col_Last : in Col_Range := Def_Col_Last) return Console is
    Line : X_Mng.Line_Definition_Rec := Line_Def;
    Env_Str : String (1 .. Font_Env_Small'Length);
    Env_Len : Natural;
    Con_Data : Console_Data;
    Con : Console;
    Screen : Window;
  begin
    Initialise;
    Con_Data.Font_No := Font_No;
    Con_Data.Row_Range_Last := Row_Last;
    Con_Data.Col_Range_Last := Col_Last;

    Env_Str := (others => '-');
    Env_Len := Env_Str'Length;
    Environ.Get_Str (Font_Env_Name, Env_Str, Env_Len);
    if Lower_Str (Env_Str (1 .. Env_Len)) = Font_Env_Small
    and then Con_Data.Font_No /= Font_No_Range'First then
      Line.No_Font := Con_Data.Font_No - 1;
    elsif Lower_Str (Env_Str (1 .. Env_Len)) = Font_Env_Large
    and then Con_Data.Font_No /= Font_No_Range'Last then
      Line.No_Font := Con_Data.Font_No + 1;
    end if;
    Line.Background := Colors'Pos(Default_Background) - 1;
    X_Mng.X_Open_Line (Line, Con_Data.Id);
    X_Mng.X_Set_Line_Name (Con_Data.Id, Argument.Get_Program_Name);
    Con_Data.Mouse_Status := Mouse_Discard;
    X_Mng.X_Get_Graphic_Characteristics(Con_Data.Id,
          Con_Data.X_Max, Con_Data.Y_Max,
          Con_Data.Font_Width, Con_Data.Font_Height, Con_Data.Font_Offset);
    -- Max is width - 1 so that range is 0 .. max
    Con_Data.X_Max := Con_Data.X_Max - 1;
    Con_Data.Y_Max := Con_Data.Y_Max - 1;
    -- Create console
    Con_Data.Initialised := True;
    Con.Init (Con_Data);
    Set_Attributes (Con, Default_Foreground, Default_Background,
                      Default_Xor_Mode, Forced => True);
    Flush (Con);
    -- Create and store Screen window
    Screen := Open (Con'Unrestricted_Access,
         (Row_Range_First, Col_Range_First),
         (Con_Data.Row_Range_Last, Con_Data.Col_Range_Last));
    Windows.Insert (Screen);
    Con.Get_Access.Screen_Window := Window_Access(Windows.Access_Current);
    return Con;
  exception
    when others =>
      raise Init_Failure;
  end Create;

  procedure Check_Init (Con : in Console) is
  begin
    if Con = Null_Console or else not Con.Get_Access.Initialised then
      raise Not_Init;
    end if;
  end Check_Init;

  procedure Destroy (Con : in out Console) is
  begin
    Check_Init (Con);
    X_Mng.X_Close_Line(Con.Get_Access.Id);
    Con := Null_Console;
  end Destroy;

  function Is_Init (Con : Console) return Boolean is
  begin
    return Con /= Null_Console and then Con.Get_Access.Initialised;
  end Is_Init;

  -- Suspend and resume con_io
  procedure Suspend (Con : in Console) is
  begin
    Check_Init (Con);
    -- Clear window and suspend
    Reset_Term (Con);
    X_Mng.X_Suspend (Con.Get_Access.Id);
  end Suspend;

  procedure Resume (Con : in Console) is
  begin
    Check_Init (Con);
    -- Resume
    X_Mng.X_Resume (Con.Get_Access.Id);
  end Resume;

  function Is_Suspended (Con : Console) return Boolean is
  begin
    Check_Init (Con);
    return X_Mng.X_Is_Suspended (Con.Get_Access.Id);
  end Is_Suspended;

  -- Get geometry
  function Row_Range_Last  (Con : Console) return Row_Range is
  begin
    Check_Init (Con);
    return Con.Get_Access.Row_Range_Last;
  end Row_Range_Last;

  function Col_Range_Last  (Con : Console) return Row_Range is
  begin
    Check_Init (Con);
    return Con.Get_Access.Col_Range_Last;
  end Col_Range_Last;

  -- Flushes X
  procedure Flush (Con : in Console) is
  begin
    Check_Init (Con);
    X_Mng.X_Flush (Con.Get_Access.Id);
  end Flush;

  procedure Bell (Con : in Console; Repeat : in Positive := 1) is
  begin
    Check_Init (Con);
    if Repeat in X_Mng.Bell_Repeat then
      X_Mng.X_Bell (Con.Get_Access.Id, Repeat);
    else
      X_Mng.X_Bell (Con.Get_Access.Id, X_Mng.Bell_Repeat'Last);
    end if;
  end Bell;

  -- Reset screen, windows and keyboard
  procedure Reset_Term (Con : in Console) is
  begin
    Check_Init (Con);
    X_Mng.X_Clear_Line (Con.Get_Access.Id);
    -- Set current attributes in cache
    Set_Attributes (Con, Default_Foreground, Default_Background,
                    Default_Xor_Mode, Forced => True);
  end Reset_Term;

  -- Screen characteristics
  function Screen (Con : Console_Access) return Window is
  begin
    if Con = null then
      raise Not_Init;
    end if;
    Check_Init (Con.all);
    return Con.Get_Access.Screen_Window.all;
  end Screen;

  -- Open a window
  function Open (Con                     : Console_Access;
                 Upper_Left, Lower_Right : in Square) return Window is
    Win_Data : Window_Data;
  begin
    Check_Init (Con.all);
    if Upper_Left.Row > Lower_Right.Row or else
       Upper_Left.Col > Lower_Right.Col then
      raise Invalid_Square;
    end if;
    Win_Data.Con := Con.all;
    Win_Data.Open := True;
    Win_Data.Upper_Left := Upper_Left;
    Win_Data.Lower_Right := Lower_Right;
    Win_Data.Current_Pos := Home;
    Win_Data.Current_Foreground := Default_Foreground;
    Win_Data.Current_Background := Default_Background;
    Win_Data.Current_Xor_Mode   := Default_Xor_Mode;
    return Win : Window do
      Win.Init (Win_Data);
      Windows.Insert ( (Win) );
    end return;
  exception
    when Constraint_Error =>
      raise Invalid_Square;
  end Open;

  procedure Check_Win (Name : in Window) is
  begin
    if Name = Null_Window or else not Name.Get_Access.Open then
      raise Window_Not_Open;
    end if;
  end Check_Win;

  -- Make window re-usable (have to re_open it)
  procedure Close (Name : in out Window) is
    W : Window;
    Moved : Boolean;
  begin
    Check_Win (Name);
    Windows.Rewind;
    loop
      Windows.Read (W, Window_List_Mng.Current);
      if W.Get_Access = Name.Get_Access then
        Windows.Deallocate (Moved => Moved);
        exit when not Moved;
      else
        exit when not Windows.Check_Move;
      end if;
    end loop;
    Name := Null_Window;
  end Close;

  function Is_Open (Name : Window) return Boolean is
  begin
    return Name /= Null_Window and then Name.Get_Access.Open;
  end Is_Open;

  procedure Clear (Name : in Window) is
    Win : access Window_Data;
  begin
    Check_Win (Name);
    Win := Name.Get_Access;
    -- Upper left and lower right, set foreground as our background
    Set_Attributes (Win.Con, Win.Current_Background,
                    Win.Current_Background, Xor_Off);
    X_Mng.X_Draw_Area (Win.Con.Get_Access.Id,
                           Win.Lower_Right.Col - Win.Upper_Left.Col + 1,
                           Win.Lower_Right.Row - Win.Upper_Left.Row + 1,
                           Win.Upper_Left.Row, Win.Upper_Left.Col);
    Set_Attributes (Win.Con, Win.Current_Foreground,
                    Win.Current_Background,
                    Win.Current_Xor_Mode);
    Move (Name);
  end Clear;

  -- Set / get colors
  procedure Set_Foreground (Name       : in Window;
                            Foreground : in Colors := Current) is
  begin
    Check_Win (Name);
    if Foreground /= Current then
      Name.Get_Access.Current_Foreground := Foreground;
    end if;
  end Set_Foreground;

  function Get_Foreground (Name : Window) return Effective_Colors is
  begin
    Check_Win (Name);
    return Name.Get_Access.Current_Foreground;
  end Get_Foreground;

  procedure Set_Background (Name       : in Window;
                            Background : in Colors := Current) is
  begin
    Check_Win (Name);
    if Background /= Current then
      Name.Get_Access.Current_Background := Background;
    end if;
  end Set_Background;

  function Get_Background (Name : Window) return Effective_Colors is
  begin
    Check_Win (Name);
    return Name.Get_Access.Current_Background;
  end Get_Background;

  procedure Set_Xor_Mode(Name     : in Window;
                         Xor_Mode : in Xor_Modes := Current) is
  begin
    Check_Win (Name);
    if Xor_Mode /= Current then
      Name.Get_Access.Current_Xor_Mode := Xor_Mode;
    end if;
  end Set_Xor_Mode;

  function Get_Xor_Mode(Name : Window) return Effective_Xor_Modes is
  begin
    Check_Win (Name);
    return Name.Get_Access.Current_Xor_Mode;
  end Get_Xor_Mode;

  -- Get Upper_Left / Lower_Right absolute coordinates of a window
  function Get_Absolute_Upper_Left (Name : Window) return Square is
  begin
    Check_Win (Name);
    return Name.Get_Access.Upper_Left;
  end Get_Absolute_Upper_Left;

  function Get_Absolute_Lower_Right (Name : Window) return Square is
  begin
    Check_Win (Name);
    return Name.Get_Access.Lower_Right;
  end Get_Absolute_Lower_Right;

  -- Get Lower_Right relative coordinates of a window
  -- (Upper_Left is (0, 0)).
  function Get_Relative_Lower_Right (Name : Window) return Square is
    Win : access Window_Data;
  begin
    Check_Win (Name);
    Win := Name.Get_Access;
    return (Win.Lower_Right.Row - Win.Upper_Left.Row,
            Win.Lower_Right.Col - Win.Upper_Left.Col);
  end Get_Relative_Lower_Right;

  -- True if the absolute square (relative to screen) is in the window.
  -- False otherwise
  function In_Window (Name            : Window;
                      Absolute_Square : Square) return Boolean is
    Win : access Window_Data;
  begin
    Check_Win (Name);
    Win := Name.Get_Access;
    return   Absolute_Square.Row >= Win.Upper_Left.Row
    and then Absolute_Square.Row <= Win.Lower_Right.Row
    and then Absolute_Square.Col >= Win.Upper_Left.Col
    and then Absolute_Square.Col <= Win.Lower_Right.Col;
  end In_Window;

  -- Returns the relative square (relative to window), being the same
  --  physical position as the absolute square (relative to screen).
  -- May raise Invalid_Square if the absolute position is not in window.
  function To_Relative (Name            : Window;
                        Absolute_Square : Square) return Square is
    Win : access Window_Data;
  begin
    if not In_Window (Name, Absolute_Square) then
      raise Invalid_Square;
    end if;
    Win := Name.Get_Access;
    return (Row => Absolute_Square.Row - Win.Upper_Left.Row,
            Col => Absolute_Square.Col - Win.Upper_Left.Col);
  end To_Relative;

  -- Returns the absolute square (in screen) corresponding to the relative
  --  square in the window
  -- May raise Invalid_Square if the relative square is not in window
  function To_Absolute (Name            : Window;
                        Relative_Square : Square) return Square is
    Win : access Window_Data;
  begin
    Check_Win (Name);
    Win := Name.Get_Access;
    if (Relative_Square.Row >
        Win.Lower_Right.Row - Win.Upper_Left.Row) or else
       (Relative_Square.Col >
        Win.Lower_Right.Col - Win.Upper_Left.Col) then
      raise Invalid_Square;
    end if;
    return (Row => Relative_Square.Row + Win.Upper_Left.Row,
            Col => Relative_Square.Col + Win.Upper_Left.Col);
  end To_Absolute;


  -- Move cursor for use with put or get
  procedure Move (Name     : in Window;
                  Position : in Square := Home) is
  begin
    Move(Name, Position.Row, Position.Col);
  end Move;

  procedure Move (Name : in Window;
                  Row  : in Row_Range;
                  Col  : in Col_Range) is
    Win : access Window_Data;
  begin
    Check_Win (Name);
    Win := Name.Get_Access;
    if (Row > Win.Lower_Right.Row - Win.Upper_Left.Row) or else
       (Col > Win.Lower_Right.Col - Win.Upper_Left.Col) then
      raise Invalid_Square;
    end if;
    Win.Current_Pos := (Row, Col);
  end Move;

  function Position (Name : Window) return Square is
  begin
    Check_Win (Name);
    return Name.Get_Access.Current_Pos;
  end Position;


  -- Internal set attributes for the window
  procedure Set_Attributes_From_Window (
                     Win        : access Window_Data;
                     Foreground : in Colors;
                     Background : in Colors) is
    Fg : Effective_Colors;
    Bg : Effective_Colors;
  begin
    if Foreground = Current then
      Fg := Win.Current_Foreground;
    else
      Fg := Foreground;
    end if;
    if Background = Current then
      Bg := Win.Current_Background;
    else
      Bg := Background;
    end if;
    Set_Attributes (Win.Con,
                    Fg, Bg, Win.Current_Xor_Mode);
  end Set_Attributes_From_Window;

  -- Internal increment col by one or row by one...
  procedure Move_1 (Win : access Window_Data) is
  begin
    if Win.Current_Pos.Col /= Win.Lower_Right.Col
                            - Win.Upper_Left.Col then
      -- Next col
      Win.Current_Pos.Col := Col_Range'Succ(Win.Current_Pos.Col);
    else
      -- 1st col
      Win.Current_Pos.Col := Col_Range'First;
      if Win.Current_Pos.Row /=
         Win.Lower_Right.Row - Win.Upper_Left.Row then
        -- Next line
        Win.Current_Pos.Row := Row_Range'Succ(Win.Current_Pos.Row);
      else
        -- No scroll :-( first row
        Win.Current_Pos.Row := Row_Range'First;
      end if;
    end if;
  end Move_1;

  -- Internal write of the string of ONE character at the current cursor
  --  position and with attributes.
  -- Lf only is interpreted
  procedure Put_1_Char (Win        : access Window_Data;
                        C          : in String;
                        Foreground : in Colors := Current;
                        Background : in Colors := Current;
                        Move       : in Boolean := True) is
  begin
    if Language.Put_Length (C) /= 1 then
      -- Internal error put a "Warning" character
      X_Mng.X_Put_String (Win.Con.Get_Access.Id, "#",
                          Win.Upper_Left.Row + Win.Current_Pos.Row,
                          Win.Upper_Left.Col + Win.Current_Pos.Col);
    elsif C /= Lfs then
      Set_Attributes_From_Window (Win, Foreground, Background);
      -- Put character
      X_Mng.X_Put_String (Win.Con.Get_Access.Id, C,
                          Win.Upper_Left.Row + Win.Current_Pos.Row,
                          Win.Upper_Left.Col + Win.Current_Pos.Col);
    end if;
    if Move then
      if C = Lfs then
        -- End of current row
        Win.Current_Pos.Col := Win.Lower_Right.Col - Win.Upper_Left.Col;
      end if;
      Move_1 (Win);
    end if;
  end Put_1_Char;


  -- Writes a character at the current cursor position and with the
  --  curent attributes. Position can be set by using move.
  -- Lf is the only special Ascii character which is interpreted.
  -- If not Move, the cursor position is not updated
  --  (Lf would be ignored then)
  procedure Put (Name       : in Window;
                 C          : in Character;
                 Foreground : in Colors := Current;
                 Background : in Colors := Current;
                 Move       : in Boolean := True) is
  begin
    Check_Win (Name);
    Put_1_Char (Name.Get_Access, C & "", Foreground, Background, Move);
  end Put;

  -- Idem with a string
  procedure Put (Name       : in Window;
                 S          : in String;
                 Foreground : in Colors := Current;
                 Background : in Colors := Current;
                 Move       : in Boolean := True) is
    Ifirst, Ilast : Natural;
    Last : Natural;
    Acc : access Window_Data;
    Con : access Console_Data;
    Saved_Pos : Square;
    Win_Last_Col : Col_Range;
    Indexes : constant Language.Index_Array
            := Language.All_Indexes_Of (S);
    Plf : Boolean;

    procedure X_Put (Str : in String) is
    begin
      if Str'Length /= 0 then
        X_Mng.X_Put_String (Con.Id, Str,
                Acc.Upper_Left.Row + Acc.Current_Pos.Row,
                Acc.Upper_Left.Col + Acc.Current_Pos.Col);
      end if;
    end X_Put;

  begin
    Check_Win (Name);
    -- Check empty string
    if S = "" then
      return;
    end if;
    Acc := Name.Get_Access;
    Con := Acc.Con.Get_Access;
    Saved_Pos :=  Acc.Current_Pos;
    Win_Last_Col := Acc.Lower_Right.Col - Acc.Upper_Left.Col;
    Set_Attributes_From_Window (Acc, Foreground, Background);
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
      if Acc.Current_Pos.Col + Ilast - Ifirst  > Win_Last_Col then
         Ilast := Ifirst + Win_Last_Col - Acc.Current_Pos.Col;
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
      Consoles.Move (Name, Acc.Current_Pos.Row,
                           Acc.Current_Pos.Col + Ilast - Ifirst);
      Move_1 (Acc);
      -- Issue Lf
      if Plf then
        Put_1_Char (Acc, Lfs);
        Ilast := Ilast + 1;
      end if;
      -- Move to next chunk
      exit when Ilast = Indexes'Last;
      Ifirst := Ilast + 1;
    end loop;

    -- Restore pos
    if not Move then
      Consoles.Move (Name, Saved_Pos);
    end if;

  end Put;

  -- Idem but appends a CR
  procedure Put_Line (Name       : in Window;
                      S          : in String;
                      Foreground : in Colors := Current;
                      Background : in Colors := Current) is
  begin
    -- Puts the string
    Put (Name, S, Foreground, Background);
    -- New line
    New_Line (Name);
  end Put_Line;

  -- Idem with a wide character
  procedure Putw (Name       : in Window;
                  W          : in Wide_Character;
                  Foreground : in Colors := Current;
                  Background : in Colors := Current;
                  Move       : in Boolean := True) is
  begin
    Put (Name, Language.Wide_To_String (W & ""),
         Foreground, Background, Move);
  end Putw;

  -- Idem with a wide string
  procedure Putw (Name       : in Window;
                  S          : in Wide_String;
                  Foreground : in Colors := Current;
                  Background : in Colors := Current;
                  Move       : in Boolean := True) is
  begin
    Put (Name, Language.Wide_To_String (S),
         Foreground, Background, Move);
  end Putw;

  -- Idem but appends a Lf
  procedure Putw_Line (Name       : in Window;
                       S          : in Wide_String;
                       Foreground : in Colors := Current;
                       Background : in Colors := Current) is
  begin
    Put_Line (Name, Language.Wide_To_String (S),
              Foreground, Background);
  end Putw_Line;

  -- Idem with a unicode number
  procedure Putu (Name       : in Window;
                  U          : in Unicode_Number;
                  Foreground : in Colors := Current;
                  Background : in Colors := Current;
                  Move       : in Boolean := True) is
    S : constant Unicode_Sequence (1 .. 1) := (1 => U);
  begin
    Put (Name, Language.Unicode_To_String (S),
         Foreground, Background, Move);
  end Putu;

  -- Idem with a unicode sequence
  procedure Putu (Name       : in Window;
                  S          : in Unicode_Sequence;
                  Foreground : in Colors := Current;
                  Background : in Colors := Current;
                  Move       : in Boolean := True) is
  begin
    Put (Name, Language.Unicode_To_String (S),
         Foreground, Background, Move);
  end Putu;

  -- Idem but appends a Lf
  procedure Putu_Line (Name       : in Window;
                       S          : in Unicode_Sequence;
                       Foreground : in Colors := Current;
                       Background : in Colors := Current) is
  begin
    Put_Line (Name, Language.Unicode_To_String (S),
              Foreground, Background);
  end Putu_Line;

  -- Puts CR
  procedure New_Line (Name   : in Window;
                      Number : in Positive := 1) is
  begin
    for I in 1 .. Number loop
      Put (Name, Lf);
    end loop;
  end New_Line;

  -- Selection (in/out) management
  -- Set/reset the selection to be transfered to other applications
  procedure Set_Selection (Con : in Console; Selection : in String) is
  begin
    Check_Init (Con);
    if Selection = "" then
      X_Mng.X_Reset_Selection (Con.Get_Access.Id);
    else
      X_Mng.X_Set_Selection (Con.Get_Access.Id, Selection);
    end if;
  end Set_Selection;

  -- Request selection from other applications. An event (Curs_Mvt) of
  --  kind Selection will be received, then Get_Selection shall be called
  procedure Request_Selection (Con : in Console) is
  begin
    Check_Init (Con);
    X_Mng.X_Request_Selection (Con.Get_Access.Id);
  end Request_Selection;

  -- Get the requested selection
  function Get_Selection (Con : Console; Max_Len : Natural) return String is
  begin
    Check_Init (Con);
    return X_Mng.X_Get_Selection (Con.Get_Access.Id, Max_Len);
  exception
    when X_Mng.X_Failure =>
      -- Unable to get selection
      return "";
  end Get_Selection;


  -- Internal X event management
  procedure Next_X_Event (Con : access Console_Data;
                          Timeout : in Timers.Delay_Rec;
                          X_Event : out X_Mng.Event_Kind) is
  -- In out for X_Mng
  X_Timeout : Timers.Delay_Rec := Timeout;
  begin
    -- Wait
    X_Mng.X_Wait_Event (Con.Id, X_Timeout, X_Event);
  end Next_X_Event;

  -- Internal: Maps some function keys (16#FF# 16#xx#) back into a normal key
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

  -- Internal: Check if a key is available until a certain time.
  -- Returns if key pressed (Esc event), then Ctrl..Kbd_Tab are significant
  -- otherwise mouse action, refresh, timeout...
  subtype Event_List is Curs_Mvt range Esc .. Refresh;
  procedure Get_Key_Time (Con         : access Console_Data;
                          Event       : out Event_List;
                          Ctrl        : out Boolean;
                          Shift       : out Boolean;
                          Code        : out Boolean;
                          Kbd_Tab     : out X_Mng.Kbd_Tab_Code;
                          Time_Out    : in Delay_Rec := Infinite_Delay) is

    X_Event : X_Mng.Event_Kind;
    use X_Mng, Ada.Calendar;
    use type Timers.Delay_Rec, Timers.Delay_List;
  begin

    Event := Timeout;
    Ctrl := False;
    Shift := False;
    Code := False;
    Next_X_Event (Con, Time_Out, X_Event);
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
        Con.Mouse_Status := X_Event;
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
        X_Mng.X_Read_Key(Con.Id, Ctrl, Shift, Code, Kbd_Tab);
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
  procedure Put_Then_Get (Name       : in Window;
                          Str        : in out Unicode_Sequence;
                          Last       : out Natural;
                          Stat       : out Curs_Mvt;
                          Pos        : in out Positive;
                          Insert     : in out Boolean;
                          Foreground : in Colors := Current;
                          Background : in Colors := Current;
                          Time_Out   : in Delay_Rec :=  Infinite_Delay;
                          Echo       : in Boolean := True) is
    -- Window and console data
    Con : access Console_Data;
    Win : access Window_Data;
    -- Local string for working on
    Lstr        : As.U.Asu_Us := As.U.Tus (Language.Unicode_To_String (Str));
    -- Indexes in Lstr of put positions
    Indexes     : Language.Index_Array
                := Language.All_Indexes_Of (Lstr.Image);
    -- Constant put width
    Width       : constant Natural := Indexes'Length;
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
    First_Pos   : Square;
    Last_Time   : Delay_Rec;

    -- Return index in str of last char of a position
    function End_Index_Of (Position : Natural) return Natural is
      -- Nb of chars for last position
      Last_Char_Nb : Positive;
    begin
      if Position > Indexes'Last then
        return 0;
      end if;
      Last_Char_Nb := Language.Nb_Chars (Lstr.Element (Indexes(Position)));
      return Indexes(Position) + Last_Char_Nb - 1;
    end End_Index_Of;
    -- Return index of last significant char of Str
    function Parse return Natural is
    begin
      for I in reverse 1 .. Width loop
        if Lstr.Element (Indexes(I)) /= ' ' then
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
        return Lstr.Slice (Indexes(First_Pos), End_Index_Of (Last_Pos));
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
      Lstr.Replace (Ifirst, Ilast, By);
      Indexes := Language.All_Indexes_Of (Lstr.Image);
    end Overwrite;

    procedure Cursor (Show : in Boolean) is
      Absolute_Pos : Square;
    begin
      Move (Name, First_Pos.Row, First_Pos.Col + Pos - 1);
      Absolute_Pos := To_Absolute (Name, Win.Current_Pos);
      if Show then
        if Insert then
          X_Mng.X_Overwrite_Char (Con.Id, 16#5E#,
                Absolute_Pos.Row, Absolute_Pos.Col);
        else
          X_Mng.X_Overwrite_Char (Con.Id, 16#5F#,
                Absolute_Pos.Row, Absolute_Pos.Col);
        end if;
      else
        X_Mng.X_Put_String (Con.Id, Slice(Pos, Pos),
              Absolute_Pos.Row, Absolute_Pos.Col);
      end if;
    end Cursor;

    use type Timers.Delay_Rec, Timers.Delay_List;
    use type X_Mng.Byte;
    use type As.U.Asu_Us;
  begin
    Check_Win (Name);
    Win := Name.Get_Access;
    Con := Win.Con.Get_Access;
    First_Pos := Win.Current_Pos;
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
        Get_Key_Time (Con, Event, Ctrl, Shift, Code, Kbd_Tab, Last_Time);
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
    if Width > Win.Lower_Right.Col - Win.Upper_Left.Col  + 1 then
      raise String_Too_Long;
    end if;

    -- Put the string
    Move (Name, First_Pos);
    if Echo then
      Put (Name, Lstr.Image, Foreground, Background, Move => False);
    end if;

    Done := False;
    loop
      -- Show cursor
      if Echo then
        Cursor (True);
      end if;
      Redraw := False;
      -- Try to get a key
      Get_Key_Time (Con, Event, Ctrl, Shift, Code, Kbd_Tab, Last_Time);
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
              Indexes := Language.All_Indexes_Of (Lstr.Image);
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
        Move (Name, First_Pos);
        Put (Name, Lstr.Image, Foreground, Background, Move => False);
     end if;
     exit when Done;
    end loop;
    Str := Language.String_To_Unicode (Lstr.Image);
  end Put_Then_Get;

  -- Idem but with a Wide_String
  procedure Put_Then_Get (Name       : in Window;
                          Str        : in out Wide_String;
                          Last       : out Natural;
                          Stat       : out Curs_Mvt;
                          Pos        : in out Positive;
                          Insert     : in out Boolean;
                          Foreground : in Colors := Current;
                          Background : in Colors := Current;
                          Time_Out   : in Delay_Rec :=  Infinite_Delay;
                          Echo       : in Boolean := True) is
    -- Copy keeps the same range
    Seq : Unicode_Sequence := Language.Copy (Str);
  begin
    Put_Then_Get (Name, Seq, Last, Stat, Pos, Insert, Foreground,
                  Background, Time_Out, Echo);
    Str := Language.Copy (Seq);
  end Put_Then_Get;

  -- Gets a string of at most width characters
  procedure Get (Name       : in Window;
                 Str        : out Unicode_Sequence;
                 Last       : out Natural;
                 Stat       : out Curs_Mvt;
                 Pos        : out Positive;
                 Insert     : out Boolean;
                 Foreground : in Colors := Current;
                 Background : in Colors := Current;
                 Time_Out   : in Delay_Rec :=  Infinite_Delay;
                 Echo       : in Boolean := True) is
    Lstr : Unicode_Sequence(Str'Range ) := (others => Space);
    Lpos : Positive;
    Lins : Boolean;
  begin
    Lpos := 1;
    Lins := False;
    -- Init empty
    Put_Then_Get (Name, Lstr, Last, Stat, Lpos, Lins,
        Foreground, Background, Time_Out, Echo);
    Str := Lstr;
    Pos := Lpos;
    Insert := Lins;
  end Get;

  function Get (Name     : in Window;
                Time_Out : in Delay_Rec := Infinite_Delay)
           return Get_Result is
    Str    : Unicode_Sequence (1 .. 1);
    Last   : Natural;
    Stat   : Curs_Mvt;
    Pos    : Positive;
    Insert : Boolean;
  begin
    Get (Name, Str, Last, Stat, Pos, Insert,
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
  procedure Pause (Con : in Console) is
    Str  : Unicode_Sequence (1 .. 0);
    Last : Natural;
    Stat : Curs_Mvt;
    Pos  : Positive;
    Ins  : Boolean;
  begin
    Check_Init (Con);
    loop
      -- Str is empty so no echo at all
      Get (Con.Get_Access.Screen_Window.all, Str, Last, Stat, Pos, Ins);
      exit when Stat /= Mouse_Button;
    end loop;
  end Pause;


  package body Graphics is

    function X_Max (Con : Console) return X_Range is
    begin
      Check_Init (Con);
      return Con.Get_Access.X_Max;
    end X_Max;

    function Y_Max (Con : Console) return Y_Range is
    begin
      Check_Init (Con);
      return Con.Get_Access.Y_Max;
    end Y_Max;

    -- Font characteristics
    function Font_Width (Con : Console) return Natural is
    begin
      Check_Init (Con);
      return Con.Get_Access.Font_Width;
    end Font_Width;

    function Font_Height (Con : Console) return Natural is
    begin
      Check_Init (Con);
      return Con.Get_Access.Font_Height;
    end Font_Height;

    function Font_Offset (Con : Console) return Natural is
    begin
      Check_Init (Con);
      return Con.Get_Access.Font_Offset;
    end Font_Offset;

    -- Internal
    procedure Set_Screen_Attributes (Con : in Console) is
      Screen : constant access Window_Data
             := Con.Get_Access.Screen_Window.all.Get_Access;
    begin
      Set_Attributes (Con, Screen.Current_Foreground,
                           Screen.Current_Background,
                           Screen.Current_Xor_Mode);
    end Set_Screen_Attributes;

    procedure Put (Con : in Console;
                   C   : in Character;
                   X   : in X_Range;
                   Y   : in Y_Range) is
      Acc : access Console_Data;
    begin
      Check_Init (Con);
      Acc := Con.Get_Access;
      Set_Screen_Attributes (Con);
      X_Mng.X_Put_Char_Pixels (Acc.Id,
                X_Mng.Byte(Character'Pos(C)), X, Acc.Y_Max - Y);
    end Put;

    procedure Put (Con : in Console;
                   S   : in String;
                   X   : in X_Range;
                   Y   : in Y_Range) is
      Lx : X_Range;
      Ly : Y_Range;
      Acc : access Console_Data;
    begin
      Check_Init (Con);
      Acc := Con.Get_Access;
      Set_Screen_Attributes (Con);
      Lx := X;
      Ly := Acc.Y_Max - Y;
      for I in S'Range loop
        X_Mng.X_Put_Char_Pixels (Acc.Id, X_Mng.Byte(Character'Pos(S(I))),
                                Lx, Ly);
        Lx := Lx + Acc.Font_Width;
      end loop;
    end Put;

    procedure Draw_Point (Con : in Console;
                          X   : in X_Range;
                          Y   : in Y_Range) is
      Acc : access Console_Data;
    begin
      Check_Init (Con);
      Acc := Con.Get_Access;
      Set_Screen_Attributes (Con);
      X_Mng.X_Draw_Point (Acc.Id, X, Acc.Y_Max - Y);
    end Draw_Point;

    procedure Draw_Line (Con : in Console;
                         X1  : in X_Range;
                         Y1  : in Y_Range;
                         X2  : in X_Range;
                         Y2  : in Y_Range) is
      Acc : access Console_Data;
    begin
      Check_Init (Con);
      Acc := Con.Get_Access;
      Set_Screen_Attributes (Con);
      X_Mng.X_Draw_Line (Acc.Id, X1, Acc.Y_Max - Y1, X2, Acc.Y_Max - Y2);
    end Draw_Line;

    procedure Draw_Rectangle (Con : in Console;
                              X1  : in X_Range;
                              Y1  : in Y_Range;
                              X2  : in X_Range;
                              Y2  : in Y_Range) is
      Acc : access Console_Data;
    begin
      Check_Init (Con);
      Acc := Con.Get_Access;
      Set_Screen_Attributes (Con);
      X_Mng.X_Draw_Rectangle (Acc.Id, X1, Acc.Y_Max - Y1, X2, Acc.Y_Max - Y2);
    end Draw_Rectangle;

    procedure Fill_Rectangle (Con : in Console;
                              X1  : in X_Range;
                              Y1  : in Y_Range;
                              X2  : in X_Range;
                              Y2  : in Y_Range) is
      Acc : access Console_Data;
    begin
      Check_Init (Con);
      Acc := Con.Get_Access;
      Set_Screen_Attributes (Con);
      X_Mng.X_Fill_Rectangle (Acc.Id, X1, Acc.Y_Max - Y1, X2, Acc.Y_Max - Y2);
    end Fill_Rectangle;

    procedure Draw_Points(Con           : in Console;
                          X, Y          : in Natural;
                          Width, Height : in Natural;
                          Points        : in Byte_Array) is
      Acc : access Console_Data;
    begin
      Check_Init (Con);
      Acc := Con.Get_Access;
      Set_Screen_Attributes (Con);
      X_Mng.X_Draw_Points (Acc.Id, X, Acc.Y_Max - Y, Width, Height, Points);
    end Draw_Points;

    procedure Fill_Area(Con : in Console; Xys : in Natural_Array) is
      Loc_Xys : Natural_Array (Xys'Range) := Xys;
      Y : Boolean;
      Acc : access Console_Data;
    begin
      Check_Init (Con);
      Acc := Con.Get_Access;
      Set_Screen_Attributes (Con);
      -- Fix the Ys, each second index of Xys
      Y := False;
      for I in Loc_Xys'Range loop
        if Y then
          Loc_Xys(I) := Acc.Y_Max - Loc_Xys(I);
        end if;
        Y := not Y;
      end loop;
      X_Mng.X_Fill_Area(Acc.Id, Loc_Xys);
    end Fill_Area;

    procedure Get_Current_Pointer_Pos (Con   : in Console;
                                       Valid : out Boolean;
                                       X     : out X_Range;
                                       Y     : out Y_Range) is
      Lx, Ly : Integer;
      Acc : access Console_Data;
    begin
      Valid := False;
      Check_Init (Con);
      Acc := Con.Get_Access;
      Set_Screen_Attributes (Con);
      X_Mng.X_Get_Current_Pointer_Position (Acc.Id, Lx, Ly);
      -- In screen? (avoiding function call for X/Y_Max)
      if       Lx in Graphics.X_Range and then Lx <= Acc.X_Max
      and then Ly in Graphics.Y_Range and then Ly <= Acc.Y_Max then
        X := Lx;
        Y := Acc.Y_Max - Ly;
        Valid := True;
      end if;
    end Get_Current_Pointer_Pos;

  end Graphics;

  -- Enable events on mouse motion
  procedure Enable_Motion_Events (Con : in Console;
                                  Motion_Enabled : in Boolean) is
    Acc : access Console_Data;
  begin
    Check_Init (Con);
    Acc := Con.Get_Access;
    if Motion_Enabled /= Acc.Motion_Enabling then
      X_Mng.X_Enable_Motion_Events (Acc.Id, Motion_Enabled);
      Acc.Motion_Enabling := Motion_Enabled;
    end if;
  end Enable_Motion_Events;

  -- Set pointer shape
  procedure Set_Pointer_Shape (Con           : in Console;
                               Pointer_Shape : in Pointer_Shape_List;
                               Grab          : in Boolean) is
  begin
    Check_Init (Con);
    if Pointer_Shape = None then
      X_Mng.X_Hide_Graphic_Pointer(Con.Get_Access.Id, Grab);
    else
      X_Mng.X_Set_Graphic_Pointer(Con.Get_Access.Id, Pointer_Shape=Cross, Grab);
    end if;
  end Set_Pointer_Shape;

  -- Get a mouse event. If valid is False, it means that a release
  -- has occured outside the screen, then only Button and status
  -- are significant
  procedure Get_Mouse_Event (
      Con             : in Console;
      Mouse_Event     : out Mouse_Event_Rec;
      Coordinate_Mode : in Coordinate_Mode_List := Row_Col) is
    Loc_Event : Mouse_Event_Rec(Coordinate_Mode);
    Button : X_Mng.Button_List;
    Row, Col : Integer;
    use X_Mng;
    Acc : access Console_Data;
  begin
    Check_Init (Con);
    Acc := Con.Get_Access;

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
    if Acc.Mouse_Status = Mouse_Discard then
      return;
    end if;

    -- Get button and pos
    X_Mng.X_Read_Tid (Acc.Id, Coordinate_Mode = Row_Col, Button, Row, Col);

    -- Event was a press release or motion?
    case Acc.Mouse_Status is
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
        if Acc.Mouse_Status = X_Mng.Tid_Press then
          Loc_Event.Status := Pressed;
        else
          Loc_Event.Status := Released;
        end if;
      when X_Mng.Tid_Motion =>
        if Button /= X_Mng.None or else not Acc.Motion_Enabling then
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
        elsif Row - 1 > Acc.Row_Range_Last then
          Loc_Event.Row := Acc.Row_Range_Last;
        end if;
        if Col - 1 in Col_Range then
          Loc_Event.Col := Col - 1;
        elsif Col - 1 < Col_Range'First then
          Loc_Event.Col := Col_Range'First;
        elsif Col - 1 > Acc.Col_Range_Last then
          Loc_Event.Col := Acc.Col_Range_Last;
        end if;
      end if;
    else
      if       Row in Graphics.X_Range and then Row <= Acc.X_Max
      and then Col in Graphics.Y_Range and then Col <= Acc.Y_Max then
        Loc_Event.Valid := True;
        Loc_Event.X := Row;
        Loc_Event.Y := Acc.Y_Max - Col;
      else
        Loc_Event.Valid := False;
        if Row in Graphics.X_Range and then Row <= Acc.X_Max then
          Loc_Event.X := Row;
        elsif Row < Graphics.X_Range'First then
          Loc_Event.X := Graphics.X_Range'First;
        elsif Row > Acc.X_Max then
          Loc_Event.X := Acc.X_Max;
        end if;
        if Col in Graphics.Y_Range and then Col <= Acc.Y_Max then
          Loc_Event.Y := Acc.Y_Max - Col;
        elsif Col < Graphics.Y_Range'First then
          Loc_Event.Y := Acc.Y_Max;
        elsif Col > Acc.Y_Max then
          Loc_Event.Y := Graphics.Y_Range'First;
        end if;
      end if;
    end if;
    Mouse_Event := Loc_Event;
    Acc.Mouse_Status := Mouse_Discard;
  exception
    when X_Mng.X_Failure =>
      null;
  end Get_Mouse_Event;


end Consoles;

