with Argument, Dyn_Data, Sys_Calls;
package body Generic_Con_Io is

  X_Init_Done : Boolean := False;

  procedure X_Initialise is
  begin
    if not X_Init_Done then
      X_Mng.X_Initialise ("");
      X_Init_Done := True;
    end if;
  end X_Initialise;

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

    type Border_List is (Erase, Simple, Blink);

    X_Event_Waiting : Boolean;
    Motion_Enabling : Boolean;

    -- DISCARD or TID_xxx
    Mouse_Status : X_Mng.Event_Kind;

    Line_Foreground : Effective_Colors := Default_Foreground;
    Line_Background : Effective_Basic_Colors := Default_Background;
    Line_Blink_Stat : Effective_Blink_Stats := Default_Blink_Stat;
    Line_Xor_Mode   : Effective_Xor_Modes := Default_Xor_Mode;

    X_Max : Graphics.X_Range;
    Y_Max : Graphics.Y_Range;
    Font_Width  : Natural;
    Font_Height : Natural;
    Font_Offset : Natural;

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
      Env_Set, Env_Tru : Boolean;
      Env_Str : String (1 .. Font_Env_Small'Length);
      Env_Len : Natural;
    begin
      if Init_Done then
        return;
      end if;
      X_Initialise;
      Sys_Calls.Getenv (Font_Env_Name, Env_Set, Env_Tru, Env_Str, Env_Len);
      if Env_Set and then not Env_Tru then
        if Env_Str (1 .. Env_Len) = Font_Env_Small
        and then Font_No /= Font_No_Range'First then
          Line.No_Font := Font_No - 1;
        elsif Env_Str (1 .. Env_Len) = Font_Env_Large
        and then Font_No /= Font_No_Range'Last then
          Line.No_Font := Font_No + 1;
        end if;
      end if;
      X_Mng.X_Open_Line (Line, Id);
      X_Mng.X_Set_Line_Name (Id, Argument.Get_Program_Name);
      Mouse_Status := X_Mng.Discard;
      X_Event_Waiting := True;
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

    -- screen characteristics
    function Screen return Window is
    begin
      return Screen_Window;
    end Screen;

    -- reset screen, windows and keyboard
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

    -- flushes X
    procedure Flush is
    begin
      if not Init_Done then
        raise Not_Init;
      end if;
      X_Mng.X_Flush (Id);
    end Flush;

    -- set / get colors
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

    procedure Set_Background (Background : in Basic_Colors := Current;
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
      return Effective_Basic_Colors is
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


    -- get UPPER_LEFT / LOWER_RIGHT absolute coordinates of a window
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
      return Name.Upper_Left;
    end Get_Absolute_Lower_Right;

    -- get LOWER_RIGHT relative coordinates of a window (UPPER_LEFT is (0, 0)).
    function Get_Relative_Lower_Right (Name : Window) return Square is
    begin
      if Name = null then
        raise Window_Not_Open;
      end if;
      return (Name.Lower_Right.Row - Name.Upper_Left.Row,
              Name.Lower_Right.Col - Name.Upper_Left.Col);
    end Get_Relative_Lower_Right;

    -- open a window
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

    -- TRUE if the absolute square (relative to screen) is in the window.
    -- FALSE otherwise
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
    -- May raise INVALID_SQUARE if the absolute position is not in window.
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
    -- May raise INVALID_SQUARE if the relative square is not in window
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
      if Forced or else Foreground /= Line_Foreground
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

    procedure Quick_Draw (Upper_Left, Lower_Right : in Square;
                          Kind                    : in Border_List;
                          Name                    : in Window) is

      type Descript is array(1 .. 6) of X_Mng.Byte;
      Desc : constant array(Border_List) of Descript := (
        Erase  => (others => Character'Pos(' ')),
        Simple => (13, 12, 11, 14, 18, 25),
        Blink  => (13, 12, 11, 14, 18, 25));
    begin
    -- check
      if Upper_Left.Row = Row_Range'First or else
         Upper_Left.Col = Col_Range'First or else
         Lower_Right.Row = Row_Range'Last or else
         Lower_Right.Col = Col_Range'Last then
        raise Frame_Impossible;
      end if;
      case Kind is
        when Blink =>
          Set_Attributes (Name.Current_Foreground, Blink,
                          Name.Current_Background, Name.Current_Xor_Mode);
        when Simple =>
          Set_Attributes (Name.Current_Foreground, Not_Blink,
                          Name.Current_Background, Name.Current_Xor_Mode);
        when Erase =>
          Set_Attributes (Name.Current_Foreground, Name.Current_Blink_Stat,
                          Name.Current_Background, Xor_Off);
      end case;
      -- draw corners
      X_Mng.X_Put_Char (Id, Desc(Kind)(1),
            Row_Range'Pred(Upper_Left.Row), Col_Range'Pred(Upper_Left.Col));
      X_Mng.X_Put_Char (Id, Desc(Kind)(2),
            Row_Range'Pred(Upper_Left.Row), Col_Range'Succ(Lower_Right.Col));
      X_Mng.X_Put_Char (Id, Desc(Kind)(3),
            Row_Range'Succ(Lower_Right.Row), Col_Range'Succ(Lower_Right.Col));
      X_Mng.X_Put_Char (Id, Desc(Kind)(4),
            Row_Range'Succ(Lower_Right.Row), Col_Range'Pred(Upper_Left.Col));
      -- draw horiz
      for I in Upper_Left.Col .. Lower_Right.Col loop
        X_Mng.X_Put_Char (Id, Desc(Kind)(5),
              Row_Range'Pred(Upper_Left.Row), I);
        X_Mng.X_Put_Char (Id, Desc(Kind)(5),
              Row_Range'Succ(Lower_Right.Row), I);
      end loop;
      -- draw verti
      for I in Upper_Left.Row .. Lower_Right.Row loop
        X_Mng.X_Put_Char (Id, Desc(Kind)(6),
              I, Col_Range'Pred(Upper_Left.Col));
        X_Mng.X_Put_Char (Id, Desc(Kind)(6),
              I, Col_Range'Succ(Lower_Right.Col));
      end loop;
    end Quick_Draw;

    -- draw a frame around a window
    procedure Frame (Blink : in Boolean := False;
                     Name : in Window) is
    begin
      if Name = null then
        raise Window_Not_Open;
      end if;
      if Blink then
        Quick_Draw(Name.Upper_Left, Name.Lower_Right, One_Con_Io.Blink, Name);
      else
        Quick_Draw(Name.Upper_Left, Name.Lower_Right, Simple, Name);
      end if;
    end Frame;

    procedure Clear_Frame (Name : in Window) is
    begin
      if Name = null then
        raise Window_Not_Open;
      end if;
      Quick_Draw(Name.Upper_Left, Name.Lower_Right, Erase, Name);
    end Clear_Frame;


    -- make window re-usable (have to re_open it)
    procedure Close (Name : in out Window) is
    begin
      if Name = null then
        raise Window_Not_Open;
      end if;
      Dyn_Win.Free(Name);
    end Close;

    -- move cursor for use with put or get
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
      -- upper left and lower right, set foreground as our background
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
      X_Mng.X_Flush (Id);
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
                       Background : in Basic_Colors) is
      Fg : Effective_Colors;
      Bl : Effective_Blink_Stats;
      Bg : Effective_Basic_Colors;
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

    -- Writes a character at the current cursor position and with attributes.
    -- Position is not updated.
    procedure Put_Int (Int        : in Int_Char;
                       Name       : in Window := Screen;
                       Foreground : in Colors := Current;
                       Blink_Stat : in Blink_Stats := Current;
                       Background : in Basic_Colors := Current) is
    begin
      if Name = null then
        raise Window_Not_Open;
      end if;
      if Int /= Character'Pos(Ascii.Cr) then
        Set_Attributes_From_Window (Name, Foreground, Blink_Stat, Background);
        -- put character
        X_Mng.X_Put_Char (Id, X_Mng.Byte(Int),
                          Name.Upper_Left.Row + Name.Current_Pos.Row,
                          Name.Upper_Left.Col + Name.Current_Pos.Col);
      end if;
    end Put_Int;

    procedure Put_Not_Move (C          : in Character;
                            Name       : in Window := Screen;
                            Foreground : in Colors;
                            Blink_Stat : in Blink_Stats;
                            Background : in Basic_Colors) is
    begin
      Put_Int(Character'Pos(C), Name, Foreground, Blink_Stat, Background);
    end Put_Not_Move;

    -- Increment col by one or row by one...
    procedure Move_One (Name : in Window := Screen) is
    begin
      if Name.Current_Pos.Col /= Name.Lower_Right.Col - Name.Upper_Left.Col then
        -- next col
        Name.Current_Pos.Col := Col_Range'Succ(Name.Current_Pos.Col);
      else
        -- 1st col
        Name.Current_Pos.Col := Col_Range'First;
        if Name.Current_Pos.Row /=
           Name.Lower_Right.Row  - Name.Upper_Left.Row then
          -- next_line
          Name.Current_Pos.Row := Row_Range'Succ(Name.Current_Pos.Row);
        else
          -- No scroll :-( first row
          Name.Current_Pos.Row := Row_Range'First;
        end if;
      end if;
    end Move_One;

    -- Writes a character at the current cursor position and with attributes.
    -- CR only is interpreted
    procedure Put (C          : in Character;
                   Name       : in Window := Screen;
                   Foreground : in Colors := Current;
                   Blink_Stat : in Blink_Stats := Current;
                   Background : in Basic_Colors := Current;
                   Move       : in Boolean := True) is
    begin
      if Name = null then
        raise Window_Not_Open;
      end if;
      if C /= Ascii.Cr then
        Put_Not_Move(C, Name, Foreground, Blink_Stat, Background);
      end if;
      if Move then
        if C = Ascii.Cr then
          -- End of current row
          Name.Current_Pos.Col := Name.Lower_Right.Col - Name.Upper_Left.Col;
        end if;
        Move_One (Name);
      end if;
    end Put;

    -- Idem with a string
    procedure Put (S          : in String;
                   Name       : in Window := Screen;
                   Foreground : in Colors := Current;
                   Blink_Stat : in Blink_Stats := Current;
                   Background : in Basic_Colors := Current;
                   Move       : in Boolean := True) is
      Sfirst, Slast, Rlast : Natural;
      Saved_Pos : constant Square := Name.Current_Pos;
      Win_Last_Col : constant Col_Range
                   := Name.Lower_Right.Col - Name.Upper_Left.Col;
      Pcr : Boolean;

      procedure X_Put (Str : in String) is
      begin
        if Str'Length /= 0 then
          X_Mng.X_Put_String (Id, Str, Name.Upper_Left.Row + Name.Current_Pos.Row,
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
      -- Put chunks of string due to CRs or too long slices
      Sfirst := S'First;
      loop
        Slast  := S'First;
        Pcr := False;
        -- Look for CR or end of string
        while Slast /= S'Last and then S(Slast) /= Ascii.Cr loop
          Slast := Slast + 1;
        end loop;
        -- Skip CR
        if S(Slast) = Ascii.Cr then
          Rlast := Slast - 1;
          Pcr := True;
        else
          Rlast := Slast;
        end if;
        -- Truncate to fit window
        -- Last - first <= Win_las_col - Pos 
        if Name.Current_Pos.Col + Rlast - Sfirst  > Win_Last_Col then
           Rlast := Sfirst + Win_Last_Col - Name.Current_Pos.Col;
        end if;
        -- Put the chunk
        X_Put (S(Sfirst .. Rlast));
        -- Update position : last character + one
        One_Con_Io.Move (Name.Current_Pos.Row,
                         Name.Current_Pos.Col + Rlast - Sfirst,
                         Name);
        Move_One (Name);
        -- Issue CR
        if Pcr then
          Put(Ascii.Cr, Name);
        end if;
        -- Move to next chunk
        exit when Slast = S'Last;
        Sfirst := Slast + 1;
      end loop;

      -- Resore pos
      if not Move then
        One_Con_Io.Move (Saved_Pos, Name);
      end if;

    end Put;

    -- Idem but appends a CR
    procedure Put_Line (S          : in String;
                        Name       : in Window := Screen;
                        Foreground : in Colors := Current;
                        Blink_Stat : in Blink_Stats := Current;
                        Background : in Basic_Colors := Current) is
    begin
      if Name = null then
        raise Window_Not_Open;
      end if;
      -- Puts the string
      Put(S, Name, Foreground, Blink_Stat, Background);
      -- New line
      New_Line(Name);
    end Put_Line;

    -- Puts CR
    procedure New_Line (Name   : in Window := Screen;
                        Number : in Positive := 1) is
    begin
      if Name = null then
        raise Window_Not_Open;
      end if;
      for I in 1 .. Number loop
        Put(Ascii.Cr, Name);
      end loop;
    end New_Line;

    procedure Next_X_Event (Timeout_Ms : in out Integer;
                            X_Event : out X_Mng.Event_Kind) is
      Event : Boolean;
      Loc_X_Event : X_Mng.Event_Kind;
      use X_Mng;
    begin
      loop
        if not X_Event_Waiting then
          -- Wait
          X_Mng.X_Select (Id, Timeout_Ms, Event);
          if not Event then
            X_Event := X_Mng.Discard;
            return;
          end if;
        end if;
        X_Mng.X_Process_Event (Id, Loc_X_Event, X_Event_Waiting);
        if Loc_X_Event /= X_Mng.Discard then
          X_Event := Loc_X_Event;
          return;
        end if;
      end loop;
    end Next_X_Event;

    procedure Translate_X_Key (Key     : in out Natural;
                               Is_Char : in out Boolean;
                               Ctrl    : in out Boolean;
                               Shift   : in out Boolean) is
    begin
      -- No translation of chars
      if Is_Char then
        return;
      end if;
      case Key is
        when 16#8D# =>
          -- Enter
          Key := 16#0D#;
        when  16#Aa# .. 16#B9# =>
          -- Oper or Num
          Is_Char := True;
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
          Key := 16#Ff#;
        when others =>
          -- No translation
          null;
      end case;
    end Translate_X_Key;

    procedure Get_X_Key (Key     : out Natural;
                         Is_Char : out Boolean;
                         Ctrl    : out Boolean;
                         Shift   : out Boolean) is
      Kbd_Tab : X_Mng.Kbd_Tab_Code;
      Loc_Key : Natural;
      Loc_Is_Char : Boolean;
      Loc_Ctrl : Boolean;
      Loc_Shift : Boolean;
      use X_Mng;
    begin
      X_Mng.X_Read_Key(Id, Kbd_Tab);

      Loc_Key := Natural (Kbd_Tab.Tab(Kbd_Tab.Nbre));
      Loc_Is_Char := True;
      Loc_Ctrl := False;
      Loc_Shift := False;

      -- Optimisation
      if Kbd_Tab.Nbre = 1 then
        Translate_X_Key (Loc_Key, Loc_Is_Char, Loc_Ctrl, Loc_Shift);
        Key := Loc_Key;
        Is_Char := Loc_Is_Char;
        Ctrl := Loc_Ctrl;
        Shift := Loc_Shift;
        return;
      elsif Kbd_Tab.Nbre = 2 then
        Loc_Is_Char := False;
        Translate_X_Key (Loc_Key, Loc_Is_Char, Loc_Ctrl, Loc_Shift);
        Key := Loc_Key;
        Is_Char := Loc_Is_Char;
        Ctrl := Loc_Ctrl;
        Shift := Loc_Shift;
        return;
      end if;

      if Kbd_Tab.Nbre mod 2 = 0 then
        Loc_Is_Char := False;
        Kbd_Tab.Nbre := Kbd_Tab.Nbre - 2;
      else
        Loc_Is_Char := True;
        Kbd_Tab.Nbre := Kbd_Tab.Nbre - 1;
      end if;

      if Kbd_Tab.Tab(2) = 16#E3# then
        -- Ctrl
        Loc_Ctrl := True;
      else
        -- Shift
        Loc_Shift := True;
      end if;
      if Kbd_Tab.Nbre = 4 then
        -- Ctrl Shift
        Loc_Ctrl := True;
        Loc_Shift := True;
      end if;
      
      Translate_X_Key (Loc_Key, Loc_Is_Char, Loc_Ctrl, Loc_Shift);
      Key := Loc_Key;
      Is_Char := Loc_Is_Char;
      Ctrl := Loc_Ctrl;
      Shift := Loc_Shift;
    end Get_X_Key;

    -- check if a key is available until a certain time.
    procedure Get_Key_Time (Check_Break : in Boolean;
                            Event       : out Event_List;
                            Key         : out Natural;
                            Is_Char     : out Boolean;
                            Ctrl        : out Boolean;
                            Shift       : out Boolean;
                            Time_Out    : in Delay_Rec := Infinite_Delay) is

      X_Event : X_Mng.Event_Kind;
      Cur_Time : Calendar.Time;
      Dur : Duration;
      Timeout_Ms : Integer;
      Loc_Key : Natural;
      Loc_Is_Char : Boolean;
      Loc_Ctrl : Boolean;
      Loc_Shift : Boolean;
      use X_Mng, Calendar;
      use type Timers.Delay_Rec, Timers.Delay_List;
    begin
      if not Init_Done then
        raise Not_Init;
      end if;

      if Time_Out = Infinite_Delay then
        Timeout_Ms := -1;
      elsif Time_Out.Delay_Kind = Timers.Delay_Sec then
        Timeout_Ms := Integer (Float(Time_Out.Delay_Seconds) * 1_000.0);
      else
        Cur_Time := Calendar.Clock;
        if Cur_Time > Time_Out.Expiration_Time then
          Timeout_Ms := 0;
        else
          Dur := Time_Out.Expiration_Time - Cur_Time;
          Timeout_Ms := Integer (Float(Dur) * 1_000.0);
        end if;
      end if;

      Next_X_Event (Timeout_Ms, X_Event);
      case X_Event is
        when X_Mng.Timer_Event =>
          -- Fd event
          Event := Timer_Event;
          return;
        when X_Mng.Fd_Event =>
          -- Fd event
          Event := Fd_Event;
          return;
        when X_Mng.Refresh =>
          -- Refresh
          Event := Refresh;
          return;
        when X_Mng.Discard =>
          -- Timeout
          Event := Timeout;
          return;
        when X_Mng.Tid_Press | X_Mng.Tid_Release | X_Mng.Tid_Motion =>
          Event := Mouse_Button;
          Mouse_Status := X_Event;
          return;
        when X_Mng.Keyboard =>
          Get_X_Key (Loc_Key, Loc_Is_Char, Loc_Ctrl, Loc_Shift);
          Key := Loc_Key;
          Is_Char := Loc_Is_Char;
          Ctrl := Loc_Ctrl;
          Shift := Loc_Shift;
          -- Check break
          if Check_Break then
            if (Loc_Key = Character'Pos('c') or else Loc_Key = 0)
            and then Loc_Is_Char and then Loc_Ctrl and then not Loc_Shift then
              -- Ctrl C or Ctrl break
              Event := Break;
              return;
            end if;
          end if;
          -- Escape for any other keyboard key 
          Event := Esc;
          return;
      end case;

    end Get_Key_Time;

    -- Gives first key code of keyboard buffer, (waits if it is empty)
    -- no echo
    procedure Get_Key (Key     : out Natural;
                       Is_Char : out Boolean;
                       Ctrl    : out Boolean;
                       Shift   : out Boolean) is
      Event : Event_List;
    begin
      if not Init_Done then
        raise Not_Init;
      end if;
      -- Wait for keyboard
      loop
        Get_Key_Time(False, Event, Key, Is_Char, Ctrl, Shift);
        if Event = Refresh then
          Key := 0;
          Is_Char := True;
          Ctrl := False;
          Shift := False;
          return;
        elsif Event = Fd_Event then
          Key := 1;
          Is_Char := True;
          Ctrl := False;
          Shift := False;
          return;
        elsif Event = Esc then
          -- A key
          return;
        end if;
      end loop;
    end Get_Key;
        


    -- Idem but the get is initialised with the initial content of the string
    --  and cursor's initial location can be set
    procedure Put_Then_Get (Str        : in out String;
                            Last       : out Natural;
                            Stat       : out Curs_Mvt;
                            Pos        : in out Positive;
                            Insert     : in out Boolean;
                            Name       : in Window := Screen;
                            Foreground : in Colors := Current;
                            Blink_Stat : in Blink_Stats := Current;
                            Background : in Basic_Colors := Current;
                            Time_Out   : in Delay_Rec :=  Infinite_Delay;
                            Echo       : in Boolean := True) is
      Width         : constant Natural := Str'Length;
      Lstr          : String(1 .. Width) := Str;
      Key           : Natural;
      Is_Char       : Boolean;
      Ctrl, Shift   : Boolean;
      Redraw        : Boolean;
      First_Pos     : constant Square := Name.Current_Pos;
      Last_Time     : Delay_Rec;
      Event         : Event_List;

      function Parse return Natural is
      begin
        for I in reverse 1 .. Width loop
          if Lstr(I) /= ' ' then
            -- this character is the last meaningfull
            return Str'First + I - 1;
          end if;
        end loop;
        -- all is spaces
        return 0;
      end Parse;

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
          X_Mng.X_Put_Char (Id, Lstr(Pos),
                Absolute_Pos.Row, Absolute_Pos.Col);
        end if;
      end Cursor;


      use type Timers.Delay_Rec, Timers.Delay_List;
    begin
      -- Time at which the get ends
      if Time_Out = Timers.Infinite_Delay or else Time_Out.Delay_Kind = Timers.Delay_Exp then
        Last_Time := Time_Out;
      else
        Last_Time := (Delay_Kind => Timers.Delay_Exp,
                      Period     => Timers.No_Period,
                      Expiration_Time => Calendar."+"(Calendar.Clock, Time_Out.Delay_Seconds) );
      end if;

      -- Emtpy string
      if Width = 0 then
        Last := Str'Last;

        loop
          Get_Key_Time (True, Event, Key, Is_Char, Ctrl, Shift, Last_Time);
          if Event /= Esc then
            -- No key ==> mouse, time out, refresh, fd...
            Stat := Event;
            return;
          elsif not Is_Char then
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
                if Ctrl then 
                  -- Ctrl Tab
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
                Stat := Left;
                return;
              when 16#53# =>
                -- -->
                Stat := Right;
                return;
              when 16#52# =>
                -- Up
                Stat := Up;
                return;
              when 16#55# =>
                -- Page Up
                if not Ctrl then
                  Stat := Pgup;
                else
                  Stat := Ctrl_Pgup;
                end if;
                return;
              when 16#54# =>
                -- Down
                Stat := Down;
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
          else  -- IS_CHAR
            if Key >= Character'Pos(' ')
            and then Key <= Character'Pos(Character'Last) then
              -- every other valid char
              Stat := Full;
              return;
            end if;
          end if;  -- function key or normal key
        end loop;  -- dicard any unaccepted key
      end if;  -- string'length = 0

      -- Check width and current_pos / window's width
      if Width > Name.Lower_Right.Col - Name.Upper_Left.Col  + 1 then
        raise String_Too_Long;
      end if;

      -- put the string
      Move(First_Pos, Name);
      if Echo then
        Put(Lstr, Name, Foreground, Blink_Stat, Background, Move => False);
      end if;

      loop
        -- show cursor
        if Echo then
          Cursor (True);
        end if;
        Redraw := False;
        -- try to get a key
        Get_Key_Time (True, Event, Key, Is_Char, Ctrl, Shift, Last_Time);
        -- hide cursor
        if Echo then
          Cursor (False);
        end if;
        if Event /= Esc then
          -- No key ==> mouse, time out, refresh, fd...
          Str := Lstr;
          Last := Parse;
          Stat := Event;
          return;
        elsif  not Is_Char then
          case Key is
            when 16#0D# =>
              -- Return
              Str := Lstr;
              Last := Parse;
              Stat := Ret;
              return;
            when 16#1B# =>
              -- Escape
              Str := Lstr;
              Last := Parse;
              Stat := Esc;
              return;
            when 16#09# =>
              if Ctrl then
                -- Ctrl Tab
                Str := Lstr;
                Last := Parse;
                Stat := Stab;
              else
                -- Tab
                Str := Lstr;
                Last := Parse;
                Stat := Tab;
              end if;
              return;
            when 16#08# =>
              -- backspace
              if Pos /= 1 then
                Pos := Pos - 1;
                Lstr(Pos .. Width - 1) := Lstr(Pos + 1 .. Width);
                Lstr(Width) := ' ';
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
              if Pos /= 1 then
                Pos := Pos - 1;
              else
                Str := Lstr;
                Last := Parse;
                Stat := Left;
                return;
              end if;
            when 16#53# =>
              -- -->
              if Pos /= Width then
                Pos := Pos + 1;
              else
                Str := Lstr;
                Last := Parse;
                Stat := Right;
                return;
              end if;
            when 16#52# =>
              -- Up
              Str := Lstr;
              Last := Parse;
              Stat := Up;
              return;
            when 16#55# =>
              -- Page Up
              Str := Lstr;
              Last := Parse;
              if not Ctrl then
                Stat := Pgup;
              else
                Stat := Ctrl_Pgup;
              end if;
              return;
            when 16#54# =>
              -- Down
              Str := Lstr;
              Last := Parse;
              Stat := Down;
              return;
            when 16#56# =>
              -- Page Down
              Str := Lstr;
              Last := Parse;
              if not Ctrl then
                Stat := Pgdown;
              else
                Stat := Ctrl_Pgdown;
              end if;
              return;
            when 16#63# =>
              -- Insert
              Insert := not Insert;
            when 16#Ff# =>
              if not Ctrl then
                -- Suppr
                Lstr(Pos .. Width - 1) := Lstr(Pos + 1 .. Width);
                Lstr(Width) := ' ';
                Redraw := True;
              else
                -- Ctrl Suppr : clear field + home
                Pos := 1;
                Lstr(1 .. Width) := (others => ' ');
                Redraw := True;
              end if;
            when others =>
              null;
          end case;
        else  -- is_char
          if Key >= Character'Pos(' ')
          and then Key <= Character'Pos(Character'Last) then
            -- all other valid chars
            if Insert then
              if Pos /= Width then
                Lstr(Pos + 1 .. Width) := Lstr(Pos .. Width - 1);
                Redraw := True;
              end if;
            end if;
            Lstr(Pos) := Character'Val(Key);
            if Pos /= Width then
              Pos := Pos + 1;
            else
              Str := Lstr;
              Last := Parse;
              Stat := Full;
              return;
            end if;
            if not Redraw and then Echo then
              Put(Character'Val(Key), Name, Foreground, Blink_Stat, Background);
            end if;
          end if;
        end if;  -- is_char

        -- redraw if necessary
        if Redraw and then Echo then
          Move(First_Pos, Name);
          Put(Lstr, Name, Foreground, Blink_Stat, Background, Move => False);
       end if;
      end loop;
    end Put_Then_Get;

    -- Gets a string of at most width characters
    procedure Get (Str        : out String;
                   Last       : out Natural;
                   Stat       : out Curs_Mvt;
                   Pos        : out Positive;
                   Insert     : out Boolean;
                   Name       : in Window := Screen;
                   Foreground : in Colors := Current;
                   Blink_Stat : in Blink_Stats := Current;
                   Background : in Basic_Colors := Current;
                   Time_Out   : in Delay_Rec :=  Infinite_Delay;
                   Echo       : in Boolean := True) is
      Lstr : String(Str'Range ) := (others => ' ');
      Lpos : Positive;
      Lins : Boolean;
    begin
      Lpos := 1;
      Lins := False;
      Put_Then_Get(Lstr, Last, Stat, Lpos, Lins, Name,
          Foreground, Blink_Stat, Background, Time_Out, Echo);
      Str := Lstr;
      Pos := Lpos;
      Insert := Lins;
    end Get;

    -- Take first character of keyboard buffer (no echo) or refresh event
    procedure Pause is
      Str  : String(1 .. 0);
      Last : Natural;
      Stat : Curs_Mvt;
      Pos  : Positive;
      Ins  : Boolean;
    begin
      loop
        -- STR is empty so no echo at all
        Get(Str, Last, Stat, Pos, Ins);
        exit when Stat /= Mouse_Button;
      end loop;
    end Pause;


    -- Gets first character (echo or not)
    -- No echo for RET, ESC, BREAK and REFRESH where
    --  ASCII.CR, ESC, EOT and NUL are returned respectively
    -- Cursor movements (UP to RIGHT, TAB and STAB) and mouse events are
    --  discarded (get does not return).
    function Get (Name : Window := Screen; Echo : in Boolean := True) return Character is
      Str  : String(1 .. 1);
      Last : Natural;
      Stat : Curs_Mvt;
      Pos  : Positive;
      Ins  : Boolean;
    begin
      loop
        Str := (others => ' ');
        Pos := 1;
        Ins := False;
        Put_Then_Get(Str, Last, Stat, Pos, Ins, Name, Echo => Echo);
        case Stat is
          when Up .. Right | Tab .. Stab =>
            -- Cursor movement
            null;
          when Full =>
            -- Character input
            return Str(1);
          when Ret =>
            return Ascii.Cr;
          when Esc =>
            return Ascii.Esc;
          when Break =>
            return Ascii.Eot;
          when Fd_Event =>
            return Ascii.Stx;
          when Timer_Event =>
            return Ascii.Syn;
          when Refresh =>
            return Ascii.Nul;
          when Mouse_Button | Timeout =>
            -- Ignore mouse. Timeout impossible.
            null;
        end case;
      end loop;
    end Get;

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
        -- In screen? (avoiding function call for X/Y_MAX) 
        if       Lx in Graphics.X_Range and then Lx <= One_Con_Io.X_Max
        and then Ly in Graphics.Y_Range and then Ly <= One_Con_Io.Y_Max then
          X := Lx;
          Y := One_Con_Io.Y_Max - Ly;
          Valid := True;
        end if;
      end Get_Current_Pointer_Pos;


    end Graphics;

    -- Set pointer shape
    procedure Set_Pointer_Shape (Pointer_Shape : in Pointer_Shape_List) is
    begin
      X_Mng.X_Set_Graphic_Pointer(Id, Pointer_Shape=Cross);
    end Set_Pointer_Shape;


    -- Get a mouse event. If valid is FALSE, it means that a release
    -- has occured outside the screen, then only BUTTON and status
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
      if Mouse_Status = X_Mng.Discard then
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
      Mouse_Status := X_Mng.Discard;
    exception
      when X_Mng.X_Failure =>
        null;
    end Get_Mouse_Event;

  end One_Con_Io;

end Generic_Con_Io;
   
