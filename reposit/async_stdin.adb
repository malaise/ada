with Ada.Calendar, Ada.Characters.Latin_1, Ada.Strings.Wide_Unbounded,
     Ada.Characters.Conversions;
with Event_Mng, Sys_Calls, Console, Dynamic_List, Environ, Utf_8, Language;
package body Async_Stdin is

  -- The user callback
  Cb : User_Callback_Access := null;
  -- Max len of result
  Max : Max_Chars_Range := 1;
  -- Are stdin/out a console (or a pipe)
  Stdio_Is_A_Tty : Boolean;
  -- Is the input flow to be activated
  Active : Boolean := True;

  package Line is
    -- Init (getenv history size)
    procedure Init;

    -- Add a wide character. Store it until a sequence or flush
    -- return true if end of get
    function Add (W : Wide_Character) return Boolean;
    function Flush return Boolean;

    -- Get / clear result
    function Get return Wide_String;
    procedure Clear;

  end Line;

  package body Line is
    package Latin_1 renames Ada.Characters.Latin_1;
    Bell : constant String := Latin_1.Bel & "";
    package Awu renames Ada.Strings.Wide_Unbounded;

    package History is
      -- Buffer for exchanges (in/out with caller)
      Buf : Awu.Unbounded_Wide_String;

      -- List default len.
      History_Size : Positive := 20;

      -- Move to first/last/next/previous history record
      -- Set string un Buf
      procedure First;
      procedure Last;
      procedure Next;
      procedure Prev;
      -- Search backwards from last a record which starts with Buf
      -- The search prev matching records
      -- Found any?
      function Search (Init : Boolean) return Boolean;

      -- Asu.Append Buf as new record
      procedure Add;
    end History;

    package body History is
      subtype Rec is Awu.Unbounded_Wide_String;
      package Dyn_List_Mng is new Dynamic_List (Rec);
      package List_Mng renames Dyn_List_Mng.Dyn_List;
      List : List_Mng.List_Type;
      function Match (Curr, Crit : Rec) return Boolean is
        use type Awu.Unbounded_Wide_String;
      begin
        -- Record in list starts like criteria
        return Awu.Length (Curr) >= Awu.Length (Crit)
        and then Awu.Unbounded_Slice (Curr, 1, Awu.Length (Crit)) = Crit;
      end Match;
      procedure Search is new List_Mng.Search (Match);


      -- Searched pattern
      Searched_Rec : Rec;

      -- Copy current Rec in Buf
      procedure Copy_Current is
        R : Rec;
      begin
        List.Read (R, List_Mng.Current);
        Buf := R;
      end Copy_Current;

      -- Make a Rec from Buf
      procedure Copy_Buf (R : out Rec) is
      begin
        R := Buf;
      end Copy_Buf;

      -- Clear Buf
      procedure Clear is
      begin
        Buf := Awu.Null_Unbounded_Wide_String;
      end Clear;

      -- Movements
      procedure First is
      begin
        if List.Is_Empty then
          Clear;
          return;
        end if;
        List.Rewind;
        Copy_Current;
      end First;

      procedure Last is
      begin
        if List.Is_Empty then
          Clear;
          return;
        end if;
        List.Rewind (True, List_Mng.Prev);
        Copy_Current;
      end Last;

      procedure Next is
      begin
        if List.Is_Empty then
          Clear;
          return;
        end if;
        if List.Check_Move then
          List.Move_To;
        end if;
        Copy_Current;
      end Next;

      procedure Prev is
      begin
        if List.Is_Empty then
          Clear;
          return;
        end if;
        if List.Check_Move (List_Mng.Prev) then
          List.Move_To (List_Mng.Prev);
        end if;
        Copy_Current;
      end Prev;

      function Search (Init : Boolean) return Boolean is
        Pos : Positive;
        Found : Boolean;
      begin
        if List.Is_Empty then
          return False;
        end if;
        -- Save pos
        Pos := List.Get_Position;

        if Init then
          -- Search present buf from last
          Copy_Buf (Searched_Rec);
          Search (List, Found, Searched_Rec, List_Mng.Prev,
                  From => List_Mng.Absolute);
        else
          -- Search second occurence if current record matches
          Search (List, Found, Searched_Rec, List_Mng.Prev,
                  From => List_Mng.Skip_Current);
        end if;

        if Found then
          Copy_Current;
        else
          -- Restore Pos
          List.Move_At (Pos);
          Clear;
        end if;
        return Found;
      end Search;


      procedure Add is
        R : Rec;
      begin
        if List.List_Length = History_Size then
          List.Rewind;
          List.Delete;
        end if;
        -- Append at the end of list
        Copy_Buf (R);
        List.Rewind (False, List_Mng.Prev);
        List.Insert (R);
      end Add;

    end History;

    procedure Init is
    begin
      Environ.Get_Pos ("ASYNC_STDIN_HISTORY_SIZE", History.History_Size);
    end Init;

    -- Current line of text and cursor position in it
    Ind : Positive := 1;
    Txt : Awu.Unbounded_Wide_String;

    -- Are we searching (Tab, Tab, Tab...)
    Searching : Boolean := False;

    -- Have we just stored a new string, so prev shall be last
    At_Last : Boolean := True;

    -- Current sequence characters: Esc + 3
    Seq_Max_Length : constant := 4;
    Seq : Awu.Unbounded_Wide_String;

    -- After this delay from Esc, we give up
    Seq_Delay   : constant Duration := 0.25;
    Escape_Time : Ada.Calendar.Time;
    -- Supported sequences
    Arrow_Left_Seq    : constant Wide_String := "[D";
    Arrow_Right_Seq   : constant Wide_String := "[C";
    Arrow_Up_Seq      : constant Wide_String := "[A";
    Arrow_Down_Seq    : constant Wide_String := "[B";
    Home_Seq          : constant Wide_String := "[1~";
    End_Seq           : constant Wide_String := "[4~";
    Home1_Seq         : constant Wide_String := "OH";
    End1_Seq          : constant Wide_String := "OF";
    Delete_Seq        : constant Wide_String := "[3~";
    Page_Up_Seq       : constant Wide_String := "[5~";
    Page_Down_Seq     : constant Wide_String := "[6~";
    Insert_Seq        : constant Wide_String := "[2~";

    -- Copy Buf and move to end of line
    procedure Update is
    begin
      if Awu.Length (History.Buf) /= 0 then
        Console.Set_Col (1);
        Console.Erase_Line;
        Txt := History.Buf;
        Sys_Calls.Put_Output (Language.Wide_To_String (Awu.To_Wide_String (
                                Txt)));
        Ind := Awu.Length (Txt) + 1;
      end if;
    end Update;

    -- Store Txt in history
    procedure Store is
    begin
      History.Buf := Txt;
      History.Add;
      At_Last := True;
    end Store;

    -- Some usefull definitions
    Esc : constant Wide_Character
        := Ada.Characters.Conversions.To_Wide_Character (Latin_1.Esc);
    Del : constant Wide_Character
        := Ada.Characters.Conversions.To_Wide_Character (Latin_1.Del);
    Awu_Null : constant Awu.Unbounded_Wide_String
             := Awu.Null_Unbounded_Wide_String;
    function Awu_Is_Null (Str : Awu.Unbounded_Wide_String) return Boolean is
      use type Awu.Unbounded_Wide_String;
    begin
      return Str = Awu_Null;
    end Awu_Is_Null;

    -- Insert and put a wide character
    function Insert_Put (W : in Wide_Character) return Boolean is
    begin
      -- Insert C at current position and move 1 right
      Txt := Awu.To_Unbounded_Wide_String (
                Awu.Slice (Txt, 1, Ind - 1)
              & W
              & Awu.Slice (Txt, Ind, Awu.Length(Txt)));
      Ind := Ind + 1;
      Sys_Calls.Put_Output (Language.Wide_To_String (
                             Awu.Slice (Txt, Ind - 1, Awu.Length(Txt))));
      for I in 1 .. Awu.Length(Txt) - Ind + 1 loop
        Console.Left;
      end loop;
      if Awu.Length(Txt) = Max then
        Store;
        return True;
      else
        return False;
      end if;
    end Insert_Put;

    -- Add a character. Store it until a sequence or flush
    -- return true if end of get
    function Add (W : Wide_Character) return Boolean is
       Saved_Searching : Boolean;
       C : Character;
    begin
      -- Simplistic treatment when stdin/out is not a tty
      if not Stdio_Is_A_Tty then
        Awu.Append (Txt, W);
        -- Done when control character or buffer full
        return W < ' ' or else Awu.Length(Txt) = Max;
      end if;

      -- Optim: Set C to character of W or to Nul
      if W <= Del then
        C := Ada.Characters.Conversions.To_Character (W, Latin_1.Nul);
      else
        C := Latin_1.Nul;
      end if;

      -- Save current searching status
      Saved_Searching := Searching;
      -- Default, we cancel search on each input except on escape
      Searching := False;
      case C is
        when Latin_1.Bs | Latin_1.Del =>
          -- Backspace
          if not Awu_Is_Null (Seq) then
            Store;
            Awu.Append (Txt, Esc);
            return True;
          end if;
          if Ind = 1 then
            Sys_Calls.Put_Output (Bell);
          else
            -- Move one left, shift tail
            Ind := Ind - 1;
            Txt := Awu.To_Unbounded_Wide_String (
                      Awu.Slice (Txt, 1, Ind - 1)
                    & Awu.Slice (Txt, Ind + 1, Awu.Length(Txt)));
            Console.Left;
            Console.Delete;
          end if;
        when Latin_1.Ht =>
          -- Search
          if not Awu_Is_Null (Seq) then
            Store;
            Awu.Append (Txt, Esc);
            return True;
          end if;
          if Awu.Length (Txt) = 0 then
            return False;
          end if;
          if not Saved_Searching then
            History.Buf := Txt;
          end if;
          if History.Search (not Saved_Searching) then
            Update;
            At_Last := False;
          else
            Sys_Calls.Put_Output (Bell);
          end if;
          Searching := True;
        when Latin_1.Esc =>
          -- Escape, validate previous escape
          if not Awu_Is_Null (Seq) then
            Store;
            Awu.Append (Txt, Esc);
            return True;
          end if;
          Seq := Awu.To_Unbounded_Wide_String (W & "");
          Escape_Time := Ada.Calendar.Clock;
        when ' ' .. '~' =>
          if Awu_Is_Null (Seq) then
            -- Valid ASCII character
            return Insert_Put (W);
          else
            -- Add C in sequence try to find one
            Awu.Append (Seq, W);
            declare
              Str : constant Wide_String
                  := Awu.Slice (Seq, 2, Awu.Length (Seq));
            begin
              if Str = Arrow_Left_Seq then
                -- Left if not at first
                if Ind = 1 then
                  Sys_Calls.Put_Output (Bell);
                else
                  Ind := Ind - 1;
                  Console.Left;
                end if;
                Seq := Awu_Null;
              elsif Str = Arrow_Right_Seq then
                -- Right if not at Last
                if Ind = Awu.Length (Txt) + 1 then
                  Sys_Calls.Put_Output (Bell);
                else
                  Ind := Ind + 1;
                  Console.Right;
                end if;
                Seq := Awu_Null;
              elsif Str = Home_Seq
              or else Str = Home1_Seq then
                -- Home
                Ind := 1;
                Console.Set_Col(1);
                Seq := Awu_Null;
              elsif Str = End_Seq
              or else Str = End1_Seq then
                -- End
                Ind := Awu.Length(Txt) + 1;
                Console.Set_Col(Ind);
                Seq := Awu_Null;
              elsif Str = Delete_Seq then
                -- Del
                if Ind /= Awu.Length (Txt) + 1 then
                  -- Move shift tail
                  Txt := Awu.To_Unbounded_Wide_String (
                          Awu.Slice (Txt, 1, Ind - 1)
                        & Awu.Slice (Txt, Ind + 1, Awu.Length(Txt)));
                  Console.Delete;
                end if;
                Seq := Awu_Null;
              elsif Str = Arrow_Up_Seq then
                -- Up
                if At_Last then
                  -- We have just stored a new string. Prev should get it
                  History.Last;
                else
                  History.Prev;
                end if;
                At_Last := False;
                Update;
                Seq := Awu_Null;
              elsif Str = Arrow_Down_Seq then
                -- Down
                if not At_Last then
                  -- If we have just stored a new string. Down should ignore
                  History.Next;
                  Update;
                end if;
                Seq := Awu_Null;
              elsif Str = Page_Up_Seq then
                -- Page Up
                History.First;
                At_Last := False;
                Update;
                Seq := Awu_Null;
              elsif Str = Page_Down_Seq then
                -- Page Down
                History.Last;
                At_Last := False;
                Update;
                Seq := Awu_Null;
              elsif Str = Insert_Seq then
                -- Discard Insert
                Seq := Awu_Null;
              -- No sequence identified
              elsif Awu.Length(Seq) = Seq_Max_Length then
                -- Sequence is full and will not be recognized
                Seq := Awu_Null;
                Store;
                Awu.Append (Txt, Esc);
                return True;
              elsif Awu.Length(Txt) + Awu.Length(Seq) = Max then
                -- Not enough final space to store sequence
                Seq := Awu_Null;
                Store;
                Awu.Append (Txt, Esc);
                return True;
              -- else W is stored in Seq and we return False
              end if;
            end;
          end if;
        when Latin_1.Nul =>
          -- Non ASCII (UTF-8) character
          if Awu_Is_Null (Seq) then
            return Insert_Put (W);
          else
            -- Drop
            Seq := Awu_Null;
            Store;
            Awu.Append (Txt, Esc);
            return True;
          end if;
        when others =>
          -- Other char: ASCII control (< ' ') or UTF-8 (> Del)
          Store;
          if not Awu_Is_Null (Seq) then
            Awu.Append (Txt, Esc);
          else
            Awu.Append (Txt, W);
          end if;
          if W <= Del or else Awu.Length(Txt) = Max then
            -- Any ASCII control char, or UTF-8 reaching max length
            return True;
          end if;
      end case;
      return False;
    end Add;

    function Flush return Boolean is
      use type Ada.Calendar.Time;
    begin
      if not Awu_Is_Null (Seq)
      and then Ada.Calendar.Clock - Escape_Time > Seq_Delay then
        -- Client wants to flush and Esc is getting old
        Awu.Append (Txt, Esc);
        Seq := Awu_Null;
        return True;
      end if;
      return False;
    end Flush;

    procedure Clear is
    begin
      Seq := Awu_Null;
      Txt := Awu_Null;
      Ind := 1;
      if Stdio_Is_A_Tty then
        Console.Set_Col(1);
      end if;
    end Clear;

    function Get return Wide_String is
    begin
      Awu.Append (Txt, Seq);
      Seq := Awu_Null;
      return Awu.To_Wide_String (Txt);
    end Get;

  end Line;

  -- Our callback
  function Fd_Callback (Fd : Sys_Calls.File_Desc;
                        Read : Boolean) return Boolean is
    pragma Unreferenced (Read);
    Status : Sys_Calls.Get_Status_List;
    C : Character;
    Str : String (1 .. Utf_8.Max_Chars);
    W : Wide_Character;
    Len : Natural;
    Pos : Positive;
    Result : Boolean;

  begin
    Len := 0;
    loop
      Sys_Calls.Get_Immediate (Fd, Status, C);
      case Status is
        when Sys_Calls.Got =>
          if Len = 0 then
            -- First char: compute length
            Len := Language.Nb_Chars (C);
            Pos := 1;
          else
            Pos := Pos + 1;
          end if;
          Str(Pos) := C;
          if Pos = Len then
            -- End of sequence
            if Len = 1 then
              -- Optim for single characters
              W := Ada.Characters.Conversions.To_Wide_Character (C);
            else
              -- First wide char of string to wide conversion
              W := Language.String_To_Wide( Str(1 .. Len))(1);
            end if;
            Len := 0;
            exit when Line.Add (W);
          end if;
        when Sys_Calls.None =>
          -- No more char
          exit when Line.Flush;
          return False;
        when Sys_Calls.Closed | Sys_Calls.Error =>
          -- Call Cb with empty txt
          Line.Clear;
          exit;
      end case;
    end loop;

    -- Fix tty output
    if Stdio_Is_A_Tty
    and then (C = Ada.Characters.Latin_1.Cr
      or else C = Ada.Characters.Latin_1.Lf) then
      Sys_Calls.New_Line_Output;
    end if;

    Result := Cb (Language.Wide_To_String (Line.Get));
    Line.Clear;
    return Result;

  end Fd_Callback;

  -- Activate asynchronous data to trigger callback
  procedure Activate (Allow_Input : Boolean := True) is
  begin
    -- Check if something changes
    if Allow_Input = Active then
      return;
    end if;
    -- Update status
    Active := Allow_Input;
    if Cb = null then
      -- Not in async mode at the moment
      return;
    end if;
    if Active then
      Event_Mng.Add_Fd_Callback (Sys_Calls.Stdin, True,
                                 Fd_Callback'Access);
    else
      Event_Mng.Del_Fd_Callback (Sys_Calls.Stdin, True);
    end if;
  end Activate;

  function Is_Active return Boolean is
  begin
    return Active;
  end Is_Active;

  -- Set asynchronous mode for stdin
  -- User callback is called when Max_Chars characters are entered
  --  or at each new line
  -- Set null callback to restore normal behaviour
  procedure Set_Async (User_Callback : in User_Callback_Access := null;
                       Max_Chars : in Max_Chars_Range := 1) is
    Result : Boolean;
    use type  Sys_Calls.File_Desc_Kind_List;
  begin
    Stdio_Is_A_Tty := Sys_Calls.File_Desc_Kind (Sys_Calls.Stdin) = Sys_Calls.Tty
             and then Sys_Calls.File_Desc_Kind (Sys_Calls.Stdout) = Sys_Calls.Tty;
    -- Check if restore
    if User_Callback = null then
      if Cb = null then
        return;
      else
        Cb := null;
        if Stdio_Is_A_Tty then
          Result := Sys_Calls.Set_Tty_Attr (Sys_Calls.Stdin,
                                            Sys_Calls.Canonical);
        else
          Result := Sys_Calls.Set_Blocking (Sys_Calls.Stdin, True);
        end if;
        if Active then
          Event_Mng.Del_Fd_Callback (Sys_Calls.Stdin, True);
        end if;
      end if;
    else
      if Cb = null then
        if Stdio_Is_A_Tty then
          Result := Sys_Calls.Set_Tty_Attr (Sys_Calls.Stdin,
                                            Sys_Calls.Transparent);
        else
          Result := Sys_Calls.Set_Blocking (Sys_Calls.Stdin, False);
        end if;
      else
        Result := True;
      end if;
      if Result then
        Cb := User_Callback;
        if Max_Chars /= 0 then
          Max := Max_Chars;
        else
          Max := Max_Chars_Range'Last;
        end if;
        Line.Init;
        Line.Clear;
        if Active then
          Event_Mng.Add_Fd_Callback (Sys_Calls.Stdin, True,
                                     Fd_Callback'Access);
        end if;
      else
        raise Error;
      end if;
    end if;
  end Set_Async;

  -- Put on stdout when in async
  procedure Put_Out (Str : in String) is
    Result : Boolean;
    pragma Unreferenced (Result);
  begin
    if Cb /= null then
      Result := Sys_Calls.Set_Blocking (Sys_Calls.Stdin, True);
      Sys_Calls.Put_Output (Str);
      Result := Sys_Calls.Set_Blocking (Sys_Calls.Stdin, False);
    else
      Sys_Calls.Put_Output (Str);
    end if;
  end Put_Out;

  procedure Put_Line_Out (Str : in String) is
    Result : Boolean;
    pragma Unreferenced (Result);
  begin
    if Cb /= null then
      Result := Sys_Calls.Set_Blocking (Sys_Calls.Stdin, True);
      Sys_Calls.Put_Line_Output (Str);
      Result := Sys_Calls.Set_Blocking (Sys_Calls.Stdin, False);
    else
      Sys_Calls.Put_Line_Output (Str);
    end if;
  end Put_Line_Out;

  procedure New_Line_Out is
    Result : Boolean;
    pragma Unreferenced (Result);
  begin
    if Cb /= null then
      Result := Sys_Calls.Set_Blocking (Sys_Calls.Stdin, True);
      Sys_Calls.New_Line_Output;
      Result := Sys_Calls.Set_Blocking (Sys_Calls.Stdin, False);
    else
      Sys_Calls.New_Line_Output;
    end if;
  end New_Line_Out;

  -- Put on stderr when in async
  procedure Put_Err (Str : in String) is
    Result : Boolean;
    pragma Unreferenced (Result);
  begin
    if Cb /= null then
      Result := Sys_Calls.Set_Blocking (Sys_Calls.Stdin, True);
      Sys_Calls.Put_Error (Str);
      Result := Sys_Calls.Set_Blocking (Sys_Calls.Stdin, False);
    else
      Sys_Calls.Put_Error (Str);
    end if;
  end Put_Err;

  procedure Put_Line_Err (Str : in String) is
    Result : Boolean;
    pragma Unreferenced (Result);
  begin
    if Cb /= null then
      Result := Sys_Calls.Set_Blocking (Sys_Calls.Stdin, True);
      Sys_Calls.Put_Line_Error (Str);
      Result := Sys_Calls.Set_Blocking (Sys_Calls.Stdin, False);
    else
      Sys_Calls.Put_Line_Error (Str);
    end if;
  end Put_Line_Err;

  procedure New_Line_Err is
    Result : Boolean;
    pragma Unreferenced (Result);
  begin
    if Cb /= null then
      Result := Sys_Calls.Set_Blocking (Sys_Calls.Stdin, True);
      Sys_Calls.New_Line_Error;
      Result := Sys_Calls.Set_Blocking (Sys_Calls.Stdin, False);
    else
      Sys_Calls.New_Line_Error;
    end if;
  end New_Line_Err;

end Async_Stdin;

