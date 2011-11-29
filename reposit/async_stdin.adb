with Ada.Calendar, Ada.Characters.Latin_1;
with Event_Mng, Sys_Calls, Console, Dynamic_List, Environ,
     Unicode, Utf_8, Language, As.U;
package body Async_Stdin is

  -- The user callback
  Cb : User_Callback_Access := null;
  -- Max len of result
  Max : Max_Chars_Range := 1;
  -- First column
  First_Col : Max_Chars_Range := 1;
  -- Insert mode
  Insert_Mode : Boolean := True;
  -- Are stdin/out a console (or a pipe)
  Stdio_Is_A_Tty : Boolean := False;
  -- Is the input flow to be activated
  Active : Boolean := True;

  -- Common utilities
  subtype Unicode_Number is Unicode.Unicode_Number;
  subtype Unicode_Sequence is Unicode.Unicode_Sequence;

  package Line is
    -- Init (getenv history size)
    procedure Init;

    -- Add a unicode number. Store it until a sequence or flush
    -- return true if end of get
    function Add (U : Unicode_Number) return Boolean;
    function Flush return Boolean;

    -- Get / clear result
    function Get return Unicode_Sequence;
    procedure Clear;

    -- Read current content of buffer and current cursor col
    function Read_Buffer return Unicode_Sequence;
    function Read_Col return Positive;

  end Line;

  package body Line is
    package Latin_1 renames Ada.Characters.Latin_1;
    Bell : constant String := Latin_1.Bel & "";
    package Uu renames Unicode.Unbounded_Unicode;

    package History is
      -- Buffer for exchanges (in/out with caller)
      Buf : Uu.Unbounded_Array;

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
      subtype Rec is Uu.Unbounded_Array;
      package Dyn_List_Mng is new Dynamic_List (Rec);
      package List_Mng renames Dyn_List_Mng.Dyn_List;
      List : List_Mng.List_Type;
      function Match (Curr, Crit : Rec) return Boolean is
        use type Uu.Unbounded_Array;
      begin
        -- Record in list starts like criteria
        return Uu.Length (Curr) >= Uu.Length (Crit)
        and then Uu.Unbounded_Slice (Curr, 1, Uu.Length (Crit)) = Crit;
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
        Buf := Uu.Null_Unbounded_Array;
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
    Txt : Uu.Unbounded_Array;

    -- Are we searching (Tab, Tab, Tab...)
    Searching : Boolean := False;

    -- Have we just stored a new string, so prev shall be last
    At_Last : Boolean := True;

    -- Current sequence characters: Esc + 3
    Seq_Max_Length : constant := 4;
    Seq : Uu.Unbounded_Array;

    -- After this delay from Esc, we give up
    Seq_Delay   : constant Duration := 0.25;
    Escape_Time : Ada.Calendar.Time;
    function Ls2U (Str : String) return Unicode_Sequence
             renames Language.String_To_Unicode;
    -- Supported sequences
    Arrow_Left_Seq    : constant Unicode_Sequence := Ls2U ("[D");
    Arrow_Right_Seq   : constant Unicode_Sequence := Ls2U ("[C");
    Arrow_Up_Seq      : constant Unicode_Sequence := Ls2U ("[A");
    Arrow_Down_Seq    : constant Unicode_Sequence := Ls2U ("[B");
    Home_Seq          : constant Unicode_Sequence := Ls2U ("[1~");
    End_Seq           : constant Unicode_Sequence := Ls2U ("[4~");
    Home1_Seq         : constant Unicode_Sequence := Ls2U ("OH");
    End1_Seq          : constant Unicode_Sequence := Ls2U ("OF");
    Delete_Seq        : constant Unicode_Sequence := Ls2U ("[3~");
    Page_Up_Seq       : constant Unicode_Sequence := Ls2U ("[5~");
    Page_Down_Seq     : constant Unicode_Sequence := Ls2U ("[6~");
    Insert_Seq        : constant Unicode_Sequence := Ls2U ("[2~");

    -- Copy Buf and move to end of line
    procedure Update is
    begin
      Console.Set_Col (First_Col);
      Console.Erase_End_Line;
      Txt := History.Buf;
      Sys_Calls.Put_Output (Language.Unicode_To_String (Uu.To_Array (Txt)));
      Ind := Uu.Length (Txt) + 1;
    end Update;

    -- Store Txt in history
    procedure Store is
    begin
      -- Don't store empty line
      if Uu.Length (Txt) = 0 then
        return;
      end if;
      History.Buf := Txt;
      History.Add;
      At_Last := True;
    end Store;

    -- Some usefull definitions
    Esc : constant Unicode_Number := Language.Char_To_Unicode (Latin_1.Esc);
    Del : constant Unicode_Number := Language.Char_To_Unicode (Latin_1.Del);
    Space : constant Unicode_Number := Language.Char_To_Unicode (' ');
    Uu_Null : constant Uu.Unbounded_Array := Uu.Null_Unbounded_Array;
    function Uu_Is_Null (Str : Uu.Unbounded_Array) return Boolean is
      use type Uu.Unbounded_Array;
    begin
      return Str = Uu_Null;
    end Uu_Is_Null;

    -- Insert and put a wide character
    function Insert_Put (U : in Unicode_Number) return Boolean is
      use type Unicode_Sequence;
    begin
      if not Insert_Mode then
        -- Overwrite mode
        if Ind <= Txt.Length then
          -- Replace current position by U
          Txt.Replace_Element (Ind, U);
        else
          -- Append U
          Txt.Append (U);
        end if;
      else
        -- Insert U at current position and move 1 right
        Txt := Uu.To_Unbounded_Array (
                  Uu.Slice (Txt, 1, Ind - 1)
                & U
                & Uu.Slice (Txt, Ind, Uu.Length(Txt)));
      end if;
      -- Move 1 right and redisplay
      Ind := Ind + 1;
      Sys_Calls.Put_Output (Language.Unicode_To_String (
                             Uu.Slice (Txt, Ind - 1, Uu.Length(Txt))));
      -- Move cursor at proper position
      for I in 1 .. Uu.Length(Txt) - Ind + 1 loop
        Console.Left;
      end loop;
      -- Detect completion of input
      if Uu.Length (Txt) = Max then
        Store;
        return True;
      else
        return False;
      end if;
    end Insert_Put;

    -- Add a character. Store it until a sequence or flush
    -- return true if end of get
    function Add (U : Unicode_Number) return Boolean is
      Saved_Searching : Boolean;
      C : Character;
      use type Unicode_Sequence;
    begin
      -- Simplistic treatment when stdin/out is not a tty
      if not Stdio_Is_A_Tty then
        Uu.Append (Txt, U);
        -- Done when control character or buffer full
        return U < Space or else Uu.Length(Txt) = Max;
      end if;

      -- Optim: Set C to character of W or to Nul
      if U <= Del then
        C := Language.Unicode_To_Char (U);
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
          if not Uu_Is_Null (Seq) then
            Store;
            Uu.Append (Txt, Esc);
            return True;
          end if;
          if Ind = 1 then
            Sys_Calls.Put_Output (Bell);
          else
            -- Move one left, shift tail
            Ind := Ind - 1;
            Txt := Uu.To_Unbounded_Array (
                      Uu.Slice (Txt, 1, Ind - 1)
                    & Uu.Slice (Txt, Ind + 1, Uu.Length(Txt)));
            Console.Left;
            Console.Delete;
          end if;
        when Latin_1.Ht =>
          -- Search
          if not Uu_Is_Null (Seq) then
            Store;
            Uu.Append (Txt, Esc);
            return True;
          end if;
          if Uu.Length (Txt) = 0 then
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
          if not Uu_Is_Null (Seq) then
            Store;
            Uu.Append (Txt, Esc);
            return True;
          end if;
          Seq := Uu.To_Unbounded_Array (U);
          Escape_Time := Ada.Calendar.Clock;
        when ' ' .. '~' =>
          if Uu_Is_Null (Seq) then
            -- Valid ASCII character
            return Insert_Put (U);
          else
            -- Add C in sequence try to find one
            Uu.Append (Seq, U);
            declare
              Str : constant Unicode_Sequence
                  := Uu.Slice (Seq, 2, Uu.Length (Seq));
            begin
              if Str = Arrow_Left_Seq then
                -- Left if not at first
                if Ind = 1 then
                  Sys_Calls.Put_Output (Bell);
                else
                  Ind := Ind - 1;
                  Console.Left;
                end if;
                Seq := Uu_Null;
              elsif Str = Arrow_Right_Seq then
                -- Right if not at Last
                if Ind = Uu.Length (Txt) + 1 then
                  Sys_Calls.Put_Output (Bell);
                else
                  Ind := Ind + 1;
                  Console.Right;
                end if;
                Seq := Uu_Null;
              elsif Str = Home_Seq
              or else Str = Home1_Seq then
                -- Home
                Ind := 1;
                Console.Set_Col(First_Col);
                Seq := Uu_Null;
              elsif Str = End_Seq
              or else Str = End1_Seq then
                -- End
                Ind := Uu.Length(Txt) + 1;
                Console.Set_Col(First_Col + Ind - 1);
                Seq := Uu_Null;
              elsif Str = Delete_Seq then
                -- Del
                if Ind /= Uu.Length (Txt) + 1 then
                  -- Move shift tail
                  Txt := Uu.To_Unbounded_Array (
                          Uu.Slice (Txt, 1, Ind - 1)
                        & Uu.Slice (Txt, Ind + 1, Uu.Length(Txt)));
                  Console.Delete;
                end if;
                Seq := Uu_Null;
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
                Seq := Uu_Null;
              elsif Str = Arrow_Down_Seq then
                -- Down
                if not At_Last then
                  -- If we have just stored a new string. Down should ignore
                  History.Next;
                  Update;
                end if;
                Seq := Uu_Null;
              elsif Str = Page_Up_Seq then
                -- Page Up
                History.First;
                At_Last := False;
                Update;
                Seq := Uu_Null;
              elsif Str = Page_Down_Seq then
                -- Page Down
                History.Last;
                At_Last := False;
                Update;
                Seq := Uu_Null;
              elsif Str = Insert_Seq then
                -- Insert
                Insert_Mode := not Insert_Mode;
                Seq := Uu_Null;
              -- No sequence identified
              elsif Uu.Length(Seq) = Seq_Max_Length then
                -- Sequence is full and will not be recognized
                Seq := Uu_Null;
                Store;
                Uu.Append (Txt, Esc);
                return True;
              elsif Uu.Length(Txt) + Uu.Length(Seq) = Max then
                -- Not enough final space to store sequence
                Seq := Uu_Null;
                Store;
                Uu.Append (Txt, Esc);
                return True;
              -- else U is stored in Seq and we return False
              end if;
            end;
          end if;
        when Latin_1.Nul =>
          -- Non ASCII (UTF-8) character
          if Uu_Is_Null (Seq) then
            return Insert_Put (U);
          else
            -- Drop
            Seq := Uu_Null;
            Store;
            Uu.Append (Txt, Esc);
            return True;
          end if;
        when others =>
          -- Other char: ASCII control (< ' ') or UTF-8 (> Del)
          Store;
          if not Uu_Is_Null (Seq) then
            Uu.Append (Txt, Esc);
          else
            Uu.Append (Txt, U);
          end if;
          if U <= Del or else Uu.Length(Txt) = Max then
            -- Any ASCII control char, or UTF-8 reaching max length
            return True;
          end if;
      end case;
      return False;
    end Add;

    function Flush return Boolean is
      use type Ada.Calendar.Time;
    begin
      if not Uu_Is_Null (Seq)
      and then Ada.Calendar.Clock - Escape_Time > Seq_Delay then
        -- Client wants to flush and Esc is getting old
        Uu.Append (Txt, Esc);
        Seq := Uu_Null;
        return True;
      end if;
      return False;
    end Flush;

    procedure Clear is
    begin
      Seq := Uu_Null;
      Txt := Uu_Null;
      Ind := 1;
      if Stdio_Is_A_Tty then
        Console.Set_Col(First_Col);
      end if;
    end Clear;

    function Get return Unicode_Sequence is
    begin
      Uu.Append (Txt, Seq);
      Seq := Uu_Null;
      return Uu.To_Array (Txt);
    end Get;

    -- Read current content of buffer and current cursor col
    function Read_Buffer return Unicode_Sequence is
      Res : Uu.Unbounded_Array;
      use type Uu.Unbounded_Array;
    begin
      Res := Txt & Seq;
      return Uu.To_Array (Res);
    end Read_Buffer;

    function Read_Col return Positive is
    begin
      return Ind;
    end Read_Col;

  end Line;

  -- Our callback
  function Fd_Callback (Fd : Sys_Calls.File_Desc;
                        Read : Boolean) return Boolean is
    pragma Unreferenced (Read);
    Status : Sys_Calls.Get_Status_List;
    C : Character;
    Str : String (1 .. Utf_8.Max_Chars);
    U : Unicode_Number;
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
              U := Language.Char_To_Unicode (C);
            else
              -- First wide char of string to wide conversion
              U := Language.String_To_Unicode (Str(1 .. Len))(1);
            end if;
            Len := 0;
            exit when Line.Add (U);
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

    declare
      Seq : constant Unicode_Sequence := Line.Get;
    begin
      -- Cb may call Put, so clear Line first
      Line.Clear;
      Console.Set_Col (1);
      Insert_Mode := True;
      Result := Cb (Language.Unicode_To_String (Seq));
    end;
    return Result;

  end Fd_Callback;

  -- Set asynchronous mode for stdin
  -- User callback is called when Max_Chars characters are entered
  --  or at each new line
  -- Set null callback to restore normal behaviour
  procedure Set_Async (User_Callback : in User_Callback_Access := null;
                       Max_Chars : in Max_Chars_Range := 1;
                       First_Col : in Max_Chars_Range := 1) is
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
        if Max_Chars /= 0 then
          Max := Max_Chars;
        else
          Max := Max_Chars_Range'Last;
        end if;
        Line.Init;
        Line.Clear;
        if Active and then Cb = null then
          Event_Mng.Add_Fd_Callback (Sys_Calls.Stdin, True,
                                     Fd_Callback'Access);
        end if;
        Cb := User_Callback;
        Async_Stdin.First_Col := First_Col;
        Console.Set_Col (First_Col);
      else
        raise Error;
      end if;
    end if;
  end Set_Async;

  function Is_Set return Boolean is
  begin
    return Cb /= null;
  end Is_Set;

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

  -- Clear internal buffer of pending characters
  procedure Clear is
  begin
    Line.Clear;
  end Clear;

  -- By default the input is in insert mode and reset in insert after
  --  each input (before calling user callback) and when calling Set_Async
  -- This operation allows setting the next input to overwrite mode
  procedure Overwrite is
  begin
    Insert_Mode := False;
  end Overwrite;

  -- Set an internal callback (overwritting any Async callback set)
  --  and wait until it is called, then unset it and return the result
  -- The buffer and callback
  Get_Line_Buffer : As.U.Asu_Us;
  function Get_Line_Cb (Buffer : String) return Boolean is
  begin
    Get_Line_Buffer.Set (Buffer);
    return True;
  end Get_Line_Cb;

  function Get_Line (Max_Chars : Max_Chars_Range := 0;
                     First_Col : Max_Chars_Range := 1) return String is
  begin
    -- Set callback
    Get_Line_Buffer.Set_Null;
    Set_Async (Get_Line_Cb'Access, Max_Chars, First_Col);
    -- Wait until it is called
    loop
      Event_Mng.Wait (Event_Mng.Infinite_Ms);
      exit when not Get_Line_Buffer.Is_Null;
    end loop;
    -- Unset callback
    Set_Async;
    return Get_Line_Buffer.Image;
  end Get_Line;

  -- Strip last character if Str if it is a control char (before space)
  function Strip_Last_Control (Str : String) return String is
  begin
    if Str'Length = 0 or else Str(Str'Last) >= ' ' then
      return Str;
    else
      return Str (Str'First .. Str'Last - 1);
    end if;
  end Strip_Last_Control;

  -- Put on stdout when in async
  procedure Put_Out (Str : in String) is
    Result : Boolean;
    pragma Unreferenced (Result);
  begin
    if Cb /= null then
      Result := Sys_Calls.Set_Blocking (Sys_Calls.Stdin, True);
      declare
        Buf : constant Unicode_Sequence := Line.Read_Buffer;
      begin
        if Buf'Length /= 0 then
          Sys_Calls.New_Line_Output;
        end if;
        Sys_Calls.Put_Output (Str);
        if Buf'Length /= 0 then
          -- Put buffer, move cursor
          Sys_Calls.Put_Output (Language.Unicode_To_String (Buf));
          Console.Set_Col (Line.Read_Col);
        end if;
      end;
      Result := Sys_Calls.Set_Blocking (Sys_Calls.Stdin, False);
    else
      Sys_Calls.Put_Output (Str);
    end if;
  end Put_Out;

  procedure Put_Line_Out (Str : in String) is
  begin
    Put_Out (Str & Ada.Characters.Latin_1.Lf);
  end Put_Line_Out;

  procedure New_Line_Out is
  begin
    Put_Out ("" & Ada.Characters.Latin_1.Lf);
  end New_Line_Out;

  procedure Flush_Out is
    Result : Boolean;
    pragma Unreferenced (Result);
  begin
    if Cb /= null then
      Result := Sys_Calls.Set_Blocking (Sys_Calls.Stdin, True);
      Sys_Calls.Flush_Output;
      Result := Sys_Calls.Set_Blocking (Sys_Calls.Stdin, False);
    else
      Sys_Calls.Flush_Output;
    end if;
  end Flush_Out;

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

  procedure Flush_Err is
    Result : Boolean;
    pragma Unreferenced (Result);
  begin
    if Cb /= null then
      Result := Sys_Calls.Set_Blocking (Sys_Calls.Stdin, True);
      Sys_Calls.Flush_Error;
      Result := Sys_Calls.Set_Blocking (Sys_Calls.Stdin, False);
    else
      Sys_Calls.Flush_Error;
    end if;
  end Flush_Err;

end Async_Stdin;

