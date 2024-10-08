with Ada.Calendar;
with Event_Mng, Console, Dynamic_List, Environ,
     Unicode, Aski.Unicode, Utf_8, Language, As.U,
     Trace, Hexa_Utils, Sys_Calls, Key_Pressed;
package body Async_Stdin is

  -- The user callback
  Cb : User_Callback_Access := null;
  -- Do we echo inputs
  Echo : Boolean := True;
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

    -- Clear history
    procedure Clear_History;

  end Line;

  package body Line is
    Bell : String renames Aski.Bel_S;
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

      -- Clear the history
      procedure Clear_List;
    end History;

    package Logger is new Trace.Basic_Logger ("Async_Stdin");

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
      function Search is new List_Mng.Search (Match);


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
      procedure Clear_Buff is
      begin
        Buf := Uu.Null_Unbounded_Array;
      end Clear_Buff;

      -- Movements
      procedure First is
      begin
        if List.Is_Empty then
          return;
        end if;
        List.Rewind;
        Copy_Current;
      end First;

      procedure Last is
      begin
        if List.Is_Empty then
          return;
        end if;
        List.Rewind (List_Mng.Prev);
        Copy_Current;
      end Last;

      procedure Next is
      begin
        if List.Is_Empty then
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
          Found := Search (List, Searched_Rec, List_Mng.Prev,
                           From => List_Mng.Absolute);
        else
          -- Search second occurence if current record matches
          Found := Search (List, Searched_Rec, List_Mng.Prev,
                           From => List_Mng.Skip_Current);
        end if;

        if Found then
          Copy_Current;
        else
          -- Restore Pos
          List.Move_At (Pos);
          Clear_Buff;
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
        List.Rewind (List_Mng.Prev, False);
        List.Insert (R);
      end Add;

      procedure Clear_List is
      begin
        List.Delete_List;
      end Clear_List;

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

    -- Max len of a sequence of characters: Esc + 6
    Seq_Max_Length : constant := 6;
    -- Current sequence characters
    Seq : Uu.Unbounded_Array;

    -- After this delay from Esc, we give up
    Seq_Delay   : constant Duration := 0.25;
    Escape_Time : Ada.Calendar.Time;
    function S2U (S : String) return Unicode_Sequence
                              renames Aski.Unicode.Decode;
    -- Supported sequences
    Arrow_Left_Seq    : constant Unicode_Sequence := S2U ("[D");
    Arrow_Right_Seq   : constant Unicode_Sequence := S2U ("[C");
    Arrow_Up_Seq      : constant Unicode_Sequence := S2U ("[A");
    Arrow_Down_Seq    : constant Unicode_Sequence := S2U ("[B");
    Home_Seq          : constant Unicode_Sequence := S2U ("[1~");
    End_Seq           : constant Unicode_Sequence := S2U ("[4~");
    Home1_Seq         : constant Unicode_Sequence := S2U ("[H");
    End2_Seq          : constant Unicode_Sequence := S2U ("[F");
    Home2_Seq         : constant Unicode_Sequence := S2U ("OH");
    End1_Seq          : constant Unicode_Sequence := S2U ("OF");
    Delete_Seq        : constant Unicode_Sequence := S2U ("[3~");
    Page_Up_Seq       : constant Unicode_Sequence := S2U ("[5~");
    Page_Down_Seq     : constant Unicode_Sequence := S2U ("[6~");
    Insert_Seq        : constant Unicode_Sequence := S2U ("[2~");
    Ctrl_Suppr_Seq    : constant Unicode_Sequence := S2U ("[3;5~");
    Shift_Suppr_Seq   : constant Unicode_Sequence := S2U ("[3;2~");
    -- Recognized and skipped sequences (all remaining combination of
    -- Ctrl, Shift, Arrows, Pg up/down, Home, End, Inser, Suppr...)
    Ctrl_Pgup_Seq        : constant Unicode_Sequence := S2U ("[5;5~");
    Ctrl_Pgdw_Seq        : constant Unicode_Sequence := S2U ("[6;5~");
    Ctrl_Left_Seq        : constant Unicode_Sequence := S2U ("[1;5D");
    Ctrl_Right_Seq       : constant Unicode_Sequence := S2U ("[1;5C");
    Ctrl_Up_Seq          : constant Unicode_Sequence := S2U ("[1;5A");
    Ctrl_Down_Seq        : constant Unicode_Sequence := S2U ("[1;5B");
    Shift_Left_Seq       : constant Unicode_Sequence := S2U ("[1;2D");
    Shift_Right_Seq      : constant Unicode_Sequence := S2U ("[1;2C");
    Shift_Up_Seq         : constant Unicode_Sequence := S2U ("[1;2A");
    Shift_Down_Seq       : constant Unicode_Sequence := S2U ("[1;2B");
    Ctrl_Shift_Left_Seq  : constant Unicode_Sequence := S2U ("[1;6D");
    Ctrl_Shift_Right_Seq : constant Unicode_Sequence := S2U ("[1;6C");
    Ctrl_Shift_Up_Seq    : constant Unicode_Sequence := S2U ("[1;6A");
    Ctrl_Shift_Down_Seq  : constant Unicode_Sequence := S2U ("[1;6B");
    Ctrl_Shift_Pgup_Seq  : constant Unicode_Sequence := S2U ("[5;6~");
    Ctrl_Shift_Pgdw_Seq  : constant Unicode_Sequence := S2U ("[6;6~");
    Ctrl_Home_Seq        : constant Unicode_Sequence := S2U ("[1;5H");
    Ctrl_End_Seq         : constant Unicode_Sequence := S2U ("[1;5F");
    Shift_Home_Seq       : constant Unicode_Sequence := S2U ("[1;2H");
    Shift_End_Seq        : constant Unicode_Sequence := S2U ("[1;2F");
    Ctrl_Shift_Home_Seq  : constant Unicode_Sequence := S2U ("[1;6H");
    Ctrl_Shift_End_Seq   : constant Unicode_Sequence := S2U ("[1;6F");
    Ctrl_Insert_Seq      : constant Unicode_Sequence := S2U ("[2;5~");
    Ctrl_Shift_Suppr_Seq : constant Unicode_Sequence := S2U ("[3;6~");

    -- Copy Buf and move to end of line
    procedure Update is
    begin
      Txt := History.Buf;
      if Echo then
        Console.Set_Col (First_Col);
        Console.Erase_End_Line;
        Basic_Proc.Put_Output_Again (Language.Unicode_To_String (Txt.To_Array));
      end if;
      Ind := Txt.Length + 1;
    end Update;

    -- Store Txt in history
    procedure Store is
    begin
      -- Don't store empty line
      if Txt.Is_Null then
        return;
      end if;
      History.Buf := Txt;
      History.Add;
      At_Last := True;
    end Store;

    -- Some usefull definitions
    Esc   : Unicode_Number renames Aski.Unicode.Esc_U;
    Del   : Unicode_Number renames Aski.Unicode.Del_U;
    Space : Unicode_Number renames Aski.Unicode.Spc_U;
    Uu_Null : constant Uu.Unbounded_Array := Uu.Null_Unbounded_Array;

    -- Insert and put a wide character
    function Insert_Put (U : in Unicode_Number) return Boolean is
      use type Uu.Unbounded_Array;
    begin
      if Insert_Mode then
        -- Insert U at current position and move 1 right
        Txt := Txt.Unb_Slice (1, Ind - 1) & U & Txt.Unb_Slice (Ind, Txt.Length);
      else
        -- Overwrite mode
        if Ind <= Txt.Length then
          -- Replace current position by U
          Txt.Replace_Element (Ind, U);
        else
          -- Append U
          Txt.Append (U);
        end if;
      end if;
      -- Move 1 right and redisplay
      Ind := Ind + 1;
      if Echo then
        Basic_Proc.Put_Output_Again (Language.Unicode_To_String (
                               Txt.Slice (Ind - 1, Txt.Length)));
        -- Move cursor at proper position
        for I in 1 .. Txt.Length - Ind + 1 loop
          Console.Left;
        end loop;
      end if;
      -- Detect completion of input
      if Txt.Length = Max then
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
      use type Unicode_Sequence, Uu.Unbounded_Array;
    begin
      Logger.Log (16#20#, "Got " & Hexa_Utils.Image (U));
      -- Simplistic treatment when stdin/out is not a tty
      if not Stdio_Is_A_Tty then
        Txt.Append (U);
        -- Done when control character or buffer full
        return U < Space or else Txt.Length = Max;
      end if;

      -- Optim: Set C to character of W or to Nul
      C := (if U <= Del then Aski.Unicode.Encode (U) else Aski.Nul);

      -- Save current searching status
      Saved_Searching := Searching;
      -- Default, we cancel search on each input except on escape
      Searching := False;
      case C is
        when Aski.Bs | Aski.Del =>
          -- Backspace
          if not Seq.Is_Null then
            Store;
            Txt.Append (Esc);
            return True;
          end if;
          if Ind = 1 then
            if Echo then
              Basic_Proc.Put_Output_Again (Bell);
            end if;
          else
            -- Move one left, shift tail
            Ind := Ind - 1;
            Txt := Txt.Unb_Slice (1, Ind - 1)
                 & Txt.Unb_Slice (Ind + 1, Txt.Length);
            if Echo then
              Console.Left;
              Console.Delete;
            end if;
          end if;
        when Aski.Ht =>
          -- Search
          if not Seq.Is_Null then
            Store;
            Txt.Append (Esc);
            return True;
          end if;
          if Txt.Is_Null then
            return False;
          end if;
          if not Saved_Searching then
            History.Buf := Txt;
          end if;
          if History.Search (not Saved_Searching) then
            Update;
            At_Last := False;
          elsif Echo then
            Basic_Proc.Put_Output_Again (Bell);
          end if;
          Searching := True;
        when Aski.Esc =>
          -- Escape, validate previous escape
          if not Seq.Is_Null then
            Store;
            Txt.Append (Esc);
            return True;
          end if;
          Seq := Uu.To_Unbounded_Array (U);
          Escape_Time := Ada.Calendar.Clock;
        when ' ' .. '~' =>
          if Seq.Is_Null then
            -- Valid ASCII character
            return Insert_Put (U);
          end if;
          -- Add C in sequence try to find one
          Seq.Append (U);
          declare
            Str : constant Unicode_Sequence
                := Seq.Slice (2, Seq.Length);
          begin
            if Str = Arrow_Left_Seq then
              -- Left if not at first
              if Ind = 1 and then Echo then
                Basic_Proc.Put_Output_Again (Bell);
              else
                Ind := Ind - 1;
                if Echo then
                  Console.Left;
                end if;
              end if;
              Seq := Uu_Null;
            elsif Str = Arrow_Right_Seq then
              -- Right if not at Last
              if Ind = Txt.Length + 1 and then Echo then
                Basic_Proc.Put_Output_Again (Bell);
              else
                Ind := Ind + 1;
                if Echo then
                  Console.Right;
                end if;
              end if;
              Seq := Uu_Null;
            elsif Str = Home_Seq
            or else Str = Home1_Seq
            or else Str = Home2_Seq then
              -- Home
              Ind := 1;
              if Echo then
                Console.Set_Col(First_Col);
              end if;
              Seq := Uu_Null;
            elsif Str = End_Seq
            or else Str = End1_Seq
            or else Str = End2_Seq then
              -- End
              Ind := Txt.Length + 1;
              if Echo then
                Console.Set_Col(First_Col + Ind - 1);
              end if;
              Seq := Uu_Null;
            elsif Str = Delete_Seq then
              -- Del
              if Ind /= Txt.Length + 1 then
                -- Move shift tail
                Txt := Txt.Unb_Slice (1, Ind - 1)
                     & Txt.Unb_Slice (Ind + 1, Txt.Length);
                if Echo then
                  Console.Delete;
                end if;
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
            elsif Str = Ctrl_Suppr_Seq then
              -- Clear line
              Ind := 1;
              Txt.Set_Null;
              if Echo then
                Console.Set_Col (First_Col);
                Console.Erase_End_Line;
              end if;
              Seq := Uu_Null;
            elsif Str = Shift_Suppr_Seq then
              -- Clear to end
              if Ind /= Txt.Length + 1 then
                Txt.Delete (Ind, Txt.Length);
                if Echo then
                  Console.Erase_End_Line;
                end if;
              end if;
              Seq := Uu_Null;

            elsif   Str = Ctrl_Pgup_Seq
            or else Str = Ctrl_Pgdw_Seq
            or else Str = Ctrl_Left_Seq
            or else Str = Ctrl_Right_Seq
            or else Str = Ctrl_Up_Seq
            or else Str = Ctrl_Down_Seq
            or else Str = Shift_Left_Seq
            or else Str = Shift_Right_Seq
            or else Str = Shift_Up_Seq
            or else Str = Shift_Down_Seq
            or else Str = Ctrl_Shift_Left_Seq
            or else Str = Ctrl_Shift_Right_Seq
            or else Str = Ctrl_Shift_Up_Seq
            or else Str = Ctrl_Shift_Down_Seq
            or else Str = Ctrl_Shift_Pgup_Seq
            or else Str = Ctrl_Shift_Pgdw_Seq
            or else Str = Ctrl_Home_Seq
            or else Str = Ctrl_End_Seq
            or else Str = Shift_Home_Seq
            or else Str = Shift_End_Seq
            or else Str = Ctrl_Shift_Home_Seq
            or else Str = Ctrl_Shift_End_Seq
            or else Str = Ctrl_Insert_Seq
            or else Str = Ctrl_Shift_Suppr_Seq then
              -- Drop this sequence
              Seq := Uu_Null;

            -- From now: No sequence identified
            elsif Seq.Length = Seq_Max_Length then
              -- Sequence is not recognized
              if Logger.Debug_On then
                -- Dump unrecognized sequence
                Logger.Log_Debug ("Unrecognized sequence");
                for I in 1 .. Seq.Length loop
                  Logger.Log_Debug ("  " & Hexa_Utils.Image (Seq.Element(I)));
                end loop;
              end if;
              Seq := Uu_Null;
              Store;
              Txt.Append (Esc);
              return True;
            elsif Txt.Length + Seq.Length = Max then
              -- Not enough final space to store sequence
              Seq := Uu_Null;
              Store;
              Txt.Append (Esc);
              return True;
            -- else U is stored in Seq and we return False
            end if;
          end;
        when Aski.Nul =>
          -- Non ASCII (UTF-8) character
          if Seq.Is_Null then
            return Insert_Put (U);
          else
            -- Drop
            Seq := Uu_Null;
            Store;
            Txt.Append (Esc);
            return True;
          end if;
        when others =>
          -- Other char: ASCII control (< ' ') or UTF-8 (> Del)
          Store;
          if not Seq.Is_Null then
            Txt.Append (Esc);
          else
            Txt.Append (U);
          end if;
          if U <= Del or else Txt.Length = Max then
            -- Any ASCII control char, or UTF-8 reaching max length
            return True;
          end if;
      end case;
      return False;
    end Add;

    function Flush return Boolean is
      use type Ada.Calendar.Time;
    begin
      if not Seq.Is_Null
      and then Ada.Calendar.Clock - Escape_Time > Seq_Delay then
        -- Client wants to flush and Esc is getting old
        Txt.Append (Esc);
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
      if Stdio_Is_A_Tty and then Echo then
        Console.Set_Col(First_Col);
      end if;
    end Clear;

    function Get return Unicode_Sequence is
    begin
      Txt.Append (Seq);
      Seq := Uu_Null;
      return Txt.To_Array;
    end Get;

    -- Read current content of buffer and current cursor col
    function Read_Buffer return Unicode_Sequence is
      Res : Uu.Unbounded_Array;
      use type Uu.Unbounded_Array;
    begin
      Res := Txt & Seq;
      return Res.To_Array;
    end Read_Buffer;

    function Read_Col return Positive is (Ind);

    procedure Clear_History is
    begin
      History.Clear_List;
    end Clear_History;

  end Line;

  -- Our callback
  function Fd_Callback (Fd : Sys_Calls.File_Desc;
                        Unused_Read : Boolean) return Boolean is
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
      if Stdio_Is_A_Tty then
        C := Key_Pressed.Get_Key;
      else
        Sys_Calls.Get_Immediate (Fd, Status, C);
        case Status is
          when Sys_Calls.Closed | Sys_Calls.Error =>
            C := Key_Pressed.Error_Key;
          when Sys_Calls.None =>
            C := Key_Pressed.No_Key;
          when Sys_Calls.Got =>
            null;
        end case;
      end if;

      case C is
        when Key_Pressed.No_Key =>
          -- No more char
          exit when Line.Flush;
          return False;
        when Key_Pressed.Error_Key =>
          -- Call Cb with empty txt
          Line.Clear;
          exit;
        when others =>
          -- Valid char
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
            -- Optim for single characters
            --  or First wide char of string to wide conversion
            U := (if Len = 1 then Language.Char_To_Unicode (C)
                  else Language.String_To_Unicode (Str(1 .. Len))(1) );
            Len := 0;
            exit when Line.Add (U);
          end if;
      end case;
    end loop;

    -- Fix tty output
    if Stdio_Is_A_Tty
    and then Echo
    and then (C = Aski.Cr
      or else C = Aski.Lf) then
      Basic_Proc.New_Line_Output_Again;
    end if;

    declare
      Seq : constant Unicode_Sequence := Line.Get;
    begin
      -- Cb may call Put, so clear Line first
      Line.Clear;
      if Stdio_Is_A_Tty and then Echo then
        Console.Set_Col (1);
      end if;
      Insert_Mode := True;
      begin
        Result := Cb (Language.Unicode_To_String (Seq));
      exception
        when others =>
          raise Cb_Error;
      end;
    end;
    return Result;

  end Fd_Callback;

  -- Set asynchronous mode for stdin
  -- User callback is called when Max_Chars characters are entered
  --  or at each new line
  -- Set null callback to restore normal behaviour
  procedure Set_Async (User_Callback : in User_Callback_Access := null;
                       Max_Chars : in Max_Chars_Range := 1;
                       Left_Col  : in Max_Chars_Range := 1;
                       Echo      : in Boolean := True) is
    Result : Boolean;
    use type Sys_Calls.File_Desc_Kind_List;
  begin
    Stdio_Is_A_Tty := Sys_Calls.File_Desc_Kind (Sys_Calls.Stdin) = Sys_Calls.Tty
             and then Sys_Calls.File_Desc_Kind (Sys_Calls.Stdout) = Sys_Calls.Tty;
    -- Check if restore
    if User_Callback = null then
      if Cb = null then
        -- No change, remain sync
        return;
      end if;
      -- Switch to sync
      Cb := null;
      if Stdio_Is_A_Tty then
        Key_Pressed.Close;
      else
        Result := Sys_Calls.Set_Blocking (Sys_Calls.Stdin, True);
      end if;
      if Active then
        Event_Mng.Del_Fd_Callback (Sys_Calls.Stdin, True);
      end if;
    else
      if Cb = null then
        -- Switch to async
        if Stdio_Is_A_Tty then
          Key_Pressed.Open;
          Result := True;
        else
          Result := Sys_Calls.Set_Blocking (Sys_Calls.Stdin, False);
        end if;
      else
        -- No change, remain async
        Result := True;
      end if;
      if Result then
        Max := (if Max_Chars /= 0 then Max_Chars else Max_Chars_Range'Last);
        Line.Init;
        Line.Clear;
        if Active and then Cb = null then
          Event_Mng.Add_Fd_Callback (Sys_Calls.Stdin, True,
                                     Fd_Callback'Access);
        end if;
        Cb := User_Callback;
        First_Col := Left_Col;
        Async_Stdin.Echo := Echo;
        if Stdio_Is_A_Tty and then Echo then
          Console.Set_Col (First_Col);
        end if;
      else
        raise Error;
      end if;
    end if;
  end Set_Async;

  function Is_Set return Boolean is (Cb /= null);

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

  function Is_Active return Boolean is (Active);

  -- Clear internal buffer of pending characters
  procedure Clear_Pending is
    Status : Sys_Calls.Get_Status_List;
    Result, Dummy : Boolean;
    C : Character;
    use type Sys_Calls.Get_Status_List;
  begin
    Line.Clear;
    if Cb /= null then
      -- Remove pending characters from stdin
      Result := Sys_Calls.Set_Blocking (Sys_Calls.Stdin, False);
      if not Result then
        -- Cannot set non blocking => give up
        return;
      end if;
      loop
        Sys_Calls.Get_Immediate (Sys_Calls.Stdin, Status, C);
        exit when Status /= Sys_Calls.Got;
      end loop;
    end if;
    Dummy := Sys_Calls.Set_Blocking (Sys_Calls.Stdin, True);
  end Clear_Pending;

  -- Clear the history
  procedure Clear_History is
  begin
    Line.Clear_History;
  end Clear_History;

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
    -- Signal error condition
    if Buffer'Length = 0 then
      raise Io_Error;
    end if;
    return True;
  end Get_Line_Cb;

  function Get_Line (Max_Chars : Max_Chars_Range := 0;
                     First_Col : Max_Chars_Range := 1;
                     Echo      : Boolean := True) return String is
  begin
    -- Set callback
    Get_Line_Buffer.Set_Null;
    Set_Async (Get_Line_Cb'Access, Max_Chars, First_Col, Echo);
    -- Wait until an event
    Event_Mng.Wait (Event_Mng.Infinite_Ms);
    -- No data got?
    if Get_Line_Buffer.Is_Null then
      raise Io_Error;
    end if;
    -- Unset callback and done
    Set_Async;
    return Get_Line_Buffer.Image;
  exception
    when others =>
      Set_Async;
      raise;
  end Get_Line;

  -- Strip last character if Str if it is a control char (before space)
  function Strip_Last_Control (Str : String) return String is
    (if Str'Length = 0 or else Str(Str'Last) >= ' ' then Str
     else Str (Str'First .. Str'Last - 1));

  -- Put on stdout when in async
  procedure Put_Out (Str : in String) is
    Dummy_Result : Boolean;
  begin
    if Cb = null then
      Basic_Proc.Put_Output_Again (Str);
    else
      Dummy_Result := Sys_Calls.Set_Blocking (Sys_Calls.Stdin, True);
      declare
        Buf : constant Unicode_Sequence := Line.Read_Buffer;
      begin
        if Stdio_Is_A_Tty and then Buf'Length /= 0 and then Echo then
          Basic_Proc.New_Line_Output_Again;
        end if;
        Basic_Proc.Put_Output_Again (Str);
        if Stdio_Is_A_Tty and then Buf'Length /= 0 and then Echo then
          -- Put buffer, move cursor
          Basic_Proc.Put_Output_Again (Language.Unicode_To_String (Buf));
          Console.Set_Col (Line.Read_Col);
        end if;
      end;
      Dummy_Result := Sys_Calls.Set_Blocking (Sys_Calls.Stdin, False);
    end if;
  end Put_Out;

  procedure Put_Line_Out (Str : in String) is
  begin
    Put_Out (Str & Aski.Lf);
  end Put_Line_Out;

  procedure New_Line_Out is
  begin
    Put_Out ("" & Aski.Lf);
  end New_Line_Out;

  procedure Flush_Out is
    Dummy_Result : Boolean;
  begin
    if Cb = null then
      Basic_Proc.Flush_Output_Again;
    else
      Dummy_Result := Sys_Calls.Set_Blocking (Sys_Calls.Stdin, True);
      Basic_Proc.Flush_Output;
      Dummy_Result := Sys_Calls.Set_Blocking (Sys_Calls.Stdin, False);
    end if;
  end Flush_Out;

  -- Put on stderr when in async
  procedure Put_Err (Str : in String) is
    Dummy_Result : Boolean;
  begin
    if Cb = null then
      Basic_Proc.Put_Error (Str);
    else
      Dummy_Result := Sys_Calls.Set_Blocking (Sys_Calls.Stdin, True);
      Basic_Proc.Put_Error (Str);
      Dummy_Result := Sys_Calls.Set_Blocking (Sys_Calls.Stdin, False);
    end if;
  end Put_Err;

  procedure Put_Line_Err (Str : in String) is
    Dummy_Result : Boolean;
  begin
    if Cb = null then
      Basic_Proc.Put_Line_Error (Str);
    else
      Dummy_Result := Sys_Calls.Set_Blocking (Sys_Calls.Stdin, True);
      Basic_Proc.Put_Line_Error (Str);
      Dummy_Result := Sys_Calls.Set_Blocking (Sys_Calls.Stdin, False);
    end if;
  end Put_Line_Err;

  procedure New_Line_Err is
    Dummy_Result : Boolean;
  begin
    if Cb = null then
      Basic_Proc.New_Line_Error;
    else
      Dummy_Result := Sys_Calls.Set_Blocking (Sys_Calls.Stdin, True);
      Basic_Proc.New_Line_Error;
      Dummy_Result := Sys_Calls.Set_Blocking (Sys_Calls.Stdin, False);
    end if;
  end New_Line_Err;

  procedure Flush_Err is
    Dummy_Result : Boolean;
  begin
    if Cb = null then
      Basic_Proc.Flush_Error;
    else
      Dummy_Result := Sys_Calls.Set_Blocking (Sys_Calls.Stdin, True);
      Basic_Proc.Flush_Error;
      Dummy_Result := Sys_Calls.Set_Blocking (Sys_Calls.Stdin, False);
    end if;
  end Flush_Err;

end Async_Stdin;

