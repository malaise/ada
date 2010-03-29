with Ada.Calendar, Ada.Characters.Latin_1, Ada.Strings.Unbounded;
with Text_Handler, Event_Mng, Sys_Calls, Console, Dynamic_List, Environ;
package body Async_Stdin is

  package Unb renames Ada.Strings.Unbounded;

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

    -- Add a character. Store it until a sequence or flush
    -- return true if end of get
    function Add (C : Character) return Boolean;
    function Flush return Boolean;

    -- Get / clear result
    function Get return String;
    procedure Clear;

  end Line;

  package body Line is
    package Latin_1 renames Ada.Characters.Latin_1;
    Bell : constant String := Latin_1.Bel & "";

    package History is
      -- Buffer for exchanges (in/out with caller)
      Buf : Unb.Unbounded_String;

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

      -- Unb.Append Buf as new record
      procedure Add;
    end History;

    package body History is
      subtype Rec is Unb.Unbounded_String;
      package Dyn_List_Mng is new Dynamic_List (Rec);
      package List_Mng renames Dyn_List_Mng.Dyn_List;
      List : List_Mng.List_Type;
      function Match (Curr, Crit : Rec) return Boolean is
      begin
        -- Record in list starts like criteria
        return Unb.Length (Curr) >= Unb.Length (Crit)
        and then Unb.Slice (Curr, 1, Unb.Length (Crit)) = Unb.To_String (Crit);
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
        Buf := Unb.Null_Unbounded_String;
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
        List.Rewind (List_Mng.Prev);
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
        -- Unb.Append at the end of list
        Copy_Buf (R);
        if not List.Is_Empty then
          List.Rewind (List_Mng.Prev);
        end if;
        List.Insert (R);
      end Add;

    end History;

    procedure Init is
    begin
      Environ.Get_Pos ("Async_Stdin_History_Size", History.History_Size);
    end Init;

    -- Current line of text and cursor position in it
    Ind : Positive := 1;
    Txt : Unb.Unbounded_String;

    -- Are we searching (Tab, Tab, Tab...)
    Searching : Boolean := False;

    -- Have we just stored a new string, so prev shall be last
    At_Last : Boolean := True;

    -- Current sequence characters
    Seq : Text_Handler.Text (4);
    -- After this delay from Esc, we give up
    Seq_Delay   : constant Duration := 0.25;
    Escape_Time : Ada.Calendar.Time;
    -- Supported sequences
    Arrow_Left_Seq    : constant String := "[D";
    Arrow_Right_Seq   : constant String := "[C";
    Arrow_Up_Seq      : constant String := "[A";
    Arrow_Down_Seq    : constant String := "[B";
    Home_Seq          : constant String := "[1~";
    End_Seq           : constant String := "[4~";
    Delete_Seq        : constant String := "[3~";
    Page_Up_Seq       : constant String := "[5~";
    Page_Down_Seq     : constant String := "[6~";
    Insert_Seq        : constant String := "[2~";

    -- Copy Buf and move to end of line
    procedure Update is
    begin
      if Unb.Length (History.Buf) /= 0 then
        Console.Set_Col (1);
        Console.Erase_Line;
        Txt := History.Buf;
        Sys_Calls.Put_Output (Unb.To_String (Txt));
        Ind := Unb.Length (Txt) + 1;
      end if;
    end Update;

    -- Store Txt in history
    procedure Store is
    begin
      History.Buf := Txt;
      History.Add;
      At_Last := True;
    end Store;

    function Add (C : Character) return Boolean is
       Saved_Searching : Boolean;
    begin
      -- Simplistic treatment when stdin/out is not a tty
      if not Stdio_Is_A_Tty then
        Unb.Append (Txt, C);
        -- Done when extra character or buffer full
        return C not in ' ' .. '~' or else Unb.Length(Txt) = Max;
      end if;

      -- Save current searching status
      Saved_Searching := Searching;
      -- Default, we cancel search on each input except on escape
      Searching := False;
      case C is
        when Latin_1.Bs | Latin_1.Del =>
          -- Backspace
          if not Text_Handler.Empty (Seq) then
            Store;
            Unb.Append (Txt, Latin_1.Esc);
            return True;
          end if;
          if Ind = 1 then
            Sys_Calls.Put_Output (Bell);
          else
            -- Move one left, shift tail
            Ind := Ind - 1;
            Txt := Unb.To_Unbounded_String (
                      Unb.Slice (Txt, 1, Ind - 1)
                    & Unb.Slice (Txt, Ind + 1, Unb.Length(Txt)));
            Console.Left;
            Console.Delete;
          end if;
        when Latin_1.Ht =>
          -- Search
          if not Text_Handler.Empty (Seq) then
            Store;
            Unb.Append (Txt, Latin_1.Esc);
            return True;
          end if;
          if Unb.Length (Txt) = 0 then
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
          if not Text_Handler.Empty (Seq) then
            Store;
            Unb.Append (Txt, Latin_1.Esc);
            return True;
          end if;
          Text_Handler.Set (Seq, C);
          Escape_Time := Ada.Calendar.Clock;
        when ' ' .. '~' =>
          if Text_Handler.Empty (Seq) then
            -- Insert C at current position and move 1 right
            Txt := Unb.To_Unbounded_String (
                      Unb.Slice (Txt, 1, Ind - 1)
                    & C
                    & Unb.Slice (Txt, Ind, Unb.Length(Txt)));
            Ind := Ind + 1;
            Sys_Calls.Put_Output (Unb.Slice (Txt, Ind - 1, Unb.Length(Txt)));
            for I in 1 .. Unb.Length(Txt) - Ind + 1 loop
              Console.Left;
            end loop;
            if Unb.Length(Txt) = Max then
              Store;
              return True;
            end if;
            return False;
          else
            -- Add C in sequence try to find one
            Text_Handler.Append (Seq, C);
            declare
              Str : constant String
                  := Text_Handler.Value(Seq)(2 .. Text_Handler.Length(Seq));
            begin
              if Str = Arrow_Left_Seq then
                -- Left if not at first
                if Ind = 1 then
                  Sys_Calls.Put_Output (Bell);
                else
                  Ind := Ind - 1;
                  Console.Left;
                end if;
                Text_Handler.Empty (Seq);
              elsif Str = Arrow_Right_Seq then
                -- Right if not at Last
                if Ind = Unb.Length (Txt) + 1 then
                  Sys_Calls.Put_Output (Bell);
                else
                  Ind := Ind + 1;
                  Console.Right;
                end if;
                Text_Handler.Empty (Seq);
              elsif Str = Home_Seq then
                -- Home
                Ind := 1;
                Console.Set_Col(1);
                Text_Handler.Empty (Seq);
              elsif Str = End_Seq then
                -- End
                Ind := Unb.Length(Txt) + 1;
                Console.Set_Col(Ind);
                Text_Handler.Empty (Seq);
              elsif Str = Delete_Seq then
                -- Del
                if Ind /= Unb.Length (Txt) + 1 then
                  -- Move shift tail
                  Txt := Unb.To_Unbounded_String (
                          Unb.Slice (Txt, 1, Ind - 1)
                        & Unb.Slice (Txt, Ind + 1, Unb.Length(Txt)));
                  Console.Delete;
                end if;
                Text_Handler.Empty (Seq);
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
                Text_Handler.Empty (Seq);
              elsif Str = Arrow_Down_Seq then
                -- Down
                if not At_Last then
                  -- If we have just stored a new string. Down should ignore
                  History.Next;
                  Update;
                end if;
                Text_Handler.Empty (Seq);
              elsif Str = Page_Up_Seq then
                -- Page Up
                History.First;
                At_Last := False;
                Update;
                Text_Handler.Empty (Seq);
              elsif Str = Page_Down_Seq then
                -- Page Down
                History.Last;
                At_Last := False;
                Update;
                Text_Handler.Empty (Seq);
              elsif Str = Insert_Seq then
                -- Discard Insert
                Text_Handler.Empty (Seq);
              elsif Unb.Length(Txt) + Text_Handler.Length(Seq) = Max then
                -- Not enough final space to store sequence
                Store;
                Unb.Append (Txt, Latin_1.Esc);
                return True;
              end if;
            end;
          end if;
        when others =>
          -- Other char
          Store;
          if not Text_Handler.Empty (Seq) then
            Unb.Append (Txt, Latin_1.Esc);
          else
            Unb.Append (Txt, C);
          end if;
          return True;
      end case;
      return False;
    end Add;

    function Flush return Boolean is
      use type Ada.Calendar.Time;
    begin
      if not Text_Handler.Empty (Seq)
      and then Ada.Calendar.Clock - Escape_Time > Seq_Delay then
        -- Client wants to flush and Esc is getting old
        Unb.Append (Txt, Latin_1.Esc);
        Text_Handler.Empty (Seq);
        return True;
      end if;
      return False;
    end Flush;

    procedure Clear is
    begin
      Text_Handler.Empty (Seq);
      Txt := Unb.Null_Unbounded_String;
      Ind := 1;
      if Stdio_Is_A_Tty then
        Console.Set_Col(1);
      end if;
    end Clear;

    function Get return String is
    begin
      Unb.Append (Txt, Text_Handler.Value(Seq));
      Text_Handler.Empty (Seq);
      return Unb.To_String (Txt);
    end Get;

  end Line;

  -- Our callback
  function Fd_Callback (Fd : Sys_Calls.File_Desc;
                        Read : Boolean) return Boolean is
    pragma Unreferenced (Read);
    Status : Sys_Calls.Get_Status_List;
    C : Character;
    Result : Boolean;

  begin
    loop
      Sys_Calls.Get_Immediate (Fd, Status, C);
      case Status is
        when Sys_Calls.Got =>
          exit when Line.Add (C);
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

    Result := Cb (Line.Get);
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

