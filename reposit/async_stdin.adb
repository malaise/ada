with Ada.Text_Io, Ada.Calendar;
with Event_Mng, Sys_Calls, Text_Handler, Console, Dynamic_List, Trace, Environ;
package body Async_Stdin is

  -- The user callback
  Cb : User_Callback_Access := null;
  -- Max len of result
  Max : Max_Chars_Range := 1;


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

    package History is
      -- Buffer for exchanges (in/out with caller)
      Buf : Text_Handler.Text (Max_Chars_Range'Last);

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

      -- Append Buf as new record
      procedure Add;
    end History;

    package body History is
      type Rec is record
         Len : Natural;
         Str : String (1 .. Max_Chars_Range'Last);
      end record;
      package List_Mng is new Dynamic_List (Rec);
      List : List_Mng.List_Type;
      function Match (Curr, Crit : Rec) return Boolean is
      begin
        -- Record in list starts like criteria
        return Curr.Len >= Crit.Len
        and then Curr.Str(1 .. Crit.Len) = Crit.Str(1 .. Crit.Len);
      end Match;
      procedure Search is new List_Mng.Safe_Search (Match);


      -- Searched pattern
      Searched_Rec : Rec;

      -- Copy current Rec in Buf
      procedure Copy_Current is
        R : Rec;
      begin
        List_Mng.Read (List, R, List_Mng.Current);
        Text_Handler.Set (Buf, R.Str(1 .. R.Len));
      end Copy_Current;

      -- Make a Rec from Buf
      procedure Copy_Buf (R : out Rec) is
      begin
        R.Len := Text_Handler.Length (Buf);
        R.Str(1 .. R.Len) := Text_Handler.Value (Buf);
      end Copy_Buf;

      -- Clear Buf
      procedure Clear is
      begin
        Text_Handler.Empty (Buf);
      end Clear;

      -- Movements
      procedure First is
      begin
        if List_Mng.Is_Empty (List) then
          Clear;
          return;
        end if;
        List_Mng.Rewind (List, List_Mng.Next);
        Copy_Current;
      end First;

      procedure Last is
      begin
        if List_Mng.Is_Empty (List) then
          Clear;
          return;
        end if;
        List_Mng.Rewind (List, List_Mng.Prev);
        Copy_Current;
      end Last;

      procedure Next is
      begin
        if List_Mng.Is_Empty (List) then
          Clear;
          return;
        end if;
        if List_Mng.Check_Move (List) then
          List_Mng.Move_To (List, List_Mng.Next);
        end if;
        Copy_Current;
      end Next;

      procedure Prev is
      begin
        if List_Mng.Is_Empty (List) then
          Clear;
          return;
        end if;
        if List_Mng.Check_Move (List, List_Mng.Prev) then
          List_Mng.Move_To (List, List_Mng.Prev);
        end if;
        Copy_Current;
      end Prev;

      function Search (Init : Boolean) return Boolean is
        Pos : Positive;
        Found : Boolean;
      begin
        if List_Mng.Is_Empty (List) then
          return False;
        end if;
        -- Save pos
        Pos := List_Mng.Get_Position (List);

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
          List_Mng.Move_To (List, List_Mng.Next, Pos - 1, False);
          Clear;
        end if;
        return Found;
      end Search;


      procedure Add is
        R : Rec;
      begin
        if List_Mng.List_Length (List) = History_Size then
          List_Mng.Rewind (List, List_Mng.Next);
          List_Mng.Delete (List);
        end if;
        -- Append at the end of list
        Copy_Buf (R);
        if not List_Mng.Is_Empty (List) then
          List_Mng.Rewind (List, List_Mng.Prev);
        end if;
        List_Mng.Insert (List, R);
      end Add;

    end History;

    procedure Init is
    begin
      Environ.Get_Pos ("Async_Stdin_History_Size", History.History_Size);
    end Init;

    -- Current line of text and cursor position in it
    Ind : Positive := 1;
    Txt : Text_Handler.Text (Max_Chars_Range'Last);

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
      if not Text_Handler.Empty (History.Buf) then
        Console.Set_Col (1);
        Console.Erase_Line;
        Text_Handler.Set (Txt, History.Buf);
        Ada.Text_Io.Put (Text_Handler.Value (Txt));
        Ind := Text_Handler.Length (Txt) + 1;
      end if;
    end Update;

    -- Store Txt in history
    procedure Store is
    begin
      Text_Handler.Set (History.Buf, Txt);
      History.Add;
      At_Last := True;
    end Store;

    function Add (C : Character) return Boolean is
       Saved_Searching : Boolean;
       use Text_Handler;
    begin
      -- Save current searching status
      Saved_Searching := Searching;
      -- Default, we cancel search on each input except on escape
      Searching := False;
      -- For debug
      -- Trace.Activate;
      -- Trace.Put (C & "->" & Integer'Image(Character'Pos(C)));
      case C is
        when Ascii.Bs | Ascii.Del =>
          -- Backspace
          if not Empty (Seq) then
            Store;
            Append (Txt, Ascii.Esc);
            return True;
          end if;
          if Ind = 1 then
            Ada.Text_Io.Put (Ascii.Bel);
          else
            -- Move one left, shift tail
            Ind := Ind - 1;
            Set (Txt, Value(Txt)(1 .. Ind - 1)
                    & Value(Txt)(Ind + 1 .. Length(Txt)));
            Console.Left;
            Console.Delete;
          end if;
        when Ascii.Ht =>
          -- Search
          if not Empty (Seq) then
            Store; 
            Append (Txt, Ascii.Esc);
            return True;
          end if;
          if Empty (Txt) then
            return False;
          end if;
          if not Saved_Searching then
            Text_Handler.Set (History.Buf, Txt);
          end if;
          if History.Search (not Saved_Searching) then
            Update;
            At_Last := False;
          else
            Ada.Text_Io.Put (Ascii.Bel);
          end if;
          Searching := True;
        when Ascii.Esc =>
          -- Escape, validate previous escape
          if not Empty (Seq) then
            Store;
            Append (Txt, Ascii.Esc);
            return True;
          end if;
          Set (Seq, C);
          Escape_Time := Ada.Calendar.Clock;
        when ' ' .. '~' =>
          if Empty (Seq) then
            -- Insert C at current position and move 1 right
            Set (Txt, Value(Txt)(1 .. Ind - 1)
                    & C
                    & Value(Txt)(Ind .. Length(Txt)));
            Ind := Ind + 1;
            Console.Save;
            Ada.Text_Io.Put (Value(Txt)(Ind - 1 .. Length(Txt)));
            Console.Restore;
            Console.Right;
            if Length(Txt) = Max then
              Store;
              return True;
            end if;
            return False;
          else
            -- Add C in sequence try to find one
            Append (Seq, C);
            declare
              Str : constant String := Value(Seq)(2 .. Length(Seq));
            begin
              if Str = Arrow_Left_Seq then
                -- Left if not at first
                if Ind = 1 then
                  Ada.Text_Io.Put (Ascii.Bel);
                else
                  Ind := Ind - 1;
                  Console.Left;
                end if;
                Empty (Seq);
              elsif Str = Arrow_Right_Seq then
                -- Right if not at Last
                if Ind = Length (Txt) + 1 then
                  Ada.Text_Io.Put (Ascii.Bel);
                else
                  Ind := Ind + 1;
                  Console.Right;
                end if;
                Empty (Seq);
              elsif Str = Home_Seq then
                -- Home
                Ind := 1;
                Console.Set_Col(1);
                Empty (Seq);
              elsif Str = End_Seq then
                -- End
                Ind := Length(Txt) + 1;
                Console.Set_Col(Ind);
                Empty (Seq);
              elsif Str = Delete_Seq then
                -- Del
                if Ind /= Length (Txt) + 1 then
                  -- Move shift tail
                  Set (Txt, Value(Txt)(1 .. Ind - 1)
                          & Value(Txt)(Ind + 1 .. Length(Txt)));
                  Console.Delete;
                end if;
                Empty (Seq);
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
                Empty (Seq);
              elsif Str = Arrow_Down_Seq then
                -- Down
                if not At_Last then
                  -- If we have just stored a new string. Down should ignore
                  History.Next;
                  Update;
                end if;
                Empty (Seq);
              elsif Str = Page_Up_Seq then
                -- Page Up
                History.First;
                At_Last := False;
                Update;
                Empty (Seq);
              elsif Str = Page_Down_Seq then
                -- Page Down
                History.Last;
                At_Last := False;
                Update;
                Empty (Seq);
              elsif Str = Insert_Seq then
                -- Discard Insert
                Empty (Seq);
              elsif Length(Txt) + Length(Seq) = Max then
                -- Not enough final space to store sequence
                Store;
                Append (Txt, Ascii.Esc);
                return True;
              end if;
            end;
          end if;
        when others =>
          -- Other char
          Store;
          if not Empty (Seq) then
            Append (Txt, Ascii.Esc);
          else
            Append (Txt, C);
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
        Text_Handler.Append (Txt, Ascii.Esc);
        Text_Handler.Empty (Seq);
        return True;
      end if;
      return False;
    end Flush;

    procedure Clear is
    begin
      Text_Handler.Empty (Seq);
      Text_Handler.Empty (Txt);
      Ind := 1;
      Console.Set_Col(1);
    end Clear;

    function Get return String is
    begin
      Text_Handler.Append (Txt, Seq);
      Text_Handler.Empty (Seq);
      return Text_Handler.Value (Txt) & Text_Handler.Value (Seq);
    end Get;

  end Line;

  -- Our callback
  function Fd_Callback (Fd : Sys_Calls.File_Desc;
                        Read : Boolean) return Boolean is
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

    Ada.Text_Io.New_Line;
    Result := Cb (Line.Get);
    Line.Clear;
    return Result;

  end Fd_Callback;

  -- Set asynchronous mode for stdin
  -- User callback is called when Max_Chars characters are entered
  --  or at each new line
  -- Set null callback to restore normal behaviour
  procedure Set_Async (User_Callback : in User_Callback_Access := null;
                       Max_Chars : in Max_Chars_Range := 1) is
    Result : Boolean;
    Stdin_Is_A_Tty : Boolean;
    use type  Sys_Calls.File_Desc_Kind_List;
  begin
    Stdin_Is_A_Tty := Sys_Calls.File_Desc_Kind (Sys_Calls.Stdin) = Sys_Calls.Tty;
    -- Check if restore
    if User_Callback = null then
      if Cb = null then
        return;
      else
        Cb := null;
        if Stdin_Is_A_Tty then
          Result := Sys_Calls.Set_Tty_Attr (Sys_Calls.Stdin, 
                                            Sys_Calls.Canonical);
        else
          Result := Sys_Calls.Set_Blocking (Sys_Calls.Stdin, True);
        end if;
        Event_Mng.Del_Fd_Callback (Sys_Calls.Stdin, True);
      end if;
    else
      if Cb = null then
        if Stdin_Is_A_Tty then
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
        Max := Max_Chars;
        Line.Init;
        Line.Clear;
        Event_Mng.Add_Fd_Callback (Sys_Calls.Stdin, True, Fd_Callback'Access);
      else
        raise Error;
      end if;
    end if;
  end Set_Async;

end Async_Stdin;

