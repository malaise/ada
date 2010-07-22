-- Test timed queue
with Ada.Text_Io, Ada.Calendar, Ada.Characters.Latin_1;
with Argument, Queues.Timed, Basic_Proc, Get_Float, Lower_Char;
procedure T_Timeq is

  procedure Usage is
  begin
    Ada.Text_Io.Put_Line ("Usage: " & Argument.Get_Program_Name
                        & "  ivd | o | e | | gv | c | x | a");
    Ada.Text_Io.Put_Line ("i => push value (0 .. 9) for duration d");
    Ada.Text_Io.Put_Line ("o => pop next value");
    Ada.Text_Io.Put_Line ("e => expire values");
    Ada.Text_Io.Put_Line ("g => get value if not expired");
    Ada.Text_Io.Put_Line ("c => clear queue");
    Ada.Text_Io.Put_Line ("x => exit");
    Ada.Text_Io.Put_Line ("a => run autotest");
  end Usage;

  subtype Val_Range is Natural range 0 .. 9;
  package Val_Queue is new Queues.Timed (5, Val_Range);
  Vals : Val_Queue.Timed_Type;

  function Equal (V1, V2 : Val_Range) return Boolean is
  begin
    return V1 = V2;
  end Equal;

  procedure Autotest is
    V : Val_Range;
    B : Boolean;
    -- Pop all queue until empty
    procedure Pop is
      D : Boolean;
    begin
      loop
        Val_Queue.Pop (Vals, V, D);
        exit when not D;
        Ada.Text_Io.Put_Line ("Got" & V'Img);
      end loop;
    end Pop;
  begin
    -- Step 1, test auto expire on push
    Ada.Text_Io.Put_Line ("Putting 11 22 33 44 55 and waiting 3 seconds");
    Val_Queue.Push (Vals, 1, 1.0);
    Val_Queue.Push (Vals, 2, 2.0);
    Val_Queue.Push (Vals, 3, 3.0);
    Val_Queue.Push (Vals, 4, 4.0);
    Val_Queue.Push (Vals, 5, 5.0);
    delay 3.0;
    Ada.Text_Io.Put_Line ("Popping first without expire");
    Val_Queue.Pop (Vals, V);
    Ada.Text_Io.Put_Line ("Got" & Val_Range'Image(V));
    Ada.Text_Io.Put_Line ("Putting 66 then popping all");
    Val_Queue.Push (Vals, 6, (0, 6.0));
    Pop;
    -- Step 2, test auto expire on get
    Ada.Text_Io.Put_Line ("Putting 11 22 33 44 55 and waiting 3 seconds");
    Val_Queue.Push (Vals, 1, 1.0);
    Val_Queue.Push (Vals, 2, 2.0);
    Val_Queue.Push (Vals, 3, 3.0);
    Val_Queue.Push (Vals, 4, 4.0);
    Val_Queue.Push (Vals, 5, 5.0);
    delay 3.0;
    Ada.Text_Io.Put_Line ("Getting 1, 2, 3, 4 and 5");
    for I in 1 .. 5 loop
      Val_Queue.Get (Vals, I, Equal'Unrestricted_Access, V, B);
      if B then
        Ada.Text_Io.Put_Line ("Got " & V'Img);
      else
        Ada.Text_Io.Put_Line ("Not data matches " & I'Img);
      end if;
    end loop;
    -- Step 3, test explicit expire
    Ada.Text_Io.Put_Line ("Putting 11 33 and waiting 1 seconds");
    Val_Queue.Push (Vals, 1, (0, 1.0));
    Val_Queue.Push (Vals, 3, (0, 3.0));
    delay 1.0;
    Ada.Text_Io.Put_Line ("Expiring then popping all");
    Val_Queue.Expire (Vals);
    Pop;
    -- Step 4, test clear
    Ada.Text_Io.Put_Line ("Putting 11 and 66, cleaning then popping all");
    Val_Queue.Push (Vals, 1, (0, 1.0));
    Val_Queue.Push (Vals, 6, (0, 6.0));
    Val_Queue.Clear (Vals);
    Pop;
  end Autotest;


  Str : String (1 .. 10);
  Len : Natural;
  C : Character;
  V : Val_Range;
  B : Boolean;
  D : Ada.Calendar.Day_Duration;
begin
    Ada.Text_Io.Put_Line (
      "Insert(Value, Duration) Pop Get(Value) Expire Clear eXit Autotest> ");
  loop
    Ada.Text_Io.Put ("Ivd | P | Gv | E | C | X | A ? ");
    Ada.Text_Io.Get_Line (Str, Len);
    if Len = 0 then
      C := Ada.Characters.Latin_1.Nul;
    else
      C := Lower_Char (Str(1));
    end if;
    case C is
      when 'i' =>
        -- Insert
        -- Parse value and duration
        if Len < 3 then
          raise Constraint_Error;
        end if;
        begin
          V := Val_Range'Value(Str(2 .. 2));
          D := Duration(Get_Float.Get_Float(Str(3 .. Len)));
          Val_Queue.Push (Vals, V, (0, D));
        exception
          when Val_Queue.Timed_Full =>
            Ada.Text_Io.Put_Line ("EXCEPTION Timed_Full");
        end;
      when 'p' =>
        -- Pop
        begin
          Val_Queue.Pop (Vals, V);
          Ada.Text_Io.Put_Line ("Popped " & V'Img);
        exception
          when Val_Queue.Timed_Empty =>
            Ada.Text_Io.Put_Line ("EXCEPTION Timed_Empty");
        end;
      when 'g' =>
        -- Get
        -- Parse value
        if Len < 2 then
          raise Constraint_Error;
        end if;
        V := Val_Range'Value(Str(2 .. 2));
        Val_Queue.Get (Vals, V, Equal'Unrestricted_Access, V, B);
        if B then
          Ada.Text_Io.Put_Line ("Got " & V'Img);
        else
          Ada.Text_Io.Put_Line ("Not data matches " & Str(2 .. 2));
        end if;
      when 'e' =>
        -- Expire
        Val_Queue.Expire (Vals);
      when 'c' =>
        -- Clear
        Val_Queue.Clear (Vals);
      when 'x' =>
        -- Exit
        exit;
      when 'a' =>
        -- Run autotest
        Autotest;
      when Ada.Characters.Latin_1.Nul =>
        null;
      when others =>
        Usage;
    end case;
  end loop;

exception
  when Ada.Text_Io.End_Error =>
    null;
  when others =>
    Basic_Proc.Set_Error_Exit_Code;
    raise;
end T_Timeq;

