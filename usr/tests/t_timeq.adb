-- Test timed queue
with Ada.Calendar, Ada.Characters.Latin_1;
with Argument, Queues.Timed, Basic_Proc, Get_Float, Lower_Char;
procedure T_Timeq is

  procedure Usage is
  begin
    Basic_Proc.Put_Line_Output ("Usage: " & Argument.Get_Program_Name
                        & "  ivd | o | e | | rv | c | x | a");
    Basic_Proc.Put_Line_Output ("i => push value (0 .. 9) for duration d");
    Basic_Proc.Put_Line_Output ("o => pop next value");
    Basic_Proc.Put_Line_Output ("e => expire values");
    Basic_Proc.Put_Line_Output ("r => read value if not expired");
    Basic_Proc.Put_Line_Output ("c => clear queue");
    Basic_Proc.Put_Line_Output ("x => exit");
    Basic_Proc.Put_Line_Output ("a => run autotest");
  end Usage;

  subtype Val_Range is Natural range 0 .. 9;
  package Val_Queue is new Queues.Timed (5, Val_Range);
  Vals : Val_Queue.Timed_Type;

  function Equal (V1, V2 : Val_Range) return Boolean is
  begin
    return V1 = V2;
  end Equal;

  procedure Read (I : Val_Range) is
    V : Val_Range;
    B : Boolean;
  begin
    Val_Queue.Read (Vals, I, Equal'Unrestricted_Access, V, B);
    if B then
      Basic_Proc.Put_Line_Output ("Read " & V'Img);
    else
      Basic_Proc.Put_Line_Output ("Not data matches " & I'Img);
    end if;
  end Read;

  procedure Autotest is
    V : Val_Range;
    -- Pop all queue until empty
    procedure Pop is
      D : Boolean;
    begin
      loop
        Val_Queue.Pop (Vals, V, D);
        exit when not D;
        Basic_Proc.Put_Line_Output ("Got" & V'Img);
      end loop;
    end Pop;
  begin
    -- Step 1, test auto expire on push
    Basic_Proc.Put_Line_Output ("Putting 11 22 33 44 55 and waiting 3 seconds");
    Val_Queue.Push (Vals, 1, 1.0);
    Val_Queue.Push (Vals, 2, 2.0);
    Val_Queue.Push (Vals, 3, 3.0);
    Val_Queue.Push (Vals, 4, 4.0);
    Val_Queue.Push (Vals, 5, 5.0);
    delay 3.0;
    Basic_Proc.Put_Line_Output ("Popping first without expire");
    Val_Queue.Pop (Vals, V);
    Basic_Proc.Put_Line_Output ("Got" & Val_Range'Image(V));
    Basic_Proc.Put_Line_Output ("Putting 66 then popping all");
    Val_Queue.Push (Vals, 6, (0, 6.0));
    Pop;
    -- Step 2, test auto expire on get
    Basic_Proc.Put_Line_Output ("Putting 11 22 33 44 55 and waiting 3 seconds");
    Val_Queue.Push (Vals, 1, 1.0);
    Val_Queue.Push (Vals, 2, 2.0);
    Val_Queue.Push (Vals, 3, 3.0);
    Val_Queue.Push (Vals, 4, 4.0);
    Val_Queue.Push (Vals, 5, 5.0);
    delay 3.0;
    Basic_Proc.Put_Line_Output ("Reading 1, 2, 3, 4, 5 and 4 again");
    for I in 1 .. 5 loop
      Read (I);
    end loop;
    Read (4);
    -- Step 3, test explicit expire
    Basic_Proc.Put_Line_Output ("Putting 11 33 and waiting 1 seconds");
    Val_Queue.Push (Vals, 1, (0, 1.0));
    Val_Queue.Push (Vals, 3, (0, 3.0));
    delay 1.0;
    Basic_Proc.Put_Line_Output ("Expiring then popping all");
    Val_Queue.Expire (Vals);
    Pop;
    -- Step 4, test clear
    Basic_Proc.Put_Line_Output ("Putting 11 and 66, cleaning then popping all");
    Val_Queue.Push (Vals, 1, (0, 1.0));
    Val_Queue.Push (Vals, 6, (0, 6.0));
    Val_Queue.Clear (Vals);
    Pop;
    Basic_Proc.Put_Line_Output ("End of auto test.");
  end Autotest;


  Str : String (1 .. 10);
  Len : Natural;
  C : Character;
  V : Val_Range;
  D : Ada.Calendar.Day_Duration;
begin
    Basic_Proc.Put_Line_Output (
      "Insert(Value, Duration) Pop Get(Value) Expire Clear eXit Autotest> ");
  loop
    Basic_Proc.Put_Output ("Ivd | P | Rv | E | C | X | A ? ");
    Basic_Proc.Get_Line (Str, Len);
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
            Basic_Proc.Put_Line_Output ("EXCEPTION Timed_Full");
        end;
      when 'p' =>
        -- Pop
        begin
          Val_Queue.Pop (Vals, V);
          Basic_Proc.Put_Line_Output ("Popped " & V'Img);
        exception
          when Val_Queue.Timed_Empty =>
            Basic_Proc.Put_Line_Output ("EXCEPTION Timed_Empty");
        end;
      when 'r' =>
        -- Read
        -- Parse value
        if Len < 2 then
          raise Constraint_Error;
        end if;
        V := Val_Range'Value(Str(2 .. 2));
        Read (V);
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
  when Basic_Proc.End_Error =>
    null;
  when others =>
    Basic_Proc.Set_Error_Exit_Code;
    raise;
end T_Timeq;

