-- Test timed queue
with Ada.Text_Io, Ada.Calendar, Ada.Characters.Latin_1;
with Argument, Queues.Timed, Basic_Proc, Get_Float;
procedure T_Timeq is

  procedure Usage is
  begin
    Ada.Text_Io.Put_Line ("Usage: " & Argument.Get_Program_Name
                        & "  pvd | g | e | c | x | a");
    Ada.Text_Io.Put_Line ("p => push value (0 .. 9) for duration d");
    Ada.Text_Io.Put_Line ("g => get next value");
    Ada.Text_Io.Put_Line ("e => expire values");
    Ada.Text_Io.Put_Line ("c => clear queue");
    Ada.Text_Io.Put_Line ("x => exit");
    Ada.Text_Io.Put_Line ("a => run autotest");
  end Usage;

  subtype Val_Range is Natural range 0 .. 9;
  package Val_Queue is new Queues.Timed (5, Val_Range);
  Vals : Val_Queue.Timed_Type;

  procedure Autotest is
    V : Val_Range;
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
    -- Step 1, test auto expire
    Ada.Text_Io.Put_Line ("Putting 11 22 33 44 55 and waiting 3 seconds");
    Val_Queue.Push (Vals, 1, 1.0);
    Val_Queue.Push (Vals, 2, 2.0);
    Val_Queue.Push (Vals, 3, 3.0);
    Val_Queue.Push (Vals, 4, 4.0);
    Val_Queue.Push (Vals, 5, 5.0);
    delay 3.0;
    Ada.Text_Io.Put_Line ("Getting first without expire");
    Val_Queue.Pop (Vals, V);
    Ada.Text_Io.Put_Line ("Got" & Val_Range'Image(V));
    Ada.Text_Io.Put_Line ("Putting 66 then getting all");
    Val_Queue.Push (Vals, 6, (0, 6.0));
    Pop;
    -- Step 2, test explicit expire
    Ada.Text_Io.Put_Line ("Putting 11 33 and waiting 1 seconds");
    Val_Queue.Push (Vals, 1, (0, 1.0));
    Val_Queue.Push (Vals, 3, (0, 3.0));
    delay 1.0;
    Ada.Text_Io.Put_Line ("Expiring then getting all");
    Val_Queue.Expire (Vals);
    Pop;
    -- Step 3, test clear
    Ada.Text_Io.Put_Line ("Putting 11 and 66, cleaning then getting all");
    Val_Queue.Push (Vals, 1, (0, 1.0));
    Val_Queue.Push (Vals, 6, (0, 6.0));
    Val_Queue.Clear (Vals);
    Pop;
  end Autotest;


  Str : String (1 .. 10);
  Len : Natural;
  C : Character;
  V : Val_Range;
  D : Ada.Calendar.Day_Duration;
begin

  loop
    Ada.Text_Io.Put ("> ");
    Ada.Text_Io.Get_Line (Str, Len);
    if Len = 0 then
      C := Ada.Characters.Latin_1.Nul;
    else
      C := Str(1);
    end if;
    case C is
      when 'p' =>
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
      when 'g' =>
        begin
          Val_Queue.Pop (Vals, V);
          Ada.Text_Io.Put_Line ("Popped " & V'Img);
        exception
          when Val_Queue.Timed_Empty =>
            Ada.Text_Io.Put_Line ("EXCEPTION Timed_Empty");
        end;
      when 'e' =>
        Val_Queue.Expire (Vals);
      when 'c' =>
        Val_Queue.Clear (Vals);
      when 'x' =>
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

