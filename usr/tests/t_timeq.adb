-- Test timed queue
with Ada.Text_Io, Ada.Calendar;
with Argument, Queues.Timed, Sys_Calls, Get_Float;
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

  package Dur_Io is new Ada.Text_Io.Fixed_Io (Ada.Calendar.Day_Duration);

  subtype Val_Range is Natural range 0 .. 9;
  package Val_Queue is new Queues.Timed (5, Val_Range);

  procedure Autotest is
    V : Val_Range;
    -- Pop all queue until empty
    procedure Pop is
    begin
      loop
        begin
          Val_Queue.Pop (V);
        exception
          when Val_Queue.Timed_Empty => exit;
        end;
        Ada.Text_Io.Put_Line ("Got" & V'Img);
      end loop;
    end Pop;
  begin
    -- Step 1, test auto expire
    Ada.Text_Io.Put_Line ("Putting 11 22 33 44 55 and waiting 3 seconds");
    Val_Queue.Push (1, (0, 1.0));
    Val_Queue.Push (2, (0, 2.0));
    Val_Queue.Push (3, (0, 3.0));
    Val_Queue.Push (4, (0, 4.0));
    Val_Queue.Push (5, (0, 5.0));
    delay 3.0;
    Ada.Text_Io.Put_Line ("Getting first without expire");
    Ada.Text_Io.Put_Line ("Got" & Val_Range'Image(Val_Queue.Pop));
    Ada.Text_Io.Put_Line ("Putting 66 then getting all");
    Val_Queue.Push (6, (0, 6.0));
    Pop;
    -- Step 2, test explicit expire
    Ada.Text_Io.Put_Line ("Putting 11 33 and waiting 1 seconds");
    Val_Queue.Push (1, (0, 1.0));
    Val_Queue.Push (3, (0, 3.0));
    delay 1.0;
    Ada.Text_Io.Put_Line ("Expiring then getting all");
    Val_Queue.Expire;
    Pop;
    -- Step 3, test clear
    Ada.Text_Io.Put_Line ("Putting 66, cleaning then getting all");
    Val_Queue.Push (1, (0, 1.0));
    Val_Queue.Push (6, (0, 6.0));
    Val_Queue.Clear;
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
      C := Ascii.Nul;
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
          Val_Queue.Push (V, (0, D));
        exception
          when Val_Queue.Timed_Full =>
            Ada.Text_Io.Put_Line ("EXCEPTION Timed_Full");
        end;
      when 'g' =>
        begin
          Val_Queue.Pop (V);
          Ada.Text_Io.Put_Line ("Popped " & V'Img);
        exception
          when Val_Queue.Timed_Empty =>
            Ada.Text_Io.Put_Line ("EXCEPTION Timed_Empty");
        end;
      when 'e' =>
        Val_Queue.Expire;
      when 'c' =>
        Val_Queue.Clear;
      when 'x' =>
        exit;
      when 'a' =>
        -- Run autotest
        Autotest;
      when Ascii.Nul =>
        null;
      when others =>
        Usage;
    end case;
  end loop;

exception
  when Ada.Text_Io.End_Error =>
    null;
  when others =>
    Sys_Calls.Set_Error_Exit_Code;
    raise;
end T_Timeq;

