-- Test timed queue
with Ada.Calendar;
with Argument, Aski, Queues.Timed, Basic_Proc, Gets, Lower_Char, Mixed_Str;
procedure T_Timeq is

  procedure Help is
  begin
    Basic_Proc.Put_Line_Output ("Commands are: ivd | o | e | | rv | c | x | a");
    Basic_Proc.Put_Line_Output ("i => push value (0 .. 9) for duration d");
    Basic_Proc.Put_Line_Output ("p => pop next value");
    Basic_Proc.Put_Line_Output ("e => expire values");
    Basic_Proc.Put_Line_Output ("r => read value if not expired");
    Basic_Proc.Put_Line_Output ("c => clear queue");
    Basic_Proc.Put_Line_Output ("x => exit");
    Basic_Proc.Put_Line_Output ("a => run autotest");
  end Help;

  subtype Val_Range is Natural range 0 .. 9;
  package Val_Queue is new Queues.Timed (5, Val_Range);
  Vals : Val_Queue.Timed_Type;

  function Equal (V1, V2 : Val_Range) return Boolean is
  begin
    return V1 = V2;
  end Equal;

  -- Read a value, return True if read
  function  Read (I : Val_Range) return Boolean is
    V : Val_Range;
    B : Boolean;
  begin
    Val_Queue.Read (Vals, I, Equal'Unrestricted_Access, V, B);
    if B then
      Basic_Proc.Put_Line_Output ("Read " & V'Img);
    else
      Basic_Proc.Put_Line_Output ("Not data matches " & I'Img);
    end if;
    return B;
  end Read;
  procedure Read (I : in Val_Range) is
    Dummy : Boolean;
  begin
    Dummy := Read (I);
  end Read;

  procedure Autotest is
    V : Val_Range;
    Expect_Error : exception;
    -- Read a value
    -- Report error if result is not the Expected
    procedure Read (I : in Val_Range; Expected : in Boolean) is
      R : Boolean;
    begin
      R := Read (I);
      if R /= Expected then
        Basic_Proc.Put_Line_Error ("Error. Reading item " & I'Img
            & " is " & Mixed_Str (R'Img) & " while expecting"
            & Mixed_Str (Expected'Img) & ".");
        raise Expect_Error;
      end if;
    end Read;

    -- Pop all queue until empty
    -- Report error if number of poped is not the Expected
    procedure Pop (Expected : in Natural) is
      D : Boolean;
      N : Natural := 0;
    begin
      loop
        if N mod 2 = 0 then
          -- Alternatvely call procedure and function
          V := Val_Queue.Pop (Vals, D);
        else
          Val_Queue.Pop (Vals, V, D);
        end if;
        exit when not D;
        Basic_Proc.Put_Line_Output ("Got" & V'Img);
        N := N + 1;
      end loop;
      if N /= Expected then
        Basic_Proc.Put_Line_Error ("Error. Got" & N'Img
            & " item while expecting" & Expected'Img & ".");
        raise Expect_Error;
      end if;
    end Pop;
  begin
    -- Step 1, test auto expire on push
    Basic_Proc.Put_Line_Output ("Putting 11 22 44 55 66 and waiting 3 seconds");
    Val_Queue.Push (Vals, 1, 1.0);
    Val_Queue.Push (Vals, 2, 2.0);
    Val_Queue.Push (Vals, 4, 4.0);
    Val_Queue.Push (Vals, 5, 5.0);
    Val_Queue.Push (Vals, 6, 6.0);
    delay 3.0;
    Basic_Proc.Put_Line_Output ("Popping first without expire");
    Val_Queue.Pop (Vals, V);
    Basic_Proc.Put_Line_Output ("Got" & Val_Range'Image(V));
    -- Expires 1, 2 and 4 and adds 6
    Basic_Proc.Put_Line_Output ("Putting 77 then popping all");
    Val_Queue.Push (Vals, 7, (0, 7.0));
    -- Expect 4, 5, 6 and 7
    Pop (4);
    -- Step 2, test auto expire on get
    Basic_Proc.Put_Line_Output ("Putting 11 22 44 55 88 and waiting 3 seconds");
    Val_Queue.Push (Vals, 1, 1.0);
    Val_Queue.Push (Vals, 2, 2.0);
    Val_Queue.Push (Vals, 4, 4.0);
    Val_Queue.Push (Vals, 5, 5.0);
    Val_Queue.Push (Vals, 8, 8.0);
    delay 3.0;
    Basic_Proc.Put_Line_Output ("Reading 1, 2, 4, 5, 8 and 5 again");
    for I in 1 .. 5 loop
      Read (I, I > 3);
    end loop;
    Read (5, True);
    -- Step 3, test explicit expire
    Basic_Proc.Put_Line_Output ("Putting 11 44 and waiting 3 seconds");
    Val_Queue.Push (Vals, 1, (0, 1.0));
    Val_Queue.Push (Vals, 4, (0, 4.0));
    delay 3.0;
    Basic_Proc.Put_Line_Output ("Expiring then popping all");
    Val_Queue.Expire (Vals);
    Pop (2);
    -- Step 4, test clear
    Basic_Proc.Put_Line_Output ("Putting 11 and 66, cleaning then popping all");
    Val_Queue.Push (Vals, 1, (0, 1.0));
    Val_Queue.Push (Vals, 6, (0, 6.0));
    Val_Queue.Clear (Vals);
    Pop (0);
    Basic_Proc.Put_Line_Output ("End of auto test.");
  end Autotest;


  Str : String (1 .. 255);
  Len : Natural;
  C : Character;
  V : Val_Range;
  D : Ada.Calendar.Day_Duration;
begin
   if Argument.Get_Nbre_Arg /= 0 then
      Basic_Proc.Put_Line_Error ("ERROR: No argument supported.");
      Basic_Proc.Put_Line_Error ("Try ""h"" at command prompt.");
      Basic_Proc.Set_Error_Exit_Code;
     return;
   end if;
   Basic_Proc.Put_Line_Output (
      "Insert(Value, Duration) Pop Read(Value) Expire Clear eXit Autotest> ");
  loop
    Basic_Proc.Put_Output ("Ivd | P | Rv | E | C | X | A ? ");
    Basic_Proc.Get_Line (Str, Len);
    if Len = 0 then
      C := Aski.Nul;
    else
      C := Lower_Char (Str(1));
    end if;
    begin
      case C is
        when 'i' =>
          -- Insert
          -- Parse value and duration
          if Len = 3 then
            begin
              V := Val_Range'Value(Str(2 .. 2));
              D := Duration(Gets.Get_Int_Float (Str(3 .. Len)));
              Val_Queue.Push (Vals, V, (0, D));
            exception
              when Val_Queue.Timed_Full =>
                Basic_Proc.Put_Line_Output ("EXCEPTION Timed_Full");
            end;
          else
            raise Constraint_Error;
          end if;
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
          if Len = 2 then
            V := Val_Range'Value(Str(2 .. 2));
            Read (V);
          else
            raise Constraint_Error;
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
        when Aski.Nul =>
          null;
        when others =>
          Help;
      end case;
    exception
      when Constraint_Error =>
        Basic_Proc.Put_Line_Output ("Invalid value or delay");
    end;
  end loop;

exception
  when Basic_Proc.End_Error =>
    null;
  when others =>
    Basic_Proc.Set_Error_Exit_Code;
    raise;
end T_Timeq;

