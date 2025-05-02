with Ada.Calendar;
with Basic_Proc, Images, Long_Longs, Argument;
use Basic_Proc;
-- Simulation of the game of Hanoi towers

procedure Hanoi is

  type Typ_Axis is (A, B, C);
  Origin, Destination : Typ_Axis;

  subtype Typ_No_Disk is Positive;
  Nb_Disks : Typ_No_Disk;

  Nb_Moves : Long_Longs.Ll_Integer;
  Overflow_Count : exception;

  Trace : Boolean;

  Time : Ada.Calendar.Time;
  Seconds : Ada.Calendar.Day_Duration;

  procedure Usage is
  begin
    Put_Line_Output ("Usage: " & Argument.Get_Program_Name
                   & " <axis> <axis> <number> [ <trace> ]");
    Put_Line_Output ("  <axis>   ::= a | b | c");
    Put_Line_Output ("  <number> ::= <positive>");
    Put_Line_Output ("  <trace>  ::= -trace");
  end Usage;

  Arg_Error : exception;
  procedure Error (Msg : in String) is
  begin
    Put_Line_Error ("ERROR: " & Msg & ".");
    Usage;
    Basic_Proc.Set_Error_Exit_Code;
    raise Arg_Error;
  end Error;

  function Find_Third (First_Axis, Second_Axis : Typ_Axis) return Typ_Axis is
  begin
    case First_Axis is
      when A =>
        case Second_Axis is
          when B =>
            return C;
          when C =>
            return B;
          when others =>
            null;
        end case;
      when B =>
        case Second_Axis is
          when C =>
            return A;
          when A =>
            return C;
          when others =>
            null;
        end case;
      when C =>
        case Second_Axis is
          when A =>
            return B;
          when B =>
            return A;
          when others =>
            null;
        end case;
    end case;
    Put_Line_Error ("ERROR: Two axes are identical.");
    raise Constraint_Error;
  end Find_Third;

  procedure Move (
   Origin, Third, Destination : in Typ_Axis;
   No_Disk : in Typ_No_Disk) is
  begin
    begin
      Move (Origin, Destination, Third, Typ_No_Disk'Pred(No_Disk));
    exception
      when Constraint_Error =>
        null;
    end;

    if Trace then
      Put_Output ("Move the disk ");
      Put_Output (Images.Integer_Image (No_Disk));
      Put_Output (" from ");
      Put_Output (Origin'Img);
      Put_Output (" to ");
      Put_Output (Destination'Img);
      Put_Line_Output (".");
    end if;

    begin
      Nb_Moves := Long_Long_Integer'Succ(Nb_Moves);
    exception
      when others =>
        raise Overflow_Count;
    end;

    begin
      Move (Third, Origin, Destination, Typ_No_Disk'Pred(No_Disk));
    exception
      when Constraint_Error =>
        null;
    end;

  end Move;

begin -- Hanoi
  Put_Line_Output ("Problem of the Hanoi towers:");
  if Argument.Get_Nbre_Arg > 4 then
    Error ("Invalid arguments");
  end if;

  begin
    Origin      := Typ_Axis'Value (Argument.Get_Parameter (Occurence => 1));
    Destination := Typ_Axis'Value (Argument.Get_Parameter (Occurence => 2));
    Nb_Disks    := Typ_No_Disk'Value (Argument.Get_Parameter (Occurence => 3));
    if Destination = Origin then
      Error ("ERROR: Both axes are identical.");
    end if;
    if Argument.Get_Nbre_Arg = 3 then
      Trace := False;
    else
      if Argument.Get_Parameter (Occurence => 4) = "-trace" then
        Trace := True;
      else
        Error ("Invalid arguments");
      end if;
    end if;
  exception
    when Arg_Error =>
      raise;
    when others =>
      Error ("Invalid arguments");
  end;

  -- Recursive moves
  Nb_Moves := 0;
  Time := Ada.Calendar.Clock;
  Move (Origin, Find_Third(Origin, Destination), Destination, Nb_Disks);
  Seconds := Ada.Calendar."-"(Ada.Calendar.Clock, Time);

  New_Line_Output;
  Put_Output ("Transfer from ");
  Put_Output (Images.Integer_Image(Nb_Disks));
  Put_Output (" disks from ");
  Put_Output (Origin'Img);
  Put_Output (" to ");
  Put_Output (Destination'Img);
  Put_Line_Output (".");

  Put_Output ("Operation done in ");
  Put_Output (Long_Longs.Image (Nb_Moves));
  Put_Line_Output (" moves ");
  Put_Output (" and in ");
  Put_Output (Images.Dur_Image (Seconds, 3, False));
  Put_Line_Output (" seconds.");

exception
  when Overflow_Count =>
    Put_Line_Error (
      "ERROR: Overflow of the counter of movements. try with less disks.");
    Basic_Proc.Set_Error_Exit_Code;
  when Arg_Error =>
    null;
  when others =>
    Basic_Proc.Set_Error_Exit_Code;
end Hanoi;

