with Ada.Calendar;
with Basic_Proc, Integer_Image, Dur_Image, Long_Image;
use Basic_Proc;
-- Simulation of the game of Hanoi towers

procedure Hanoi is

  type Typ_Support is (A, B, C);
  Origin, Destination : Typ_Support;
  Same_Supports : exception;

  subtype Typ_No_Disk is Positive;
  Nb_Disks : Typ_No_Disk;

  Nb_Moves : Long_Long_Integer;
  Overflow_Count : exception;

  Response : Character;
  Trace : Boolean;

  Time : Ada.Calendar.Time;
  Seconds : Ada.Calendar.Day_Duration;

  function Find_Third (First_Suport, Second_Support : Typ_Support)
           return Typ_Support is
  begin
    case First_Suport is
      when A =>
        case Second_Support is
          when B =>
            return C;
          when C =>
            return B;
          when others =>
            null;
        end case;
      when B =>
        case Second_Support is
          when C =>
            return A;
          when A =>
            return C;
          when others =>
            null;
        end case;
      when C =>
        case Second_Support is
          when A =>
            return B;
          when B =>
            return A;
          when others =>
            null;
        end case;
    end case;
    Put_Line_Output ("PROBLEM: Two axes are identical.");
    raise Same_Supports;
  end Find_Third;

  procedure Move (
   Origin, Third, Destination : in Typ_Support;
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
      Put_Output (Integer_Image (No_Disk));
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
  loop
    New_Line_Output;

    loop
      begin
        Put_Output ("Enter the origin axis (a, b, c) ? ");
        Origin := Typ_Support'Value (Get_Line);
        New_Line_Output;
        exit;
      exception
        when others =>
          Put_Line_Output ("ERROR of input, try again.");
      end;
    end loop;

    loop
      begin
        Put_Output ("Enter the destination axis (a, b, c) ? ");
        Destination := Typ_Support'Value (Get_Line);
        if Destination = Origin then
          raise Same_Supports;
        end if;
        New_Line_Output;
        exit;
      exception
        when Same_Supports =>
          Put_Line_Output ("ERROR, both axes are equal, try again.");
        when others =>
          Put_Line_Output ("ERREUR of input, try again.");
      end;
    end loop;

    loop
      begin
        Put_Output("Enter the number of disks (at least 1) ? ");
        Nb_Disks := Typ_No_Disk'Value (Get_Line);
        New_Line_Output;
        exit;
      exception
        when others =>
          Put_Line_Output ("ERROR, try again.");
      end;
    end loop;

    loop
      begin
        Put_Output ("Do you want a detailed trace (Y/N) ? ");
        declare
          Str : constant String := Get_Line;
        begin
          if Str'Length = 1 then
            Response := Str(1);
          else
            Response := ' ';
          end if;
        end;
        if Response /= 'y' and then Response /= 'Y' and then
           Response /= 'n' and then Response /= 'N' then
          raise Constraint_Error;
        end if;
        New_Line_Output;
        Trace := Response = 'y' or else Response = 'Y';
        exit;
      exception
        when others =>
          Skip_Line;
          Put_Line_Output ("ERROR, try again.");
      end;
    end loop;

    New_Line_Output;

    begin
      Nb_Moves := 0;
      Time := Ada.Calendar.Clock;
      Move (Origin, Find_Third(Origin, Destination), Destination, Nb_Disks);
      Seconds := Ada.Calendar."-"(Ada.Calendar.Clock, Time);

      New_Line_Output;
      Put_Output ("Transfer from ");
      Put_Output (Integer_Image(Nb_Disks));
      Put_Output (" disks from ");
      Put_Output (Origin'Img);
      Put_Output (" to ");
      Put_Output (Destination'Img);
      Put_Line_Output (".");

      Put_Output ("Operation done in ");
      Put_Output (Long_Image(Nb_Moves));
      Put_Line_Output (" moves ");
      Put_Output (" and in ");
      Put_Output (Dur_Image (Seconds, 3, False));
      Put_Line_Output (" seconds.");

    exception
      when Overflow_Count =>
        Put_Output (
            "Overflow of the counter of movements. try with less disks.")
          ;
    end;

  end loop;

end Hanoi;

