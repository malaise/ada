with Ada.Text_Io, Ada.Calendar;
with My_Io; use My_Io;
-- Simulation of the game of Hanoi towers

procedure Hanoi is

  type Typ_Support is (A, B, C);
  Origin, Destination : Typ_Support;
  package Support_Io is new Ada.Text_Io.Enumeration_Io(Typ_Support);
  Same_Supports : exception;

  subtype Typ_No_Disk is Positive;
  Nb_Disks : Typ_No_Disk;

  Nb_Moves : Long_Long_Integer;
  Overflow_Count : exception;

  Response : Character;
  Trace : Boolean;

  Time : Ada.Calendar.Time;
  Seconds : Ada.Calendar.Day_Duration;
  package Duration_Io is new Ada.Text_Io.Fixed_Io(Duration);

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
    Put_Line("PROBLEM: Two axes are identical.");
    raise Same_Supports;
  end Find_Third;

  procedure Move (
   Origin, Third, Destination : in Typ_Support;
   No_Disk : in Typ_No_Disk) is
  begin
    begin
      Move(Origin, Destination, Third, Typ_No_Disk'Pred(No_Disk));
    exception
      when Constraint_Error =>
        null;
    end;

    if Trace then
      Put("Move the disk ");
      Put(No_Disk);
      Put(" from ");
      Support_Io.Put(Origin);
      Put(" to ");
      Support_Io.Put(Destination);
      Put_Line(".");
    end if;

    begin
      Nb_Moves := Long_Long_Integer'Succ(Nb_Moves);
    exception
      when others =>
        raise Overflow_Count;
    end;

    begin
      Move(Third, Origin, Destination, Typ_No_Disk'Pred(No_Disk));
    exception
      when Constraint_Error =>
        null;
    end;

  end Move;

begin -- Hanoi
  Put_Line("Problem of the Hanoi towers:");
  loop
    New_Line;

    loop
      begin
        Put("Enter the origin axis (a, b, c) ? ");
        Support_Io.Get(Origin);
        New_Line;
        exit;
      exception
        when others =>
          Skip_Line;
          Put_Line("ERROR of input, try again.");
      end;
    end loop;

    loop
      begin
        Put("Enter the destination axis (a, b, c) ? ");
        Support_Io.Get(Destination);
        if (Destination = Origin) then
          raise Same_Supports;
        end if;
        New_Line;
        exit;
      exception
        when Same_Supports =>
          Skip_Line;
          Put_Line("ERROR, both axes are equal, try again.");
        when others =>
          Skip_Line;
          Put_Line("ERREUR of input, try again.");
      end;
    end loop;

    loop
      begin
        Put("Enter the number of disks (at least 1) ? ");
        Get(Nb_Disks);
        New_Line;
        exit;
      exception
        when others =>
          Skip_Line;
          Put_Line("ERROR, try again.");
      end;
    end loop;

    loop
      begin
        Put("Do you want a detailed trace (Y/N) ? ");
        Get(Response);
        if (Response /= 'y') and then (Response /= 'Y') and then
           (Response /= 'n') and then (Response /= 'N') then
          raise Constraint_Error;
        end if;
        New_Line;
        Trace := ((Response = 'y') or else (Response = 'Y'));
        exit;
      exception
        when others =>
          Skip_Line;
          Put_Line("ERROR, try again.");
      end;
    end loop;

    New_Line;

    begin
      Nb_Moves := 0;
      Time := Ada.Calendar.Clock;
      Move (Origin, Find_Third(Origin, Destination), Destination, Nb_Disks);
      Seconds := Ada.Calendar."-"(Ada.Calendar.Clock, Time);

      New_Line;
      Put("Transfer from ");
      Put(Nb_Disks);
      Put(" disks from ");
      Support_Io.Put(Origin);
      Put(" to ");
      Support_Io.Put(Destination);
      Put_Line(".");

      Put("Operation done in ");
      Put(Nb_Moves);
      Put_Line(" moves ");
      Put(" and in ");
      Duration_Io.Put(Seconds);
      Put_Line(" seconds.");

    exception
      when Overflow_Count =>
        Put("Overflow of the counter of movements. try with less disks.")
          ;
    end;

  end loop;

end Hanoi;

