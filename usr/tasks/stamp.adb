with My_Io, Rnd, Dos, Calendar;
use My_Io;
procedure Stamp is

  pragma Priority (5);

  I : Natural := 0;

  task St is
    pragma Priority (20);
  end St;

  task Scan is
    entry Start;
    pragma Priority (10);
  end Scan;

  task body St is
    use Calendar;
    Gap : constant Duration := 1.0;
    Next_Event : Time;
  begin
    Next_Event := Clock + Gap;
    loop
      delay Next_Event - Clock;
      Dos.Sound;
      New_Line;
      Put (I);
      Put_Line ("           Stamp");
      I := 0;
      Next_Event := Next_Event + Gap;
    end loop;
  end St;

  task body Scan is
    Max_Win : constant Positive := 5;
  begin
    accept Start;
    loop
      I := I+ 1;
      Put (I,2); New_Line;
      delay Rnd.Dur_Random(0.0, 0.1);
    end loop;
  end Scan;

begin
  Scan.Start;
end Stamp;
