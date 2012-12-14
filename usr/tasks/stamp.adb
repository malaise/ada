with Ada.Calendar;
with Basic_Proc, Rnd, Console, Normal;
use Basic_Proc;
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
    use Ada.Calendar;
    Gap : constant Duration := 1.0;
    Next_Event : Time;
  begin
    Next_Event := Clock + Gap;
    loop
      delay Next_Event - Clock;
      Console.Sound;
      New_Line_Output;
      Put_Line_Output (Normal (I, 2) & "           Stamp");
      I := 0;
      Next_Event := Next_Event + Gap;
    end loop;
  end St;

  task body Scan is
  begin
    accept Start;
    loop
      I := I + 1;
      Put_Line_Output (Normal (I, 2));
      delay Rnd.Gen.Dur_Random(0.0, 0.1);
    end loop;
  end Scan;

begin
  Rnd.Gen.Randomize;
  Scan.Start;
end Stamp;
