with MY_IO, RND, DOS, CALENDAR;
use MY_IO;
procedure STAMP is

  pragma PRIORITY (5);

  I : NATURAL := 0;

  task ST is
    pragma PRIORITY (20);
  end ST;

  task SCAN is
    entry START;
    pragma PRIORITY (10);
  end SCAN;

  task body ST is
    use CALENDAR;
    GAP : constant DURATION := 1.0;
    NEXT_EVENT : TIME;
  begin
    NEXT_EVENT := CLOCK + GAP;
    loop
      delay NEXT_EVENT - CLOCK;
      DOS.SOUND;
      NEW_LINE;
      PUT (I);
      PUT_LINE ("           Stamp");
      I := 0;
      NEXT_EVENT := NEXT_EVENT + GAP;
    end loop;
  end ST;

  task body SCAN is
    MAX_WIN : constant POSITIVE := 5;
  begin
    accept START;
    loop
      I := I+ 1;
      PUT (I,2); NEW_LINE;
      delay RND.DUR_RANDOM(0.0, 0.1);
    end loop;
  end SCAN;

begin
  SCAN.START;
end STAMP;
