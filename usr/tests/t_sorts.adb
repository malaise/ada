with MY_IO, SORTS, RND, NORMAL;
use MY_IO;
procedure T_SORTS is

  subtype INDEX is INTEGER range 1 .. 25;

  type ARR is array (INDEX range <>) of INTEGER;

  subtype D_ARR is ARR (INDEX);

  LAST : NATURAL range 0 .. INDEX'LAST;
  INIT, RES_BUL, RES_TAS, RES_RAP : D_ARR;

  package TAB_SORTS is new SORTS (INTEGER, INDEX, "<", ARR);
  OK : BOOLEAN;
  CURRENT_SORT : STRING (1 .. 3);
begin

  loop

    -- init of init and results
    LAST := RND.INT_RANDOM (0, INDEX'LAST);
    for I in 1 .. LAST loop
      INIT(I) := RND.INT_RANDOM(0, 50);
    end loop;
LAST := 2; INIT(1) := 6; INIT(2) := 8;
    RES_BUL(1 .. LAST) := INIT(1..LAST);
    RES_TAS(1 .. LAST) := INIT(1..LAST);
    RES_RAP(1 .. LAST) := INIT(1..LAST);

    CURRENT_SORT := "BUL";
    TAB_SORTS.BUBBLE_SORT (RES_BUL(1..LAST));
    CURRENT_SORT := "TAS";
    TAB_SORTS.HEAP_SORT   (RES_TAS(1..LAST));
    CURRENT_SORT := "RAP";
    TAB_SORTS.QUICK_SORT  (RES_RAP(1..LAST));
    CURRENT_SORT := "   ";

    OK := TRUE;
    for I in 1 .. LAST loop
      if RES_BUL(I) /= RES_TAS(I) or else RES_BUL(I) /= RES_RAP(I) then
        OK := FALSE;
        exit;
      end if;
    end loop;

    if not OK then
      MY_IO.PUT_LINE ("ERROR:");

      MY_IO.PUT_LINE ("      INIT    BUBBLE      HEAP     QUICK");
      for I in 1 .. LAST loop
        MY_IO.PUT (NORMAL(INIT(I),    10));
        MY_IO.PUT (NORMAL(RES_BUL(I), 10));
        MY_IO.PUT (NORMAL(RES_TAS(I), 10));
        MY_IO.PUT (NORMAL(RES_RAP(I), 10));
        MY_IO.NEW_LINE;
      end loop;
      exit;
    else
      for I in 1 .. LAST loop
        MY_IO.PUT (NORMAL(INIT(I), 3));
      end loop;
      MY_IO.NEW_LINE;
      for I in 1 .. LAST loop
        MY_IO.PUT (NORMAL(RES_RAP(I), 3));
      end loop;
      MY_IO.PUT_LINE (" OK");
      MY_IO.NEW_LINE;
      delay 1.0;
    end if;

  end loop;
exception
  when others =>
    MY_IO.PUT_LINE ("Exception when sorting with " & CURRENT_SORT & " on:");
    for I in 1 .. LAST loop
      MY_IO.PUT (NORMAL(INIT(I), 3));
    end loop;
    MY_IO.NEW_LINE;
    raise;
end T_SORTS;
