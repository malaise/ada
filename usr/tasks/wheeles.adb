with MY_IO;
procedure WHEELES is

  NB_WHEELES : constant := 3;
  subtype WHEELES_RANGE is POSITIVE range 1 .. NB_WHEELES;

  task type T_WHEELE is
    entry START (NO : in WHEELES_RANGE);
    entry STOP;
  end T_WHEELE;

  WHEELES_ARRAY : array (WHEELES_RANGE) of T_WHEELE;

  task body T_WHEELE is
    NO : WHEELES_RANGE;
  begin
    accept START (NO : in WHEELES_RANGE) do
      T_WHEELE.NO := NO;
      MY_IO.PUT_LINE ("Task " & WHEELES_RANGE'IMAGE(NO) & " is starting.");
    end START;
    MY_IO.PUT_LINE ("Task " & WHEELES_RANGE'IMAGE(NO) & " is started.");

    loop
      MY_IO.PUT_LINE ("Task " & WHEELES_RANGE'IMAGE(NO) & " is running.");
      select 
        accept STOP do
          MY_IO.PUT_LINE ("Task " & WHEELES_RANGE'IMAGE(NO) & " is exiting.");
        end STOP;
        exit;
      or
        delay 0.01;
      end select;
    end loop;
    MY_IO.PUT_LINE ("Task " & WHEELES_RANGE'IMAGE(NO) & " is exited.");
  end T_WHEELE;


begin
  MY_IO.PUT_LINE ("Starting tasks.");
  for I in WHEELES_RANGE loop
    WHEELES_ARRAY(I).START(I);
  end loop;

  loop
    declare
      STR : STRING (1 .. 132);
      LEN : NATURAL;
    begin
      delay 1.0;
--      MY_IO.GET_LINE (STR, LEN); 
--      exit when STR(1..LEN) = "exit";
    exception
      when others => null;
    end;
  end loop;
  MY_IO.PUT_LINE ("Stopping tasks.");

  for I in WHEELES_RANGE loop
    WHEELES_ARRAY(I).STOP;
  end loop;
  MY_IO.PUT_LINE ("Exiting.");
end WHEELES;
