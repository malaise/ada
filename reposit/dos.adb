with MY_IO;
package body DOS is

  procedure SOUND (N_TIMES : in POSITIVE := 1) is
  begin
    for I in 1 .. N_TIMES loop
      MY_IO.PUT (ASCII.BEL);
      MY_IO.FLUSH;
      delay 0.2;
    end loop;
  end SOUND;

end DOS;
