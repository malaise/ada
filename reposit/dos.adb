with MY_IO;
package body DOS is

  procedure SOUND (N_TIMES : in POSITIVE := 1) is
  begin
    for I in 1 .. N_TIMES loop
      MY_IO.PUT (ASCII.BEL);
    end loop;
    MY_IO.FLUSH;
  end SOUND;

end DOS;
