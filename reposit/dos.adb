with My_Io;
package body Dos is

  procedure Sound (N_Times : in Positive := 1) is
  begin
    for I in 1 .. N_Times loop
      My_Io.Put (Ascii.Bel);
      My_Io.Flush;
      delay 0.2;
    end loop;
  end Sound;

end Dos;
