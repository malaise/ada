with Basic_Proc, Rnd, Dur_Image, Key_Pressed;
procedure T_Dur_Image is
  First : Boolean := True;
  D : Duration;
  N : Natural;
begin
  Rnd.Randomize;
  Key_Pressed.Open (False);

  -- Loop of random durations
  loop
    for I in 1 .. 10 loop
      if First then
        D := 0.0;
        First := False;
      else
        D := Rnd.Dur_Random (-99.999999, 100.0);
      end if;
      Basic_Proc.Put_Line_Output ("D is " & D'Img);
      -- Loop of various Nb_Digits
      for I in 1 .. 10 loop
        N := Rnd.Int_Random (Maxi => 10);
        Basic_Proc.Put_Line_Output (N'Img & " ->" & Dur_Image (D, N, True)
                                          & "<->" & Dur_Image (D, N, False)
                                          & "<");
      end loop;
    end loop;
    exit when Key_Pressed.Key_Pressed;
    Basic_Proc.Put_Line_Output ("Hit a key to stop.");
    delay (1.0);
  end loop;

  Key_Pressed.Close;
exception
  when others =>
    Key_Pressed.Close;
    raise;
end T_Dur_Image;

