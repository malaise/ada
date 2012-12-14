with Basic_Proc, Rnd, Images, Key_Pressed, Argument;
procedure T_Dur_Image is
  First : Boolean := True;
  D : Duration;
  N : Natural;
  Nb_Loops, Id_Loop : Natural;
begin
  Nb_Loops := 0;
  if Argument.Get_Nbre_Arg = 1 then
    begin
      Nb_Loops := Natural'Value (Argument.Get_Parameter(1));
    exception
      when Constraint_Error =>
        Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
          & " [ <nb_loops> ]");
        Basic_Proc.Set_Error_Exit_Code;
        return;
    end;
  end if;

  Rnd.Gen.Randomize;
  Key_Pressed.Open (False);
  Id_Loop := 0;

  -- Loop of random durations
  loop
    for I in 1 .. 10 loop
      if First then
        D := 0.0;
        First := False;
      else
        D := Rnd.Gen.Dur_Random (-99.999999, 100.0);
      end if;
      Basic_Proc.Put_Line_Output ("D is " & D'Img);
      -- Loop of various Nb_Digits
      for I in 1 .. 10 loop
        N := Rnd.Gen.Int_Random (Maxi => 10);
        Basic_Proc.Put_Line_Output (
             N'Img & " ->" & Images.Dur_Image (D, N, True)
                   & "<->" & Images.Dur_Image (D, N, False)
                                          & "<");
      end loop;
    end loop;
    Id_Loop := Id_Loop + 1;
    exit when Id_Loop = Nb_Loops;
    exit when Key_Pressed.Key_Pressed;
    if Nb_Loops = 0 then
      Basic_Proc.Put_Line_Output ("Hit a key to stop.");
      delay (1.0);
    end if;
    exit when Key_Pressed.Key_Pressed;
  end loop;

  Key_Pressed.Close;
exception
  when others =>
    Key_Pressed.Close;
    raise;
end T_Dur_Image;

