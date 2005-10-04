with Ada.Text_Io;
with Argument, Rnd, Sys_Calls;
with Types;
-- Generate a scrambler for enigma config file
procedure Scrambler_Generator is

  -- Letter index 1 .. 26
  subtype Id_Range is Positive range 1 .. Positive(Types.Lid'Last) + 1;
  function Id_Random is new Rnd.Discr_Random (Id_Range);

  Offset : constant := Character'Pos('A') - 1;
  function To_Letter (Id : Id_Range) return Character is
  begin
    return Character'Val(Id + Offset);
  end To_Letter;

  -- Error/Usage
  procedure Error is
  begin
    Sys_Calls.Put_Line_Error ("ERROR. Usage " & Argument.Get_Program_Name
       & " [ -s ]");
    Sys_Calls.Set_Error_Exit_Code;
  end Error;

  -- Shall we generate a symetric scrambler or not
  Symetric : Boolean := False;

  -- Array of assigned letters
  Map : array (Id_Range) of Id_Range;

  -- Array of remaining letters to map
  subtype Natural_Id is Natural range 0 .. Id_Range'Last;
  Nb_Remain : Natural_Id;
  Remain : array (Id_Range) of Id_Range;

  -- The random Id in Remain
  Id : Id_Range;

  -- Index of I in Remain
  I_Id : Natural_Id;
begin
  -- One optionnal "-s"
  if Argument.Get_Nbre_Arg > 1
  or else (Argument.Get_Nbre_Arg = 1
           and then Argument.Get_Parameter /= "-s") then
    Error;
    return;
  end if;
  Symetric := Argument.Get_Nbre_Arg = 1;

  -- Init
  Rnd.Randomize;
  Nb_Remain := Id_Range'Last;
  for I in Id_Range'Range loop
    Remain(I) := I;
    Map(I) := I;
  end loop;

  -- Search random mapping
  if Symetric then
    for I in Id_Range'Range loop
      -- Skip entries already mapped
      if Map(I) = I then
        -- Forbid identity if symetric
        loop
          -- Random index in Remain (1 .. Nb_Remain);
          Id := Id_Random (Maxi => Nb_Remain);
          exit when Remain(Id) /= I;
        end loop;
        -- So I -> Remain(Id)
        Map(I) := Remain(Id);
        -- Remove Id from Remain
        for J in Id + 1 .. Nb_Remain loop
          Remain(J - 1) := Remain(J);
        end loop;
        Nb_Remain := Nb_Remain - 1;
        -- Also map the reverse way
        Map(Map(I)) := I;
        -- Locate I in Remain
        I_Id := 0;
        for J in 1 .. Nb_Remain loop
          if Remain(J) = I then
            I_Id := J;
            exit;
          end if;
        end loop;
        -- I should be found
        if I_Id = 0 then
          Ada.Text_Io.Put_Line ("Symetric not found!!!");
          raise Program_Error;
        end if;
        -- Remove I from Remain
        for J in I_Id + 1 .. Nb_Remain loop
          Remain(J - 1) := Remain(J);
        end loop;
        Nb_Remain := Nb_Remain - 1;
      end if; 
    end loop;
  else
    -- Not symetric
    for I in Id_Range'Range loop
      -- Random index in Remain (1 .. Nb_Remain);
      Id := Id_Random (Maxi => Nb_Remain);
      -- So I -> Remain(Id)
      Map(I) := Remain(Id);
      -- Remove Id from Remain
      for J in Id + 1 .. Nb_Remain loop
        Remain(J - 1) := Remain(J);
      end loop;
      Nb_Remain := Nb_Remain - 1;
    end loop;
  end if;

  -- Put reference then map
  for I in Id_Range'Range loop
    Ada.Text_Io.Put (To_Letter (I));
  end loop;
  Ada.Text_Io.New_Line;
  for I in Id_Range'Range loop
    Ada.Text_Io.Put (To_Letter (Map(I)));
  end loop;

end Scrambler_Generator;

