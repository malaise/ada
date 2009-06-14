with Ada.Text_Io;
with Rnd;
with Types;
package body Scrambler_Gen is

  -- Generate a scrambler for enigma config file
  function Generate (Symetric : Boolean) return String is
    -- Letter index 1 .. 26
    subtype Id_Range is Positive range 1 .. Positive(Types.Lid'Last) + 1;
    function Id_Random is new Rnd.Discr_Random (Id_Range);

    -- String image
    Offset : constant := Character'Pos('A') - 1;
    function To_Letter (Id : Id_Range) return Character is
    begin
      return Character'Val(Id + Offset);
    end To_Letter;

    -- Array of assigned letters
    Map : array (Id_Range) of Id_Range;
    Str : String (Id_Range);

    -- Array of remaining letters to map
    subtype Natural_Id is Natural range 0 .. Id_Range'Last;
    Nb_Remain : Natural_Id;
    Remain : array (Id_Range) of Id_Range;

    -- The random Id in Remain
    Id : Id_Range;

    -- Index of I in Remain
    I_Id : Natural_Id;
  begin

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
    for I in Id_Range loop
      Str(I) := To_Letter (Map(I));
    end loop;
    return Str;
  end Generate;

end Scrambler_Gen;

