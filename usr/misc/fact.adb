

with My_Io, Text_Io; use My_Io; 
-- calcul de la factorielle d'un entier;

procedure Fact is 

  Nombre   : Natural; 
  Resultat : Float; 

  function Calcul_Factoriel (N : Float) return Float is
  begin
    if N = 0.0 then
      return 1.0;
    else
      return N*Calcul_Factoriel(N - 1.0);
    end if;
  end Calcul_Factoriel;

begin
  Put_Line("Calcul de la factorielle d'un nombre:");
  New_Line;
  loop

    loop
      begin
        Put("Entrez le nombre ? ");
        Get(Nombre);
        exit;
      exception
        when others =>
          Skip_Line;
          Put_Line("ERREUR, il faut un entier positif ou nul. Recommencez.");
      end;
    end loop;

    begin
      if (Nombre > 170) then
        raise Constraint_Error;
      end if;

      Put(Nombre);
      Put("! = ");
      Resultat := Calcul_Factoriel(Float(Nombre));
      Put(Resultat);
      New_Line;
      New_Line; 
    exception
      when Constraint_Error | Numeric_Error | Storage_Error => 
        Put_Line("Nombre trop grand. Recommencez."); 
        New_Line; 
    end; 
  end loop; 
end Fact; 
