

with MY_IO, TEXT_IO; use MY_IO; 
-- calcul de la factorielle d'un entier;

procedure FACT is 

  NOMBRE   : NATURAL; 
  RESULTAT : FLOAT; 

  function CALCUL_FACTORIEL (N : FLOAT) return FLOAT is
  begin
    if N = 0.0 then
      return 1.0;
    else
      return N*CALCUL_FACTORIEL(N - 1.0);
    end if;
  end CALCUL_FACTORIEL;

begin
  PUT_LINE("Calcul de la factorielle d'un nombre:");
  NEW_LINE;
  loop

    loop
      begin
        PUT("Entrez le nombre ? ");
        GET(NOMBRE);
        exit;
      exception
        when others =>
          SKIP_LINE;
          PUT_LINE("ERREUR, il faut un entier positif ou nul. Recommencez.");
      end;
    end loop;

    begin
      if (NOMBRE > 170) then
        raise CONSTRAINT_ERROR;
      end if;

      PUT(NOMBRE);
      PUT("! = ");
      RESULTAT := CALCUL_FACTORIEL(FLOAT(NOMBRE));
      PUT(RESULTAT);
      NEW_LINE;
      NEW_LINE; 
    exception
      when CONSTRAINT_ERROR | NUMERIC_ERROR | STORAGE_ERROR => 
        PUT_LINE("Nombre trop grand. Recommencez."); 
        NEW_LINE; 
    end; 
  end loop; 
end FACT; 
