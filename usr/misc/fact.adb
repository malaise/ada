with My_Math, My_Io;
use My_Math;
use My_Io;
-- Calcul de la factorielle d'un entier

procedure Fact is

  subtype Nat is Inte range 0 .. Inte'Last;
  N : Natural;
  Nombre   : Nat;
  Resultat : Real;

  function Calcul_Factoriel (N : Real) return Real is
  begin
    if N = 0.0 then
      return 1.0;
    else
      return N * Calcul_Factoriel (N - 1.0);
    end if;
  end Calcul_Factoriel;

begin
  Put_Line("Calcul de la factorielle d'un nombre:");
  New_Line;
  loop

    loop
      begin
        Put("Entrez le nombre ? ");
        Get(N);
        Nombre := Inte(N);
        exit;
      exception
        when others =>
          Skip_Line;
          Put_Line("ERREUR, il faut un entier positif ou nul. Recommencez.");
      end;
    end loop;

    begin

      Put(Nombre);
      Put("! = ");
      Resultat := Calcul_Factoriel(Real(Nombre));
      Put(Resultat'Img);
      New_Line;
      New_Line;
    exception
      when Constraint_Error | Storage_Error =>
        Put_Line("Nombre trop grand. Recommencez.");
        New_Line;
    end;
  end loop;
end Fact;
