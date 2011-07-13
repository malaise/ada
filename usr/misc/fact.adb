-- Compute factorial of a integer
with My_Math, My_Io;
use My_Math;
use My_Io;

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
  Put_Line("Computation of the factorial of a number:");
  New_Line;
  loop

    loop
      begin
        Put("Enter the number? ");
        Get(N);
        Nombre := Inte(N);
        exit;
      exception
        when others =>
          Skip_Line;
          Put_Line("ERROR, a positive or null integer is required. Try again.");
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
        Put_Line("ERROR, the number is too big. Try again.");
        New_Line;
    end;
  end loop;
end Fact;

