-- Compute factorial of a integer
with My_Math, Basic_Proc, Gets;
use Basic_Proc, My_Math;

procedure Fact is

  subtype Nat is Inte range 0 .. Inte'Last;
  N : Natural;
  Nombre   : Nat;
  Resultat : Real;

  function Calcul_Factoriel (N : Real) return Real is
    (if N = 0.0 then 1.0 else N * Calcul_Factoriel (N - 1.0));

begin
  Put_Line_Output ("Computation of the factorial of a number:");
  New_Line_Output;
  All_Gets: loop

    One_Get: loop
      begin
        Put_Output ("Enter the number? ");
        N := Gets.Get_Int (Get_Line);
        Nombre := Inte(N);
        exit One_Get;
      exception
        when Basic_Proc.End_Error =>
          exit All_Gets;
        when others =>
          Skip_Line;
          Put_Line_Output (
              "ERROR, a positive or null integer is required. Try again.");
      end;
    end loop One_Get;

    begin

      Put_Output (Nombre'Img);
      Put_Output ("! = ");
      Resultat := Calcul_Factoriel(Real(Nombre));
      Put_Output (Resultat'Img);
      New_Line_Output;
      New_Line_Output;
    exception
      when Constraint_Error | Storage_Error =>
        Put_Line_Output ("ERROR, the number is too big. Try again.");
        New_Line_Output;
    end;
  end loop All_Gets;
end Fact;

