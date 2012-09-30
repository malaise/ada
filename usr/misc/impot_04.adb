with My_Math, Basic_Proc, Clear_Screen, Get_Float, Normalization;
use Basic_Proc;
procedure Impot_04 is

  -- Tout montant en francs
  type Somme is digits 12 range -1_000_000_000.00 .. 1_000_000_000.00;

  -- le salaire net imposable
  Salaire : Somme;
  -- le revenu brut global
  Revenu_Brut_Global : Somme;

  -- les charges deduites
  Charges_Deduites : Somme;
  -- le revenu net
  Revenu_Net_Imposable : Somme;

  -- le nombre de parts, le quotient familial et l'impot i
  Nbre_Part : Positive;     -- Le nombre saisi * 10
  Quotient_Familial : Somme;
  Impot_I : Somme;

  -- l'impot apres correction
  Impot_Apres_Correction : Somme;
  -- l'impot apres deduction
  Impot_Apres_Deduction  : Somme;
  pragma Unreferenced (Impot_Apres_Correction, Impot_Apres_Deduction);


  -- l'impot a payer
  Impot_A_Payer : Somme;

  -- les resultats intermediaires des calculs
  Resultat : Somme;

  -- pour lire un float tres securise
  function Get_Real return My_Math.Real is
    Valeur : Float;
  begin
    Valeur := Get_Float.Get_Float (Basic_Proc.Get_Line);
    return My_Math.Real(Valeur);
  end Get_Real;

  -- Pour lire une somme avec une invite
  function Get_Somme (Message : String) return Somme is
    Val : My_Math.Real;
  begin
    loop
      begin
        Put_Output (Message & " : ");
        Val := Get_Real;
        return Somme (Val);
      exception
        when others => Put_Line_Output ("ERREUR. Recommencez.");
      end;
    end loop;
  end Get_Somme;

  -- Pour lire le nombre de parts avec une invite
  --  rend le nombre * 10
  function Get_Part (Message : String) return Positive is
    Val : My_Math.Real;
    Frac : My_Math.Real;
    use My_Math;
  begin
    loop
      begin
        Put_Output (Message);
        Put_Output (" : ");
        Val := Get_Real;
        Frac := My_Math.Frac (Val);
        if Frac /= 0.0 and then Frac /= 0.5 then
          raise Constraint_Error;
        end if;
        if Val <= 0.0 then
          raise Constraint_Error;
        end if;
        return Positive(My_Math.Int (Val * 10.0) );
      exception
        when others => Put_Line_Output ("ERREUR. Recommencez.");
      end;
    end loop;
  end Get_Part;

  -- Pour imprimer une somme entre 2 messages
  procedure Ecrit (Mes_1 : in String; Valeur : in Somme; Mes_2 : in String) is
  begin
    Put_Output (Mes_1 & ' ');
    Put_Output (Normalization.Normal_Fixed (My_Math.Real (Valeur), 11, 8));
    Put_Line_Output (' ' & Mes_2);
  end Ecrit;


begin
  Clear_Screen;

  -- 1. CALCUL DU REVENU BRUT GLOBAL
  declare
    -- les deductions plafonnees
    Deduction_1, Deduction_2 : Somme;
    -- les plafonds
    -- deduction 10%
    Plafond_1_H : constant Somme := 12_862.00;
    -- abattement 20%
    Plafond_2_H : constant Somme := 23_580.00;
  begin
    -- le salaire net imposable
    Salaire := Get_Somme ("Entrez votre salaire net imposable");
    Resultat := Salaire;

    -- 1.1. deduction 10%
    Deduction_1 := Resultat * (10.0 / 100.0);
    if Deduction_1 > Plafond_1_H then
      Deduction_1 := Plafond_1_H;
    end if;
    Resultat := Resultat - Deduction_1;
    -- ecrit ("apres deduction 10% et plafonds : ", resultat, "");

    -- 1.3. abattement 20%
    Deduction_2 := Resultat * (20.0 / 100.0);
    if Deduction_2 > Plafond_2_H then
      Deduction_2 := Plafond_2_H;
    end if;
    Resultat := Resultat - Deduction_2;
    -- ecrit ("apres deduction 20% et plafonds : ", inter, "");

    -- le revenu
    Revenu_Brut_Global := Resultat;
    Ecrit ("Revenu brut global   : ", Revenu_Brut_Global, "e");
  end;

  -- 2. CALCUL DU REVENU NET IMPOSABLE
  declare
    use My_Math;
  begin
    -- 2.1. deductions diverses
    Charges_Deduites := 0.0;
    Resultat := Resultat - Charges_Deduites;

    -- arrondi a la dizaine de francs inferieure}
    Resultat := Somme (My_Math.Inte (My_Math.Real (Resultat/10.0) * 10.0) );
    Revenu_Net_Imposable := Resultat;
    Ecrit ("Revenu net imposable : ", Revenu_Net_Imposable, "e");
  end;

  -- 3. 4. 5. NOMBRE DE PARTS, QUOTIENT FAMILIAL et de L'IMPOT << I >>
  declare

    -- numero de tranche
    type Indice_Tranche is new Positive range 1..7;
    No_Tranche : Indice_Tranche := Indice_Tranche'First;
    -- les tranches
    type Descripteur_Tranche_Familiale is record
      Somme_Max : Somme;
      Coefficient : Natural;
      Fixe : Somme;
    end record;
    type Liste_Tranches_Familiales is array (Indice_Tranche range <>)
     of Descripteur_Tranche_Familiale;
    Les_Tranches_Familiales : constant Liste_Tranches_Familiales := (
      1 => ( Somme_Max=>04_334.0,  Coefficient=>0000, Fixe=>00_000.00),
      2 => ( Somme_Max=>08_524.0,  Coefficient=>0683, Fixe=>00_296.01),
      3 => ( Somme_Max=>15_004.0,  Coefficient=>1914, Fixe=>01_345.32),
      4 => ( Somme_Max=>24_294.0,  Coefficient=>2826, Fixe=>02_713.68),
      5 => ( Somme_Max=>39_529.0,  Coefficient=>3738, Fixe=>04_929.29),
      6 => ( Somme_Max=>48_747.0,  Coefficient=>4262, Fixe=>07_000.61),
      7 => ( Somme_Max=>Somme'Last,Coefficient=>4809, Fixe=>09_667.07));
  begin
    -- saisie du nombre de parts
    Nbre_Part := Get_Part ("Entrez le nombre de parts");

    -- quotient familial
    Quotient_Familial := Resultat / (Somme(Nbre_Part) / 10.0 );
    Ecrit ("Quotient familial    : ", Quotient_Familial, "e");

    -- determination de la tranche
    while Quotient_Familial > Les_Tranches_Familiales(No_Tranche).Somme_Max loop
      No_Tranche := No_Tranche + 1;
    end loop;
    -- calcul de l'impot << i >>
    Resultat := Resultat
     * (Somme (Les_Tranches_Familiales(No_Tranche).Coefficient) / 10_000.0);
    Resultat := Resultat
     - ( Somme (Nbre_Part) / 10.0 ) * Les_Tranches_Familiales(No_Tranche).Fixe;
    Impot_I := Resultat;
    Put_Output ("Tranche No : ");
    Put_Output (Normalization.Normal_Int (Integer(No_Tranche), 2));
    New_Line_Output;
    Ecrit ("Impot <<i>>          : ", Impot_I, "e");
  end;

  -- 6. CALCUL DE L'IMPOT APRES CORRECTION
  begin
    Impot_Apres_Correction := Resultat;
  end;

  -- 7. CALCUL DE L'IMPOT APRES DEDUCTION
  declare
    Dons : Somme;
    -- Plafond des dons : 10% du revenu net imposable
    Plafond : constant Somme := 10.0 * Revenu_Net_Imposable;
    -- Deduction : 50% des dons
    Percent : constant Somme := 0.5;
  begin
    Dons := Get_Somme ("Montant des dons aux oeuvres d'interet general");
    if Dons > Plafond then
      Dons := Plafond;
    end if;
    Resultat := Resultat - Percent * Dons;
  end;

  -- declare
  --   Assurance : Somme;
  --   Plafond : Somme;
  -- begin
  --   Assurance := Get_Somme ("Montant des primes assurance vie");
  --   Plafond des dons av : 4000 (+ 1000/enfant non implemente)
  --   Plafond := 4000.0;
  --   if Assurance > Plafond then
  --     Assurance := Plafond;
  --   end if;
  --   Deduction : 25% des av
  --   Resultat := Resultat - 0.25 * Assurance;
  -- end;

  Impot_Apres_Deduction := Resultat;

  -- IMPOT A PAYER
  begin
    Impot_A_Payer := Resultat;
    New_Line_Output;
    Ecrit ("Votre impot a payer est de", Impot_A_Payer, "euros.");
  end;

end; -- IMPOT_xx

