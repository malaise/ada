with Flo_Io, Long_Io, My_Math, My_Io, Clear_Screen;
use My_Io;
procedure Impot_02 is

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

  -- l'impot a payer
  Impot_A_Payer : Somme;

  -- les resultats intermediaires des calculs
  Resultat : Somme;

  -- pour lire un float tres securise
  function Get_Real return My_Math.Real is
    Chaine : String(1..132);
    Dernier : Natural;
    Valeur : Float;
    Int_Val : My_Math.Inte;
  begin
    My_Io.Get_Line (Chaine, Dernier);
    begin
      Flo_Io.Get (Chaine(1..Dernier), Valeur, Dernier);
      return My_Math.Real(Valeur);
    exception
      when others =>
        Long_Io.Get(Chaine(1..Dernier), Int_Val, Dernier);
        return My_Math.Real (Int_Val);
    end;
  end Get_Real;

  -- Pour lire une somme avec une invite
  function Get_Somme (Message : String) return Somme is
    Val : My_Math.Real;
  begin
    loop
      begin
        Put (Message & " : ");
        Val := Get_Real;
        return Somme(Val);
      exception
        when others => Put_Line ("ERREUR. Recommencez.");
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
        Put (Message);
        Put (" : ");
        Val := Get_Real;
        Frac := My_Math.Frac (Val);
        if (Frac /= 0.0) and then (Frac /= 0.5) then
          raise Constraint_Error;
        end if;
        if (Val <= 0.0) then
          raise Constraint_Error;
        end if;
        return Positive(My_Math.Int (Val * 10.0) );
      exception
        when others => Put_Line ("ERREUR. Recommencez.");
      end;
    end loop;
  end Get_Part;

  -- Pour imprimer une somme entre 2 messages
  procedure Ecrit (Mes_1 : in String; Valeur : in Somme; Mes_2 : in String) is
  begin
    Put (Mes_1 & ' ');
    Put (Float(Valeur), 11, 2, 0);
    Put_Line (' ' & Mes_2);
  end Ecrit;


begin
  Clear_Screen;

  -- 1. CALCUL DU REVENU BRUT GLOBAL
  declare
    -- les deductions plafonnees
    Deduction_1, Deduction_2 : Somme;
    -- les plafonds
    -- deduction 10%
    Plafond_1_H : constant Somme := 12_229.00;
    -- abattement 20%
    Plafond_2_H : constant Somme := 22_380.00;
  begin
    -- le salaire net imposable
    Salaire := Get_Somme ("Entrez votre salaire net imposable");
    Resultat := Salaire;

    -- 1.1. deduction 10%
    Deduction_1 := Resultat * (10.0 / 100.0);
    if (Deduction_1 > Plafond_1_H) then
      Deduction_1 := Plafond_1_H;
    end if;
    Resultat := Resultat - Deduction_1;
    -- ecrit ("apres deduction 10% et plafonds : ", resultat, "");

    -- 1.3. abattement 20%
    Deduction_2 := Resultat * (20.0 / 100.0);
    if (Deduction_2 > Plafond_2_H) then
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
      1 => ( Somme_Max=>04_121.0,  Coefficient=>0000, Fixe=>00_000.00),
      2 => ( Somme_Max=>08_104.0,  Coefficient=>0750, Fixe=>00_309.08),
      3 => ( Somme_Max=>14_264.0,  Coefficient=>2100, Fixe=>01_403.12),
      4 => ( Somme_Max=>23_096.0,  Coefficient=>3100, Fixe=>02_829.52),
      5 => ( Somme_Max=>37_579.0,  Coefficient=>4100, Fixe=>05_139.12),
      6 => ( Somme_Max=>46_343.0,  Coefficient=>4675, Fixe=>07_299.91),
      7 => ( Somme_Max=>Somme'Last,Coefficient=>5275, Fixe=>10_080.49));
  begin
    -- saisie du nombre de parts
    Nbre_Part := Get_Part ("Entrez le nombre de parts");

    -- quotient familial
    Quotient_Familial := Resultat / (Somme(Nbre_Part) / 10.0 );
    Ecrit ("Quotient familial    : ", Quotient_Familial, "e");

    -- determination de la tranche
    while (Quotient_Familial > Les_Tranches_Familiales(No_Tranche).Somme_Max)
    loop
      No_Tranche := No_Tranche + 1;
    end loop;
    -- calcul de l'impot << i >>
    Resultat := Resultat
     * (Somme (Les_Tranches_Familiales(No_Tranche).Coefficient) / 10_000.0);
    Resultat := Resultat
     - ( Somme (Nbre_Part) / 10.0 ) * Les_Tranches_Familiales(No_Tranche).Fixe;
    Impot_I := Resultat;
    Put ("Tranche No : "); Put (Integer(No_Tranche), 2); New_Line;
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
    New_Line;
    Ecrit ("Votre impot a payer est de", Impot_A_Payer, "euros.");
  end;

end; -- IMPOT_xx

