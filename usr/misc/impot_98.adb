with FLO_IO, LONG_IO, MY_MATH, MY_IO, CLEAR_SCREEN;
use MY_IO;
procedure IMPOT_98 is

  -- Tout montant en francs
  type SOMME is digits 12 range -1_000_000_000.00 .. 1_000_000_000.00;

  -- le salaire net imposable
  SALAIRE : SOMME;
  -- le revenu brut global
  REVENU_BRUT_GLOBAL : SOMME;

  -- les charges deduites
  CHARGES_DEDUITES : SOMME;
  -- le revenu net
  REVENU_NET_IMPOSABLE : SOMME;

  -- le nombre de parts, le quotient familial et l'impot i
  NBRE_PART : POSITIVE;     -- Le nombre saisi * 10
  QUOTIENT_FAMILIAL : SOMME;
  IMPOT_I : SOMME;

  -- l'impot apres correction
  IMPOT_APRES_CORRECTION : SOMME;

  -- l'impot apres deduction
  IMPOT_APRES_DEDUCTION  : SOMME;

  -- l'impot a payer ...
  IMPOT_A_PAYER : SOMME;

  -- les resultats intermediaires des calculs
  RESULTAT : SOMME;

  -- pour lire un float tres securise
  function GET_REAL return MY_MATH.REAL is
    CHAINE : STRING(1..132);
    DERNIER : NATURAL;
    VALEUR : FLOAT;
    INT_VAL : MY_MATH.INTE;
  begin
    MY_IO.GET_LINE (CHAINE, DERNIER);
    begin
      FLO_IO.GET (CHAINE(1..DERNIER), VALEUR, DERNIER);
      return MY_MATH.REAL(VALEUR);
    exception
      when others =>
        LONG_IO.GET(CHAINE(1..DERNIER), INT_VAL, DERNIER);
        return MY_MATH.REAL (INT_VAL);
    end;
  end GET_REAL;

  -- Pour lire une somme avec une invite
  function GET_SOMME (MESSAGE : STRING) return SOMME is
    VAL : MY_MATH.REAL;
  begin
    loop
      begin
        PUT (MESSAGE & " : ");
        VAL := GET_REAL;
        return SOMME(VAL);
      exception
        when others => PUT_LINE ("ERREUR. Recommencez.");
      end;
    end loop;
  end GET_SOMME;

  -- Pour lire le nombre de parts avec une invite
  --  rend le nombre * 10
  function GET_PART (MESSAGE : STRING) return POSITIVE is
    VAL : MY_MATH.REAL;
    FRAC : MY_MATH.REAL;
    use MY_MATH;
  begin
    loop
      begin
        PUT (MESSAGE);
        PUT (" : ");
        VAL := GET_REAL;
        FRAC := MY_MATH.FRAC (VAL);
        if (FRAC /= 0.0) and then (FRAC /= 0.5) then
          raise CONSTRAINT_ERROR;
        end if;
        if (VAL <= 0.0) then
          raise CONSTRAINT_ERROR;
        end if;
        return POSITIVE(MY_MATH.INT (VAL * 10.0) );
      exception
        when others => PUT_LINE ("ERREUR. Recommencez.");
      end;
    end loop;
  end GET_PART;

  -- Pour imprimer une somme entre 2 messages
  procedure ECRIT (MES_1 : in STRING; VALEUR : in SOMME; MES_2 : in STRING) is
  begin
    PUT (MES_1 & ' ');
    PUT (FLOAT(VALEUR), 11, 2, 0);
    PUT_LINE (' ' & MES_2);
  end ECRIT;


begin
  CLEAR_SCREEN;

  -- 1. CALCUL DU REVENU BRUT GLOBAL
  declare
    -- les deductions plafonnees
    DEDUCTION_1, DEDUCTION_2 : SOMME;
    -- les plafonds
    -- deduction 10%
    PLAFOND_1_H : constant SOMME := 077_4600.00;
    -- abattement 20%
    PLAFOND_2_H : constant SOMME := 141_400.00;
  begin
    -- le salaire net imposable
    SALAIRE := GET_SOMME ("Entrez votre salaire net imposable");
    RESULTAT := SALAIRE;

    -- 1.1. deduction 10%
    DEDUCTION_1 := RESULTAT * (10.0 / 100.0);
    if (DEDUCTION_1 > PLAFOND_1_H) then
      DEDUCTION_1 := PLAFOND_1_H;
    end if;
    RESULTAT := RESULTAT - DEDUCTION_1;
    -- ecrit ("apres deduction 10% et plafonds : ",resultat,"");

    -- 1.3. abattement 20%
    DEDUCTION_2 := RESULTAT * (20.0 / 100.0);
    if (DEDUCTION_2 > PLAFOND_2_H) then
      DEDUCTION_2 := PLAFOND_2_H;
    end if;
    RESULTAT := RESULTAT - DEDUCTION_2;
    -- ecrit ("apres deduction 20% et plafonds : ",inter, "");

    -- le revenu
    REVENU_BRUT_GLOBAL := RESULTAT;
    ECRIT ("Revenu brut global   : ",REVENU_BRUT_GLOBAL,"F");
  end;

  -- 2. CALCUL DU REVENU NET IMPOSABLE
  declare
    use MY_MATH;
  begin
    -- 2.1. deductions diverses
    CHARGES_DEDUITES := 0.0;
    RESULTAT := RESULTAT - CHARGES_DEDUITES;

    -- arrondi a la dizaine de francs inferieure}
    RESULTAT := SOMME (MY_MATH.INTE (MY_MATH.REAL (RESULTAT/10.0) * 10.0) );
    REVENU_NET_IMPOSABLE := RESULTAT;
    ECRIT ("Revenu net imposable : ", REVENU_NET_IMPOSABLE,"F");
  end;

  -- 3. 4. 5. NOMBRE DE PARTS, QUOTIENT FAMILIAL et de L'IMPOT << I >>
  declare

    -- numero de tranche
    type INDICE_TRANCHE is new POSITIVE range 1..7;
    NO_TRANCHE : INDICE_TRANCHE := INDICE_TRANCHE'FIRST;
    -- les tranches
    type DESCRIPTEUR_TRANCHE_FAMILIALE is record
      SOMME_MAX : SOMME;
      COEFFICIENT : NATURAL;
      FIXE : SOMME;
    end record;
    type LISTE_TRANCHES_FAMILIALES is array (INDICE_TRANCHE range <>)
     of DESCRIPTEUR_TRANCHE_FAMILIALE;
    LES_TRANCHES_FAMILIALES : constant LISTE_TRANCHES_FAMILIALES := (
      1=> ( SOMME_MAX=>026_100.0, COEFFICIENT=>000, FIXE=>00_000.00),
      2=> ( SOMME_MAX=>051_340.0, COEFFICIENT=>105, FIXE=>02_740.50),
      3=> ( SOMME_MAX=>090_370.0, COEFFICIENT=>240, FIXE=>09_671.40),
      4=> ( SOMME_MAX=>146_320.0, COEFFICIENT=>330, FIXE=>17_804.70),
      5=> ( SOMME_MAX=>238_080.0, COEFFICIENT=>430, FIXE=>32_436.70),
      6=> ( SOMME_MAX=>292_600.0, COEFFICIENT=>480, FIXE=>44_340.70),
      7=> ( SOMME_MAX=>SOMME'LAST,COEFFICIENT=>540, FIXE=>61_956.60));
  begin
    -- saisie du nombre de parts
    NBRE_PART := GET_PART ("Entrez le nombre de parts");

    -- quotient familial
    QUOTIENT_FAMILIAL := RESULTAT / (SOMME(NBRE_PART) / 10.0 );
    ECRIT ("Quotient familial    : ", QUOTIENT_FAMILIAL,"F");

    -- determination de la tranche
    while (QUOTIENT_FAMILIAL>LES_TRANCHES_FAMILIALES(NO_TRANCHE).SOMME_MAX)
    loop
      NO_TRANCHE := NO_TRANCHE + 1;
    end loop;
    -- calcul de l'impot << i >>
    RESULTAT := RESULTAT
     * (SOMME (LES_TRANCHES_FAMILIALES(NO_TRANCHE).COEFFICIENT) / 1_000.0);
    RESULTAT := RESULTAT
     - ( SOMME (NBRE_PART) / 10.0 ) * LES_TRANCHES_FAMILIALES(NO_TRANCHE).FIXE;
    IMPOT_I := RESULTAT;
    PUT ("Tranche No : "); PUT (INTEGER(NO_TRANCHE),2); NEW_LINE;
    ECRIT ("Impot <<i>>          : ", IMPOT_I,"F");
  end;

  -- 6. CALCUL DE L'IMPOT APRES CORRECTION
  begin
    IMPOT_APRES_CORRECTION := RESULTAT;
  end;

  -- 7. CALCUL DE L'IMPOT APRES DEDUCTION
  declare
    DONS, ASSURANCE : SOMME;
    PLAFOND : SOMME;
  begin
    DONS := GET_SOMME ("Montant des dons aux oeuvres d'interet general");
    -- Plafond des dons ig : 1.25 du revenu net imposable
    PLAFOND := 1.25 * REVENU_NET_IMPOSABLE;
    if DONS > PLAFOND then
      DONS := PLAFOND;
    end if;
    -- Deduction : 40% des ig
    RESULTAT := RESULTAT - 0.4 * DONS;

    ASSURANCE := GET_SOMME ("Montant des primes assurance vie");
    -- Plafond des dons av : 4000 (+ 1000/enfant non implemente)
    PLAFOND := 4000.0;
    if ASSURANCE > PLAFOND then
      ASSURANCE := PLAFOND;
    end if;
    -- Deduction : 25% des av
    RESULTAT := RESULTAT - 0.25 * ASSURANCE;

    IMPOT_APRES_DEDUCTION := RESULTAT;
  end;

  -- IMPOT A PAYER
  begin
    IMPOT_A_PAYER := RESULTAT;
    NEW_LINE;
    ECRIT ("Votre impot a payer est de", IMPOT_A_PAYER, "Francs.");
  end;

end; -- IMPOT_xx

