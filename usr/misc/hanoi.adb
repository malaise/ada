with TEXT_IO, MY_IO, CALENDAR; use MY_IO; 
-- Simulation du jeu des tours de hanoi

procedure HANOI is 

  type TYP_SUPPORT is (A, B, C); 
  ORIGINE, DESTINATION : TYP_SUPPORT; 
  package SUPPORT_IO is 
    new TEXT_IO.ENUMERATION_IO(TYP_SUPPORT); 
  SUPPORTS_IDENTIQUES : exception; 

  subtype TYP_NUMERO_DE_DISQUE is POSITIVE; 
  NOMBRE_DE_DISQUES      : TYP_NUMERO_DE_DISQUE; 

  NOMBRE_DE_DEPLACEMENTS : LONG_LONG_INTEGER;
  COMPTEUR_OVERFLOW      : exception;

  REPONSE                : CHARACTER;
  TRACE                  : BOOLEAN;

  TEMPS                  : CALENDAR.TIME;
  SECONDE                : CALENDAR.DAY_DURATION;
  package DURATION_IO is
    new TEXT_IO.FIXED_IO(DURATION);

  function DETERMINE_INTERMEDIAIRE (
   PREMIER_SUPPORT, DEUXIEME_SUPPORT : TYP_SUPPORT) return TYP_SUPPORT is
  begin
    case PREMIER_SUPPORT is
      when A =>
        case DEUXIEME_SUPPORT is
          when B =>
            return C;
          when C =>
            return B;
          when others =>
            null;
        end case;
      when B =>
        case DEUXIEME_SUPPORT is
          when C =>
            return A;
          when A =>
            return C;
          when others =>
            null;
        end case;
      when C =>
        case DEUXIEME_SUPPORT is
          when A =>
            return B;
          when B =>
            return A;
          when others =>
            null;
        end case;
    end case;
    PUT_LINE("PROBLEME: Deux supports identiques.");
    raise SUPPORTS_IDENTIQUES;
  end DETERMINE_INTERMEDIAIRE;

  procedure DEPLACER (
   ORIGINE, INTERMEDIAIRE, DESTINATION : in TYP_SUPPORT;
   NUMERO_DE_DISQUE                    : in TYP_NUMERO_DE_DISQUE) is
  begin
    begin
      DEPLACER(ORIGINE, DESTINATION, INTERMEDIAIRE, TYP_NUMERO_DE_DISQUE'PRED(
        NUMERO_DE_DISQUE));
    exception
      when CONSTRAINT_ERROR =>
        null;
    end; 

    if TRACE then 
      PUT("Deplacer le disque "); 
      PUT(NUMERO_DE_DISQUE); 
      PUT(" de "); 
      SUPPORT_IO.PUT(ORIGINE); 
      PUT(" vers "); 
      SUPPORT_IO.PUT(DESTINATION); 
      PUT_LINE("."); 
    end if; 

    begin
      NOMBRE_DE_DEPLACEMENTS := LONG_LONG_INTEGER'SUCC(NOMBRE_DE_DEPLACEMENTS); 
    exception
      when others => 
        raise COMPTEUR_OVERFLOW; 
    end; 

    begin
      DEPLACER(INTERMEDIAIRE, ORIGINE, DESTINATION, TYP_NUMERO_DE_DISQUE'PRED(
        NUMERO_DE_DISQUE)); 
    exception
      when CONSTRAINT_ERROR => 
        null; 
    end; 

  end DEPLACER; 

begin -- hanoi
  PUT_LINE("Probleme des tours de HANOI:"); 
  loop
    NEW_LINE; 

    loop
      begin
        PUT("Entrez le support de depart (a, b, c) ? "); 
        SUPPORT_IO.GET(ORIGINE); 
        NEW_LINE; 
        exit;
      exception
        when others => 
          SKIP_LINE; 
          PUT_LINE("ERREUR de saisie, recommencez."); 
      end; 
    end loop; 

    loop
      begin
        PUT("Entrez le support d'arrivee (a, b, c) ? "); 
        SUPPORT_IO.GET(DESTINATION); 
        if (DESTINATION = ORIGINE) then 
          raise SUPPORTS_IDENTIQUES; 
        end if; 
        NEW_LINE; 
        exit; 
      exception
        when SUPPORTS_IDENTIQUES => 
          SKIP_LINE; 
          PUT_LINE("ERREUR, les deux supports sont identiques. Recommencez."); 
        when others => 
          SKIP_LINE; 
          PUT_LINE("ERREUR de saisie, recommencez."); 
      end; 
    end loop; 

    loop
      begin
        PUT("Entrez le nombre de disques (au moins 1) ? "); 
        GET(NOMBRE_DE_DISQUES); 
        NEW_LINE; 
        exit; 
      exception
        when others => 
          SKIP_LINE; 
          PUT_LINE("ERREUR, recommencez."); 
      end; 
    end loop; 

    loop
      begin
        PUT("Voulez-vous une trace detaillee (O/N) ? "); 
        GET(REPONSE); 
        if (REPONSE /= 'o') and then (REPONSE /= 'O') and then
           (REPONSE /= 'n') and then (REPONSE /= 'N') then
          raise CONSTRAINT_ERROR;
        end if; 
        NEW_LINE; 
        TRACE := ((REPONSE = 'o') or else (REPONSE = 'O'));
        exit; 
      exception
        when others => 
          SKIP_LINE; 
          PUT_LINE("ERREUR, recommencez."); 
      end; 
    end loop; 

    NEW_LINE; 

    begin
      NOMBRE_DE_DEPLACEMENTS := 0; 
      TEMPS := CALENDAR.CLOCK; 
      DEPLACER(ORIGINE, DETERMINE_INTERMEDIAIRE(ORIGINE, DESTINATION), 
        DESTINATION, NOMBRE_DE_DISQUES); 
      SECONDE := CALENDAR."-"(CALENDAR.CLOCK, TEMPS); 

      NEW_LINE; 
      PUT("Transfert de "); 
      PUT(NOMBRE_DE_DISQUES); 
      PUT(" disques de "); 
      SUPPORT_IO.PUT(ORIGINE); 
      PUT(" vers "); 
      SUPPORT_IO.PUT(DESTINATION); 
      PUT_LINE(" ."); 

      PUT("Operation effectuee en "); 
      PUT(NOMBRE_DE_DEPLACEMENTS); 
      PUT_LINE(" deplacements "); 
      PUT(" et en "); 
      DURATION_IO.PUT(SECONDE); 
      PUT_LINE(" secondes."); 

    exception
      when COMPTEUR_OVERFLOW => 
        PUT("Depassement de la capacite du compteur. Mettez moins de disques.")
          ; 
    end; 

  end loop; 

end HANOI; 

