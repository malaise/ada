--  Triangle de PASCAL

with MY_IO; use MY_IO;
with TEXT_IO;
-- Calcul des elements des 33 premieres lignes du
--  TRIANGLE DE PASCAL
procedure PASCAL is

  -- element calcule et sorti
  ELEMENT      : LONG_LONG_INTEGER;
  -- table des resultats
  NO_LIGNE_MAX : constant INTEGER := 33;
  INDICE_MAX   : constant INTEGER := NO_LIGNE_MAX + 1;
  subtype TYP_NO_LIGNE is INTEGER range 0 .. NO_LIGNE_MAX;
  subtype TYP_INDICE is INTEGER range 1 .. INDICE_MAX;
  TABLE : array(TYP_NO_LIGNE, TYP_INDICE) of LONG_LONG_INTEGER :=
   (others => (others => 0));

begin
  PUT("Calcul des ");
  PUT(NO_LIGNE_MAX);
  PUT_LINE(" premieres lignes du Triangle de PASCAL:");
  NEW_LINE;

  PRINCIPALE : for NO_LIGNE in TYP_NO_LIGNE loop

    -- numero de ligne pour le calcul
    PUT(NO_LIGNE);
    PUT_LINE(" : ");

    PUT("   ");
    for INDICE in TYP_INDICE range TYP_INDICE'FIRST .. TYP_NO_LIGNE'SUCC(
      NO_LIGNE) loop

      -- 6 elements par ligne d'ecran
      if ((INDICE - 1) mod 6 = 0) and then (INDICE /= 1) then
        NEW_LINE;
        PUT("-> ");
      end if; 

      -- elements de la ligne separes par '/'
      if INDICE = TYP_INDICE'FIRST then 
        ELEMENT := 1; 
      else 
        ELEMENT := TABLE(TYP_NO_LIGNE'PRED(NO_LIGNE), TYP_INDICE'PRED(INDICE)) +
                      TABLE(TYP_NO_LIGNE'PRED(NO_LIGNE), INDICE); 
      end if; 
      TABLE(NO_LIGNE, INDICE) := ELEMENT; 
      PUT(ELEMENT); 
      if INDICE /= TYP_INDICE'SUCC(NO_LIGNE) then 
        PUT('/'); 
      end if; 
    end loop; 

    -- fin du traitement
    NEW_LINE; 
    NEW_LINE; 
  end loop PRINCIPALE; 

exception

  -- un element est trop grand
  when others => 
    PUT_LINE("PROBLEME: Degre trop grand."); 
    NEW_LINE; 
end PASCAL; 

