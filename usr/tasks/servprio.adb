with MY_IO, RND, TEXT_IO, SCHEDULE; use MY_IO; 
procedure SERVPRIO is 

  type URGENCE is (CRITIQUE, URGENT, NORMAL, LENT); 
  subtype CLIENT_RANGE is POSITIVE range 1 .. 10; 

  type TABLEAU is array(URGENCE) of NATURAL; 
  TOTALS, TOTALC : TABLEAU := (others => 0); 
  TOTAL_APPEL    : NATURAL := 0; 

  function MY_RANDOM is 
    new RND.DISCR_RANDOM(URGENCE); 

  FICH : TEXT_IO.FILE_TYPE; 
  NOM  : constant STRING := ("servprio.dat"); 

  task SERVEUR is 
    entry INIT; 
    entry SERVICE(URGENCE)(NO_CLIENT : in CLIENT_RANGE); 
    entry FIN; 
  end SERVEUR; 

  task type CLIENT is 
    entry INIT(NO : in CLIENT_RANGE); 
    entry FIN(TOTAL : in out TABLEAU); 
  end CLIENT; 

  CLIENTS : array(CLIENT_RANGE) of CLIENT; 

  procedure PRINT(AUTEUR    : in STRING; 
                  DEGRE     : in URGENCE; 
                  CLIENT_NO : in CLIENT_RANGE) is 
  begin
    TEXT_IO.PUT(FICH, AUTEUR); 
    TEXT_IO.PUT(FICH, " de "); 
    TEXT_IO.PUT(FICH, CLIENT_RANGE'IMAGE(CLIENT_NO)); 
    TEXT_IO.PUT(FICH, " priorite "); 
    TEXT_IO.PUT_LINE(FICH, URGENCE'IMAGE(DEGRE)); 
  end PRINT; 


  task body SERVEUR is 
    I : URGENCE := URGENCE'FIRST; 

  begin
    accept INIT; 

    loop
      SCHEDULE; 

      select
        accept SERVICE(I)(NO_CLIENT : in CLIENT_RANGE) do
          PRINT("Service", I, NO_CLIENT); 
          PUT_LINE("Service"); 
          TOTALS(I) := TOTALS(I) + 1; 
        end SERVICE; 
        I := URGENCE'FIRST; 
      or 
        delay 0.0; 
        if I /= URGENCE'LAST then 
          I := URGENCE'SUCC(I); 
        else 
          I := URGENCE'FIRST; 
        end if; 
      or 
        accept FIN; 
        exit; 
      end select; 
    end loop; 

  exception
    when others => 
      PUT_LINE("Exception serveur"); 
  end SERVEUR; 


  task body CLIENT is 
    NO       : CLIENT_RANGE; 
    PRIO     : URGENCE; 
    MAX_LOOP : constant POSITIVE := 10; 
    N_LOOP   : POSITIVE; 
    CALCUL   : TABLEAU := (others => 0); 
  begin
    accept INIT(NO : in CLIENT_RANGE) do
      CLIENT.NO := NO; 
    end INIT; 

    N_LOOP := RND.INT_RANDOM(1, MAX_LOOP); 
    for I in 1 .. N_LOOP loop
      SCHEDULE; 
      PRIO := MY_RANDOM; 
      PRINT("                           Requete", PRIO, NO); 

      SERVEUR.SERVICE(PRIO)(NO); 

      CALCUL(PRIO) := CALCUL(PRIO) + 1; 
    end loop; 

    accept FIN(TOTAL : in out TABLEAU) do
      for I in URGENCE loop
        TOTAL(I) := TOTAL(I) + CALCUL(I); 
      end loop; 
    end FIN; 

    TEXT_IO.PUT(FICH, "                           Fin client "); 
    TEXT_IO.PUT(FICH, CLIENT_RANGE'IMAGE(NO)); 
    TEXT_IO.PUT(FICH, " , "); 
    TEXT_IO.PUT(FICH, POSITIVE'IMAGE(N_LOOP)); 
    TEXT_IO.PUT_LINE(FICH, " appels"); 

  exception
    when others => 
      PUT_LINE("Exception client"); 
      raise; 
  end CLIENT; 

begin
  TEXT_IO.CREATE(FICH, TEXT_IO.OUT_FILE, NOM); 

  for I in CLIENT_RANGE loop
    CLIENTS(I).INIT(I); 
  end loop; 

  SERVEUR.INIT; 

  for I in CLIENT_RANGE loop
    CLIENTS(I).FIN(TOTALC); 
  end loop; 

  SERVEUR.FIN; 

  TEXT_IO.PUT_LINE(FICH, "TOTAUX"); 
  for I in URGENCE loop
    TEXT_IO.PUT(FICH, URGENCE'IMAGE(I)); 
    TEXT_IO.PUT(FICH, " -> "); 
    TEXT_IO.PUT(FICH, POSITIVE'IMAGE(TOTALS(I))); 
    TEXT_IO.PUT(FICH, " & "); 
    TEXT_IO.PUT_LINE(FICH, POSITIVE'IMAGE(TOTALC(I))); 
    TOTAL_APPEL := TOTAL_APPEL + TOTALC(I); 
  end loop; 
  TEXT_IO.PUT_LINE(FICH, " TOTAL APPELS : " & POSITIVE'IMAGE(TOTAL_APPEL)); 

  TEXT_IO.CLOSE(FICH); 

exception
  when others => 
    PUT_LINE("Exception prog"); 
    raise; 
end SERVPRIO; 
