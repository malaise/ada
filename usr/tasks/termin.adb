-- dependance de taches
with TEXT_IO;
procedure TERMIN is
  use TEXT_IO;

  type T_CREATION is (PAR_ACCES, DIRECT);

  package CREATION_IO is new TEXT_IO.ENUMERATION_IO (T_CREATION);

  LOOPS : constant array (T_CREATION) of POSITIVE := (4, 2);

  -- priorite du programme principal
  pragma PRIORITY (5);

  -- type de tache
  task type T_TACHE is
    entry INIT (NOM : in T_CREATION);
    pragma PRIORITY (1);
  end T_TACHE;

  type T_PTR_TACHE is access T_TACHE;

  SEPARATEUR : constant STRING := "-------------";

  task body T_TACHE is
    NOM : T_CREATION;
  begin
    accept INIT (NOM : in T_CREATION) do
      T_TACHE.NOM := INIT.NOM;
    end INIT;
    PUT ("tache "); CREATION_IO.PUT (NOM, T_CREATION'width);
    PUT_LINE (" lancee");
    for I in 1..LOOPS(NOM) loop
      delay 0.5;
      PUT ("tache ");CREATION_IO.PUT (NOM, T_CREATION'width);
      PUT_LINE (" active");
    end loop;
    PUT ("tache "); CREATION_IO.PUT (NOM, T_CREATION'width);
    PUT_LINE (" achevee");
  end T_TACHE;

begin
  PUT_LINE ("Une tache depend du bloc ou elle est declaree, sauf une tache "
    & "accedee,");
  PUT_LINE (" laquelle depend du bloc qui declare le type acces:");
  NEW_LINE (2);

  declare
    PA : T_PTR_TACHE := new T_TACHE;
    PB : T_TACHE;
  begin
    PUT (SEPARATEUR); PUT (" Debut bloc "); PUT_LINE (SEPARATEUR);
    PA.INIT (PAR_ACCES);
    PB.INIT (DIRECT);
  end;
  PUT (SEPARATEUR); PUT (" Fin   bloc "); PUT_LINE (SEPARATEUR);

end TERMIN;
