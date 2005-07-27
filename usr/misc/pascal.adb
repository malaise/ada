-- Triangle de Pascal

with My_Io; use My_Io;
-- Calcul des elements des 33 premieres lignes du
--  triangle de Pascal
procedure Pascal is

  -- element calcule et sorti
  Element      : Long_Long_Integer;
  -- table des resultats
  No_Ligne_Max : constant Integer := 33;
  Indice_Max   : constant Integer := No_Ligne_Max + 1;
  subtype Typ_No_Ligne is Integer range 0 .. No_Ligne_Max;
  subtype Typ_Indice is Integer range 1 .. Indice_Max;
  Table : array(Typ_No_Ligne, Typ_Indice) of Long_Long_Integer :=
   (others => (others => 0));

begin
  Put("Calcul des ");
  Put(No_Ligne_Max);
  Put_Line(" premieres lignes du Triangle de PASCAL:");
  New_Line;

  Principale : for No_Ligne in Typ_No_Ligne loop

    -- numero de ligne pour le calcul
    Put(No_Ligne);
    Put_Line(" : ");

    Put("   ");
    for Indice in Typ_Indice range Typ_Indice'First .. Typ_No_Ligne'Succ(
      No_Ligne) loop

      -- 6 elements par ligne d'ecran
      if ((Indice - 1) mod 6 = 0) and then (Indice /= 1) then
        New_Line;
        Put("-> ");
      end if;

      -- elements de la ligne separes par '/'
      if Indice = Typ_Indice'First then
        Element := 1;
      else
        Element := Table(Typ_No_Ligne'Pred(No_Ligne), Typ_Indice'Pred(Indice)) +
                      Table(Typ_No_Ligne'Pred(No_Ligne), Indice);
      end if;
      Table(No_Ligne, Indice) := Element;
      Put(Element);
      if Indice /= Typ_Indice'Succ(No_Ligne) then
        Put('/');
      end if;
    end loop;

    -- fin du traitement
    New_Line;
    New_Line;
  end loop Principale;

exception

  -- un element est trop grand
  when others =>
    Put_Line("PROBLEME: Degre trop grand.");
    New_Line;
end Pascal;

