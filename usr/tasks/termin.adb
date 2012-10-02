-- dependance de taches
with Basic_Proc;
procedure Termin is
  use Basic_Proc;

  type T_Creation is (Par_Acces, Direct);

  Loops : constant array (T_Creation) of Positive := (4, 2);

  -- priorite du programme principal
  pragma Priority (5);

  -- type de tache
  task type T_Tache is
    entry Init (Nom : in T_Creation);
    pragma Priority (1);
  end T_Tache;

  type T_Ptr_Tache is access T_Tache;

  Separateur : constant String := "-------------";

  task body T_Tache is
    Nom : T_Creation;
  begin
    accept Init (Nom : in T_Creation) do
      T_Tache.Nom := Init.Nom;
    end Init;
    Put_Output ("tache "); Put_Output (Nom'Img);
    Put_Line_Output (" lancee");
    for I in 1..Loops(Nom) loop
      delay 0.5;
      Put_Output ("tache "); Put_Output (Nom'Img);
      Put_Line_Output (" active");
    end loop;
    Put_Output ("tache "); Put_Output (Nom'Img);
    Put_Line_Output (" achevee");
  end T_Tache;

begin
  Put_Line_Output ("Une tache depend du bloc ou elle est declaree, sauf une tache "
    & "accedee,");
  Put_Line_Output (" laquelle depend du bloc qui declare le type acces:");
  New_Line_Output;
  New_Line_Output;

  declare
    Pa : constant T_Ptr_Tache := new T_Tache;
    Pb : T_Tache;
  begin
    Put_Output (Separateur); Put_Output (" Debut bloc "); Put_Line_Output (Separateur);
    Pa.Init (Par_Acces);
    Pb.Init (Direct);
  end;
  Put_Output (Separateur); Put_Output (" Fin   bloc "); Put_Line_Output (Separateur);

end Termin;
