-- dependance de taches
with Ada.Text_Io;
procedure Termin is
  use Ada.Text_Io;

  type T_Creation is (Par_Acces, Direct);

  package Creation_Io is new Ada.Text_Io.Enumeration_Io (T_Creation);

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
    Put ("tache "); Creation_Io.Put (Nom, T_Creation'Width);
    Put_Line (" lancee");
    for I in 1..Loops(Nom) loop
      delay 0.5;
      Put ("tache ");Creation_Io.Put (Nom, T_Creation'Width);
      Put_Line (" active");
    end loop;
    Put ("tache "); Creation_Io.Put (Nom, T_Creation'Width);
    Put_Line (" achevee");
  end T_Tache;

begin
  Put_Line ("Une tache depend du bloc ou elle est declaree, sauf une tache "
    & "accedee,");
  Put_Line (" laquelle depend du bloc qui declare le type acces:");
  New_Line (2);

  declare
    Pa : constant T_Ptr_Tache := new T_Tache;
    Pb : T_Tache;
  begin
    Put (Separateur); Put (" Debut bloc "); Put_Line (Separateur);
    Pa.Init (Par_Acces);
    Pb.Init (Direct);
  end;
  Put (Separateur); Put (" Fin   bloc "); Put_Line (Separateur);

end Termin;
