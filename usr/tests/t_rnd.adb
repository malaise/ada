with Rnd, My_Io;
use My_Io;
procedure T_Rnd is

  type Couleur is (Bleu, Rouge, Jaune, Violet, Vert, Orange, Blanc, Noir);

  function My_Random is new Rnd.Discr_Random (Couleur);

  Tableau : array (Couleur) of Natural := (others => 0);

  Essai : Couleur;

  Tot : Natural := 0;
begin
  Rnd.Randomize;
  for I in 1 .. 1_000 loop
    Essai := My_Random (Rouge, Blanc);
    Tableau(Essai) := Tableau(Essai) + 1;
  end loop;
  for I in Couleur loop
    Put (Couleur'image (I));
    Put (" -> ");
    Put (Tableau (I));
    Tot := Tot + Tableau (I);
    New_Line;
  end loop;
  Put ("total :" );
  Put (Tot);
  New_Line;
end T_Rnd;
