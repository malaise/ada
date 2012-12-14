with Rnd, Basic_Proc;
procedure T_Rnd is

  type Couleur is (Bleu, Rouge, Jaune, Violet, Vert, Orange, Blanc, Noir);

  function My_Random is new Rnd.Discr_Random (Couleur);

  Tableau : array (Couleur) of Natural := (others => 0);

  Essai : Couleur;

  Tot : Natural := 0;
  G : Rnd.Generator;
begin
  G.Randomize;
  for I in 1 .. 1_000 loop
    Essai := My_Random (G, Rouge, Blanc);
    Tableau(Essai) := Tableau(Essai) + 1;
  end loop;
  for I in Couleur loop
    Basic_Proc.Put_Output (Couleur'Image (I));
    Basic_Proc.Put_Output (" -> ");
    Basic_Proc.Put_Output (Tableau (I)'Img);
    Tot := Tot + Tableau (I);
    Basic_Proc.New_Line_Output;
  end loop;
  Basic_Proc.Put_Output ("total:" );
  Basic_Proc.Put_Output (Tot'Img);
  Basic_Proc.New_Line_Output;

  loop
    Tot := G.Int_Random (1, 5);
    Basic_Proc.Put_Line_Output (Tot'Img);
    exit when Tot < 1 or else Tot > 5;
  end loop;

end T_Rnd;

