with Argument, Rnd, Basic_Proc, Images;
procedure T_Rnd is

  type Couleur is (Bleu, Rouge, Jaune, Violet, Vert, Orange, Blanc, Noir);

  function My_Random is new Rnd.Discr_Random (Couleur);

  Tableau : array (Couleur) of Natural := (others => 0);

  Essai : Couleur;

  Tot : Natural := 0;
  G : Rnd.Generator;
begin
  G.Randomize;
  if Argument.Get_Nbre_Arg = 1 then
    Basic_Proc.Put_Line_Output (Images.Integer_Image (
        G.Int_Random (0, Integer'Value (Argument.Get_Parameter))));
    return;
  end if;
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

end T_Rnd;

