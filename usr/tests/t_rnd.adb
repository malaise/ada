with RND, MY_IO;
use MY_IO;
procedure T_RND is

  type COULEUR is (BLEU, ROUGE, JAUNE, VIOLET, VERT, ORANGE, BLANC, NOIR);

  function MY_RANDOM is new RND.DISCR_RANDOM (COULEUR);

  TABLEAU : array (COULEUR) of NATURAL := (others => 0);

  ESSAI : COULEUR;

  TOT : NATURAL := 0;
begin
  RND.RANDOMIZE;
  for I in 1 .. 1_000 loop
    ESSAI := MY_RANDOM (ROUGE, BLANC);
    TABLEAU(ESSAI) := TABLEAU(ESSAI) + 1;
  end loop;
  for I in COULEUR loop
    PUT (COULEUR'image (I));
    PUT (" -> ");
    PUT (TABLEAU (I));
    TOT := TOT + TABLEAU (I);
    NEW_LINE;
  end loop;
  PUT ("total :" );
  PUT (TOT);
  NEW_LINE;
end T_RND;
