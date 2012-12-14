-- Random number of Sokoban frame (1 .. 50)
with Rnd, Basic_Proc, Normal;
function Rndsok return Integer is
  N : Integer;
begin
  Rnd.Gen.Randomize;
  N := Rnd.Gen.Int_Random(1, 50);
  Basic_Proc.Put_Line_Output (Normal(N, 3));
  return N;
end Rndsok;
