with Rnd, My_Io, Normal;
function Rndsok return Integer is
  N : Integer;
begin
  Rnd.Randomize;
  N := Rnd.Int_Random(1, 50);
  My_Io.Put_Line (Normal(N, 3));
  Return N;
end Rndsok;
