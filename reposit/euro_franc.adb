package body Euro_Franc is

  function Francs_To_Euros (Franc : Francs) return Euros is
  begin
    return Euros(Franc / Francs_In_One_Euro);
  end Francs_To_Euros;

  function Euros_To_Francs (Euro : Euros)   return Francs is
  begin
    return Francs(Euro) * Francs_In_One_Euro;
  end Euros_To_Francs;

end Euro_Franc;

