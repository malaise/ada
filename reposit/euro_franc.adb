package body Euro_Franc is

  function Francs_To_Euros (Franc : Francs) return Euros is
  begin
    return Euros(Franc / Francs_In_One_Euro);
  end;

  function Euros_To_Francs (Euro : Euros)   return Francs is
  begin
    return Francs(Euro) * Francs_In_One_Euro;
  end;

end Euro_Franc;

