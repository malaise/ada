package body Euro_Franc is

  function Francs_To_Euros (Franc : Francs) return Euros is
    (Euros(Franc / Francs_In_One_Euro));

  function Euros_To_Francs (Euro : Euros)   return Francs is
    (Francs(Euro) * Francs_In_One_Euro);

end Euro_Franc;

