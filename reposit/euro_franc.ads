generic
  type Euros  is digits <>;
  type Francs is digits <>;
package Euro_Franc is

  Francs_In_One_Euro : constant := 6.55957;

  function Francs_To_Euros (Franc : Francs) return Euros;
  function Euros_To_Francs (Euro : Euros)   return Francs;

end Euro_Franc;

