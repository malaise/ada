with Smart_Reference;
package T_Smart_Int is

  procedure Set (Dest : in out Integer; Val : in Integer);
  procedure Fin (Val : in Integer);
  package Int_Ref is new Smart_Reference (Integer, Set, Fin);

end T_Smart_Int;

