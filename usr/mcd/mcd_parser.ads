with Mcd_Mng;

package Mcd_Parser is

  -- Parse next item from input flow
  Parsing_Error : exception;
  function Next_Item return Mcd_Mng.Item_Rec;

  -- Display help
  procedure Print_Help (Command : in Boolean);

  -- Display instruction stack if debug history is on
  procedure Dump_Stack;

  -- Check if a string can be arbi, frac, inte, real, bool, reg or prog
  -- Raise Mcd_Mng.Invalid_Argument is not Chrs
  -- Return a Bool Item_Rec
  subtype Bool_Rec is Mcd_Mng.Item_Rec (Mcd_Mng.Bool);
  function Can_Arbi (Item : in Mcd_Mng.Item_Rec) return Bool_Rec;
  function Can_Frac (Item : in Mcd_Mng.Item_Rec) return Bool_Rec;
  function Can_Inte (Item : in Mcd_Mng.Item_Rec) return Bool_Rec;
  function Can_Real (Item : in Mcd_Mng.Item_Rec) return Bool_Rec;
  function Can_Bool (Item : in Mcd_Mng.Item_Rec) return Bool_Rec;
  function Can_Reg  (Item : in Mcd_Mng.Item_Rec) return Bool_Rec;
  function Can_Prog (Item : in Mcd_Mng.Item_Rec) return Bool_Rec;

end Mcd_Parser;

