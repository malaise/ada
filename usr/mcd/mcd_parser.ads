with Mcd_Mng;

package Mcd_Parser is

  function Next_Item return Mcd_Mng.Item_Rec; 

  Parsing_Error : exception;

  procedure Print_Help;

  procedure Dump_Stack;

end Mcd_Parser;

