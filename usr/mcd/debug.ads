with Mcd_Mng;
package Debug is

  type Debug_Level_List is (Parser, Input, Call, Stack, Register, Oper,
                            Flow, History);
  Debug_Level_Array : array (Debug_Level_List) of Boolean;

  procedure Init;

  procedure Put (Item : in Mcd_Mng.Item_Rec);

end Debug;

