with Trace.Loggers;
with Mcd_Mng;
package Debug is

  type Level_List is (Parser, Input, Call, Stack, Register, Oper,
                            Flow, History);
  -- Loggers
  Loggers : array (Level_List) of Trace.Loggers.Logger;

  -- Init loggers
  procedure Init;

  -- Shortcut to log
  procedure Log (Level : in Level_List; Message : in String);

  -- Log an optional message then an item
  procedure Log (Level   : in Level_List;
                 Item    : in Mcd_Mng.Item_Rec;
                 Message : in String := "");

end Debug;

