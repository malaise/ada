with Oper_Def;
package Unit_Format is

  type Units_List is (Euros, Francs);
  Default_Unit : constant Units_List := Euros;

  -- Current unit switching
  function Get_Current_Unit return Units_List;
  procedure Set_Unit_To (Unit : Units_List);
  procedure Switch_Unit;

  --  May be raised by any IMAGE/VALUE
  Format_Error : Exception;

  -- Date: 25/10/2001
  subtype Date_Str is String(1 .. 10);
  function Date_Image(Date : Oper_Def.Date_Rec) return Date_Str;
  function Date_Value(Str : Date_Str) return Oper_Def.Date_Rec;

  -- Short date: 25/10/01
  subtype Short_Date_Str is String(1 .. 8);
  function Short_Date_Image(Date : Oper_Def.Date_Rec) return Short_Date_Str;

  -- Short status: Yes No Def
  subtype Short_Status_Str is String(1 .. 3);
  function Short_Status_Image (Status : Oper_Def.Status_List)
           return Short_Status_Str;

  -- Short kind:  Cheq Card Tran Draw
  subtype Short_Kind_Str is String(1 .. 4);
  function Short_Kind_Image (Kind : Oper_Def.Kind_List)
           return Short_Kind_Str;

  -- Amount: -12345678.12
  subtype Amount_Str is String (1 .. 12);
  -- From an amount (in euros) return 'image (euros/francs)
  function Image (Amount_In_Euros : Oper_Def.Amount_Range;
                  Align_Left : in Boolean) return Amount_Str;


  -- From a string (euros/francs) return amount in euros
  function Value (Str : Amount_Str) return Oper_Def.Amount_Range;


  -- Amount of an operation in LIST: -12345.12
  subtype Short_Amount_Str is String (1 .. 9);
  -- From an amount (in euros) return 'image (euros/francs)
  -- Truncation rule:
  --  Sign is kept, three lower digits of unit removed,
  --  cents and dot replaced by " k "
  -- Result has first digit set to '-' or ' ' and aligned on right
  function Short_Image (Amount_In_Euros : Oper_Def.Amount_Range)
                       return Short_Amount_Str;

end Unit_Format;

