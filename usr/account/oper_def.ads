with Calendar;
package Oper_Def is

  subtype Oper_Range is Natural;
  subtype Oper_Index is Positive;

  -- Date
  type Date_Rec is record
    Year : Calendar.Year_Number := 2001;
    Month : Calendar.Month_Number := 08;
    Day   : Calendar.Day_Number := 21;
  end record;

  function Current_Date return Date_Rec;

  -- Oper amount
  type Amount_Range is digits 13 range -99_999_999.99999 .. 99_999_999.99999;

  -- An amount computed
  type Amount_Rec is record
    Amount : Oper_Def.Amount_Range := 0.0;
    Overflow : Boolean := False;
  end record;

  -- Oper kind
  type Kind_List is (Cheque, Credit, Transfer, Withdraw);

  -- Oper status
  type Status_List is (Entered, Not_Entered, Defered);

  -- Can an oper of kind be defered
  Kind_Can_Be_Defered : constant array (Kind_List) of Boolean
                      := (Cheque   => False,
                          Credit   => True,
                          Transfer => False,
                          Withdraw => False);


  -- Oper strings
  subtype Reference_Str is String (1 .. 10);
  subtype Destination_Str is String (1 .. 20);
  subtype Comment_Str is String (1 .. 20);

  -- Oper
  type Oper_Rec is record
    Date : Date_Rec;
    Amount : Amount_Range := 21.21;
    Kind : Kind_List := Kind_List'First; 
    Status : Status_List := Status_List'First;
    Destination : Destination_Str := (others => '0');
    Comment : Comment_Str := (others => '0');
    Reference : Reference_Str := (others => '1');
  end record;

  -- Criteria for sorting opers: dates
  function Before (Oper_1, Oper_2 : Oper_Rec) return Boolean;

end Oper_Def;

