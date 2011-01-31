with Ada.Calendar;
package Oper_Def is

  subtype Oper_Range is Natural range 0 .. 9999;
  subtype Oper_Index is Positive range 1 .. Oper_Range'Last;

  -- Date
  type Date_Rec is record
    Year : Ada.Calendar.Year_Number := 2001;
    Month : Ada.Calendar.Month_Number := 08;
    Day   : Ada.Calendar.Day_Number := 21;
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
  type Kind_List is (Cheque, Credit, Transfer, Withdraw, Savings);

  -- Oper status
  type Status_List is (Entered, Not_Entered, Defered);


  -- Can an oper of a given kind have a given status
  Kind_Can_Be : constant array (Kind_List, Status_List) of Boolean
  --                           Entered  NotEntered Defered
              := (Cheque   => (True,    True,      False),
                  Credit   => (True,    False,     True),
                  Transfer => (True,    True,      False),
                  Withdraw => (True,    True,      False),
                  Savings  => (False,   True,      False));

  -- Oper strings
  subtype Destination_Str is Wide_String (1 .. 20);
  subtype Comment_Str is Wide_String (1 .. 20);
  subtype Reference_Str is Wide_String (1 .. 10);

  -- Oper
  type Oper_Rec is record
    Date : Date_Rec;
    Amount : Amount_Range := 22.22;
    Kind : Kind_List := Kind_List'First;
    Status : Status_List := Status_List'First;
    Destination : Destination_Str := (others => '0');
    Comment : Comment_Str := (others => '0');
    Reference : Reference_Str := (others => '1');
  end record;

  -- May differ, adapt convert in this case
  type Read_Oper_Rec is new Oper_Rec;

  -- Convertion from read oper to new (current) one
  procedure Convert (Read_Oper : in Oper_Def.Read_Oper_Rec;
                     Oper      : out Oper_Def.Oper_Rec);

  -- Criteria for sorting opers by dates
  function Before (Oper_1, Oper_2 : Oper_Rec) return Boolean;

  -- Criteria for sorting opers by amounts
  function Smaller (Oper_1, Oper_2 : Oper_Rec) return Boolean;

end Oper_Def;

