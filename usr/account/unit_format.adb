with Text_Io, Calendar;
with My_Math, Normal, Euro_Franc;
package body Unit_Format is

  Current_Unit : Units_List := Default_Unit;

  package Amount_Io is new Text_Io.Float_Io(Oper_Def.Amount_Range);
  package Real_Io is new Text_Io.Float_Io(My_Math.Real);
  package Inte_Io is new Text_Io.Integer_Io(My_Math.Inte);

  package Mef is new Euro_Franc(Oper_Def.Amount_Range, Oper_Def.Amount_Range);

  -- Date: 25/10/2001
  function Date_Image(Date : Oper_Def.Date_Rec) return Date_Str is
  begin
    return Normal(Date.Day, 2, Gap => '0') & '/'
         & Normal(Date.Month, 2, Gap => '0') & '/'
         & Normal(Date.Year, 4, Gap => '0');
  end Date_Image;

  function Date_Value(Str : Date_Str) return Oper_Def.Date_Rec is
    Date : Oper_Def.Date_Rec;
    Time : Calendar.Time;
  begin
    Date.Day   := Calendar.Day_Number'Value  (Str(1 ..  2));
    Date.Month := Calendar.Month_Number'Value(Str(4 ..  5));
    Date.Year  := Calendar.Year_Number'Value (Str(7 .. 10));
    -- Check validity
    Time := Calendar.Time_Of(Date.Year, Date.Month, Date.Day, 0.0);
    return Date;
  exception
    when others =>
      raise Format_Error;
  end Date_Value;

  -- Short date: 25/10/01
  function Short_Date_Image(Date : Oper_Def.Date_Rec) return Short_Date_Str is
  begin
    return Normal(Date.Day, 2, Gap => '0') & '/'
         & Normal(Date.Month, 2, Gap => '0') & '/'
         & Normal(Date.Year, 4, Gap => '0')(3..4);
  end Short_Date_Image;

  -- Short status: Yes No Def
  function Short_Status_Image (Status : Oper_Def.Status_List)
           return Short_Status_Str is
  begin
    case Status is
      when Oper_Def.Entered =>
        return "Yes";
      when Oper_Def.Not_Entered =>
        return "No ";
      when Oper_Def.Defered =>
        return "Def";
    end case;
  end Short_Status_Image;

  -- Short kind:  Cheq Card Tran Draw
  function Short_Kind_Image (Kind : Oper_Def.Kind_List)
           return Short_Kind_Str is
  begin
    case Kind is
      when Oper_Def.Cheque =>
        return "Cheq";
      when Oper_Def.Credit =>
        return "Cred";
      when Oper_Def.Transfer =>
        return "Xfer";
      when Oper_Def.Withdraw =>
        return "Draw";
    end case;
  end Short_Kind_Image;

  -- Current unit switching
  function Get_Current_Unit return Units_List is
  begin
    return Current_Unit;
  end Get_Current_Unit;

  procedure Set_Unit_To (Unit : Units_List) is
  begin
    Current_Unit := Unit;
  end Set_Unit_To;

  procedure Switch_Unit is
  begin
    if Current_Unit = Euros then
      Current_Unit := Francs;
    else
      Current_Unit := Euros;
    end if;
  end Switch_Unit;

  function First_Dig (Str : String) return Positive is
  begin
    for I in Str'Range loop
      if Str(I) /= ' ' then return I; end if;
    end loop;
    raise Format_Error;
  end First_Dig;

  function Last_Dig (Str : String) return Positive is
  begin
    for I in reverse Str'Range loop
      if Str(I) /= ' ' then return I; end if;
    end loop;
    raise Format_Error;
  end Last_Dig;

  -- Amount: -12345678.12
  -- subtype AMOUNT_STR is STRING (1 .. 12);
  -- From an amount (in euros) return 'image (euros/francs)
  function Image (Amount_In_Euros : Oper_Def.Amount_Range;
                  Align_Left : in Boolean) return Amount_Str is
    Str, Str_Ret : Amount_Str;
    Amount_In_Unit : Oper_Def.Amount_Range;
    First : Positive;
  begin
    if Get_Current_Unit = Euros then
      Amount_In_Unit := Amount_In_Euros;
    else
      Amount_In_Unit := Mef.Euros_To_Francs(Amount_In_Euros);
    end if;
    Str := (others => ' ');
    Amount_Io.Put(Str, Amount_In_Unit, 2, 0);
    if Align_Left then
      -- Put string at the beginning
      Str_Ret := (others => ' ');
      First := First_Dig(Str);
      Str_Ret(1 .. Amount_Str'Last - First + 1) :=
           Str(First .. Amount_Str'Last);
    else
      Str_Ret := Str;
    end if;
    return Str_Ret;
  exception
    when others =>
      raise Format_Error;
  end Image;

  -- From a string (euros/francs) return amount in euros
  function Value (Str : Amount_Str) return Oper_Def.Amount_Range is
    Amount_In_Unit : Oper_Def.Amount_Range;


    function Has_Dot (S : String) return Boolean is
    begin
      for I in S'Range loop
        if S(I) = '.' then return True; end if;
      end loop;
      return False;
    end Has_Dot;
        
  begin
    -- Get amount or int from significant characters
    declare
      -- Strip blancs
      Tmp : constant String := Str(First_Dig(Str) .. Last_Dig(Str));
      I : My_Math.Inte;
      Last : Positive;
    begin
      if Has_Dot(Tmp) then
        Amount_Io.Get(Tmp, Amount_In_Unit, Last);
      else
        Inte_Io.Get(Tmp, I, Last);
        Amount_In_Unit := Oper_Def.Amount_Range(I);
      end if;
      if Last /= Tmp'Last then
        raise Format_Error;
      end if;
    end;
    -- Convert if needed
    if Get_Current_Unit = Euros then
      return Amount_In_Unit;
    else
      return Mef.Francs_To_Euros(Amount_In_Unit);
    end if;
  exception
    when others =>
      raise Format_Error;
  end Value;

  -- Amount of an operation in LIST: -12345.12
  -- subtype SHORT_AMOUNT_STR is STRING (1 .. 9);
  -- From an amount (in euros) return 'image (euros/francs)
  -- Truncation rule:
  --  Sign is kept, three lower digits of unit removed,
  --  cents and dot replaced by " k "
  function Short_Image (Amount_In_Euros : Oper_Def.Amount_Range)
                       return Short_Amount_Str is
    Str : Amount_Str;
    First : Positive;
  begin
    -- Get full string in proper unit
    Str := Image(Amount_In_Euros, False);

    -- Look for first digit
    First := First_Dig(Str);

    -- Enough space?
    if First < 4 then
      -- No
      declare
        Last : constant := Str'Last;
        Dot : constant := Last -2;
        R : My_Math.Real;
        L : Positive;
        I : My_Math.Inte;
        use My_Math;
      begin
        -- Get INT value
        Real_Io.Get(Str, R, L);
        R := R / 1000.0;
        I := My_Math.Round(R);
        Str (4 .. Dot-1) := Normal(Integer(I), Dot-4);
    
        -- Remove digits
        Str(Dot .. Last) := " k ";
      end;
    end if; 
  
    return Str(4 .. Str'Last);
  exception
    when others =>
      raise Format_Error;
  end Short_Image;

end Unit_Format;

