with Ada.Calendar;
with My_Math, Normal, Euro_Franc, Language, Normalization;
package body Unit_Format is

  Current_Unit : Units_List := Default_Unit;

  package Mef is new Euro_Franc(Oper_Def.Amount_Range, Oper_Def.Amount_Range);

  -- Date: 25/10/2001
  function Date_Image(Date : Oper_Def.Date_Rec) return Date_Str is
    -- Bug in Gnat4.0
    -- Returning Normal & Normal & Normal produces corrupted string
    -- Using and intermediate variable solves the Pb.
    Result : Date_Str;
  begin
    Result := Normal(Date.Day, 2, Gap => '0') & '/'
            & Normal(Date.Month, 2, Gap => '0') & '/'
            & Normal(Date.Year, 4, Gap => '0');
    return Result;
  end Date_Image;

  function Date_Value(Str : Date_Str) return Oper_Def.Date_Rec is
    Date : Oper_Def.Date_Rec;
    Time : Ada.Calendar.Time;
    pragma Unreferenced (Time);
  begin
    Date.Day   := Ada.Calendar.Day_Number'Value  (Str(1 ..  2));
    Date.Month := Ada.Calendar.Month_Number'Value(Str(4 ..  5));
    Date.Year  := Ada.Calendar.Year_Number'Value (Str(7 .. 10));
    -- Check validity
    Time := Ada.Calendar.Time_Of(Date.Year, Date.Month, Date.Day, 0.0);
    return Date;
  exception
    when others =>
      raise Format_Error;
  end Date_Value;

  -- Short date: 25/10/01
  function Short_Date_Image(Date : Oper_Def.Date_Rec) return Short_Date_Str is
    -- Bug in Gnat4.0
    -- Returning Normal & Normal & Normal produces corrupted string
    -- Using and intermediate variable solves the Pb.
    Result : Short_Date_Str;
  begin
    Result := Normal(Date.Day, 2, Gap => '0') & '/'
            & Normal(Date.Month, 2, Gap => '0') & '/'
            & Normal(Date.Year, 4, Gap => '0')(3..4);
    return Result;
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

  function Short_Status_Value (Str : Short_Status_Str)
           return Oper_Def.Status_List is
  begin
    if Str = "Yes" then
      return Oper_Def.Entered;
    elsif Str = "No " then
      return Oper_Def.Not_Entered;
    elsif  Str = "Def" then
      return Oper_Def.Defered;
    else
      raise Format_Error;
    end if;
  end Short_Status_Value;

  -- Short kind:Cheq Card Tran Draw
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
      when Oper_Def.Savings =>
        return "Save";
    end case;
  end Short_Kind_Image;

  function Short_Kind_Value (Str : Short_Kind_Str)
           return Oper_Def.Kind_List is
  begin
    if Str = "Cheq" then
      return Oper_Def.Cheque;
    elsif Str = "Cred" then
      return Oper_Def.Credit;
    elsif Str = "Xfer" then
      return Oper_Def.Transfer;
    elsif Str = "Draw" then
      return Oper_Def.Withdraw;
    else
      raise Format_Error;
    end if;
  end Short_Kind_Value;


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
  -- subtype Amount_Str is String (1 .. 12);
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
    Str := Normalization.Normal_Fixed (My_Math.Real (Amount_In_Unit),
                                       Amount_Str'Length, 9, 'O');
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
      R : My_Math.Real;
      I : My_Math.Inte;
    begin
      if Has_Dot(Tmp) then
        R := My_Math.Get (Tmp);
        Amount_In_Unit := Oper_Def.Amount_Range(R);
      else
        I := My_Math.Get (Tmp);
        Amount_In_Unit := Oper_Def.Amount_Range(I);
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

  -- Amount of an operation in List: -12345.12
  -- subtype Short_Amount_Str is String (1 .. 9);
  -- From an amount (in euros) return 'image (euros/francs)
  -- Truncation rule:
  --  Sign is kept, three lower digits of unit removed,
  --  cents and dot replaced by " k"
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
        I : My_Math.Inte;
        use My_Math;
      begin
        -- Get Int value
        R := My_Math.Get (Str);
        R := R / 1000.0;
        I := My_Math.Round(R);
        Str(4 .. Dot) := Normal (Integer(I), Dot-3);

        -- Remove digits
        Str(Dot+1 .. Last) := " k";
      end;
    end if;

    return Str(4 .. Str'Last);
  exception
    when others =>
      raise Format_Error;
  end Short_Image;

  -- Full operation image/value
  function Image (Rec : Oper_Def.Oper_Rec) return Oper_Str is
  begin
    return Language.String_To_Wide (Date_Image(Rec.Date)
                                  & Image(Rec.Amount, True)
                                  & Short_Kind_Image(Rec.Kind)
                                  & Short_Status_Image(Rec.Status))
         & Rec.Destination
         & Rec.Comment
         & Rec.Reference;
  end Image;

  function Value (Str : Oper_Str) return Oper_Def.Oper_Rec is
    Oper : Oper_Def.Oper_Rec;
    Index : Positive;
  begin
    Index := 1;

    Oper.Date := Date_Value (Language.Wide_To_String (
           Str(Index .. Index + Date_Str'Length - 1)));
    Index := Index + Date_Str'Length;

    Oper.Amount := Value(Language.Wide_To_String (
           Str(Index .. Index + Amount_Str'Length - 1)));
    Index := Index + Amount_Str'Length;

    Oper.Kind := Short_Kind_Value (Language.Wide_To_String (
           Str(Index .. Index + Short_Kind_Str'Length - 1)));
    Index := Index + Short_Kind_Str'Length;

    Oper.Status := Short_Status_Value (Language.Wide_To_String (
           Str(Index .. Index + Short_Status_Str'Length - 1)));
    Index := Index + Short_Status_Str'Length;

    Oper.Destination := Str
                 (Index .. Index + Oper_Def.Destination_Str'Length - 1);
    Index := Index + Oper_Def.Destination_Str'Length;

    Oper.Comment := Str
                 (Index .. Index + Oper_Def.Comment_Str'Length - 1);
    Index := Index + Oper_Def.Comment_Str'Length;

    Oper.Reference := Str
                 (Index .. Index + Oper_Def.Reference_Str'Length - 1);
    return Oper;
  exception
    when others =>
      raise Format_Error;
  end Value;

end Unit_Format;

