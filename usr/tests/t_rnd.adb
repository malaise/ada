with Ada.Calendar;
with Argument, Argument_Parser, Rnd, Basic_Proc,
     My_Math, Normalization,
     Images, Mixed_Str, Str_Util, As.U.Utils;
procedure T_Rnd is

  Default_Kind : constant Rnd.Kind_List := Rnd.Universal;

  -- The keys
  Dscr : Argument_Parser.Parsed_Dscr;
  Keys : constant Argument_Parser.The_Keys_Type := (
   1 => (False, 'h', As.U.Tus ("help"),   False),
   2 => (True,  'g', As.U.Tus ("gen"),    True,  True, As.U.Tus ("generator")),
   3 => (False, 'c', As.U.Tus ("colors"), False),
   4 => (True,  'm', As.U.Tus ("max"),    False, True, As.U.Tus ("max_int")),
   5 => (False, 's', As.U.Tus ("stats"), False) );

  Helps : constant As.U.Utils.Asu_Array (3 .. 5) := (
    3 => As.U.Tus ("Random colors"),
    4 => As.U.Tus ("Random 0 <= I < max_int"),
    5 => As.U.Tus ("Statistics of distribution"));

  -- The possible modes
  type Mode_List is (Colors, Maxint, Stats);
  Mode : Mode_List;

  -- For Colors mode
  type Color is (Blue, Red, Yellow, Purple, Green, Orange, White, Black);
  Arr : array (Color) of Natural := (others => 0);
  Try : Color;
  function Color_Random is new Rnd.Discr_Random (Color);

  -- For Stats mode
  Max_Rnd : constant := 1_000_000;
  Max_Range : constant := 1_000;
  subtype Val_Range is Positive range 1 .. Max_Range;
  Val : Val_Range;
  Vals : array (Val_Range) of Natural := (others => 0);
  Avg, Tmp, Deviation : My_Math.Real;
  T0 : Ada.Calendar.Time;
  Dur : Ada.Calendar.Day_Duration;

  -- For all and Stats mode
  Total : Natural := 0;

  -- Kind of Rnadom generator
  Kind : Rnd.Kind_List;
  G : access Rnd.Generator;

  procedure Help is
    use type Rnd.Kind_List;
  begin
    Basic_Proc.Put_Line_Output ("Usage: " & Argument.Get_Program_Name
        & " [ -g <generator> | --gen=<generator> ] <mode>");
    Basic_Proc.Put_Output ("  <generator> ::= ");
    for K in Rnd.Kind_List loop
      Basic_Proc.Put_Output (Mixed_Str (K'Img)
          & (if K /= Rnd.Kind_List'Last then " | " else ""));
    end loop;
    Basic_Proc.Put_Line_Output ("  // Default: "
        & Mixed_Str (Default_Kind'Img));
    Basic_Proc.Put_Line_Output ("  <mode> ::=");
    for I in 3 .. 5 loop
      Basic_Proc.Put_Line_Output ("      "
          & Str_Util.Procuste (Argument_Parser.Image (Keys(I)), 37)
          & " // " & Helps(I).Image);
    end loop;
    Basic_Proc.Set_Error_Exit_Code;
  end Help;

  use type My_Math.Real, Ada.Calendar.Time;
begin
  Dscr :=  Argument_Parser.Parse (Keys);
  if not Dscr.Is_Ok then
    Help;
    return;
  end if;

  -- Help
  if Dscr.Is_Set (1) then
    Help;
    return;
  end if;

  -- Get optional kind of generator
  Kind := Default_Kind;
  if Dscr.Is_Set (2) then
    begin
      Kind := Rnd.Kind_List'Value (Dscr.Get_Option (2));
    exception
      when others =>
         Help;
         return;
     end;
  end if;
  G := new  Rnd.Generator (Kind);
  G.Randomize;

  -- Get Mode
  if Dscr.Is_Set (3) then
    if Dscr.Is_Set (4) or else Dscr.Is_Set (5) then
      Help;
      return;
    end if;
    Mode := Colors;
  elsif Dscr.Is_Set (4) then
    if Dscr.Is_Set (5) or else Dscr.Is_Set (3) then
      Help;
      return;
    end if;
    Mode := Maxint;
    begin
      Total := Natural'Value (Dscr.Get_Option (4));
    exception
      when others =>
         Help;
         return;
     end;
  elsif Dscr.Is_Set (5) then
    if Dscr.Is_Set (3) or else Dscr.Is_Set (4) then
      Help;
      return;
    end if;
    Mode := Stats;
  else
    Help;
    return;
  end if;

  -- Process according to mode
  case Mode is
    when Colors =>
      -- Play with colors: random except first and last of enum
      for I in 1 .. 1_000 loop
        Try := Color_Random (G.all, Color'Succ(Color'First),
                                    Color'Pred(Color'Last));
        Arr(Try) := Arr(Try) + 1;
      end loop;
      for I in Color loop
        Basic_Proc.Put_Output (Color'Image (I));
        Basic_Proc.Put_Output (" -> ");
        Basic_Proc.Put_Output (Arr(I)'Img);
        Total := Total + Arr (I);
        Basic_Proc.New_Line_Output;
      end loop;
      Basic_Proc.Put_Line_Output ("Total: " & Images.Integer_Image (Total));
    when Maxint =>
      -- Random Natural from 0 to max_val
      Basic_Proc.Put_Line_Output (Images.Integer_Image (
          G.Int_Random (0, Total)));
    when Stats =>
      -- Display distribution statistics
      -- Random entires
      T0 := Ada.Calendar.Clock;
      for I in 1 .. Max_Rnd loop
        Val := G.Int_Random (1, Max_Range);
        Vals(Val) := Vals(Val) + 1;
      end loop;
      Dur := Ada.Calendar.Clock - T0;
      -- Standard deviation
      Avg := My_Math.Real (Max_Rnd) / My_Math.Real (Max_Range);
      Deviation := 0.0;
      for V of Vals loop
        Tmp := My_Math.Real (V) - Avg;
        Deviation := Deviation + Tmp * Tmp;
      end loop;
      Deviation := My_Math.Sqrt (Deviation / My_Math.Real (Max_Range));
      Basic_Proc.Put_Line_Output ("Standard deviation:"
          & Normalization.Normal_Fixed (Deviation, 6, 3, '0') & " in "
          &  Images.Dur_Image (Dur));
  end case;

end T_Rnd;

