with My_Math, Normal, Round_At;
-- to convert got strings in data fields, and fields in string to be put
package body Nav_Format is

  -- from speed to string
  function Imag (Speed : Nav_Types.T_Speed; Set : Boolean := True)
   return String is
    Lspeed : My_Math.Real;
    Str_Unset : constant String := "     ";
    Str : String (1 .. Str_Unset'Last);
    I : Integer;
    use type My_Math.Real;
  begin
    if Set then
      Lspeed := Round_At (My_Math.Real(Speed), -1);
      Str (1 .. 3) := Normal (Integer (My_Math.Trunc (Lspeed)), 3, True, ' ');
      Str (4) := '.';
      I := Integer (My_Math.Round (My_Math.Frac(Lspeed) * 10.0));
      Str (5 .. 5) := Normal (I, 1, False, '0');
      return Str;
    else
      return Str_Unset;
    end if;
  end Imag;

  -- from angle to string
  function Imag (Angle : Nav_Types.T_Angle; Set : Boolean := True)
   return String is
    Str_Unset : constant String := "      ";
    Str : String (1 .. Str_Unset'Last);
  begin
    if Set then
      Str (1 .. 3) := Normal (Integer (Angle.Degrees), 3, True, ' ');
      Str (4) := '.';
      Str (5 .. 6) := Normal (Integer (Angle.Minutes), 2, True, '0');
      return Str;
    else
      return Str_Unset;
    end if;
  end Imag;

  -- from drift to string
  function Imag (Drift : Nav_Types.T_Drift; Set : Boolean := True)
   return String is
    Str_Unset : constant String := "       ";
    Str : String (1 .. Str_Unset'Last);
  begin
    if Set then
      if Drift.Positiv then
        Str(1) := '+';
      else
        Str(1) := '-';
      end if;
      Str (2 .. 4) := Normal (Integer (Drift.Degrees), 3, True, ' ');
      Str (5) := '.';
      Str (6 .. 7) := Normal (Integer (Drift.Minutes), 2, True, '0');
      return Str;
    else
      return Str_Unset;
    end if;
  end Imag;

  function Is_Digit (C : Character) return Boolean is
  begin
    return C >= '0' and then C <= '9';
  end Is_Digit;


  -- to check generaly the string:
  -- [{' '}] ['+'|'-'] [{' '}] {d} ['.'{d}] [{' '}]
  -- [] : non or once, {} : once or more, | : or, d : digit
  -- returns index of first and last significant character
  -- of ok, pos is index of '.' or 0; if error, it's error position;
  --  if clear, no meaning
  procedure Check (Str : in String;
   First, Last : out Positive; Sign : out Boolean;
   Pos : out Natural; Res : out Format_Result) is
    F, L : Positive;
    First_Digit : Positive;
    Digit_Found : Boolean;
    C : Character;
    Dot : Boolean;
  begin
    First := 1;
    Last := 1;
    Sign := False;
    -- parse leading and tailing spaces
    F := Str'First;
    while F < Str'Last and then Str(F) = ' ' loop
      F := F + 1;
    end loop;
    if Str'Length = 0 or else Str(F) = ' ' then
      -- only spaces : unset
      Pos := Str'First;
      Res := Unset;
      return;
    end if;
    L := Str'Last;
    while L > Str'First and then Str(L) = ' ' loop
      L := L - 1;
    end loop;

    First := F;
    Last := L;
    Sign := False;


    -- first significant char may be + -
    if Str(F) = '+' or else Str(F) = '-' then
      Sign := True;
      -- go on starting from next significant char
      First_Digit := F + 1;
      while First_Digit <= Str'Last and then Str(First_Digit) = ' ' loop
        First_Digit := First_Digit + 1;
      end loop;
    else
      First_Digit := F;
    end if;

   -- general syntax {d} ['.'{d}]
   Dot := False; -- no dot
   Pos := 0;
   for I in First_Digit .. L loop
     C := Str(I);
     if Is_Digit(C) then
       Digit_Found := True;
     elsif C = '.' then
       Pos := I;
       if Dot then
         -- a second dot : Err
         Res := Error;
         return;
       else
         Dot := True;
         if not Digit_Found then
           -- begin with Dot???
           Res := Error;
           return;
         end if;
       end if;
     else
       -- unknown char
       Pos := I;
       Res := Error;
       return;
     end if;
   end loop;

   -- last significant char must be a digit
   if not Is_Digit(C) then
     Pos := L;
     Res := Error;
     return;
   else
     Res := Set;
     return;
   end if;

  end Check;


  -- from string to speed
  procedure Value (Str : in String; Speed : out Nav_Types.T_Speed;
   Res : out Format_Result; Pos : out Positive) is
    F, L : Positive;
    S : Boolean;
    P : Natural;
    R : Format_Result;
    Spe : Float;
  begin
    Pos := 1;
    Check (Str, F, L, S, P, R);
    if R = Unset then
      Res := Unset;
      Pos := 1;
      Speed := 0.0;
      return;
    end if;
    if S then
      -- signed speed forbidden: error at first signif char
      P := F;
      R := Error;
    end if;

    if R = Set and then P /= 0 and then L - P > 1 then
      -- only 1 digit after dot is allowed
      Pos := P + 2;
      Res := Error;
      return;
    end if;
    if R = Error then
      Pos := P;
      Res := R;
      return;
    end if;

    if P = 0 then
      -- no dot
      Spe := Float(Integer'Value( Str(F..L) ));
    else
      -- dot and 1 digit
      Spe := Float(Integer'Value( Str(F..P-1) ));
      Spe := Spe + ( Float(Integer'Value( Str(P+1..L) )) / 10.0);
    end if;
    Speed := Nav_Types.T_Speed (Spe);
    Res := Set;
  end Value;




  -- from string to angle
  procedure Value (Str : in String; Angle : out Nav_Types.T_Angle;
   Res : out Format_Result; Pos : out Positive) is
    F, L : Positive;
    S : Boolean;
    P : Natural;
    R : Format_Result;
  begin
    Pos := 1;
    Check (Str, F, L, S, P, R);
    if R = Unset then
      Res := Unset;
      return;
    end if;
    if S then
      -- signed angle forbidden: error at first signif char
      P := F;
      R := Error;
    end if;

    if R = Set and then P /= 0 and then L - P > 2 then
      -- only 1 or 2 digits after dot is allowed
      Pos := P + 3;
      Res := Error;
      return;
    end if;
    if R = Error then
      Pos := P;
      Res := R;
      return;
    end if;

    if P = 0 then
      -- no dot
      Angle.Degrees := Nav_Types.T_Degree'Value( Str(F..L) );
      Angle.Minutes := 0;
    elsif P = L-1 then
      -- dot and 1 digit which is in 10 minutes
      declare
        use Nav_Types;
      begin
        Angle.Degrees := Nav_Types.T_Degree'Value( Str(F..P-1) );
        Angle.Minutes := Nav_Types.T_Minute'Value( Str(P+1..L) )
         * Nav_Types.T_Minute'(10);
      end;
    else
      -- dot and 2 digits which are minutes
      Angle.Degrees := Nav_Types.T_Degree'Value( Str(F..P-1) );
      Angle.Minutes := Nav_Types.T_Minute'Value( Str(P+1..L) );
    end if;
    Res := Set;
 end Value;

  -- from string to drift
  procedure Value (Str : in String; Drift : out Nav_Types.T_Drift;
   Res : out Format_Result; Pos : out Positive) is
    F, L : Positive;
    S : Boolean;
    P : Natural;
    R : Format_Result;
  begin
    Pos := 1;
    Check (Str, F, L, S, P, R);
    if R = Unset then
      Res := Unset;
      return;
    end if;

    if R = Set and then P /= 0 and then L - P > 2 then
      -- only 1 or 2 digits after dot is allowed
      Pos := P + 3;
      Res := Error;
      return;
    end if;
    if R = Error then
      Pos := P;
      Res := R;
      return;
    end if;

    if S then
      -- signed drift at first signif char
      Drift.Positiv := (Str (F) = '+');
      loop
        F := F + 1;
        exit when Str(F) /= ' ';
      end loop;
    else
      Drift.Positiv := True;
    end if;

    if P = 0 then
      -- no dot
      Drift.Degrees := Nav_Types.T_Deg_Drift'Value( Str(F..L) );
      Drift.Minutes := 0;
    elsif P = L-1 then
      -- dot and 1 digit which is in 10 minutes
      declare
        use Nav_Types;
      begin
        Drift.Degrees := Nav_Types.T_Deg_Drift'Value( Str(F..P-1) );
        Drift.Minutes := Nav_Types.T_Minute'Value( Str(P+1..L) )
         * Nav_Types.T_Minute'(10);
       end;
    else
      -- dot and 2 digits which are minutes
      Drift.Degrees := Nav_Types.T_Deg_Drift'Value( Str(F..P-1) );
      Drift.Minutes := Nav_Types.T_Minute'Value( Str(P+1..L) );
    end if;
    Res := Set;
 end Value;

end Nav_Format;

