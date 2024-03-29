with Bit_Ops;
separate (Mcd_Mng)

package body Operations is
  use all type My_Math.Real;

  function Is_True (X : Item_Rec) return Boolean is
  begin
    if X.Kind /= Bool then
      raise Invalid_Argument;
    end if;
    return X.Val_Bool;
  end Is_True;

  function Is_Arbi_Or_Inte_Or_Real (X : Item_Rec) return Boolean is
    (X.Kind = Arbi or else X.Kind = Inte or else X.Kind = Real);

  function Is_Arbi_Or_Frac_Or_Inte_Or_Real (X : Item_Rec) return Boolean is
    (X.Kind = Arbi or else X.Kind = Frac
     or else X.Kind = Inte or else X.Kind = Real);

  function Is_Arbi_Or_Inte (X : Item_Rec) return Boolean is
    (X.Kind = Arbi or else X.Kind = Inte);

  function Is_Arbi_Or_Frac_Or_Inte_Or_Real_Or_Bool_Or_Chars_Or_Regi
    (X : Item_Rec) return Boolean is
    (X.Kind = Arbi or else X.Kind = Frac
     or else X.Kind = Inte or else X.Kind = Real
     or else X.Kind = Bool or else X.Kind = Chrs or else X.Kind = Regi);

  -- Arbi,Arbi->Arbi or Frac,Frac->Frac or Inte,Inte->Inte or Real,Real->Real
  function Add     (L, R : Item_Rec) return Item_Rec is
    use type Arbitrary.Number, Arbitrary.Fractions.Fraction;
  begin
    if not Is_Arbi_Or_Frac_Or_Inte_Or_Real(L)
    or else not Is_Arbi_Or_Frac_Or_Inte_Or_Real(R) then
      raise Invalid_Argument;
    end if;
    if L.Kind /= R.Kind then
      raise Argument_Mismatch;
    end if;
    if L.Kind = Arbi then
      return (Kind => Arbi, Val_Arbi => L.Val_Arbi + R.Val_Arbi);
    elsif L.Kind = Frac then
      return (Kind => Frac, Val_Frac => L.Val_Frac + R.Val_Frac);
    elsif L.Kind = Inte then
      return (Kind => Inte, Val_Inte => L.Val_Inte + R.Val_Inte);
    else
      return (Kind => Real, Val_Real => L.Val_Real + R.Val_Real);
    end if;
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Add;

  function Sub     (L, R : Item_Rec) return Item_Rec is
    use type Arbitrary.Number, Arbitrary.Fractions.Fraction;
  begin
    if not Is_Arbi_Or_Frac_Or_Inte_Or_Real(L)
    or else not Is_Arbi_Or_Frac_Or_Inte_Or_Real(R) then
      raise Invalid_Argument;
    end if;
    if L.Kind /= R.Kind then
      raise Argument_Mismatch;
    end if;
    if L.Kind = Arbi then
      return (Kind => Arbi, Val_Arbi => L.Val_Arbi - R.Val_Arbi);
    elsif L.Kind = Frac then
      return (Kind => Frac, Val_Frac => L.Val_Frac - R.Val_Frac);
    elsif L.Kind = Inte then
      return (Kind => Inte, Val_Inte => L.Val_Inte - R.Val_Inte);
    else
      return (Kind => Real, Val_Real => L.Val_Real - R.Val_Real);
    end if;
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Sub;

  function Mult    (L, R : Item_Rec) return Item_Rec is
    use type Arbitrary.Number, Arbitrary.Fractions.Fraction;
  begin
    if not Is_Arbi_Or_Frac_Or_Inte_Or_Real(L)
    or else not Is_Arbi_Or_Frac_Or_Inte_Or_Real(R) then
      raise Invalid_Argument;
    end if;
    if L.Kind /= R.Kind then
      raise Argument_Mismatch;
    end if;
    if L.Kind = Arbi then
      return (Kind => Arbi, Val_Arbi => L.Val_Arbi * R.Val_Arbi);
    elsif L.Kind = Frac then
      return (Kind => Frac, Val_Frac => L.Val_Frac * R.Val_Frac);
    elsif L.Kind = Inte then
      return (Kind => Inte, Val_Inte => L.Val_Inte * R.Val_Inte);
    else
      return (Kind => Real, Val_Real => L.Val_Real * R.Val_Real);
    end if;
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Mult;

  function Div     (L, R : Item_Rec) return Item_Rec is
    use type Arbitrary.Number, Arbitrary.Fractions.Fraction;
  begin
    if not Is_Arbi_Or_Frac_Or_Inte_Or_Real(L)
    or else not Is_Arbi_Or_Frac_Or_Inte_Or_Real(R) then
      raise Invalid_Argument;
    end if;
    if L.Kind /= R.Kind then
      raise Argument_Mismatch;
    end if;
    if L.Kind = Arbi then
      return (Kind => Arbi, Val_Arbi => L.Val_Arbi / R.Val_Arbi);
    elsif L.Kind = Frac then
      return (Kind => Frac, Val_Frac => L.Val_Frac / R.Val_Frac);
    elsif L.Kind = Inte then
      return (Kind => Inte, Val_Inte => L.Val_Inte / R.Val_Inte);
    else
      return (Kind => Real, Val_Real => L.Val_Real / R.Val_Real);
    end if;
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Div;

  function Roundiv (L, R : Item_Rec) return Item_Rec is
    Plus : Boolean;
    Res : Item_Rec;
    use type Arbitrary.Fractions.Fraction;
  begin
    if not Is_Arbi_Or_Frac_Or_Inte_Or_Real(L)
    or else not Is_Arbi_Or_Frac_Or_Inte_Or_Real(R) then
      raise Invalid_Argument;
    end if;
    if L.Kind /= R.Kind then
      raise Argument_Mismatch;
    end if;
    if L.Kind = Arbi then
      return (Kind => Arbi,
              Val_Arbi => Arbitrary.Roundiv (L.Val_Arbi, R.Val_Arbi));
    elsif L.Kind = Frac then
      Res := (Kind => Frac, Val_Frac => L.Val_Frac / R.Val_Frac);
      -- Roundiv (N/D) : 1
      Res.Val_Frac := Arbitrary.Fractions.Set (
        Numerator => Arbitrary.Roundiv (
          Arbitrary.Fractions.Numerator (Res.Val_Frac),
          Arbitrary.Fractions.Denominator (Res.Val_Frac)),
        Denominator => Arbitrary.One);
      return Res;
    elsif L.Kind = Inte then
      return (Kind => Inte,
              Val_Inte => My_Math.Roundiv (L.Val_Inte,  R.Val_Inte));
    else
      Res := (Kind => Real, Val_Real => L.Val_Real / R.Val_Real);
      -- See if result rounds to Int or Int+1
      Plus := My_Math.Frac (abs Res.Val_Real) >= 0.5;
      Res.Val_Real := My_Math.Int (Res.Val_Real);
      if Plus then
        if Res.Val_Real >= 0.0 then
          Res.Val_Real := Res.Val_Real + 1.0;
        else
          Res.Val_Real := Res.Val_Real - 1.0;
        end if;
      end if;
      return Res;
    end if;
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Roundiv;

  -- Arbi,Arbi->Arbi or Frac,Arbi->Frac or Inte,Inte->Inte or Real,Real->Real
  function Pow     (L, R : Item_Rec) return Item_Rec is
    use type Arbitrary.Number, Arbitrary.Fractions.Fraction;
  begin
    if not Is_Arbi_Or_Frac_Or_Inte_Or_Real(L)
    or else not Is_Arbi_Or_Inte_Or_Real(R) then
      raise Invalid_Argument;
    end if;
    if L.Kind = Arbi and then R.Kind = Arbi then
      if R.Val_Arbi < Arbitrary.Zero then
        raise Invalid_Argument;
      end if;
      return (Kind => Arbi, Val_Arbi => L.Val_Arbi ** R.Val_Arbi);
    elsif L.Kind = Frac and then R.Kind = Arbi then
      return (Kind => Frac, Val_Frac => L.Val_Frac ** R.Val_Arbi);
    elsif L.Kind = Inte and then R.Kind = Inte then
      if R.Val_Inte < 0 then
        raise Invalid_Argument;
      end if;
      return (Kind => Inte, Val_Inte => L.Val_Inte ** Natural(R.Val_Inte));
    elsif L.Kind = Real and then R.Kind = Inte then
      return (Kind => Real,
              Val_Real => My_Math.Real(Float(L.Val_Real)
                                    ** Integer(R.Val_Inte)));
    elsif L.Kind = Real and then R.Kind = Real then
      return (Kind => Real, Val_Real => L.Val_Real ** R.Val_Real);
    else
      raise Argument_Mismatch;
    end if;
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Pow;

  -- Arbi,Arbi->Arbi or Inte,Inte->Inte
  function Remind  (L, R : Item_Rec) return Item_Rec is
    use type Arbitrary.Number;
  begin
    if not Is_Arbi_Or_Inte(L) or else not Is_Arbi_Or_Inte(R) then
      raise Invalid_Argument;
    end if;
    if L.Kind = Arbi then
      return (Kind => Arbi, Val_Arbi => L.Val_Arbi rem R.Val_Arbi);
    else
      return (Kind => Inte, Val_Inte => L.Val_Inte rem R.Val_Inte);
    end if;
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Remind;

  -- Inte->Inte
  function Bitneg  (X : Item_Rec) return Item_Rec is
    use Bit_Ops;
  begin
    if X.Kind /= Inte then
      raise Invalid_Argument;
    end if;
    return (Kind => Inte, Val_Inte => not X.Val_Inte);
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Bitneg;

  -- Inte,Inte->Inte
  function Bitand  (L, R : Item_Rec) return Item_Rec is
    use Bit_Ops;
  begin
    if L.Kind /= Inte or else R.Kind /= Inte then
      raise Invalid_Argument;
    end if;
    return (Kind => Inte, Val_Inte => L.Val_Inte and R.Val_Inte);
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Bitand;

  function Bitor   (L, R : Item_Rec) return Item_Rec is
    use Bit_Ops;
  begin
    if L.Kind /= Inte or else R.Kind /= Inte then
      raise Invalid_Argument;
    end if;
    return (Kind => Inte, Val_Inte => L.Val_Inte or R.Val_Inte);
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Bitor;

  function Bitxor  (L, R : Item_Rec) return Item_Rec is
    use Bit_Ops;
  begin
    if L.Kind /= Inte or else R.Kind /= Inte then
      raise Invalid_Argument;
    end if;
    return (Kind => Inte, Val_Inte => L.Val_Inte xor R.Val_Inte);
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Bitxor;

  function Shl     (L, R : Item_Rec) return Item_Rec is
    use Bit_Ops;
  begin
    if L.Kind /= Inte or else R.Kind /= Inte then
      raise Invalid_Argument;
    end if;
    return (Kind => Inte, Val_Inte => Shl(L.Val_Inte, Integer(R.Val_Inte)));
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Shl;

  function Shr     (L, R : Item_Rec) return Item_Rec is
    use Bit_Ops;
  begin
    if L.Kind /= Inte or else R.Kind /= Inte then
      raise Invalid_Argument;
    end if;
    return (Kind => Inte, Val_Inte => Shr(L.Val_Inte, Integer(R.Val_Inte)));
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Shr;

  -- Arbi->Arbi or Frac->Frac or Inte->Inte or Real->Real
  function Minus   (X : Item_Rec) return Item_Rec is
    use type Arbitrary.Number, Arbitrary.Fractions.Fraction;
  begin
    if not Is_Arbi_Or_Frac_Or_Inte_Or_Real(X) then
      raise Invalid_Argument;
    end if;
    if X.Kind = Arbi then
      return (Kind => Arbi, Val_Arbi => - X.Val_Arbi);
    elsif X.Kind = Frac then
      return (Kind => Frac, Val_Frac => - X.Val_Frac);
    elsif X.Kind = Inte then
      return (Kind => Inte, Val_Inte => - X.Val_Inte);
    else
      return (Kind => Real, Val_Real => - X.Val_Real);
    end if;
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Minus;

  -- Arbi->Arbi or Frac->Frac or Inte->Inte or Real->Real
  function Absv   (X : Item_Rec) return Item_Rec is
    use type Arbitrary.Number, Arbitrary.Fractions.Fraction;
  begin
    if not Is_Arbi_Or_Frac_Or_Inte_Or_Real(X) then
      raise Invalid_Argument;
    end if;
    if X.Kind = Arbi then
      return (Kind => Arbi, Val_Arbi => abs X.Val_Arbi);
    elsif X.Kind = Frac then
      return (Kind => Frac, Val_Frac => abs X.Val_Frac);
    elsif X.Kind = Inte then
      return (Kind => Inte, Val_Inte => abs X.Val_Inte);
    else
      return (Kind => Real, Val_Real => abs X.Val_Real);
    end if;
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Absv;

  -- Arbi->Arbi or Inte->Real
  -- X!
  function Fact   (X : Item_Rec) return Item_Rec is
    One_Arbi, I_Arbi, R_Arbi : Arbitrary.Number;
    Res_Real : My_Math.Real;
    use type Arbitrary.Number;
  begin
    if X.Kind /= Arbi and then X.Kind /= Inte then
      raise Invalid_Argument;
    end if;
    if X.Kind = Arbi then
      if X.Val_Arbi < Arbitrary.Zero then
        raise Invalid_Argument;
      end if;
      One_Arbi := Arbitrary.One;
      R_Arbi := One_Arbi;
      I_Arbi := One_Arbi;
      while I_Arbi <= X.Val_Arbi loop
        R_Arbi := R_Arbi * I_Arbi;
        I_Arbi := I_Arbi + One_Arbi;
      end loop;
      return (Kind => Arbi, Val_Arbi => R_Arbi);
    else
      if X.Val_Inte < 0 then
        raise Invalid_Argument;
      end if;
      Res_Real := 1.0;
      for I in 1 .. X.Val_Inte loop
        Res_Real := Res_Real * My_Math.Real(I);
      end loop;
      return (Kind => Real, Val_Real => Res_Real);
    end if;
  exception
    when Invalid_Argument =>
      raise;
    when others =>
      raise Compute_Error;
  end Fact;

  -- Arbi,Arbi->Bool or Frac,Frac->Bool or Inte,Inte->Bool or Real,Real->Bool or
  -- Bool,Bool->Bool or Regi,Regi->Bool or Chars,Chars->Bool
  function Equal   (L, R : Item_Rec) return Item_Rec is
    use type As.U.Asu_Us, Arbitrary.Number, Arbitrary.Fractions.Fraction;
  begin
    if      not Is_Arbi_Or_Frac_Or_Inte_Or_Real_Or_Bool_Or_Chars_Or_Regi(L)
    or else not Is_Arbi_Or_Frac_Or_Inte_Or_Real_Or_Bool_Or_Chars_Or_Regi(R) then
      raise Invalid_Argument;
    end if;
    if L.Kind /= R.Kind then
      raise Argument_Mismatch;
    end if;
    if L.Kind = Arbi then
      return (Kind => Bool, Val_Bool => L.Val_Arbi = R.Val_Arbi);
    elsif L.Kind = Frac then
      return (Kind => Bool, Val_Bool => L.Val_Frac = R.Val_Frac);
    elsif L.Kind = Inte then
      return (Kind => Bool, Val_Bool => L.Val_Inte = R.Val_Inte);
    elsif L.Kind = Real then
      return (Kind => Bool, Val_Bool => L.Val_Real = R.Val_Real);
    elsif L.Kind = Bool then
      return (Kind => Bool, Val_Bool => L.Val_Bool = R.Val_Bool);
    elsif L.Kind = Regi then
      return (Kind => Bool, Val_Bool => L.Val_Regi = R.Val_Regi);
    else
      return (Kind => Bool, Val_Bool => L.Val_Text = R.Val_Text);
    end if;
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Equal;

  function Diff    (L, R : Item_Rec) return Item_Rec is
    use type As.U.Asu_Us, Arbitrary.Number, Arbitrary.Fractions.Fraction;
  begin
    if      not Is_Arbi_Or_Frac_Or_Inte_Or_Real_Or_Bool_Or_Chars_Or_Regi(L)
    or else not Is_Arbi_Or_Frac_Or_Inte_Or_Real_Or_Bool_Or_Chars_Or_Regi(R) then
      raise Invalid_Argument;
    end if;
    if L.Kind /= R.Kind then
      raise Argument_Mismatch;
    end if;
    if L.Kind = Arbi then
      return (Kind => Bool, Val_Bool => L.Val_Arbi /= R.Val_Arbi);
    elsif L.Kind = Frac then
      return (Kind => Bool, Val_Bool => L.Val_Frac /= R.Val_Frac);
    elsif L.Kind = Inte then
      return (Kind => Bool, Val_Bool => L.Val_Inte /= R.Val_Inte);
    elsif L.Kind = Real then
      return (Kind => Bool, Val_Bool => L.Val_Real /= R.Val_Real);
    elsif L.Kind = Bool then
      return (Kind => Bool, Val_Bool => L.Val_Bool /= R.Val_Bool);
    elsif L.Kind = Regi then
      return (Kind => Bool, Val_Bool => L.Val_Regi /= R.Val_Regi);
    else
      return (Kind => Bool, Val_Bool => L.Val_Text /= R.Val_Text);
    end if;
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Diff;

  function Greater (L, R : Item_Rec) return Item_Rec is
    use type As.U.Asu_Us, Arbitrary.Number, Arbitrary.Fractions.Fraction;
  begin
    if      not Is_Arbi_Or_Frac_Or_Inte_Or_Real_Or_Bool_Or_Chars_Or_Regi(L)
    or else not Is_Arbi_Or_Frac_Or_Inte_Or_Real_Or_Bool_Or_Chars_Or_Regi(R) then
      raise Invalid_Argument;
    end if;
    if L.Kind /= R.Kind then
      raise Argument_Mismatch;
    end if;
    if L.Kind = Arbi then
      return (Kind => Bool, Val_Bool => L.Val_Arbi > R.Val_Arbi);
    elsif L.Kind = Frac then
      return (Kind => Bool, Val_Bool => L.Val_Frac > R.Val_Frac);
    elsif L.Kind = Inte then
      return (Kind => Bool, Val_Bool => L.Val_Inte > R.Val_Inte);
    elsif L.Kind = Real then
      return (Kind => Bool, Val_Bool => L.Val_Real > R.Val_Real);
    elsif L.Kind = Bool then
      return (Kind => Bool, Val_Bool => L.Val_Bool > R.Val_Bool);
    elsif L.Kind = Regi then
      return (Kind => Bool, Val_Bool => L.Val_Regi > R.Val_Regi);
    else
      return (Kind => Bool, Val_Bool => L.Val_Text > R.Val_Text);
    end if;
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Greater;

  function Smaller (L, R : Item_Rec) return Item_Rec is
    use type As.U.Asu_Us, Arbitrary.Number, Arbitrary.Fractions.Fraction;
  begin
    if      not Is_Arbi_Or_Frac_Or_Inte_Or_Real_Or_Bool_Or_Chars_Or_Regi(L)
    or else not Is_Arbi_Or_Frac_Or_Inte_Or_Real_Or_Bool_Or_Chars_Or_Regi(R) then
      raise Invalid_Argument;
    end if;
    if L.Kind /= R.Kind then
      raise Argument_Mismatch;
    end if;
    if L.Kind = Arbi then
      return (Kind => Bool, Val_Bool => L.Val_Arbi < R.Val_Arbi);
    elsif L.Kind = Frac then
      return (Kind => Bool, Val_Bool => L.Val_Frac < R.Val_Frac);
    elsif L.Kind = Inte then
      return (Kind => Bool, Val_Bool => L.Val_Inte < R.Val_Inte);
    elsif L.Kind = Real then
      return (Kind => Bool, Val_Bool => L.Val_Real < R.Val_Real);
    elsif L.Kind = Bool then
      return (Kind => Bool, Val_Bool => L.Val_Bool < R.Val_Bool);
    elsif L.Kind = Regi then
      return (Kind => Bool, Val_Bool => L.Val_Regi < R.Val_Regi);
    else
      return (Kind => Bool, Val_Bool => L.Val_Text < R.Val_Text);
    end if;
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Smaller;

  function Greateq (L, R : Item_Rec) return Item_Rec is
    use type As.U.Asu_Us, Arbitrary.Number, Arbitrary.Fractions.Fraction;
  begin
    if      not Is_Arbi_Or_Frac_Or_Inte_Or_Real_Or_Bool_Or_Chars_Or_Regi(L)
    or else not Is_Arbi_Or_Frac_Or_Inte_Or_Real_Or_Bool_Or_Chars_Or_Regi(R) then
      raise Invalid_Argument;
    end if;
    if L.Kind /= R.Kind then
      raise Argument_Mismatch;
    end if;
    if L.Kind = Arbi then
      return (Kind => Bool, Val_Bool => L.Val_Arbi >= R.Val_Arbi);
    elsif L.Kind = Frac then
      return (Kind => Bool, Val_Bool => L.Val_Frac >= R.Val_Frac);
    elsif L.Kind = Inte then
      return (Kind => Bool, Val_Bool => L.Val_Inte >= R.Val_Inte);
    elsif L.Kind = Real then
      return (Kind => Bool, Val_Bool => L.Val_Real >= R.Val_Real);
    elsif L.Kind = Bool then
      return (Kind => Bool, Val_Bool => L.Val_Bool >= R.Val_Bool);
    elsif L.Kind = Regi then
      return (Kind => Bool, Val_Bool => L.Val_Regi >= R.Val_Regi);
    else
      return (Kind => Bool, Val_Bool => L.Val_Text >= R.Val_Text);
    end if;
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Greateq;

  function Smalleq (L, R : Item_Rec) return Item_Rec is
    use type As.U.Asu_Us, Arbitrary.Number, Arbitrary.Fractions.Fraction;
  begin
    if      not Is_Arbi_Or_Frac_Or_Inte_Or_Real_Or_Bool_Or_Chars_Or_Regi(L)
    or else not Is_Arbi_Or_Frac_Or_Inte_Or_Real_Or_Bool_Or_Chars_Or_Regi(R) then
      raise Invalid_Argument;
    end if;
    if L.Kind /= R.Kind then
      raise Argument_Mismatch;
    end if;
    if L.Kind = Arbi then
      return (Kind => Bool, Val_Bool => L.Val_Arbi <= R.Val_Arbi);
    elsif L.Kind = Frac then
      return (Kind => Bool, Val_Bool => L.Val_Frac <= R.Val_Frac);
    elsif L.Kind = Inte then
      return (Kind => Bool, Val_Bool => L.Val_Inte <= R.Val_Inte);
    elsif L.Kind = Real then
      return (Kind => Bool, Val_Bool => L.Val_Real <= R.Val_Real);
    elsif L.Kind = Bool then
      return (Kind => Bool, Val_Bool => L.Val_Bool <= R.Val_Bool);
    elsif L.Kind = Regi then
      return (Kind => Bool, Val_Bool => L.Val_Regi <= R.Val_Regi);
    else
      return (Kind => Bool, Val_Bool => L.Val_Text <= R.Val_Text);
    end if;
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Smalleq;

  -- Arbi,Inte->Inte
  function Tointe (X : Item_Rec) return Item_Rec is
  begin
    if X.Kind = Inte then
      return X;
    end if;
    if X.Kind /= Arbi then
      raise Invalid_Argument;
    end if;
    return (Kind => Inte, Val_Inte => My_Math.Inte'Value(Arbitrary.Image (X.Val_Arbi)));
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Tointe;

  -- Arbi,Inte->Arbi
  function Toarbi (X : Item_Rec) return Item_Rec is
  begin
    if X.Kind = Arbi then
      return X;
    end if;
    if X.Kind /= Inte then
      raise Invalid_Argument;
    end if;
    return (Kind => Arbi, Val_Arbi => Arbitrary.Set (X.Val_Inte));
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when Constraint_Error =>
      raise Invalid_Argument;
    when others =>
      raise Compute_Error;
  end Toarbi;

  -- Inte,Real->Real
  function Toreal  (X : Item_Rec) return Item_Rec is
  begin
    if X.Kind = Real then
      return X;
    end if;
    if X.Kind = Inte then
      return (Kind => Real, Val_Real => My_Math.Real(X.Val_Inte));
    elsif X.Kind = Arbi then
      return (Kind => Real,
              Val_Real => My_Math.Real'Value(Arbitrary.Image (X.Val_Arbi)));
    elsif X.Kind = Frac then
      return (Kind => Real,
              Val_Real =>
          My_Math.Real'Value(Arbitrary.Image (
                      Arbitrary.Fractions.Numerator (X.Val_Frac)))
        / My_Math.Real'Value(Arbitrary.Image (
                      Arbitrary.Fractions.Denominator (X.Val_Frac))) );
    else
      raise Invalid_Argument;
    end if;
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Toreal;

  -- Inte,Real->Inte
  function Round (X : Item_Rec) return Item_Rec is
  begin
    if X.Kind = Inte then
      return X;
    end if;
    if X.Kind /= Real then
      raise Invalid_Argument;
    end if;
    return (Kind => Inte, Val_Inte => My_Math.Round(X.Val_Real));
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Round;

  function Trunc (X : Item_Rec) return Item_Rec is
  begin
    if X.Kind = Inte then
      return X;
    end if;
    if X.Kind /= Real then
      raise Invalid_Argument;
    end if;
    return (Kind => Inte, Val_Inte => My_Math.Trunc(X.Val_Real));
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Trunc;

  -- Real->Real
  function Int (X : Item_Rec) return Item_Rec is
  begin
    if X.Kind = Inte then
      return X;
    end if;
    if X.Kind /= Real then
      raise Invalid_Argument;
    end if;
    return (Kind => Real, Val_Real => My_Math.Int(X.Val_Real));
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Int;

  function Frac (X : Item_Rec) return Item_Rec is
  begin
    if X.Kind = Inte then
      return (Kind => Inte, Val_Inte => 0);
    end if;
    if X.Kind /= Real then
      raise Invalid_Argument;
    end if;
    return (Kind => Real, Val_Real => My_Math.Frac(X.Val_Real));
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Frac;

  function Maxint return Item_Rec is
    ( (Kind => Inte, Val_Inte => My_Math.Inte'Last) );

  function Minint return Item_Rec is
    ( (Kind => Inte, Val_Inte => My_Math.Inte'First) );

  -- Inte,Real->Inte or Real
  function Roundif (X : Item_Rec) return Item_Rec is
  begin
    if X.Kind = Inte then
      return X;
    end if;
    if X.Kind /= Real then
      raise Invalid_Argument;
    end if;
    if X.Val_Real > My_Math.Real (My_Math.Inte'Last)
    or else X.Val_Real < My_Math.Real (My_Math.Inte'First) then
      return X;
    else
      return (Kind => Inte, Val_Inte => My_Math.Round(X.Val_Real));
    end if;
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when My_Math.Math_Error =>
      return X;
    when others =>
      raise Compute_Error;
  end Roundif;


  function Degms (X : Item_Rec) return Item_Rec is
    I, M, S, F, R : My_Math.Real;
  begin
    if X.Kind /= Real then
      raise Invalid_Argument;
    end if;
    -- Int part and frac parts
    I := My_Math.Int (X.Val_Real);
    S := My_Math.Frac (X.Val_Real);

    -- Frac of hours -> seconds
    S := S * 3600.0;
    -- Round to micros
    S := My_Math.Round_At (S, -6);

    -- Rounding may lead to next hour
    if S >= 3600.0 then
      S := S  - 3600.0;
     I := I + 1.0;
    end if;

    -- Get int and frac of seconds
    F := My_Math.Frac (S);
    S := My_Math.Real (My_Math.Trunc (S));

    -- Split seconds into minutes and seconds
    M := My_Math.Real (My_Math.Inte (S) / 60);
    S := My_Math.Real (My_Math.Inte (S) rem 60);

    -- Set minutes, seconds, frac of seconds
    R := I + M / 100.0 + S / 10000.0 + F / 1_0000.0;

    return (Kind => Real, Val_Real => R);
  end Degms;

  function Degfr (X : Item_Rec) return Item_Rec is
    I, M, S, F, R : My_Math.Real;
  begin
    if X.Kind /= Real then
      raise Invalid_Argument;
    end if;

    -- Int part and frac parts
    I := My_Math.Int (X.Val_Real);
    F := My_Math.Frac (X.Val_Real);

    -- F is 0.MmSsxxxyyy, x and y for milis and micros
    -- Round micros
    F := Round_At (F, -10);
    -- Extract Minutes and seconds. Round
    F := F * 100.0;
    M := My_Math.Int (F);
    if M >= 60.0 then
      raise Invalid_Argument;
    end if;
    S := (F - M) * 100.0;
    if S >= 60.0 then
      raise Invalid_Argument;
    end if;

    -- Compute hours and fracs of hours
    R := I + M / 60.0 + S / 3600.0;

    return (Kind => Real, Val_Real => R);
  end Degfr;

  -- Real,Arbi,Inte->Real,Arbi,Inte
  function Sqrt (X : Item_Rec) return Item_Rec is
    R : My_Math.Real;
  begin
    if X.Kind = Real then
      return (Kind => Real, Val_Real => My_Math.Sqrt (X.Val_Real));
    elsif X.Kind = Arbi then
      return (Kind => Arbi, Val_Arbi => Arbitrary.Sqrt (X.Val_Arbi));
    elsif X.Kind = Inte then
      R := My_Math.Sqrt (My_Math.Real(X.Val_Inte));
      return (Kind => Inte, Val_Inte => My_Math.Trunc(R));
    else
      raise Invalid_Argument;
    end if;
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Sqrt;

  -- *->Bool
  function Isarbi  (X : Item_Rec) return Item_Rec is
    ( (Kind => Bool, Val_Bool => X.Kind = Arbi) );

  function Isfrac  (X : Item_Rec) return Item_Rec is
    ( (Kind => Bool, Val_Bool => X.Kind = Frac) );

  function Isinte  (X : Item_Rec) return Item_Rec is
    ( (Kind => Bool, Val_Bool => X.Kind = Inte) );

  function Isreal  (X : Item_Rec) return Item_Rec is
    ( (Kind => Bool, Val_Bool => X.Kind = Real) );

  function Isbool  (X : Item_Rec) return Item_Rec is
    ( (Kind => Bool, Val_Bool => X.Kind = Bool) );

  function Isstr  (X : Item_Rec) return Item_Rec is
    ( (Kind => Bool, Val_Bool => X.Kind = Chrs) );

  function Isreg  (X : Item_Rec) return Item_Rec is
    ( (Kind => Bool, Val_Bool => X.Kind = Regi) );

  function Isprog  (X : Item_Rec) return Item_Rec is
    ( (Kind => Bool, Val_Bool => X.Kind = Prog) );

  -- Arbi->Bool or Inte->Bool or Real->Bool
  function Ispos  (X : Item_Rec) return Item_Rec is
    use type Arbitrary.Number, Arbitrary.Fractions.Fraction;
  begin
    if X.Kind = Arbi then
      return (Kind => Bool, Val_Bool => X.Val_Arbi > Arbitrary.Zero);
    elsif X.Kind = Frac then
      return (Kind => Bool, Val_Bool => X.Val_Frac > Arbitrary.Fractions.Zero);
    elsif X.Kind = Inte then
      return (Kind => Bool, Val_Bool => X.Val_Inte > 0);
    elsif  X.Kind = Real then
      return (Kind => Bool, Val_Bool => X.Val_Real > 0.0);
    else
      raise Invalid_Argument;
    end if;
  end Ispos;

  function Isnull  (X : Item_Rec) return Item_Rec is
    use type Arbitrary.Number, Arbitrary.Fractions.Fraction;
  begin
    if X.Kind = Arbi then
      return (Kind => Bool, Val_Bool => X.Val_Arbi = Arbitrary.Zero);
    elsif X.Kind = Frac then
      return (Kind => Bool, Val_Bool => X.Val_Frac = Arbitrary.Fractions.Zero);
    elsif X.Kind = Inte then
      return (Kind => Bool, Val_Bool => X.Val_Inte = 0);
    elsif  X.Kind = Real then
      return (Kind => Bool, Val_Bool => X.Val_Real = 0.0);
    else
      raise Invalid_Argument;
    end if;
  end Isnull;

  function Isntnull  (X : Item_Rec) return Item_Rec is
    use type Arbitrary.Number, Arbitrary.Fractions.Fraction;
  begin
    if X.Kind = Arbi then
      return (Kind => Bool, Val_Bool => X.Val_Arbi /= Arbitrary.Zero);
    elsif X.Kind = Frac then
      return (Kind => Bool, Val_Bool => X.Val_Frac /= Arbitrary.Fractions.Zero);
    elsif X.Kind = Inte then
      return (Kind => Bool, Val_Bool => X.Val_Inte /= 0);
    elsif  X.Kind = Real then
      return (Kind => Bool, Val_Bool => X.Val_Real /= 0.0);
    else
      raise Invalid_Argument;
    end if;
  end Isntnull;

  function Isneg  (X : Item_Rec) return Item_Rec is
    use type Arbitrary.Number, Arbitrary.Fractions.Fraction;
  begin
    if X.Kind = Arbi then
      return (Kind => Bool, Val_Bool => X.Val_Arbi < Arbitrary.Zero);
    elsif X.Kind = Frac then
      return (Kind => Bool, Val_Bool => X.Val_Frac < Arbitrary.Fractions.Zero);
    elsif X.Kind = Inte then
      return (Kind => Bool, Val_Bool => X.Val_Inte < 0);
    elsif  X.Kind = Real then
      return (Kind => Bool, Val_Bool => X.Val_Real < 0.0);
    else
      raise Invalid_Argument;
    end if;
  end Isneg;

  -- Bool,Bool->Bool
  function Boland  (L, R : Item_Rec) return Item_Rec is
  begin
    if L.Kind /= Bool or else R.Kind /= Bool then
      raise Invalid_Argument;
    end if;
    return (Kind => Bool, Val_Bool => L.Val_Bool and then R.Val_Bool);
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Boland;

  function Bolor   (L, R : Item_Rec) return Item_Rec is
  begin
    if L.Kind /= Bool or else R.Kind /= Bool then
      raise Invalid_Argument;
    end if;
    return (Kind => Bool, Val_Bool => L.Val_Bool or else R.Val_Bool);
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Bolor;

  function Bolxor  (L, R : Item_Rec) return Item_Rec is
  begin
    if L.Kind /= Bool or else R.Kind /= Bool then
      raise Invalid_Argument;
    end if;
    return (Kind => Bool, Val_Bool => L.Val_Bool /= R.Val_Bool);
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Bolxor;

  -- Bool->Bool
  function Bolneg  (X : Item_Rec) return Item_Rec is
  begin
    if X.Kind /= Bool then
      raise Invalid_Argument;
    end if;
    return (Kind => Bool, Val_Bool => not X.Val_Bool);
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Bolneg;

  -- Bool,*,*->*
  function Ifte    (X, A, B : Item_Rec) return Item_Rec is
  begin
    if X.Kind /= Bool then
      raise Invalid_Argument;
    end if;
    if A.Kind /= B.Kind then
      raise Argument_Mismatch;
    end if;
    if X.Val_Bool then
      return A;
    else
      return B;
    end if;
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Ifte;

  -- Real -> Real
  function Sin     (X : Item_Rec) return Item_Rec is
  begin
    if X.Kind /= Real then
      raise Invalid_Argument;
    end if;
    return (Kind => Real, Val_Real => My_Math.Sin(X.Val_Real));
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Sin;

  function Cos     (X : Item_Rec) return Item_Rec is
  begin
    if X.Kind /= Real then
      raise Invalid_Argument;
    end if;
    return (Kind => Real, Val_Real => My_Math.Cos(X.Val_Real));
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Cos;

  function Tan     (X : Item_Rec) return Item_Rec is
  begin
    if X.Kind /= Real then
      raise Invalid_Argument;
    end if;
    return (Kind => Real, Val_Real => My_Math.Tan(X.Val_Real));
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Tan;

  function Asin     (X : Item_Rec) return Item_Rec is
  begin
    if X.Kind /= Real then
      raise Invalid_Argument;
    end if;
    return (Kind => Real, Val_Real => My_Math.Arc_Sin(X.Val_Real));
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Asin;

  function Acos     (X : Item_Rec) return Item_Rec is
  begin
    if X.Kind /= Real then
      raise Invalid_Argument;
    end if;
    return (Kind => Real, Val_Real => My_Math.Arc_Cos(X.Val_Real));
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Acos;

  function Atan     (X : Item_Rec) return Item_Rec is
  begin
    if X.Kind /= Real then
      raise Invalid_Argument;
    end if;
    return (Kind => Real, Val_Real => My_Math.Arc_Tan(X.Val_Real));
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Atan;

  function Ln  (X : Item_Rec) return Item_Rec is
  begin
    if X.Kind /= Real then
      raise Invalid_Argument;
    end if;
    return (Kind => Real, Val_Real => My_Math.Ln(X.Val_Real));
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Ln;

  function Lg (X : Item_Rec) return Item_Rec is
  begin
    if X.Kind /= Real then
      raise Invalid_Argument;
    end if;
    return (Kind => Real, Val_Real => My_Math.Lg(X.Val_Real));
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Lg;

  -- Arbi,Arbi,Arbi->Arbi or Frac,Frac,Frac->Frac or
  -- Inte,Inte,Inte->Inte or Real,Real,Real->Real
  -- Z * Y / X
  function Proport   (X, Y, Z : Item_Rec) return Item_Rec is
    use type Arbitrary.Number, Arbitrary.Fractions.Fraction;
  begin
    if not Is_Arbi_Or_Frac_Or_Inte_Or_Real(X)
     or else not Is_Arbi_Or_Frac_Or_Inte_Or_Real(Y)
     or else not Is_Arbi_Or_Frac_Or_Inte_Or_Real(Z) then
      raise Invalid_Argument;
    end if;
    if X.Kind /= Y.Kind or else X.Kind /= Z.Kind then
      raise Argument_Mismatch;
    end if;
    if X.Kind = Arbi then
      return (Kind => Arbi,
              Val_Arbi => Arbitrary.Roundiv (Z.Val_Arbi * Y.Val_Arbi,
                                             X.Val_Arbi));
    elsif X.Kind = Frac then
      return (Kind => Frac, Val_Frac => Z.Val_Frac * Y.Val_Frac / X.Val_Frac);
    elsif X.Kind = Inte then
      return (Kind => Inte,
              Val_Inte => My_Math.Roundiv (Z.Val_Inte * Y.Val_Inte,
                                           X.Val_Inte));
    else
      return (Kind => Real, Val_Real => Z.Val_Real * Y.Val_Real / X.Val_Real);
    end if;
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Proport;

  -- Real,Inte->Real
  function Roundat (X : Item_Rec; N : Item_Rec) return Item_Rec is
  begin
    if X.Kind /= Real or else N.Kind /= Inte then
      raise Invalid_Argument;
    end if;
    if       N.Val_Inte < My_Math.Inte (Integer'First)
    or else  N.Val_Inte > My_Math.Inte (Integer'Last)  then
      raise Invalid_Argument;
    end if;
    return (Kind => Real,
            Val_Real => My_Math.Round_At (X.Val_Real, N.Val_Inte));
  end Roundat;


  -- Frac <-> Arbi
  function Mkfrac (N, D : Item_Rec) return Item_Rec is
  begin
    if N.Kind /= Arbi or else D.Kind /= Arbi then
      raise Invalid_Argument;
    end if;
    return (Kind => Frac,
            Val_Frac => Arbitrary.Fractions.Set (N.Val_Arbi, D.Val_Arbi));
  exception
    when others =>
      raise Invalid_Argument;
  end Mkfrac;

  function Numerof  (X : Item_Rec) return Item_Rec is
  begin
    if X.Kind /= Frac then
      raise Invalid_Argument;
    end if;
    return (Kind => Arbi,
            Val_Arbi => Arbitrary.Fractions.Numerator (X.Val_Frac));
  end Numerof;

  function Denomof (X : Item_Rec) return Item_Rec is
  begin
    if X.Kind /= Frac then
      raise Invalid_Argument;
    end if;
    return (Kind => Arbi,
            Val_Arbi => Arbitrary.Fractions.Denominator (X.Val_Frac));
  end Denomof;

end Operations;

