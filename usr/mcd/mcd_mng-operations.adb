with Bit_Ops;
separate (Mcd_Mng)

package body Operations is
  use My_Math;

  function Is_True (X : Item_Rec) return Boolean is
  begin
    if X.Kind /= Bool then
      raise Invalid_Argument;
    end if;
    return X.Val_Bool;
  end Is_True;

  function Is_Inte_Or_Real (X : Item_Rec) return Boolean is
  begin
    return X.Kind = Inte or else X.Kind = Real;
  end Is_Inte_Or_Real;

  function Is_Inte_Or_Real_Or_Bool (X : Item_Rec) return Boolean is
  begin
    return X.Kind = Inte or else X.Kind = Real or else X.Kind = Bool;
  end Is_Inte_Or_Real_Or_Bool;

  function Is_Inte_Or_Real_Or_Bool_Or_Chars (X : Item_Rec)
           return Boolean is
  begin
    return X.Kind = Inte or else X.Kind = Real
   or else X.Kind = Bool or else X.Kind = Chrs;
  end Is_Inte_Or_Real_Or_Bool_Or_Chars;

  function Is_Inte_Or_Real_Or_Bool_Or_Chars_Or_Regi (X : Item_Rec)
           return Boolean is
  begin
    return X.Kind = Inte or else X.Kind = Real
   or else X.Kind = Bool or else X.Kind = Chrs or else X.Kind = Regi;
  end Is_Inte_Or_Real_Or_Bool_Or_Chars_Or_Regi;

  -- INTE,INTE->INTE or REAL,REAL->REAL
  function Add     (L, R : Item_Rec) return Item_Rec is
  begin
    if not Is_Inte_Or_Real(L) or else not Is_Inte_Or_Real(R) then
      raise Invalid_Argument;
    end if;
    if L.Kind /= R.Kind then
      raise Argument_Mismatch;
    end if;
    if L.Kind = Inte then
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
  begin
    if not Is_Inte_Or_Real(L) or else not Is_Inte_Or_Real(R) then
      raise Invalid_Argument;
    end if;
    if L.Kind /= R.Kind then
      raise Argument_Mismatch;
    end if;
    if L.Kind = Inte then
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
  begin
    if not Is_Inte_Or_Real(L) or else not Is_Inte_Or_Real(R) then
      raise Invalid_Argument;
    end if;
    if L.Kind /= R.Kind then
      raise Argument_Mismatch;
    end if;
    if L.Kind = Inte then
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
  begin
    if not Is_Inte_Or_Real(L) or else not Is_Inte_Or_Real(R) then
      raise Invalid_Argument;
    end if;
    if L.Kind /= R.Kind then
      raise Argument_Mismatch;
    end if;
    if L.Kind = Inte then
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

  function Pow     (L, R : Item_Rec) return Item_Rec is
    use My_Math; -- for real ** real
  begin
    if not Is_Inte_Or_Real(L) or else not Is_Inte_Or_Real(R) then
      raise Invalid_Argument;
    end if;
    if L.Kind = Inte and then R.Kind = Inte then
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

  -- INTE,INTE->INTE
  function Remind  (L, R : Item_Rec) return Item_Rec is
  begin
    if L.Kind /= Inte or else R.Kind /= Inte then
      raise Invalid_Argument;
    end if;
    return (Kind => Inte, Val_Inte => L.Val_Inte rem R.Val_Inte);
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Remind;

  -- INTE,INTE->INTE
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

  -- INTE->INTE or REAL->REAL
  function Minus   (X : Item_Rec) return Item_Rec is
  begin
    if not Is_Inte_Or_Real(X) then
      raise Invalid_Argument;
    end if;
    if X.Kind = Inte then
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

  -- INTE->INTE or REAL->REAL
  function Absv   (X : Item_Rec) return Item_Rec is
  begin
    if not Is_Inte_Or_Real(X) then
      raise Invalid_Argument;
    end if;
    if X.Kind = Inte then
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

  -- INTE->INTE
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

  -- INTE,INTE->BOOL or REAL,REAL->BOOL or BOOL,BOOL->BOOL 
  -- REGI,REGI->BOOL or CHARS,CHARS->BOOL
  function Equal   (L, R : Item_Rec) return Item_Rec is
  begin
    if      not Is_Inte_Or_Real_Or_Bool_Or_Chars_Or_Regi(L)
    or else not Is_Inte_Or_Real_Or_Bool_Or_Chars_Or_Regi(R) then
      raise Invalid_Argument;
    end if;
    if L.Kind /= R.Kind then
      raise Argument_Mismatch;
    end if;
    if L.Kind = Inte then
      return (Kind => Bool, Val_Bool => L.Val_Inte = R.Val_Inte);
    elsif L.Kind = Real then
      return (Kind => Bool, Val_Bool => L.Val_Real = R.Val_Real);
    elsif L.Kind = Bool then
      return (Kind => Bool, Val_Bool => L.Val_Bool = R.Val_Bool);
    elsif L.Kind = Regi then
      return (Kind => Bool, Val_Bool => L.Val_Regi = R.Val_Regi);
    else
      return (Kind => Bool, Val_Bool => L.Val_Text(1 .. L.Val_Len)
                                      = R.Val_Text(1 .. R.Val_Len));
    end if;
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Equal;

  function Diff    (L, R : Item_Rec) return Item_Rec is
  begin
    if      not Is_Inte_Or_Real_Or_Bool_Or_Chars_Or_Regi(L)
    or else not Is_Inte_Or_Real_Or_Bool_Or_Chars_Or_Regi(R) then
      raise Invalid_Argument;
    end if;
    if L.Kind /= R.Kind then
      raise Argument_Mismatch;
    end if;
    if L.Kind = Inte then
      return (Kind => Bool, Val_Bool => L.Val_Inte /= R.Val_Inte);
    elsif L.Kind = Real then
      return (Kind => Bool, Val_Bool => L.Val_Real /= R.Val_Real);
    elsif L.Kind = Bool then
      return (Kind => Bool, Val_Bool => L.Val_Bool /= R.Val_Bool);
    elsif L.Kind = Regi then
      return (Kind => Bool, Val_Bool => L.Val_Regi /= R.Val_Regi);
    else
      return (Kind => Bool, Val_Bool => L.Val_Text(1 .. L.Val_Len)
                                     /= R.Val_Text(1 .. R.Val_Len));
    end if;
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Diff;

  function Greater (L, R : Item_Rec) return Item_Rec is
  begin
    if      not Is_Inte_Or_Real_Or_Bool_Or_Chars_Or_Regi(L)
    or else not Is_Inte_Or_Real_Or_Bool_Or_Chars_Or_Regi(R) then
      raise Invalid_Argument;
    end if;
    if L.Kind /= R.Kind then
      raise Argument_Mismatch;
    end if;
    if L.Kind = Inte then
      return (Kind => Bool, Val_Bool => L.Val_Inte > R.Val_Inte);
    elsif L.Kind = Real then
      return (Kind => Bool, Val_Bool => L.Val_Real > R.Val_Real);
    elsif L.Kind = Bool then
      return (Kind => Bool, Val_Bool => L.Val_Bool > R.Val_Bool);
    elsif L.Kind = Regi then
      return (Kind => Bool, Val_Bool => L.Val_Regi > R.Val_Regi);
    else
      return (Kind => Bool, Val_Bool => L.Val_Text(1 .. L.Val_Len)
                                      > R.Val_Text(1 .. R.Val_Len));
    end if;
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Greater;

  function Smaller (L, R : Item_Rec) return Item_Rec is
  begin
    if      not Is_Inte_Or_Real_Or_Bool_Or_Chars_Or_Regi(L)
    or else not Is_Inte_Or_Real_Or_Bool_Or_Chars_Or_Regi(R) then
      raise Invalid_Argument;
    end if;
    if L.Kind /= R.Kind then
      raise Argument_Mismatch;
    end if;
    if L.Kind = Inte then
      return (Kind => Bool, Val_Bool => L.Val_Inte < R.Val_Inte);
    elsif L.Kind = Real then
      return (Kind => Bool, Val_Bool => L.Val_Real < R.Val_Real);
    elsif L.Kind = Bool then
      return (Kind => Bool, Val_Bool => L.Val_Bool < R.Val_Bool);
    elsif L.Kind = Regi then
      return (Kind => Bool, Val_Bool => L.Val_Regi < R.Val_Regi);
    else
      return (Kind => Bool, Val_Bool => L.Val_Text(1 .. L.Val_Len)
                                      < R.Val_Text(1 .. R.Val_Len));
    end if;
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Smaller;

  function Greateq (L, R : Item_Rec) return Item_Rec is
  begin
    if      not Is_Inte_Or_Real_Or_Bool_Or_Chars_Or_Regi(L)
    or else not Is_Inte_Or_Real_Or_Bool_Or_Chars_Or_Regi(R) then
      raise Invalid_Argument;
    end if;
    if L.Kind /= R.Kind then
      raise Argument_Mismatch;
    end if;
    if L.Kind = Inte then
      return (Kind => Bool, Val_Bool => L.Val_Inte >= R.Val_Inte);
    elsif L.Kind = Real then
      return (Kind => Bool, Val_Bool => L.Val_Real >= R.Val_Real);
    elsif L.Kind = Bool then
      return (Kind => Bool, Val_Bool => L.Val_Bool >= R.Val_Bool);
    elsif L.Kind = Regi then
      return (Kind => Bool, Val_Bool => L.Val_Regi >= R.Val_Regi);
    else
      return (Kind => Bool, Val_Bool => L.Val_Text(1 .. L.Val_Len)
                                     >= R.Val_Text(1 .. R.Val_Len));
    end if;
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Greateq;

  function Smalleq (L, R : Item_Rec) return Item_Rec is 
  begin
    if      not Is_Inte_Or_Real_Or_Bool_Or_Chars_Or_Regi(L)
    or else not Is_Inte_Or_Real_Or_Bool_Or_Chars_Or_Regi(R) then
      raise Invalid_Argument;
    end if;
    if L.Kind /= R.Kind then
      raise Argument_Mismatch;
    end if;
    if L.Kind = Inte then
      return (Kind => Bool, Val_Bool => L.Val_Inte <= R.Val_Inte);
    elsif L.Kind = Real then
      return (Kind => Bool, Val_Bool => L.Val_Real <= R.Val_Real);
    elsif L.Kind = Bool then
      return (Kind => Bool, Val_Bool => L.Val_Bool <= R.Val_Bool);
    elsif L.Kind = Regi then
      return (Kind => Bool, Val_Bool => L.Val_Regi <= R.Val_Regi);
    else
      return (Kind => Bool, Val_Bool => L.Val_Text(1 .. L.Val_Len)
                                     <= R.Val_Text(1 .. R.Val_Len));
    end if;
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Smalleq;

  -- INTE->REAL
  function Toreal  (X : Item_Rec) return Item_Rec is
  begin
    if X.Kind = Real then
      return X;
    end if;
    if X.Kind /= Inte then
      raise Invalid_Argument;
    end if;
    return (Kind => Real, Val_Real => My_Math.Real(X.Val_Inte));
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Toreal;

  -- REAL->INTE
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

  -- REAL->REAL
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

  function Dms (X : Item_Rec) return Item_Rec is
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

    -- Round micro
    S := My_Math.Real (My_Math.Round (S * 1000000.0)) / 1000000.0;

    -- Get int and frac of seconds
    F := My_Math.Frac (S);
    S := My_Math.Real (My_Math.Trunc (S));

    -- Split seconds into minutes and seconds
    M := My_Math.Real (My_Math.Inte (S) / 60);
    S := My_Math.Real (My_Math.Inte (S) rem 60);

    -- Set minutes, seconds, frac of seconds
    R := I + M / 100.0 + S / 10000.0 + F / 1_0000.0;
    
    return (Kind => Real, Val_Real => R);
  end Dms;

  function Msd (X : Item_Rec) return Item_Rec is
    I, M, S, F, R : My_Math.Real;
  begin
    if X.Kind /= Real then
      raise Invalid_Argument;
    end if;

    -- Int part and frac parts
    I := My_Math.Int (X.Val_Real);
    F := My_Math.Frac (X.Val_Real);

    -- Extract Minutes and seconds
    F := F * 100.0;
    F := My_Math.Real (My_Math.Round (F * 100000.0)) / 100000.0;
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
  end Msd;

  -- *->BOOL
  function Isreal  (X : Item_Rec) return Item_Rec is
  begin
    return (Kind => Bool, Val_Bool => X.Kind = Real);
  end Isreal;

  function Isinte  (X : Item_Rec) return Item_Rec is
  begin
    return (Kind => Bool, Val_Bool => X.Kind = Inte);
  end Isinte;

  function Isbool  (X : Item_Rec) return Item_Rec is
  begin
    return (Kind => Bool, Val_Bool => X.Kind = Bool);
  end Isbool;

  function Isstr  (X : Item_Rec) return Item_Rec is
  begin
    return (Kind => Bool, Val_Bool => X.Kind = Chrs);
  end Isstr;

  function Isreg  (X : Item_Rec) return Item_Rec is
  begin
    return (Kind => Bool, Val_Bool => X.Kind = Regi);
  end Isreg;

  function Isprog  (X : Item_Rec) return Item_Rec is
  begin
    return (Kind => Bool, Val_Bool => X.Kind = Prog);
  end Isprog;


  -- BOOL,BOOL->BOOL
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


  -- BOOL->BOOL
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

  -- BOOL,*,*->*
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

  -- REAL -> REAL
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
    return (Kind => Real, Val_Real => My_Math.Tg(X.Val_Real));
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
    return (Kind => Real, Val_Real => My_Math.Arc_Tg(X.Val_Real));
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

  function Log (X : Item_Rec) return Item_Rec is
  begin
    if X.Kind /= Real then
      raise Invalid_Argument;
    end if;
    return (Kind => Real, Val_Real => My_Math.Log_10(X.Val_Real));
  exception
    when Invalid_Argument | Argument_Mismatch =>
      raise;
    when others =>
      raise Compute_Error;
  end Log;

end Operations;

