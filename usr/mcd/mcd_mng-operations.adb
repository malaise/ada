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

  function Is_Inte_Or_Real_Or_Bool_Or_Chars (X : Item_Rec) return Boolean is
  begin
    return X.Kind = Inte or else X.Kind = Real or else X.Kind = Bool or else X.Kind = Chrs;
  end Is_Inte_Or_Real_Or_Bool_Or_Chars;

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
    when others =>
      raise Invalid_Argument;
  end Pow;

  -- INTE,INTE->INTE
  function Remind  (L, R : Item_Rec) return Item_Rec is
  begin
    if L.Kind /= Inte or else R.Kind /= Inte then
      raise Invalid_Argument;
    end if;
    return (Kind => Inte, Val_Inte => L.Val_Inte rem R.Val_Inte);
  end Remind;

  -- INTE,INTE->INTE
  function Bitand  (L, R : Item_Rec) return Item_Rec is
    use Bit_Ops;
  begin
    if L.Kind /= Inte or else R.Kind /= Inte then
      raise Invalid_Argument;
    end if;
    return (Kind => Inte, Val_Inte => L.Val_Inte and R.Val_Inte);
  end Bitand;

  function Bitor   (L, R : Item_Rec) return Item_Rec is
    use Bit_Ops;
  begin
    if L.Kind /= Inte or else R.Kind /= Inte then
      raise Invalid_Argument;
    end if;
    return (Kind => Inte, Val_Inte => L.Val_Inte or R.Val_Inte);
  end Bitor;

  function Bitxor  (L, R : Item_Rec) return Item_Rec is
    use Bit_Ops;
  begin
    if L.Kind /= Inte or else R.Kind /= Inte then
      raise Invalid_Argument;
    end if;
    return (Kind => Inte, Val_Inte => L.Val_Inte xor R.Val_Inte);
  end Bitxor;

  function Shl     (L, R : Item_Rec) return Item_Rec is
    use Bit_Ops;
  begin
    if L.Kind /= Inte or else R.Kind /= Inte then
      raise Invalid_Argument;
    end if;
    return (Kind => Inte, Val_Inte => Shl(L.Val_Inte, Integer(R.Val_Inte)));
  end Shl;

  function Shr     (L, R : Item_Rec) return Item_Rec is
    use Bit_Ops;
  begin
    if L.Kind /= Inte or else R.Kind /= Inte then
      raise Invalid_Argument;
    end if;
    return (Kind => Inte, Val_Inte => Shr(L.Val_Inte, Integer(R.Val_Inte)));
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
  end Absv;

  -- INTE->INTE
  function Bitneg  (X : Item_Rec) return Item_Rec is
    use Bit_Ops;
  begin
    if X.Kind /= Inte then
      raise Invalid_Argument;
    end if;
    return (Kind => Inte, Val_Inte => not X.Val_Inte);
  end Bitneg;

  -- INTE,INTE->BOOL or REAL,REAL->BOOL or BOOL,BOOL->BOOL 
  function Equal   (L, R : Item_Rec) return Item_Rec is
  begin
    if not Is_Inte_Or_Real_Or_Bool_Or_Chars(L) or else not Is_Inte_Or_Real_Or_Bool_Or_Chars(R) then
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
    else
      return (Kind => Bool, Val_Bool => L.Val_Text(1 .. L.Val_Len) = R.Val_Text(1 .. R.Val_Len));
    end if;
  end Equal;

  function Diff    (L, R : Item_Rec) return Item_Rec is
  begin
    if not Is_Inte_Or_Real_Or_Bool_Or_Chars(L) or else not Is_Inte_Or_Real_Or_Bool_Or_Chars(R) then
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
    else
      return (Kind => Bool, Val_Bool => L.Val_Text(1 .. L.Val_Len) /= R.Val_Text(1 .. R.Val_Len));
    end if;
  end Diff;

  function Greater (L, R : Item_Rec) return Item_Rec is
  begin
    if not Is_Inte_Or_Real_Or_Bool_Or_Chars(L) or else not Is_Inte_Or_Real_Or_Bool_Or_Chars(R) then
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
    else
      return (Kind => Bool, Val_Bool => L.Val_Text(1 .. L.Val_Len) > R.Val_Text(1 .. R.Val_Len));
    end if;
  end Greater;

  function Smaller (L, R : Item_Rec) return Item_Rec is
  begin
    if not Is_Inte_Or_Real_Or_Bool_Or_Chars(L) or else not Is_Inte_Or_Real_Or_Bool_Or_Chars(R) then
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
    else
      return (Kind => Bool, Val_Bool => L.Val_Text(1 .. L.Val_Len) < R.Val_Text(1 .. R.Val_Len));
    end if;
  end Smaller;

  function Greateq (L, R : Item_Rec) return Item_Rec is
  begin
    if not Is_Inte_Or_Real_Or_Bool_Or_Chars(L) or else not Is_Inte_Or_Real_Or_Bool_Or_Chars(R) then
      raise Invalid_Argument;
    end if;
    if L.Kind /= R.Kind then
      raise Argument_Mismatch;
    end if;
    if L.Kind = Inte then
      return (Kind => Bool, Val_Bool => L.Val_Inte >= R.Val_Inte);
    elsif L.Kind = Real then
      return (Kind => Bool, Val_Bool => L.Val_Real >= R.Val_Real);
    else
      return (Kind => Bool, Val_Bool => L.Val_Bool >= R.Val_Bool);
    end if;
  end Greateq;

  function Smalleq (L, R : Item_Rec) return Item_Rec is 
  begin
    if not Is_Inte_Or_Real_Or_Bool_Or_Chars(L) or else not Is_Inte_Or_Real_Or_Bool_Or_Chars(R) then
      raise Invalid_Argument;
    end if;
    if L.Kind /= R.Kind then
      raise Argument_Mismatch;
    end if;
    if L.Kind = Inte then
      return (Kind => Bool, Val_Bool => L.Val_Inte <= R.Val_Inte);
    elsif L.Kind = Real then
      return (Kind => Bool, Val_Bool => L.Val_Real <= R.Val_Real);
    else
      return (Kind => Bool, Val_Bool => L.Val_Bool <= R.Val_Bool);
    end if;
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
  end Int;

  function Frac (X : Item_Rec) return Item_Rec is
  begin
    if X.Kind = Inte then
      return X;
    end if;
    if X.Kind /= Real then
      raise Invalid_Argument;
    end if;
    return (Kind => Real, Val_Real => My_Math.Frac(X.Val_Real));
  end Frac;

  -- *->BOOL
  function Isreal  (X : Item_Rec) return Item_Rec is
  begin
    return (Kind => Bool, Val_Bool => X.Kind = Real);
  end Isreal;

  function Isinte  (X : Item_Rec) return Item_Rec is
  begin
    return (Kind => Bool, Val_Bool => X.Kind = Inte);
  end Isinte;

  function Isstr  (X : Item_Rec) return Item_Rec is
  begin
    return (Kind => Bool, Val_Bool => X.Kind = Chrs);
  end Isstr;

  function Isreg  (X : Item_Rec) return Item_Rec is
  begin
    return (Kind => Bool, Val_Bool => X.Kind = Regi);
  end Isreg;


  -- BOOL,BOOL->BOOL
  function Boland  (L, R : Item_Rec) return Item_Rec is
  begin
    if L.Kind /= Bool or else R.Kind /= Bool then
      raise Invalid_Argument;
    end if;
    return (Kind => Bool, Val_Bool => L.Val_Bool and then R.Val_Bool);
  end Boland;

  function Bolor   (L, R : Item_Rec) return Item_Rec is
  begin
    if L.Kind /= Bool or else R.Kind /= Bool then
      raise Invalid_Argument;
    end if;
    return (Kind => Bool, Val_Bool => L.Val_Bool or else R.Val_Bool);
  end Bolor;

  function Bolxor  (L, R : Item_Rec) return Item_Rec is
  begin
    if L.Kind /= Bool or else R.Kind /= Bool then
      raise Invalid_Argument;
    end if;
    return (Kind => Bool, Val_Bool => L.Val_Bool /= R.Val_Bool);
  end Bolxor;


  -- BOOL->BOOL
  function Bolneg  (X : Item_Rec) return Item_Rec is
  begin
    if X.Kind /= Bool then
      raise Invalid_Argument;
    end if;
    return (Kind => Bool, Val_Bool => not X.Val_Bool);
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
  end Ifte;

  -- REAL -> REAL
  function Sin     (X : Item_Rec) return Item_Rec is
  begin
    if X.Kind /= Real then
      raise Invalid_Argument;
    end if;
    return (Kind => Real, Val_Real => My_Math.Sin(X.Val_Real));
  exception
    when My_Math.Math_Error =>
      raise Invalid_Argument;
  end Sin;

  function Cos     (X : Item_Rec) return Item_Rec is
  begin
    if X.Kind /= Real then
      raise Invalid_Argument;
    end if;
    return (Kind => Real, Val_Real => My_Math.Cos(X.Val_Real));
  exception
    when My_Math.Math_Error =>
      raise Invalid_Argument;
  end Cos;

  function Tan     (X : Item_Rec) return Item_Rec is
  begin
    if X.Kind /= Real then
      raise Invalid_Argument;
    end if;
    return (Kind => Real, Val_Real => My_Math.Tg(X.Val_Real));
  exception
    when My_Math.Math_Error =>
      raise Invalid_Argument;
  end Tan;

  function Asin     (X : Item_Rec) return Item_Rec is
  begin
    if X.Kind /= Real then
      raise Invalid_Argument;
    end if;
    return (Kind => Real, Val_Real => My_Math.Arc_Sin(X.Val_Real));
  exception
    when My_Math.Math_Error =>
      raise Invalid_Argument;
  end Asin;

  function Acos     (X : Item_Rec) return Item_Rec is
  begin
    if X.Kind /= Real then
      raise Invalid_Argument;
    end if;
    return (Kind => Real, Val_Real => My_Math.Arc_Cos(X.Val_Real));
  exception
    when My_Math.Math_Error =>
      raise Invalid_Argument;
  end Acos;

  function Atan     (X : Item_Rec) return Item_Rec is
  begin
    if X.Kind /= Real then
      raise Invalid_Argument;
    end if;
    return (Kind => Real, Val_Real => My_Math.Arc_Tg(X.Val_Real));
  exception
    when My_Math.Math_Error =>
      raise Invalid_Argument;
  end Atan;

end Operations;

