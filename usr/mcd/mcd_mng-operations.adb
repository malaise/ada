with BIT_OPS;
separate (MCD_MNG)

package body OPERATIONS is
  use MY_MATH;

  function IS_TRUE (X : ITEM_REC) return BOOLEAN is
  begin
    if X.KIND /= BOOL then
      raise INVALID_ARGUMENT;
    end if;
    return X.VAL_BOOL;
  end IS_TRUE;

  function IS_INTE_OR_REAL (X : ITEM_REC) return BOOLEAN is
  begin
    return X.KIND = INTE or else X.KIND = REAL;
  end IS_INTE_OR_REAL;

  function IS_INTE_OR_REAL_OR_BOOL (X : ITEM_REC) return BOOLEAN is
  begin
    return X.KIND = INTE or else X.KIND = REAL or else X.KIND = BOOL;
  end IS_INTE_OR_REAL_OR_BOOL;

  function IS_INTE_OR_REAL_OR_BOOL_OR_CHARS (X : ITEM_REC) return BOOLEAN is
  begin
    return X.KIND = INTE or else X.KIND = REAL or else X.KIND = BOOL or else X.KIND = CHRS;
  end IS_INTE_OR_REAL_OR_BOOL_OR_CHARS;

  -- INTE,INTE->INTE or REAL,REAL->REAL
  function ADD     (L, R : ITEM_REC) return ITEM_REC is
  begin
    if not IS_INTE_OR_REAL(L) or else not IS_INTE_OR_REAL(R) then
      raise INVALID_ARGUMENT;
    end if;
    if L.KIND /= R.KIND then
      raise ARGUMENT_MISMATCH;
    end if;
    if L.KIND = INTE then
      return (KIND => INTE, VAL_INTE => L.VAL_INTE + R.VAL_INTE);
    else
      return (KIND => REAL, VAL_REAL => L.VAL_REAL + R.VAL_REAL);
    end if;
  end ADD;
        
  function SUB     (L, R : ITEM_REC) return ITEM_REC is
  begin
    if not IS_INTE_OR_REAL(L) or else not IS_INTE_OR_REAL(R) then
      raise INVALID_ARGUMENT;
    end if;
    if L.KIND /= R.KIND then
      raise ARGUMENT_MISMATCH;
    end if;
    if L.KIND = INTE then
      return (KIND => INTE, VAL_INTE => L.VAL_INTE - R.VAL_INTE);
    else
      return (KIND => REAL, VAL_REAL => L.VAL_REAL - R.VAL_REAL);
    end if;
  end SUB;

  function MULT    (L, R : ITEM_REC) return ITEM_REC is
  begin
    if not IS_INTE_OR_REAL(L) or else not IS_INTE_OR_REAL(R) then
      raise INVALID_ARGUMENT;
    end if;
    if L.KIND /= R.KIND then
      raise ARGUMENT_MISMATCH;
    end if;
    if L.KIND = INTE then
      return (KIND => INTE, VAL_INTE => L.VAL_INTE * R.VAL_INTE);
    else
      return (KIND => REAL, VAL_REAL => L.VAL_REAL * R.VAL_REAL);
    end if;
  end MULT;

  function DIV     (L, R : ITEM_REC) return ITEM_REC is
  begin
    if not IS_INTE_OR_REAL(L) or else not IS_INTE_OR_REAL(R) then
      raise INVALID_ARGUMENT;
    end if;
    if L.KIND /= R.KIND then
      raise ARGUMENT_MISMATCH;
    end if;
    if L.KIND = INTE then
      return (KIND => INTE, VAL_INTE => L.VAL_INTE / R.VAL_INTE);
    else
      return (KIND => REAL, VAL_REAL => L.VAL_REAL / R.VAL_REAL);
    end if;
  end DIV;

  function POW     (L, R : ITEM_REC) return ITEM_REC is
    use MY_MATH; -- for real ** real
  begin
    if not IS_INTE_OR_REAL(L) or else not IS_INTE_OR_REAL(R) then
      raise INVALID_ARGUMENT;
    end if;
    if L.KIND /= R.KIND then
      raise ARGUMENT_MISMATCH;
    end if;
    if L.KIND = INTE then
      return (KIND => INTE, VAL_INTE => ROUND(MY_MATH.REAL(L.VAL_INTE)
                                           ** MY_MATH.REAL(R.VAL_INTE)));
    else
      return (KIND => REAL, VAL_REAL => L.VAL_REAL ** R.VAL_REAL);
    end if;
  end POW;

  -- INTE,INTE->INTE
  function REMIND  (L, R : ITEM_REC) return ITEM_REC is
  begin
    if L.KIND /= INTE or else R.KIND /= INTE then
      raise INVALID_ARGUMENT;
    end if;
    return (KIND => INTE, VAL_INTE => L.VAL_INTE rem R.VAL_INTE);
  end REMIND;

  -- INTE,INTE->INTE
  function BITAND  (L, R : ITEM_REC) return ITEM_REC is
    use BIT_OPS;
  begin
    if L.KIND /= INTE or else R.KIND /= INTE then
      raise INVALID_ARGUMENT;
    end if;
    return (KIND => INTE, VAL_INTE => L.VAL_INTE and R.VAL_INTE);
  end BITAND;

  function BITOR   (L, R : ITEM_REC) return ITEM_REC is
    use BIT_OPS;
  begin
    if L.KIND /= INTE or else R.KIND /= INTE then
      raise INVALID_ARGUMENT;
    end if;
    return (KIND => INTE, VAL_INTE => L.VAL_INTE or R.VAL_INTE);
  end BITOR;

  function BITXOR  (L, R : ITEM_REC) return ITEM_REC is
    use BIT_OPS;
  begin
    if L.KIND /= INTE or else R.KIND /= INTE then
      raise INVALID_ARGUMENT;
    end if;
    return (KIND => INTE, VAL_INTE => L.VAL_INTE xor R.VAL_INTE);
  end BITXOR;

  function SHL     (L, R : ITEM_REC) return ITEM_REC is
    use BIT_OPS;
  begin
    if L.KIND /= INTE or else R.KIND /= INTE then
      raise INVALID_ARGUMENT;
    end if;
    return (KIND => INTE, VAL_INTE => SHL(L.VAL_INTE, INTEGER(R.VAL_INTE)));
  end SHL;

  function SHR     (L, R : ITEM_REC) return ITEM_REC is
    use BIT_OPS;
  begin
    if L.KIND /= INTE or else R.KIND /= INTE then
      raise INVALID_ARGUMENT;
    end if;
    return (KIND => INTE, VAL_INTE => SHR(L.VAL_INTE, INTEGER(R.VAL_INTE)));
  end SHR;

  -- INTE->INTE or REAL->REAL
  function MINUS   (X : ITEM_REC) return ITEM_REC is
  begin
    if not IS_INTE_OR_REAL(X) then
      raise INVALID_ARGUMENT;
    end if;
    if X.KIND = INTE then
      return (KIND => INTE, VAL_INTE => - X.VAL_INTE);
    else
      return (KIND => REAL, VAL_REAL => - X.VAL_REAL);
    end if;
  end MINUS;

  -- INTE->INTE or REAL->REAL
  function ABSV   (X : ITEM_REC) return ITEM_REC is
  begin
    if not IS_INTE_OR_REAL(X) then
      raise INVALID_ARGUMENT;
    end if;
    if X.KIND = INTE then
      return (KIND => INTE, VAL_INTE => abs X.VAL_INTE);
    else
      return (KIND => REAL, VAL_REAL => abs X.VAL_REAL);
    end if;
  end ABSV;

  -- INTE->INTE
  function BITNEG  (X : ITEM_REC) return ITEM_REC is
    use BIT_OPS;
  begin
    if X.KIND /= INTE then
      raise INVALID_ARGUMENT;
    end if;
    return (KIND => INTE, VAL_INTE => not X.VAL_INTE);
  end BITNEG;

  -- INTE,INTE->BOOL or REAL,REAL->BOOL or BOOL,BOOL->BOOL 
  function EQUAL   (L, R : ITEM_REC) return ITEM_REC is
  begin
    if not IS_INTE_OR_REAL_OR_BOOL_OR_CHARS(L) or else not IS_INTE_OR_REAL_OR_BOOL_OR_CHARS(R) then
      raise INVALID_ARGUMENT;
    end if;
    if L.KIND /= R.KIND then
      raise ARGUMENT_MISMATCH;
    end if;
    if L.KIND = INTE then
      return (KIND => BOOL, VAL_BOOL => L.VAL_INTE = R.VAL_INTE);
    elsif L.KIND = REAL then
      return (KIND => BOOL, VAL_BOOL => L.VAL_REAL = R.VAL_REAL);
    elsif L.KIND = BOOL then
      return (KIND => BOOL, VAL_BOOL => L.VAL_BOOL = R.VAL_BOOL);
    else
      return (KIND => BOOL, VAL_BOOL => L.VAL_TEXT(1 .. L.VAL_LEN) = R.VAL_TEXT(1 .. R.VAL_LEN));
    end if;
  end EQUAL;

  function DIFF    (L, R : ITEM_REC) return ITEM_REC is
  begin
    if not IS_INTE_OR_REAL_OR_BOOL_OR_CHARS(L) or else not IS_INTE_OR_REAL_OR_BOOL_OR_CHARS(R) then
      raise INVALID_ARGUMENT;
    end if;
    if L.KIND /= R.KIND then
      raise ARGUMENT_MISMATCH;
    end if;
    if L.KIND = INTE then
      return (KIND => BOOL, VAL_BOOL => L.VAL_INTE /= R.VAL_INTE);
    elsif L.KIND = REAL then
      return (KIND => BOOL, VAL_BOOL => L.VAL_REAL /= R.VAL_REAL);
    elsif L.KIND = BOOL then
      return (KIND => BOOL, VAL_BOOL => L.VAL_BOOL /= R.VAL_BOOL);
    else
      return (KIND => BOOL, VAL_BOOL => L.VAL_TEXT(1 .. L.VAL_LEN) /= R.VAL_TEXT(1 .. R.VAL_LEN));
    end if;
  end DIFF;

  function GREATER (L, R : ITEM_REC) return ITEM_REC is
  begin
    if not IS_INTE_OR_REAL_OR_BOOL_OR_CHARS(L) or else not IS_INTE_OR_REAL_OR_BOOL_OR_CHARS(R) then
      raise INVALID_ARGUMENT;
    end if;
    if L.KIND /= R.KIND then
      raise ARGUMENT_MISMATCH;
    end if;
    if L.KIND = INTE then
      return (KIND => BOOL, VAL_BOOL => L.VAL_INTE > R.VAL_INTE);
    elsif L.KIND = REAL then
      return (KIND => BOOL, VAL_BOOL => L.VAL_REAL > R.VAL_REAL);
    elsif L.KIND = BOOL then
      return (KIND => BOOL, VAL_BOOL => L.VAL_BOOL > R.VAL_BOOL);
    else
      return (KIND => BOOL, VAL_BOOL => L.VAL_TEXT(1 .. L.VAL_LEN) > R.VAL_TEXT(1 .. R.VAL_LEN));
    end if;
  end GREATER;

  function SMALLER (L, R : ITEM_REC) return ITEM_REC is
  begin
    if not IS_INTE_OR_REAL_OR_BOOL_OR_CHARS(L) or else not IS_INTE_OR_REAL_OR_BOOL_OR_CHARS(R) then
      raise INVALID_ARGUMENT;
    end if;
    if L.KIND /= R.KIND then
      raise ARGUMENT_MISMATCH;
    end if;
    if L.KIND = INTE then
      return (KIND => BOOL, VAL_BOOL => L.VAL_INTE < R.VAL_INTE);
    elsif L.KIND = REAL then
      return (KIND => BOOL, VAL_BOOL => L.VAL_REAL < R.VAL_REAL);
    elsif L.KIND = BOOL then
      return (KIND => BOOL, VAL_BOOL => L.VAL_BOOL < R.VAL_BOOL);
    else
      return (KIND => BOOL, VAL_BOOL => L.VAL_TEXT(1 .. L.VAL_LEN) < R.VAL_TEXT(1 .. R.VAL_LEN));
    end if;
  end SMALLER;

  function GREATEQ (L, R : ITEM_REC) return ITEM_REC is
  begin
    if not IS_INTE_OR_REAL_OR_BOOL_OR_CHARS(L) or else not IS_INTE_OR_REAL_OR_BOOL_OR_CHARS(R) then
      raise INVALID_ARGUMENT;
    end if;
    if L.KIND /= R.KIND then
      raise ARGUMENT_MISMATCH;
    end if;
    if L.KIND = INTE then
      return (KIND => BOOL, VAL_BOOL => L.VAL_INTE >= R.VAL_INTE);
    elsif L.KIND = REAL then
      return (KIND => BOOL, VAL_BOOL => L.VAL_REAL >= R.VAL_REAL);
    else
      return (KIND => BOOL, VAL_BOOL => L.VAL_BOOL >= R.VAL_BOOL);
    end if;
  end GREATEQ;

  function SMALLEQ (L, R : ITEM_REC) return ITEM_REC is 
  begin
    if not IS_INTE_OR_REAL_OR_BOOL_OR_CHARS(L) or else not IS_INTE_OR_REAL_OR_BOOL_OR_CHARS(R) then
      raise INVALID_ARGUMENT;
    end if;
    if L.KIND /= R.KIND then
      raise ARGUMENT_MISMATCH;
    end if;
    if L.KIND = INTE then
      return (KIND => BOOL, VAL_BOOL => L.VAL_INTE <= R.VAL_INTE);
    elsif L.KIND = REAL then
      return (KIND => BOOL, VAL_BOOL => L.VAL_REAL <= R.VAL_REAL);
    else
      return (KIND => BOOL, VAL_BOOL => L.VAL_BOOL <= R.VAL_BOOL);
    end if;
  end SMALLEQ;

  -- INTE->REAL
  function TOREAL  (X : ITEM_REC) return ITEM_REC is
  begin
    if X.KIND = REAL then
      return X;
    end if;
    if X.KIND /= INTE then
      raise INVALID_ARGUMENT;
    end if;
    return (KIND => REAL, VAL_REAL => MY_MATH.REAL(X.VAL_INTE));
  end TOREAL;

  -- REAL->INTE
  function ROUND (X : ITEM_REC) return ITEM_REC is
  begin
    if X.KIND = INTE then
      return X;
    end if;
    if X.KIND /= REAL then
      raise INVALID_ARGUMENT;
    end if;
    return (KIND => INTE, VAL_INTE => MY_MATH.ROUND(X.VAL_REAL));
  end ROUND;

  function TRUNC (X : ITEM_REC) return ITEM_REC is
  begin
    if X.KIND = INTE then
      return X;
    end if;
    if X.KIND /= REAL then
      raise INVALID_ARGUMENT;
    end if;
    return (KIND => INTE, VAL_INTE => MY_MATH.TRUNC(X.VAL_REAL));
  end TRUNC;

  -- REAL->REAL
  function INT (X : ITEM_REC) return ITEM_REC is
  begin
    if X.KIND = INTE then
      return X;
    end if;
    if X.KIND /= REAL then
      raise INVALID_ARGUMENT;
    end if;
    return (KIND => REAL, VAL_REAL => MY_MATH.INT(X.VAL_REAL));
  end INT;

  function FRAC (X : ITEM_REC) return ITEM_REC is
  begin
    if X.KIND = INTE then
      return X;
    end if;
    if X.KIND /= REAL then
      raise INVALID_ARGUMENT;
    end if;
    return (KIND => REAL, VAL_REAL => MY_MATH.FRAC(X.VAL_REAL));
  end FRAC;

  -- *->BOOL
  function ISREAL  (X : ITEM_REC) return ITEM_REC is
  begin
    return (KIND => BOOL, VAL_BOOL => X.KIND = REAL);
  end ISREAL;

  function ISINTE  (X : ITEM_REC) return ITEM_REC is
  begin
    return (KIND => BOOL, VAL_BOOL => X.KIND = INTE);
  end ISINTE;

  function ISSTR  (X : ITEM_REC) return ITEM_REC is
  begin
    return (KIND => BOOL, VAL_BOOL => X.KIND = CHRS);
  end ISSTR;

  function ISREG  (X : ITEM_REC) return ITEM_REC is
  begin
    return (KIND => BOOL, VAL_BOOL => X.KIND = REGI);
  end ISREG;


  -- BOOL,BOOL->BOOL
  function BOLAND  (L, R : ITEM_REC) return ITEM_REC is
  begin
    if L.KIND /= BOOL or else R.KIND /= BOOL then
      raise INVALID_ARGUMENT;
    end if;
    return (KIND => BOOL, VAL_BOOL => L.VAL_BOOL and then R.VAL_BOOL);
  end BOLAND;

  function BOLOR   (L, R : ITEM_REC) return ITEM_REC is
  begin
    if L.KIND /= BOOL or else R.KIND /= BOOL then
      raise INVALID_ARGUMENT;
    end if;
    return (KIND => BOOL, VAL_BOOL => L.VAL_BOOL or else R.VAL_BOOL);
  end BOLOR;

  function BOLXOR  (L, R : ITEM_REC) return ITEM_REC is
  begin
    if L.KIND /= BOOL or else R.KIND /= BOOL then
      raise INVALID_ARGUMENT;
    end if;
    return (KIND => BOOL, VAL_BOOL => L.VAL_BOOL /= R.VAL_BOOL);
  end BOLXOR;


  -- BOOL->BOOL
  function BOLNEG  (X : ITEM_REC) return ITEM_REC is
  begin
    if X.KIND /= BOOL then
      raise INVALID_ARGUMENT;
    end if;
    return (KIND => BOOL, VAL_BOOL => not X.VAL_BOOL);
  end BOLNEG;

  -- BOOL,*,*->*
  function IFTE    (X, A, B : ITEM_REC) return ITEM_REC is
  begin
    if X.KIND /= BOOL then
      raise INVALID_ARGUMENT;
    end if;
    if A.KIND /= B.KIND then
      raise ARGUMENT_MISMATCH;
    end if;
    if X.VAL_BOOL then
      return A;
    else
      return B;
    end if;
  end IFTE;

end OPERATIONS;

