package body C_NBRES is

  use MY_MATH;

  -- ANGLES en RADIAN
  package POLAR is

    type POLAR is record
      MODULE   : REAL;
      ARGUMENT : REAL;
    end record;

    function TO_POLAR   (X : COMPLEX) return POLAR;
    function TO_COMPLEX (X : POLAR)   return COMPLEX;

  end POLAR;

  package body POLAR is

    function TO_POLAR (X : COMPLEX) return POLAR is
      X_POL : POLAR;
    begin
      X_POL.MODULE := SQRT
       (X.PART_REAL * X.PART_REAL + X.PART_IMAG * X.PART_IMAG);
      if (X.PART_REAL /= 0.0) then
        X_POL.ARGUMENT := ARC_TG (X.PART_IMAG / X.PART_REAL);
      else
        X_POL.ARGUMENT := PI / 2.0;
      end if;
      if X.PART_REAL < 0.0 then
        X_POL.ARGUMENT := X_POL.ARGUMENT + PI;
      end if;
      return (X_POL);
    end TO_POLAR;

    function TO_COMPLEX (X : POLAR)  return COMPLEX is
      X_CART : COMPLEX;
    begin
      X_CART.PART_REAL := X.MODULE * COS (X.ARGUMENT);
      X_CART.PART_IMAG := X.MODULE * SIN (X.ARGUMENT);
      return (X_CART);
    end TO_COMPLEX;

  end POLAR;


  function REDUCT (A : RADIAN) return REDUCTED_RADIAN is
    N : INTE;
    TWO_PI : constant RADIAN := REDUCTED_RADIAN'LAST;
    R : RADIAN;
  begin
    N := TRUNC (REAL(A / (TWO_PI)));
    R := A - RADIAN(N) * RADIAN(TWO_PI);
    if R < 0.0 then R := R + TWO_PI; end if;
    return R;
  end REDUCT;

  function REDUCT (A : DEGREE)  return REDUCTED_DEGREE is
    N : INTE;
    ONE_CIRCLE : constant DEGREE := REDUCTED_DEGREE'LAST;
    R : DEGREE;
  begin
    N := TRUNC (REAL(A / (ONE_CIRCLE)));
    R := A - DEGREE(N) * DEGREE(ONE_CIRCLE);
    if R < 0.0 then R := R + ONE_CIRCLE; end if;
    return R;
  end REDUCT;

  function TO_DEGREE (A : RADIAN) return REDUCTED_DEGREE is
    R : REAL;
  begin
    R := REAL(REDUCT(A)/REDUCTED_RADIAN'LAST) * REAL(REDUCTED_DEGREE'LAST);
    return REDUCTED_DEGREE (R);
  end TO_DEGREE;

  function TO_RADIAN (A : DEGREE) return REDUCTED_RADIAN is
    R : REAL;
  begin
    R := REAL(REDUCT(A)/REDUCTED_DEGREE'LAST) * REAL(REDUCTED_RADIAN'LAST);
    return REDUCTED_RADIAN (R);
  end TO_RADIAN;


  function CREATE_COMPLEX (M : TYP_MODULE; A : RADIAN) return COMPLEX is
  begin
    return POLAR.TO_COMPLEX (
     (MODULE => REAL(M), ARGUMENT => REAL(REDUCT(A))));
  end CREATE_COMPLEX;

  function CREATE_COMPLEX (M : TYP_MODULE; A : DEGREE) return COMPLEX is
  begin
    return POLAR.TO_COMPLEX (
     (MODULE => REAL(M), ARGUMENT => REAL(TO_RADIAN(A))) );
  end CREATE_COMPLEX;

  function MODULE (C : COMPLEX) return TYP_MODULE is
    M : REAL := POLAR.TO_POLAR (C).MODULE;
  begin
    return TYP_MODULE (abs(M));
  end MODULE;

  function ANGLE_RADIAN (C : COMPLEX) return REDUCTED_RADIAN is
  begin
    return REDUCT(RADIAN(POLAR.TO_POLAR(C).ARGUMENT));
  end ANGLE_RADIAN;

  function ANGLE_DEGREE (C : COMPLEX) return REDUCTED_DEGREE is
  begin
    return REDUCT (TO_DEGREE(ANGLE_RADIAN(C)));
  end ANGLE_DEGREE;



  function PART_REAL (C : COMPLEX) return REAL is
  begin
    return C.PART_REAL;
  end PART_REAL;

  function PART_IMAG (C : COMPLEX) return REAL is
  begin
    return C.PART_IMAG;
  end PART_IMAG;

  function CREATE_COMPLEX (REAL_PART, IMAG_PART : REAL) return COMPLEX is
  begin
    return (PART_REAL => REAL_PART, PART_IMAG => IMAG_PART);
  end CREATE_COMPLEX;

  function CREATE_COMPLEX (REAL_PART : REAL) return COMPLEX is
  begin
    return (PART_REAL => REAL_PART, PART_IMAG => 0.0);
  end CREATE_COMPLEX;

  -----------------------------------------------------------------------------

  function "+" (X, Y : COMPLEX) return COMPLEX is
  begin
    return (PART_REAL => X.PART_REAL + Y.PART_REAL,
            PART_IMAG => X.PART_IMAG + Y.PART_IMAG);
  end "+";

  function "+" (X : REAL; Y : COMPLEX) return COMPLEX is
  begin
    return (PART_REAL => X   + Y.PART_REAL,
            PART_IMAG =>       Y.PART_IMAG);
  end "+";

  function "+" (X : COMPLEX; Y : REAL) return COMPLEX is
  begin
    return (PART_REAL => X.PART_REAL + Y,
            PART_IMAG => X.PART_IMAG);
  end "+";

  function "+" (X : REAL; Y : REAL) return COMPLEX is
  begin
    return (PART_REAL => X + Y,
            PART_IMAG => 0.0);
  end "+";

  -----------------------------------------------------------------------------

  function "-" (X, Y : COMPLEX) return COMPLEX is
  begin
    return (PART_REAL => X.PART_REAL - Y.PART_REAL,
            PART_IMAG => X.PART_IMAG - Y.PART_IMAG);
  end "-";

  function "-" (X : REAL; Y : COMPLEX) return COMPLEX is
  begin
    return (PART_REAL => X   - Y.PART_REAL,
            PART_IMAG =>     - Y.PART_IMAG);
  end "-";

  function "-" (X : COMPLEX; Y : REAL) return COMPLEX is
  begin
    return (PART_REAL => X.PART_REAL - Y,
            PART_IMAG => X.PART_IMAG);
  end "-";

  function "-" (X : REAL; Y : REAL) return COMPLEX is
  begin
    return (PART_REAL => X - Y,
            PART_IMAG => 0.0);
  end "-";

  -----------------------------------------------------------------------------

  function "*" (X, Y : COMPLEX) return COMPLEX is
  begin
    return (PART_REAL => X.PART_REAL * Y.PART_REAL
                       - X.PART_IMAG * Y.PART_IMAG,
            PART_IMAG => X.PART_REAL * Y.PART_IMAG
                       + X.PART_IMAG * Y.PART_REAL);
  end "*";

  function "*" (X : REAL; Y : COMPLEX) return COMPLEX is
  begin
    return (PART_REAL => X * Y.PART_REAL,
            PART_IMAG => X * Y.PART_IMAG);
  end "*";

  function "*" (X : COMPLEX; Y : REAL) return COMPLEX is
  begin
    return (PART_REAL => X.PART_REAL * Y,
            PART_IMAG => X.PART_IMAG * Y);
  end "*";

  function "*" (X : REAL; Y : REAL) return COMPLEX is
  begin
    return (PART_REAL => X * Y,
            PART_IMAG => 0.0);
  end "*";

  -----------------------------------------------------------------------------

  function "/" (X, Y : COMPLEX) return COMPLEX is
    MODULE_Y : REAL := Y.PART_REAL * Y.PART_REAL
                     + Y.PART_IMAG * Y.PART_IMAG;
  begin
    return (PART_REAL =>
                ( (X.PART_REAL * Y.PART_REAL) + (X.PART_IMAG * Y.PART_IMAG) )
                / MODULE_Y ,
      PART_IMAG =>
                ( (X.PART_IMAG * Y.PART_REAL) - (X.PART_REAL * Y.PART_IMAG) )
                / MODULE_Y );
  end "/";

  function "/" (X : REAL; Y : COMPLEX) return COMPLEX is
    MODULE_Y : REAL := Y.PART_REAL * Y.PART_REAL
                     + Y.PART_IMAG * Y.PART_IMAG;
  begin
    return (PART_REAL => (  X * Y.PART_REAL / MODULE_Y),
            PART_IMAG => (- X * Y.PART_IMAG / MODULE_Y) );
  end "/";

  function "/" (X : COMPLEX; Y : REAL) return COMPLEX is
  begin
    return (PART_REAL => X.PART_REAL / Y,
            PART_IMAG => X.PART_IMAG / Y);
  end "/";

  function "/" (X : REAL; Y : REAL) return COMPLEX is
  begin
    return (PART_REAL => X / Y,
            PART_IMAG => 0.0);
  end "/";

  -----------------------------------------------------------------------------

  function "**" (X : COMPLEX; Y : REAL) return COMPLEX is
    X_POLAR : POLAR.POLAR;
    RES_COMPLEX : COMPLEX;
  begin
    X_POLAR := POLAR.TO_POLAR (X);
    X_POLAR := (MODULE   => X_POLAR.MODULE ** Y,
                ARGUMENT => X_POLAR.ARGUMENT *  Y);
    RES_COMPLEX := POLAR.TO_COMPLEX (X_POLAR);
    return RES_COMPLEX;
  end "**";

  -------------------------------------------------------------------------------

  use MY_REAL_IO;

  procedure PUT (C : in COMPLEX) is
    IMAG_PART : REAL := PART_IMAG (C);
  begin
    PUT (PART_REAL (C) );
    if (IMAG_PART >= 0.0) then
      TEXT_IO.PUT (" +");
    else
      TEXT_IO.PUT (" -");
    end if;
    PUT ( abs (IMAG_PART) );
    TEXT_IO.PUT (" * i ");
  end PUT;

  procedure GET (C : out COMPLEX) is
    REAL_PART, IMAG_PART : REAL;
  begin
    TEXT_IO.PUT ("Reel? ");
    GET (REAL_PART);
    TEXT_IO.PUT ("Imag? ");
    GET (IMAG_PART);
    C := CREATE_COMPLEX (REAL_PART, IMAG_PART);
  end GET;


end C_NBRES;
