with TEXT_IO;
with MATH;
package NBRES_C is

  subtype REAL is MATH.REAL;

  package MY_REAL_IO is new TEXT_IO.FLOAT_IO (REAL);

  type COMPLEX is private;
  I : constant COMPLEX;

  type RADIAN is new REAL;
  subtype REDUCTED_RADIAN is RADIAN range 0.0 .. 2.0*MATH.PI;

  type DEGREE is new REAL;
  subtype REDUCTED_DEGREE is DEGREE range 0.0 .. 360.0;

  function REDUCT (A : RADIAN) return REDUCTED_RADIAN;
  function REDUCT (A : DEGREE) return REDUCTED_DEGREE;

  function TO_DEGREE (A : RADIAN) return REDUCTED_DEGREE;
  function TO_RADIAN (A : DEGREE) return REDUCTED_RADIAN;

  subtype TYP_MODULE is REAL range 0.0 .. REAL'LAST;
  function CREATE_COMPLEX (M : TYP_MODULE; A : RADIAN) return COMPLEX;
  function CREATE_COMPLEX (M : TYP_MODULE; A : DEGREE) return COMPLEX;
  function MODULE (C : COMPLEX) return TYP_MODULE;
  function ANGLE_RADIAN (C : COMPLEX) return REDUCTED_RADIAN;
  function ANGLE_DEGREE (C : COMPLEX) return REDUCTED_DEGREE;

  function PART_REAL (C : COMPLEX) return REAL;
  function PART_IMAG (C : COMPLEX) return REAL;
  function CREATE_COMPLEX (REAL_PART, IMAG_PART : REAL) return COMPLEX;
  function CREATE_COMPLEX (REAL_PART : REAL)            return COMPLEX;

  function "+" (X, Y : COMPLEX)           return COMPLEX;
  function "+" (X : REAL;    Y : COMPLEX) return COMPLEX;
  function "+" (X : COMPLEX; Y : REAL)    return COMPLEX;
  function "+" (X : REAL;    Y : REAL)    return COMPLEX;

  function "-" (X, Y : COMPLEX)           return COMPLEX;
  function "-" (X : REAL;    Y : COMPLEX) return COMPLEX;
  function "-" (X : COMPLEX; Y : REAL)    return COMPLEX;
  function "-" (X : REAL;    Y : REAL)    return COMPLEX;

  function "*" (X, Y : COMPLEX)           return COMPLEX;
  function "*" (X : REAL;    Y : COMPLEX) return COMPLEX;
  function "*" (X : COMPLEX; Y : REAL)    return COMPLEX;
  function "*" (X : REAL;    Y : REAL)    return COMPLEX;

  function "/" (X, Y : COMPLEX)           return COMPLEX;
  function "/" (X : REAL;    Y : COMPLEX) return COMPLEX;
  function "/" (X : COMPLEX; Y : REAL)    return COMPLEX;
  function "/" (X : REAL;    Y : REAL)    return COMPLEX;

  function "**" (X : COMPLEX; Y : REAL)   return COMPLEX;

  procedure PUT (C : in COMPLEX);
  procedure GET (C : out COMPLEX);

  private
    type COMPLEX is record
      PART_REAL : REAL;
      PART_IMAG : REAL;
    end record;

    I : constant COMPLEX := (0.0, 1.0);

end NBRES_C;

