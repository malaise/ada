with My_Math;
package C_Nbres is

  subtype Real is My_Math.Real;

  type Complex is private;
  I : constant Complex;

  type Radian is new Real;
  subtype Reducted_Radian is Radian range 0.0 .. 2.0 * My_Math.Pi;

  type Degree is new Real;
  subtype Reducted_Degree is Degree range 0.0 .. 360.0;

  function Reduct (A : Radian) return Reducted_Radian;
  function Reduct (A : Degree) return Reducted_Degree;

  function To_Degree (A : Radian) return Reducted_Degree;
  function To_Radian (A : Degree) return Reducted_Radian;

  subtype Typ_Module is Real range 0.0 .. Real'Last;
  function Create_Complex (M : Typ_Module; A : Radian) return Complex;
  function Create_Complex (M : Typ_Module; A : Degree) return Complex;
  function Module (C : Complex) return Typ_Module;
  function Angle_Radian (C : Complex) return Reducted_Radian;
  function Angle_Degree (C : Complex) return Reducted_Degree;

  function Part_Real (C : Complex) return Real;
  function Part_Imag (C : Complex) return Real;
  function Create_Complex (Real_Part, Imag_Part : Real) return Complex;
  function Create_Complex (Real_Part : Real)            return Complex;

  function "+" (X, Y : Complex)           return Complex;
  function "+" (X : Real;    Y : Complex) return Complex;
  function "+" (X : Complex; Y : Real)    return Complex;
  function "+" (X : Real;    Y : Real)    return Complex;

  function "-" (X, Y : Complex)           return Complex;
  function "-" (X : Real;    Y : Complex) return Complex;
  function "-" (X : Complex; Y : Real)    return Complex;
  function "-" (X : Real;    Y : Real)    return Complex;

  function "*" (X, Y : Complex)           return Complex;
  function "*" (X : Real;    Y : Complex) return Complex;
  function "*" (X : Complex; Y : Real)    return Complex;
  function "*" (X : Real;    Y : Real)    return Complex;

  function "/" (X, Y : Complex)           return Complex;
  function "/" (X : Real;    Y : Complex) return Complex;
  function "/" (X : Complex; Y : Real)    return Complex;
  function "/" (X : Real;    Y : Real)    return Complex;

  function "**" (X : Complex; Y : Real)   return Complex;

  procedure Put (C : in Complex);
  procedure Get (C : out Complex);

  private
    type Complex is record
      Part_Real : Real;
      Part_Imag : Real;
    end record;

    I : constant Complex := (0.0, 1.0);

end C_Nbres;

