with My_Math;
package Complexes is

  subtype Real is My_Math.Real;

  -- Complex type
  type Complex is tagged private;
  I : constant Complex;
  function Create_Complex (Real_Part, Imag_Part : Real) return Complex;
  procedure Set (C : out Complex; Real_Part, Imag_Part : in Real);
  function Create_Complex (Real_Part : Real) return Complex;
  procedure Set (C : out Complex; Real_Part : in Real);
  function Part_Real (C : Complex) return Real;
  function Part_Imag (C : Complex) return Real;

  -- Types for angles of polar representation
  type Radian is new Real;
  subtype Reducted_Radian is Radian range 0.0 .. 2.0 * My_Math.Pi;
  function Reduct (A : Radian) return Reducted_Radian;

  type Degree is new Real;
  subtype Reducted_Degree is Degree range 0.0 .. 360.0;
  function Reduct (A : Degree) return Reducted_Degree;

  function To_Degree (A : Radian) return Reducted_Degree;
  function To_Radian (A : Degree) return Reducted_Radian;

  -- Polar representation
  type Polar is private;
  subtype Typ_Module is Real range 0.0 .. Real'Last;
  function Create_Polar (M : Typ_Module; A : Radian) return Polar;
  function Create_Polar (M : Typ_Module; A : Degree) return Polar;
  function Module (P : Polar) return Typ_Module;
  function Angle_Radian (P : Polar) return Reducted_Radian;
  function Angle_Degree (P : Polar) return Reducted_Degree;

  -- Conversion
  function To_Polar   (C : Complex) return Polar;
  function To_Complex (P : Polar)   return Complex;

  -- Operations
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

  function Image (C : Complex) return String;
  function Get (X, Y : String) return Complex;
  procedure Get (C : out Complex; X, Y : in String);

  private
    type Complex is tagged record
      Part_Real : Real;
      Part_Imag : Real;
    end record;


    type Polar is record
      Module   : Typ_Module;
      Argument : Reducted_Radian;
    end record;

    I : constant Complex := (0.0, 1.0);

end Complexes;

