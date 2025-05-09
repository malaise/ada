-- Get environment variables
with As.U;
package Environ is

  -- Getenv for a String. Returns empty string if ENV var is not set.
  function Getenv (Env_Name : String) return String;

  -- Getenv for a String. Raises Name_Error if ENV var is not set.
  Name_Error : exception;
  function Getenv_If_Set (Env_Name : String) return String;

  -- Getenv for a String.
  -- Leave result and length unchanged if ENV var is not set or trunc,
  --  otherwise set them.
  procedure Get_Str (Name : in String; Result : in out String;
                                       Length : in out Natural);
  -- Getenv for a unbounded string
  -- Leave result unchanged if ENV var is not set, otherwise set it.
  procedure Get_Us (Name : in String; Result : in out As.U.Asu_Us);

  -- Getenv an Integer
  -- First variant returns Default if ENV var is not set or trunc or empty,
  --  or if has an invalid content
  -- Second variant leaves Result unchanged in these cases
  function  Get_Int (Name : String; Default : Integer) return Integer;
  procedure Get_Int (Name : in String; Result : in out Integer);

  -- Getenv a Natural
  function  Get_Nat (Name : String; Default : Natural) return Natural;
  procedure Get_Nat (Name : in String; Result : in out Natural);

  -- Getenv a Positive
  function  Get_Pos (Name : String; Default : Positive) return Positive;
  procedure Get_Pos (Name : in String; Result : in out Positive);

  -- Getenv a generic integer or modular (first variant)
  generic
    type Num is range <>;
  function Get_Num (Name : String; Default : Num) return Num;
  generic
    type Modul is mod <>;
  function Get_Mod (Name : String; Default : Modul) return Modul;


  -- Getenv a Duration (positive or null)
  subtype Pos_Duration is Duration range 0.0 .. Duration'Last;
  function  Get_Dur (Name : String; Default : Pos_Duration) return Pos_Duration;
  procedure Get_Dur (Name : in String; Result : in out Pos_Duration);

  -- Is ENV var set
  function Is_Set (Name : String) return Boolean;

  -- Is ENV var set and its lower case is "y" or "yes"
  function Is_Yes (Name : String) return Boolean;
  -- Is ENV var set and its lower case is "n" or "no"
  function Is_No (Name : String) return Boolean;

end Environ;

