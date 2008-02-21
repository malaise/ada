with Text_Handler;
package Environ is

  -- Getenv for a String. Returns empty string if not set.
  function Getenv (Env_Name : String) return String;

  -- Getenv for a String. Raises Name_Error if not set.
  function Getenv_If_Set (Env_Name : String) return String;
  Name_Error : exception;

  -- Getenv for a String.
  -- Leave result and length unchanged if not set or trunc or empty
  -- otherwise set them.
  procedure Get_Str (Name : String; Result : in out String;
                                    Length : in out Natural);
  -- Getenv for a Text.
  procedure Get_Txt (Name : String; Result : in out Text_Handler.Text);

  -- Getenv an Integer
  function  Get_Int (Name : String; Default : Integer) return Integer;
  procedure Get_Int (Name : String; Result : in out Integer);

  -- Getenv a Natural
  function  Get_Nat (Name : String; Default : Natural) return Natural;
  procedure Get_Nat (Name : String; Result : in out Natural);

  -- Getenv a Positive
  function  Get_Pos (Name : String; Default : Positive) return Positive;
  procedure Get_Pos (Name : String; Result : in out Positive);

  -- Getenv a Duration (positive or null)
  subtype Pos_Duration is Duration range 0.0 .. Duration'Last;
  function  Get_Dur (Name : String; Default : Pos_Duration) return Pos_Duration;
  procedure Get_Dur (Name : String; Result : in out Pos_Duration);

  -- Is variable set
  function Is_Set (Name : String) return Boolean;

  -- Is variable set and its lower case is "y" or "yes"
  function Is_Yes (Name : String) return Boolean;
  -- Is variable set and its lower case is "n" or "no"
  function Is_No (Name : String) return Boolean;

end Environ;

