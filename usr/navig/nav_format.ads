with Nav_Types;
-- to convert got strings in data fields, and fields in string to be put
package Nav_Format is

  type Format_Result is (Set, Unset, Error);

  -- from speed to string
  function Imag (Speed : Nav_Types.T_Speed; Set : Boolean := True)
   return String;
  -- from string to angle
  function Imag (Angle : Nav_Types.T_Angle; Set : Boolean := True)
   return String;
  -- from string to drift
  function Imag (Drift : Nav_Types.T_Drift; Set : Boolean := True)
   return String;

  -- from string to speed
  -- if error, then pos is it's position
  procedure Value (Str : in String;
   Speed : out Nav_Types.T_Speed; Res : out Format_Result; Pos : out Positive);
  -- from string to angle
  procedure Value (Str : in String;
   Angle : out Nav_Types.T_Angle; Res : out Format_Result; Pos : out Positive);
  -- from drift to speed
  procedure Value (Str : in String;
   Drift : out Nav_Types.T_Drift; Res : out Format_Result; Pos : out Positive);

end Nav_Format;

