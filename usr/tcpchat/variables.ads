with As.U; use As.U;
package Variables is

  -- Reset all variables
  procedure Reset;

  -- Set a variable
  procedure Set (Name, Value : in Asu_Us);

  -- Expand the expression, using defined variables
  --  or env variables (or dummy variables if check)
  Expand_Error : exception;
  function Expand (Text : Asu_Us; Check_Only : Boolean := False) return String;
  function Expand (Text : Asu_Us; Check_Only : Boolean := False) return Asu_Us;

end Variables;

