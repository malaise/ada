with As.U; use As.U;
package Variables is

  -- Check that expression expands correctly (even if some Vars are unknown)
  Check_Error : exception;
  procedure Check (Text : in Asu_Us);

  -- Set a variable
  procedure Set (Name, Value : in Asu_Us);

  -- Reset all variables
  procedure Reset;

  -- Expand the expression, using defined variables or env variables
  Expand_Error : exception;
  function Expand (Text : Asu_Us) return String;
  function Expand (Text : Asu_Us) return Asu_Us;

end Variables;

