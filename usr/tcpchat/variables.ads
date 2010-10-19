with As.U; use As.U;
package Variables is

  -- Reset all variables
  procedure Reset;

  -- Set a variable
  Invalid_Name : exception;
  procedure Set (Name, Value : in Asu_Us);
  function Is_Set (Name : Asu_Us) return Boolean;

  -- Set a volatile variable : Clean all volatile variables
  procedure Set_Volatile (Name, Value : in Asu_Us);
  procedure Clear_Volatiles;


  -- Expand the expression, using defined variables
  --  or env variables (or dummy variables if check)
  Expand_Error : exception;
  function Expand (Text : Asu_Us; Check_Only : Boolean := False) return String;
  function Expand (Text : Asu_Us; Check_Only : Boolean := False) return Asu_Us;
  -- Compute a numeric expression
  function Compute (Text : Asu_Us) return Asu_Us;

end Variables;

