with As.U;
package Variables is

  -- Reset all variables
  procedure Reset;

  -- Set a variable
  Invalid_Name : exception;
  procedure Set (Name, Value : in As.U.Asu_Us);
  function Is_Set (Name : As.U.Asu_Us) return Boolean;

  -- Set a volatile variable : Clean all volatile variables
  procedure Set_Volatile (Name, Value : in As.U.Asu_Us);
  procedure Clear_Volatiles;


  -- Expand the expression, using defined variables
  --  or env variables (or dummy variables if check)
  Expand_Error : exception;
  function Expand (Text : As.U.Asu_Us; Check_Only : Boolean := False)
                  return String;
  function Expand (Text : As.U.Asu_Us; Check_Only : Boolean := False)
                  return As.U.Asu_Us;
  -- Compute a numeric expression
  function Compute (Text : As.U.Asu_Us) return As.U.Asu_Us;

end Variables;

