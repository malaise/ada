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


  -- Expansion can be either
  -- Check_Only : check syntax (replace only local vars that are set)
  -- Local_Only : replace local vars, forbid env vars
  -- Local_Env  : replace local and envviron vars
  type Exp_Mode_List is (Check_Only, Local_Only, Local_Env);


  -- Expand the expression, using defined variables
  --  or env variables (or dummy variables if check)
  -- Except if Check_Only, raise expand_error if a local or env var is not set
  Expand_Error : exception;
  function Expand (Text : As.U.Asu_Us; Exp_Mode : Exp_Mode_List)
                  return String;
  function Expand (Text : As.U.Asu_Us; Exp_Mode : Exp_Mode_List)
                  return As.U.Asu_Us;
  -- Compute a numeric expression
  function Compute (Text : As.U.Asu_Us) return As.U.Asu_Us;

end Variables;

