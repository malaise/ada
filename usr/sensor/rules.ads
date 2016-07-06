package Rules is

  -- Store a rule by name
  Already_Defined : exception;
  procedure Store (Name : in String; Action : in String);

  -- Check that a rule exists
  Known_Rule : exception;
  procedure Check_Exists (Name : in String); 

  -- Check that all variables of a rule are defined (dummy expansion)
  -- Return the name of an unknonw variable (or empty)
  function Check_Variables (Action : String) return String;

  -- Read a rule and expand the action
  Unknown_Variable: exception;
  function Expand (Name : String; Line : String) return String;

end Rules;

