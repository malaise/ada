package Rules is

  -- Store a rule by name
  procedure Store (Name : in String; Action : in String);

  -- Check that a rule exists
  function Exists (Name : in String) return Boolean;

  -- Check an action (variables)
  -- Return the error message or empty
  function Check_Action (Action : String) return String;

  -- Read a rule and expand the action
  Unknown_Variable: exception;
  function Expand (Name : String; Line : String) return String;

end Rules;

