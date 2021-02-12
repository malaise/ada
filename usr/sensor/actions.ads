package Actions is

  -- Define a variable
  Invalid_Variable : exception;
  procedure Define_Variable (Name : in String; Value : in String);

  -- Resolve variables in a string
  Unknown_Variable : exception;
  function Expand_Variables (Text : String) return String;

  -- Store an action by name
  procedure Store (Name : in String; Action : in String);

  -- Check that an action exists
  function Exists (Name : in String) return Boolean;

  -- Check a command (variables)
  -- Return the error message or empty
  function Check_Command (Command : String) return String;

  -- Read an action and expand the command
  Unknown_Action: exception;
  function Expand (Name : String; Lines : String) return String;

end Actions;

