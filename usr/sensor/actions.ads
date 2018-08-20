package Actions is

  -- Store an action by name
  procedure Store (Name : in String; Action : in String);

  -- Check that an action exists
  function Exists (Name : in String) return Boolean;

  -- Check a command (variables)
  -- Return the error message or empty
  Unknown_Variable : exception;
  function Check_Command (Command : String) return String;

  -- Read an action and expand the command
  Unknown_Action: exception;
  function Expand (Name : String; Lines : String) return String;

end Actions;

