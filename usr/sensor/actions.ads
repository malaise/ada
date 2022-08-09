with As.U.Utils;
package Actions is

  -- Define a variable
  Invalid_Variable : exception;
  procedure Define_Variable (Name : in String; Value : in String);

  -- Define / Undefine the triggereing Action
  procedure Set_Action (Value : in String);
  procedure Unset_Action;

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

  -- Register a repetition
  procedure Add_Repeat (Name : in String;
                        Number : in Positive;
                        During : in Duration;
                        Triggers : in String);

  -- Declare several occurences
  -- Which may trigger the execution of several actions
  function Occurs (Action : in String; Nb : in Positive)
           return As.U.Utils.Asu_Array;

   -- Purge old occurences
   procedure Purge;

end Actions;

