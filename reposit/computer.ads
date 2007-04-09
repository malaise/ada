-- Basic computation of a oper b...
-- Where oper is +, -, * or /,
--  a and b are integers or ${Variable}
-- Supports parentheses.
package Computer is
  
  -- Variable management
  ----------------------
  -- Reset all variables
  procedure Reset;
  -- Set (store), maybe overwrite a variable
  procedure Set (Name : in String;
                 Value : in Integer);
  -- Check if a varaible is set
  function Is_Set (Name : String) return Boolean;
  -- Get a variable
  -- May raise Unknown_Variable
  function Get (Name : String) return Integer;
  

  -- Computation of expression
  -- May raise Invalid_Expression (parentheses, operations, values...)
  function Eval (Expression : String) return Integer;


  -- On Set, Get or Is_Set if empty name
  Invalid_Variable : exception;
  -- On Get or Eval when unknown variable
  Unknown_Variable : exception;
  -- On Eval when invalid expression (parentheses, operations, values...)
  Invalid_Expression : exception;
  
 end Computer;
 
