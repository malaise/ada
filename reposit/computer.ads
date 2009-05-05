-- Basic computation of a oper b...
-- Where oper is +, -, * or /,
--  a and b are integers or ${Variable}
-- Supports parentheses.
package Computer is

  -- Variable management
  ----------------------
  -- Reset not persistent or all variables
  procedure Reset (Not_Persistent : in Boolean);
  -- Set (store), maybe overwrite a variable
  -- May raise Constant_Exists if Modifiable is set and a variable
  --  with this name already exists, or if Modifiable is not set but a
  --  non modifiable variable with this name already exists.
  procedure Set (Name : in String;
                 Value : in String;
                 Modifiable : in Boolean;
                 Persistent : in Boolean);
  -- Check if a variable is set
  function Is_Set (Name : String) return Boolean;
  -- Get a variable
  -- May raise Unknown_Variable
  function Get (Name : String) return String;
  -- Get characteristics, may raise Unknown_Variable
  function Is_Modifiable (Name : String) return Boolean;
  function Is_Persistent (Name : String) return Boolean;


  -- External resolver of variables:
  -- If a variable is not Set, then Eval or Compute will call this resolver.
  -- If this resolver raises any exeption, then Unknown_Variable will be
  --  raised
  type Resolver_Access is access function (Name : String) return String;
  External_Resolver : Resolver_Access := null;

  -- Resolv variables of an expresssion
  function Eval (Expression : String) return String;

  -- Computation of expression
  -- May raise Invalid_Expression (parentheses, operations, values...)
  function Compute (Expression : String) return Integer;

  -- On Set, Get or Is_Set if empty name
  Invalid_Variable : exception;
  -- On Set if Modifiable is set and a variable
  --  with this name already exists, or if Modifiable is not set but a
  --  non modifiable variable with this name already exists.
  Constant_Exists : exception;
  -- On Get or Eval when unknown variable
  Unknown_Variable : exception;
  -- On Eval when invalid expression (parentheses, operations, values...)
  Invalid_Expression : exception;

 end Computer;

