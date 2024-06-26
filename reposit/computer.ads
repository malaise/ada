-- Store variables and perform evaluation of an expression referencing them
-- Also perform basic computation of an expression like: a o b...
--  where operation "o" is an operator +, -, * or /,
--  and operands "a" or "b" are either arbitrary integers (num, +num and -num)
--  or references to variables ${Variable}
--  Supports parentheses
-- Both support an optional external variable resolver
private with As.U, Hashed_List.Unique, Environ;
package Computer is

  -- A memory set
  type Memory_Type is tagged limited private;

  -- Variable management
  ----------------------
  -- There are 2 kinds of variables: persistent or not. This is just to provide
  --  a way to reset only the volatile (non-persistent) variables or all the
  --  variables

  -- Reset (delete) only volatile variables
  --  or all (volatile and persistent) variables
  procedure Reset (Memory : in out Memory_Type; Only_Volatile : in Boolean);

  -- Set (store), maybe overwrite a variable
  -- May raise Invalid_Variable is name contains a space or Htab
  -- May raise Constant_Exists if a variable with this name already exists
  --  and if either previous value or new value is not Modifiable
  procedure Set (Memory : in out Memory_Type;
                 Name : in String;
                 Value : in String;
                 Modifiable : in Boolean;
                 Persistent : in Boolean);

  -- Check if a variable is set
  function Is_Set (Memory : in out Memory_Type;
                   Name : in String) return Boolean;

  -- Unset (delete) a variable
  -- May raise Unknown_Variable
  -- May raise Constant_Exists if a variable with this name already exists
  --  and is not Modifiable
  procedure Unset (Memory : in out Memory_Type;
                   Name : in String);

  -- Get a variable
  -- May raise Unknown_Variable
  function Get (Memory : in out Memory_Type;
                Name : in String) return String;

  -- Get characteristics, may raise Unknown_Variable
  function Is_Modifiable (Memory : in out Memory_Type;
                          Name : in String) return Boolean;
  function Is_Persistent (Memory : in out Memory_Type;
                          Name : in String) return Boolean;


  -- External resolver of variables:
  -- If a variable is not Set, then Eval or Compute will call this resolver
  -- If no resolver is set (null) then Unknown_Variable is raised
  -- If this resolver raises any exception, then Unknown_Variable is raised
  type Resolver_Access is access function (Name : String) return String;
  procedure Set_External_Resolver (Memory : in out Memory_Type;
                                   Resolver : in Resolver_Access);

  -- Convenient resolver searching environment variables
  Env_Resolver : constant Resolver_Access;

  -- Resolve variables of an expresssion
  -- Variable delimiters may be backslashed for no expansion, but in this case
  --  then they must be both backslashed. Ex: \${Var\}
  function Eval (Memory : in out Memory_Type;
                 Expression : in String) return String;

  -- On Set, Get or Is_Set if empty or incorrect name
  Invalid_Variable : exception;
  -- On Set if a constant (not modifiable variable) with this name already
  --  exists, or if Modifiable is set but a variable (modifiable or not) with
  --  this name already exists.
  -- On Unset if a constant (not modifiable variable) with this name already
  -- exists.
  Constant_Exists : exception;
  -- On Get or Eval when unknown variable
  Unknown_Variable : exception;
  -- On Eval when invalid expression (parentheses, operations, values...)
  Invalid_Expression : exception;

private
  -- List of variables
  type Var_Rec is record
    -- Variable name
    Name : As.U.Asu_Us;
    -- Variable value
    Value : As.U.Asu_Us;
    -- Persistent:
    -- If modifiable, at most one persistent and one volatile variables
    --  of same name,
    -- If not modifiable, either one persistent or one volatile
    Persistent : Boolean;
    -- Modifiable
    Modifiable : Boolean;
  end record;

  -- Unique Hash list of variables
  procedure Set (To : out Var_Rec; Val : in Var_Rec);
  function Image (Element : Var_Rec) return String;
  overriding function "=" (Current : Var_Rec;
                           Criteria : Var_Rec ) return Boolean;
  package Var_List_Mng is new Hashed_List (Var_Rec, Set, "=", Image);
  package Var_Mng is new Var_List_Mng.Unique;

  -- Memory set
  type Memory_Type is tagged limited record
    Var_List : Var_Mng.Unique_List_Type;
    External_Resolver : Resolver_Access := null;
  end record;

  -- Resolver based on Getenv
  Env_Resolver : constant Resolver_Access := Environ.Getenv_If_Set'Access;

  -- Trace in logger
  procedure Log (Msg : in String);

  -- Evaluate an expression
  function Internal_Eval (Memory : in out Memory_Type;
                          Expression : in String;
                          Check : in Boolean) return String;
end Computer;

