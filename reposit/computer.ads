-- Basic computation of a oper b...
-- Where oper is +, -, * or /,
--  a and b are integers or ${Variable}
-- Supports parentheses.
with Ada.Finalization;
with As.U, Hashed_List.Unique;
package Computer is

  type Memory_Type is tagged limited private;

  -- Variable management
  ----------------------
  -- There are 2 kinds of variables: persistent or not. This is just to provide
  --  a way to reset only the volatile (non-persistent) variables or all the
  --  variables

  -- Reset (delete) volatile or all variables
  procedure Reset (Memory : in out Memory_Type; Only_Volatile : in Boolean);

  -- Set (store), maybe overwrite a variable
  -- May raise Constant_Exists if a variable with this name already exists
  --  and if either previous value or new value is not Modifiable
  procedure Set (Memory : in out Memory_Type;
                 Name : in String;
                 Value : in String;
                 Modifiable : in Boolean;
                 Persistent : in Boolean);

  -- Check if a variable is set
  function Is_Set (Memory : Memory_Type; Name : String) return Boolean;

  -- Unset (delete) a variable
  -- May raise Unknown_Variable
  -- May raise Constant_Exists if a variable with this name already exists
  --  and is not Modifiable
  procedure Unset (Memory : in out Memory_Type;
                   Name : in String);

  -- Get a variable
  -- May raise Unknown_Variable
  function Get (Memory : Memory_Type; Name : String) return String;

  -- Get characteristics, may raise Unknown_Variable
  function Is_Modifiable (Memory : Memory_Type; Name : String) return Boolean;
  function Is_Persistent (Memory : Memory_Type; Name : String) return Boolean;


  -- External resolver of variables:
  -- If a variable is not Set, then Eval or Compute will call this resolver.
  -- If this resolver raises any exeption, then Unknown_Variable will be
  --  raised
  type Resolver_Access is access function (Name : String) return String;
  procedure Set_External_Resolver (Memory : in out Memory_Type;
                                   Resolver : in Resolver_Access);

  -- Resolve variables of an expresssion
  -- Variable delimiters may be backslashed for non expansion but then they
  --  must both be backslashed. Ex: \${Var\}
  function Eval (Memory : Memory_Type; Expression : String) return String;

  -- Computation of expression
  -- All variables must resolve to a number or empty
  -- Then only numbers, operators and penthesis are allowed
  -- May raise Invalid_Expression (space, parentheses, operations, values...)
  function Compute (Memory : Memory_Type; Expression : String) return Integer;

  -- On Set, Get or Is_Set if empty name
  Invalid_Variable : exception;
  -- On Set if a constant (not modifiable variable) with this name already
  --  exists, if Modifiable is set but a variable (modifiable or not) with this
  --  name already exists.
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
  type Var_Access is access all Var_Rec;
  procedure Set (To : out Var_Rec; Val : in Var_Rec);
  function Image (Element : Var_Rec) return String;
  function "=" (Current : Var_Rec ; Criteria : Var_Rec ) return Boolean;
  package Var_List_Mng is new Hashed_List (Var_Rec, Var_Access,
                                           Set, "=", Image);
  package Var_Mng is new Var_List_Mng.Unique;
  type List_Access is access Var_Mng.Unique_List_Type;

  type Memory_Type is limited new Ada.Finalization.Limited_Controlled with record
    Var_List : List_Access := new Var_Mng.Unique_List_Type;
    External_Resolver : Resolver_Access := null;
  end record;

  overriding procedure Finalize (Memory : in out Memory_Type);
end Computer;

