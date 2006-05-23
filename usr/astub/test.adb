-- This is a "complete" example
-- with Ada.Text_Io, Ada.Exceptions;
-- use Ada.Text_Io;
-- generic
  -- Some generic formal arguments
  -- V : Integer := 5;
  -- type Typ is private;
  -- A function as generic formal argument
  -- with function ">" (A, B : Typ) return Boolean is <>;
package body Test is

  -- A type
  -- type Typ1 is new String;

  -- Another type
  -- type Typ2 is record
  --   Field1 : Integer;
  --   Field2 : Character;
  -- end record;

  -- A representation clause
  -- for Typ2 use record at mod 8;
  --   Field1 at 0 range 0 .. 31;
  --   Field2 at 4 range 0 ..  7;
  -- end record;


  -- A record type with discriminant
  -- type Typ3 (D : Character := ' ') is record
  --   case D is
  --     when '0' .. '9' =>
  --       Fi : Integer;
  --     when 'a' .. 'z' | 'A' .. 'Z' =>
  --       Fr : Float;
  --     when others =>
  --       null;
  --   end case;
  -- end record;

  -- Yet another type
  -- type Typ4 is access procedure (A : in Integer);


  -- A private type
  -- type Typ5 is private;


  -- A variable and a constant
  -- Var1 : Typ2;
  -- Const1 : constant Typ1 := "Test";
  -- Const2 : constant Typ5;

  -- A use clause within package spec
  -- use Ada.Exceptions;

  -- A procedure
  procedure Proc1 (A : in Integer) is
  begin
    null;
  end Proc1;


  -- A function
  function Func1 (A, B : Integer;
                    C : Character) return String is
  begin
    return Func1 (A, B, C);
  end Func1;

  -- Another function
  function Func2 return Integer is
  begin
    return Func2;
  end Func2;


  -- A package
  package body Pack1 is

    -- A function
    function Func3 (A, B : Integer; C : Character) return String is
    begin
      return Func3 (A, B, C);
    end Func3;

    -- A procedure
    procedure Proc2 (A : in Integer) is
    begin
      null;
    end Proc2;

  end Pack1;

  -- Another package
  package body Pack2 is

  end Pack2;


  -- A task
  task body Task1 is
  begin
    null;
  end Task1;

  -- A task type
  task body Task2 is
  begin
    -- entry Entr1 (Character) (I : in Integer);
    null;
  end Task2;


  -- A protected object
  protected body Prot1 is

    -- Procedure in protected
    procedure Proc3 (A : in Integer) is
    begin
      null;
    end Proc3;

    -- Function in protected
    function Func4 (I : in Character) return Positive is
    begin
      return Func4 (I);
    end Func4;

    -- Entry in protected
    entry Entr2 (for I in Character) (I : in Integer) when True is
    begin
      null;
    end Entr2;

  end Prot1;


  -- Another protected type
  protected body Prot2 is

  end Prot2;


  -- Two renaming declarations
  -- procedure Proc4 (A : in Integer) renames Proc1;

  -- function Func3 return Integer renames Func2;


-- private


  -- type Typ5 is new Integer;

  -- Const2 : constant Typ5 := 21;
  function Func6 return Typ5 is
  begin
    return Func6;
  end Func6;


end Test;


