-- This is a "complete" example
with Ada.Text_Io, Ada.Exceptions;
use Ada.Text_Io;
generic
  -- Some generic formal arguments
  V : Integer := 5;
  type Typ is private;
  -- A function as generic formal argument
  with function ">" (A, B : Typ) return Boolean is <>;
package Test is

  -- A type
  type Typ1 is new String;
  -- Another type
  type Typ2 is record
    Field1 : Integer;
    Field2 : Character;
  end record;
  -- A representation clause
  for Typ2 use record at mod 8;
    Field1 at 0 range 0 .. 31;
    Field2 at 4 range 0 ..  7;
  end record;

  -- A record type with discriminant
  type Typ3 (D : Character := ' ') is record
    case D is
      when '0' .. '9' =>
        Fi : Integer;
      when 'a' .. 'z' | 'A' .. 'Z' =>
        Fr : Float;
      when others =>
        null;
    end case;
  end record;
  -- Yet another type
  type Typ4 is access procedure (A : in Integer);

  -- A private type
  type Typ5 is private;

  -- A type with comments
  type Typ6 is (
   Red,      -- The colors
   Orange,   -- Of a
   Green);   -- Traffic light

  -- A variable and a constant
  Var1 : Typ2;
  Const1 : constant Typ1 := "Test";
  Const2 : constant Typ5;

  -- A use clause within package spec
  use Ada.Exceptions;

  -- A procedure
  procedure Proc1 (A : in Integer);

  -- A function
  function Func1 (A, B : Integer;
                  C : Character) return String;
  -- Another function
  function Func2 return Integer;

  -- A package
  package Pack1 is
    -- A function
    function Func3 (A, B : Integer; C : Character) return String;
    -- A procedure
    procedure Proc2 (A : in Integer);
  end Pack1;
  -- Another package
  package Pack2 is
  end Pack2;

  -- A task
  task Task1 is
    -- Comment in a task
  end Task1;
  -- A task type
  task type Task2 is
    -- Entry in a task
    entry Entr1 (Character) (I : in Integer);
  end Task2;

  -- A protected object
  protected Prot1 is
    -- Procedure in protected
    procedure Proc3 (A : in Integer);
    -- Function in protected
    function Func4 (I : in Character) return Positive;
    -- Entry in protected
    entry Entr2 (Character) (I : in Integer);
  end Prot1;

  -- Another protected type
  protected type Prot2 is
  end Prot2;

  -- Three renaming declarations
  package Pack3 renames Pack2;
  procedure Proc4 (A : in Integer) renames Proc1;
  function Func3 return Integer renames Func2;

  -- Some OO features
  type Rw is limited interface;
  subtype Item is Integer;
  procedure Write(Obj: out Rw; X: in Item) is abstract;
  procedure Read(Obj: out Rw; X: out Item) is abstract;

  type Simple_Rw is new Rw with record
    V: Item;
  end record;
  overriding procedure Write(Obj: out Simple_Rw; X: in Item);
  overriding procedure Read(Obj: out Simple_Rw; X: out Item);

  type Sync_Rw is synchronized interface and Rw;
  protected type Prot_Rw is new Sync_Rw with
    overriding procedure Write(X: in Item);
    overriding procedure Read(X: out Item);
  private
    V: Item;
  end;

  protected type Multi_Prot_Rw is new Sync_Rw with
    overriding procedure Write(X: in Item);
    not overriding function Read return Item;
  private
     V: Item;
  end;
  overriding procedure Read(Obj: in Multi_Prot_Rw; X: out Item);


private

  type Typ5 is new Integer;
  Const2 : constant Typ5 := 21;
  function Func6 return Typ5;

end Test;

