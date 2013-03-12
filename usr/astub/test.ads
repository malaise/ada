-- This is a "complete" example
with Ada.Text_Io;
limited with Ada.Exceptions;
private with Ada.Directories;
limited private with Ada.Numerics;
use Ada.Text_Io;
generic
  -- Some generic formal arguments
  V : Integer := 5;
  type Typ is private;
  -- A function as generic formal argument
  with function ">" (A, B : Typ) return Boolean is <>;
package Test with Pure is

  -- A type
  type Typ1 is new String
    with Default_Component_Value => Ada.Characters.Latin_1.Space;
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
  type Typ5 is private with Type_Invariant => Check(Typ5);
  function Check (V: Typ5) return Boolean;

  -- A type with comments
  type Typ6 is (
   Red,      -- The colors
   Orange,   -- Of a
   Green);   -- Traffic light

  -- An enumerated
  type Typ7 is (Black, Red, White)
    with Default_Value => Black;

  -- A variable and a constant
  Var1 : Typ2;
  Const1 : constant Typ1 := "Test";
  Const2 : constant Typ5;

  -- A use clause within package spec
  use Ada.Text_Io;

  -- A procedure
  procedure Proc1 (A : in Integer);

  -- A function
  function Func1 (A, B : in out Integer;
                  C : Character) return String;
  -- Another function
  function Func2 return Integer;

  -- An expression function
  function Incr (A : Integer) return Integer is (A + 1);

  -- A procedure and function with access
  procedure Proca (A : in Integer;
                   P : access procedure (I : Integer);
                   F : access function (I : Integer) return Boolean;
                   B : in Integer);
  function Funca (A : Integer;
                  P : access procedure (I : Integer);
                  F : access function (I : Integer) return Boolean;
                  B : Integer) return Integer;

  -- A package
  package Pack1 is
    subtype Int is Integer
      with Dynamic_Predicate => Int mod 2 = 0;
    subtype Char is Character
      with Static_Predicate => Char in 'a' .. 'f' | 'a' .. 'F' | '0'.. '9';
    -- A function
    function Func3 (A, B : Int; C : Char) return String with Priority => 10;
    -- A procedure
    procedure Proc2 (A : in Integer) with Priority => 11;
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

  -- Package instanciation
  package Pack4 is new Ada.Text_Io.Integer_Io (Integer);

  -- Some OO features
  type Otyp6 is limited interface;
  subtype Otyp7 is Integer;
  procedure Write(Obj: in out Otyp6; X: in Otyp7) is abstract;
  procedure Read(Obj: in out Otyp6; X: out Otyp7) is abstract;

  type Otyp8 is new Otyp6 with record
    V: Otyp7;
  end record;
  overriding procedure Write(Obj: in out Otyp8; X: in Otyp7);
  overriding procedure Read(Obj: in out Otyp8; X: out Otyp7);

  type Otyp9 is synchronized interface and Otyp6;
  protected type Otyp10 is new Otyp9 with
    overriding procedure Write(X: in Otyp7);
    overriding procedure Read(X: out Otyp7);
  private
    V: Otyp7;
  end;

  protected type Otyp11 is new Otyp9 with
    overriding procedure Write(X: in Otyp7);
    not overriding function Read return Otyp7;
  private
     V: Otyp7;
  end Otyp11;
  overriding procedure Read(Obj: in Otyp11; X: out Otyp7);

  type Otyp12 is tagged;
  type Otyp12 is tagged null record;

private

  type Typ5 is new Integer;
  function Func6 return Typ5;
  Const2 : constant Typ5 := 21;

end Test;

