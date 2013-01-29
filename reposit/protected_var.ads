-- Protected access to a variable of type T
with Mutex_Manager;
generic
  type T is private;
package Protected_Var is

  subtype Mutex_Kind is Mutex_Manager.Mutex_Kind;
  type Protected_T (Kind : Mutex_Kind) is tagged limited private;

  procedure Set (Var : in out Protected_T; Val : in T);

  function Get (Var : Protected_T) return T;

private

  type Protected_T (Kind : Mutex_Kind) is tagged limited record
    Mutex : Mutex_Manager.Mutex (Kind, False);
    Val : T;
  end record;

end Protected_Var;

