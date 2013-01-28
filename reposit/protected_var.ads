-- Protected access to a variable of type T
with Mutex_Manager;
generic
  type T is private;
package Protected_Var is

  type Protected_T is tagged limited private;

  procedure Set (Var : in out Protected_T; Val : in T);

  function Get (Var : Protected_T) return T;

private

  type Protected_T is tagged limited record
    Mutex : Mutex_Manager.Mutex (Mutex_Manager.Write_Read, False);
    Val : T;
  end record;

end Protected_Var;

