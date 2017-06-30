-- Protected access to a variable of type T
with Mutexes;
generic
  type T is private;
package Protected_Var is

  -- A variable of type T protected by a mutex
  subtype Mutex_Kind is Mutexes.Mutex_Kind;
  type Protected_T (Kind : Mutex_Kind := Mutexes.Simple)
                   is tagged limited private;

  -- Set Val to Var, protect the operation by a mutex (infinite wait)
  procedure Set (Var : in out Protected_T; Val : in T);

  -- Get value of Var, protect the operation by a mutex (infinite wait)
  function Get (Var : in out Protected_T) return T;

private

  type Protected_T (Kind : Mutex_Kind := Mutexes.Simple)
                   is tagged limited record
    Mutex : Mutexes.Mutex (Kind, False);
    Val : T;
  end record;

end Protected_Var;

