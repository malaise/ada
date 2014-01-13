pragma Ada_2012;
package Access_Func is

  -- "in out" or "out" parameter of a access function is rejected by the
  --  compiler
  type Af is access function (I : in out Integer) return Integer;

end Access_Func;

