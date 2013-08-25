pragma Ada_2012;
package Access_Func is

  type Af is access function (I : in out Integer) return Integer;

end Access_Func;

