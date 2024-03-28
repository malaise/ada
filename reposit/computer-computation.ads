with Arbitrary;
package Computer.Computation is
  type Memory_Type is new Computer.Memory_Type with null record;

  -- Computation of expression
  -- First, all the variables are resolved and must lead to a valid
  --  operator, operand or a parenthesis
  -- Then the operations are computed in the proper order
  -- May raise Invalid_Expression (space, parentheses, operations, values...)
  function Compute (Memory : in out Memory_Type;
                    Expression : in String) return Arbitrary.Number;

end Computer.Computation;

