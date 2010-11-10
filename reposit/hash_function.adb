with Bit_Ops;
package body Hash_Function is

  Nat_Max_Hash : constant Nat_Hash := Nat_Hash(Max_Hash);

  -- For immediate computation
  function Hash (Str : in String) return Hash_Range is
    Buffer : Hash_Buffer;
  begin
    Buffer.Add (Str);
    return Buffer.Get;
  end Hash;

  -- For iterative compuations:
  -- Reset computing of hash value
  procedure Rst (Buffer : in out Hash_Buffer) is
  begin
    Buffer.Value := 0;
  end Rst;

  -- Add Str to computed hash value
  procedure Add (Buffer : in out Hash_Buffer; Str : in String) is
    use Bit_Ops;
  begin
    for I in Str'Range loop
      Buffer.Value := (Shl (Buffer.Value, 5)
                       + Buffer.Value
                       + Natural (Character'Pos (Str(I)))) and Nat_Max_Hash;
    end loop;

  end Add;
  pragma Inline (Add);

  -- Get currently computed hash value (which is not reset)
  function Get (Buffer : in Hash_Buffer) return Hash_Range is
  begin
    return Hash_Range (Buffer.Value);
  end Get;

end Hash_Function;

