package Hash_Function is

  type Hash_Buffer is tagged private;

  -- Maximum value of hash result
  Max_Hash : constant := 16#FFFFFF#;
  type Hash_Range is new Integer range 0 .. Max_Hash;

  -- For immediate computation
  function Hash (Str : in String) return Hash_Range;

  -- For iterative compuations:
  -- Reset computing of hash value
  procedure Rst (Buffer : in out Hash_Buffer);

  -- Add Str to computed hash value
  procedure Add (Buffer : in out Hash_Buffer; Str : in String);

  -- Get currently computed hash value (which is not reset)
  function Get (Buffer : in Hash_Buffer) return Hash_Range;

private
  subtype Nat_Hash is Natural range 0 .. Natural(Max_Hash);
  type Hash_Buffer is tagged record
    Value : Nat_Hash := 0;
  end record;

end Hash_Function;

