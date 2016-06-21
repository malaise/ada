with Ada.Text_Io, Ada.Unchecked_Conversion;
package body Address_Ops is

  type Readable_Address is mod System.Memory_Size;
  package Addr_Io is new Ada.Text_Io.Modular_Io(Readable_Address);

  function To_Readable is new Ada.Unchecked_Conversion (
    System.Address, Readable_Address);
  function To_Address is new Ada.Unchecked_Conversion (
    Readable_Address, System.Address);

  function "+" (Addr : System.Address; Offset : Long_Longs.Llu_Natural)
               return System.Address is
    R : constant Readable_Address := To_Readable (Addr);
  begin
    return To_Address (R + Readable_Address(Offset));
  end "+";
  function "-" (Addr : System.Address; Offset : Long_Longs.Llu_Natural)
               return System.Address is
    R : constant Readable_Address := To_Readable (Addr);
  begin
    return To_Address (R + Readable_Address(Offset));
  end "-";

  function "-" (Addr1, Addr2 : System.Address) return Long_Longs.Llu_Natural is
    R1 : constant Readable_Address := To_Readable (Addr1);
    R2 : constant Readable_Address := To_Readable (Addr2);
    use type System.Address;
  begin
    if Addr1 < Addr2 then
      raise Constraint_Error;
    end if;
    return Long_Longs.Llu_Natural(R1 - R2);
  end "-";


  function Image (Addr : System.Address) return String is
    -- "16#<Image>#    "
    S : String (1 .. Readable_Address'Width + 4);
    Start : Positive;
    Stop : Natural;
  begin
    Addr_Io.Put (S, To_Readable(Addr), 16);
    -- Strip heading spaces
    Start := 1;
    for I in S'Range loop
      if S(I) /= ' ' then
        Start := I;
        exit;
      end if;
    end loop;
    -- Strip trailing spaces
    Stop := 0;
    for I in reverse S'Range loop
      if S(I) /= ' ' then
        Stop := I;
        exit;
      end if;
    end loop;
    return S(Start .. Stop);
  end Image;

end Address_Ops;

