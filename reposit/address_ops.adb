with Ada.Text_Io, Unchecked_Conversion;
package body Address_Ops is



  type Readable_Address is mod System.Memory_Size;
  package Addr_Io is new Ada.Text_Io.Modular_Io(Readable_Address);

  function To_Readable is new Unchecked_Conversion (
    System.Address, Readable_Address);


  function "-" (Addr1, Addr2 : System.Address) return Long_Long_Integer is
    R1 : constant Readable_Address := To_Readable (Addr1);
    R2 : constant Readable_Address := To_Readable (Addr2);
    use type System.Address;
  begin
    if Addr1 >= Addr2 then
      return Long_Long_Integer(R1 - R2);
    else
      return - Long_Long_Integer(R2 - R1);
    end if;
  end "-";


  function Image (Addr : System.Address) return String is
    S : String (1 .. Addr_Io.Default_Base + 4);
  begin
    Addr_Io.Put (S, To_Readable(Addr), 16);
    for I in reverse S'Range loop
      if S(I) /= ' ' then
        return S(1 .. I);
      end if;
    end loop;
    return "";
  end Image;

end Address_Ops;

