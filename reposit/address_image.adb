with Ada.Text_Io, System, Unchecked_Conversion;
function Address_Image (Addr : System.Address) return String is


  type Readable_Address is mod System.Memory_Size;
  package Addr_Io is new Ada.Text_Io.Modular_Io(Readable_Address);

  function To_Readable is new Unchecked_Conversion (
    System.Address, Readable_Address);

   S : String (1 .. Addr_Io.Default_Base + 4);

begin
  Addr_Io.Put (S, To_Readable(Addr), 16);
  for I in reverse S'Range loop
    if S(I) /= ' ' then
      return S(1 .. I);
    end if;
  end loop;
  return "";
end Address_Image;

