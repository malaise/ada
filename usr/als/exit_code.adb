with Basic_Proc;
package body Exit_Code is

  Code : Natural := Found;
  procedure Update (New_Code : in Natural) is
  begin
    if New_Code > Code then
      Code := New_Code;
      Basic_Proc.Set_Exit_Code (Code);
    end if;
  end Update;

end Exit_Code;

