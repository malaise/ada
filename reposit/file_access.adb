with Sys_Calls, Bit_Ops;
-- Check that a user (User_Id, Group_Id) has access (read/write/exec)
-- to a file knowing its owner (File_User, File_Group) and Rights
procedure File_Access (User_Id, Group_Id : in Natural;
                       File_User, File_Group : in Natural;
                       File_Rights : in Natural;
                       Can_Read, Can_Write, Can_Exec : out Boolean) is
  use Bit_Ops;
begin
  if File_User = User_Id then
    Can_Read  := (File_Rights and Shl (1, 8)) /= 0;
    Can_Write := (File_Rights and Shl (1, 7)) /= 0;
    Can_Exec  := (File_Rights and Shl (1, 6)) /= 0;
  elsif File_Group = Group_Id then
    Can_Read  := (File_Rights and Shl (1, 5)) /= 0;
    Can_Write := (File_Rights and Shl (1, 4)) /= 0;
    Can_Exec  := (File_Rights and Shl (1, 3)) /= 0;
  else
    Can_Read  := (File_Rights and Shl (1, 2)) /= 0;
    Can_Write := (File_Rights and Shl (1, 1)) /= 0;
    Can_Exec  := (File_Rights and Shl (1, 0)) /= 0;
  end if;
end File_Access;

