with Bit_Ops;
package body Sys_Calls.File_Access is

  -- Check that a user has access (read/write/exec) to a file
  procedure Has_Access (User_Id, Group_Id : in Natural;
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
  end Has_Access;

  procedure Has_Access (User_Id, Group_Id : in Natural;
                        Stat : in File_Stat_Rec;
                        Can_Read, Can_Write, Can_Exec : out Boolean) is
  begin
    Has_Access (User_Id, Group_Id, Stat.User_Id, Stat.Group_Id, Stat.Rights,
                Can_Read, Can_Write, Can_Exec);
  end Has_Access;

  procedure Has_Access (User_Id, Group_Id : in Natural;
                        File_Name : in String;
                        Can_Read, Can_Write, Can_Exec : out Boolean) is
    Stat : constant File_Stat_Rec := File_Stat (File_Name);
  begin
    Has_Access (User_Id, Group_Id, Stat, Can_Read, Can_Write, Can_Exec);
  end Has_Access;


  -- Check that current user has access (read/write/exec) to a file
  procedure Has_Access (File_User, File_Group : in Natural;
                        File_Rights : in Natural;
                        Can_Read, Can_Write, Can_Exec : out Boolean) is
  begin
    Has_Access (Get_Effective_User_Id, Get_Effective_Group_Id,
                File_User, File_Group, File_Rights,
                Can_Read, Can_Write, Can_Exec);
  end Has_Access;

  procedure Has_Access (Stat : in File_Stat_Rec;
                        Can_Read, Can_Write, Can_Exec : out Boolean) is
  begin
    Has_Access (Stat.User_Id, Stat.Group_Id, Stat.Rights,
                Can_Read, Can_Write, Can_Exec);
  end Has_Access;

  procedure Has_Access (File_Name : in String;
                        Can_Read, Can_Write, Can_Exec : out Boolean) is
    Stat : constant File_Stat_Rec := File_Stat (File_Name);
  begin
    Has_Access (Stat, Can_Read, Can_Write, Can_Exec);
  end Has_Access;

end Sys_Calls.File_Access;

