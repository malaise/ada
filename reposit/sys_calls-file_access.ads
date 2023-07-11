-- Manage rights of users to access files
package Sys_Calls.File_Access is

  -- Check that a user (User_Id, Group_Id) has access (read/write/exec)
  -- to a file, knowing its owner (File_User, File_Group) and Rights
  procedure Has_Access (User_Id, Group_Id : in Natural;
                        File_User, File_Group : in Natural;
                        File_Rights : in Natural;
                        Can_Read, Can_Write, Can_Exec : out Boolean);

  -- Check that a user (User_Id, Group_Id) has access (read/write/exec)
  -- to a file, given the File_Stat
  procedure Has_Access (User_Id, Group_Id : in Natural;
                        Stat : in File_Stat_Rec;
                        Can_Read, Can_Write, Can_Exec : out Boolean);

  -- Check that a user (User_Id, Group_Id) has access (read/write/exec)
  -- to a file, given the File_Name
  procedure Has_Access (User_Id, Group_Id : in Natural;
                        File_Name : in String;
                        Can_Read, Can_Write, Can_Exec : out Boolean);


  -- Check that current user has access (read/write/exec) to a file,
  -- knowing its owner (File_User, File_Group) and Rights
  procedure Has_Access (File_User, File_Group : in Natural;
                        File_Rights : in Natural;
                        Can_Read, Can_Write, Can_Exec : out Boolean);

  -- Check that current user has access (read/write/exec) to a file,
  -- given the File_Stat
  procedure Has_Access (Stat : in File_Stat_Rec;
                        Can_Read, Can_Write, Can_Exec : out Boolean);

  -- Check that current user has access (read/write/exec) to a file,
  -- given the File_Name
  procedure Has_Access (File_Name : in String;
                        Can_Read, Can_Write, Can_Exec : out Boolean);

end Sys_Calls.File_Access;

