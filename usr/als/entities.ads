with Ada.Calendar, Ada.Strings.Unbounded;
with Directory, Sys_Calls, Dynamic_List;
package Entities is

  subtype Asu_Us is Ada.Strings.Unbounded.Unbounded_String;

  -- An entry in directory
  type Entity is record
    -- The name of the entity itself and its path
    Name : Asu_Us;
    Path : Asu_Us;
    -- The kind (file, dir...)
    Kind : Directory.File_Kind_List;
    -- The access rights
    Rights : Natural;
    -- The owner user and group
    User_Id, Group_Id : Natural;
    -- Last modification time
    Modif_Time : Ada.Calendar.Time;
    -- Size
    Size : Sys_Calls.Size_T;
    -- Symbolic link destination (if link)
    Link : Asu_Us;
    -- Symbolic link final target exists (if link)
    Link_Ok : Boolean;
  end record;

  -- List of entities
  package Entity_Dyn_List_Mng is new Dynamic_List (Entity);
  package Entity_List_Mng renames Entity_Dyn_List_Mng.Dyn_List;
  subtype Entity_List is Entity_List_Mng.List_Type;

  -- Date criteria
  type Date_Oper_List is (Equal, Less_Than, Less_Or_Equal,
                                 Greater_Than, Greater_Or_Equal, None);
  type Date_Spec_Rec is record
    Oper : Date_Oper_List := None;
    Date : Ada.Calendar.Time;
  end record;

  -- Kind of dots
  type Dots_Kind_List is (Basic, Basic_Dots, Basic_Dots_Roots);

end Entities;

