with Ada.Strings.Unbounded;
with Dynamic_List;
with Entities;
package Lister is


  -- List content of Dir, possibly dots, matching criteria
  procedure List (Ent_List : in out Entities.Entity_List;
                  Dir : in String;
                  Dots : in Entities.Dots_Kind_List;
                  Only_Dirs : in Boolean;
                  Date1, Date2 : in Entities.Date_Spec_Rec);

  -- Add a file if it matches
  procedure List (Ent_List : in out Entities.Entity_List;
                  File : in String;
                  Date1, Date2 : in Entities.Date_Spec_Rec);

  -- List subdirs of Dir
  package Dir_Dyn_List_Mng is new Dynamic_List
             (Ada.Strings.Unbounded.Unbounded_String);
  package Dir_List_Mng renames Dir_Dyn_List_Mng.Dyn_List;
  subtype Dir_List is Dir_List_Mng.List_Type;

  procedure List_Dirs (Dir : in String;
                       List : out Dir_List);
  
end Lister;

