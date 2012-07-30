with As.U.Utils;
with Entities;
package Lister is

  -- List of strings
  package Str_List_Mng renames As.U.Utils.Asu_Dyn_List_Mng;

  -- Set selection criteria
  type Link_Criteria_List is (All_Links, Broken_Links, No_Link);
  procedure Set_Criteria (Only_Dirs, Only_Files : in Boolean;
                          Only_Links : in Link_Criteria_List;
                          Only_Others : in Boolean;
                          Follow_Links : in Boolean;
                          Date1, Date2 : in Entities.Date_Spec_Rec;
                          Utc : in Boolean);

  -- Add a file match or exclude template or regex
  -- File will match if no matching template or if it matches one of the
  --  matching templates, and if it does not match any exclude template
  Invalid_Template : exception;
  procedure Add_Match (Template : in String; Regex : in Boolean);
  procedure Add_Exclude (Template : in String; Regex : in Boolean);

  -- List content of Dir, possibly dots, matching criteria
  procedure List (Ent_List : in out Entities.Entity_List;
                  Dir : in String;
                  Dots : in Entities.Dots_Kind_List);

  -- Add a file if it matches criteria
  procedure List (Ent_List : in out Entities.Entity_List;
                  File : in String);

  -- Add a dir match or exclude template or regex
  -- Dir will match if no matching template or if it matches one of the
  --  matching templates, and if it does not match any exclude template
  procedure Add_Dir_Match   (Template : in String; Regex : in Boolean);
  procedure Add_Dir_Exclude (Template : in String; Regex : in Boolean);

  -- Does a dir (full path) match
  function Dir_Matches (Dir : String) return Boolean;

  -- List subdirs of Dir (apply matching and exclude criteria)
  package Dir_List_Mng renames Str_List_Mng;
  subtype Dir_List is Dir_List_Mng.List_Type;

  procedure List_Dirs (Dir : in String;
                       List : out Dir_List);

  -- Activate, then later get grand total
  subtype Size_Type is Long_Long_Integer range 0 .. Long_Long_Integer'Last;
  procedure Activate_Total;
  function Get_Total return Size_Type;

end Lister;

