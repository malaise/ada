with As.U.Utils, Trilean;
with Entities;
package Lister is

  -- List of strings
  package Str_List_Mng renames As.U.Utils.Asu_Dyn_List_Mng;

  -- Set selection criteria
  type Link_Criteria_List is (All_Links, Broken_Links, No_Link);
  type Rights_List is (Read, Write, Exec);
  type Access_Rights is array (Rights_List) of Trilean.Trilean;
  procedure Set_Criteria (Only_Dirs, Only_Files : in Boolean;
                          Rights : in Access_Rights;
                          Only_Links : in Link_Criteria_List;
                          Only_Others : in Boolean;
                          Follow_Links : in Boolean;
                          Date1, Date2 : in Entities.Date_Spec_Rec;
                          Utc : in Boolean);

  -- Add a file match or exclude template or regex
  -- File will match if no matching template or if its name (without path)
  --  matches one of the matching templates, and if it does not match any
  --  exclude template
  Invalid_Template : exception;
  procedure Add_Match (Template : in String; Regex : in Boolean);
  procedure Add_Exclude (Template : in String; Regex : in Boolean);

  -- List content of Dir, possibly dots, matching criteria
  procedure List (Ent_List : in out Entities.Entity_List;
                  Dir : in String;
                  Dots : in Entities.Dots_Kind_List;
                  Count_Dot : in Boolean);

  -- Add a file if it matches criteria
  procedure List (Ent_List : in out Entities.Entity_List;
                  File : in String);

  -- Add a dir match or exclude template or regex
  -- Dir will match if no matching template or if its name (without path)
  --  matches one of the matching templates, and if it does not match any
  --  discard or exclude template
  procedure Add_Dir_Match   (Template : in String; Regex : in Boolean);
  procedure Add_Dir_Exclude (Template : in String; Regex : in Boolean);
  procedure Add_Dir_Discard (Template : in String; Regex : in Boolean);

  -- Does a dir (without path) match
  -- Discard : matches a discard
  --       => discard this dir and its subdirs
  -- True: Does not match any discard nor exclusion and matches one inclusion
  --       => show this dir and process its subdirs
  -- False: Does not match any discard but matches an exclusion or
  --        does not match any inclusion
  --       => don't show this dir but process its subdirs
  Discard : constant Trilean.Trilean := Trilean.Other;
  function Dir_Matches (Dir : String) return Trilean.Trilean;

  -- List subdirs of Dir (without discard, exclude or matching criteria)
  -- Sort by alphabetic order
  package Dir_List_Mng renames Str_List_Mng;
  subtype Dir_List is Dir_List_Mng.List_Type;

  procedure List_Dirs (Dir : in String;
                       List : out Dir_List);

  -- Activate, then later on get the grand total
  subtype Size_Type is Long_Long_Integer range 0 .. Long_Long_Integer'Last;
  procedure Activate_Total;
  function Get_Number return Natural;
  function Get_Total return Size_Type;

end Lister;

