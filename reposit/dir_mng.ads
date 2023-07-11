-- Make a list of entries of a directory
with As.U, Dynamic_List, Directory;
package Dir_Mng is

  -- List of entries in a directory
  subtype File_Kind_List is Directory.File_Kind_List;
  type File_Entry_Rec is record
    Name : As.U.Asu_Us;
    Kind : File_Kind_List;
  end record;
  package File_Dyn_List_Mng is
          new Dynamic_List (Element_Type => File_Entry_Rec);
  package File_List_Mng renames File_Dyn_List_Mng.Dyn_List;

  -- List files of a directory
  --  and append them at the end of the current list
  -- Current is set to the last item appended or not changed if no file found.
  -- If Dir is empty, then current dir is assumed
  -- May raise Name_Error if Dir is not valid or not existing
  -- May raise Access_Error if Dir cannot be read
  procedure List_Dir (List : in out File_List_Mng.List_Type;
                      Dir  : in String := "";
                      Template : in String := "");
  procedure List_Dir (List : in out File_List_Mng.List_Type;
                      Dir  : in As.U.Asu_Us := As.U.Asu_Null;
                      Template : in As.U.Asu_Us := As.U.Asu_Null);

  -- To sort files. Directories, then others, by name.
  function Less_Than (El1, El2 : in File_Entry_Rec) return Boolean;
  -- Sorts
  procedure File_Sort is new File_List_Mng.Sort(Less_Than);

  Name_Error : exception renames Directory.Name_Error;
  Access_Error : exception renames Directory.Access_Error;

end Dir_Mng;

