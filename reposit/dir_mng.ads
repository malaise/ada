with Text_Handler;
with Dynamic_List;
with Directory;
package Dir_Mng is

  subtype File_Str_Range is Positive range 1 .. Directory.Max_Dir_Name_Len;
  subtype File_Str is String (File_Str_Range);
  subtype File_Txt is Text_Handler.Text (Directory.Max_Dir_Name_Len);

  subtype File_Kind_List is Directory.File_Kind_List;

  type File_Entry_Rec is record
    Name : File_Str;
    Len  : File_Str_Range;
    Kind : File_Kind_List;
  end record;
  package File_List_Mng is new Dynamic_List (Element_Type => File_Entry_Rec);

  -- List files of a directory
  --  and append them at the end of the current list
  -- Current is set to the last item appended or not changed if no file found.
  -- If DIR is empty, then current dir is assumed
  -- May raise NAME_ERROR if DIR is not valid or not existing
  -- May raise ACCESS_ERROR if DIR cannot be read
  procedure List_Dir (List : in out File_List_Mng.List_Type;
                      Dir  : in String := "";
                      Template : in String := "");
  procedure List_Dir (List : in out File_List_Mng.List_Type;
                      Dir  : in File_Txt := Text_Handler.Empty_Text;
                      Template : in File_Txt := Text_Handler.Empty_Text);

  -- To sort files. Directories, then others, by name.
  function Less_Than (El1, El2 : in File_Entry_Rec) return Boolean;
  -- Sorts 
  procedure File_Sort is new File_List_Mng.Sort(Less_Than);

  Name_Error : exception renames Directory.Name_Error;
  Access_Error : exception renames Directory.Access_Error;

end Dir_Mng;

