package body Dir_Mng is

  Path_Separator : constant Character := '/';

  function Less_Than (El1, El2 : in File_Entry_Rec) return Boolean is
    use type Directory.File_Kind_List;
  begin
    -- Only one is dir
    return (if El1.Kind /= El2.Kind
            and then (El1.Kind = Directory.Dir
                      or else El2.Kind = Directory.Dir) then
              El1.Kind = Directory.Dir
            else
              El1.Name.Image < El2.Name.Image);
  end Less_Than;


  procedure List_Dir (List : in out File_List_Mng.List_Type;
                      Dir  : in String := "";
                      Template : in String := "") is
    Dir_Desc : Directory.Dir_Desc;
    File_Rec : File_Entry_Rec;
    File_Name : As.U.Asu_Us;
  begin

    Dir_Desc.Open ((if Dir = "" then "." else Dir));

    loop
       Dir_Desc.Next_Entry (File_Name);

      if Template = ""
      or else Directory.File_Match(File_Name.Image, Template) then
        File_Rec.Name := File_Name;
        begin
          File_Rec.Kind := Directory.File_Kind (
              (if Dir = "" then File_Name.Image
               else Dir & Path_Separator & File_Name.Image));
        exception
          when Directory.Name_Error | Directory.Access_Error =>
            File_Rec.Kind := Directory.Unknown;
        end;
        List.Insert (Item => File_Rec, Where => File_List_Mng.Next);
      end if;
    end loop;
  exception
    when Directory.End_Error =>
      Dir_Desc.Close;
  end List_Dir;

  procedure List_Dir (List : in out File_List_Mng.List_Type;
                      Dir  : in As.U.Asu_Us := As.U.Asu_Null;
                      Template : in As.U.Asu_Us := As.U.Asu_Null) is
  begin
    List_Dir (List, Dir.Image, Template.Image);
  end List_Dir;

end Dir_Mng;

