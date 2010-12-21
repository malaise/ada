package body Dir_Mng is

  Path_Separator : constant Character := '/';

  function Less_Than (El1, El2 : in File_Entry_Rec) return Boolean is
    use type Directory.File_Kind_List;
  begin
    -- Only one is dir
    if El1.Kind /= El2.Kind and then
     (El1.Kind = Directory.Dir or else El2.Kind = Directory.Dir) then
      return El1.Kind = Directory.Dir;
    else
      return El1.Name.Image < El2.Name.Image;
    end if;
  end Less_Than;


  procedure List_Dir (List : in out File_List_Mng.List_Type;
                      Dir  : in String := "";
                      Template : in String := "") is
    Dir_Desc : Directory.Dir_Desc;
    File_Rec : File_Entry_Rec;
    File_Name : Asu_Us;
  begin

    if Dir = "" then
      Dir_Desc := Directory.Open (".");
    else
      Dir_Desc := Directory.Open (Dir);
    end if;

    loop
      Directory.Next_Entry (Dir_Desc, File_Name);

      if Template = ""
      or else Directory.File_Match(File_Name.Image, Template) then
        File_Rec.Name := File_Name;
        begin
          if Dir = "" then
            File_Rec.Kind := Directory.File_Kind (File_Name.Image);
          else
            File_Rec.Kind := Directory.File_Kind (
             Dir & Path_Separator & File_Name.Image);
          end if;
        exception
          when Directory.Name_Error | Directory.Access_Error =>
            File_Rec.Kind := Directory.Unknown;
        end;
        List.Insert (Item => File_Rec, Where => File_List_Mng.Next);
      end if;
    end loop;
  exception
    when Directory.End_Error =>
      Directory.Close (Dir_Desc);
  end List_Dir;

  procedure List_Dir (List : in out File_List_Mng.List_Type;
                      Dir  : in Asu_Us := Asu_Null;
                      Template : in Asu_Us := Asu_Null) is
  begin
    List_Dir (List, Dir.Image, Template.Image);
  end List_Dir;

end Dir_Mng;

