package body Dir_Mng is

  Dir_Internal_Error : exception;

  Path_Separator : constant Character := '/';

  function Less_Than (El1, El2 : in File_Entry_Rec) return Boolean is
    use Directory;
  begin
    -- Only one is dir
    if El1.Kind /= El2.Kind and then
     (El1.Kind = Directory.Dir or else El2.Kind = Directory.Dir) then
      return El1.Kind = Directory.Dir;
    else
      return El1.Name (1 .. El1.Len) < El2.Name (1 .. El2.Len);
    end if;
  end Less_Than;


  procedure List_Dir (List : in out File_List_Mng.List_Type;
                      Dir  : in String := "";
                      Template : in String := "") is
    Dir_Desc : Directory.Dir_Desc;
    File_Rec : File_Entry_Rec;
    File_Name : File_Txt;
    File_Rights : Natural;
    File_Mtime : Directory.Time_T;
  begin

    if Dir = "" then
      Dir_Desc := Directory.Open (".");
    else
      Dir_Desc := Directory.Open (Dir);
    end if;

    loop
      Text_Handler.Set (File_Name, Directory.Next_Entry (Dir_Desc));

      if Template = ""
      or else Directory.File_Match(Text_Handler.Value (File_Name),
                                   Template) then
        File_Rec.Len := Text_Handler.Length (File_Name);
        File_Rec.Name (1 .. File_Rec.Len) := Text_Handler.Value (File_Name);
        begin
          if Dir = "" then
            Directory.File_Stat (
             Text_Handler.Value (File_Name),
             File_Rec.Kind, File_Rights, File_Mtime);
          else
            Directory.File_Stat (
             Dir & Path_Separator & Text_Handler.Value (File_Name),
             File_Rec.Kind, File_Rights, File_Mtime);
          end if;
        exception
          when Directory.Name_Error | Directory.Access_Error =>
            File_Rec.Kind := Directory.Unknown;
        end;
        File_List_Mng.Insert (List => List,
                              Item => File_Rec,
                              Where => File_List_Mng.Next);
      end if;
    end loop;
  exception
    when Directory.End_Error =>
      Directory.Close (Dir_Desc);
  end List_Dir;

  procedure List_Dir (List : in out File_List_Mng.List_Type;
                      Dir  : in File_Txt := Text_Handler.Empty_Text;
                      Template : in File_Txt := Text_Handler.Empty_Text) is
  begin
    List_Dir (List, Text_Handler.Value(Dir), Text_Handler.Value(Template));
  end List_Dir;


end Dir_Mng;

