with Ada.Text_Io;

with Argument;

with Oper_Def, Oper_List_Mng, File_Mng, Unit_Format;

procedure Export is

  procedure Usage is
  begin
    Ada.Text_Io.Put_Line("Usage: " & Argument.Get_Program_Name
                        & " <account_file> <ascii_file>");
  end Usage;

  File : Ada.Text_Io.File_Type;
  Str  : Unit_Format.Oper_Str;
  Oper : Oper_Def.Oper_Rec;
  Oper_List : Oper_List_Mng.List_Type;
  Can_Write : Boolean;
  No : Positive;

begin

  if Argument.Get_Nbre_Arg /= 2 then
    Usage;
    return;
  end if;

  begin
    Ada.Text_Io.Open(File, Ada.Text_Io.Out_File,
                     Argument.Get_Parameter(Occurence => 2));
    Ada.Text_Io.Put_Line("Error. Ascii file " 
                         & Argument.Get_Parameter(Occurence => 2)
                         & " already exists");
    Usage;
    Ada.Text_Io.Close(File);
    return;
  exception
    when Ada.Text_Io.Name_Error =>
      null;
  end;

  begin
    File_Mng.Load(Argument.Get_Parameter(Occurence => 1), Oper_List, Can_Write);
  exception
    when File_Mng.F_Access_Error =>
      Ada.Text_Io.Put_Line("Error. Cannot read from file "
                         & Argument.Get_Parameter(Occurence => 1));
    when File_Mng.F_Io_Error =>
      Ada.Text_Io.Put_Line("Error. Reading from file "
                         & Argument.Get_Parameter(Occurence => 1));
  end;

  begin
    Ada.Text_Io.Create(File, Ada.Text_Io.Out_File,
                       Argument.Get_Parameter(Occurence => 2));
  exception
    when others =>
      Ada.Text_Io.Put_Line("Error. Cannot create ascii file "
                         & Argument.Get_Parameter(Occurence => 2));
      Usage;
      return;
  end;

  Oper_List_Mng.Rewind(Oper_List);

  No := 1;
  loop
    Oper_List_Mng.Read(Oper_List, Oper, Oper_List_Mng.Current);

    begin
      Str := Unit_Format.Image(Oper);
    exception
      when others =>
        Ada.Text_Io.Put_Line("Error. At record no " 
                           & Positive'Image(No));
        Ada.Text_Io.Close(File);
        return;
    end;
    

    begin
      Ada.Text_Io.Put_Line(File, Str);
    exception
      when others =>
        Ada.Text_Io.Put_Line("Error. Writing ascii file, record no "
                           & Positive'Image(No));
        Ada.Text_Io.Close(File);
        return;
    end;

    exit when not Oper_List_Mng.Check_Move (Oper_List);
    Oper_List_Mng.Move_To(Oper_List);

    No := No + 1;

  end loop;

  Ada.Text_Io.New_Line(File);
  Ada.Text_Io.Close(File);

end Export;

