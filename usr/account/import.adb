with Ada.Text_Io;

with Argument;

with Oper_Def, Oper_List_Mng, File_Mng, Unit_Format;

procedure Import is

  procedure Usage is
  begin
    Ada.Text_Io.Put_Line("Usage: " & Argument.Get_Program_Name
                        & " <ascii_file> <account_file>");
  end Usage;

  File : Ada.Text_Io.File_Type;
  Str  : Unit_Format.Oper_Str;
  Last : Natural;
  Oper : Oper_Def.Oper_Rec;
  Oper_List : Oper_List_Mng.List_Type;
  No : Natural;


begin

  if Argument.Get_Nbre_Arg /= 2 then
    Usage;
    return;
  end if;

  begin
    Ada.Text_Io.Open(File, Ada.Text_Io.Out_File,
                     Argument.Get_Parameter(Occurence => 2));
    Ada.Text_Io.Close(File);
    raise Program_Error;
  exception
    when Ada.Text_Io.Name_Error =>
      null;
    when others =>
      Ada.Text_Io.Put_Line("Error. Account file " 
                           & Argument.Get_Parameter(Occurence => 2)
                           & " already exists");
      Usage;
      return;
  end;

  begin
    Ada.Text_Io.Open(File, Ada.Text_Io.In_File,
                     Argument.Get_Parameter(Occurence => 1));
  exception
    when Ada.Text_Io.Name_Error =>
      Ada.Text_Io.Put_Line("Error. Cannot open ascii file "
                         & Argument.Get_Parameter(Occurence => 1));
      Usage;
      return;
  end;

  No := 1;
  loop
    begin
      Ada.Text_Io.Get_Line(File, Str, Last); 
      if Last = 0 then
        No := No - 1;
        exit;
      end if;
      if Last /= Str'Last then
        raise Constraint_Error;
      end if;
    exception
      when others =>
        Ada.Text_Io.Put_Line("Error. In ascii file, record no "
                           & Positive'Image(No));
        Ada.Text_Io.Close(File);
        return;
    end;

    begin
      Oper := Unit_Format.Value(Str);
    exception
      when others =>
        Ada.Text_Io.Put_Line("Error. At record no " 
                           & Positive'Image(No));
        Ada.Text_Io.Close(File);
        return;
    end;
    
    Oper_List_Mng.Insert(Oper_List, Oper);

    begin
      Ada.Text_Io.Get_Line(File, Str, Last);
      if Last /= 0 then
        raise Constraint_Error;
      end if;
    exception
      when others =>
        Ada.Text_Io.Put_Line("Error. In ascii file, record no "
                           & Positive'Image(No));
        Ada.Text_Io.Close(File);
        return;
    end;

    No := No + 1;

  end loop;

  begin
    Ada.Text_Io.Get_Line(File, Str, Last);
    raise Constraint_Error;
  exception
    when Ada.Text_Io.End_Error =>
      null;
    when others =>
      Ada.Text_Io.Put_Line("Error. In ascii file, after last record");
      Ada.Text_Io.Close(File);
      return;
  end;


  Ada.Text_Io.Close(File);

  if No = 0 then
    Ada.Text_Io.Put_Line("Error. Empty file "
                         & Argument.Get_Parameter(Occurence => 1));
    return;
  end if;


  begin
    File_Mng.Save(Argument.Get_Parameter(Occurence => 2), Oper_List);
  exception
    when File_Mng.F_Access_Error =>
      Ada.Text_Io.Put_Line("Error. Cannot write to file "
                         & Argument.Get_Parameter(Occurence => 2));
    when File_Mng.F_Io_Error =>
      Ada.Text_Io.Put_Line("Error. Writing to file "
                         & Argument.Get_Parameter(Occurence => 2));
  end;

end Import;

