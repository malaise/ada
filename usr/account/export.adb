with Ada.Wide_Text_Io;

with Basic_Proc, Argument;

with Oper_Def, Oper_Dyn_List_Mng, File_Mng, Unit_Format;

procedure Export is

  procedure Usage is
  begin
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
                        & " <account_file> <ascii_file>");
  end Usage;

  package Oper_List_Mng renames Oper_Dyn_List_Mng.Dyn_List;
  File : Ada.Wide_Text_Io.File_Type;
  Str  : Unit_Format.Oper_Str;
  Oper : Oper_Def.Oper_Rec;
  Oper_List : Oper_List_Mng.List_Type;
  Dummy : Boolean;
  No : Positive;

begin

  if Argument.Get_Nbre_Arg /= 2 then
    Usage;
    return;
  end if;

  -- Check that out (ascii) file does not exist
  begin
    Ada.Wide_Text_Io.Open (File, Ada.Wide_Text_Io.In_File,
                           Argument.Get_Parameter(Occurence => 2));
    Basic_Proc.Put_Line_Error ("Error. Ascii file "
                         & Argument.Get_Parameter(Occurence => 2)
                         & " already exists");
    Usage;
    Ada.Wide_Text_Io.Close(File);
    return;
  exception
    when Ada.Wide_Text_Io.Name_Error =>
      null;
  end;

  -- Load account and rewind
  begin
    Dummy := File_Mng.Load (Argument.Get_Parameter (Occurence => 1), Oper_List);
  exception
    when File_Mng.F_Access_Error =>
      Basic_Proc.Put_Line_Error ("Error. Cannot read from file "
                               & Argument.Get_Parameter (Occurence => 1));
    when File_Mng.F_Io_Error =>
      Basic_Proc.Put_Line_Error ("Error. Reading from file "
                               & Argument.Get_Parameter (Occurence => 1));
  end;
  Oper_List.Rewind;

  -- Create out file
  begin
    Ada.Wide_Text_Io.Create (File, Ada.Wide_Text_Io.Out_File,
                       Argument.Get_Parameter (Occurence => 2));
  exception
    when others =>
      Basic_Proc.Put_Line_Error ("Error. Cannot create ascii file "
                         & Argument.Get_Parameter (Occurence => 2));
      Usage;
      return;
  end;

  -- Save opers image one by one
  No := 1;
  loop
    Oper_List.Read (Oper, Oper_List_Mng.Current);

    begin
      Str := Unit_Format.Image (Oper);
    exception
      when others =>
        Basic_Proc.Put_Line_Error ("Error. At record no "
                           & Positive'Image (No));
        Ada.Wide_Text_Io.Close (File);
        return;
    end;


    begin
      Ada.Wide_Text_Io.Put_Line (File, Str);
    exception
      when others =>
        Basic_Proc.Put_Line_Error ("Error. Writing ascii file, record no "
                           & Positive'Image (No));
        Ada.Wide_Text_Io.Close (File);
        return;
    end;

    exit when not Oper_List.Check_Move;
    Oper_List.Move_To;

    No := No + 1;

  end loop;

  -- Done
  Ada.Wide_Text_Io.New_Line (File);
  Ada.Wide_Text_Io.Close (File);

end Export;

