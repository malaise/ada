with Ada.Wide_Text_Io;

with Basic_Proc, Argument;

with Oper_Def, Oper_Dyn_List_Mng, File_Mng, Unit_Format;

procedure Import is

  procedure Usage is
  begin
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
                        & " <ascii_file> <account_file>");
  end Usage;

  package Oper_List_Mng renames Oper_Dyn_List_Mng.Dyn_List;
  File : Ada.Wide_Text_Io.File_Type;
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

  -- Out (account) file shall not exist
  begin
    Ada.Wide_Text_Io.Open (File, Ada.Wide_Text_Io.In_File,
                     Argument.Get_Parameter(Occurence => 2));
    Ada.Wide_Text_Io.Close(File);
    raise Program_Error;
  exception
    when Ada.Wide_Text_Io.Name_Error =>
      null;
    when others =>
      Basic_Proc.Put_Line_Error ("Error. Account file "
                           & Argument.Get_Parameter(Occurence => 2)
                           & " already exists");
      Usage;
      return;
  end;

  -- Open In (Ascii) file
  begin
    Ada.Wide_Text_Io.Open(File, Ada.Wide_Text_Io.In_File,
                     Argument.Get_Parameter(Occurence => 1));
  exception
    when Ada.Wide_Text_Io.Name_Error =>
      Basic_Proc.Put_Line_Error ("Error. Cannot open ascii file "
                         & Argument.Get_Parameter(Occurence => 1));
      Usage;
      return;
  end;

  -- Read Ascii file
  No := 1;
  loop
    begin
      Ada.Wide_Text_Io.Get_Line(File, Str, Last);
      if Last = 0 then
        No := No - 1;
        exit;
      end if;
      if Last /= Str'Last then
        raise Constraint_Error;
      end if;
    exception
      when others =>
        Basic_Proc.Put_Line_Error ("Error. In ascii file, record no "
                           & Positive'Image(No));
        Ada.Wide_Text_Io.Close(File);
        return;
    end;

    -- Parse Ascii line
    begin
      Oper := Unit_Format.Value(Str);
    exception
      when others =>
        Basic_Proc.Put_Line_Error ("Error. At record no "
                           & Positive'Image(No));
        Ada.Wide_Text_Io.Close(File);
        return;
    end;

    -- Append oper
    Oper_List.Insert(Oper);

    -- Read CR, check there was no extra char
    begin
      Ada.Wide_Text_Io.Get_Line(File, Str, Last);
      if Last /= 0 then
        raise Constraint_Error;
      end if;
    exception
      when others =>
        Basic_Proc.Put_Line_Error ("Error. In ascii file, record no "
                           & Positive'Image(No));
        Ada.Wide_Text_Io.Close(File);
        return;
    end;

    No := No + 1;

  end loop;

  -- Read CR, check there was no extra char
  begin
    Ada.Wide_Text_Io.Get_Line(File, Str, Last);
    raise Constraint_Error;
  exception
    when Ada.Wide_Text_Io.End_Error =>
      null;
    when others =>
      Basic_Proc.Put_Line_Error ("Error. In ascii file, after last record");
      Ada.Wide_Text_Io.Close(File);
      return;
  end;

  -- Done, check there was data
  Ada.Wide_Text_Io.Close(File);
  if No = 0 then
    Basic_Proc.Put_Line_Error ("Error. Empty file "
                         & Argument.Get_Parameter(Occurence => 1));
    return;
  end if;

  -- Save account
  begin
    File_Mng.Save(Argument.Get_Parameter(Occurence => 2), Oper_List);
  exception
    when File_Mng.F_Access_Error =>
      Basic_Proc.Put_Line_Error ("Error. Cannot write to file "
                         & Argument.Get_Parameter(Occurence => 2));
    when File_Mng.F_Io_Error =>
      Basic_Proc.Put_Line_Error ("Error. Writing to file "
                         & Argument.Get_Parameter(Occurence => 2));
  end;

end Import;

