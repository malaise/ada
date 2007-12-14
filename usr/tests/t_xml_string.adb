with Ada.Text_Io, Ada.Strings.Unbounded, Ada.Exceptions;
with Argument, Xml_Parser, Normal, Basic_Proc, Sys_Calls, Text_Line,
     String_Mng.Regex, Directory, Dir_Mng, Upper_Str, Rnd;
procedure T_Xml_String is
  Ctx : Xml_Parser.Ctx_Type;
  Prologue, Root : Xml_Parser.Element_Type;
  package Asu renames Ada.Strings.Unbounded;
  subtype Asu_Us is Asu.Unbounded_String;

  Data_Dir : constant String := "data";

  Dtds : array (1 .. 3) of Xml_Parser.Dtd_Type;
  subtype Dtd_Index_Range is Positive range Dtds'Range;
  Dtd_Index : Dtd_Index_Range;
  Parse_Ok : Boolean;

  If_Name_Key : constant String := "Interface_Name";
  If_Name_Val : constant String := "Try_Interface";
  If_Name_Ok : Boolean := False;
  If_Vers_Key : constant String := "Interface_Version";
  If_Vers_Crit : constant String := "([0-9]+)\.([0-9]+)";
  If_Vers_Ok : Boolean := False;
  Interface_Error : exception;

  File_List : Dir_Mng.File_List_Mng.List_Type;
  File_Entry : Dir_Mng.File_Entry_Rec;
  use type Directory.File_Kind_List;

  Separator : constant String :=
   "  ---------------------------------------------------------------------------";


  procedure Usage is
  begin
    Basic_Proc.Put_Line_Error ("Usage: "
        & Argument.Get_Program_Name & " <xml_file>");
  end Usage;

  -- Read a dtd or a xml file
  function Read_File  (Name : String) return String is
    Fd : Sys_Calls.File_Desc;
    File : Text_Line.File_Type;
    Str, Res : Asu_Us;
    use type Asu_Us;
  begin
    Fd := Sys_Calls.Open (Name, Sys_Calls.In_File);
    Text_Line.Open (File, Text_Line.In_File, Fd);
    loop
      Str := File.Get;
      exit when Asu.Length (Str) = 0;
      Res := Res & Str;
    end loop;
    Text_Line.Close (File);
    Sys_Calls.Close (Fd);
    return Asu.To_String (Res);
  exception
    when Sys_Calls.Name_Error =>
      Basic_Proc.Put_Line_Error ("Cannot open file " & Name);
      raise;
  end Read_File;

  ---------------------
  -- Put the Xml way --
  ---------------------
  procedure Put_Attributes (Elt : in Xml_Parser.Element_Type;
                            Level : in Natural;
                            Offset : in Positive) is
    Attrs : Xml_Parser.Attributes_Array := Ctx.Get_Attributes (Elt);
    Indent : constant String (1 .. 2 * Level + Offset) := (others => ' ');
    use type Asu_Us;
  begin
    for I in Attrs'Range loop
      if I /= 1 then
        -- Indent
        Ada.Text_Io.Put (Indent);
      end if;
      Ada.Text_Io.Put (" " & Asu.To_String (Attrs(I).Name
                     & "=""" & Attrs(I).Value) & """");
      if I /= Attrs'Last then
        Ada.Text_Io.New_Line;
      end if;
    end loop;
  end Put_Attributes;

  Prologue_Level : constant := -1;
  Prologue_Instruction_Level : constant := -2;
  procedure Put_Element (Elt : in Xml_Parser.Element_Type;
                         Level : in Integer) is
    Name : constant String := Asu.To_String(Ctx.Get_Name (Elt));
    Children : Xml_Parser.Nodes_Array := Ctx.Get_Children (Elt);
    Indent : constant String (1 .. 2 * Level) := (others => ' ');
    Prev_Is_Text : Boolean;
    use type Xml_Parser.Node_Kind_List;
  begin
    if Level = Prologue_Level then
      if Name /= "" then
        -- A prologue
        -- Put the xml directive with attributes
        Ada.Text_Io.Put ("<?" & Name);
        Put_Attributes (Elt, 0, 2 + Name'Length);
        Ada.Text_Io.Put_Line ("?>");
        for I in Children'Range loop
          if Children(I).Kind = Xml_Parser.Element then
            -- Put PIs
            Put_Element (Children(I), Prologue_Instruction_Level);
          end if;
        end loop;
      end if;
      return;
    elsif Level = Prologue_Instruction_Level then
      -- A PI of the prologue: 0 child or 1 text child
      -- Put PI directive
      Ada.Text_Io.Put ("<?" & Name);
      if Children'Length = 1 then
        Ada.Text_Io.Put (" " & Ctx.Get_Text (Children(1)));
      elsif Children'Length /= 0 then
        Basic_Proc.Put_Line_Error ("Error. Prologue processing instruction "
            & Name & " has several text children.");
        raise Program_Error;
      end if;
      Ada.Text_Io.Put_Line ("?>");
      if Name = If_Name_Key then
        if Children'Length = 1
        and then Ctx.Get_Text (Children(1)) = If_Name_Val then
          If_Name_Ok := True;
        else
          Basic_Proc.Put_Line_Error ("Error. Invalid interface name");
          raise Interface_Error;
        end if;
      elsif Name = If_Vers_Key then
        if Children'Length = 1 then
          declare
            Str : constant String := Ctx.Get_Text (Children(1));
            Strs : constant String_Mng.Regex.String_Slice
                 := String_Mng.Regex.Split (Str, If_Vers_Crit, 2);
          begin
            if Strs'Length /= 2 then
              raise Interface_Error;
            end if;
            Dtd_Index := Natural'Value (Asu.To_String (Strs (1)));
          exception
            when others =>
              Basic_Proc.Put_Line_Error ("Error. Invalid interface version");
              raise Interface_Error;
          end;
          If_Vers_Ok := True;
        else
          Basic_Proc.Put_Line_Error ("Error. Invalid interface version");
          raise Interface_Error;
        end if;
      end if;
      return;
    else
      -- Put element, attributes and children recursively
      Ada.Text_Io.Put (Indent);
      Ada.Text_Io.Put ("<" & Name);
      Put_Attributes (Elt, Level, 1 + Name'Length);
      if Children'Length = 0 then
        -- No child, terminate tag now
        Ada.Text_Io.Put ("/>");
      else
        Ada.Text_Io.Put (">");
        Prev_Is_Text := False;
        for I in Children'Range loop
          if Children(I).Kind = Xml_Parser.Element then
            -- Recursive dump child
            if I = 1 or else not Prev_Is_Text then
              -- Father did not New_Line because of possible text
              --  or prev was not text and did not New_Line because
              --  of possible text
              Ada.Text_Io.New_Line;
              Put_Element (Children(I), Level + 1);
            elsif I = 1 then
              -- First Child
              Put_Element (Children(I), Level + 1);
            else
              -- Child element following text
              --  we bet that it has no child itself, so no New_Line nor Indent
              Put_Element (Children(I), 0);
            end if;
            if I = Children'Last then
              Ada.Text_Io.New_Line;
            end if;
            Prev_Is_Text := False;
          else
            -- Specific put text
            Ada.Text_Io.Put (Ctx.Get_Text (Children(I)));
            Prev_Is_Text := True;
          end if;
        end loop;
        -- Terminate tag after children
        if not Prev_Is_Text then
          Ada.Text_Io.Put (Indent);
        end if;
        Ada.Text_Io.Put ("</" & Name & ">");
      end if;
    end if;
  end Put_Element;

  procedure Do_One (Name : in String) is
  begin
    -- Parse string of file provided as arg
    Ada.Text_Io.Put_Line ("Parsing prologue of string of file " & Name);
    Ctx.Parse_Prologue (Read_File (Name), Parse_Ok);
    if not Parse_Ok then
      Basic_Proc.Put_Line_Error (Xml_Parser.Get_Parse_Error_Message (Ctx));
      Basic_Proc.Set_Error_Exit_Code;
      Xml_Parser.Clean (Ctx);
      return;
    end if;
    Prologue := Ctx.Get_Prologue;
    Ada.Text_Io.Put_Line ("Got Prologue:");
    Put_Element (Prologue, Prologue_Level);
    Ada.Text_Io.New_Line;

    -- Identify Dtd
    if not If_Name_Ok or else not If_Vers_Ok then
      Basic_Proc.Put_Line_Error (
           "Error. Missing or incomplete interface definition");
      raise Interface_Error;
    end if;
    Ada.Text_Io.Put_Line ("Selected Dtd:" & Dtd_Index'Img);
    -- Parse remaining with this dtd
    Ada.Text_Io.Put_Line ("Parsing remaining of string");
    Ctx.Parse_Elements (Dtds (Dtd_Index), Parse_Ok);
    if not Parse_Ok then
      Basic_Proc.Put_Line_Error (Xml_Parser.Get_Parse_Error_Message (Ctx));
      Basic_Proc.Set_Error_Exit_Code;
      Xml_Parser.Clean (Ctx);
    else
      Ada.Text_Io.Put_Line ("Got Elements:");
      Root := Ctx.Get_Root_Element;
      Put_Element (Root, 0);
      Ada.Text_Io.New_Line;
    end if;
    Ada.Text_Io.Put_Line (Separator);

    -- Done
    Ctx.Clean;
  end Do_One;

begin
  if Argument.Get_Nbre_Arg = 0 then
    Usage;
    Basic_Proc.Set_Error_Exit_Code;
    return;
  elsif Argument.Get_Parameter = "-h"
  or else Argument.Get_Parameter = "--help" then
    Usage;
    Basic_Proc.Set_Error_Exit_Code;
    return;
  end if;

  -- Parse the Dtds
  for I in Dtds'Range loop
    declare
      File_Name : constant String
                := Data_Dir & "/dtd_" & Normal (I, 1) & ".dtd";
    begin
      if I mod 2 = 1 then
        Ada.Text_Io.Put_Line ("Parsing file " & File_Name);
        Xml_Parser.Parse_Dtd_File (File_Name, Dtds(I));
      else
        Ada.Text_Io.Put_Line ("Parsing string of file " & File_Name);
        Xml_Parser.Parse_Dtd_String (Read_File (File_Name), Dtds(I));
      end if;
    end;
  end loop;
  Ada.Text_Io.Put_Line (Separator);

  if Argument.Get_Nbre_Arg = 1
  and then Upper_Str (Argument.Get_Parameter) = "RND" then
    Dir_Mng.List_Dir (File_List, Data_Dir, "xml_*.xml");
    loop
      -- Select a random entry
      Dir_Mng.File_List_Mng.Move_To (
        File_List,
        Where => Dir_Mng.File_List_Mng.Next,
        Number => Rnd.Int_Random (0,
                   Dir_Mng.File_List_Mng.List_Length (File_List) - 1),
        From_Current => False);
      Dir_Mng.File_List_Mng.Read (File_List, File_Entry,
                                  Dir_Mng.File_List_Mng.Current);
      if File_Entry.Kind = Directory.File then
        Do_One (Data_Dir & "/" & File_Entry.Name (1 .. File_Entry.Len));
      end if;
    end loop;
  else
    -- Do each argument
    for I in 1 .. Argument.Get_Nbre_Arg loop
      Do_One (Argument.Get_Parameter (Occurence => I));
    end loop;
  end if;

exception
  when Error:others =>
    Basic_Proc.Put_Line_Error ("Exception "
        & Ada.Exceptions.Exception_Name (Error)
        & " raised.");
    Usage;
    Basic_Proc.Set_Error_Exit_Code;
end T_Xml_String;

