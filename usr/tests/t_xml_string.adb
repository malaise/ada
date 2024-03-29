-- Parse the 3 dtds data/xml*.xml, either as file or as string, then
-- With Arg Rnd, loop randomly on each of data/xml*.xml, otherwise process
--  files provided as arg
-- Read file into a string
-- Parse the prologue, get the Dtd num from PI Interface_Version (major number),
--  then parse the elements with this Dtd
with Ada.Exceptions;
with As.U.Utils, Argument, Xml_Parser, Normal, Basic_Proc, Sys_Calls,
     Text_Line, Str_Util.Regex, Directory, Dir_Mng, Upper_Str, Rnd;
procedure T_Xml_String is
  Ctx : Xml_Parser.Ctx_Type;
  Prologue, Root : Xml_Parser.Element_Type;

  Data_Dir : constant String := "data";

  Dtd : Xml_Parser.Dtd_Type;
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
  Dtd_Error : exception;
  Interface_Error : exception;
  Error_Msg : As.U.Asu_Us;

  File_List : Dir_Mng.File_List_Mng.List_Type;
  File_Entry : Dir_Mng.File_Entry_Rec;
  use type Directory.File_Kind_List;

  Separator : constant String :=
   "  ---------------------------------------------------------------------------";


  procedure Usage is
  begin
    Basic_Proc.Put_Line_Error ("Usage: "
        & Argument.Get_Program_Name & " <xml_file> | rnd");
  end Usage;

  -- Read a dtd or a xml file
  function Read_File  (Name : String) return String is
    Fd : Sys_Calls.File_Desc;
    File : Text_Line.File_Type;
    Str, Res : As.U.Asu_Us;
    use type As.U.Asu_Us;
  begin
    Fd := Sys_Calls.Open (Name, Sys_Calls.In_File);
    Text_Line.Open (File, Text_Line.In_File, Fd);
    loop
      Str := File.Get;
      exit when Str.Length = 0;
      Res := Res & Str;
    end loop;
    Text_Line.Close (File);
    Sys_Calls.Close (Fd);
    return Res.Image;
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
    Attrs : constant Xml_Parser.Attributes_Array := Ctx.Get_Attributes (Elt);
    Indent : constant String (1 .. 2 * Level + Offset) := (others => ' ');
  begin
    for I in Attrs'Range loop
      if I /= 1 then
        -- Indent
        Basic_Proc.Put_Output (Indent);
      end if;
      Basic_Proc.Put_Output (" " & Attrs(I).Name.Image
                     & "=""" & Attrs(I).Value.Image & """");
      if I /= Attrs'Last then
        Basic_Proc.New_Line_Output;
      end if;
    end loop;
  end Put_Attributes;


  procedure Put_Pi (Pi : in Xml_Parser.Pi_Type) is
    Target : constant String := Ctx.Get_Target (Pi).Image;
    Text : constant String := Ctx.Get_Pi (Pi).Image;
  begin
    Basic_Proc.Put_Output ("<?" & Target);
    if Text /= ""  then
      Basic_Proc.Put_Output (" " & Text);
    end if;
    Basic_Proc.Put_Line_Output ("?>");
    if Target = If_Name_Key then
      if Text = If_Name_Val then
        If_Name_Ok := True;
      else
        Basic_Proc.Put_Line_Error ("Error. Invalid interface name "
                                 & Text);
        raise Interface_Error;
      end if;
    elsif Target = If_Vers_Key then
      declare
        Strs : constant As.U.Utils.Asu_Array
             := Str_Util.Regex.Split (Text, If_Vers_Crit, 2);
      begin
        if Strs'Length /= 2 then
          raise Interface_Error;
        end if;
        Dtd_Index := Natural'Value (Strs (1).Image);
      exception
        when others =>
          Basic_Proc.Put_Line_Error ("Error. Invalid interface version "
                                     & Text);
          raise Interface_Error;
      end;
      If_Vers_Ok := True;
    end if;
  end Put_Pi;

  Prologue_Level : constant := -1;
  procedure Put_Element (Elt : in Xml_Parser.Element_Type;
                         Level : in Integer) is
    Name : constant String := Ctx.Get_Name (Elt).Image;
    Children : constant Xml_Parser.Nodes_Array := Ctx.Get_Children (Elt);
    Indent : constant String (1 .. 2 * Level) := (others => ' ');
    Prev_Is_Text : Boolean;
    use type Xml_Parser.Node_Kind_List;
  begin
    if Level = Prologue_Level then
      if Name /= "" then
        -- A prologue
        -- Put the xml directive with attributes
        Basic_Proc.Put_Output ("<?" & Name);
        Put_Attributes (Elt, 0, 2 + Name'Length);
        Basic_Proc.Put_Line_Output ("?>");
        for Child of Children loop
          if Child.Kind = Xml_Parser.Pi then
            -- Put PIs
            Put_Pi (Child);
          end if;
        end loop;
      end if;
      return;
    else
      -- Put element, attributes and children recursively
      Basic_Proc.Put_Output (Indent);
      Basic_Proc.Put_Output ("<" & Name);
      Put_Attributes (Elt, Level, 1 + Name'Length);
      if Children'Length = 0 then
        -- No child, terminate tag now
        Basic_Proc.Put_Output ("/>");
      else
        Basic_Proc.Put_Output (">");
        Prev_Is_Text := False;
        for I in Children'Range loop
          if Children(I).Kind = Xml_Parser.Element then
            -- Recursive dump child
            if I = 1 or else not Prev_Is_Text then
              -- Father did not New_Line because of possible text
              --  or prev was not text and did not New_Line because
              --  of possible text
              Basic_Proc.New_Line_Output;
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
              Basic_Proc.New_Line_Output;
            end if;
            Prev_Is_Text := False;
          else
            -- Specific put text
            Basic_Proc.Put_Output (Ctx.Get_Text (Children(I)));
            Prev_Is_Text := True;
          end if;
        end loop;
        -- Terminate tag after children
        if not Prev_Is_Text then
          Basic_Proc.Put_Output (Indent);
        end if;
        Basic_Proc.Put_Output ("</" & Name & ">");
      end if;
    end if;
  end Put_Element;

  procedure Do_One (Name : in String) is
  begin
    -- Parse string of file provided as arg
    Basic_Proc.Put_Line_Output ("Parsing prologue of string of file " & Name);
    Xml_Parser.Clean_Dtd (Dtd);
    Ctx.Parse_Prologue (Read_File (Name), Dtd, Parse_Ok, Dtd_Path => Data_Dir);
    if not Parse_Ok then
      Basic_Proc.Put_Line_Error (Ctx.Get_Parse_Error_Message);
      Basic_Proc.Set_Error_Exit_Code;
      Ctx.Clean;
      return;
    end if;
    Prologue := Ctx.Get_Prologue;
    Basic_Proc.Put_Line_Output ("Got Prologue:");
    Put_Element (Prologue, Prologue_Level);
    Basic_Proc.New_Line_Output;

    -- Identify Dtd
    if not If_Name_Ok or else not If_Vers_Ok then
      Basic_Proc.Put_Line_Error (
           "Error. Missing or incomplete interface definition");
      raise Interface_Error;
    end if;
    Basic_Proc.Put_Line_Output ("Selected Dtd:" & Dtd_Index'Img);
    -- Parse remaining with this dtd
    Basic_Proc.Put_Line_Output ("Parsing remaining of string");
    Ctx.Parse_Elements (Dtds (Dtd_Index), Parse_Ok);
    if not Parse_Ok then
      Basic_Proc.Put_Line_Error (Ctx.Get_Parse_Error_Message);
      Basic_Proc.Set_Error_Exit_Code;
      Ctx.Clean;
    else
      Basic_Proc.Put_Line_Output ("Got Elements:");
      Root := Ctx.Get_Root_Element;
      Put_Element (Root, 0);
      Basic_Proc.New_Line_Output;
    end if;
    Basic_Proc.Put_Line_Output (Separator);

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
        Basic_Proc.Put_Line_Output ("Parsing file " & File_Name);
        Xml_Parser.Parse_Dtd_File (File_Name, null, Dtds(I), Error_Msg);
      else
        Basic_Proc.Put_Line_Output ("Parsing string of file " & File_Name);
        Xml_Parser.Parse_Dtd_String (Read_File (File_Name), null, Dtds(I),
                                     Error_Msg);
      end if;
      if not Error_Msg.Is_Null then
        Basic_Proc.Put_Line_Error (
             "Error. Invalid Dtd: " & Error_Msg.Image);
        raise Dtd_Error;
      end if;
    end;
  end loop;
  Basic_Proc.Put_Line_Output (Separator);

  if Argument.Get_Nbre_Arg = 1
  and then Upper_Str (Argument.Get_Parameter) = "RND" then
    Dir_Mng.List_Dir (File_List, Data_Dir, "xml_*.xml");
    Rnd.Gen.Randomize;
    loop
      -- Select a random entry
      File_List.Move_At (Rnd.Gen.Int_Random (1, File_List.List_Length));
      File_List.Read (File_Entry, Dir_Mng.File_List_Mng.Current);
      if File_Entry.Kind = Directory.File then
        Do_One (Data_Dir & "/" & File_Entry.Name.Image);
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

