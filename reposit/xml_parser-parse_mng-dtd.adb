separate (Xml_Parser.Parse_Mng)
package body Dtd is

  -- Is there a dtd set, otherwise check is always ok
  Dtd_Set : Boolean;

  -- Init (clear) Dtd data
  procedure Init is
  begin
    Dtd_Set := False;
    -- @@@ Reset lists
  end Init;

  -- Parse an instruction:
  -- Check xml version, append any other instruction to prologue
  procedure Parse_Instruction is
  begin
    -- @@@
    Util.Parse_Until_Str ("?>");
    Trace ("Dtd parsed instruction " & Asu.To_String(Util.Get_Curr_Str));
    Util.reset_Curr_Str;
  end Parse_Instruction;

  -- Parse a directive
  procedure Parse_Directive is
  begin
    if Util.Try ("ELEMENT ") then
      -- @@@
      Util.Parse_Until_Str (">");
      Trace ("Dtd parsed directive ELEMENT " & Asu.To_String(Util.Get_Curr_Str));
      Util.reset_Curr_Str;
    elsif Util.Try ("ATTLIST ") then
      -- @@@
      Util.Parse_Until_Str (">");
      Trace ("Dtd parsed directive ATTLIST " & Asu.To_String(Util.Get_Curr_Str));
      Util.reset_Curr_Str;
    elsif Util.Try ("ENTITY ") then
      -- @@@
      Util.Parse_Until_Str (">");
      Trace ("Dtd parsed directive ENTITY " & Asu.To_String(Util.Get_Curr_Str));
      Util.reset_Curr_Str;
    elsif Util.Try ("NOTATION ") then
      Util.Error ("Unsupported NOTATION directive");
    else
      -- Skip CDATA and comments
      Parse_Directive (Only_Skip => True);
    end if;
  end Parse_Directive;

  -- Parse current dtd
  -- If external, will stop end end of file
  -- otherwise, will stop on ']'
  procedure Parse (External : in Boolean) is
  begin
    loop
      begin
        Util.Skip_Separators;
      exception
        when Util.End_Error =>
          if External then
            return;
          else
            Util.Error ("Unexpected end of file while parsing internal dtd");
          end if;
      end;
      if Util.Try (Util.Start & Util.Instruction) then
        Parse_Instruction;
      elsif Util.Try (Util.Start & Util.Directive) then
        Parse_Directive;
      elsif Util.Get = (']') and then not External then
        return;
      else
        Util.Error ("Unexpected characters while parsing dtd");
      end if;
    end loop;
  end Parse;

  -- Parse a dtd (either a external file or internal if name is empty)
  procedure Parse (File_Name : in String) is
    
    Dtd_File, Dummy_File : Text_Char.File_Type;
  begin
    if File_Name = "" then
      -- Internal declarations
      Trace ("Dtd parsing internal definition");
      Parse (False);
    else
      -- External declarations
      Trace ("Dtd parsing file " & File_Name);
      File_Mng.Open (File_Name, Dtd_File);
      Util.Init (False, Dtd_File);
      Parse (True);
      Util.Init (True, Dummy_File);
      File_Mng.Close (Dtd_File);
    end if;
    -- Dtd is now valid
    -- @@@ Dtd_Set := True;
  exception
    when File_Error =>
      Util.Error ("Cannot open dtd file " & File_Name);
  end Parse;

  -- Check Current element of the tree
  procedure Check_Element is
  begin
    if not Dtd_Set then
      -- No dtd => no check
      return;
    end if;
    -- @@@ Check current element vs dtd setting
  end Check_Element;

end Dtd;

