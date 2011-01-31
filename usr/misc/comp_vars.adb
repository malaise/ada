with Ada.Text_Io;
with As.U, Argument, Text_Line, Sys_Calls, Xml_Parser, Computer, Environ,
     Int_Image;
procedure Comp_Vars is

  -- Computer memory
  Memory : Computer.Memory_Type;

  -- Image of a computed value
  function Comp_Image is new Int_Image (Integer);

  -- Getenv raising exception if not set
  function My_Getenv (Name : String) return String is
  begin
    -- Try to resolve through environ
    return Environ.Getenv_If_Set (Name);
  exception
    when Environ.Name_Error =>
      -- Raise Any exception
      Sys_Calls.Put_Line_Error ("Error: Environ variable "
                               & Name & " not found.");
      raise;
  end My_Getenv;

  -- Usage
  Shell_Opt_C : constant String := "-s";
  Shell_Opt_S : constant String := "--shell";
  procedure Usage is
  begin
    Ada.Text_Io.Put_Line ("Usage: " & Argument.Get_Program_Name
     & "[ " & Shell_Opt_C & " | " & Shell_Opt_S & " ] { <file> }");
  end Usage;

  -- Output flow (stdout)
  Out_File : Text_Line.File_Type;
  Xml_Format : Boolean := True;

  -- Parses one file (or input flow)
  -- Return True if parsing OK
  function Do_One (File_Name : String) return Boolean is
    Ctx : Xml_Parser.Ctx_Type;
    Step_Parsing : Boolean;
    -- Log an error
    procedure Error (Msg  : in String;
                     Node : in Xml_Parser.Node_Type) is
      use type Xml_Parser.Node_Kind_List;
    begin
      Sys_Calls.Put_Error ("Error ");
      if Step_Parsing then
        Sys_Calls.Put_Error ("parsing ");
      else
        Sys_Calls.Put_Error ("processing ");
      end if;
      if File_Name /= "" then
        Sys_Calls.Put_Error ("file " & File_Name);
      else
        Sys_Calls.Put_Error ("input flow");
      end if;
      if Xml_Parser.Is_Valid (Node) then
        Sys_Calls.Put_Error (" at line"
           & Positive'Image (Ctx.Get_Line_No (Node)));
        if Node.Kind = Xml_Parser.Element then
          Sys_Calls.Put_Error (" on node " & Ctx.Get_Name (Node));
        end if;
      end if;
      if Msg = "" then
        Sys_Calls.Put_Line_Error (".");
      else
        Sys_Calls.Put_Line_Error (":");
        Sys_Calls.Put_Line_Error (Msg);
      end if;
      Ctx.Clean;
    end Error;

    -- Parsed objects, root, variables, attributes and value
    Vars : Xml_Parser.Element_Type;
    Var, Val : Xml_Parser.Node_Type;
    Attr1, Attr2 : Xml_Parser.Attribute_Rec;
    -- Is variable an int or a string
    Var_Is_Int : Boolean;
    Text : As.U.Asu_Us;
    -- Result of evaluation
    Result : As.U.Asu_Us;
    use type Xml_Parser.Node_Kind_List;
  begin
    -- Parse
    Step_Parsing := True;
    declare
      Ok : Boolean;
    begin
      Ctx.Parse (File_Name, Ok);
      if not Ok then
        Error (Ctx.Get_Parse_Error_Message, Vars);
        return False;
      end if;
    exception
      when Xml_Parser.File_Error =>
        Error ("Cannot open file.", Vars);
        return False;
    end;
    -- Get the root
    Vars := Ctx.Get_Root_Element;
    -- Must be "Variables"
    if Ctx.Get_Name (Vars) /= "Variables" then
      Error ("Unexpected node name.", Vars);
      return False;
    end if;
    -- Must have no attributes
    if Ctx.Get_Nb_Attributes (Vars) /= 0 then
      Error ("Unexpected attribute(s) to ""Variables"".", Vars);
      return False;
    end if;

    -- Each variable
    for I in 1 .. Ctx.Get_Nb_Children (Vars) loop
      -- A variable
      Var := Ctx.Get_Child (Vars, I);
      if Var.Kind /= Xml_Parser.Element then
        Error ("Invalid element as Variable content.", Var);
        return False;
      end if;
      if Ctx.Get_Name (Var) /= "Var" then
        Error ("Unexpected node name.", Var);
        return False;
      end if;

      -- Check and get attributes Name and Type
      if Ctx.Get_Nb_Attributes (Var) = 1 then
        -- One attribute: a string
        Attr1 := Ctx.Get_Attribute (Var, 1);
        Var_Is_Int := False;
      elsif Ctx.Get_Nb_Attributes (Var) = 2 then
        -- Two attributes
        Attr1 := Ctx.Get_Attribute (Var, 1);
        Attr2 := Ctx.Get_Attribute (Var, 2);
        if Attr1.Name.Image = "Type" then
          -- Swap so that Attr1 is Name and Attr2 is Type
          declare
            Attrt : constant Xml_Parser.Attribute_Rec := Attr1;
          begin
            Attr1 := Attr2;
            Attr2 := Attrt;
          end;
        end if;
        -- Second attribute must be type
        if Attr2.Name.Image /= "Type" then
          Error ("Invalid attribute " & Attr2.Name.Image
               & " to Variable.", Var);
          return False;
        end if;

        -- Check Type is Int or Str
        if Attr2.Value.Image = "Int" then
          Var_Is_Int := True;
        elsif Attr2.Value.Image = "Str" then
          Var_Is_Int := False;
        else
          Error ("Invalid type " & Attr2.Value.Image
               & " to Variable.", Var);
          return False;
        end if;
      else
        -- 0 or more than 2 attributes
        Error ("Invalid attribute(s) to Variable.", Var);
        return False;
      end if;
      -- First attribute must be Name
      if Attr1.Name.Image /= "Name" then
        Error ("Invalid attribute " & Attr1.Name.Image
             & " to Variable.", Var);
        return False;
      end if;

      -- Get Value
      if Ctx.Get_Nb_Children (Var) = 0
      and then not Var_Is_Int then
        -- Empty string
        Text.Set_Null;
      elsif Ctx.Get_Nb_Children (Var) = 1 then
        Val := Ctx.Get_Child (Var, 1);
        Text := Ctx.Get_Text (Val);
      else
        Error ("Invalid content of Variable.", Var);
        return False;
      end if;

      -- Eval or compute
      Step_Parsing := False;
      declare
        Expr : constant String := Text.Image;
      begin
        if Var_Is_Int then
          Result := As.U.Tus (Comp_Image (Memory.Compute (Expr)));
        else
          Result := As.U.Tus (Memory.Eval (Expr));
        end if;
      exception
        when Computer.Unknown_Variable =>
          Error ("Unknown variable while evaluating variable.", Var);
          return False;
        when Computer.Invalid_Expression =>
          Error ("Invalid expression while evaluating variable.", Var);
          return False;
      end;

      -- Store Result
      Memory.Set (Name  => Attr1.Value.Image,
                    Value => Result.Image,
                    Modifiable => True, Persistent => False);
      -- Display result
      if Xml_Format then
        Out_File.Put ("  <Var "
          & "Name=""" & Attr1.Value.Image & """ "
          & "Type=""");
        if Var_Is_Int then
          Out_File.Put ("Int");
        else
          Out_File.Put ("Str");
        end if;
        Out_File.Put_Line (""">" & Result.Image & "</Var>");
      else
        Out_File.Put ("export " & Attr1.Value.Image
           & "=");
        if Var_Is_Int then
          Out_File.Put_Line (Result.Image);
        else
          Out_File.Put_Line ("""" & Result.Image & """");
        end if;
      end if;

    end loop;
    -- Done
    return True;
  end Do_One;

  -- First file argument
  First_File : Positive := 1;
  -- Ok so far
  Ok : Boolean;
begin

  -- Init
  Memory.Set_External_Resolver (My_Getenv'Unrestricted_Access);

  -- Parse Help
  if Argument.Get_Nbre_Arg >= 1
  and then (Argument.Get_Parameter (Occurence => 1) = "-h"
    or else Argument.Get_Parameter (Occurence => 1) = "--help") then
    Usage;
    Sys_Calls.Set_Error_Exit_Code;
    return;
  end if;

  -- Parse Option
  if Argument.Get_Nbre_Arg >= 1
  and then (Argument.Get_Parameter (Occurence => 1) = Shell_Opt_C
    or else Argument.Get_Parameter (Occurence => 1) = Shell_Opt_S) then
    Xml_Format := False;
    First_File := 2;
  end if;

  -- Prepare output
  Out_File.Open (Text_Line.Out_File, Sys_Calls.Stdout);
  if Xml_Format then
    Out_File.Put_Line ("<?xml version=""1.1""?>");
    Out_File.Put_Line ("<Variables>");
  else
    Out_File.Put_Line ("#!/bin/bash");
  end if;

  -- Process arguments
  Ok := True;
  if First_File > Argument.Get_Nbre_Arg then
    -- No file => Stdin
    Ok := Do_One ("");
  else
    for I in First_File .. Argument.Get_Nbre_Arg loop
      -- Each argument
      Ok := Ok and then Do_One (Argument.Get_Parameter (Occurence => I));
    end loop;
  end if;

  -- Finalize output
  if Ok then
    if Xml_Format then
      Out_File.Put_Line ("</Variables>");
    end if;
  else
    Sys_Calls.Set_Error_Exit_Code;
  end if;
  Out_File.Close;

exception
  when others =>
    if Out_File.Is_Open then
      Out_File.Close;
    end if;
    raise;
end Comp_Vars;

