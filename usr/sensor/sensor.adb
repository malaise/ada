with Ada.Exceptions;
with Argument, Basic_Proc, As.U, Long_Longs, Timers, Event_Mng, Xml_Parser,
     Regular_Expressions;
with Debug, Rules, Filters, Executor;
procedure Sensor is

  Version : constant String := "V0.1";

  procedure Help is
  begin
    Basic_Proc.Put_Line_Error (Argument.Get_Program_Name
        & " <configuration file> | -h | --help | -v | --version");
  end Help;

  -- Xml parsing
  Ctx : Xml_Parser.Ctx_Type;
  Ok : Boolean;
  Root, Node, Child : Xml_Parser.Element_Type;
  Name, Text, Tmp : As.U.Asu_Us;
  Filter : Filters.Filter_Rec;
  Hist_Size : Natural;

begin

  Debug.Logger.Init ("Sensor");
  Basic_Proc.Set_Error_Exit_Code;
  -- Help mode
  if Argument.Get_Nbre_Arg /= 1 then
    Help;
    return;
  elsif Argument.Get_Parameter = "-h"
  or else Argument.Get_Parameter = "--help" then
    Help;
    return;
  elsif Argument.Get_Parameter = "-v"
  or else Argument.Get_Parameter = "--version" then
    Basic_Proc.Put_Line_Error (Argument.Get_Program_Name & " " & Version);
    return;
  end if;

  -- Parse configuration file
  Debug.Log ("Parsing file " & Argument.Get_Parameter);
  Ctx.Parse (Argument.Get_Parameter, Ok);
  if not Ok then
    Basic_Proc.Put_Line_Error ("Parse error in config: "
                                 & Ctx.Get_Parse_Error_Message);
    return;
  end if;
  Root := Ctx.Get_Root_Element;

  -- Check and store rules
  Node := Ctx.Get_Child (Root, 1);
  for I in 1 .. Ctx.Get_Nb_Children (Node) loop
    Child := Ctx.Get_Child (Node, I);
    -- Get name
    Name := Ctx.Get_Attribute (Child, 1).Value;
    -- Get action and check variables
    Text := Ctx.Get_Text (Ctx.Get_Child (Child, 1));
    Tmp := As.U.Tus (Rules.Check_Action (Text.Image));
    if not Tmp.Is_Null then
      Basic_Proc.Put_Line_Error ("Rule error on " & Name.Image
          & " => " & Tmp.Image & ".");
      return;
    end if;
    -- Store
    Debug.Log ("Storing rule " & Name.Image);
    Rules.Store (Name.Image, Text.Image);
  end loop;

  -- Store filters
  Node := Ctx.Get_Child (Root, 2);
  for I in 1 .. Ctx.Get_Nb_Children (Node) loop
    Child := Ctx.Get_Child (Node, I);
    -- Fill fields, checks types
    Filter.File := Ctx.Get_Attribute (Child, "File");
    begin
      Filter.Period := Timers.Period_Range'Value (
          Ctx.Get_Attribute (Child, "Period"));
    exception
      when others =>
        Basic_Proc.Put_Line_Error ("Filter at line"
            & Ctx.Get_Line_No (Child)'Img
            & " has invalid period " & Ctx.Get_Attribute (Child, "Period"));
        return;
    end;
    begin
      Filter.Tail := Long_Longs.Ll_Positive'Value (
          Ctx.Get_Attribute (Child, "Tail"));
    exception
      when others =>
        Basic_Proc.Put_Line_Error ("Filter at line "
            & Ctx.Get_Line_No (Child)'Img
            & " has invalid tail size " & Ctx.Get_Attribute (Child, "Tail"));
        return;
    end;
    -- Compile pattern
    Text := Ctx.Get_Text (Ctx.Get_Child (Child, 1));
    Filter.Pattern := new Regular_Expressions.Compiled_Pattern;
    Filter.Pattern.Compile (Ok, Text.Image);
    if not Ok then
      Basic_Proc.Put_Line_Error ("Filter at line "
            & Ctx.Get_Line_No (Child)'Img
            & " uses invalid pattern => " & Filter.Pattern.Error);
      return;
    end if;
    -- No circular buffer if 0
    begin
      Hist_Size := Natural'Value (Ctx.Get_Attribute (Child, "History"));
      if Hist_Size /= 0 then
        Filter.History := new Filters.Hist_Mng.Circ_Type (Hist_Size);
      end if;
    exception
      when others =>
        Basic_Proc.Put_Line_Error ("Filter at line "
            & Ctx.Get_Line_No (Child)'Img
            & " has invalid history size "
            & Ctx.Get_Attribute (Child, "History"));
        return;
    end;
    -- Rule must be known
    Filter.Rule := Ctx.Get_Attribute (Child, "Rule");
    if not Rules.Exists (Filter.Rule.Image) then
      Basic_Proc.Put_Line_Error ("Filter at line "
          & Ctx.Get_Line_No (Child)'Img
          & " refers to an unknown rule " & Filter.Rule.Image & ".");
      return;
    end if;
    -- Ok, store
    Debug.Log ("Storing filter " & Text.Image & " on " & Filter.File.Image);
    begin
      Filters.Store (Filter);
    exception
      when Filters.File_Not_Found =>
        Basic_Proc.Put_Line_Error ("Filter at line "
              & Ctx.Get_Line_No (Child)'Img
              & " is based on unknonw file " & Filter.File.Image);
      return;
    end;
  end loop;

  -- Init executor
  Executor.Init;

  -- Main loop
  Debug.Log ("Main loop");
  loop
    Event_Mng.Wait (Event_Mng.Infinite_Ms);
    exit when Executor.Exit_Requested;
  end loop;

  -- Done on SigTerm
  Debug.Log ("Stopping");
  Basic_Proc.Set_Ok_Exit_Code;

exception
  when Xml_Parser.File_Error =>
    Basic_Proc.Put_Line_Error (
        "Cannot open config file: " & Argument.Get_Parameter & ".");
  when Error: others =>
    Basic_Proc.Put_Line_Error (
        "Exception " & Ada.Exceptions.Exception_Name (Error) & " raised.");
    Basic_Proc.Set_Error_Exit_Code;
end Sensor;


