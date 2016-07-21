with Ada.Exceptions;
with Argument, Basic_Proc, Sys_Calls, As.U, Timers, Event_Mng, Xml_Parser,
     File_Access, Regular_Expressions, Date_Text;
with Debug, Rules, Filters, Executor;
procedure Sensor is

  Version : constant String := "V2.0";

  procedure Help is
  begin
    Basic_Proc.Put_Line_Error (Argument.Get_Program_Name
        & " <configuration file> | -h | --help | -v | --version");
  end Help;

  -- Xml parsing
  Ctx : Xml_Parser.Ctx_Type;
  Ok : Boolean;
  Root, Class, Node, Child : Xml_Parser.Element_Type;
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
  Class := Ctx.Get_Child (Root, 1);
  for I in 1 .. Ctx.Get_Nb_Children (Class) loop
    Node := Ctx.Get_Child (Class, I);
    -- Get name
    Name := Ctx.Get_Attribute (Node, 1).Value;
    -- Get action and check variables
    Text := Ctx.Get_Text (Ctx.Get_Child (Node, 1));
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

  -- Check and Store filters
  Class := Ctx.Get_Child (Root, 2);
  for I in 1 .. Ctx.Get_Nb_Children (Class) loop
    Node := Ctx.Get_Child (Class, I);

    -- Fill fields, checks types
    Filter.File := Ctx.Get_Attribute (Node, "File");
    declare
      File_Stat : Sys_Calls.File_Stat_Rec;
      Can_Read, Dummy_Write, Dummy_Exec : Boolean;
    begin
      File_Stat := Sys_Calls.File_Stat (Filter.File.Image);
      File_Access (Sys_Calls.Get_Effective_User_Id,
                   Sys_Calls.Get_Effective_Group_Id,
                   File_Stat.User_Id, File_Stat.Group_Id, File_Stat.Rights,
                   Can_Read, Dummy_Write, Dummy_Exec);
      if not Can_Read then
        raise Sys_Calls.Access_Error;
      end if;
    exception
      when others =>
        Basic_Proc.Put_Line_Error ("Filter at line"
            & Ctx.Get_Line_No (Node)'Img
            & " refers to unreadable file " & Filter.File.Image);
        return;
    end;
    -- Period
    begin
      Filter.Period := Timers.Period_Range'Value (
          Ctx.Get_Attribute (Node, "Period"));
      if Filter.Period < 1.0 then
        raise Constraint_Error;
      end if;
    exception
      when others =>
        Basic_Proc.Put_Line_Error ("Filter at line"
            & Ctx.Get_Line_No (Node)'Img
            & " has invalid period " & Ctx.Get_Attribute (Node, "Period"));
        return;
    end;
    -- Tail
    begin
      Filter.Tail := Filters.Tail_Length'Value (
          Ctx.Get_Attribute (Node, "Tail"));
    exception
      when others =>
        Basic_Proc.Put_Line_Error ("Filter at line "
            & Ctx.Get_Line_No (Node)'Img
            & " has invalid tail size " & Ctx.Get_Attribute (Node, "Tail"));
        return;
    end;
    -- No history if 0
    begin
      Hist_Size := Natural'Value (Ctx.Get_Attribute (Node, "History"));
      if Hist_Size /= 0 then
        Filter.History := new Filters.Hist_Mng.Circ_Type (Hist_Size);
      end if;
    exception
      when others =>
        Basic_Proc.Put_Line_Error ("Filter at line "
            & Ctx.Get_Line_No (Node)'Img
            & " has invalid history size "
            & Ctx.Get_Attribute (Node, "History"));
        return;
    end;
    -- Rule must be known
    Filter.Rule := Ctx.Get_Attribute (Node, "Rule");
    if not Rules.Exists (Filter.Rule.Image) then
      Basic_Proc.Put_Line_Error ("Filter at line "
          & Ctx.Get_Line_No (Node)'Img
          & " refers to an unknown rule " & Filter.Rule.Image & ".");
      return;
    end if;
    -- Optional past
    if Ctx.Get_Nb_Children (Node) = 1 then
      Filter.Seconds := 0;
      -- Set Child to pattern
      Child := Ctx.Get_Child (Node, 1);
    else
      -- Get seconds and time format
      Child := Ctx.Get_Child (Node, 1);
      begin
        Filter.Seconds := Filters.Tail_Length'Value (
          Ctx.Get_Attribute (Child, "Seconds"));
      exception
        when others =>
          Basic_Proc.Put_Line_Error ("Filter at line "
              & Ctx.Get_Line_No (Node)'Img
              & " has invalid seconds " & Ctx.Get_Attribute (Node, "Seconds"));
          return;
      end;
      Filter.Time_Format := Ctx.Get_Text (Ctx.Get_Child (Child, 1));
      declare
        Dummy_Len : Natural;
      begin
        Dummy_Len := Date_Text.Length (Filter.Time_Format.Image);
      exception
        when others =>
          Basic_Proc.Put_Line_Error ("Filter at line "
              & Ctx.Get_Line_No (Node)'Img
              & " has invalid format " & Filter.Time_Format.Image);
      end;
      -- Set Child to pattern
      Child := Ctx.Get_Child (Node, 2);
    end if;
    -- Compile pattern
    Text := Ctx.Get_Text (Ctx.Get_Child (Child, 1));
    Filter.Pattern := new Regular_Expressions.Compiled_Pattern;
    Filter.Pattern.Compile (Ok, Text.Image);
    if not Ok then
      Basic_Proc.Put_Line_Error ("Filter at line "
            & Ctx.Get_Line_No (Node)'Img
            & " uses invalid pattern => " & Filter.Pattern.Error);
      return;
    end if;
    -- Ok, store
    Debug.Log ("Storing filter " & Text.Image & " on " & Filter.File.Image);
    begin
      Filters.Store (Filter);
    exception
      when Filters.File_Not_Found =>
        Basic_Proc.Put_Line_Error ("Filter at line "
              & Ctx.Get_Line_No (Node)'Img
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


