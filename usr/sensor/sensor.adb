with Ada.Exceptions, Ada.Calendar;
with Argument, Basic_Proc, Sys_Calls, As.U, Timers, Event_Mng,
     Xml_Parser, Reg_Exp, Date_Text, Queues;
with Debug, Actions, Rules, Executor;
procedure Sensor is

  Version : constant String := "V5.0";

  procedure Help is
  begin
    Basic_Proc.Put_Line_Error (Argument.Get_Program_Name
        & " <configuration file> | -h | --help | -v | --version");
  end Help;

  -- Xml parsing
  Ctx : Xml_Parser.Ctx_Type;
  Ok : Boolean;
  Root, Vars, Class, Node, Child : Xml_Parser.Element_Type;
  Name, Text, Tmp : As.U.Asu_Us;
  Rule : Rules.Rule_Rec;
  Hist_Size : Queues.Len_Range;

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

  -- Define variables
  if Ctx.Get_Nb_Children (Root) = 2 then
    -- No variables
    Class := Ctx.Get_Child (Root, 1);
  else
    Vars := Ctx.Get_Child (Root, 1);
    Class := Ctx.Get_Brother (Vars);
    for I in 1 .. Ctx.Get_Nb_Children (Vars) loop
      Node := Ctx.Get_Child (Vars, I);
      -- Get name and value
      Name := Ctx.Get_Attribute (Node, 1).Value;
      Text := Ctx.Get_Text (Ctx.Get_Child (Node, 1));
      -- Define variable
      begin
        Actions.Define (Name.Image, Text.Image);
      exception
        when Actions.Invalid_Variable =>
          Basic_Proc.Put_Line_Error ("Invalid variable " & Name.Image);
          return;
      end;
    end loop;
  end if;

  -- Check and store actions
  for I in 1 .. Ctx.Get_Nb_Children (Class) loop
    Node := Ctx.Get_Child (Class, I);
    -- Get name
    Name := Ctx.Get_Attribute (Node, 1).Value;
    -- Get action and check variables
    Text := Ctx.Get_Text (Ctx.Get_Child (Node, 1));
    Tmp := As.U.Tus (Actions.Check_Command (Text.Image));
    if not Tmp.Is_Null then
      Basic_Proc.Put_Line_Error ("Action error on " & Name.Image
          & " => " & Tmp.Image & ".");
      return;
    end if;
    -- Store action
    Debug.Log ("Storing action " & Name.Image);
    Actions.Store (Name.Image, Text.Image);
  end loop;

  -- Check and Store rules
  Class := Ctx.Get_Brother (Class);
  for I in 1 .. Ctx.Get_Nb_Children (Class) loop
    Node := Ctx.Get_Child (Class, I);

    -- Fill fields, checks types
    Rule.File := Ctx.Get_Attribute (Node, "File");
    if not Sys_Calls.File_Found (Rule.File.Image) then
      Basic_Proc.Put_Line_Error ("Rule at line"
          & Ctx.Get_Line_No (Node)'Img
          & " refers to unreadable file " & Rule.File.Image);
      return;
    end if;
    -- Period
    begin
      Rule.Period := Timers.Period_Range'Value (
          Ctx.Get_Attribute (Node, "Period"));
      if Rule.Period < 1.0 then
        raise Constraint_Error;
      end if;
    exception
      when others =>
        Basic_Proc.Put_Line_Error ("Rule at line"
            & Ctx.Get_Line_No (Node)'Img
            & " has invalid period " & Ctx.Get_Attribute (Node, "Period"));
        return;
    end;
    -- Tail
    begin
      Rule.Tail := Rules.Tail_Length'Value (
          Ctx.Get_Attribute (Node, "Tail"));
    exception
      when others =>
        Basic_Proc.Put_Line_Error ("Rule at line "
            & Ctx.Get_Line_No (Node)'Img
            & " has invalid tail size " & Ctx.Get_Attribute (Node, "Tail"));
        return;
    end;
    -- No history if 0
    begin
      Hist_Size := Queues.Len_Range'Value (Ctx.Get_Attribute (Node, "History"));
      if Hist_Size /= 0 then
        Rule.History := new Rules.Hist_Mng.Circ_Type (Hist_Size);
      end if;
    exception
      when others =>
        Basic_Proc.Put_Line_Error ("Rule at line "
            & Ctx.Get_Line_No (Node)'Img
            & " has invalid history size "
            & Ctx.Get_Attribute (Node, "History"));
        return;
    end;
    -- Action must be known
    Rule.Action := Ctx.Get_Attribute (Node, "Action");
    if not Actions.Exists (Rule.Action.Image) then
      Basic_Proc.Put_Line_Error ("Rule at line "
          & Ctx.Get_Line_No (Node)'Img
          & " refers to an unknown action " & Rule.Action.Image & ".");
      return;
    end if;
    -- Optional latency
    begin
      Text := Ctx.Get_Attribute (Node, "Latency");
      Rule.Latency := Rules.Tail_Length'Value (Text.Image);
    exception
      when Xml_Parser.Attribute_Not_Found =>
        -- Default
        Rule.Latency := 0;
      when others =>
        Basic_Proc.Put_Line_Error ("Rule at line "
          & Ctx.Get_Line_No (Node)'Img
          & " defines invalid latency " & Text.Image & ".");
      return;
    end;
    -- Previous execution (for latency)
    Rule.Previous := new Ada.Calendar.Time'(
        Ada.Calendar.Time_Of (Ada.Calendar.Year_Number'First,
                              Ada.Calendar.Month_Number'First,
                              Ada.Calendar.Day_Number'First));
    -- Optional past
    if Ctx.Get_Nb_Children (Node) = 1 then
      Rule.Seconds := 0;
      -- Set Child to pattern
      Child := Ctx.Get_Child (Node, 1);
    else
      -- Get seconds and time format
      Child := Ctx.Get_Child (Node, 1);
      begin
        Rule.Seconds := Rules.Tail_Length'Value (
          Ctx.Get_Attribute (Child, "Seconds"));
      exception
        when others =>
          Basic_Proc.Put_Line_Error ("Rule at line "
              & Ctx.Get_Line_No (Node)'Img
              & " has invalid seconds " & Ctx.Get_Attribute (Node, "Seconds"));
          return;
      end;
      Rule.Time_Format := Ctx.Get_Text (Ctx.Get_Child (Child, 1));
      declare
        Dummy_Len : Natural;
      begin
        Dummy_Len := Date_Text.Length (Rule.Time_Format.Image);
      exception
        when others =>
          Basic_Proc.Put_Line_Error ("Rule at line "
              & Ctx.Get_Line_No (Node)'Img
              & " has invalid format " & Rule.Time_Format.Image);
      end;
      -- Set Child to pattern
      Child := Ctx.Get_Child (Node, 2);
    end if;
    -- Compile pattern
    Text := Ctx.Get_Text (Ctx.Get_Child (Child, 1));
    Rule.Pattern := new Reg_Exp.Compiled_Pattern;
    Rule.Pattern.Compile (Ok, Text.Image);
    if not Ok then
      Basic_Proc.Put_Line_Error ("Rule at line "
            & Ctx.Get_Line_No (Node)'Img
            & " uses invalid pattern => " & Rule.Pattern.Error);
      return;
    end if;
    Rule.Result := new As.U.Asu_Us;
    -- Ok, store
    Debug.Log ("Storing rule " & Text.Image & " on " & Rule.File.Image);
    begin
      Rules.Store (Rule);
    exception
      when Rules.File_Not_Found =>
        Basic_Proc.Put_Line_Error ("Rule at line "
              & Ctx.Get_Line_No (Node)'Img
              & " is based on unknonw file " & Rule.File.Image);
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


