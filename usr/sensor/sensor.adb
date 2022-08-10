with Ada.Exceptions, Ada.Calendar;
with Argument, Basic_Proc, Sys_Calls, As.U, Timers, Event_Mng,
     Xml_Parser, Reg_Exp, Date_Text, Queues;
with Debug, Actions, Rules, Executor;
procedure Sensor is

  Version : constant String := "V7.1";

  procedure Help is
  begin
    Basic_Proc.Put_Line_Error (Argument.Get_Program_Name
        & " [ -c | --check ] <configuration file> | -h | --help | -v | --version");
  end Help;

  -- File and option
  File : As.U.Asu_Us;
  Check_Only : Boolean;

  -- Xml parsing
  Ctx : Xml_Parser.Ctx_Type;
  Ok : Boolean;
  Root, Var, Class, Node, Child : Xml_Parser.Element_Type;
  Name, Value, Text, Tmp : As.U.Asu_Us;
  Rule : Rules.Rule_Rec;
  Hist_Size : Queues.Len_Range;

  -- Shortcut
  function Expand (Text : String) return String
    renames Actions.Expand_Variables;
  function Expand (Text : String) return As.U.Asu_Us is
    (As.U.Tus (Actions.Expand_Variables (Text)));

begin

  -- Init
  -------
  Debug.Logger.Init ("Sensor");
  Basic_Proc.Set_Error_Exit_Code;
  Check_Only := False;
  -- Help mode, version, option
  if Argument.Get_Nbre_Arg = 0 or else Argument.Get_Nbre_Arg > 2 then
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
  elsif Argument.Get_Nbre_Arg = 2 and then
      (Argument.Get_Parameter = "-c"
       or else Argument.Get_Parameter = "--check") then
    Check_Only := True;
  elsif Argument.Get_Nbre_Arg /= 1 then
    Help;
    return;
  end if;

  -- Parse configuration file
  ---------------------------
  Argument.Get_Parameter (File, Occurence => (if Check_Only then 2 else 1) );
  Debug.Log ("Parsing file " & File.Image);
  Ctx.Parse (File.Image, Ok);
  if not Ok then
    Basic_Proc.Put_Line_Error ("Parse error in config: "
                                 & Ctx.Get_Parse_Error_Message);
    return;
  end if;
  Root := Ctx.Get_Root_Element;

  -- Define variables
  for I in 1 .. Ctx.Get_Nb_Children (Root) - 2 loop
    Var := Ctx.Get_Child (Root, I);
    Class := Ctx.Get_Brother (Var);
    -- Get name and value
    Name := Ctx.Get_Attribute (Var, 1).Value;
    Value := Ctx.Get_Attribute (Var, 2).Value;
    -- Define variable
    begin
      Actions.Define_Variable (Expand (Name.Image), Expand (Value.Image));
    exception
      when Actions.Invalid_Variable =>
        Basic_Proc.Put_Line_Error ("Invalid variable " & Name.Image);
        return;
    end;
    Debug.Log ("Got variable " & Name.Image & "=" & Value.Image);
  end loop;
  if Ctx.Get_Nb_Children (Root) = 2 then
    Class := Ctx.Get_Child (Root, 1);
  end if;

  -- Check and store actions
  --------------------------
  for I in 1 .. Ctx.Get_Nb_Children (Class) loop
    Node := Ctx.Get_Child (Class, I);
    -- Get name
    Name := Expand (Ctx.Get_Attribute (Node, 1).Value.Image);
    -- Get action and check variables
    Text := Ctx.Get_Attribute (Node, 2).Value;
    Tmp := As.U.Tus (Actions.Check_Command (Text.Image));
    if not Tmp.Is_Null then
      Basic_Proc.Put_Line_Error ("Action error on " & Name.Image
          & " => " & Tmp.Image & ".");
      return;
    end if;
    -- Store action
    Actions.Store (Name.Image, Text.Image);
    Debug.Log ("Stored action " & Name.Image & ": " & Text.Image);
    -- Parse repeats
    for J in 1 .. Ctx.Get_Nb_Children (Node) loop
      Child := Ctx.Get_Child (Node, J);
      begin
        Actions.Add_Repeat (
            Name.Image,
            Positive'Value (Ctx.Get_Attribute (Child, "Number")),
            Duration'Value (Ctx.Get_Attribute (Child, "During")),
            Ctx.Get_Attribute (Child, "Action"));
      exception
        when others =>
          Basic_Proc.Put_Line_Error ("Invalid repetition at line"
            & Ctx.Get_Line_No (Child)'Img);
          return;
      end;
      Debug.Log ("Added repetition for " & Name.Image
          & " repeated " & Ctx.Get_Attribute (Child, "Number")
          & " times within " & Ctx.Get_Attribute (Child, "During")
          & "s, triggering " & Ctx.Get_Attribute (Child, "Action"));
    end loop;
  end loop;

  -- Check and store Rules
  ------------------------
  Class := Ctx.Get_Brother (Class);
  for I in 1 .. Ctx.Get_Nb_Children (Class) loop
    Node := Ctx.Get_Child (Class, I);

    -- Scan
    Child := Ctx.Get_Child (Node, 1);
    -- File
    Rule.File := Expand (Ctx.Get_Attribute (Child, "File"));
    if not Sys_Calls.File_Found (Rule.File.Image) then
      Basic_Proc.Put_Line_Error ("Rule at line"
          & Ctx.Get_Line_No (Node)'Img
          & " refers to unreadable file " & Rule.File.Image);
      return;
    end if;
    Debug.Log ("  Got file " & Rule.File.Image);
    -- Compile pattern
    Text := Expand (Ctx.Get_Attribute (Child, "Criteria"));
    Rule.Pattern := new Reg_Exp.Compiled_Pattern;
    Rule.Pattern.Compile (Ok, Text.Image);
    if not Ok then
      Basic_Proc.Put_Line_Error ("Rule at line "
            & Ctx.Get_Line_No (Child)'Img
            & " uses invalid pattern => " & Rule.Pattern.Error);
      return;
    end if;
    Debug.Log ("  Got pattern " & Text.Image);
    -- Period
    begin
      Text := Ctx.Get_Attribute (Child, "Period");
      Text := Expand (Text.Image);
      Rule.Period := Timers.Period_Range'Value (Text.Image);
      if Rule.Period < 0.1 then
        raise Constraint_Error;
      end if;
    exception
      when others =>
        Basic_Proc.Put_Line_Error ("Rule at line"
            & Ctx.Get_Line_No (Child)'Img
            & " has invalid period " & Text.Image);
        return;
    end;
    Debug.Log ("  Got period " & Text.Image);
    -- Tail
    begin
      Rule.Tail := Rules.Tail_Length'Value (
          Expand (Ctx.Get_Attribute (Child, "Tail")));
    exception
      when others =>
        Basic_Proc.Put_Line_Error ("Rule at line "
            & Ctx.Get_Line_No (Child)'Img
            & " has invalid tail size " & Ctx.Get_Attribute (Child, "Tail"));
        return;
    end;
    -- Optional Past
    if Ctx.Get_Nb_Children (Child) = 0 then
      Debug.Log ("  No Past");
      Rule.Aging := 0.0;
    else
      Child := Ctx.Get_Child (Child, 1);
      -- Get time, format
      Rule.Time_Format := Expand (Ctx.Get_Attribute (Child, "Format"));
      declare
        Dummy_Len : Natural;
      begin
        Dummy_Len := Date_Text.Length (Rule.Time_Format.Image);
      exception
        when others =>
          Basic_Proc.Put_Line_Error ("Rule at line "
              & Ctx.Get_Line_No (Child)'Img
              & " has invalid format " & Rule.Time_Format.Image);
      end;
      Debug.Log ("  Got past format " & Rule.Time_Format.Image);
      -- Past age in seconds
      begin
        Rule.Aging := Duration'Value (
          Expand (Ctx.Get_Attribute (Child, "Seconds")));
        if Rule.Aging <= 0.0 then
          raise Constraint_Error;
        end if;
      exception
        when others =>
          Basic_Proc.Put_Line_Error ("Rule at line "
              & Ctx.Get_Line_No (Child)'Img
              & " has invalid age " & Ctx.Get_Attribute (Child, "Seconds"));
          return;
      end;
      Debug.Log ("  Got past seconds " & Rule.Aging'Img);
    end if;

    -- Execute
    Child := Ctx.Get_Child (Node, 2);
    -- Action must be known
    Rule.Action := Expand (Ctx.Get_Attribute (Child, "Action"));
    if not Actions.Exists (Rule.Action.Image) then
      Basic_Proc.Put_Line_Error ("Rule at line "
          & Ctx.Get_Line_No (Child)'Img
          & " refers to an unknown action " & Rule.Action.Image & ".");
      return;
    end if;
    Debug.Log ("  Got action " & Rule.Action.Image);
    -- No history if 0
    begin
      Hist_Size := Queues.Len_Range'Value (
          Expand (Ctx.Get_Attribute (Child, "History")));
      if Hist_Size /= 0 then
        Rule.History := new Rules.Hist_Mng.Circ_Type (Hist_Size);
        Debug.Log ("  Got history " & Hist_Size'Img);
      end if;
    exception
      when others =>
        Basic_Proc.Put_Line_Error ("Rule at line "
            & Ctx.Get_Line_No (Child)'Img
            & " has invalid history size "
            & Ctx.Get_Attribute (Child, "History"));
        return;
    end;
    -- Optional latency
    begin
      Text := Ctx.Get_Attribute (Child, "Latency");
      Text := Expand (Text.Image);
      Rule.Latency := Duration'Value (Text.Image);
      if Rule.Latency < 0.0 then
        raise Constraint_Error;
      end if;
      Debug.Log ("  Got latency " & Rule.Latency'Img);
    exception
      when Xml_Parser.Attribute_Not_Found =>
        -- Default
        Rule.Latency := 0.0;
      when others =>
        Basic_Proc.Put_Line_Error ("Rule at line "
          & Ctx.Get_Line_No (Child)'Img
          & " defines invalid latency " & Text.Image & ".");
      return;
    end;

    -- Previous execution (for latency)
    Rule.Previous := new Ada.Calendar.Time'(
        Ada.Calendar.Time_Of (Ada.Calendar.Year_Number'First,
                              Ada.Calendar.Month_Number'First,
                              Ada.Calendar.Day_Number'First));
    Rule.Nb_Match := new Natural'(0);
    Rule.Matches := new As.U.Asu_Us;

    -- Ok, store
    Debug.Log ("Storing rule for " & Rule.Action.Image
             & " on " & Rule.File.Image);
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

  -- Only check config file
  if Check_Only then
    Debug.Log ("Check only");
    Basic_Proc.Set_Ok_Exit_Code;
    return;
  end if;

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
  Executor.Close;
  Basic_Proc.Set_Ok_Exit_Code;
  Debug.Log ("Done");

exception
  when Xml_Parser.File_Error =>
    Basic_Proc.Put_Line_Error (
        "Cannot open config file: " & File.Image & ".");
  when Error: others =>
    Basic_Proc.Put_Line_Error (
        "Exception " & Ada.Exceptions.Exception_Name (Error) & " raised.");
    Basic_Proc.Set_Error_Exit_Code;
end Sensor;


