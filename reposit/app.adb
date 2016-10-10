with Basic_Proc, Trace.Loggers, As.U, Argument, Argument_Parser, Long_Longs,
     Mixed_Str, Str_Util.Regex, Ada_Words, Text_Line, Unbounded_Arrays,
     Many_Strings, Reg_Exp, Unlimited_Pool;
procedure App is

  Version : constant String := "V02.01";

  -- Log an error and raise
  Raised_Error : exception;
  procedure Error (Msg : in String) is
  begin
    Basic_Proc.Put_Line_Error ("ERROR: " & Msg & ".");
    raise Raised_Error;
  end Error;

  -- Options
  Keys : constant Argument_Parser.The_Keys_Type := (
    1 => (False, 'h', As.U.Tus ("help"),    False),
    2 => (False, 'v', As.U.Tus ("version"), False),
    3 => (True,  'p', As.U.Tus ("prefix"),  False, True, As.U.Tus ("prefix")));
  Arg_Dscr : Argument_Parser.Parsed_Dscr;
  No_Key_Index : constant Argument_Parser.The_Keys_Index
               := Argument_Parser.No_Key_Index;

  -- Put usage
  procedure Usage is
  begin
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
        & " [ <prefix> ] [ { <definition> } ]");
    Basic_Proc.Put_Line_Error (
        "<prefix>     ::= --prefix=<regex> | -p <regex>     // default ""#""");
    Basic_Proc.Put_Line_Error (
        "<definition> ::= <name>[=[<value>]]");
  end Usage;

  -- Multi-purpose string
  Str : As.U.Asu_Us;

  -- Index of "=" in argument
  Index : Natural;

  -- Trace logger
  Logger : Trace.Loggers.Logger;

  -- Prefix, initialized to default
  Prefix : As.U.Asu_Us := As.U.Tus ("#");

  -- Current line of input
  Line_No : Long_Longs.Ll_Natural;

  -- Definitions
  --------------
  type Definition_Rec is record
    Name, Value : As.U.Asu_Us;
  end record;
  type Definition_Array is array (Positive range <>) of Definition_Rec;
  package Definitions_Mng is new Unbounded_Arrays (Definition_Rec,
                                                   Definition_Array);
  Definitions : Definitions_Mng.Unb_Array;
  Definition : Definition_Rec;
  -- Append or replace
  Found : Boolean;

  -- Check, then replace or add a definition
  procedure Define (Definition : in Definition_Rec) is
    use type As.U.Asu_Us;
  begin
    -- Check name validity
    if not Ada_Words.Is_Identifier (Definition.Name.Image) then
      Error ("Invalid definition name " & Definition.Name.Image);
    end if;

    -- Store: replace or append
    Found := False;
    for I in 1 .. Definitions.Length loop
      if Definitions.Element(I).Name = Definition.Name then
        Definitions.Replace_Element (I, Definition);
        Found := True;
        Logger.Log_Debug ("Definition replaced >" & Definition.Name.Image
                        & "=" & Definition.Value.Image & "<");
        exit;
      end if;
    end loop;
    if not Found then
      Definitions.Append (Definition);
      Logger.Log_Debug ("Definition appended >" & Definition.Name.Image
                      & "=" & Definition.Value.Image & "<");
    end if;
  end Define;

  -- Regex associated to
  -- Valid <name>{|[<name>]} or <name>{&[<name>]}
  Valid_Or, Valid_And : Reg_Exp.Compiled_Pattern;

  -- Check that a string of names ORed or ANDed matches a definition
  function Check (Names : As.U.Asu_Us; Ored : Boolean) return Boolean is
    Match : Boolean;
    Splitted : Many_Strings.Many_String;
    Name : As.U.Asu_Us;
    use type As.U.Asu_Us;
  begin
    -- Check syntax of Names
    if Ored then
      Match := Valid_Or.Match (Names.Image, Strict => True);
    else
      Match := Valid_And.Match (Names.Image, Strict => True);
    end if;
    if not Match then
      Error ("Invalid names " & Names.Image & " in directive at line"
           & Line_No'Img);
    end if;

    -- Split names
    Splitted := Str_Util.Split (Names.Image, (if Ored then '|' else '&'));

    -- Look for a match among the Definition names
    if Definitions.Is_Null then
      Logger.Log_Debug ("Check, no defintion => False");
      return False;
    end if;
    Found := False;
    for I in 1 .. Splitted.Nb loop
      Name := Splitted.Nth (I);
      for J in 1 .. Definitions.Length loop
        if Name = Definitions.Element(J).Name then
          -- Name matches a definition
          Logger.Log_Debug ("Check found "
                          & Definitions.Element(J).Name.Image
                          & " matching with " & Names.Image);
          return True;
        end if;
      end loop;
    end loop;
    -- All or no match at all
    Logger.Log_Debug ("Check no match found => False");
    return False;
  end Check;

  -- Value of a name
  function Value_Of (Name : As.U.Asu_Us) return String is
    use type As.U.Asu_Us;
  begin
    -- Look for a match among the Definition names
    for I in 1 .. Definitions.Length loop
      if Name = Definitions.Element(I).Name then
        return Definitions.Element(I).Value.Image;
      end if;
    end loop;
    -- No match found
    return "";
  end Value_Of;

  -- The regexes associated to the prefix and various keywords
  --------------
  Rdef, Rifdef, Rifnotdef, Relsifdef, Relsifnotdef, Relsedef, Rendifdef,
  Rrefdef, Rdefine : Reg_Exp.Compiled_Pattern;

  Refdef_Str : constant String := "RefDef";

  -- Compile a regex, with or without a name, with or without a value
  -- Report error
  procedure Compile (Pattern : in out Reg_Exp.Compiled_Pattern;
                     Keyword : in String;
                     Name : in Boolean;
                     Value : in Boolean := False) is
    Ok : Boolean;
  begin
    -- Opt spaces, then prefix and keyword
    --   space(s) then (name)
    --     (space(s) then (value))?
    -- Opt spaces
    Logger.Log_Debug ("Compiling "
      & "^[[:blank:]]*" & Prefix.Image & Keyword
      & (if Name then "[[:blank:]]+([^[:blank:]]+)" else "")
      & (if Value then "([[:blank:]]+([^[:blank:]].*))?" else "[[:blank:]]*")
      & "$");
    Pattern.Compile (Ok,
        "^[[:blank:]]*" & Prefix.Image & Keyword
      & (if Name then "[[:blank:]]+([^[:blank:]]+)" else "")
      & (if Value then "([[:blank:]]+([^[:blank:]].*))?" else "[[:blank:]]*")
      & "$");
    if not Ok then
      Error ("Regex " & Keyword & " does not compile: "
           & Reg_Exp.Error (Pattern));
    end if;
  end Compile;

  -- Check if Str strictly matches the Regex
  -- Store last name and last value if they are set
  Last_Name, Last_Value : As.U.Asu_Us;
  function Match (Pattern : Reg_Exp.Compiled_Pattern;
                  Str : As.U.Asu_Us) return Boolean is
    N : Natural;
    Info : Reg_Exp.Match_Array (1 .. 4);
    Result : Boolean;
  begin
    Reg_Exp.Exec (Pattern, Str.Image, N, Info);
    -- Check strict match
    Result := N >= 1
              and then Reg_Exp.Strict_Match (Str.Image, Info(1));
    -- Store name (first substring, info(2))
    if N >= 2 then
      Last_Name := Str.Uslice (Info(2).First_Offset, Info(2).Last_Offset_Stop);
    else
      Last_Name.Set_Null;
    end if;
    -- Store value (third substring, info(4))
    if N = 4 then
      Last_Value := Str.Uslice (Info(4).First_Offset, Info(4).Last_Offset_Stop);
    else
      Last_Value.Set_Null;
    end if;
    return Result;
  end Match;

  -- Input and output flows, current line
  In_Flow, Out_Flow : Text_Line.File_Type;

  -- Current line without trailing Lf
  Line : As.U.Asu_Us;

  -- Keep/skip current line
  Skip : Boolean;

  -- Stacks: Do we keep current text, have we already kept text for this level
  package Bool_Pool is new Unlimited_Pool (Boolean);
  Keep, Kept : Bool_Pool.Upool.Pool_Type;

  -- Stack of IfDef / IfNotDef
  package Long_Pool is new Unlimited_Pool (Long_Longs.Ll_Positive);
  Lines : Long_Pool.Upool.Pool_Type;

  -- Check that current level is not 0
  procedure Check_Level is
  begin
    if Keep.Length = 1 then
      Error ("Unexpected ""else*"" without ""if*"" at line " & Line_No'Img);
    end if;
  end Check_Level;

  -- No else* after a else
  Last_Else : Boolean;
  procedure Check_Else is
  begin
    if Last_Else then
      Error ("Unexpected ""else"" after ""else"" at line " & Line_No'Img);
    end if;
  end Check_Else;

begin
  Logger.Init ("App");

  -- Compile pattern of valid names
  if not Valid_Or.Compile (
      "[a-zA-Z](_?[a-zA-Z0-9])*(\|[a-zA-Z](_?[a-zA-Z0-9])*)*") then
    Error ("Failed to compile valid Or");
  end if;
  if not Valid_And.Compile (
      "[a-zA-Z](_?[a-zA-Z0-9])*(&[a-zA-Z](_?[a-zA-Z0-9])*)*") then
    Error ("Failed to compile valid Or");
  end if;

  -- Parse arguments
  Arg_Dscr := Argument_Parser.Parse (Keys);
  if not Arg_Dscr.Is_Ok then
    Error (Arg_Dscr.Get_Error);
    return;
  end if;
  -- Any definition must be after prefix
  if Arg_Dscr.Get_Nb_Embedded_Arguments /= 0 then
    Error ("Definitions must appear after prefix");
    return;
  end if;

  -- Help and version
  if Arg_Dscr.Is_Set (1) then
    Usage;
    return;
  elsif Arg_Dscr.Is_Set (2) then
    Basic_Proc.Put_Line_Output ("Version: " & Version);
    return;
  end if;

  -- Parse prefix
  if Arg_Dscr.Is_Set (3) then
    Prefix := As.U.Tus (Arg_Dscr.Get_Option (3));
  end if;
  Logger.Log_Debug ("Prefix is >" & Prefix.Image & "<");

  -- Parse definitions
  for I in 1 .. Arg_Dscr.Get_Nb_Occurences (No_Key_Index) loop
    Str := As.U.Tus (Arg_Dscr.Get_Option (No_Key_Index, I));
    Index := Str_Util.Locate (Str.Image, "=");
    case Index is
      when 0 =>
        -- "<name>"
        Definition.Name := Str;
        Definition.Value := As.U.Asu_Null;
      when 1 =>
        Error ("Invalid definition " & Str.Image);
      when others =>
        -- "<name>=[<value>]"
        Definition.Name := Str.Uslice (1, Index - 1);
        Definition.Value := Str.Uslice (Index + 1, Str.Length);
    end case;

    -- Check, then add or replace
    Define (Definition);

  end loop;

  -- Compile the Regexes
  -- A simple and general test for optimisation
  if not Rdef.Compile ("^[[:blank:]]*" & Prefix.Image & "[^[:blank:]].*$") then
    Error ("Failed to compile Rdef ");
  end if;
  -- Specific tests
  Compile (Rifdef, "IfDef", True);
  Compile (Rifnotdef, "IfNotDef", True);
  Compile (Relsifdef, "ElsifDef", True);
  Compile (Relsifnotdef, "ElsifNotDef", True);
  Compile (Relsedef, "ElseDef", False);
  Compile (Rendifdef, "EndifDef", False);
  Compile (Rrefdef, Refdef_Str, True);
  Compile (Rdefine, "Define", True, True);

  -- Open stdin and stdout flows
  In_Flow.Open_All (Text_Line.In_File);
  Out_Flow.Open_All (Text_Line.Out_File);

  -- Main loop
  Keep.Push (True);
  Line_No := 0;
  Last_Else := False;
  loop
    -- Get a line of input
    Str := In_Flow.Get;
    exit when Str.Is_Null;
    Line_No := Line_No + 1;
    -- Input line without trailing Lf
    Line := Str;
    Text_Line.Trim (Line);
    -- Keep line
    Skip := False;
    Logger.Log_Debug ("Processing line " & Line.Image);

    if Match (Rdef, Line) then
      -- Check if conditionnal change
      if Match (Rifdef, Line) then
        Logger.Log_Debug ("  Match IfDef " & Line.Image);
        -- Keep if already keeping at prev level and then check
        Keep.Push (Keep.Look and then Check (Last_Name, True));
        Kept.Push (Keep.Look);
        Lines.Push (Line_No);
        Last_Else := False;
        Skip := True;
      elsif Match (Rifnotdef, Line) then
        Logger.Log_Debug ("  Match IfNotDef " & Line.Image);
        -- Keep if already keeping at prev level and then check
        Keep.Push (Keep.Look and then not Check (Last_Name, False));
        Kept.Push (Keep.Look);
        Lines.Push (Line_No);
        Last_Else := False;
        Skip := True;
      elsif Match (Relsifdef, Line) then
        Logger.Log_Debug ("  Match ElsifDef " & Line.Image);
        Check_Level;
        Check_Else;
        -- Keep if already keeping at prev level and not yet at this level
        --  and then check
        Keep.Pop;
        Keep.Push (Keep.Look and then not Kept.Look
                   and then Check (Last_Name, True));
        Kept.Push (Kept.Pop or Keep.Look); --## rule line off Andor_Boolean
        Skip := True;
      elsif Match (Relsifnotdef, Line) then
        Logger.Log_Debug ("  Match ElsifNotDef " & Line.Image);
        Check_Level;
        Check_Else;
        -- Keep if already keeping at prev level and not yet at this level
        --  and then check
        Keep.Pop;
        Keep.Push (Keep.Look and then not Kept.Look
                   and then not Check (Last_Name, False));
        Kept.Push (Kept.Pop or Keep.Look); --## rule line off Andor_Boolean
        Skip := True;
      elsif Match (Relsedef, Line) then
        Logger.Log_Debug ("  Match ElseDef " & Line.Image);
        Check_Level;
        Check_Else;
        -- Keep if already keeping at prev level and not yet at this level
        Keep.Pop;
        Keep.Push (Keep.Look and then not Kept.Look);
        Last_Else := True;
        Skip := True;
      elsif Match (Rendifdef, Line) then
        Logger.Log_Debug ("  Match EndifDef " & Line.Image);
        Check_Level;
        Keep.Pop;
        Kept.Pop;
        Lines.Pop;
        Last_Else := False;
        Skip := True;
      elsif Keep.Look and then Match (Rrefdef, Line) then
        Logger.Log_Debug ("  Match RefDef " & Line.Image);
        if not Ada_Words.Is_Identifier (Last_Name.Image) then
          Error ("Invalid name " & Last_Name.Image & " at line " & Line_No'Img);
        end if;
        -- A reference: replace in Str
        Str := As.U.Tus (Str_Util.Regex.Substit (
          Str.Image,
          Prefix.Image & Refdef_Str & "[[:blank:]]+" & Last_Name.Image & ".*",
          Value_Of (Last_Name)));
        Logger.Log_Debug ("  Replaced by " & Text_Line.Trim (Str.Image));
      elsif Keep.Look and then Match (Rdefine, Line) then
        Logger.Log_Debug ("  Match Define " & Line.Image);
        if not Ada_Words.Is_Identifier (Last_Name.Image) then
          Error ("Invalid name " & Last_Name.Image & " at line " & Line_No'Img);
        end if;
        -- A definition, store
        Definition.Name := Last_Name;
        Definition.Value := Last_Value;
        Define (Definition);
        Skip := True;
      end if;
    end if;

    Logger.Log_Debug ("  After check, Keep is "
                    & Mixed_Str (Boolean'Image(Keep.Look))
                    & ", Skip is " & Mixed_Str (Skip'Img));
    if Keep.Look and then not Skip then
      -- Output line
      Out_Flow.Put (Str.Image);
    end if;
  end loop;

  -- Done
  Out_Flow.Close_All;
  In_Flow.Close_All;

exception
  when Raised_Error =>
    Basic_Proc.Set_Error_Exit_Code;
end App;

