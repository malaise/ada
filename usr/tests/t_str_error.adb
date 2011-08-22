with Ada.Exceptions;
with Basic_Proc, Sys_Calls, Normal, As.U, Regular_Expressions,
     String_Mng, Command, Many_Strings, Argument, Parser,
     Hashed_List.Unique, Unbounded_Arrays, Dynamic_List, Integer_Image;
procedure T_Str_Error is

  -- Command, flow and line read
  Cmd : Many_Strings.Many_String;
  Flow : Command.Flow_Rec(Command.List);
  Exit_Code : Command.Exit_Code_Range;
  Line : As.U.Asu_Us;

  -- Paterns to match and result of exec
  Patc, Pata : Regular_Expressions.Compiled_Pattern;
  Ok : Boolean;
  Nb_Match : Natural;
  Matches : Regular_Expressions.Match_Array (1 .. 3);
  Valid : Boolean;

  -- Max len of mnemonic
  Max_Len : constant := 15;

  -- Hash list of entries
  type Def_Rec is record
    Name : As.U.Asu_Us;
    Code : Natural;
    Alias_Of : As.U.Asu_Us;
  end record;
  Def, Alias : Def_Rec;
  type Def_Access is access all Def_Rec;
  procedure Set (To : out Def_Rec; Val : in Def_Rec) is
  begin
    To := Val;
  end Set;
  function "=" (Current : Def_Rec; Criteria : Def_Rec) return Boolean is
    use type As.U.Asu_Us;
  begin
    return Current.Name = Criteria.Name;
  end "=";
  function Key_Image (Element : Def_Rec) return String is
  begin
    return Element.Name.Image;
  end Key_Image;
  package Hash_Def_Mng is new Hashed_List (Def_Rec, Def_Access,
    Set, "=", Key_Image);
  package Unique_Def_Mng is new Hash_Def_Mng.Unique;
  Defs : Unique_Def_Mng.Unique_List_Type;

  -- For iterators
  Max_Search : constant := 100;
  Moved : Boolean;

  -- Aliases to solve
  type Def_Array is array (Positive range <>) of Def_Rec;
  package Unbounded_Defs is new Unbounded_Arrays (Def_Rec, Def_Array);
  Aliases : Unbounded_Defs.Unb_Array;

  -- List of codes and associated names
  type Code_Rec is record
    Code : Positive;
    Names : As.U.Asu_Us;
  end record;
  Code_Crit : Code_Rec;
  package Codes_Dyn_List_Mng is new Dynamic_List(Code_Rec);
  package Codes_List_Mng renames Codes_Dyn_List_Mng.Dyn_List;
  Codes : Codes_List_Mng.List_Type;
  function Less_Than (El1, El2 : Code_Rec) return Boolean is
  begin
    return El1.Code < El2.Code;
  end Less_Than;
  procedure Sort is new Codes_List_Mng.Sort (Less_Than);
  function Match (Current, Criteria : Code_Rec) return Boolean is
  begin
    return Current.Code = Criteria.Code;
  end Match;
  procedure Search is new Codes_List_Mng.Search (Match);
  Found : Boolean;


  -- Function to dump the Hash list
  procedure Put_Def (Def : Def_Rec; Show_Alias : in Boolean) is
  begin
    if Def.Alias_Of.Is_Null then
      -- A definition
      Basic_Proc.Put_Line_Output (
        Normal(Def.Code, 3) & " "
        & String_Mng.Procuste (Def.Name.Image, Max_Len)
        & " -> "
        & Sys_Calls.Str_Error(Def.Code) );
    elsif Show_Alias then
      -- An alias to dump
      Basic_Proc.Put_Line_Output ("    "
         & String_Mng.Procuste (Def.Name.Image, Max_Len)
         & " = "
         & Def.Alias_Of.Image);
    else
      -- An alias to put after its definition
      Basic_Proc.Put_Line_Output ("  = " & Def.Name.Image);
    end if;
  end Put_Def;
  procedure Dump_Iterator (Current : in Def_Rec;
                           Go_On   : in out Boolean) is
    pragma Unreferenced (Go_On);
  begin
    Put_Def (Current, True);
  end Dump_Iterator;

  procedure Put_Code (Code : in Code_Rec) is
    Iterator : Parser.Iterator;
    Def : Def_Rec;
  begin
      Def.Code := Code.Code;
      Def.Alias_Of.Set_Null;
      -- Split Names
      Iterator.Set (Code.Names.Image);
      loop
        Def.Name := As.U.Tus (Iterator.Next_Word);
        exit when Def.Name.Is_Null;
        Put_Def (Def, False);
        -- All but first name are aliases of it
        if Def.Alias_Of.Is_Null then
          Def.Alias_Of := Def.Name;
        end if;
      end loop;
  end Put_Code;

begin

  -- Help
  if Argument.Get_Nbre_Arg = 1
  and then (Argument.Get_Parameter = "-h"
    or else Argument.Get_Parameter = "--help") then
    Basic_Proc.Put_Line_Output ("Usage: " & Argument.Get_Program_Name
        & " [ <help> | <dump> | <list> | { <code> | <menmonic> } ]");
    Basic_Proc.Put_Line_Output (" <help> ::= -h | --help    This help message");
    Basic_Proc.Put_Line_Output (" <dump> ::= -d | --dump    Dump definitions");
    Basic_Proc.Put_Line_Output (" <list> ::= -l | --list    List definitions"
       & " (default)");
    Basic_Proc.Set_Ok_Exit_Code;
    return;
  end if;

  -- Launch command
  Cmd.Set ("cpp"); Cmd.Cat ("-dD"); Cmd.Cat ("/usr/include/errno.h");
  begin
    Command.Execute (Cmd, True, Command.Only_Out,
                     Flow'Unrestricted_Access, null, Exit_Code);
  exception
    when Command.Terminate_Request =>
      Basic_Proc.Put_Line_Error ("ERROR, command aborted");
      Basic_Proc.Set_Error_Exit_Code;
      return;
    when Command.Spawn_Error =>
      Basic_Proc.Put_Line_Error ("ERROR launching command: " & Cmd.Image);
      Basic_Proc.Set_Error_Exit_Code;
      return;
  end;
  if Exit_Code /= Basic_Proc.Exit_Code_Ok then
    Basic_Proc.Put_Line_Error ("ERROR, command: " & Cmd.Image
                 & " has failed with code" & Exit_Code'Img);
    Basic_Proc.Set_Error_Exit_Code;
    return;
  end if;
  if Flow.List.Is_Empty then
    Basic_Proc.Put_Line_Error ("ERROR, command: " & Cmd.Image
                 & " has no output");
    Basic_Proc.Set_Error_Exit_Code;
    return;
  end if;

  -- Compile regexps for expected texts
  -- #define mnemonic code
  Regular_Expressions.Compile (Patc, Ok,
     "^#define[[:blank:]]+(E[A-Z]+)[[:blank:]]+([0-9]+)[[:blank:]]*$");
  if not Ok then
    Basic_Proc.Put_Line_Error ("ERROR, invalid regex: "
          & Regular_Expressions.Error (Patc) & '.');
    Basic_Proc.Set_Error_Exit_Code;
    return;
  end if;
  -- #define mnemonic mnemonic
  Regular_Expressions.Compile (Pata, Ok,
    "^#define[[:blank:]]+(E[A-Z]+)[[:blank:]]+(E[A-Z]+)[[:blank:]]*$");
  if not Ok then
    Basic_Proc.Put_Line_Error ("ERROR, invalid regex: "
          & Regular_Expressions.Error (Pata) & '.');
    Basic_Proc.Set_Error_Exit_Code;
    return;
  end if;

  -- Read command output, store definitions and aliases in the Unique list
  Flow.List.Rewind;
  for I in 1 .. Flow.List.List_Length loop
    Flow.List.Get (Line);
    Regular_Expressions.Exec (Patc, Line.Image, Nb_Match, Matches);
    if Nb_Match = 3 then
      -- Valid line "#define mnemo code"
      begin
        Def.Code := Positive'Value (Line.Slice (Matches(3).First_Offset,
                                                Matches(3).Last_Offset_Stop));
        Valid := True;
      exception
        when Constraint_Error =>
          Valid := False;
      end;
      if Valid then
        -- Insert Mnemo and Code
        Def.Name := Line.Uslice (Matches(2).First_Offset,
                                 Matches(2).Last_Offset_Stop);
        Def.Alias_Of.Set_Null;
        Defs.Insert (Def);
      end if;
    else
      Regular_Expressions.Exec (Pata, Line.Image, Nb_Match, Matches);
      if  Nb_Match = 3 then
        -- Valid line "#define mnemo mnemo"
        -- Insert alias definition if it does not overwrite a code definition
        Def.Code := 0;
        Def.Name := Line.Uslice (Matches(2).First_Offset,
                                 Matches(2).Last_Offset_Stop);
        Def.Alias_Of := Line.Uslice (Matches(3).First_Offset,
                                     Matches(3).Last_Offset_Stop);
        Defs.Insert_If_New (Def);
        Aliases.Append (Def);
      end if;
    end if;

  end loop;
  if Defs.Is_Empty then
    Basic_Proc.Put_Line_Error ("ERROR, empty list");
    Basic_Proc.Set_Error_Exit_Code;
    return;
  end if;

  -- Dump raw result of parsing
  if Argument.Get_Nbre_Arg = 1
  and then (Argument.Get_Parameter = "-d"
    or else Argument.Get_Parameter = "--dump") then
    -- Dump Hash list and array of aliases
    Defs.Iterate (Dump_Iterator'Access);
    Basic_Proc.Set_Ok_Exit_Code;
    return;
  end if;

  -- Resolve aliases, update resolved aliases in list
  for I in 1 .. Aliases.Length loop
    Def := Aliases.Element (I);
    Defs.Read (Def);
    -- Resolve aliases until a code is set
    Alias := Def;
    for I in 1 .. Max_Search loop
      exit when Alias.Code /= 0;
      -- Check (recursively) if this Alias_Of is defined
      Alias.Name := Def.Alias_Of;
      Defs.Search (Alias, Found);
      if not Found then
        -- This Alias_Of is not defined
        Basic_Proc.Put_Line_Error (
          "ERROR, cannot resolve alias " & Alias.Name.Image);
        Basic_Proc.Set_Error_Exit_Code;
        return;
      end if;
      Defs.Read_Current (Alias);
    end loop;

    if Alias.Code = 0 then
      -- Not found after Max_Search loops
      Basic_Proc.Put_Line_Error (
        "ERROR, infinite loop while resolving alias " & Def.Name.Image);
      Basic_Proc.Set_Error_Exit_Code;
      return;
    end if;
    -- Found, insert in list
    Def.Code := Alias.Code;
    Def.Alias_Of.Set_Null;
    Defs.Insert (Def);
  end loop;
  Aliases.Set_Null;

  -- Make a list of codes with names, sorted by codes
  Defs.Rewind;
  loop
    -- Got a definition or an alias
    Defs.Read_Next (Def, Moved);
    Code_Crit.Code := Def.Code;
    -- Read code entry if one exists in Codes
    Search (Codes, Found, Code_Crit, From => Codes_List_Mng.Absolute);
    if Found then
      Codes.Read (Code_Crit, Codes_List_Mng.Current);
    else
      Code_Crit.Names.Set_Null;
    end if;
    -- If this a definition then prepend name, else append it
    -- So Names contains the name then the aliases
    -- There might be extra leading, tailing and intermediate spaces
    --  but never mind, Names will be parsed with Parser
    if Def.Alias_Of.Is_Null then
      Code_Crit.Names.Prepend (Def.Name.Image & " ");
    else
      Code_Crit.Names.Append (" " & Def.Name.Image);
    end if;
    -- And store
    if Found then
      Codes.Modify (Code_Crit, Codes_List_Mng.Current);
    else
      Codes.Insert (Code_Crit);
    end if;
    exit when not Moved;
  end loop;
  -- And sort
  Sort (Codes);

  -- Put the list
  if Argument.Get_Nbre_Arg = 0
  or else (Argument.Get_Nbre_Arg = 1
    and then (Argument.Get_Parameter = "-l"
      or else Argument.Get_Parameter = "--list") ) then
    -- Dump sorted list of codes
    Codes.Rewind;
    loop
      Codes.Read (Code_Crit, Moved => Moved);
      -- Put the different mnemonics for this code
      Put_Code (Code_Crit);
      exit when not Moved;
    end loop;
    Basic_Proc.Set_Ok_Exit_Code;
    return;
  end if;

  -- Process arguments
  for I in 1 .. Argument.Get_Nbre_Arg loop
    Argument.Get_Parameter (Line, Occurence => I);
    if Regular_Expressions.Match ("E[A-Z]*", Line.Image, True) then
      -- A mnemonic, read its definition
      Def.Name := Line;
      Defs.Search (Def, Found);
      if Found then
        Defs.Read_Current (Def);
        Code_Crit.Code := Def.Code;
      else
        Basic_Proc.Put_Line_Output (Line.Image & " => Mnemonic not found");
      end if;
    elsif Regular_Expressions.Match ("[1-9][0-9]*", Line.Image, True) then
      -- A code, read it
      Def.Name.Set_Null;
      begin
        Code_Crit.Code := Positive'Value (Line.Image);
        Found := True;
      exception
        when Constraint_Error =>
          -- 'Value failed
          Basic_Proc.Put_Line_Output (Line.Image & " => Invalid code");
          Found := False;
          Basic_Proc.Set_Error_Exit_Code;
      end;
    else
      Basic_Proc.Put_Line_Output (Line.Image & " => Invalid argument");
      Found := False;
      Basic_Proc.Set_Error_Exit_Code;
    end if;
    if Found then
      -- At this point Code_Crit has the code to search
      Search (Codes, Found, Code_Crit, From => Codes_List_Mng.Absolute);
      if Found then
        Codes.Read (Code_Crit, Codes_List_Mng.Current);
        -- Put the different mnemonics for this code
        Put_Code (Code_Crit);
      else
        if not Def.Name.Is_Null then
          -- Mnemonic
          Basic_Proc.Put_Output (Line.Image & " => ");
        end if;
        -- Mnemonic or code
        Basic_Proc.Put_Line_Output (Integer_Image (Code_Crit.Code)
                                  & " => Code not found");
        Basic_Proc.Set_Error_Exit_Code;
      end if;
    end if;
  end loop;

exception
  when Error:others =>
    Basic_Proc.Put_Line_Error ("ERROR, exception: "
        & Ada.Exceptions.Exception_Name (Error) & " raised");
    Basic_Proc.Set_Error_Exit_Code;
end T_Str_Error;

