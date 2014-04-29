with As.U.Utils;
with Regular_Expressions, Str_Util.Regex, Any_Def, Images, Trilean;
with Variables, Debug, Error;
package body Matcher is

  -- Expand Node.Text (maybe Check_Only)
  -- See if Str matches Node.Text: string comparison if not Node.Regex
  --  or Regular_Expressions.Exec)
  function Compute (Node : Tree.Node_Rec;
                    Str : As.U.Asu_Us;
                    Check_Only : Boolean) return Boolean is
    Expanding, Expanded, Result : As.U.Asu_Us;
    Compiled : Regular_Expressions.Compiled_Pattern;
    Ok : Boolean;
    N_Matched : Natural;
    Match_Info : Regular_Expressions.Match_Array (1 .. 10);
    use type Regular_Expressions.Match_Cell, Any_Def.Any_Kind_List,
             Tree.Node_Kind, Trilean.Trilean, As.U.Asu_Us;
  begin
    -- Case of the Eval/Set statement: One variable to assign
    if Node.Kind = Tree.Eval
    or else Node.Kind = Tree.Set then
      -- Expand variable name
      Expanding := Node.Assign(Node.Assign'First).Name;
      if Check_Only then
        Expanded := Variables.Expand (Expanding, Variables.Check_Only);
      else
        Expanded := Variables.Expand (Expanding, Variables.Local_Only);
      end if;
      if Check_Only then
        -- Check that content expands OK
        Result := Variables.Expand (Str, Variables.Check_Only);
      else
        -- Set variable
        if Node.Compute then
          Result := Variables.Compute (Str);
        else
          Result := Variables.Expand (Str, Variables.Local_Env);
          if Str_Util.Locate (Result.Image, "=") /= 0 then
            Error ("Invalid value in assignment " & Result.Image);
            raise Match_Error;
          end if;
        end if;
        Debug.Logger.Log_Debug ("Variable " & Expanded.Image
                              & " set to " & Result.Image);
        Variables.Set (Expanded, Result);
      end if;
      return True;
    end if;

    -- Other case of assign (chat/expect/read => tree kind Read)
    --  or Condif or Repeat
    -- Expand expression
    Expanding := Node.Critext;
    if Check_Only then
      Expanded := Variables.Expand (Expanding, Variables.Check_Only);
    else
      Expanded := Variables.Expand (Expanding, Variables.Local_Env);
    end if;

    -- Case of the Cond or Repeat: maybe fixed result if unset
    if Check_Only then
      Result := Variables.Expand (Str, Variables.Check_Only);
    elsif Node.Kind = Tree.Condif or else Node.Kind = Tree.Repeat then
      -- Expand variable name => Var
      Expanding := Variables.Expand (Str, Variables.Local_Only);
      if not Variables.Is_Set (Expanding) then
        -- Var is not set
        Error ("Unknown variable " & Expanding.Image & " in cond or repeat");
        raise Match_Error;
      else
        -- Var is set : Expand content of var
        Result := Variables.Expand ("${" & Expanding & "}",
                                    Variables.Local_Only);
      end if;
    else
      Result := Variables.Expand (Str, Variables.Local_Env);
    end if;

    if not Check_Only then
      Debug.Logger.Log_Debug ("Matching " & Result.Image
                            & " with " & Expanded.Image);
    end if;

    -- Pure string comparison or regexp?
    if not Node.Regexp then
      return Result = Expanded;
    end if;

    -- This is a Regexp, compile it
    Expanded := "^"  & Expanded & "$";
    Regular_Expressions.Compile (Compiled, Ok, Expanded.Image);
    if not Ok then
      Error ("Cannot compile regex " & Expanded.Image
        & " : " & Regular_Expressions.Error (Compiled));
      raise Match_Error;
    end if;
    Debug.Logger.Log_Debug ("Regex compiled " & Expanded.Image);

    -- Execute the regexp
    Regular_Expressions.Exec (Compiled, Result.Image, N_Matched, Match_Info);
    if Match_Info(1) = Regular_Expressions.No_Match then
      Debug.Logger.Log_Debug ("Regex no match");
      return False;
    end if;
    Debug.Logger.Log_Debug ("Regex match " & N_Matched'Img);

    -- Case of the Cond or Repeat: One variable to compare
    if Node.Kind = Tree.Condif
    or else Node.Kind = Tree.Repeat then
      return True;
    end if;

    -- Assign variables after real Match, if any
    if not Check_Only
    and then Node.Assign(1).Value.Kind /= Any_Def.None_Kind then
      -- Set volatile variables to matching substring
      for I in Match_Info'Range  loop
        -- ${0} is first match ...
        Expanding := As.U.Tus (Images.Integer_Image (I - 1));
        if I <= N_Matched
        and then Regular_Expressions.Valid_Match (Match_Info(I)) then
          Expanded := Result.Uslice (Match_Info(I).First_Offset,
                                     Match_Info(I).Last_Offset_Stop);
        else
          Expanded.Set_Null;
        end if;
        Variables.Set_Volatile (Expanding, Expanded);
        Debug.Logger.Log_Debug ("Volatile " & Expanding.Image
                              & "=" & Expanded.Image);
      end loop;
      -- Assign variables
      for I in Node.Assign'Range loop
        exit when Node.Assign(I).Value.Kind = Any_Def.None_Kind;
        Expanding := Node.Assign(I).Value.Str;
        Expanded := Variables.Expand (Expanding, Variables.Local_Env);
        if Str_Util.Locate (Expanded.Image, "=") /= 0 then
          Error ("Invalid value in assignment " & Expanded.Image);
          raise Match_Error;
        end if;
        Variables.Set (Node.Assign(I).Name, Expanded);
        Debug.Logger.Log_Debug ("Assigned " & Node.Assign(I).Name.Image
                              & "=" & Expanded.Image);
      end loop;
      Variables.Clear_Volatiles;
    end if;
    return True;
  exception
    when Variables.Expand_Error =>
      Error ("Cannot expand expression " & Node.Critext.Image);
      raise Match_Error;
  end Compute;

  procedure Parse_Assign (Node : in out Tree.Node_Rec;
                          I : in Tree.Assignments_Range;
                          Statement : in As.U.Asu_Us) is
    Index : Natural;
  begin
    Index := Str_Util.Locate (Statement.Image, "=");
    if Index <= 1 or else Index = Statement.Length then
      Error ("Invalid assignment " & Statement.Image);
      raise Match_Error;
    end if;
    -- Name <- the head (before the "=") and Value <- the tail
    Node.Assign(I) := (
       Name => Statement.Uslice (1, Index - 1),
       Value => (Kind => Any_Def.Str_Kind,
                 Str  => Statement.Uslice (Index + 1, Statement.Length)));
    -- Name cannot be "0", "1"...
    if Regular_Expressions.Match ("[0-9]+", Node.Assign(I).Name.Image,
                                  True) then
      Error ("Invalid variable name in assignment "
           & Node.Assign(I).Name.Image);
      raise Match_Error;
    end if;
    -- Check the value expands properly
    declare
      Expanded : As.U.Asu_Us;
      pragma Unreferenced (Expanded);
    begin
      Expanded := Variables.Expand (Node.Assign(I).Value.Str,
                                    Variables.Check_Only);
    exception
      when Variables.Expand_Error =>
        Error ("Invalid value in assignment "
             & Node.Assign(I).Value.Str.Image);
        raise Match_Error;
    end;
    -- Check that value does not contain refs > 9 ("\$\{[0-9][0-9]+\}")
    --  and does not contain "="
    if Regular_Expressions.Match ("(\$\{[0-9][0-9]+\}|=)",
            Node.Assign(I).Value.Str.Image, False) then
      Error ("Invalid value in assignment "
           & Node.Assign(I).Value.Str.Image);
      raise Match_Error;
    end if;
    Debug.Logger.Log_Debug ("Parsed assignement " & Node.Assign(I).Name.Image
                          & "=" & Node.Assign(I).Value.Str.Image);
  end Parse_Assign;

  -- Expand Node.Text in mode check
  -- Compile and test regex with a Dummy text
  -- Compute Node.Assign properties from Assign string
  procedure Check (Node : in out Tree.Node_Rec;
                   Assign : in As.U.Asu_Us) is
    Dummy : Boolean;
    Assign_Index : Positive;
    pragma Unreferenced (Dummy);
    use type Tree.Node_Kind;
  begin
    -- Check
    Dummy := Compute (Node, As.U.Tus ("Dummy"), True);

    -- Case of the Eval/Set/Cond/Repeat statement: One variable to assign
    if Node.Kind = Tree.Eval
    or else Node.Kind = Tree.Set
    or else Node.Kind = Tree.Condif
    or else Node.Kind = Tree.Repeat then
      -- Set it as first assign name
      Node.Assign(Node.Assign'First) := (
         Name => Assign,
         Value => (Kind => Any_Def.None_Kind) );
      return;
    end if;

    -- Other case of assign (chat/expect/read => tree kind Read)
    if Assign.Is_Null then
      return;
    end if;
    if not Node.Regexp then
      Error ("Assignment only allowed with regexp");
      raise Match_Error;
    end if;

    -- Check and compute Assign
    declare
      -- Split into assignments
      Statements : constant As.U.Utils.Asu_Array
                 := Str_Util.Regex.Split_Sep (Assign.Image, "[\n\t ]+");
    begin
      Debug.Logger.Log_Debug ("Found "
                            & Images.Integer_Image (Statements'Length)
                            & " assignments");
      if Statements'Length = 0 then
        -- No separator => Parse the whole Assign string
        Parse_Assign (Node, 1, Assign);
      else
        -- Parse each assignment
        Assign_Index := 1;
        for I in 1 .. Statements'Length loop
          if not Statements(I).Is_Null then
            -- Leading or trailing separator in assignments string
            --  lead to empty elements, to be skipped
            Parse_Assign (Node, Assign_Index, Statements(I));
            Assign_Index := Assign_Index + 1;
          end if;
        end loop;
      end if;
    end;
  end Check;

  -- Expand Node.Text
  -- See if Str matches Node.Text: string comparison if not Node.Regex
  --  or Regular_Expressions.Exec)
  function Match (Node : Tree.Node_Rec;
                  Str : As.U.Asu_Us) return Boolean is
  begin
    return Compute (Node, Str, False);
  end Match;

end Matcher;

