with As.U.Utils;
with Regular_Expressions, Str_Util.Regex, Any_Def, Images, Mixed_Str;
with Variables, Debug, Error;
package body Matcher is

  function Expand (Txt  : As.U.Asu_Us;
                   Mode : Tree.Eval_List) return As.U.Asu_Us is
  begin
    return (case Mode is
          when Tree.None    => Txt,
          when Tree.Resolve => Variables.Expand (Txt, Variables.Local_Env),
          when Tree.Compute =>
            Variables.Compute (Variables.Expand (Txt, Variables.Local_Env)));
  end Expand;

  -- If Node.Eval is Resolve or Compute, then
  -- - Expand Node.Critext. If Compute and Node.Oper is not Match nor Notmatch
  --   then compute it
  -- - If Node.Kind leads to define Node.Expression
  --   (Condif | Repeat | Parse | Set | Eval), then expand or compute it
  --   otherwide do the same for Str
  -- See if Critext matches the expression: string comparison, Regex match
  --  or integer comparison
  -- Assign variables
  function Compute (Node : Tree.Node_Rec;
                    Str : As.U.Asu_Us;
                    Check_Only : Boolean) return Boolean is
    Expanding, Expanded, Result : As.U.Asu_Us;
    Compiled : Regular_Expressions.Compiled_Pattern;
    Ok : Boolean;
    N_Matched : Natural;
    Match_Info : Regular_Expressions.Match_Array (1 .. 10);
    Assign : Boolean;
    V1, V2 : Integer;
    use type Regular_Expressions.Match_Cell, Any_Def.Any_Kind_List,
             Tree.Node_Kind, Tree.Oper_List, Tree.Eval_List, As.U.Asu_Us;
  begin

    case Node.Kind is
      when Tree.Eval | Tree.Set =>
        -- Eval or Set statement: One variable to assign to Str or Critext
        -- Expand Variable name and do None/Resolve/Compute of expression
        -- Expand variable name (check or local vars only)
        Expanded := Variables.Expand (Node.Expression,
          (if Check_Only then Variables.Check_Only else Variables.Local_Only));
        -- None/Resolve/Compute Str or Critext
        Expanding := (if Node.Kind = Tree.Eval then Str else Node.Critext);
        if Check_Only and then Node.Eval /= Tree.None then
          -- Check that content expands OK
          Result := Variables.Expand (Expanding, Variables.Check_Only);
        elsif not Check_Only then
          -- Set value
          Result := Expand (Expanding, Node.Eval);
        end if;
        Variables.Set (Expanded, Result);
        Debug.Logger.Log_Debug ("Variable >" & Expanded.Image
                              & "< set to >" & Result.Image & "<");
        -- Done
        return True;

      when Tree.Expect | Tree.Condif | Tree.Repeat | Tree.Read | Tree.Parse =>
        -- Other case of match: chat/expect/if/elsif/while/read/parse
        -- Check/Expand Critext
        Expanding := Node.Critext;
        if Check_Only and then Node.Eval /= Tree.None then
          Expanded := Variables.Expand (Node.Critext, Variables.Check_Only);
        elsif not Check_Only then
          Expanded := Expand (Node.Critext, Node.Eval);
        end if;
        if Check_Only then
          -- Check Expr
          if Node.Eval /= Tree.None
          and then Node.Kind /= Tree.Expect and then Node.Kind /= Tree.Read then
            -- if/elsif/while/parse: check Expr
            Result := Variables.Expand (Node.Expression, Variables.Check_Only);
          end if;
          return True;
        end if;
        Expanding :=
          (if Node.Kind = Tree.Expect or else Node.Kind = Tree.Read then Str
           else Node.Expression);
        -- Var is set : Expand content of var
        Result := Expand (Expanding, Node.Eval);
      when others =>
        -- No expansion
        null;
    end case;

    -- Now we check if Result  matches Expanded
    Debug.Logger.Log_Debug ("Matching >" & Result.Image
                            & "< oper " & Mixed_Str (Node.Oper'Img)
                            & " with >" & Expanded.Image & "<");

    -- Pure string comparison or regexp?
    Assign := False;
    case Node.Eval is
      when Tree.None =>
        -- No assignement possible, so we have done
        return (if Node.Oper = Tree.Equal then Result = Expanded
                else Result /= Expanded);
      when Tree.Resolve =>
        if Node.Oper = Tree.Equal then
          Ok := Result = Expanded;
        elsif Node.Oper = Tree.Noteq then
          Ok := Result /= Expanded;
        else
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
          Regular_Expressions.Exec (Compiled, Result.Image, N_Matched,
                                    Match_Info);
          if Match_Info(1) /= Regular_Expressions.No_Match then
            Debug.Logger.Log_Debug ("Regex match " & N_Matched'Img);
            Ok := Node.Oper = Tree.Match;
            Assign := True;
          else
            Debug.Logger.Log_Debug ("Regex no match");
            Ok := Node.Oper = Tree.Notmatch;
          end if;
       end if;

      when Tree.Compute =>
        V1 := Integer'Value (Result.Image);
        V2 := Integer'Value (Expanded.Image);
        Ok := (case Node.Oper is
          when Tree.Equal     => V1 =  V2,
          when Tree.Noteq     => V1 /= V2,
          when Tree.Greater   => V1 >  V2,
          when Tree.Less      => V1 <  V2,
          when Tree.Greatereq => V1 >= V2,
          when Tree.Lesseq    => V1 <= V2,
          -- No match operation when Compute
          when others => False);
    end case;

    -- No Assign when not Ok or Condif or Repeat or when Oper /= Match
    if not Ok
    or else not Assign
    or else Node.Kind = Tree.Condif
    or else Node.Kind = Tree.Repeat
    or else Node.Oper /= Tree.Match
    or else Node.Assign(1).Value.Kind = Any_Def.None_Kind then
      return Ok;
    end if;

    -- Set volatile variables to matching substring of Result
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
      Debug.Logger.Log_Debug ("Assigned >" & Node.Assign(I).Name.Image
                            & "< = >" & Expanded.Image & "<");
    end loop;
    Variables.Clear_Volatiles;
    return Ok;
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
    -- Name must match varable construction (Ada) rules
    if not Regular_Expressions.Match (
      "[A-Za-z](_?[A-Za-z0-9])*", Node.Assign(I).Name.Image, True) then
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

  -- If Node.Eval is Resolve or Compute, then
  -- - Expand Node.Critext in mode check
  -- - If Node.Kind leads to define Node.Expression
  --   (Condif | Repeat | Parse | Set | Eval),
  --    then expand it in mode check
  -- Compute Node.Assign properties from Assign string
  procedure Check (Node : in out Tree.Node_Rec;
                   Assign : in As.U.Asu_Us) is
    Result : Boolean;
    Assign_Index : Positive;
    use type Tree.Node_Kind, Tree.Oper_List, Tree.Eval_List;
  begin
    -- Check Oper versus Eval
    Result := (case Node.Eval is
      -- Only Equal or Notequal
      when Tree.None => Node.Oper <= Tree.Noteq,
      -- Equal or Notequal or Match or Notmatch
      when Tree.Resolve => Node.Oper <= Tree.Notmatch,
      -- Equal or Notequal or Greater...
      when Tree.Compute =>
        (Node.Oper <= Tree.Noteq) or else (Node.Oper >= Tree.Greater));
    if not Result then
      Error ("Operation " & Mixed_Str (Node.Oper'Img)
           & " incompatible with Evaluation " & Mixed_Str (Node.Eval'Img));
    end if;

    -- Assign requires Match
    if not Assign.Is_Null and then Node.Oper /= Tree.Match then
      Error ("Assignment " & Assign.Image & " requires Match operation, got "
           & Mixed_Str (Node.Oper'Img));
    end if;

    -- Check
    Result := Compute (Node, As.U.Tus ("Dummy"), True);

    -- Other case of assign (chat/expect/read => tree kind Read)
    if Assign.Is_Null then
      return;
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
      elsif Statements'Length > Tree.Max_Assignments then
        Error ("Too many assignments");
        raise Match_Error;
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

  -- Do the real test
  function Match (Node : Tree.Node_Rec;
                  Str : As.U.Asu_Us := As.U.Asu_Null) return Boolean is
  begin
    return Compute (Node, Str, False);
  end Match;

end Matcher;

