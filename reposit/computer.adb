-- Basic computation of a oper b...
-- Where oper is +, -, * or /,
--  a and b are integers or ${Variable}
-- Supports parentheses.
with Str_Util, Dynamic_List, Parser, Trace;
package body Computer is

  package Logger is new Trace.Basic_Logger ("Computer");
  procedure Trace (Msg : in String) renames Logger.Log_Debug;

  -- Operation on stored variables
  procedure Set (To : out Var_Rec; Val : in Var_Rec) is
  begin
    To := Val;
  end Set;
  function Image (Element : Var_Rec) return String is
    (Element.Name.Image);
  overriding function "=" (Current : Var_Rec;
                           Criteria : Var_Rec ) return Boolean is
    use type As.U.Asu_Us;
  begin
    return Current.Name = Criteria.Name;
  end "=";

  -- Variable management
  ----------------------
  -- Reset volatile or all variables
  procedure Reset (Memory : in out Memory_Type; Only_Volatile : in Boolean) is
    Vol_List : As.U.Asu_Us;
    -- Iterator to build list of names of volatile variables
    procedure List_Iter (Current : in Var_Rec;
                         Go_On   : in out Boolean) is
      pragma Unreferenced (Go_On);
    begin
      if not Current.Persistent then
        Vol_List.Append (Current.Name.Image & " ");
      end if;
    end List_Iter;
    Iter : Parser.Iterator;
    Var : Var_Rec;
  begin
    if not Only_Volatile then
      -- Delete all
      Trace ("Deleting all variables");
      Memory.Var_List.Delete_List;
      return;
    end if;
    -- Make list of names of volatile variables
    Memory.Var_List.Iterate (List_Iter'Access);
    -- Delete each volatile variable
    Iter.Set (Vol_List.Image, Parser.Is_Space_Or_Htab_Function'Access);
    loop
      declare
        Name : constant String := Iter.Next_Word;
      begin
        exit when Name = "";
        Var.Name := As.U.Tus (Name);
        Trace ("Deleting volatile " & Image (Var));
        Memory.Var_List.Delete (Var);
      end;
    end loop;
    Iter.Del;
  end Reset;

  -- Internal check of Name validity
  procedure Check_Name (Name : in String; Caller : in String) is
  begin
    if Name = "" then
      Trace (Caller & " raises Invalid_Variable on " & Name);
      raise Invalid_Variable;
    end if;
    for C of Name loop
      if Parser.Is_Space_Or_Htab_Function (C) then
        Trace (Caller & " raises Invalid_Variable on " & Name);
        raise Invalid_Variable;
      end if;
    end loop;
  end Check_Name;

  -- Set (store), maybe overwrite a variable
  procedure Set (Memory : in out Memory_Type;
                 Name : in String;
                 Value : in String;
                 Modifiable : in Boolean;
                 Persistent : in Boolean) is
    Var : Var_Rec;
    Found : Boolean;
  begin
    Check_Name (Name, "Set");
    -- Check if this variable exists (persistent or not) and is modifiable
    Var.Name := As.U.Tus (Name);
    Memory.Var_List.Search (Var, Found);
    if Found then
      Memory.Var_List.Read (Var);
      if not Var.Modifiable
      or else not Modifiable then
        -- One of the original or of the new (or both) is not modifiable
        Trace ("Set raises Constant_Exists on " & Name);
        raise Constant_Exists;
      end if;
    end if;
    -- Insert or overwrite modifiable variable, or insert new constant
    Var.Value := As.U.Tus (Value);
    Var.Persistent := Persistent;
    Var.Modifiable := Modifiable;
    Memory.Var_List.Insert (Var);
    Trace ("Inserted "
           & (if Persistent then "persistent " else "volatile ")
           & (if Modifiable then "variable " else "constant ")
           & Image (Var) & ", " & Value);
  end Set;

  -- Check if a variable is set
  function Is_Set (Memory : in out Memory_Type;
                   Name : in String) return Boolean is
    Crit : Var_Rec;
    Found : Boolean;
  begin
    Check_Name (Name, "Is_Set");
    Crit.Name := As.U.Tus (Name);
    Memory.Var_List.Search (Crit, Found);
    return Found;
  end Is_Set;

  -- Unset a variable
  procedure Unset (Memory : in out Memory_Type;
                   Name : in String) is
    Var : Var_Rec;
    Found : Boolean;
  begin
    Check_Name (Name, "Unset");
    -- Check that this variable exists (persistent or not) and is modifiable
    Var.Name := As.U.Tus (Name);
    Memory.Var_List.Search (Var, Found);
    if not Found then
      Trace ("Unset raises Unknown_Variable on " & Name);
      raise Unknown_Variable;
    end if;
    Memory.Var_List.Read (Var);
    if not Var.Modifiable then
      -- This is a constant
      Trace ("Unset raises Constant_Exists on " & Name);
      raise Constant_Exists;
    end if;
    Trace ("Deleting volatile " & Image (Var));
    Memory.Var_List.Delete (Var);
  end Unset;

  -- Read a variable rec (internal)
  -- May raise Unknown_Variable
  function Read (Memory : in out Memory_Type;
                 Name : in String) return Var_Rec is
    Found : Boolean;
    Res : Var_Rec;
  begin
    Check_Name (Name, "Read");
    -- First check if variable is volatile
    Res.Name := As.U.Tus (Name);
    Memory.Var_List.Search (Res, Found);
    if Found then
      Memory.Var_List.Read (Res);
      Trace ("Read " & Name & " => " & Res.Value.Image);
      return Res;
    else
      Trace ("Read raises Unknown_Variable on " & Name);
      raise Unknown_Variable;
    end if;
  end Read;

  -- Get a variable
  function Get (Memory : in out Memory_Type;
                Name : in String) return String is
    Var : Var_Rec;
  begin
    Trace ("Getting >" & Name & "<");
    Var := Read (Memory, Name);
    return Var.Value.Image;
  end Get;

   -- Get characteristics
  function Is_Modifiable (Memory : in out Memory_Type;
                          Name : in String) return Boolean is
    Var : Var_Rec;
  begin
    Var := Read (Memory, Name);
    return Var.Modifiable;
  end Is_Modifiable;

  function Is_Persistent (Memory : in out Memory_Type;
                          Name : in String) return Boolean is
    Var : Var_Rec;
  begin
    Var := Read (Memory, Name);
    return Var.Persistent;
  end Is_Persistent;


  -- External resolver of variables:
  procedure Set_External_Resolver (Memory : in out Memory_Type;
                                   Resolver : in Resolver_Access) is
  begin
    Memory.External_Resolver := Resolver;
  end Set_External_Resolver;


  -- Resolv variables of an expresssion
  function Internal_Eval (Memory : in out Memory_Type;
                          Expression : in String;
                          Check : in Boolean) return String is
    -- Get a variable, invokes external resolver if needed
    function Ext_Get (Name : String) return String is
    begin
      begin
        -- Get internal variable if set
        return Get (Memory, Name);
      exception
        when Unknown_Variable =>
          if Memory.External_Resolver = null then
            -- Variable is not set and no external resolver
            Trace ("Resolv raises Unknown_Variable on " & Name);
            raise;
          end if;
          -- Will go on trying external resolver
      end;
      begin
        return Memory.External_Resolver.all (Name);
      exception
        when others =>
          Trace ("Resolv raises Unknown_Variable on " & Name);
          raise Unknown_Variable;
      end;
    end Ext_Get;

    -- Get a variable, check if it is empty or contains spaces
    function Check_Get (Name : String) return String is
      Result : constant String := Ext_Get (Name);
    begin
      if Check and then
      (Result = "" or else Str_Util.Locate (Result, " ") /= 0) then
        Trace ("Check_Get raises Invalid_Expression on " & Name);
        raise Invalid_Expression;
      end if;
      return Result;
    end Check_Get;

  begin
    return Str_Util.Eval_Variables (
              Expression, "${", "}", Check_Get'Access,
              Muliple_Passes   => True,
              No_Check_Stop    => False,
              Skip_Backslashed => True);
  exception
    when Str_Util.Inv_Delimiter | Str_Util.Delimiter_Mismatch =>
      Trace ("Internal_Eval raises Invalid_Expression on " & Expression);
      raise Invalid_Expression;
  end Internal_Eval;

  function Eval (Memory : in out Memory_Type;
                 Expression : in String) return String is
    (Internal_Eval (Memory, Expression, Check => False));

  -- Fix expression
  function Fix (Memory : in out Memory_Type;
                Expression : in String) return String is
    Exp : As.U.Asu_Us;
  begin
    -- Replace each operator and parenthese 'op' by ' op '
    Exp := As.U.Tus (Expression);
    -- No space not Htab
    for C of Expression loop
      if Parser.Is_Space_Or_Htab_Function (C) then
        Trace ("Fix raises Invalid_Expression on " & Expression);
        raise Invalid_Expression;
      end if;
    end loop;
    -- Variables must not follow one each other (${var}${var})
    if Str_Util.Locate (Exp.Image, "}$") /= 0 then
        Trace ("Fix raises Invalid_Expression on " & Expression);
      raise Invalid_Expression;
    end if;
    -- Isolate variables
    Exp := As.U.Tus (Str_Util.Substit (Exp.Image, "${", " ${"));
    Exp := As.U.Tus (Str_Util.Substit (Exp.Image, "}", "} "));
    -- Expand all variables
    Exp := As.U.Tus (Internal_Eval (Memory, Exp.Image, Check => True));

    -- +X and -X will be analysed while parsing
    Exp := As.U.Tus (Str_Util.Substit (Exp.Image, "+", " +"));
    Exp := As.U.Tus (Str_Util.Substit (Exp.Image, "-", " -"));
    Exp := As.U.Tus (Str_Util.Substit (Exp.Image, "*", " * "));
    Exp := As.U.Tus (Str_Util.Substit (Exp.Image, "/", " / "));
    Exp := As.U.Tus (Str_Util.Substit (Exp.Image, "(", " ( "));
    Exp := As.U.Tus (Str_Util.Substit (Exp.Image, ")", " ) "));
    -- Replace each "  " by " "
    while Str_Util.Locate (Exp.Image, "  ") /= 0 loop
      Exp := As.U.Tus (Str_Util.Substit (Exp.Image, "  ", " "));
    end loop;
    Trace ("Fixed expression: " & Exp.Image);
    return Exp.Image;
  end Fix;

  -- List of members of parsed expression
  -- None is a pseudo value returned when no more member in list
  type Member_Kind_List is
   (Val, Add, Sub, Mult, Div, Open, Close, None);
  subtype Oper_Kind_List is Member_Kind_List range Add .. Div;
  subtype High_Kind_List is Oper_Kind_List range Mult .. Div;
  subtype Low_Kind_List is Oper_Kind_List range Add .. Sub;
  type Member_Rec (Kind : Member_Kind_List := Val) is record
    case Kind is
      when Val =>
        Value : Arbitrary.Number;
      when others =>
        null;
    end case;
  end record;
  package Members_List_Mng is new Dynamic_List (Member_Rec);
  package Members_Mng renames Members_List_Mng.Dyn_List;

  -- Parse the expression into a list of members
  procedure Parse (Members_List : in out Members_Mng.List_Type;
                   Exp : in String) is
    Iter : Parser.Iterator;
    -- Must +X or -X be binary (after a ')' or a Val)
    Must_Be_Binary : Boolean;
    First_Char : Character;
    Member : Member_Rec;
  begin
    Iter.Set (Exp, Parser.Is_Space_Or_Htab_Function'Access);
    -- Expression starting with +X means +X
    Must_Be_Binary := False;
    loop
      declare
        Word : constant String := Iter.Next_Word;
      begin
        exit when Word = "";
        First_Char :=  Word(Word'First);
        if Word'Length = 1 then
          case First_Char is
            when '+' =>
              Member := (Kind => Add);
              Must_Be_Binary := False;
            when '-' =>
              Member := (Kind => Sub);
              Must_Be_Binary := False;
            when '*' =>
              Member := (Kind => Mult);
              Must_Be_Binary := False;
            when '/' =>
              Member := (Kind => Div);
              Must_Be_Binary := False;
            when '(' =>
              Member := (Kind => Open);
              Must_Be_Binary := False;
            when ')' =>
              Member := (Kind => Close);
              Must_Be_Binary := True;
            when '0' .. '9' =>
              Must_Be_Binary := True;
              Member := (Kind => Val, Value => Arbitrary.Set (Word));
            when others =>
              raise Invalid_Expression;
          end case;
        elsif (First_Char = '+' or else First_Char = '-')
        and then Must_Be_Binary then
          -- Someting like Y -X or ) -X => parse -X as two members
          Members_List.Insert ( if First_Char = '+' then (Kind => Add)
                                else (Kind => Sub));
          Member := (Kind => Val,
                     Value => Arbitrary.Set (
                        Word (Positive'Succ(Word'First) .. Word'Last)));
          Must_Be_Binary := True;
        else
          Member := (Kind => Val, Value => Arbitrary.Set (Word));
          Must_Be_Binary := True;
        end if;
      end;
      Members_List.Insert (Member);
    end loop;
    Iter.Del;
    if not Members_List.Is_Empty then
      Members_List.Rewind;
    end if;
  exception
    when Constraint_Error =>
      if Iter.Is_Set then
        Iter.Del;
      end if;
      Trace ("Parse raises Invalid_Expression on " & Exp);
      raise Invalid_Expression;
  end Parse;

  -- Get member of list
  procedure Get_Member (Members_List : in out Members_Mng.List_Type;
                        End_Reached : in out Boolean;
                        Member : out Member_Rec) is
  begin
    if Members_List.Is_Empty then
      -- Members_List is empty
      raise Invalid_Expression;
    elsif End_Reached then
      -- End of expression is reached
      Member := (Kind => None);
    elsif Members_List.Check_Move then
      -- Next (not last) member
      Members_List.Read (Member);
    else
      -- Last member
      End_Reached := True;
      Members_List.Read (Member, Members_Mng.Current);
    end if;
  end Get_Member;

  -- Unget some members got
  procedure Unget_Member (Members_List : in out Members_Mng.List_Type;
                          End_Reached : in out Boolean) is
  begin
    if End_Reached then
      End_Reached := False;
    else
      Members_List.Move_To (Members_Mng.Prev);
    end if;
  end Unget_Member;

  -- One operation
  use type Arbitrary.Number;
  function Compute_One (I1 : Arbitrary.Number;
                        Op : Oper_Kind_List;
                        I2 : Arbitrary.Number) return Arbitrary.Number is
    (case Op is
       when Add  => I1 + I2,
       when Sub  => I1 - I2,
       when Mult => I1 * I2,
       when Div  => I1 / I2);

  -- Compute an expression
  -- Level is the level of parentheses (0 at startup)
  -- Higher_Prio is set when low prio followed by high prio
  --  (e.g. X + Y * Z) require new evaluation.
  --  The way to return then differs
  procedure Compute (Members_List : in out Members_Mng.List_Type;
                     End_Reached : in out Boolean;
                     Result : out Arbitrary.Number;
                     Level : in Natural;
                     Higher_Prio : in Boolean) is
    M1, M2, M3, M4 : Member_Rec;
    Tmp : Arbitrary.Number;
  begin
    if Level = 0 then
      End_Reached := False;
    end if;
    Get_Member (Members_List, End_Reached, M1);
    -- First member must be an int or a (
    if M1.Kind = Open then
      Trace ("Level++");
      Compute (Members_List, End_Reached, Result, Level + 1, False);
    elsif M1.Kind = Val then
      Result := M1.Value;
      Trace ("Value " & Result.Image);
    else
      Trace ("Invalid M1 " & M1.Kind'Img);
      raise Invalid_Expression;
    end if;

    loop
      -- M1 was a value, M2 must be none or close or an operation
      Get_Member (Members_List, End_Reached, M2);
      if M2.Kind = None then
        if Level = 0 then
          -- Done
          Trace ("The end");
          return;
        else
          -- Unexpected end
          Trace ("Unexpected end");
          raise Invalid_Expression;
        end if;
      elsif M2.Kind = Close then
        if Level = 0 then
          Trace ("Unmatched ')'");
          raise Invalid_Expression;
        else
          -- (X), return X
          Trace ("Level--");
          return;
        end if;
      elsif M2.Kind not in Oper_Kind_List then
        Trace ("Invalid M2 " & M2.Kind'Img);
        raise Invalid_Expression;
      else
        Trace ("Oper " & M2.Kind'Img);
      end if;

      -- We have a value and an oper
      -- Now we must have either a value, or an opening parenthese
      Get_Member (Members_List, End_Reached, M3);
      if M3.Kind = Open then
        Trace ("Level++");
        Compute (Members_List, End_Reached, Tmp, Level + 1, False);
      elsif M3.Kind /= Val then
        Trace ("Invalid M3 " & M3.Kind'Img);
        raise Invalid_Expression;
      else
        Tmp := M3.Value;
        Trace ("Value " & Tmp.Image);
      end if;

      -- Now we have a value, we need to see if it is followed
      --  by an operation of another level
      -- Here we must read an operation, a close or none
      Get_Member (Members_List, End_Reached, M4);
      if M4.Kind in Oper_Kind_List then
        if M4.Kind in High_Kind_List
        and then M2.Kind in Low_Kind_List then
          -- X + Y *, level increases due to prio,
          -- keep Y * for next level, unget this ope and value
          Unget_Member (Members_List, End_Reached);
          Unget_Member (Members_List, End_Reached);
          Trace ("Higher prio");
          Compute (Members_List, End_Reached, Tmp, Level, True);
          Result := Compute_One (Result, M2.Kind, Tmp);
        elsif M4.Kind in Low_Kind_List
        and then M2.Kind in High_Kind_List then
          -- X * Y +, compute X * Y,
          Unget_Member (Members_List, End_Reached);
          Result := Compute_One (Result, M2.Kind, Tmp);
          -- Level decreases if current is due to higher prio
          -- e.g. T + X * Y +
          if Higher_Prio then
            Trace ("End of higher prio");
            return;
          end if;
          -- Current level was at high prio and becomes
          -- at low prio
          Trace ("Lower prio");
          M2 := M4;
        else
          -- X * Y /, same level, keep /
          Unget_Member (Members_List, End_Reached);
          Trace ("Same prio");
          Result := Compute_One (Result, M2.Kind, Tmp);
        end if;
      elsif M4.Kind = Close then
        if Level = 0 then
          Trace ("Unmatched ')'");
          raise Invalid_Expression;
        end if;
        -- X * Y ), level decreases
        if Higher_Prio then
          -- T + X * Y ), let calling level process the Close
          Trace ("End of higher prio");
          Unget_Member (Members_List, End_Reached);
        end if;
        Trace ("Level--");
        Result := Compute_One (Result, M2.Kind, Tmp);
        return;
      elsif M4.Kind = None then
        -- End of expression
        if Level = 0 then
          -- Level decreases
          Trace ("The end");
          Result := Compute_One (Result, M2.Kind, Tmp);
          return;
        else
          -- Unexpected end
          Trace ("Unexpected end");
          raise Invalid_Expression;
        end if;
      end if;
    end loop;
  end Compute;

  -- Computation of expression
  function Compute (Memory : in out Memory_Type;
                    Expression : in String) return Arbitrary.Number is
    Result : Arbitrary.Number;
    Members_List : Members_Mng.List_Type;
    End_Reached : Boolean := False;
  begin
    -- Fix and Parse expression
    Parse (Members_List, Fix (Memory, Expression));
    -- Compute
    Compute (Members_List, End_Reached, Result, 0, False);
    -- Clean parsing
    Members_List.Delete_List;
    -- Done
    return Result;
  end Compute;

end Computer;

