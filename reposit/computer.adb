-- Basic computation of a oper b...
-- Where oper is +, -, * or /,
--  a and b are integers or ${Variable}
-- Supports parentheses.
with As.U, Environ, Basic_Proc, Hashed_List.Unique,
     String_Mng, Dynamic_List, Parser;
package body Computer is

  Debug_Read : Boolean := False;
  Debug_On : Boolean := False;
  procedure Trace (Msg : in String) is
  begin
    if not Debug_Read then
      Debug_On := Environ.Is_Yes ("COMPUTER_DEBUG");
      Debug_Read := True;
    end if;
    if Debug_On then
      Basic_Proc.Put_Line_Error ("COMP:  " & Msg);
    end if;
  end Trace;

  -- List of variables
  type Var_Rec is record
    -- Variable name
    Name : As.U.Asu_Us;
    -- Variable value
    Value : As.U.Asu_Us;
    -- Persistent:
    -- If modifiable, at most one persistent and one volatile variables
    --  of same name,
    -- If not modifiable, either one persistent or one volatile
    Persistent : Boolean;
    -- Modifiable
    Modifiable : Boolean;
  end record;
  type Var_Access is access all Var_Rec;
  procedure Set (To : out Var_Rec; Val : in Var_Rec) is
  begin
    To := Val;
  end Set;
  function Image (Element : Var_Rec) return String is
  begin
    if Element.Persistent then
      return "P " & Element.Name.Image;
    else
      return "V " & Element.Name.Image;
    end if;
  end Image;
  function "=" (Current : Var_Rec ; Criteria : Var_Rec ) return Boolean is
    use type As.U.Asu_Us;
  begin
    return Current.Persistent = Criteria.Persistent
    and then Current.Name = Criteria.Name;
  end "=";
  package Var_List_Mng is new Hashed_List (Var_Rec, Var_Access,
                                           Set, "=", Image);
  package Var_Mng is new Var_List_Mng.Unique;
  Var_List : Var_Mng.Unique_List_Type;

  -- Variable management
  ----------------------
  -- Reset all variables
  procedure Reset (Not_Persistent : in Boolean) is
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
    if not Not_Persistent then
      -- Delete all
      Trace ("Deleting all variables");
      Var_List.Delete_List;
      return;
    end if;
    -- Make list of names of volatile variables
    Var_List.Iterate (List_Iter'Access);
    -- Delete each volatile variable
    Iter.Set (Vol_List.Image, Parser.Is_Space_Or_Htab_Function'Access);
    loop
      declare
        Name : constant String := Iter.Next_Word;
      begin
        exit when Name = "";
        Var.Name := As.U.Tus (Name);
        Var.Persistent := False;
        Trace ("Deleting volatile " & Image (Var));
        Var_List.Delete (Var);
      end;
    end loop;
    Iter.Del;
  end Reset;

  -- Set (store), maybe overwrite a variable
  procedure Set (Name : in String;
                 Value : in String;
                 Modifiable : in Boolean;
                 Persistent : in Boolean) is
    Var : Var_Rec;
    Found : Boolean;
  begin
    if Name = "" then
      raise Invalid_Variable;
    end if;
    -- Check if this variable exists (persistent or not) and is modifiable
    Var.Name := As.U.Tus (Name);
    Var.Persistent := False;
    Var_List.Search (Var, Found);
    if Found then
      Var_List.Read (Var);
      if not Var.Modifiable
      or else not Modifiable then
        -- One of the original and of the new (or both)
        --  is not modifiable
        raise Constant_Exists;
      end if;
    end if;
    Var.Persistent := True;
    Var_List.Search (Var, Found);
    if Found then
      Var_List.Read (Var);
      if not Var.Modifiable
      or else not Modifiable then
        -- One of the original and of the new (or both)
        --  is not modifiable
        raise Constant_Exists;
      end if;
    end if;
    -- Insert or overwrite modifiable variable, or insert new constant
    Var.Persistent := Persistent;
    Var.Value := As.U.Tus (Value);
    Var.Modifiable := Modifiable;
    Var_List.Insert (Var);
    if Modifiable then
      Trace ("Inserted variable " & Image (Var) & ", " & Value);
    else
      Trace ("Inserted constant " & Image (Var) & ", " & Value);
    end if;
  end Set;

  -- Check if a variable is set
  function Is_Set (Name : String) return Boolean is
    Crit : Var_Rec;
    Found : Boolean;
  begin
    if Name = "" then
      raise Invalid_Variable;
    end if;
    -- First check if variable is volatile
    Crit.Name := As.U.Tus (Name);
    Crit.Persistent := False;
    Var_List.Search (Crit, Found);
    if Found then
      return True;
    end if;
    -- Then check if it is persistent
    Crit.Persistent := True;
    Var_List.Search (Crit, Found);
    return Found;
  end Is_Set;

  -- Read a variable rec (internal)
  -- May raise Unknown_Variable
  function Read (Name : String) return Var_Rec is
    Found : Boolean;
    Res : Var_Rec;
  begin
    Trace ("Reading >" & Name & "<");
    if Name = "" then
      raise Invalid_Variable;
    end if;
    -- First check if variable is volatile
    Res.Name := As.U.Tus (Name);
    Res.Persistent := False;
    Var_List.Search (Res, Found);
    if Found then
      Var_List.Read (Res);
      Trace ("Read >" & Res.Value.Image & "<");
      return Res;
    end if;
    -- Then try to read this variable as persistent
    Res.Persistent := True;
    Var_List.Read (Res);
    Trace ("Read >" & Res.Value.Image & "<");
    return Res;
  exception
    when Var_Mng.Not_In_List =>
      raise Unknown_Variable;
  end Read;

  -- Get a variable
  function Get (Name : String) return String is
    Var : Var_Rec;
  begin
    Trace ("Getting >" & Name & "<");
    Var := Read (Name);
    return Var.Value.Image;
  end Get;

   -- Get characteristics
  function Is_Modifiable (Name : String) return Boolean is
    Var : Var_Rec;
  begin
    Var := Read (Name);
    return Var.Modifiable;
  end Is_Modifiable;

  function Is_Persistent (Name : String) return Boolean is
    Var : Var_Rec;
  begin
    Var := Read (Name);
    return Var.Persistent;
  end Is_Persistent;


  -- Get a variable, invokes external resolver if needed
  function Ext_Get (Name : String) return String is
  begin
    begin
      -- Get internal variable if set
      return Get (Name);
    exception
      when Unknown_Variable =>
        if External_Resolver = null then
          -- Variable is not set and no external resolver
          raise;
        end if;
        -- Will go on trying external resolver
    end;
    begin
      return External_Resolver.all (Name);
    exception
      when others =>
        raise Unknown_Variable;
    end;
  end Ext_Get;

  -- Resolv variables of an expresssion
  function Eval (Expression : String) return String is
  begin
    return String_Mng.Eval_Variables (
              Expression, "${", "}", Ext_Get'Access,
              Muliple_Passes   => True,
              No_Check_Stop    => False,
              Skip_Backslashed => True);
  exception
    when String_Mng.Inv_Delimiter | String_Mng.Delimiter_Mismatch =>
      raise Invalid_Expression;
  end Eval;

  -- Fix expression
  function Fix (Expression : String) return String is
    Exp : As.U.Asu_Us;
  begin
    -- Replace each operator and parenthese 'op' by ' op '
    Exp := As.U.Tus (Expression);
    -- No space not Htab
    for I in Expression'Range loop
      if Parser.Is_Space_Or_Htab_Function (Expression(I)) then
        raise Invalid_Expression;
      end if;
    end loop;
    -- Variables must not follow one each other (${var}${var})
    if String_Mng.Locate (Exp.Image, "}$") /= 0 then
      raise Invalid_Expression;
    end if;
    -- Isolate variables
    Exp := As.U.Tus (String_Mng.Replace (Exp.Image, "${", " ${"));
    Exp := As.U.Tus (String_Mng.Replace (Exp.Image, "}", "} "));
    -- Expand variables
    Exp := As.U.Tus (Eval (Exp.Image));

    -- +X and -X will be analysed while parsing
    Exp := As.U.Tus (String_Mng.Replace (Exp.Image, "+", " +"));
    Exp := As.U.Tus (String_Mng.Replace (Exp.Image, "-", " -"));
    Exp := As.U.Tus (String_Mng.Replace (Exp.Image, "*", " * "));
    Exp := As.U.Tus (String_Mng.Replace (Exp.Image, "/", " / "));
    Exp := As.U.Tus (String_Mng.Replace (Exp.Image, "(", " ( "));
    Exp := As.U.Tus (String_Mng.Replace (Exp.Image, ")", " ) "));
    -- Replace each "  " by " "
    while String_Mng.Locate (Exp.Image, "  ") /= 0 loop
      Exp := As.U.Tus (String_Mng.Replace (Exp.Image, "  ", " "));
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
        Value : Integer;
      when others =>
        null;
    end case;
  end record;
  package Members_List_Mng is new Dynamic_List (Member_Rec);
  package Members_Mng renames Members_List_Mng.Dyn_List;
  Members_List : Members_Mng.List_Type;

  -- Parse the expression into a list of members
  procedure Parse (Exp : in String) is
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
              Member := (Kind => Val, Value => Integer'Value (Word));
            when others =>
              raise Invalid_Expression;
          end case;
        elsif (First_Char = '+' or else First_Char = '-')
        and then Must_Be_Binary then
          -- Someting like Y -X or ) -X => parse -X as two members
          if First_Char = '+' then
            Members_List.Insert ((Kind => Add));
          else
            Members_List.Insert ((Kind => Sub));
          end if;
          Member := (Kind => Val,
                     Value => Integer'Value (
                        Word (Positive'Succ(Word'First) .. Word'Last)));
          Must_Be_Binary := True;
        else
          Member := (Kind => Val, Value => Integer'Value (Word));
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
      Trace ("Constraint Error when parsing");
      raise Invalid_Expression;
  end Parse;

  -- Get member of list
  End_Reached : Boolean;
  function Get_Member return Member_Rec is
    Member : Member_Rec;
  begin
    if Members_List.Is_Empty then
      -- Members_List is empty
      raise Invalid_Expression;
    elsif End_Reached then
      -- End of expression is reached
      return (Kind => None);
    elsif Members_List.Check_Move then
      -- Next (not last) member
      Members_List.Read (Member);
    else
      -- Last member
      End_Reached := True;
      Members_List.Read (Member, Members_Mng.Current);
    end if;
    return Member;
  end Get_Member;

  -- Unget some members got
  procedure Unget_Member is
  begin
    if End_Reached then
      End_Reached := False;
    else
      Members_List.Move_To (Members_Mng.Prev);
    end if;
  end Unget_Member;

  -- One operation
  function Compute_One (I1 : Integer;
                        Op : Oper_Kind_List;
                        I2 : Integer) return Integer is
  begin
    case Op is
      when Add  => return I1 + I2;
      when Sub  => return I1 - I2;
      when Mult => return I1 * I2;
      when Div  => return I1 / I2;
    end case;
  end Compute_One;

  -- Compute an expression
  -- Level is the level of parentheses (0 at startup)
  -- Higher_Prio is set when low prio followed by high prio
  --  (e.g. X + Y * Z) require new evaluation.
  --  The way to return then differs
  function Compute (Level : Natural;
                 Higher_Prio : in Boolean) return Integer is
    M1, M2, M3, M4 : Member_Rec;
    Tmp, Result : Integer;
  begin
    if Level = 0 then
      End_Reached := False;
    end if;
    M1 := Get_Member;
    -- First member must be an int or a (
    if M1.Kind = Open then
      Trace ("Level++");
      Result := Compute (Level + 1, False);
    elsif M1.Kind = Val then
      Result := M1.Value;
      Trace ("Value " & Result'Img);
    else
      Trace ("Invalid M1 " & M1.Kind'Img);
      raise Invalid_Expression;
    end if;

    loop
      -- M1 was a value, M2 must be none or close or an operation
      M2 := Get_Member;
      if M2.Kind = None then
        if Level = 0 then
          -- Done
          Trace ("The end");
          return Result;
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
          return Result;
        end if;
      elsif M2.Kind not in Oper_Kind_List then
        Trace ("Invalid M2 " & M2.Kind'Img);
        raise Invalid_Expression;
      else
        Trace ("Oper " & M2.Kind'Img);
      end if;

      -- We have a value and an oper
      -- Now we must have either a value, or an opening parenthese
      M3 := Get_Member;
      if M3.Kind = Open then
        Trace ("Level++");
        Tmp := Compute (Level + 1, False);
      elsif M3.Kind /= Val then
        Trace ("Invalid M3 " & M3.Kind'Img);
        raise Invalid_Expression;
      else
        Tmp := M3.Value;
        Trace ("Value " & Tmp'Img);
      end if;

      -- Now we have a value, we need to see if it is followed
      --  by an operation of another level
      -- Here we must read an operation, a close or none
      M4 := Get_Member;
      if M4.Kind in Oper_Kind_List then
        if M4.Kind in High_Kind_List
        and then M2.Kind in Low_Kind_List then
          -- X + Y *, level increases due to prio,
          -- keep Y * for next level, unget this ope and value
          Unget_Member;
          Unget_Member;
          Trace ("Higher prio");
          Tmp := Compute (Level, True);
          Result := Compute_One (Result, M2.Kind, Tmp);
        elsif M4.Kind in Low_Kind_List
        and then M2.Kind in High_Kind_List then
          -- X * Y +, compute X * Y,
          Unget_Member;
          Result := Compute_One (Result, M2.Kind, Tmp);
          -- Level decreases if current is due to higher prio
          -- e.g. T + X * Y +
          if Higher_Prio then
            Trace ("End of higher prio");
            return Result;
          else
            -- Current level was at high prio and becomes
            -- at low prio
            Trace ("Lower prio");
            M2 := M4;
          end if;
        else
          -- X * Y /, same level, keep /
          Unget_Member;
          Trace ("Same prio");
          Result := Compute_One (Result, M2.Kind, Tmp);
        end if;
      elsif M4.Kind = Close then
        if Level = 0 then
          Trace ("Unmatched ')'");
          raise Invalid_Expression;
        else
          -- X * Y ), level decreases
          if Higher_Prio then
            -- T + X * Y ), let calling level process the Close
            Trace ("End of higher prio");
            Unget_Member;
          end if;
          Trace ("Level--");
          return Compute_One (Result, M2.Kind, Tmp);
        end if;
      elsif M4.Kind = None then
        -- End of expression
        if Level = 0 then
          -- Level decreases
          Trace ("The end");
          return Compute_One (Result, M2.Kind, Tmp);
        else
          -- Unexpected end
          Trace ("Unexpected end");
          raise Invalid_Expression;
        end if;
      end if;
    end loop;
  end Compute;

  -- Computation of expression
  function Compute (Expression : String) return Integer is
    Result : Integer;
  begin
    -- Fix and Parse expression
    Parse (Fix (Expression));
    -- Compute
    Result := Compute (0, False);
    -- Clean parsing
    Members_List.Delete_List;
    -- Done
    return Result;
  end Compute;

end Computer;

