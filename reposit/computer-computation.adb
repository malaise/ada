with Dynamic_List, Parser, Str_Util;
package body Computer.Computation is

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
        Log ("Fix raises Invalid_Expression on " & Expression);
        raise Invalid_Expression;
      end if;
    end loop;
    -- Variables must not follow one each other (${var}${var})
    if Str_Util.Locate (Exp.Image, "}$") /= 0 then
        Log ("Fix raises Invalid_Expression on " & Expression);
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
    Log ("Fixed expression: " & Exp.Image);
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
      Log ("Parse raises Invalid_Expression on " & Exp);
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
      Log ("Level++");
      Compute (Members_List, End_Reached, Result, Level + 1, False);
    elsif M1.Kind = Val then
      Result := M1.Value;
      Log ("Value " & Result.Image);
    else
      Log ("Invalid M1 " & M1.Kind'Img);
      raise Invalid_Expression;
    end if;

    loop
      -- M1 was a value, M2 must be none or close or an operation
      Get_Member (Members_List, End_Reached, M2);
      if M2.Kind = None then
        if Level = 0 then
          -- Done
          Log ("The end");
          return;
        else
          -- Unexpected end
          Log ("Unexpected end");
          raise Invalid_Expression;
        end if;
      elsif M2.Kind = Close then
        if Level = 0 then
          Log ("Unmatched ')'");
          raise Invalid_Expression;
        else
          -- (X), return X
          Log ("Level--");
          return;
        end if;
      elsif M2.Kind not in Oper_Kind_List then
        Log ("Invalid M2 " & M2.Kind'Img);
        raise Invalid_Expression;
      else
        Log ("Oper " & M2.Kind'Img);
      end if;

      -- We have a value and an oper
      -- Now we must have either a value, or an opening parenthese
      Get_Member (Members_List, End_Reached, M3);
      if M3.Kind = Open then
        Log ("Level++");
        Compute (Members_List, End_Reached, Tmp, Level + 1, False);
      elsif M3.Kind /= Val then
        Log ("Invalid M3 " & M3.Kind'Img);
        raise Invalid_Expression;
      else
        Tmp := M3.Value;
        Log ("Value " & Tmp.Image);
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
          Log ("Higher prio");
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
            Log ("End of higher prio");
            return;
          end if;
          -- Current level was at high prio and becomes
          -- at low prio
          Log ("Lower prio");
          M2 := M4;
        else
          -- X * Y /, same level, keep /
          Unget_Member (Members_List, End_Reached);
          Log ("Same prio");
          Result := Compute_One (Result, M2.Kind, Tmp);
        end if;
      elsif M4.Kind = Close then
        if Level = 0 then
          Log ("Unmatched ')'");
          raise Invalid_Expression;
        end if;
        -- X * Y ), level decreases
        if Higher_Prio then
          -- T + X * Y ), let calling level process the Close
          Log ("End of higher prio");
          Unget_Member (Members_List, End_Reached);
        end if;
        Log ("Level--");
        Result := Compute_One (Result, M2.Kind, Tmp);
        return;
      elsif M4.Kind = None then
        -- End of expression
        if Level = 0 then
          -- Level decreases
          Log ("The end");
          Result := Compute_One (Result, M2.Kind, Tmp);
          return;
        else
          -- Unexpected end
          Log ("Unexpected end");
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


end Computer.Computation;

