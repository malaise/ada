-- Basic computation of a oper b...
-- Where oper is +, -, * or /,
--  a and b are integers or ${Variable}
-- Supports parentheses.
with Ada.Strings.Unbounded;
with Environ, Basic_Proc, Unique_List, Int_Image,
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

  function Val_Image is new Int_Image (Integer);

  -- Unbounded strings
  package Asu renames Ada.Strings.Unbounded;
  subtype Asu_Us is Ada.Strings.Unbounded.Unbounded_String;
  function Asu_Ts (Source : in Asu_Us) return String
                  renames Asu.To_String;
  function Asu_Tus (Source : in String) return Asu_Us
                  renames Asu.To_Unbounded_String;


  -- List of variables
  type Var_Rec is record
    -- Variable name
    Name : Asu_Us;
    -- Int_Image of value
    Value : Asu_Us;
  end record;
  type Var_Access is access all Var_Rec;
  procedure Set (To : out Var_Rec; Val : in Var_Rec) is
  begin
    To := Val;
  end Set;
  function Image (Element : Var_Rec) return String is
  begin
    return Asu_Ts (Element.Name);
  end Image;
  function "=" (Current : Var_Rec ; Criteria : Var_Rec ) return Boolean is
    use type Asu_Us;
  begin
    return Current.Name = Criteria.Name;
  end "=";
  package Var_Mng is new Unique_List (Var_Rec, Var_Access, Set, Image, "=");
  Var_List : Var_Mng.List_Type;

  -- List of members
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

  -- Variable management
  ----------------------
  -- Reset all variables
  procedure Reset is
  begin
    Var_Mng.Delete_List (Var_List);
    Members_Mng.Delete_List (Members_List);
  end Reset;

  -- Set (store), maybe overwrite a variable
  procedure Set (Name : in String;
                 Value : in Integer) is
  begin
    if Name = "" then
      raise Invalid_Variable;
    end if;
    Var_Mng.Insert (Var_List, (Name => Asu_Tus (Name),
                               Value => Asu_Tus(Val_Image(Value))));
    Trace ("Inserted variable " & Name & ", " & Val_Image(Value));
  end Set;

  -- Check if a variable is set
  function Is_Set (Name : String) return Boolean is
    Crit : Var_Rec;
    Found : Boolean;
  begin
    if Name = "" then
      raise Invalid_Variable;
    end if;
    Crit.Name := Asu_Tus (Name);
    Var_Mng.Search (Var_List, Crit, Found);
    return Found;
  end Is_Set;

  -- Get a variable
  -- May raise Unknown_Variable
  -- This one is for us
  function Get (Name : String) return String is
    Res : Var_Rec;
  begin
    if Name = "" then
      raise Invalid_Variable;
    end if;
    Res.Name := Asu_Tus (Name);
    Var_Mng.Read (Var_List, Res, Res);
    return Asu_Ts (Res.Value);
  exception
    when Var_Mng.Not_In_List =>
      raise Unknown_Variable;
  end Get;
  -- This one is the API
  function Get (Name : String) return Integer is
    Res : Var_Rec;
  begin
    return Integer'Value (Get (Name));
  end Get;

  -- Fix expression
  function Fix (Expression : String) return String is
    Exp : Asu_Us;
  begin
    -- Replace each operator and parenthese 'op' by ' op '
    Exp := Asu_Tus (Expression);
    -- +X and -X will be analysed while parsing
    Exp := Asu_Tus (String_Mng.Replace (Asu_Ts (Exp), "+", " +"));
    Exp := Asu_Tus (String_Mng.Replace (Asu_Ts (Exp), "-", " -"));
    Exp := Asu_Tus (String_Mng.Replace (Asu_Ts (Exp), "*", " * "));
    Exp := Asu_Tus (String_Mng.Replace (Asu_Ts (Exp), "/", " / "));
    Exp := Asu_Tus (String_Mng.Replace (Asu_Ts (Exp), "(", " ( "));
    Exp := Asu_Tus (String_Mng.Replace (Asu_Ts (Exp), ")", " ) "));
    -- Replace each "  " by " "
    while String_Mng.Locate (Asu_Ts (Exp), 1, "  ") /= 0 loop
      Exp := Asu_Tus (String_Mng.Replace (Asu_Ts (Exp), "  ", " "));
    end loop;
    -- Remove heading and trailing spaces
    if Asu.Element (Exp, 1) = ' ' then
      Exp := Asu_Tus (String_Mng.Cut (Asu_Ts (Exp), 1, True));
    end if;
    if Asu.Element (Exp, Asu.Length (Exp)) = ' ' then
      Exp := Asu_Tus (String_Mng.Cut (Asu_Ts (Exp), 1, False));
    end if;
    -- Variables must not follow one each other (${var}${var})
    if String_Mng.Locate (Asu_Ts (Exp), 1, "}$") /= 0 then
      raise Invalid_Expression;
    end if;
    -- Expand variables
    Exp := Asu_Tus (String_Mng.Eval_Variables (Asu_Ts (Exp),
                    "${", "}", Get'Access));
    Trace ("Fixed expression: " & Asu_Ts (Exp));
    return Asu_Ts (Exp);
  end Fix;

  -- Parse the expression into a list of members
  procedure Parse (Exp : in String ) is
    Iter : Parser.Iterator;
    -- Can +X or -X  be unary
    Unary : Boolean;
    First_Char : Character;
    Member : Member_Rec;
  begin
    Parser.Set (Iter, Exp, Parser.Is_Space_Or_Htab_Function'Access);
    Unary := True;
    loop
      declare
        Word : constant String := Parser.Next_Word (Iter);
      begin
        exit when Word = "";
        First_Char :=  Word(Word'First);
        if Word'Length = 1 then
          case First_Char is
            when '+' => Member := (Kind => Add);
            when '-' => Member := (Kind => Sub);
            when '*' => Member := (Kind => Mult);
            when '/' => Member := (Kind => Div);
            when '(' => Member := (Kind => Open);
            when ')' => Member := (Kind => Close);
            when '0' .. '9' =>
              Member := (Kind => Val, Value => Integer'Value (Word));
            when others =>
              raise Invalid_Expression;
          end case;
        elsif not Unary
        and then (First_Char = '+' or else First_Char = '-') then
          -- Someting like Y -X, parse -X as two members
          if First_Char = '+' then
            Members_Mng.Insert (Members_List, (Kind => Add));
          else
            Members_Mng.Insert (Members_List, (Kind => Sub));
          end if;
          Member := (Kind => Val,
                     Value => Integer'Value (
                        Word (Positive'Succ(Word'First) .. Word'Last)));
        else
          Trace ("Value");
          Member := (Kind => Val, Value => Integer'Value (Word));
        end if;
      end;
      Members_Mng.Insert (Members_List, Member);
      -- Next can be unary if current is not a value
      Unary := Member.Kind /= Val;
    end loop;
    Parser.Del (Iter);
    Members_Mng.Rewind (Members_List);
  exception
    when Constraint_Error =>
      if Parser.Is_Set (Iter) then
        Parser.Del (Iter);
      end if;
      Trace ("Constraint Error when parsing");
      raise Invalid_Expression;
  end Parse;

  -- Get member of list
  End_Reached : Boolean := False;
  function Get_Member return Member_Rec is
    Member : Member_Rec;
  begin
    if End_Reached then
      return (Kind => None);
    end if;
    if Members_Mng.Check_Move (Members_List) then
      Members_Mng.Read (Members_List, Member);
    else
      End_Reached := True;
      Members_Mng.Read (Members_List, Member, Members_Mng.Current);
    end if;
    return Member;
  end Get_Member;

  -- Unget some members got
  procedure Unget_Member is
  begin
    if End_Reached then
      End_Reached := False;
    else
      Members_Mng.Move_To (Members_List, Members_Mng.Prev);
    end if;
  end Unget_Member;

  -- One operation
  function Compute (I1 : Integer;
                    Op : Oper_Kind_List;
                    I2 : Integer) return Integer is
  begin
    case Op is
      when Add  => return I1 + I2;
      when Sub  => return I1 - I2;
      when Mult => return I1 * I2;
      when Div  => return I1 / I2;
    end case;
  end Compute;
  -- Eval a level of expression
  function Eval (Level : in Natural) return Integer is
    M1, M2, M3, M4 : Member_Rec;
    Tmp, Result : Integer;
  begin
    M1 := Get_Member;
    -- First member must be an int or a (
    if M1.Kind = Open then
      Result := Eval (Level + 1);
    elsif M1.Kind = Val then
      Result := M1.Value;
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
          return Result;
        end if;
      elsif M2.Kind not in Oper_Kind_List then
        Trace ("Invalid M2 " & M2.Kind'Img);
        raise Invalid_Expression;
      end if;
      -- We have a value and an oper
      -- Now we must have either a value, or an opening parenthese
      M3 := Get_Member;
      if M3.Kind = Open then
        Tmp := Eval (Level + 1);
      elsif M3.Kind /= Val then
        Trace ("Invalid M2 " & M2.Kind'Img);
        raise Invalid_Expression;
      else
        Tmp := M3.Value;
      end if;

      -- Now we have a value, we need to see if it is followed
      --  by an operation of another level
      -- Here we must read an operation, a close or none
      M4 := Get_Member;
      if M4.Kind in Oper_Kind_List then
        if M4.Kind in High_Kind_List
        and then M2.Kind in Low_Kind_List then
          -- X + Y *, level increases, keep Y * for next level
          -- Unget this ope and value
          Unget_Member;
          Unget_Member;
          Tmp := Eval (Level);
          return Compute (Result, M2.Kind, Tmp);
        elsif M4.Kind in Low_Kind_List
        and then M2.Kind in High_Kind_List then
          -- X * Y +, compute X * Y, now we are at low level
          Unget_Member;
          Result := Compute (Result, M2.Kind, Tmp);
          M2 := M4;
        else
          -- X * Y /, same level, keep /
          Unget_Member;
          Result := Compute (Result, M2.Kind, Tmp);
        end if;
      elsif M4.Kind = Close then
        if Level = 0 then
          Trace ("Unmatched ')'");
          raise Invalid_Expression;
        else
          -- X * Y ), level decreases, consume all
          return Compute (Result, M2.Kind, Tmp);
        end if;
      elsif M4.Kind = None then
        -- End of expression
        if Level = 0 then
          -- Level decreases
          return Compute (Result, M2.Kind, Tmp);
        else
          -- Unexpected end
          Trace ("Unexpected end");
          raise Invalid_Expression;
        end if;
      end if;
    end loop;
  end Eval;
  -- Computation of expression
  function Eval (Expression : String) return Integer is
  begin
    -- Fix expression
    -- Parse expression
    Parse (Fix (Expression));
    -- Eval
    return Eval (0);
  end Eval;

end Computer;

