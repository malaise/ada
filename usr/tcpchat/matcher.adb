with Regular_Expressions, String_Mng.Regex, Any_Def, Int_Image;
with Variables, Debug, Error;
package body Matcher is

  function Nat_Image is new Int_Image (Natural);

  -- Expand Node.Text (maybe Check_Only)
  -- See if Str matches Node.Text: string comparison if not Node.Regex
  --  or Regular_Expressions.Exec)
  function Compute (Node : Tree.Node_Rec;
                    Str : Asu_Us;
                    Check_Only : Boolean) return Boolean is
    Expanding, Expanded : Asu_Us;
    Compiled : Regular_Expressions.Compiled_Pattern;
    Ok : Boolean;
    N_Matched : Natural;
    Match_Info : Regular_Expressions.Match_Array (1 .. 10);
    use type Asu_Us, Regular_Expressions.Match_Cell, Any_Def.Any_Kind_List,
             Tree.Node_Kind;
  begin
    -- Case of the Eval/Set statement: One variable to assign
    if Node.Kind = Tree.Eval
    or else Node.Kind = Tree.Set then
      -- Expand variable name
      Expanding := Node.Assign(Node.Assign'First).Name;
      Expanded := Variables.Expand (Expanding, Check_Only);
      if not Check_Only then
        -- Set variable
        Debug.Log ("Variable " & Asu_Ts (Expanded) & " set to "
                 & Asu_Ts (Str));
        Variables.Set (Expanded, Str);
      end if;
      return True;
    end if;

    -- Other case of assign (chat/expect/read => tree kind Read)
    --  or Cond
    -- Expand expression
    Expanding := Node.Text;
    Expanded := Variables.Expand (Expanding, Check_Only);

    if not Node.Regexp then
      -- Pure string comparison
      return Str = Expanded;
    end if;

    -- This is a Regexp, compile it
    Expanded := "^"  & Expanded & "$";
    Regular_Expressions.Compile (Compiled, Ok, Asu_Ts (Expanded));
    if not Ok then
      Error ("Cannot compile regex " & Asu_Ts (Expanded)
        & " : " & Regular_Expressions.Error (Compiled));
      raise Match_Error;
    end if;
    Debug.Log ("Regex compiled " & Asu_Ts (Expanded));

    -- Execute the regexp
    Regular_Expressions.Exec (Compiled, Asu_Ts (Str), N_Matched, Match_Info);
    if Match_Info(1) = Regular_Expressions.No_Match then
      Debug.Log ("Regex no match");
      return False;
    end if;
    Debug.Log ("Regex match " & N_Matched'Img);

    -- Case of the Cond: One variable to compare
    if Node.Kind = Tree.Cond then
      return True;
    end if;

    -- Assign variables after real Match, if any
    if not Check_Only
    and then Node.Assign(1).Value.Kind /= Any_Def.None_Kind then
      -- Set volatile variables to matching substring
      for I in Match_Info'Range  loop
        -- ${0} is first match ...
        Expanding := Asu_Tus (Nat_Image (I - 1));
        if I <= N_Matched then
          Expanded := Asu.Unbounded_Slice (Str, Match_Info(I).First_Offset,
                                                Match_Info(I).Last_Offset_Stop);
        else
          Expanded := Asu_Null;
        end if;
        Variables.Set_Volatile (Expanding, Expanded);
        Debug.Log ("Volatile " & Asu_Ts (Expanding)
                   & "=" & Asu_Ts (Expanded));
      end loop;
      -- Assign variables
      for I in Node.Assign'Range loop
        exit when Node.Assign(I).Value.Kind = Any_Def.None_Kind;
        Expanding := Node.Assign(I).Value.Str;
        Expanded := Variables.Expand (Expanding);
        Variables.Set (Node.Assign(I).Name, Expanded);
        Debug.Log ("Assigned " & Asu_Ts (Node.Assign(I).Name)
                 & "=" & Asu_Ts (Expanded));
      end loop;
      Variables.Clear_Volatiles;
    end if;
    return True;
  exception
    when Variables.Expand_Error =>
      Error ("Cannot expand expression " & Asu_Ts (Node.Text));
      raise Match_Error;
  end Compute;

  procedure Parse_Assign (Node : in out Tree.Node_Rec;
                          I : in Tree.Assignments_Range;
                          Statement : in Asu_Us) is
    Index : Natural;
  begin

    Index := String_Mng.Locate (Asu_Ts (Statement), "=");
    if Index <= 1 or else Index = Asu.Length (Statement) then
      Error ("Invalid assignment " & Asu_Ts (Statement));
      raise Match_Error;
    end if;
    -- Name <- the head (before the "=") and Value <- the tail
    Node.Assign(I) := (
       Name => Asu.Unbounded_Slice (Statement, 1, Index - 1),
       Value => (Kind => Any_Def.Str_Kind,
                 Str  => Asu.Unbounded_Slice (Statement,
                           Index + 1, Asu.Length (Statement))));
    -- Name cannot be "0", "1"...
    if Regular_Expressions.Match ("[0-9]+", Asu_Ts (Node.Assign(I).Name),
                                  True) then
      Error ("Invalid variable name in assignment "
           & Asu_Ts (Node.Assign(I).Name));
      raise Match_Error;
    end if;
    -- Check the value expands properly
    declare
      Expanded : Asu_Us;
      pragma Unreferenced (Expanded);
    begin
      Expanded := Variables.Expand (Node.Assign(I).Value.Str, True);
    exception
      when Variables.Expand_Error =>
        Error ("Invalid value in assignment "
             & Asu_Ts (Node.Assign(I).Value.Str));
        raise Match_Error;
    end;
    -- Check that value does not contain refs > 9 ("\$\{[0-9][0-9]+\})
    if Regular_Expressions.Match ("\$\{[0-9][0-9]+\}",
            Asu_Ts (Node.Assign(I).Value.Str), False) then
      Error ("Invalid value in assignment "
           & Asu_Ts (Node.Assign(I).Value.Str));
      raise Match_Error;
    end if;
    Debug.Log ("Parsed assignement " & Asu_Ts (Node.Assign(I).Name)
             & "=" & Asu_Ts (Node.Assign(I).Value.Str));
  end Parse_Assign;

  -- Expand Node.Text in mode check
  -- Compile and test regex with a Dummy text
  -- Compute Node.Assign properties from Assign string
  procedure Check (Node : in out Tree.Node_Rec;
                   Assign : in Asu_Us) is
    Dummy : Boolean;
    pragma Unreferenced (Dummy);
    use type Tree.Node_Kind;
  begin
    -- Check
    Dummy := Compute (Node, Asu_Tus ("Dummy"), True);

    -- Case of the Eval/Set/Cond statement: One variable to assign
    if Node.Kind = Tree.Eval
    or else Node.Kind = Tree.Set
    or else Node.Kind = Tree.Cond then
      -- Set it as first assign name
      Node.Assign(Node.Assign'First) := (
         Name => Assign,
         Value => (Kind => Any_Def.None_Kind) );
      return;
    end if;

    -- Other case of assign (chat/expect/read => tree kind Read)
    if Asu_Is_Null (Assign) then
      return;
    end if;
    if not Node.Regexp then
      Error ("Assignment only allowed with regexp");
      raise Match_Error;
    end if;

    -- Check and compute Assign
    declare
      -- Split into assignments
      Statements : constant String_Mng.Regex.String_Slice
                 := String_Mng.Regex.Split (Asu_Ts (Assign),
                                           "(\n|\t| )+", 10);
    begin
      Debug.Log ("Found " & Natural'Image (Statements'Length)
               & " assignments");
      if Statements'Length = 0 then
        -- No separator => Parse the whole Assign string
        Parse_Assign (Node, 1, Assign);
      else
        -- Parse each assignment
        for I in Statements'Range loop
          Parse_Assign (Node, I, Statements(I));
        end loop;
      end if;
    end;
  end Check;

  -- Expand Node.Text
  -- See if Str matches Node.Text: string comparison if not Node.Regex
  --  or Regular_Expressions.Exec)
  function Match (Node : Tree.Node_Rec;
                  Str : Asu_Us) return Boolean is
  begin
    return Compute (Node, Str, False);
  end Match;

end Matcher;

