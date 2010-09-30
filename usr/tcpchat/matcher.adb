with Regular_Expressions, String_Mng.Regex;
with Variables, Error;
package body Matcher is
  -- Expand Node.Text (maybe Check_Only)
  -- See if Str matches Node.Text: string comparison if not Node.Regex
  --  or Regular_Expressions.Exec)
  function Compute (Node : Tree.Node_Rec;
                    Str : Asu_Us;
                    Check_Only : Boolean) return Boolean is
    Expanded : Asu_Us;
    Compiled : Regular_Expressions.Compiled_Pattern;
    Ok : Boolean;
    N_Matched : Natural;
    Match_Info : Regular_Expressions.Match_Array (1 .. 10);
    use type Asu_Us, Regular_Expressions.Match_Cell;
  begin
    Expanded := Variables.Expand (Node.Text, Check_Only);
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

    -- Execute the regexp
    Regular_Expressions.Exec (Compiled, Asu_Ts (Str), N_Matched, Match_Info);
    if Match_Info(1) = Regular_Expressions.No_Match then
      return False;
    end if;

    if not Check_Only then
      -- Assign variables
      -- @@@
      null;
    end if;
    return True;
  exception
    when Variables.Expand_Error =>
      raise Match_Error;
  end Compute;


  -- Expand Node.Text in mode check
  -- Compile and test regex with a Dummy text
  -- Compute Node.Assign properties from Assign string
  procedure Check (Node : in out Tree.Node_Rec;
                  Assign : in Asu_Us) is
    Dummy : Boolean;
    pragma Unreferenced (Dummy);
  begin
    Dummy := Compute (Node, Asu_Tus ("Dummy"), True);
    -- Check and compute Assign
    declare
      Statements : constant String_Mng.Regex.String_Slice
                 := String_Mng.Regex.Split (Asu_Ts (Assign),
                                           "\n|\t| ", 10);
    begin
      -- @@@
      null;
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

