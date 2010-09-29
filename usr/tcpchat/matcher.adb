with Regular_Expressions;
with Debug;
package body Matcher is
  -- Expand Node.Text (maybe Check_Only)
  -- See if Str matches Node.Text: string comparison if not Node.Regex
  --  or Regular_Expressions.Exec)
  function Match (Node : Tree.Node_Rec;
                  Str : Asu_Us;
                  Check_Only : Boolean := False) return Boolean is
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
    Regular_Expressions.Compile (Compiled, Ok,
         "^"  & Asu_Ts (Expanded) & "$");
    if not Ok then
      Debug.Log ("ERROR when compiling regex " & Asu_Ts (Expanded)
        & " : " & Regular_Expressions.Error (Compiled));
      raise Regexp_Error;
    end if;

    -- Execute the regexp
    Regular_Expressions.Exec (Compiled, Asu_Ts (Str), N_Matched, Match_Info);
    if Match_Info(1) = Regular_Expressions.No_Match then
      return False;
    end if;

    -- Assign variables
    -- @@@
    return True;
  end Match;
end Matcher;

