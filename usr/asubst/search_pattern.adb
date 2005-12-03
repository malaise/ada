with Ada.Characters.Latin_1, Ada.Strings.Unbounded;
with Sys_Calls, Argument, Unique_List, String_Mng, Text_Line, Debug;
package body Search_Pattern is

  -- Unique list of patterns
  type Line_Pat_Rec is record
    Num : Positive;
    Is_Delim : Boolean;
    Pat : Regular_Expressions.Compiled_Pattern;
  end record;
  procedure Set (To : out Line_Pat_Rec; Val : in Line_Pat_Rec) is
  begin
    To.Num := Val.Num;
    To.Is_Delim := Val.Is_Delim;
    -- Regexp cannot be copied, so it will be assigned
    --  to the Line_Pat_Rec access
  end Set;
  function Image (Line_Pat : Line_Pat_Rec) return String is
  begin
    return Line_Pat.Num'Img;
  end Image;
  function "=" (Current : Line_Pat_Rec; Criteria : Line_Pat_Rec)
               return Boolean is
  begin
    -- Unicity of Num
    return Current.Num = Criteria.Num;
  end "=";
  package Unique_Pattern is new Unique_List (Line_Pat_Rec, Set, Image, "=");
  Pattern_List : Unique_Pattern.List_Type;

  Is_Multiple : Boolean;

  package Asu renames Ada.Strings.Unbounded;

  -- Reports a parsing error
  procedure Error (Msg : in String) is
  begin
    Sys_Calls.Put_Line_Error (Argument.Get_Program_Name
        & " ERROR parsing search pattern: "
        & Msg & ".");
    raise Parse_Error;
  end Error;

  -- Parses the search patern
  -- Reports errors on stderr and raises Parse_Error.
  procedure Parse (Pattern : in String;
                   Extended, Case_Sensitive : in Boolean) is

    -- Add a line pattern
    procedure Add (Crit : in String) is
      Upat : Line_Pat_Rec;
      Upat_Access : Unique_Pattern.Element_Access;
      Ok : Boolean;
      Match : Regular_Expressions.One_Match_Array;
      Nmatch : Natural;
    begin
      -- Compute new pattern number and type
      Upat.Num := Unique_Pattern.List_Length (Pattern_List) + 1;
      Upat.Is_Delim := Crit = "";
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Search adding regex "
               &  Upat.Num'img & " >" & Crit & "<");
      end if;
      -- Empty pattern is a delimiter
      if Upat.Is_Delim then
        -- Insert delimiter
        Unique_Pattern.Insert (Pattern_List, Upat);
        return;
      end if;
      -- Regex compiled patterns cannot be set:
      -- Insert an empty pattern with the correct num
      Unique_Pattern.Insert (Pattern_List, Upat);
      -- Get access to it and compile in this access
      Unique_Pattern.Get_Access (Pattern_List, Upat, Upat_Access);
      Regular_Expressions.Compile (Upat_Access.Pat, Ok, Crit,
                                   Extended => Extended,
                                   Case_Sensitive => Case_Sensitive);
      if not Ok then
        Error ("Invalid pattern """ & Crit
          & """. Error is " & Regular_Expressions.Error(Upat_Access.Pat));
      end if;
      -- Check that this pattern is not ambiguous
      -- It should not match a special cahracter
      Regular_Expressions.Exec (Upat_Access.Pat,
                                "" & Ada.Characters.Latin_1.Bs,
                                Nmatch, Match);
      if Nmatch = 1 then
        Error ("Ambiguous pattern """ & Crit & """");
      end if;
    end Add;

    -- Check that the string does not contain the fragment
    procedure Check (Str : in String; Frag : in String) is
    begin
      if String_Mng.Locate (Str, Str'First, Frag) /= 0 then
        Error ("Pattern """ & Str & """ cannot contain """ & Frag & """");
      end if;
   end Check;

    -- Start line and stop line strings in regex
    function Start_String (Delim : in Boolean) return String is
    begin
      if Delim then return "^";
      else return "";
      end if;
    end Start_String;
    function Stop_String (Delim : in Boolean) return String is
    begin
      if Delim then return "$";
      else return "";
      end if;
    end Stop_String;

    -- Pattern delimiter
    Delimiter : constant String := "\n";
    -- Indexes in Pattern
    Start_Index : Positive;
    Stop_Index : Natural;
    -- Was previous item a delimiter
    Prev_Delim : Boolean;
    -- Is next item a delimiterA (true except at the end)
    Next_Delim : Boolean;
  begin
    if Debug.Set then
      Sys_Calls.Put_Line_Error ("Search parsing pattern >" & Pattern & "<");
    end if;
    -- Free previous pattern
    Unique_Pattern.Delete_List (Pattern_List);
    Is_Multiple := False;
    -- Reject empty pattern
    if Pattern = "" then
      Error ("Empty pattern");
    end if;
    -- Locate all New_Line and split pattern (one per line)
    Start_Index := Pattern'First;
    Prev_Delim := False;
    loop
      Stop_Index := String_Mng.Locate_Escape (Pattern,
                                              Start_Index,
                                              Delimiter);
      if Stop_Index = Start_Index then
        -- A Delim
        Add ("");
        Prev_Delim := True;
        Stop_Index := Stop_Index + 1;
      else
        -- A Regex: see if it followed by a delim (always except at the end)
        Next_Delim := Stop_Index /= 0;
        -- Make Stop_Index be the last index of regex
        if Stop_Index = 0 then Stop_Index := Pattern'Last;
        else Stop_Index := Stop_Index - 1;
        end if;
        -- It must not contain Start_String if preeceded by a delim
        Check (Pattern(Start_Index .. Stop_Index), Start_String (Prev_Delim));
        Check (Pattern(Start_Index .. Stop_Index), Stop_String (Next_Delim));
        -- Add this regex
        Add (Start_String (Prev_Delim)
           & Pattern(Start_Index .. Stop_Index)
           & Stop_String (Next_Delim));
        -- See if this is a single regex and if it can apply several times
        --  to one line of input (no ^ not $)
        if not Prev_Delim
        and then not Next_Delim
        and then Pattern(Start_Index..Start_Index) /= Start_String (True)
        and then Pattern(Stop_Index ..Stop_Index)  /= Stop_String (True) then
          Is_Multiple := True;
        end if;
      end if;
      -- Done
      exit when Stop_Index = Pattern'Last;
      Start_Index := Stop_Index + 1;
    end loop;
    -- Done
  exception
    when Parse_Error =>
      -- Free previous pattern
      Unique_Pattern.Delete_List (Pattern_List);
      raise;
  end Parse;

  -- Returns the number of lines that it covered by the
  -- search pattern (one per regex and one per New_Line.
  -- Raises No_Regex if the pattern was not parsed OK
  function Number return Positive is
    N : constant Natural := Unique_Pattern.List_Length (Pattern_List);
  begin
    if N = 0 then
      raise No_Regex;
    else
      return N;
    end if;
  end Number;

  -- Tells if the search pattern can be applied several times
  -- on one line of input (i.e. does not contain '\n', '^' or '$'
  -- Raises No_Regex if the pattern was not parsed OK
  function Multiple return Boolean is
  begin
    -- Must be some pattern compiled
    if Unique_Pattern.List_Length (Pattern_List) = 0 then
      raise No_Regex;
    end if;
    return Is_Multiple;
  end Multiple;

  -- Checks if the input string matches one regex
  -- Returns a Match_Cell (set to (0, 0) if no match)
  -- Raises No_Regex if the Regex_Index is higher than
  --  the number of regex (retruned by Parse)
  function Check (Str : String;
                  Regex_Index : Positive)
           return Regular_Expressions.Match_Cell is
    Upat : Line_Pat_Rec;
    Upat_Access : Unique_Pattern.Element_Access;
    Match : Regular_Expressions.One_Match_Array;
    Nmatch : Natural;
  begin
    -- Get access to the pattern
    Upat.Num := Regex_Index;
    Unique_Pattern.Get_Access (Pattern_List, Upat, Upat_Access);
    -- Delimiter matches delimiter
    if Upat_Access.Is_Delim then
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Search check pattern " & Regex_Index'Img &
                                  " is delim");
      end if;
      if Str = "" & Text_Line.Line_Feed then
        return (1, 1);
      else
        return (0, 0);
      end if;
    elsif Str = "" & Text_Line.Line_Feed then
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Search check empty vs not delim");
      end if;
      return (0, 0);
    else
      if Debug.Set then
        Sys_Calls.Put_Line_Error ("Search check pattern " & Regex_Index'Img);
      end if;
      Regular_Expressions.Exec (Upat_Access.Pat,
                                Str,
                                Nmatch, Match);
      if Nmatch = 1 and then Match(1).Start_Offset <= Match(1).End_Offset then
        return Match(1);
      else
        return (0, 0);
      end if;
    end if;
  exception
    when Unique_Pattern.Not_In_List =>
      -- Invalid Regex_Index or empty list
      raise No_Regex;
  end Check;

end Search_Pattern;

