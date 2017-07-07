with Parser, Unbounded_Arrays, Trace.Loggers, Mixed_Str;
package body Num_Match is

  -- Separator within a range
  Range_Sep : constant Character := '-';
  Str_Range_Sep : constant String := Range_Sep & "";

  -- Unbounded array of Integer_Type
  package Integer_Unbounded_Arrays is new Unbounded_Arrays (Integer_Type,
                                                            Integer_Array);
  subtype Unb_Ints is Integer_Unbounded_Arrays.Unbounded_Array;

  -- Logger
  Logger : Trace.Loggers.Logger;

  -- Separator of criteria
  Spec_Sep : constant Character := ',';
  Str_Spec_Sep : constant String := Spec_Sep & "";
  function Is_Sep (C : Character) return Boolean is (C = Spec_Sep);

  -- Locate Range_Sep in Str (0 if not found)
  function Range_Sep_Index (Str : in String) return Natural is
    Index : Natural;
  begin
    -- Init as not found
    Index := 0;
    -- Look for one (and only one) sep
    for I in Str'Range loop
      if Str(I) = Range_Sep then
        if Index /= 0 then
          raise Invalid_Criteria;
        end if;
        Index := I;
      end if;
    end loop;
    return Index;
  end Range_Sep_Index;

  -- The common procedure that parses Criteriria and, depending on Match, either
  -- Check if Num matches and sets Matches (Expanded empty)
  -- Expands the range of Integer_Type into Expanded (Matches unsignificant)
  procedure Parse (Num : in Integer_Type; Criteria : in String; Match : in Boolean;
                   Matches : out Boolean; Expanded : out Unb_Ints) is
    Integer_First : Integer_Type;
    Iter : Parser.Iterator;
    First : Boolean;
  begin
    Logger.Init ("Num_Match");
    -- Check Num
    if Num <  0 then
      raise Constraint_Error;
    end if;
    -- Set first non negative Integer_Type
    Integer_First := Integer_Type'First;
    if Integer_First < 0 then
      Integer_First := 0;
    end if;
    -- Optim
    if Criteria = Str_Range_Sep then
      Logger.Log_Debug ("Optim All");
      if Match then
        -- Any Num matches "-"
        Matches := True;
      else
        -- First .. Num
        Expanded.Set_Null;
        for I in Integer_First .. Num loop
          Expanded.Append (I);
        end loop;
      end if;
      return;
    elsif Criteria = "" then
      Logger.Log_Debug ("Optim None");
      if Match then
        -- No Num match ""
        Matches := False;
      else
        Expanded.Set_Null;
      end if;
      return;
    end if;

    -- Init for check
    Iter.Set (Criteria, Is_Sep'Unrestricted_Access);
    -- First spec
    First := True;
    -- Mach is kept along all specs cause we parse (check) all
    -- for syntax
    Matches := False;
    Expanded.Set_Null;

    -- Check all specs
    All_Specs:
    loop

      One_Spec:
      declare
        Cur_Spec : constant String := Iter.Next_Word;
        Range_Index : Natural;
        Range_First, Range_Last, Range_Max : Integer_Type;
        Loc_Matches : Boolean;
      begin
        if First then
          -- First word
          if Cur_Spec = "" then
            -- Empty criteria
            exit All_Specs;
          end if;
          if Iter.Prev_Separators /= "" then
            -- Criteria starts with Sep(s)
            raise Invalid_Criteria;
          end if;
          First := False;
        elsif Cur_Spec /= "" then
          -- Not first word nor the end shall be separated by one Sep
          if Iter.Prev_Separators /= Str_Spec_Sep then
            -- Not separated by Sep
            raise Invalid_Criteria;
          end if;
        else
          -- The end, last word shall not be followed by Seps(s)
          if Iter.Prev_Separators /= "" then
            raise Invalid_Criteria;
          end if;
          exit All_Specs;
        end if;

        -- Check for range separator
        Range_Index := Range_Sep_Index (Cur_Spec);
        Range_First := Integer_First;
        Range_Last  := Integer_Type'Last;
        Range_Max := Num;
        -- Set the value(s) or raise Invalid_Criteria
        if Range_Index = 0 then
          -- No range
          Range_First := Integer_Type'Value(Cur_Spec);
          Range_Last  := Range_First;
        elsif Range_Index = Cur_Spec'First
        and then Range_Index = Cur_Spec'Last then
          -- No limit
          null;
        elsif Range_Index = Cur_Spec'First then
          -- Upper limit
          Range_Last  := Integer_Type'Value (
                           Cur_Spec(Range_Index+1 .. Cur_Spec'Last));
          Range_Max := Range_Last;
        elsif Range_Index = Cur_Spec'Last then
           -- Lower limit
          Range_First := Integer_Type'Value (
                           Cur_Spec(Cur_Spec'First .. Range_Index-1));
        else
          -- Two limits
          Range_First := Integer_Type'Value (
                           Cur_Spec(Cur_Spec'First .. Range_Index-1));
          Range_Last  := Integer_Type'Value (
                           Cur_Spec(Range_Index+1 .. Cur_Spec'Last));
          Range_Max := Range_Last;
        end if;

        -- Check for match
        Loc_Matches := Num >= Range_First and then Num <= Range_Last;

        if Logger.Debug_On then
          Logger.Log_Debug ("Checked spec >" & Cur_Spec & "<  -> "
                          & Range_First'Img & " -" & Range_Last'Img & " --> "
                          & Mixed_Str (Loc_Matches'Img));
        end if;
        Matches := Matches or else Loc_Matches;

        -- Expand if needed
        if not Match then
          for I in Range_First .. Range_Max loop
            Expanded.Append (I);
          end loop;
        end if;
      end One_Spec;

    end loop All_Specs;

    -- Cleanup and Done
    Iter.Del;
  exception
    when others =>
      raise Invalid_Criteria;
  end Parse;

  -- Check if a given num matches a criteria
  function Matches (Num : in Integer_Type; Criteria : in String)
                   return Boolean is
    Result : Boolean;
    Dummy_Ints : Unb_Ints;
  begin
    Parse (Num, Criteria, True, Result, Dummy_Ints);
    return Result;
  end Matches;

  -- Expand a <specs> as a list of nums
  function Expand (Criteria : in String; Max : in Integer_Type)
           return Integer_Array is
    Dummy_Match : Boolean;
    Ints : Unb_Ints;
  begin
    Parse (Max, Criteria, False, Dummy_Match, Ints);
    return Ints.To_Array;
  end Expand;

end Num_Match;

