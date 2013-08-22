with Parser;
package body Num_Match is

  Range_Sep : constant Character := '-';
  Str_Range_Sep : constant String := Range_Sep & "";

  -- Separator of criteria
  Spec_Sep : constant Character := ',';
  Str_Spec_Sep : constant String := Spec_Sep & "";
  function Is_Sep (C : Character) return Boolean is
  begin
    return C = Spec_Sep;
  end Is_Sep;

  -- Locate Range_Sep in Str (0 if not found)
  function Range_Sep_Index (Str : in String) return Natural is
    Index : Natural;
  begin
    -- Init as not found
    Index := 0;
    -- Look for one (and only one) sep
    for I in Str'Range loop
      if Str(I) = Range_Sep then
        if Index = 0 then
          Index := I;
        else
          raise Invalid_Criteria;
        end if;
      end if;
    end loop;
    return Index;
  end Range_Sep_Index;

  function Matches (Num : in Integer_Type; Criteria : in String)
                   return Boolean is
    Iter : Parser.Iterator;
    First : Boolean;
    Match : Boolean;

  begin
    -- Check Num
    if Num <  0 then
      raise Constraint_Error;
    end if;
    -- Optim
    if Criteria = Str_Range_Sep then
      return True;
    elsif Criteria = "" then
      return False;
    end if;

    -- Init for check
    Iter.Set (Criteria, Is_Sep'Unrestricted_Access);
    -- First spec
    First := True;
    -- Mach is kept along all specs cause we parse (check) all
    -- for syntax
    Match := False;

    -- Check all specs
    All_Specs:
    loop

      One_Spec:
      declare
        Cur_Spec : constant String := Iter.Next_Word;
        Range_Index : Natural;
        Range_First, Range_Last : Integer_Type;
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
        Range_First := Integer_Type'First;
        if Range_First < 0 then
          Range_First := 0;
        end if;
        Range_Last  := Integer_Type'Last;
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
        end if;

        -- Check for match if needed
        Match := Match or else (Num >= Range_First and then Num <= Range_Last);

        -- Trace for debug
        -- Basic_Proc.Put_Line_Output ("Checked spec >" & Cur_Spec & "<  -> "
        --                     & Range_First'Img & " -" & Range_Last'Img);
      end One_Spec;

    end loop All_Specs;

    -- Cleanup and Done
    Iter.Del;
    return Match;
  exception
    when others =>
      raise Invalid_Criteria;
  end Matches;

end Num_Match;

