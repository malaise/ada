with Parser;
function Num_Match (Num : in Natural; Criteria : in String) return Boolean is

  -- Separator of criteria
  Spec_Sep : constant Character := ',';
  Str_Sep : constant String := Spec_Sep & "";
  function Is_Sep (C : Character) return Boolean is
  begin
    return C = Spec_Sep;
  end Is_Sep;

  -- Locate Range_Sep in Str (0 if not found)
  function Range_Sep_Index (Str : in String) return Natural is
    Range_Sep : constant Character := '-';
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
          raise Constraint_Error;
        end if;
      end if;
    end loop;
    return Index;
  end Range_Sep_Index;

  Iter : Parser.Iterator;
  First : Boolean;
  Match : Boolean;

begin
  -- Init for check
  Parser.Create (Criteria, Is_Sep'Unrestricted_Access, Iter);
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
      Cur_Spec : constant String := Parser.Next_Word (Iter);
      Range_Index : Natural;
      Range_First, Range_Last : Natural;
    begin
      if First then
        -- First word
        if Cur_Spec = "" then
          -- Empty criteria
          exit All_Specs;
        end if;
        if Parser.Prev_Separators (Iter) /= "" then
          -- Criteria starts with Sep(s)
          raise Constraint_Error;
        end if;
        First := False;
      elsif Cur_Spec /= "" then
        -- Not first word nor the end shall be separated by one Sep
        if Parser.Prev_Separators (Iter) /= Str_Sep then
          -- Not separated by Sep
          raise Constraint_Error;
        end if;
      else
        -- The end, last word shall not be followed by Seps(s)
        if Parser.Prev_Separators (Iter) /= "" then
          raise Constraint_Error;
        end if;
        exit All_Specs;
      end if;

      -- Check for range separator
      Range_Index := Range_Sep_Index (Cur_Spec);
      Range_First := Natural'First;
      Range_Last  := Natural'Last;
      -- Set the value(s) or raise Constraint_Error
      if Range_Index = 0 then
        -- No range
        Range_First := Natural'Value(Cur_Spec);
        Range_Last  := Range_First;
      elsif Range_Index = Cur_Spec'First
      and then Range_Index = Cur_Spec'Last then
        -- No limit
        null;
      elsif Range_Index = Cur_Spec'First then
        -- Upper limit
        Range_Last  := Natural'Value(Cur_Spec(Range_Index+1 .. Cur_Spec'Last));
      elsif Range_Index = Cur_Spec'Last then
         -- Lower limit
        Range_First := Natural'Value(Cur_Spec(Cur_Spec'First .. Range_Index-1));
      else
        -- Two limits
        Range_First := Natural'Value(Cur_Spec(Cur_Spec'First .. Range_Index-1));
        Range_Last  := Natural'Value(Cur_Spec(Range_Index+1 .. Cur_Spec'Last));
      end if;

      -- Check for match if needed
      Match := Match or else (Num >= Range_First and then Num <= Range_Last);

      -- Trace for debug
      -- Ada.Text_Io.Put_Line ("Checked spec >" & Cur_Spec & "<  -> "
      --                     & Range_First'Img & " -" & Range_Last'Img);
    end One_Spec;

  end loop All_Specs;

  -- Cleanup and Done
  Parser.Delete (Iter);
  return Match;

end Num_Match;

