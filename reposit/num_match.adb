function Num_Match (Num : in Natural; Criteria : in String) return Boolean is

  Spec_Start : Positive := Positive'Last;
  Spec_Sep : constant Character := ',';
  No_Spec : constant String := "";

  procedure Init_Spec is
  begin
    if Criteria = "" 
    or else Criteria(Criteria'First) = Spec_Sep
    or else Criteria(Criteria'Last)  = Spec_Sep then
      raise Constraint_Error;
    end if;
    Spec_Start := 1;
  end Init_Spec;

  function Next_Spec return String is
    Start, Stop : Positive;
  begin
    -- End of criteria?
    if Spec_Start > Criteria'Last then
      return No_Spec;
    end if;
    -- Check "SepSep"
    if  Criteria(Spec_Start) = Spec_Sep then
      raise Constraint_Error;
    end if;
    -- Search next ','
    Start := Spec_Start;
    Stop := Start;
    loop
      exit when Stop = Criteria'Last
      or else Criteria(Stop + 1) = ',';
      Stop := Stop + 1;
    end loop;
    -- Set Spec_Start for next search
    Spec_Start := Stop + 2;
    -- Done
    return Criteria(Start .. Stop);
  end Next_Spec;

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

  Match : Boolean;
begin
  -- Init for check
  Init_Spec;
  Match := False;

  -- Check all specs
  All_Specs:
  loop

    One_Spec:
    declare
      Cur_Spec : constant String := Next_Spec;
      Range_Index : Natural;
      Range_First, Range_Last : Natural;
    begin
      exit when Cur_Spec = No_Spec;
      -- Check for range separator
      Range_Index := Range_Sep_Index (Cur_Spec);
      Range_First := Natural'First;
      Range_Last  := Natural'Last;
      if Range_Index = 0 then
        -- No range, one value
        Range_First := Natural'Value(Cur_Spec);
        Range_Last  := Range_First;
      elsif Range_Index = Cur_Spec'First and then Range_Index = Cur_Spec'Last then
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

  return Match;
end Num_Match;

