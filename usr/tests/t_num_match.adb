-- Test Num with num and criteria arguments, for integer and arbitrary
with Basic_Proc, Argument, Arbitrary, Match, Images;
procedure T_Num_Match is

  package My_Num_Match is new Match.Num_Match (Integer);

  procedure Usage is
  begin
    Basic_Proc.Put_Line_Output ("Usage: " & Argument.Get_Program_Name
       & " <Natural> [ <Criteria_String> ]");
  end Usage;

  Str : String(1 .. 256);
  Len : Natural;
  Num : Natural;
  Arbi : Arbitrary.Number;
  Resn, Resa : Boolean;
begin

  if Argument.Get_Nbre_Arg = 1 then
    Num := Natural'Value (Argument.Get_Parameter);
    Len := 0;
  elsif Argument.Get_Nbre_Arg = 2 then
    Num := Natural'Value (Argument.Get_Parameter (Occurence => 1));
    Argument.Get_Parameter (Str, Len, 2);
  else
    Usage;
    return;
  end if;
  Arbi.Set (Num);

  -- Check match
  Resn := My_Num_Match.Matches (Num, Str(1 .. Len));
  Resa := Match.Arbi_Match.Matches (Arbi, Str(1 .. Len));

  if Resn /= Resa then
    Basic_Proc.Put_Line_Output ("Discrepancy");
  end if;

  Basic_Proc.Put_Output (Num'Img);
  if Resn then
    Basic_Proc.Put_Output (" matches");
  else
    Basic_Proc.Put_Output (" does not match");
  end if;
  Basic_Proc.Put_Line_Output (" >" & Str(1 .. Len) & "<");

  -- Put expanded
  declare
    Ranges : constant Match.Arbi_Match.Arbitrary_Array
           := Match.Arbi_Match.Expand (Str(1 .. Len), Arbi);
  begin
    for I in Ranges'Range loop
      Basic_Proc.Put_Output (Images.Arbitrary_Image (Ranges(I)));
      if I /= Ranges'Last then
        Basic_Proc.Put_Output (", ");
      else
        Basic_Proc.New_Line_Output;
      end if;
    end loop;
  end;

exception
  when Argument.Argument_Not_Found | Argument.Argument_Too_Long =>
    Usage;
end T_Num_Match;

