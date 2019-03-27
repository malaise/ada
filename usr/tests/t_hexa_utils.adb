with Basic_Proc, Hexa_Utils, Argument;
procedure T_Hexa_Utils is
  Min, Max : Integer;

  function Valid (I : Positive) return Integer is
  begin
    return Integer'Value (Argument.Get_Parameter (Occurence => I));
  exception
      when others =>
        Basic_Proc.Put_Line_Error ("Invalid_Argument");
        raise;
  end Valid;
begin
  if Argument.Get_Nbre_Arg >= 1 then
    Min := Valid (1);
  else
    Min := Integer'First;
  end if;
  if Argument.Get_Nbre_Arg >= 2 then
    Max := Valid (2);
  else
    Max := Integer'Last;
  end if;
  declare
    subtype Subinteger is Integer range Min .. Max;
    function Image is new Hexa_Utils.Int_Image (Subinteger);
  begin
    for I in 1 .. Argument.Get_Nbre_Arg loop
      Basic_Proc.Put_Line_Output (">" & Image (Valid (I)) & "<");
    end loop;
  end;
end T_Hexa_Utils;

