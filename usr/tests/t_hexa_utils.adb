with Basic_Proc, Hexa_Utils, Argument;
procedure T_Hexa_Utils is
  Valid : Boolean;
  V : Integer;
  function Image is new Hexa_Utils.Int_Image (Integer);
begin

  for I in 1 .. Argument.Get_Nbre_Arg loop
    begin
      Valid := False;
      V := Integer'Value (Argument.Get_Parameter (Occurence => I));
      Valid := True;
    exception
      when others =>
        Basic_Proc.Put_Line_Error ("Invalid_Argument");
raise;
    end;
    if Valid then
      Basic_Proc.Put_Line_Output (">" & Image (V) & "<");
    end if;
  end loop;
end T_Hexa_Utils;

