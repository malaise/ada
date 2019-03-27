with Basic_Proc, Hexa_Utils, Argument;
procedure T_Hexa_Utils is
  Valid : Boolean;
  V : Integer;
  function Int_Image is new Hexa_Utils.Int_Image (Integer);

  subtype Subinteger is Integer range -21 .. Integer'Last;
  function Sub_Image is new Hexa_Utils.Int_Image (Subinteger);
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
      Basic_Proc.Put_Line_Output (">" &
        (if V in Subinteger then Sub_Image (V) else Int_Image (V)) & "<");
    end if;
  end loop;
end T_Hexa_Utils;

