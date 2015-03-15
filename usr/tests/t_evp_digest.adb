with Basic_Proc, Argument, Evp_Digest, Hexa_Utils;
procedure T_Evp_Digest is

  procedure Usage is
  begin
    Basic_Proc.Put_Line_Output ("Usage : " & Argument.Get_Program_Name
        & "<algo>" & " { <text> }");
  end Usage;

  Ctx : Evp_Digest.Context;
begin
  if Argument.Get_Nbre_Arg < 1 then
    Usage;
    Basic_Proc.Set_Error_Exit_Code;
    return;
  end if;

  -- Init context
  Ctx.Init (Argument.Get_Parameter (Occurence => 1));

  -- Add arguments
  for I in 2 .. Argument.Get_Nbre_Arg loop
    Ctx.Update (Argument.Get_Parameter (Occurence => I));
  end loop;

  -- Get Code and display it
  declare
    Res : constant Evp_Digest.Byte_Array
        := Ctx.Final;
    Str : String (1 .. Res'Length * 2);
  begin
    for I in 0 .. Res'Last - 1 loop
      Str(1 + 2 * I .. 1 + 2 * I + 1) :=
          Hexa_Utils.Image (Natural (Res(I + 1)), 2);
    end loop;
    Basic_Proc.Put_Line_Output (Str);
  end;

end T_Evp_Digest;

