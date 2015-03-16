with Basic_Proc, Argument, Evp_Digest, Hexa_Utils, Images;
procedure T_Evp_Digest is

  procedure Usage is
  begin
    Basic_Proc.Put_Line_Output ("Usage : " & Argument.Get_Program_Name
        & " <algo>" & " { <text> | -- }");
    Basic_Proc.Put_Line_Output ("<text> adds text, -- shows digest and resets");
  end Usage;


  -- Get digest and put
  -- Is digest already shown
  Shown : Boolean := False;
  procedure Show (Ctx : in out Evp_Digest.Context) is
  begin
    if Shown then
      return;
    end if;
    declare
      Res : constant Evp_Digest.Byte_Array := Ctx.Get;
      Str : String (1 .. Res'Length * 2);
    begin
      for I in 0 .. Res'Last - 1 loop
        Str(1 + 2 * I .. 1 + 2 * I + 1) :=
            Hexa_Utils.Image (Natural (Res(I + 1)), 2);
      end loop;
      Basic_Proc.Put_Line_Output (Str
          & " (" & Images.Integer_Image (Res'Length) & ")");
    end;
    Shown := True;
 end Show;

  -- The context
  Ctx : Evp_Digest.Context;

begin
  if Argument.Get_Nbre_Arg < 1 then
    Usage;
    Basic_Proc.Set_Error_Exit_Code;
    return;
  end if;

  -- Init context
  Ctx.Init (Argument.Get_Parameter (Occurence => 1));

  -- Add arguments, when "--" then show
  for I in 2 .. Argument.Get_Nbre_Arg loop
    if Argument.Get_Parameter (Occurence => I) /= "--" then
      Ctx.Update (Argument.Get_Parameter (Occurence => I));
      Shown := False;
    else
      Show (Ctx);
    end if;
  end loop;

  -- Show if not shown yet
  Show (Ctx);

end T_Evp_Digest;

