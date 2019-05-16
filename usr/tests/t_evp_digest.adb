with Basic_Proc, Argument, Evp_Digest, Hexa_Utils, Images, Lower_Str, Text_Line;
procedure T_Evp_Digest is

  procedure Usage is
  begin
    Basic_Proc.Put_Line_Output ("Usage : " & Argument.Get_Program_Name
        & " <algo>" & " { <text> | -- | -}");
    Basic_Proc.Put_Line_Output (
        """<text>"" adds text, ""--"" shows digest and resets");
    Basic_Proc.Put_Line_Output (
        """-"" processes stdin");
    Basic_Proc.Put_Line_Output ("or :    " & Argument.Get_Program_Name
        & " list");
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
          & " (" & Images.Integer_Image (Res'Length) & " bytes)");
    end;
    Shown := True;
 end Show;

  -- The stdin input flow
  Flow : Text_Line.File_Type;

  -- The context
  Ctx : Evp_Digest.Context;

begin
  if Argument.Get_Nbre_Arg < 1 then
    Usage;
    Basic_Proc.Set_Error_Exit_Code;
    return;
  elsif Argument.Get_Nbre_Arg = 1
  and then Argument.Get_Parameter (Occurence => 1) = "list" then
    -- List all the digests
    for Digest in Evp_Digest.Digest_List loop
      begin
        Ctx.Init (Digest);
        Basic_Proc.Put_Line_Output (Lower_Str (Digest'Img) & " => OK");
        Ctx.Reset;
      exception
        when Evp_Digest.Name_Error =>
          Basic_Proc.Put_Line_Output (Lower_Str (Digest'Img)
                                    & " => not implemented");
      end;
    end loop;
    return;
  end if;

  -- Init context
  Ctx.Init (Argument.Get_Parameter (Occurence => 1));

  if Argument.Get_Nbre_Arg = 2
  and then Argument.Get_Parameter (Occurence => 2) = "-" then
    Flow.Open_All (Text_Line.In_File);
    loop
      declare
        Str : constant String := Flow.Get;
      begin
        exit when Str = "";
        Ctx.Update (Str);
        exit when Str(Str'Last) /= Text_Line.Line_Feed_Char;
      end;
    end loop;
    Flow.Close_All;
  else
    -- Add arguments, when "--" then show
    for I in 2 .. Argument.Get_Nbre_Arg loop
      if Argument.Get_Parameter (Occurence => I) /= "--" then
        Ctx.Update (Argument.Get_Parameter (Occurence => I));
        Shown := False;
      else
        Show (Ctx);
      end if;
    end loop;
  end if;

  -- Show if not shown yet
  Show (Ctx);

  -- Done
  Ctx.Reset;

end T_Evp_Digest;

