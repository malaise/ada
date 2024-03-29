-- Fetch HTTP URL provided as argument and dump result
with Basic_Proc, Argument, Http, Mixed_Str, Int_Img;
procedure T_Http is
  function Code_Image (Code : Integer) return String renames Int_Img;
  Result : Http.Result_Type;
begin

  if Argument.Get_Nbre_Arg /= 1 then
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name & " <URL>");
    Basic_Proc.Set_Error_Exit_Code;
    return;
  end if;

  Result := Http.Get (Argument.Get_Parameter (1));
  case Result.Kind is
    when Http.Ok =>
      Basic_Proc.Put_Output (Result.Content.Image);
      Basic_Proc.Flush_Output;
    when Http.Client_Error =>
      Basic_Proc.Put_Line_Error ("Client error: "
                               & Mixed_Str (Result.Error'Img));
      Basic_Proc.Set_Error_Exit_Code;
    when Http.Server_Error =>
      Basic_Proc.Put_Line_Error ("Server error: "
                                & Code_Image (Result.Code)
                                & " " & Result.Message.Image);
      Basic_Proc.Set_Error_Exit_Code;
  end case;

end T_Http;

