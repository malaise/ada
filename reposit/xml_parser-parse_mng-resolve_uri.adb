with Int_Image, Event_Mng, Http;
separate (Xml_Parser.Parse_Mng)

-- Resolve an URI:
-- if no ':' before first '/' -> Build_Full_Name (Uri), return File
-- if "file://" -> Build_Full_Name (Tail), return File
-- if "http://" -> Fetch content, return String
-- Else Error: unsupported URI scheme
procedure Resolve_Uri (Ctx : in out Ctx_Type;
                       Uri : in As.U.Asu_Us;
                       Is_File : out Boolean;
                       Content : out As.U.Asu_Us) is
  Ind1, Ind2 : Natural;
  Scheme, Luri : As.U.Asu_Us;
  File_Scheme : constant String := "file";
  Http_Scheme : constant String := "http";
  Result : Http.Result_Type;
  function Code_Image is new Int_Image (Http.Server_Code_Range);
begin
  Trace ("URI expanding " & Uri.Image);
  -- See if first '/' (if any) is ":/"
  Ind1 := Str_Util.Locate (Uri.Image, "/");
  Ind2 := Str_Util.Locate (Uri.Image, "://");
  if Ind2 = 0 or else Ind1 /= Ind2 + 1 then
    Content := Build_Full_Name (Uri, Ctx.Flow.Curr_Flow.Name);
    Is_File := True;
    Trace ("URI is file " & Content.Image);
    return;
  end if;
  -- Now its is xxx://
  Scheme := Uri.Uslice (1, Ind2 - 1);
  -- Handle "file" scheme
  if Scheme.Image = File_Scheme then
    -- Remove "file://" and build file name with tail
    Luri := Uri;
    Luri.Delete (1, File_Scheme'Length + 3);
    Content := Build_Full_Name (Luri, Ctx.Flow.Curr_Flow.Name);
    Is_File := True;
    Trace ("URI expanded as file " & Content.Image);
  elsif Scheme.Image = Http_Scheme then
    Trace ("URI fetching through http " & Uri.Image);
    -- Handle "http" scheme
    Result := Http.Get (Uri.Image);
    -- If Sigterm/Sigint occured, resend
    Event_Mng.Reset_Default_Signals_Policy;
    case Result.Kind is
      when Http.Ok =>
        -- Done
        Is_File := False;
        Content := Result.Content;
      when Http.Client_Error =>
        Util.Error (Ctx.Flow, "Http client error on URI " & Uri.Image
                  & " : " & Mixed_Str (Result.Error'Img));
      when Http.Server_Error =>
        Util.Error (Ctx.Flow, "Http server error on URI " & Uri.Image
                 & " : " & Code_Image (Result.Code)
                 & " " & Result.Message.Image);
    end case;
  else
    Util.Error (Ctx.Flow, "Unsupported URI scheme " & Scheme.Image);
  end if;
end Resolve_Uri;

