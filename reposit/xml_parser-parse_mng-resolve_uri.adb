with Int_Image, Event_Mng, Http;
separate (Xml_Parser.Parse_Mng)

-- Resolve an URI:
-- if no ':' before first '/' -> Build_Full_Name (Uri), return File
-- if "file://" -> Build_Full_Name (Tail), return File
-- if "http://" -> Fetch content, return String
-- Else Error: unsupported URI scheme
procedure Resolve_Uri (Ctx : in out Ctx_Type;
                       Uri : in Asu_Us;
                       Is_File : out Boolean;
                       Content : out Asu_Us) is
  Ind1, Ind2 : Natural;
  Scheme : Asu_Us;
  File_Scheme : constant String := "file";
  Http_Scheme : constant String := "http";
  Result : Http.Result_Type;
  function Code_Image is new Int_Image (Http.Server_Code_Range);
begin
  Trace ("URI expanding " & Asu_Ts (Uri));
  -- See if first '/' (if any) is ":/"
  Ind1 := String_Mng.Locate (Asu_Ts (Uri), "/");
  Ind2 := String_Mng.Locate (Asu_Ts (Uri), "://");
  if Ind2 = 0 or else Ind1 /= Ind2 + 1 then
    Content := Build_Full_Name (Uri, Ctx.Flow.Curr_Flow.Name);
    Is_File := True;
    Trace ("URI is file " & Asu_Ts (Content));
    return;
  end if;
  -- Now its is xxx://
  Scheme := Asu.Unbounded_Slice (Uri, 1, Ind2 - 1);
  -- Handle "file" scheme
  if Asu_Ts (Scheme) = File_Scheme then
    -- Remove "file://" and build file name with tail
    Content := Build_Full_Name (
       Asu.Delete (Uri, 1, File_Scheme'Length + 3),
       Ctx.Flow.Curr_Flow.Name);
    Is_File := True;
    Trace ("URI expanded as file " & Asu_Ts (Content));
  elsif Asu_Ts (Scheme) = Http_Scheme then
    Trace ("URI fetching through http " & Asu_Ts (Uri));
    -- Handle "http" scheme
    Result := Http.Get (Asu_Ts (Uri));
    -- If Sigterm/Sigint occured, resend
    Event_Mng.Reset_Default_Signals_Policy;
    case Result.Kind is
      when Http.Ok =>
        -- Done
        Is_File := False;
        Content := Result.Content;
      when Http.Client_Error =>
        Util.Error (Ctx.Flow, "Http client error on URI " & Asu_Ts (Uri)
                  & " : " & Mixed_Str (Result.Error'Img));
      when Http.Server_Error =>
        Util.Error (Ctx.Flow, "Http server error on URI " & Asu_Ts (Uri)
                 & " : " & Code_Image (Result.Code)
                 & " " & Asu_Ts (Result.Message));
    end case;
  else
    Util.Error (Ctx.Flow, "Unsupported URI scheme " & Asu_Ts (Scheme));
  end if;
end Resolve_Uri;

