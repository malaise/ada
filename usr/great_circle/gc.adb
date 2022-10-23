with Ada.Characters.Latin_1;
with As.U, As.B, Argument, Basic_Proc, Con_Io, Afpx.Utils, Str_Util, Language,
     Reg_Exp, Trilean;
use all type Trilean.Trilean;
with Units, Lat_Lon, String_Util, Great_Circle, Afpx_Xref;
procedure Gc is

  procedure Usage is
  begin
    Basic_Proc.Put_Line_Output ("Usage: " & Argument.Get_Program_Name
      & " [ -x ] | [ --gui ]      // interactive mode");
    Basic_Proc.Put_Line_Output ("   or: " & Argument.Get_Program_Name
      & " add.mm.ssss/oddd.mm.ssss add.mm.ssss/oddd.mm.ssss");
    Basic_Proc.Put_Line_Output ("   or: " & Argument.Get_Program_Name
      & " add.ijklmn/oddd.ijklmn add.ijklmn/oddd.ijklmn");
    Basic_Proc.Put_Line_Output ("   or: " & Argument.Get_Program_Name
      & " [<context>:]<mapcode> [<context>:]<mapcode>");
    Basic_Proc.Put_Line_Output ("   or: " & Argument.Get_Program_Name
      & " <open_location_code> <open_location_code>");
    Basic_Proc.Put_Line_Output ("   or: " & Argument.Get_Program_Name
      & " <geohash36_code> <geohash36_code>");
    Basic_Proc.Put_Line_Output ("   or: " & Argument.Get_Program_Name
      & " <geohash_code> <geohash_code>");
    Basic_Proc.Put_Line_Output (" where a is N or S and o is E or W.");
  end Usage;

  Use_Afpx : Boolean;

  A, B : Lat_Lon.Lat_Lon_Rad_Rec;
  Heading  : Units.Rad_Coord_Range;
  Distance : String_Util.Distance;

  Get_Handle : Afpx.Get_Handle_Rec;
  Result : Afpx.Result_Rec;

  -- Result of decoding of a data (a point or a mapcode) or of both data
  -- Global: True if both OK, Other if both are empty or if one is empty
  --   and the other is OK, False if an error
  -- Each data: True if OK, Other if empty, False if error
  type Decode_Status_Rec is record
    Global : Trilean.Trilean;
    Data1, Data2 : Trilean.Trilean;
  end record;
  Decode_Status : Decode_Status_Rec;

  Sexa_Pattern : constant String :=
      "[NnSs][0-9]{2}\.[0-9]{2}\.[0-9]{4}/[EeWw][0-9]{3}\.[0-9]{2}\.[0-9]{4}";
  Deci_Pattern : constant String
               := "[NnSs][0-9]{2}\.[0-9]{6}/[EeWw][0-9]{3}\.[0-9]{6}";
  Map_Pattern : constant String
              := "([A-Z-]+:)?(.*[A-Z].*\..+|.+\..*[A-Z].*)";
  Olc_Pattern : constant String
              := "[A-Z0-9]+\+[A-Z0-9]*";
  Gh36_Pattern : constant String
               := "[23456789bBCdDFgGhHjJKlLMnNPqQrRtTVWX]+(@GH36)?";
  Gh_Pattern : constant String
               := "[0123456789bcdefghjkmnpqrstuvwxyz]+(@HH)?";

  Mode_Field  : constant Afpx.Field_Range := Afpx_Xref.Main.Mode;
  subtype A_Flds is Afpx.Field_Range
                    range Afpx_Xref.Main.A_First .. Afpx_Xref.Main.A_Last;
  subtype B_Flds is Afpx.Field_Range
                    range Afpx_Xref.Main.B_First .. Afpx_Xref.Main.B_Last;
  subtype Map_Flds is Afpx.Field_Range
                    range Afpx_Xref.Main.A_Ctx .. Afpx_Xref.Main.B_Map;
  subtype Code_Flds is Afpx.Field_Range
                    range Afpx_Xref.Main.A_Code .. Afpx_Xref.Main.B_Code;
  Heading_Ab_Field  : constant Afpx.Field_Range := Afpx_Xref.Main.Heading;
  Distance_Field  : constant Afpx.Field_Range := Afpx_Xref.Main.Distance;
  Heading_Ba_Field  : constant Afpx.Field_Range := Afpx_Xref.Main.Revert;
  Sexa_Field : constant Afpx.Field_Range := Afpx_Xref.Main.To_Sexa;
  Deci_Field : constant Afpx.Field_Range := Afpx_Xref.Main.To_Deci;
  Map_Field : constant Afpx.Field_Range := Afpx_Xref.Main.To_Map;
  Olc_Field : constant Afpx.Field_Range := Afpx_Xref.Main.To_Olc;
  Gh36_Field : constant Afpx.Field_Range := Afpx_Xref.Main.To_Gh36;
  Gh_Field : constant Afpx.Field_Range := Afpx_Xref.Main.To_Gh;
  Clear_Field  : constant Afpx.Field_Range := Afpx_Xref.Main.Clear;
  Compute_Field  : constant Afpx.Field_Range := Afpx_Xref.Main.Compute;
  Exit_Field  : constant Afpx.Field_Range := Afpx_Xref.Main.Quit;

  type Mode_List is (Sexa_Mode, Deci_Mode, Map_Mode, Olc_Mode, Gh36_Mode,
                     Gh_Mode);
  subtype Code_Kind_List is Mode_List range Olc_Mode .. Gh_Mode;
  Mode : Mode_List := Sexa_Mode;
  Need_Clean : Boolean := False;

  Deg : constant String := "°"; --## rule line off Char

  use type Afpx.Field_Range, Afpx.Event_List, Afpx.Keyboard_Key_List;

  procedure Clear_Result is
  begin
    Afpx.Clear_Field (Heading_Ab_Field);
    Afpx.Clear_Field (Distance_Field);
    Afpx.Clear_Field (Heading_Ba_Field);
  end Clear_Result;

  Degree_Sign : constant Wide_Character
              := Language.Char_To_Wide (Ada.Characters.Latin_1.Degree_Sign);

  procedure Reset_Field (Field : in Afpx.Absolute_Field_Range) is
    Char : Wide_Character;
  begin
    Afpx.Reset_Field (Field);
    if Mode /= Deci_Mode then
      return;
    end if;
    if Afpx.Get_Field_Width (Field) <= 2 then
      Char := Afpx.Decode_Wide_Field (Field, 0)(1);
      -- "o" -> ".", "'" -> " " and """[/]" becomes "o[/]"
      if Char = Language.String_To_Wide(Deg)(1) then
        Afpx.Encode_Wide_Field (Field, (0, 0), ".");
      elsif Char = ''' then
        Afpx.Encode_Wide_Field (Field, (0, 0), " ");
      elsif Char = '"' then
        Afpx.Encode_Wide_Field (Field, (0, 0), Degree_Sign & "");
      end if;
    end if;
  end Reset_Field;

  procedure Reset is
  begin
    -- Deactivate / clear fields
    if Mode = Map_Mode or else Mode >= Olc_Mode then
      for Field in A_Flds loop
        Afpx.Set_Field_Activation (Field, False);
      end loop;
      for Field in B_Flds loop
        Afpx.Set_Field_Activation (Field, False);
      end loop;
      if Mode = Map_Mode then
        for Field in Map_Flds loop
          Reset_Field (Field);
        end loop;
        for Field in Code_Flds loop
          Afpx.Set_Field_Activation (Field, False);
        end loop;
      else
        for Field in Map_Flds loop
          Afpx.Set_Field_Activation (Field, False);
        end loop;
        for Field in Code_Flds loop
          Reset_Field (Field);
        end loop;
      end if;
    else
      for Field in A_Flds loop
        Reset_Field (Field);
      end loop;
      for Field in B_Flds loop
        Reset_Field (Field);
      end loop;
      for Field in Map_Flds loop
        Afpx.Set_Field_Activation (Field, False);
      end loop;
      for Field in Code_Flds loop
        Afpx.Set_Field_Activation (Field, False);
      end loop;
    end if;
    Clear_Result;
    -- Update Mode text and Switch buttons
    for Field in Sexa_Field .. Gh_Field loop
      Afpx.Utils.Protect_Field (Field, False);
    end loop;
    case Mode is
      when Sexa_Mode =>
        Afpx.Encode_Field (Mode_Field, (0, 0), "Sexigesimal mode");
        Afpx.Utils.Protect_Field (Sexa_Field, True);
      when Deci_Mode =>
        Afpx.Encode_Field (Mode_Field, (0, 0), "Decimal mode    ");
        Afpx.Utils.Protect_Field (Deci_Field, True);
      when Map_Mode =>
        Afpx.Encode_Field (Mode_Field, (0, 0), "Mapcode mode    ");
        Afpx.Utils.Protect_Field (Map_Field, True);
      when Olc_Mode =>
        Afpx.Encode_Field (Mode_Field, (0, 0), "Open Loc mode   ");
        Afpx.Utils.Protect_Field (Olc_Field, True);
      when Gh36_Mode =>
        Afpx.Encode_Field (Mode_Field, (0, 0), "Geohash36 mode  ");
        Afpx.Utils.Protect_Field (Gh36_Field, True);
      when Gh_Mode =>
        Afpx.Encode_Field (Mode_Field, (0, 0), "Geohash36 mode  ");
        Afpx.Utils.Protect_Field (Gh_Field, True);
    end case;
    if Mode = Map_Mode then
      Get_Handle.Cursor_Field := Map_Flds'First;
    elsif Mode >= Olc_Mode then
      Get_Handle.Cursor_Field := Code_Flds'First;
    else
      Get_Handle.Cursor_Field := A_Flds'First;
    end if;
    Get_Handle.Cursor_Col := 0;
    Get_Handle.Insert := False;
  end Reset;

  -- Clear result fields during input
  function Next_Field_Cb (Unused_Cursor_Field : Afpx.Field_Range;
                          Unused_New_Field : Boolean;
                          Cursor_Col : Con_Io.Col_Range;
                          Unused_Offset : Con_Io.Col_Range;
                          Enter_Field_Cause : Afpx.Enter_Field_Cause_List;
                          Str : Afpx.Unicode_Sequence) return Con_Io.Col_Range is
    Last : Con_Io.Col_Range;
    use type Afpx.Enter_Field_Cause_List;
  begin
    if Need_Clean then
      Clear_Result;
      Need_Clean := False;
    end if;
    if Enter_Field_Cause = Afpx.Left then
      return Afpx.Last_Index (Str, False);
    elsif Enter_Field_Cause = Afpx.Mouse
    or else Enter_Field_Cause = Afpx.Selection then
      if Enter_Field_Cause /= Afpx.Selection then
        Afpx.Set_Selection (Language.Unicode_To_String (Str));
      end if;
      Last := Afpx.Last_Index (Str, True);
      if Cursor_Col <= Last then
        return Cursor_Col;
      else
        return Last;
      end if;
    else
      return 0;
    end if;
  end Next_Field_Cb;

  -- Decode a point, sexa or deci
  procedure Decode_Point (First_Fld, Last_Fld : in Afpx.Field_Range;
                          Point : out Lat_Lon.Lat_Lon_Rad_Rec;
                          Status : out Trilean.Trilean;
                          Cursor : in out Afpx.Field_Range) is
    -- Two '"' added and two 'o' instead of '.' in Afpx screen
    Point_Txt : As.B.Asb_Bs (String_Util.Geo_Str'Length+4);
    Default_Content : Boolean;

    -- Replace trailing spaces by '0' for decimal numeric fields (len = 4)
    function Pad_Field (Field : Afpx.Field_Range) return String is
      Default : constant Language.Unicode_Sequence
              := Afpx.Get_Init_Field (Field, 0);
      Content : constant Language.Unicode_Sequence
              := Afpx.Decode_Field (Field, 0);
      Txt : constant String := Language.Unicode_To_String (Content);
      Str : String := Txt;
      use type Language.Unicode_Sequence;
    begin
      if Afpx.Is_Get_Kind (Field) and then Content /= Default then
        Default_Content := False;
      end if;
      if Str'Length = 4 then
        for I in reverse Str'Range loop
          exit when Str(I) /= ' ';
          Str(I) := '0';
        end loop;
      end if;
      return Str;
    end Pad_Field;

  begin
    Point_Txt.Set_Null;
    Default_Content := True;
    for Field in First_Fld .. Last_Fld loop
      Point_Txt.Append (Pad_Field(Field));
    end loop;
    if Default_Content then
      Great_Circle.Logger.Log_Debug ("Point is empty");
      Status := Other;
      Cursor := First_Fld;
      return;
    end if;

    Great_Circle.Logger.Log_Debug ("Decoded point: " & Point_Txt.Image);
    if Mode = Sexa_Mode then
      -- Replace Nddomm'ssss"/Edddomm'ssss" by Ndd.mm.ssss/Eddd.mm.ssss
      Point_Txt.Set (Str_Util.Substit (Point_Txt.Image, Deg, "."));
      Point_Txt.Set (Str_Util.Substit (Point_Txt.Image, "'", "."));
      Point_Txt.Set (Str_Util.Substit (Point_Txt.Image, """", ""));
      Great_Circle.Logger.Log_Debug ("Parsed point: " & Point_Txt.Image);
      Point := Lat_Lon.Geo2Rad (String_Util.Str2Geo(Point_Txt.Image));
    else -- Deci_Mode
      -- Replace Ndd.ij klmno/Eddd.ij klmno by Ndd.ijklmn/Eddd.ijklmn
      Point_Txt.Set (Str_Util.Substit (Point_Txt.Image, Deg, ""));
      Point_Txt.Set (Str_Util.Substit (Point_Txt.Image, " ", ""));
      Great_Circle.Logger.Log_Debug ("Parsed point: " & Point_Txt.Image);
      Point := Lat_Lon.Dec2Rad (String_Util.Str2Dec(Point_Txt.Image));
    end if;
    Great_Circle.Logger.Log_Debug ("Got point OK");
    Status := True;
  exception
    when others =>
      Great_Circle.Logger.Log_Debug ("Decode point Exception");
      Status := False;
      Cursor := First_Fld;
  end Decode_Point;

  -- Decode a mapcode
  -- 6 for context, ":" and 18 for mapcode
  subtype Mapcode_Txt is As.B.Asb_Bs(25);
  procedure Decode_Mapcode (First_Fld, Last_Fld : in Afpx.Field_Range;
                            Point : out Lat_Lon.Lat_Lon_Rad_Rec;
                            Mapcode : out Mapcode_Txt;
                            Status : out Trilean.Trilean;
                            Cursor : in out Afpx.Field_Range) is
    Txt : As.U.Asu_Us;
    Default_Content : Boolean;
  begin
    Mapcode.Set_Null;
    Default_Content := True;
    for Field in First_Fld .. Last_Fld loop
      if Afpx.Is_Get_Kind (Field) and then
             Afpx.Decode_Field  (Field, 0, False)
          /= Afpx.Get_Init_Field (Field, 0, False) then
        Default_Content := False;
      end if;
      if Afpx.Is_Get_Kind (Field) or else not Txt.Is_Null then
        -- Skip ":" put field if no prefix
        Txt.Set (Str_Util.Strip (
          Afpx.Decode_Field(Field, 0, False), Str_Util.Both));
      end if;
      Mapcode.Append (Txt.Image);
    end loop;
    if Default_Content then
      Great_Circle.Logger.Log_Debug ("Mapcode is empty");
      Status := Other;
      Cursor := First_Fld;
      return;
    end if;
    Great_Circle.Logger.Log_Debug ("Parsed mapcode: " & Mapcode.Image);
    Point := Lat_Lon.Mapcode2Rad (Mapcode.Image);
    Great_Circle.Logger.Log_Debug ("Got point OK");
    Status := True;
  exception
    when others =>
      Great_Circle.Logger.Log_Debug ("Decode mapcode Exception");
      Status := False;
      Cursor := First_Fld;
  end Decode_Mapcode;

  -- Decode an open location code, geohash36 or geohash
  subtype Code_Txt is As.B.Asb_Bs(16);
  procedure Decode_Code (Field : in Afpx.Field_Range;
                         Kind : in Code_Kind_List;
                         Point : out Lat_Lon.Lat_Lon_Rad_Rec;
                         Code : out Code_Txt;
                         Status : out Trilean.Trilean;
                         Cursor : in out Afpx.Field_Range) is
    Default_Content : Boolean;
  begin
    Code.Set_Null;
    Default_Content := True;
    if Afpx.Decode_Field  (Field, 0, False)
        /= Afpx.Get_Init_Field (Field, 0, False) then
      Default_Content := False;
    end if;
    Code.Set (Str_Util.Strip (
          Afpx.Decode_Field(Field, 0, False), Str_Util.Both));
    if Default_Content then
      Great_Circle.Logger.Log_Debug ("Code is empty");
      Status := Other;
      Cursor := Field;
      return;
    end if;
    Great_Circle.Logger.Log_Debug ("Parsed Code: " & Code.Image);
    case Kind is
      when Olc_Mode =>
        Point := Lat_Lon.Olc2Rad (Code.Image);
      when Gh36_Mode =>
        Point := Lat_Lon.Gh362Rad (Code.Image);
      when Gh_Mode =>
        Point := Lat_Lon.Gh2Rad (Code.Image);
    end case;
    Great_Circle.Logger.Log_Debug ("Got point OK");
    Status := True;
  exception
    when others =>
      Great_Circle.Logger.Log_Debug ("Decode code Exception");
      Status := False;
      Cursor := Field;
  end Decode_Code;

  -- Decode points, mapcodes or olcs, set A and B. Return
  -- Ok if both are OK, Empty if one is Ok and the other is empty
  function Decode return Decode_Status_Rec is
    Status : Decode_Status_Rec;
    Mapa, Mapb : Mapcode_Txt;
    Code_A, Code_B : Code_Txt;
  begin
    Get_Handle.Cursor_Col := 0;
    Get_Handle.Insert := False;
    Clear_Result;
    -- Decode both points/maps in order to set Default_Content
    if Mode < Olc_Mode and then Mode /= Map_Mode then
      -- Coordinates
      Get_Handle.Cursor_Field := A_Flds'First;
      Decode_Point (A_Flds'First, A_Flds'Last, A, Status.Data1,
                    Get_Handle.Cursor_Field);
      if Status.Data1 /= False then
        Get_Handle.Cursor_Field := B_Flds'First;
        Decode_Point (B_Flds'First, B_Flds'Last, B, Status.Data2,
                      Get_Handle.Cursor_Field);
      end if;
    elsif Mode = Map_Mode then
      -- Mapcode
      Get_Handle.Cursor_Field := Map_Flds'First;
      Decode_Mapcode (Map_Flds'First, Map_Flds'First + 2, A, Mapa,
                      Status.Data1, Get_Handle.Cursor_Field);
      if Status.Data1 /= False then
        Get_Handle.Cursor_Field := Map_Flds'First + 3;
        Decode_Mapcode (Map_Flds'First + 3, Map_Flds'Last, B, Mapb,
                        Status.Data2, Get_Handle.Cursor_Field);
      end if;
      if Status.Data1 = True and then Status.Data2 = True then
        Afpx.Set_Selection (Mapa.Image & " " & Mapb.Image);
      end if;
    else
      -- Olc, GH36 or GH
      Get_Handle.Cursor_Field := Code_Flds'First;
      Decode_Code (Code_Flds'First, Mode, A, Code_A,
                  Status.Data1, Get_Handle.Cursor_Field);
      if Status.Data1 /= False then
        Get_Handle.Cursor_Field := Code_Flds'Last;
        Decode_Code (Code_Flds'Last, Mode, B, Code_B,
                    Status.Data2, Get_Handle.Cursor_Field);
      end if;
      if Status.Data1 = True and then Status.Data2 = True then
        Afpx.Set_Selection (Code_A.Image & " " & Code_B.Image);
      end if;
    end if;
    Great_Circle.Logger.Log_Debug (
          "End of decoding, status: " & Status.Data1'Img
        & " " & Status.Data2'Img);
    if Status.Data1 = True and then Status.Data2 = True then
      Great_Circle.Logger.Log_Debug ("Got point A:" & A.X'Img & A.Y'Img);
      Great_Circle.Logger.Log_Debug ("Got point B:" & B.X'Img & B.Y'Img);
    end if;
    if Status.Data1 = True and then Status.Data2 = True then
      Status.Global := True;
    elsif   (Status.Data1 = True  and then Status.Data2 = Other)
    or else (Status.Data1 = Other and then Status.Data2 = True)
    or else (Status.Data1 = Other and then Status.Data2 = Other) then
      Status.Global := Other;
    else
      Status.Global := False;
    end if;
    return Status;
  end Decode;

  -- Encode a point in degree of decimal
  procedure Encode_Point (First_Fld, Last_Fld : in Afpx.Field_Range;
                          Point : in Lat_Lon.Lat_Lon_Rad_Rec) is
    Str : constant String := (
        if Mode = Sexa_Mode then String_Util.Geo2Str (Lat_Lon.Rad2Geo (Point))
        else                     String_Util.Dec2Str (Lat_Lon.Rad2Dec (Point)));
    Index : Positive := Str'First;
    Len : Positive;
  begin
    for Field in First_Fld .. Last_Fld loop
      Len := Afpx.Get_Field_Width (Field);
      if Afpx.Is_Get_Kind (Field) then
        Afpx.Encode_Field (Field, (0, 0), Str(Index .. Index + Len - 1));
      end if;
      -- Index of next field,
      if Afpx.Is_Get_Kind (Field) then
        Index := Index + Len;
      elsif Mode = Sexa_Mode then
        -- Skip the last "" (in '"/')
        Index := Index + 1;
      elsif Mode = Deci_Mode then
        -- Skip the space in the middle of deci part
        --  and the Deg at the end ("o/")
        for Char of String'(Afpx.Decode_Field (Field, 0, False)) loop
          if Char = '.' or else Char = '/' then
            Index := Index + 1;
          end if;
        end loop;
      end if;
    end loop;
  end Encode_Point;

  -- Encode a mapcode
  procedure Encode_Mapcode (Unused_First_Fld, Last_Fld : in Afpx.Field_Range;
                            Point : in Lat_Lon.Lat_Lon_Rad_Rec) is
    Code : constant String := Lat_Lon.Rad2Mapcode (Point);
  begin
    Afpx.Encode_Field (Last_Fld, (0, 0), Code);
  end Encode_Mapcode;

  -- Encode a olc
  procedure Encode_Olc (First_Fld, Unused_Last_Fld : in Afpx.Field_Range;
                        Point : in Lat_Lon.Lat_Lon_Rad_Rec) is
    Code : constant String := Lat_Lon.Rad2Olc (Point);
  begin
    Afpx.Encode_Field (First_Fld, (0, 0), Code);
  end Encode_Olc;

  -- Encode a Gh36 code
  procedure Encode_Gh36 (First_Fld, Unused_Last_Fld : in Afpx.Field_Range;
                         Point : in Lat_Lon.Lat_Lon_Rad_Rec) is
    Code : constant String := Lat_Lon.Rad2Gh36 (Point);
  begin
    Afpx.Encode_Field (First_Fld, (0, 0), Code);
  end Encode_Gh36;

  -- Encode a Gh code
  procedure Encode_Gh (First_Fld, Unused_Last_Fld : in Afpx.Field_Range;
                       Point : in Lat_Lon.Lat_Lon_Rad_Rec) is
    Code : constant String := Lat_Lon.Rad2Gh (Point);
  begin
    Afpx.Encode_Field (First_Fld, (0, 0), Code);
  end Encode_Gh;

  -- Encode A and B as points or mapcodes
  procedure Encode (Encode1, Encode2 : in Boolean) is
  begin
    if Mode = Map_Mode then
      if Encode1 then
        Encode_Mapcode (Map_Flds'First,     Map_Flds'First + 2, A);
      end if;
      if Encode2 then
        Encode_Mapcode (Map_Flds'First + 3, Map_Flds'Last,      B);
      end if;
    elsif Mode = Olc_Mode then
      if Encode1 then
        Encode_Olc (Code_Flds'First, Code_Flds'First, A);
      end if;
      if Encode2 then
        Encode_Olc (Code_Flds'Last,  Code_Flds'Last,  B);
      end if;
    elsif Mode = Gh36_Mode then
      if Encode1 then
        Encode_Gh36 (Code_Flds'First, Code_Flds'First, A);
      end if;
      if Encode2 then
        Encode_Gh36 (Code_Flds'Last,  Code_Flds'Last,  B);
      end if;
    elsif Mode = Gh_Mode then
      if Encode1 then
        Encode_Gh (Code_Flds'First, Code_Flds'First, A);
      end if;
      if Encode2 then
        Encode_Gh (Code_Flds'Last,  Code_Flds'Last,  B);
      end if;
    else
      if Encode1 then
        Encode_Point (A_Flds'First, A_Flds'Last, A);
      end if;
      if Encode2 then
        Encode_Point (B_Flds'First, B_Flds'Last, B);
      end if;
    end if;
  end Encode;

  -- Encode Heading (in degrees, with special degree char, if mode is not
  --  decimal)
  procedure Encode_Heading (F : in Afpx.Field_Range;
                            H : in Units.Rad_Coord_Range) is
  begin
    if Mode /= Deci_Mode then
      -- Sexa or code
      declare
        Str : constant String := String_Util.Geoangle2Str(Units.Rad2Geo(H));
        -- Will append " and set o and ' instead of 2 first .
        Wstr : Wide_String (1 .. Str'Length + 1);
      begin
        Wstr := Language.String_To_Wide (Str) & '"';
        Wstr(4) := Language.Char_To_Wide (
                     Ada.Characters.Latin_1.Degree_Sign);
        Wstr(7) := ''';
        Afpx.Encode_Wide_Field (F, (0, 0), Wstr);
      end;
    else
      -- Deci
      declare
        Str : constant String := String_Util.Decangle2Str(Units.Rad2Dec(H));
        -- Will append o
        Wstr : Wide_String (1 .. Str'Length + 1);
      begin
        Wstr := Language.String_To_Wide (Str) & Degree_Sign;
        Afpx.Encode_Wide_Field (F, (0, 0), Wstr);
      end;
      Great_Circle.Logger.Log_Debug ("Heading encoded");
    end if;
  end Encode_Heading;

begin
  Great_Circle.Init_Logger;
  if Argument.Get_Nbre_Arg = 1
    and then (Argument.Get_Parameter = "-h"
      or else Argument.Get_Parameter = "--help") then
    Usage;
    return;
  elsif Argument.Get_Nbre_Arg = 0 then
    Use_Afpx := True;
  elsif Argument.Get_Nbre_Arg = 1
    and then (Argument.Get_Parameter = "-x"
      or else Argument.Get_Parameter = "--gui") then
    Use_Afpx := True;
  elsif Argument.Get_Nbre_Arg = 2 then
    Use_Afpx := False;
  else
    Basic_Proc.Put_Line_Error ("ERROR: Invalid argument.");
    Basic_Proc.Set_Error_Exit_Code;
    Usage;
    return;
  end if;

  -- Convert args in lat_lon of A and B
  if not Use_Afpx then
    -- See if Ndd.mm.ss or Ndd.ijkl
    if Reg_Exp.Match (Sexa_Pattern, Argument.Get_Parameter(1), True)
    and then Reg_Exp.Match (Sexa_Pattern, Argument.Get_Parameter(2), True) then
      Mode := Sexa_Mode;
    elsif Reg_Exp.Match (Deci_Pattern, Argument.Get_Parameter(1), True)
    and then Reg_Exp.Match (Deci_Pattern, Argument.Get_Parameter(2), True) then
      Mode := Deci_Mode;
    elsif Reg_Exp.Match (Map_Pattern, Argument.Get_Parameter(1), True)
    and then Reg_Exp.Match (Map_Pattern, Argument.Get_Parameter(2), True) then
      Mode := Map_Mode;
    elsif Reg_Exp.Match (Olc_Pattern, Argument.Get_Parameter(1), True)
    and then Reg_Exp.Match (Olc_Pattern, Argument.Get_Parameter(2), True) then
      Mode := Olc_Mode;
    elsif Reg_Exp.Match (Gh36_Pattern, Argument.Get_Parameter(1), True)
    and then Reg_Exp.Match (Gh36_Pattern, Argument.Get_Parameter(2), True) then
      Mode := Gh36_Mode;
    elsif Reg_Exp.Match (Gh_Pattern, Argument.Get_Parameter(1), True)
    and then Reg_Exp.Match (Gh_Pattern, Argument.Get_Parameter(2), True) then
      Mode := Gh_Mode;
    else
      Basic_Proc.Put_Line_Error ("ERROR: Invalid argument.");
      Basic_Proc.Set_Error_Exit_Code;
      Usage;
      return;
    end if;
    if (Mode = Gh36_Mode
      and then Reg_Exp.Match (Gh_Pattern, Argument.Get_Parameter(1), True)
      and then Reg_Exp.Match (Gh_Pattern, Argument.Get_Parameter(2), True) )
    or else (Mode = Gh_Mode
      and then Reg_Exp.Match (Gh36_Pattern, Argument.Get_Parameter(1), True)
      and then Reg_Exp.Match (Gh36_Pattern, Argument.Get_Parameter(2), True) )
    then
      Basic_Proc.Put_Line_Error ("ERROR: Ambiguous argument.");
      Basic_Proc.Set_Error_Exit_Code;
      Usage;
      return;
    end if;

    Great_Circle.Logger.Log_Debug ("Mode: " & Mode'Img);
    begin
      -- Parse arguments
      if Mode = Sexa_Mode then
        A := Lat_Lon.Geo2Rad (String_Util.Str2Geo(Argument.Get_Parameter(1)));
        B := Lat_Lon.Geo2Rad (String_Util.Str2Geo(Argument.Get_Parameter(2)));
      elsif Mode = Deci_Mode then
        A := Lat_Lon.Dec2Rad (String_Util.Str2Dec(Argument.Get_Parameter(1)));
        B := Lat_Lon.Dec2Rad (String_Util.Str2Dec(Argument.Get_Parameter(2)));
      elsif Mode = Map_Mode then
        -- Coordinates of mapcodes
        A := Lat_Lon.Mapcode2Rad (Argument.Get_Parameter(1));
        B := Lat_Lon.Mapcode2Rad (Argument.Get_Parameter(2));
      elsif Mode = Olc_Mode then
        -- Coordinates of open location codes
        A := Lat_Lon.Olc2Rad (Argument.Get_Parameter(1));
        B := Lat_Lon.Olc2Rad (Argument.Get_Parameter(2));
      elsif Mode = Gh36_Mode then
        -- Coordinates of geohash36 codes
        A := Lat_Lon.Gh362Rad (Argument.Get_Parameter(1));
        B := Lat_Lon.Gh362Rad (Argument.Get_Parameter(2));
      elsif Mode = Gh_Mode then
        -- Coordinates of geohash codes
        A := Lat_Lon.Gh2Rad (Argument.Get_Parameter(1));
        B := Lat_Lon.Gh2Rad (Argument.Get_Parameter(2));
      end if;
      Great_Circle.Logger.Log_Debug ("Got point A:" & A.X'Img & A.Y'Img);
      Great_Circle.Logger.Log_Debug ("Got point B:" & B.X'Img & B.Y'Img);
      -- Compute
      Great_Circle.Compute_Route(A, B, Heading, Distance);
      -- Put result
      if Mode /= Deci_Mode then
        -- Sexa or Code
        Basic_Proc.Put_Output ("Route: "
            & String_Util.Geoangle2Str(Units.Rad2Geo(Heading)));
      else
        -- Deci
        Basic_Proc.Put_Output ("Route: "
            & String_Util.Decangle2Str(Units.Rad2Dec(Heading)));
      end if;
      Basic_Proc.Put_Line_Output ("   Distance(Nm): "
                        & String_Util.Dist2Str(Distance));
    exception
      when others =>
        Basic_Proc.Set_Error_Exit_Code;
        Usage;
        return;
    end;
  else
    Afpx.Use_Descriptor (Afpx_Xref.Main.Dscr_Num);
    Get_Handle := (others => <>);
    Reset;
    loop
      Afpx.Put_Then_Get (Get_Handle, Result, False, Next_Field_Cb'Access);

      if (Result.Event = Afpx.Keyboard
          and then Result.Keyboard_Key = Afpx.Break_Key)
      or else (Result.Event = Afpx.Mouse_Button
               and then Result.Field_No = Exit_Field) then
        -- Exit
        exit;
      elsif (Result.Event = Afpx.Keyboard
             and then Result.Keyboard_Key = Afpx.Escape_Key)
      or else (Result.Event = Afpx.Mouse_Button
               and then Result.Field_No = Clear_Field) then
        -- Reset
        Reset;
      elsif (Result.Event = Afpx.Keyboard
          and then Result.Keyboard_Key = Afpx.Return_Key)
      or else (Result.Event = Afpx.Mouse_Button
               and then Result.Field_No = Compute_Field) then
        -- Decode input
        Decode_Status := Decode;
        if Decode_Status.Global = True then
          -- Compute
          Great_Circle.Compute_Route(A => A, B => B,
                                     Head => Heading, Dist => Distance);
          Encode_Heading (Heading_Ab_Field, Heading);
          Afpx.Encode_Field (Distance_Field, (0, 0),
                             String_Util.Dist2Str(Distance));
          Great_Circle.Logger.Log_Debug ("Distance encoded");
          -- Compute reverse heading
          Great_Circle.Compute_Route(A => B, B => A,
                                     Head => Heading, Dist => Distance);
          Encode_Heading (Heading_Ba_Field, Heading);
          -- Clean the result fields at next cursor change field
          Need_Clean := True;
        end if;
      elsif Result.Event = Afpx.Mouse_Button then
        -- Switches
        -- Decode current point / mapcodes
        Decode_Status := Decode;
        if Decode_Status.Global /= False then
          -- Switch
          case Result.Field_No is
            when Sexa_Field =>
              Mode := Sexa_Mode;
            when Deci_Field =>
              Mode := Deci_Mode;
            when Map_Field =>
              Mode := Map_Mode;
            when Olc_Field =>
              Mode := Olc_Mode;
            when Gh36_Field =>
              Mode := Gh36_Mode;
            when Gh_Field =>
              Mode := Gh_Mode;
            when others =>
              null;
          end case;
          -- Clear
          Reset;
          -- Encode points / mapcodes
          Encode (Decode_Status.Data1 = True, Decode_Status.Data2 = True);
        end if;
      end if;

    end loop;

  end if;

end Gc;

