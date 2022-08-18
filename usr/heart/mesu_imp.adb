with Ada.Calendar, Ada.Exceptions;
with As.U.Utils, Directory, Upper_Str, Basic_Proc, Sys_Calls,
     Trace.Loggers, Images, Dynamic_List, Xml_Parser, Date_Text, Get_Line;
with Pers_Def;
package body Mesu_Imp is

  Logger : Trace.Loggers.Logger;


  -- Import Tcx file
  -- Import Date, and Samples according to Sampling_Delta, from .tcx file
  procedure Import_Tcx (File_Name : in String;
                        Mesure : in out Mesu_Def.Mesure_Rec;
                        Ok : out Boolean) is
    -- Tempo result
    Res : Mesu_Def.Mesure_Rec;
    -- XML parsing
    Ctx : Xml_Parser.Ctx_Type;
    Root, Lap : Xml_Parser.Element_Type;
    Node : Xml_Parser.Node_Type;
    Txt : As.U.Asu_Us;
    -- Tempo date
    Date : Date_Text.Date_Rec;
    -- yyyy-mm-ddThh:mm:ss
    Date_Len : constant := 19;
    -- First time
    Init_Time : Ada.Calendar.Time;
    -- Points collected from file
    type Point_Rec is record
      Time : Ada.Calendar.Time;
      Bmp : Pers_Def.Bpm_Range := Pers_Def.Bpm_Range'First;
    end record;
    package Point_List_Mng is new Dynamic_List (Point_Rec);
    Points : Point_List_Mng.Dyn_List.List_Type;
    Point : Point_Rec;
    Res_Index : Mesu_Def.Sample_Nb_Range;
    use type Ada.Calendar.Time;
  begin
    Logger.Init ("Import");
    -- Init result
    Res := Mesure;
    Res.Samples := (others => Pers_Def.Bpm_Range'First);
    -- Parse
    Logger.Log_Debug ("Parsing file " & File_Name
                      & " with delta" & Mesure.Sampling_Delta'Img);
    Ctx.Parse (File_Name, Ok);
    if not Ok then
      Basic_Proc.Put_Line_Error ("Parse error in Tcx: "
                                   & Ctx.Get_Parse_Error_Message);
      return;
    end if;
    Ok := False;
    Root := Ctx.Get_Root_Element;
    -- Root is TrainingCenterDatabase and must have a child Activities
    for C of Ctx.Get_Children (Root) loop
      if Ctx.Get_Name (C) = "Activities" then
        Node := C;
        exit;
      end if;
    end loop;
    if not Xml_Parser.Is_Valid (Node) then
      -- No Activities
      Basic_Proc.Put_Line_Error ("No Activities Tcx");
      return;
    end if;
    -- Activities must have first child Activity
    if Ctx.Get_Nb_Children (Node) = 0 then
      Basic_Proc.Put_Line_Error ("Activities has no child");
      return;
    end if;
    Root := Ctx.Get_Child (Node, 1);
    if Ctx.Get_Name (Root) /= "Activity" then
      Basic_Proc.Put_Line_Error ("Activities has invalid first child "
          & Ctx.Get_Name (Root));
      return;
    end if;
    -- Root is now Activity, with one Id, some Lap and possibly other fields

    -- Id is the data ISO: 2022-08-17T08:33:56 possibly Zulu
    Node := Ctx.Get_Child (Ctx.Get_Child (Root, 1), 1);
    Txt := Ctx.Get_Text (Node);
    declare
      Time_Local : Ada.Calendar.Time;
    begin
      -- Scan and convert to local time if necessary
      Date := Date_Text.Scan (Txt.Slice (1, Date_Len), "%Y-%m-%dT%H:%M:%S");
      Logger.Log_Debug ("Got Id with date " & Txt.Image);
      Init_Time := Date_Text.Pack (Date);
      if Txt.Element (Txt.Length) = 'Z' then
        Time_Local := Init_Time + Sys_Calls.Gmt_Offset;
        Date := Date_Text.Split (Time_Local);
      end if;
      -- And put date (skip hours, minutes, secs)
      Res.Date := Date_Text.Put (Date, "%Y%m%d");
      Logger.Log_Debug ("Setting date to " & Res.Date);
    exception
      when others =>
        Basic_Proc.Put_Line_Error ("Invalid date in Id " & Txt.Image);
        return;
    end;

    -- Scan all Lap and all Track and all Tackpoint with a HeartRateBpm
    for I in 2 .. Ctx.Get_Nb_Children (Root) loop
      Lap := Ctx.Get_Child (Root, I);
      exit when Ctx.Get_Name (Lap) /= "Lap";
      for Lap_Child of Ctx.Get_Children (Lap) loop
        if Ctx.Get_Name (Lap_Child) = "Track" then
          for Trackpoint of Ctx.Get_Children (Lap_Child) loop
            for Pointdata of Ctx.Get_Children (Trackpoint) loop
              if Ctx.Get_Name (Pointdata) = "HeartRateBpm" then
                -- One child Value with Bmp as text
                Node := Ctx.Get_Child (Ctx.Get_Child (Pointdata, 1), 1);
                Txt := Ctx.Get_Text (Node);
                Point.Bmp := Pers_Def.Bpm_Range'Value (Txt.Image);
                -- Time is first child of Trackpoint
                Node := Ctx.Get_Child (Ctx.Get_Child (Trackpoint, 1), 1);
                Txt := Ctx.Get_Text (Node);
                Date := Date_Text.Scan (Txt.Slice (1, Date_Len),
                                        "%Y-%m-%dT%H:%M:%S");
                Point.Time := Date_Text.Pack (Date);
                -- Avoid duplicated successive times
                if Points.Is_Empty
                or else Point.Time /= Points.Access_Current.Time then
                  Points.Insert (Point);
                  Logger.Log_Debug ("  Got point " & Txt.Image & Point.Bmp'Img);
                end if;
              end if;
            end loop;
          end loop;
        end if;
      end loop;
    end loop;

    if Points.Is_Empty then
      return;
    end if;

    -- Copy a sample at each delta
    Points.Rewind;
    Point := Points.Get;
    Res.Samples(1) := Point.Bmp;
    Logger.Log_Debug ("  Keeping point "
             & Images.Date_Image (Point.Time) & Point.Bmp'Img);
    Init_Time := Point.Time + Duration (Mesure.Sampling_Delta);
    Res_Index := 2;
    while not Points.Is_Empty loop
      Point := Points.Get;
      if Point.Time >= Init_Time then
         Res.Samples(Res_Index) := Point.Bmp;
         Logger.Log_Debug ("  Keeping point "
             & Images.Date_Image (Point.Time) & Point.Bmp'Img);
         exit when Res_Index = Mesu_Def.Sample_Nb_Range'Last;
         Res_Index := Res_Index + 1;
         Init_Time := Init_Time + Duration (Mesure.Sampling_Delta);
      end if;
    end loop;

    -- All is OK
    Ok := True;
    Mesure := Res;
  exception
    when Error:others =>
      Logger.Log_Debug ("Exception " & Ada.Exceptions.Exception_Name (Error));
  end Import_Tcx;

  -- Import all Samples from .txt file
  procedure Import_Txt (File_Name : in String;
                        Mesure : in out Mesu_Def.Mesure_Rec;
                        Ok : out Boolean) is

    package Get_Sample is new Get_Line (Comment => "#");
    Samples : Mesu_Def.Max_Sample_Array
            := (others => Pers_Def.Bpm_Range'First);
    Samples_Index : Mesu_Def.Sample_Nb_Range := Mesu_Def.Sample_Nb_Range'First;
    Sample_Line : As.U.Utils.Asu_Ua.Unbounded_Array;

  begin
    -- Open file
    Get_Sample.Open(File_Name);

    -- Read, decode, store in Samples
    loop
      -- Split line in words
      Get_Sample.Get_Words (Sample_Line);
      -- Not empty nor a comment
      for I in 1 .. Get_Sample.Get_Word_Number loop
        -- Decode a Bpm
        Samples(Samples_Index) := Pers_Def.Bpm_Range'Value (
                                     Sample_Line.Element(I).Image);
        Samples_Index := Samples_Index + 1;
      end loop;

      -- Read next line and exit when end of file
      begin
        Get_Sample.Read_Next_Line;
      exception
        when Get_Sample.End_Error =>
          exit;
      end;
    end loop;

    -- Close file
    Get_Sample.Close;

    -- So far so good... Copy Samples in mesure
    Mesure.Samples := Samples;

    -- Done
    Ok := True;
  exception
    when others =>
      -- Ok is False. Close file if open.
      begin
        Get_Sample.Close;
      exception
        when others =>
          null;
      end;
  end Import_Txt;

  -- Import Date, and Samples according to Sampling_Delta, from .tcx file
  -- Import all Samples from .txt file
  procedure Import (File_Name : in String;
                    Mesure : in out Mesu_Def.Mesure_Rec;
                    Ok : out Boolean) is
  begin
    if Upper_Str (Directory.File_Suffix (File_Name)) = ".TCX" then
      Import_Tcx (File_Name, Mesure, Ok);
    elsif Upper_Str (Directory.File_Suffix (File_Name)) = ".TXT" then
      Import_Txt (File_Name, Mesure, Ok);
    else
      Ok := False;
    end if;
  end Import;

end Mesu_Imp;

