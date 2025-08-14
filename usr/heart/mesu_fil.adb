with Ada.Exceptions;
with Str_Util, Text_Line, Basic_Proc, Sys_Calls, As.U, Trace.Loggers;
with Pers_Def, Str_Mng;
package body Mesu_Fil is

  Logger : Trace.Loggers.Logger;

  -- 3 for Sampling delta, 20 for comment, 6x3 Bmps, Nx3 Mesures
  subtype File_Txt is String (1 .. 41);

  -- Text file of mesure (new format)
  Txt_File : Text_Line.File_Type;

  procedure Open (File_Name : in String; Create : in Boolean) is
  begin
    if Create then
      Logger.Log_Debug ("Creating file " & File_Name);
      begin
        begin
          -- Reuse or create Txt file for writing
          Txt_File.Open_All (Text_Line.Out_File, File_Name, Text_Line.Trunc);
          Logger.Log_Debug ("  Reusing (trunc)");
        exception
          when Text_Line.Name_Error =>
            Logger.Log_Debug ("  Creating");
            Txt_File.Create_All (File_Name);
        end;
        Logger.Log_Debug ("Created file " & File_Name);
      exception
        when Error:others =>
          Logger.Log_Debug ("Exception "
                          & Ada.Exceptions.Exception_Name (Error));
          Basic_Proc.Put_Line_Error ("Exception on Create "
              & Ada.Exceptions.Exception_Name (Error) & " " & File_Name);
          raise Io_Error;
      end;
      return;
    end if;

    -- Try to open existing Txt file for reading
    Logger.Log_Debug ("Opening file " & File_Name);
    Txt_File.Open_All (Text_Line.In_File, File_Name);
    -- Try to read Txt content
    declare
      Str : constant As.U.Asu_Us := As.U.Tus (Text_Line.Trim (Txt_File.Get));
      Sampling : Pers_Def.Sampling_Delta_Range;
      use type Pers_Def.Sampling_Delta_Range;
    begin
      -- Len must be File_Txt'Length + N*3
      if Str.Length < File_Txt'Length
      or else (Str.Length - File_Txt'Length) rem 3 /= 0 then
        Logger.Log_Debug ("  Invalid length" & Str.Length'Img);
        raise Io_Error;
      end if;
      Sampling :=
           Str_Mng.To_Sampling (Str.Slice (1, Str_Mng.Sampling_Str'Last));
      if Sampling = Pers_Def.No_Sampling_Delta then
        Logger.Log_Debug ("  Invalid sampling " & Sampling'Img);
        raise Io_Error;
      end if;
      -- Looks as the start of a sampling rate: Rewind
      Logger.Log_Debug ("  Content OK");
      Txt_File.Close_All;
      Txt_File.Open_All (Text_Line.In_File, File_Name);
      Logger.Log_Debug ("Opened file " & File_Name);
    exception
      when Io_Error =>
        raise;
      when Error:others =>
        Logger.Log_Debug ("Exception "
                        & Ada.Exceptions.Exception_Name (Error));
        Logger.Log_Error ("Exception on Open "
            & Ada.Exceptions.Exception_Name (Error) & " " & File_Name);
        Txt_File.Close_All;
        raise Io_Error;
    end;
  exception
    when others =>
      raise Io_Error;
  end Open;

  procedure Close is
  begin
    Logger.Log_Debug ("Closing file");
    if Txt_File.Is_Open then
      Logger.Log_Debug ("Closed file");
      Txt_File.Close_All;
    end if;
  exception
    when others =>
      null;
  end Close;

  function Load (File_Name : Mesu_Nam.File_Name_Str)
  return Mesu_Def.Mesure_Rec is
    Tmp_Txt : As.U.Asu_Us;
    Mesure : Mesu_Def.Mesure_Rec;
    Date : Mesu_Nam.File_Date_Str;
    Time : Mesu_Nam.File_Time_Str;
    Pid  : Mesu_Nam.File_Pid_Str;
    Sampling : Str_Mng.Sampling_Str;
    Bpm : Pers_Def.Bpm_Range;
    Start : Positive;
    use type Pers_Def.Bpm_Range;
  begin
    Logger.Init ("HeartFile");
    Logger.Log_Debug ("Loading file " & File_Name);
    Mesu_Nam.Split_File_Name (File_Name, Date, Time, Pid);
    if      Date = Mesu_Nam.Wild_Date_Str
    or else Time = Mesu_Nam.Wild_Time_Str
    or else Pid  = Mesu_Nam.Wild_Pid_Str then
      Logger.Log_Debug ("Invalid file name");
      raise File_Name_Error;
    end if;
    Open (File_Name, False);

    -- New text format
    Tmp_Txt := As.U.Tus (Text_Line.Trim (Txt_File.Get));
    Logger.Log_Debug ("Content read");
    Close;
    Mesure.Pid := Pers_Def.Pid_Range'Value(Pid);
    Logger.Log_Debug ("  Got PID " & Pid);
    Mesure.Date := Date;
    Mesure.Time := Time;
    Logger.Log_Debug ("  Got time " & Date & "-" & Time);
    -- Sampling delta
    Sampling := Tmp_Txt.Slice (1, 3);
    Mesure.Sampling_Delta := Str_Mng.To_Sampling (Sampling);
    Logger.Log_Debug ("  Got sampling delta " & Mesure.Sampling_Delta'Img);
    -- Comment
    Mesure.Comment := Tmp_Txt.Slice (4, 23);
    Logger.Log_Debug ("  Got comment " & Mesure.Comment);
    -- 6 Bmps
    Start := 24;
    for I in Pers_Def.Person_Tz_Array'Range loop
      Mesure.Tz(I) := Str_Mng.To_Bpm (Tmp_Txt.Slice (Start, Start + 2));
      Logger.Log_Debug ("  Got TZ" & Mesure.Tz(I)'Img);
      Start := Start + 3;
    end loop;
    -- N samples of 3 chars
    -- No more sample? (end or file)
    while Start /= Tmp_Txt.Length + 1 loop
      exit when Start = Tmp_Txt.Length + 1;
      Bpm := Str_Mng.To_Bpm (Tmp_Txt.Slice (Start, Start + 2));
      -- No more sample? (padding)
      exit when Bpm = Pers_Def.No_Bpm;
      Mesure.Samples.Append (Bpm);
      Logger.Log_Debug ("  Got Bpm" & Bpm'Img);
      Start := Start + 3;
    end loop;
    Logger.Log_Debug ("Loaded file");

    return Mesure;
  exception
    when File_Not_Found_Error | Io_Error =>
      raise;
    when Error:others =>
      Logger.Log_Error ("Exception on Load "
          & Ada.Exceptions.Exception_Name (Error) & " " & File_Name);
      Close;
      raise Io_Error;
  end Load;

  procedure Save (Mesure  : in Mesu_Def.Mesure_Rec) is
    File_Name : Mesu_Nam.File_Name_Str;
    Tmp_Txt : As.U.Asu_Us;
  begin
    Logger.Init ("HeartFile");
    Logger.Log_Debug ("Saving");
    File_Name := Mesu_Nam.Build_File_Name (Mesure);
    Logger.Log_Debug ("Saving file " & File_Name);
    -- Sampling delta
    Tmp_Txt.Append (Str_Mng.To_Str (Mesure.Sampling_Delta));
    -- Comment
    Tmp_Txt.Append (Mesure.Comment);
    -- 6 Bmps
    for I in Pers_Def.Person_Tz_Array'Range loop
      Tmp_Txt.Append (Str_Mng.To_Str (Mesure.Tz(I)));
    end loop;
    -- N measures
    for I in 1 .. Mesure.Samples.Length loop
      Tmp_Txt.Append (Str_Mng.To_Str (Mesure.Samples.Element(I)));
    end loop;
    Logger.Log_Debug ("  Data built");

    Sys_Calls.Unlink (File_Name);
    Logger.Log_Debug ("  File deleted");
    Open (File_Name, True);
    Logger.Log_Debug ("  File created");
    Txt_File.Put_Line (Tmp_Txt.Image);
    Logger.Log_Debug ("  File written");
    Close;
    Logger.Log_Debug ("File saved");
  exception
    when Error:others =>
      Basic_Proc.Put_Line_Error ("Exception on save "
          & Ada.Exceptions.Exception_Name (Error) & " " & File_Name);
      Logger.Log_Debug ("Exception "
                      & Ada.Exceptions.Exception_Name (Error));
      raise Io_Error;
  end Save;

  -- Delete a mesure file
  -- Pid and date of the mesure are used to build the file name
  procedure Delete (File_Name : in Mesu_Nam.File_Name_Str) is
  begin
    Logger.Init ("HeartFile");
    Logger.Log_Debug ("Deleting file " & File_Name);
    -- No space in file_name
    if Str_Util.Locate (File_Name, " ") /= 0 then
      Logger.Log_Debug ("Invalid file name");
      raise File_Name_Error;
    end if;
    Sys_Calls.Unlink (File_Name);
    Logger.Log_Debug ("Deleted file");
  exception
    when Error:others =>
      Logger.Log_Error ("Exception "
                      & Ada.Exceptions.Exception_Name (Error));
      raise Io_Error;
  end Delete;

end Mesu_Fil;

