with Ada.Direct_Io, Ada.Exceptions;
with Normal, Str_Util, Text_Line, Basic_Proc, Sys_Calls, As.U;
with Pers_Def, Str_Mng;
package body Mesu_Fil is

  -- A mesure in file - initial binary format
  --  (same as in definition but without pid nor date)
  subtype Hundred_Samples is Mesu_Def.Sample_Array (1 .. 100);
  type File_Rec is record
    Sampling_Delta : Pers_Def.Sampling_Delta_Range
                   := Pers_Def.Default_Sampling_Delta;
    Comment : Mesu_Def.Comment_Str := (others => ' ');
    -- Time zones for the mesure
    Tz : Pers_Def.Person_Tz_Array := (others => Pers_Def.Bpm_Range'First);
    Samples : Hundred_Samples;
  end record;

  -- 3 for Sampling delta, 20 for comment, 6x3 Bmps, Nx3 Mesures
  subtype File_Txt is String (1 .. 41);

  -- Direct_Io of mesure (initial binary format)
  package Mesure_Io is new Ada.Direct_Io (Element_Type => File_Rec);
  Mesure_File : Mesure_Io.File_Type;

  -- Text file of measure (new format)
  Txt_File : Text_Line.File_Type;

  procedure Open (File_Name : in String; Create : in Boolean) is
  begin
    if Create then
      begin
        begin
          -- Reuse or create Txt file for writing
          Txt_File.Open_All (Text_Line.Out_File, File_Name, Text_Line.Trunc);
        exception
          when Text_Line.Name_Error =>
            Txt_File.Create_All (File_Name);
        end;
      exception
        when Error:others =>
          Basic_Proc.Put_Line_Error ("Exception on Create "
              & Ada.Exceptions.Exception_Name (Error) & " " & File_Name);
          raise Io_Error;
      end;
      return;
    end if;

    -- Try to open existing Txt file for reading
    begin
      Txt_File.Open_All (Text_Line.In_File, File_Name);
      -- Try to read Txt content
      declare
        Str : constant As.U.Asu_Us := As.U.Tus (Text_Line.Trim (Txt_File.Get));
        Sampling : Pers_Def.Sampling_Delta_Range;
        use type Pers_Def.Sampling_Delta_Range;
      begin
        -- Len must be File_Txt'Length + N*3
        if Str.Length >= File_Txt'Length
        and then (Str.Length - File_Txt'Length) rem 3 = 0 then
          Sampling :=
               Str_Mng.To_Sampling (Str.Slice (1, Str_Mng.Sampling_Str'Last));
          if Sampling /= Pers_Def.No_Sampling_Delta then
            -- Looks as the start of a sampling rate: Rewind
            Txt_File.Close_All;
            Txt_File.Open_All (Text_Line.In_File, File_Name);
            return;
          end if;
        end if;
        Txt_File.Close_All;
      exception
        when others =>
          Txt_File.Close_All;
      end;
    exception
      when others =>
        null;
    end;

    -- Try to open existing binary file for reading
    begin
      Mesure_Io.Open (Mesure_File, Mesure_Io.Inout_File, File_Name);
    exception
      when Mesure_Io.Name_Error =>
        raise File_Not_Found_Error;
      when Error:others =>
        Basic_Proc.Put_Line_Error ("Exception on Open "
            & Ada.Exceptions.Exception_Name (Error) & " " & File_Name);
        raise Io_Error;
    end;
  end Open;

  procedure Close is
  begin
    if Txt_File.Is_Open then
      Txt_File.Close_All;
    end if;
    if Mesure_Io.Is_Open (Mesure_File) then
      Mesure_Io.Close (Mesure_File);
    end if;
  exception
    when others =>
      null;
  end Close;

  function Load (File_Name : Mesu_Nam.File_Name_Str)
                return Mesu_Def.Mesure_Rec is
    Tmp_Rec : File_Rec;
    Tmp_Txt : As.U.Asu_Us;
    Mesure : Mesu_Def.Mesure_Rec;
    Date : Mesu_Nam.File_Date_Str;
    No   : Mesu_Nam.File_No_Str;
    Pid  : Mesu_Nam.File_Pid_Str;
    Sampling : Str_Mng.Sampling_Str;
    Bpm : Pers_Def.Bpm_Range;
    Start : Positive;
    Trail : Natural;
    use type Pers_Def.Bpm_Range;
  begin
    Mesu_Nam.Split_File_Name (File_Name, Date, No, Pid);
    if      Date = Mesu_Nam.Wild_Date_Str
    or else No   = Mesu_Nam.Wild_No_Str
    or else Pid  = Mesu_Nam.Wild_Pid_Str then
      raise File_Name_Error;
    end if;
    Open (File_Name, False);

    if Mesure_Io.Is_Open (Mesure_File) then
      -- Old binary format
      Mesure_Io.Read (Mesure_File, Tmp_Rec);
      Close;
      Mesure := (Pid => Pers_Def.Pid_Range'Value(Pid),
                 Date => Date,
                 Sampling_Delta => Tmp_Rec.Sampling_Delta,
                 Comment => Tmp_Rec.Comment,
                 Tz => Tmp_Rec.Tz,
                 Samples => Mesu_Def.Sample_Array_Mng.To_Unb_Array (
                     Tmp_Rec.Samples));
      -- Purge trailing empty samples
      Trail := 0;
      for I in reverse 1 .. Mesure.Samples.Length loop
        if Mesure.Samples.Element (I) = Pers_Def.No_Bpm then
          Trail := Trail + 1;
        end if;
      end loop;
      Mesure.Samples.Trail (Trail);
    else
      -- New text format
      Tmp_Txt := As.U.Tus (Text_Line.Trim (Txt_File.Get));
      Close;
      Mesure.Pid := Pers_Def.Pid_Range'Value(Pid);
      Mesure.Date :=  Date;
      -- Sampling delta
      Sampling := Tmp_Txt.Slice (1, 3);
      Mesure.Sampling_Delta := Str_Mng.To_Sampling (Sampling);
      -- Comment
      Mesure.Comment := Tmp_Txt.Slice (4, 23);
      -- 6 Bmps
      Start := 24;
      for I in Pers_Def.Person_Tz_Array'Range loop
        Mesure.Tz(I) := Str_Mng.To_Bpm (Tmp_Txt.Slice (Start, Start + 2));
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
        Start := Start + 3;
      end loop;
    end if;

    return Mesure;
  exception
    when File_Not_Found_Error =>
      raise;
    when Error:others =>
      Basic_Proc.Put_Line_Error ("Exception on Load "
          & Ada.Exceptions.Exception_Name (Error) & " " & File_Name);
      Close;
      raise Io_Error;
  end Load;

  procedure Save (File_No : in Mesu_Nam.File_No_Str;
                  Mesure  : in Mesu_Def.Mesure_Rec) is
    File_Name : Mesu_Nam.File_Name_Str;
    Tmp_Txt : As.U.Asu_Us;
  begin
    if File_No = Mesu_Nam.Wild_No_Str then
      raise File_Name_Error;
    end if;
    File_Name := Mesu_Nam.Build_File_Name (Mesure.Date, File_No,
                          Normal(Integer(Mesure.Pid), 3, Gap => '0'));
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

    Sys_Calls.Unlink (File_Name);
    Open (File_Name, True);
    Txt_File.Put_Line (Tmp_Txt.Image);
    Close;
  exception
    when Error:others =>
      Basic_Proc.Put_Line_Error ("Exception on save "
          & Ada.Exceptions.Exception_Name (Error) & " " & File_Name);
      Close;
      raise Io_Error;
  end Save;

  -- Delete a mesure file
  -- Pid and date of the mesure are used to build the file name
  procedure Delete (File_Name : in Mesu_Nam.File_Name_Str) is
  begin
    -- No space in file_name
    if Str_Util.Locate (File_Name, " ") /= 0 then
      raise File_Name_Error;
    end if;
    Sys_Calls.Unlink (File_Name);
  exception
    when others =>
      raise Io_Error;
  end Delete;

end Mesu_Fil;

