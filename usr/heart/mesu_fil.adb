with Ada.Direct_Io, Ada.Text_Io, Ada.Exceptions;
with Normal, As.B;
with Pers_Def, Str_Mng;
package body Mesu_Fil is

  -- A mesure in file - initial binary format
  --  (same as in definition but without pid)
  type File_Rec is record
    Sampling_Delta : Pers_Def.Sampling_Delta_Range
                   := Pers_Def.Default_Sampling_Delta;
    Comment : Mesu_Def.Comment_Str := (others => ' ');
    -- Time zones for the mesure
    Tz : Pers_Def.Person_Tz_Array := (others => Pers_Def.Bpm_Range'First);
    Samples : Mesu_Def.Max_Sample_Array
            := (others => Pers_Def.Bpm_Range'First);
  end record;

  -- 3 for Sampling delta, 20 for comment, 6x3 Bmps, 100x3 Mesures => 341
  subtype File_Txt is String (1 .. 341);

  -- Direct_Io of mesure (initial binary format)
  package Mesure_Io is new Ada.Direct_Io (Element_Type => File_Rec);
  Mesure_File : Mesure_Io.File_Type;

  -- Text_Io of measure (new format)
  Txt_File : Ada.Text_Io.File_Type;

  procedure Open (File_Name : in String; Create : Boolean) is
  begin
    if Create then
      begin
        begin
          -- Reuse or create Txt file for writing
          Ada.Text_Io.Open (Txt_File, Ada.Text_Io.Out_File, File_Name);
        exception
          when Ada.Text_Io.Name_Error =>
            Ada.Text_Io.Create (Txt_File, Ada.Text_Io.Out_File, File_Name);
        end;
      exception
        when Error:others =>
          Ada.Text_Io.Put_Line ("Exception on create "
              & Ada.Exceptions.Exception_Name (Error) & " " & File_Name);
          raise Io_Error;
      end;
      return;
    end if;

    -- Try to open existing Txt file for reading
    begin
      Ada.Text_Io.Open (Txt_File, Ada.Text_Io.In_File, File_Name);
      -- Try to read Txt content
      declare
        Str : constant String := Ada.Text_Io.Get_Line (Txt_File);
      begin
        if Str'Length = File_Txt'Length
        and then (Str(1) = ' ' or else Str(1) = '1') then
          -- Looks as the start of a sampling rate
          Ada.Text_Io.Reset (Txt_File);
          return;
        end if;
        Ada.Text_Io.Close (Txt_File);
      exception
        when others =>
          Ada.Text_Io.Close (Txt_File);
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
        Ada.Text_Io.Put_Line ("Exception on Open "
            & Ada.Exceptions.Exception_Name (Error) & " " & File_Name);
        raise Io_Error;
    end;
  end Open;

  procedure Close is
  begin
    if Ada.Text_Io.Is_Open (Txt_File) then
      Ada.Text_Io.Close (Txt_File);
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
    Tmp_Txt : File_Txt;
    Mesure : Mesu_Def.Mesure_Rec;
    Date : Mesu_Nam.File_Date_Str;
    No   : Mesu_Nam.File_No_Str;
    Pid  : Mesu_Nam.File_Pid_Str;
    Sampling : Str_Mng.Sampling_Str;
    Bmp : Str_Mng.Bpm_Str;
    Start : Positive;
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
                 Samples => Tmp_Rec.Samples);
    else
      -- New text format
      Tmp_Txt := Ada.Text_Io.Get_Line (Txt_File);
      Close;
      Mesure.Pid := Pers_Def.Pid_Range'Value(Pid);
      Mesure.Date :=  Date;
      -- Sampling delta
      Sampling := Tmp_Txt(1 .. 3);
      Mesure.Sampling_Delta := Str_Mng.To_Sampling (Sampling);
      -- Comment
      Mesure.Comment := Tmp_Txt(4 .. 23);
      -- 6 Bmps
      Start := 24;
      for I in Pers_Def.Person_Tz_Array'Range loop
        Bmp := Tmp_Txt(Start .. Start + 2);
        Mesure.Tz(I) := Str_Mng.To_Bpm (Bmp);
        Start := Start + 3;
      end loop;
      -- 100 measures
      for I in Mesu_Def.Max_Sample_Array'Range loop
        Bmp := Tmp_Txt(Start .. Start + 2);
        Mesure.Samples(I) := Str_Mng.To_Bpm (Bmp);
        Start := Start + 3;
      end loop;
    end if;

    return Mesure;
  exception
    when File_Not_Found_Error =>
      raise;
    when Error:others =>
      Ada.Text_Io.Put_Line ("Exception on Load "
          & Ada.Exceptions.Exception_Name (Error) & " " & File_Name);
      Close;
      raise Io_Error;
  end Load;

  procedure Save (File_No : in Mesu_Nam.File_No_Str;
                  Mesure  : in Mesu_Def.Mesure_Rec) is
    File_Name : Mesu_Nam.File_Name_Str;
    Tmp_Txt : File_Txt := (others => ' ');
    Start : Positive;
  begin
    if File_No = Mesu_Nam.Wild_No_Str then
      raise File_Name_Error;
    end if;
    File_Name := Mesu_Nam.Build_File_Name (Mesure.Date, File_No,
                          Normal(Integer(Mesure.Pid), 3, Gap => '0'));
    -- Sampling delta
    Tmp_Txt(1 .. 3) := Str_Mng.To_Str (Mesure.Sampling_Delta);
    -- Comment
    Tmp_Txt(4 .. 23) := Mesure.Comment;
    -- 6 Bmps
    Start := 24;
    for I in Pers_Def.Person_Tz_Array'Range loop
      Tmp_Txt(Start .. Start + 2) := Str_Mng.To_Str (Mesure.Tz(I));
      Start := Start + 3;
    end loop;
    -- 100 measures
    for I in Mesu_Def.Max_Sample_Array'Range loop
      Tmp_Txt(Start .. Start + 2) := Str_Mng.To_Str (Mesure.Samples(I));
      Start := Start + 3;
    end loop;

    Open (File_Name, True);
    Ada.Text_Io.Put_Line (Txt_File, Tmp_Txt);
    Close;
  exception
    when Error:others =>
      Ada.Text_Io.Put_Line ("Exception on save "
          & Ada.Exceptions.Exception_Name (Error) & " " & File_Name);
      Close;
      raise Io_Error;
  end Save;

  -- Delete a mesure file
  -- Pid and date of the mesure are used to build the file name
  procedure Delete (File_Name : in Mesu_Nam.File_Name_Str) is
  begin
    -- No space in file_name
    if As.B.Locate (As.B.Tbs (File_Name), " ") /= 0 then
      raise File_Name_Error;
    end if;
    Open (File_Name, False);
    if Ada.Text_Io.Is_Open (Txt_File) then
      Ada.Text_Io.Delete (Txt_File);
    end if;
    if Mesure_Io.Is_Open (Mesure_File) then
      Mesure_Io.Delete (Mesure_File);
    end if;
  exception
    when others =>
      Close;
      raise Io_Error;
  end Delete;

end Mesu_Fil;

