with Ada.Direct_Io;
with Normal, As.B;
with Pers_Def;
package body Mesu_Fil is


  -- A mesure in file (same as in definition but without pid)
  type File_Rec is record
    Sampling_Delta : Mesu_Def.Sampling_Delta_Range := 60;
    Comment : Mesu_Def.Comment_Str := (others => ' ');
    -- Time zones for the mesure
    Tz : Pers_Def.Person_Tz_Array := (others => Pers_Def.Bpm_Range'First);
    Samples : Mesu_Def.Max_Sample_Array
            := (others => Pers_Def.Bpm_Range'First);
  end record;


  -- Direct_io of mesures
  package Mesure_Io is new Ada.Direct_Io (Element_Type => File_Rec);
  Mesure_File : Mesure_Io.File_Type;

  procedure Open (File_Name : in String; Create : Boolean := True) is
  begin
    -- Try to open existing file
    begin
      Mesure_Io.Open (Mesure_File, Mesure_Io.Inout_File, File_Name);
    exception
      when Mesure_Io.Name_Error =>
        if Create then
          Mesure_Io.Create (Mesure_File, Mesure_Io.Inout_File, File_Name);
        else
          raise File_Not_Found_Error;
        end if;
    end;
  exception
    when File_Not_Found_Error =>
      raise;
    when others =>
      raise Io_Error;
  end Open;

  procedure Close is
  begin
    Mesure_Io.Close (Mesure_File);
  exception
    when others =>
      null;
  end Close;

  function Load (File_Name : Mesu_Nam.File_Name_Str)
  return Mesu_Def.Mesure_Rec is
    Mesure : Mesu_Def.Mesure_Rec;
    Tmp : File_Rec;
    Date : Mesu_Nam.File_Date_Str;
    No   : Mesu_Nam.File_No_Str;
    Pid  : Mesu_Nam.File_Pid_Str;
  begin
    Mesu_Nam.Split_File_Name (File_Name, Date, No, Pid);
    if      Date = Mesu_Nam.Wild_Date_Str
    or else No   = Mesu_Nam.Wild_No_Str
    or else Pid  = Mesu_Nam.Wild_Pid_Str then
      raise File_Name_Error;
    end if;
    Open (File_Name, False);
    Mesure_Io.Read (Mesure_File, Tmp);
    Mesure_Io.Close (Mesure_File);
    Mesure := (Pid => Pers_Def.Pid_Range'Value(Pid),
               Date => Date,
               Sampling_Delta => Tmp.Sampling_Delta,
               Comment => Tmp.Comment,
               Tz => Tmp.Tz,
               Samples => Tmp.Samples);

    return Mesure;
  exception
    when File_Not_Found_Error =>
      raise;
    when others =>
      Close;
      raise Io_Error;
  end Load;

  procedure Save (File_No : in Mesu_Nam.File_No_Str;
                  Mesure  : in Mesu_Def.Mesure_Rec) is
    File_Name : Mesu_Nam.File_Name_Str;
    Tmp : File_Rec;
  begin
    if File_No = Mesu_Nam.Wild_No_Str then
      raise File_Name_Error;
    end if;
    File_Name := Mesu_Nam.Build_File_Name (Mesure.Date, File_No,
                          Normal(Integer(Mesure.Pid), 3, Gap => '0'));
    Tmp := (Sampling_Delta => Mesure.Sampling_Delta,
            Comment => Mesure.Comment,
            Tz => Mesure.Tz,
            Samples => Mesure.Samples);
    Open (File_Name);
    Mesure_Io.Write (Mesure_File, Tmp);
    Mesure_Io.Close (Mesure_File);
  exception
    when others =>
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
    Open (File_Name);
    Mesure_Io.Delete (Mesure_File);
  exception
    when others =>
      Close;
      raise Io_Error;
  end Delete;

end Mesu_Fil;

