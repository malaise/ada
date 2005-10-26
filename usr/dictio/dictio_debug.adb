with Ada.Text_Io, Ada.Calendar;
with Sys_Calls, Normal, Day_Mng, Environ;
package body Dictio_Debug is

  procedure Init is
  begin
    Level_Array := (others => False);

    Level_Array(Status) := Environ.Is_Yes ("DICTIO_DEBUG_STATUS");
    Level_Array(Intra) := Environ.Is_Yes ("DICTIO_DEBUG_INTRA");
    Level_Array(Fight) := Environ.Is_Yes ("DICTIO_DEBUG_FIGHT");
    Level_Array(Online) := Environ.Is_Yes ("DICTIO_DEBUG_ONLINE");
    Level_Array(Client) := Environ.Is_Yes ("DICTIO_DEBUG_CLIENT");
    Level_Array(Client_Data) := Environ.Is_Yes ("DICTIO_DEBUG_CLIENT_DATA");
    Level_Array(Client_Notify) :=
                Environ.Is_Yes ("DICTIO_DEBUG_CLIENT_NOTIFY");
    Level_Array(Client_Alias) := Environ.Is_Yes ("DICTIO_DEBUG_CLIENT_ALIAS");
    Level_Array(Sync) := Environ.Is_Yes ("DICTIO_DEBUG_SYNC");
    Level_Array(Lib) := Environ.Is_Yes ("DICTIO_DEBUG_LIB");

    if Environ.Is_Yes ("DICTIO_DEBUG_ALL") then
      Level_Array := (others => True);
    end if;
  end Init;

  function Date_Image return String is
    Year : Ada.Calendar.Year_Number;
    Month : Ada.Calendar.Month_Number;
    Day : Ada.Calendar.Day_Number;
    Dur : Ada.Calendar.Day_Duration;
    Hour : Day_Mng.T_Hours;
    Minute : Day_Mng.T_Minutes;
    Second : Day_Mng.T_Seconds;
    Millisec : Day_Mng.T_Millisec;
  begin
    Ada.Calendar.Split (Ada.Calendar.Clock, Year, Month, Day, Dur);
    Day_Mng.Split (Dur, Hour, Minute, Second, Millisec);
    return Normal (Year, 4, Gap => '0') & "/"
         & Normal (Month, 2, Gap => '0') & "/"
         & Normal (Day, 2, Gap => '0') & "-"
         & Normal (Hour, 2, Gap => '0') & ":"
         & Normal (Minute, 2, Gap => '0') & ":"
         & Normal (Second, 2, Gap => '0') & "."
         & Normal (Millisec, 3, Gap => '0');
  end Date_Image;

  procedure Put (Str : in String) is
  begin
    Ada.Text_Io.Put_Line (Date_Image & " " & Str);
  end Put;

  procedure Put_Error (Str : in String) is
  begin
    Sys_Calls.Put_Line_Error (Date_Image & " " & Str);
  end Put_Error;

end Dictio_Debug;

