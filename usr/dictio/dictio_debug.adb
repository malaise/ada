with Ada.Text_Io, Ada.Calendar;
with Sys_Calls, Normal, Day_Mng;
use Sys_Calls;
package body Debug is
  Val : String (1 .. 1);
  Set, Trunc : Boolean;
  Len : Natural;

  procedure Init is
  begin
    Level_Array := (others => False);

    Getenv ("DICTIO_DEBUG_STATUS", Set, Trunc, Val, Len);
    Level_Array(Status) := Set;

    Getenv ("DICTIO_DEBUG_INTRA", Set, Trunc, Val, Len);
    Level_Array(Intra) := Set;

    Getenv ("DICTIO_DEBUG_FIGHT", Set, Trunc, Val, Len);
    Level_Array(Fight) := Set;
    
    Getenv ("DICTIO_DEBUG_ONLINE", Set, Trunc, Val, Len);
    Level_Array(Online) := Set;
    
    Getenv ("DICTIO_DEBUG_CLIENT", Set, Trunc, Val, Len);
    Level_Array(Client) := Set;
    
    Getenv ("DICTIO_DEBUG_SYNC", Set, Trunc, Val, Len);
    Level_Array(Sync) := Set;
    
    Getenv ("DICTIO_DEBUG_LIB", Set, Trunc, Val, Len);
    Level_Array(Lib) := Set;
    
    Getenv ("DICTIO_DEBUG_ALL", Set, Trunc, Val, Len);
    if Set then
      Level_Array := (others => True);
    end if;
  end Init;

  procedure Put (Str : in String) is
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
    Ada.Text_Io.Put (Normal (Hour, 2, Gap => '0') & ":"
                   & Normal (Minute, 2, Gap => '0') & ":"
                   & Normal (Second, 2, Gap => '0') & "."
                   & Normal (Millisec, 3, Gap => '0') & " ");
    Ada.Text_Io.Put_Line (Str);
  end Put;

end Debug;

