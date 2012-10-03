with Ada.Calendar;
with Normal, Sys_Calls, Images, Text_Line;
package body Trace is

  function Pid_Image is new Images.Int_Image (Sys_Calls.Pid);

  File            : Text_Line.File_Type;
  Trace_File_Name : constant String := "_trace_";
  Count           : Positive        := Positive'First;
  Activated       : Boolean := True;
  Created         : Boolean := False;

  procedure Activate (On : in Boolean := True) is
  begin
    Activated := On;
  end Activate;

  procedure Create is
  begin
    File.Create_All (Trace_File_Name & Pid_Image (Sys_Calls.Get_Pid));
    Created := True;
  end Create;

  procedure Put (Message : in String; Date : in Boolean;
                 Flush : in Boolean := False) is
  begin
    if not Activated then
      return;
    end if;
    if not Created then
      Create;
    end if;
    if Message /= "" or else Date then
      File.Put (Normal(Count, 5));
      if Date then
        File.Put (" " & Images.Date_Image (Ada.Calendar.Clock, True));
      end if;
      File.Put_Line (" ->" & Message & "<");
    end if;
    if Flush then
      File.Flush;
    end if;
    if Count /= Positive'Last then
      Count := Positive'Succ(Count);
    else
      Count := Positive'First;
    end if;
  end Put;

end Trace;

