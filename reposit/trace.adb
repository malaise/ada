with Ada.Text_Io, Ada.Calendar;
with Normal, Sys_Calls, Int_Image, Date_Image;
package body Trace is

  function Pid_Image is new Int_Image (Sys_Calls.Pid);

  File            : Ada.Text_Io.File_Type;
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
    Ada.Text_Io.Create (
         File => File,
         Mode => Ada.Text_Io.Out_File,
         Name => Trace_File_Name & Pid_Image (Sys_Calls.Get_Pid));
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
      Ada.Text_Io.Put (File => File,
                       Item => Normal(Count, 5));
      if Date then
        Ada.Text_Io.Put (File => File,
                         Item => " " & Date_Image (Ada.Calendar.Clock, True));
      end if;
      Ada.Text_Io.Put_Line (File => File,
                            Item => " ->" & Message & "<");
    end if;
    if Flush then
      Ada.Text_Io.Flush (File);
    end if;
    if Count /= Positive'Last then
      Count := Positive'Succ(Count);
    else
      Count := Positive'First;
    end if;
  end Put;

end Trace;

