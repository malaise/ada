with Text_Io;
with Normal;
package body Trace is

  File            : Text_Io.File_Type;
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
    Text_Io.Create (File => File, Mode => Text_Io.Out_File,
     Name => Trace_File_Name);
    Created := True;
  end Create;

  procedure Put (Message : in String := "") is
  begin
    if not Activated then
      return;
    end if;
    if not Created then
      Create;
    end if;
    Text_Io.Put_Line (
     File => File,
     Item => Normal(Count, 5) & " ->" & Message & "<");
    if Count /= Positive'Last then
      Count := Positive'Succ(Count);
    else
      Count := Positive'First;
    end if;
  end Put;

end Trace;

