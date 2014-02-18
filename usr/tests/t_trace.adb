with Trace.Loggers, Basic_Proc;
procedure T_Trace is

 package Bloga is new Trace.Basic_Logger ("");
 package Blogn is new Trace.Basic_Logger ("Blogn");

 Loga, Logn : Trace.Loggers.Logger;

begin
  Basic_Proc.Put_Line_Output ("Starting");
  Logn.Init ("Logn");
  Bloga.Log_Info ("Bloga Info");
  Blogn.Log_Info ("Blogn Info");
  Loga.Log_Info ("Loga Info");
  Logn.Log_Info ("Logn Info");
  Bloga.Log_Debug ("Bloga Debug");
  Blogn.Log_Debug ("Blogn Debug");
  Loga.Log_Debug ("Loga Debug");
  Logn.Log_Debug ("Logn Debug");
  Basic_Proc.Put_Line_Output ("Done");
end T_Trace;

