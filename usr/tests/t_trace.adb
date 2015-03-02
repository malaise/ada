with Trace.Loggers, Basic_Proc;
procedure T_Trace is

 package Bloga is new Trace.Basic_Logger ("");
 package Blogn is new Trace.Basic_Logger ("Blogn");

 Loga, Logn : Trace.Loggers.Logger;

begin
  Basic_Proc.Put_Line_Output ("Starting");
  Logn.Init ("Logn");

  Bloga.Log_Error ("Bloga Error");
  Blogn.Log_Error ("Blogn Error");
  Loga.Log_Error ("Loga Error");
  Logn.Log_Error ("Logn Error");

  Bloga.Log_Warning ("Bloga Warning");
  Blogn.Log_Warning ("Blogn Warning");
  Loga.Log_Warning ("Loga Warning");
  Logn.Log_Warning ("Logn Warning");

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

