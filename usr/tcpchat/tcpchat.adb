-- Execute chat script on a TCP connection
with Ada.Exceptions;
with As.U, Basic_Proc, Argument, Argument_Parser;
with Debug, Ios, Tree, Events;
procedure Tcpchat is

  Version : constant String := "2.4";

  -- The keys and descriptor of parsed keys
  Keys : constant Argument_Parser.The_Keys_Type := (
   01 => (False, 'h', As.U.Tus ("help"),    False),
   02 => (True,  'p', As.U.Tus ("port"),    False, True, As.U.Asu_Null),
   03 => (True,  'f', As.U.Tus ("file"),    False, True, As.U.Asu_Null),
   04 => (False, 'v', As.U.Tus ("version"), False));
  Arg_Dscr : Argument_Parser.Parsed_Dscr;

  procedure Usage is
  begin
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
      & " <tcp_port> <chat_file>");
    Basic_Proc.Put_Line_Error ("   or: " & Argument.Get_Program_Name
      & " " & Argument_Parser.Image (Keys(1))
      & " | " & Argument_Parser.Image (Keys(4)));
    Basic_Proc.Put_Line_Error ("  <tcp_port>  ::= "
      & Argument_Parser.Image (Keys(2)));
    Basic_Proc.Put_Line_Error ("  <chat_file> ::= "
      & Argument_Parser.Image (Keys(3)));
  end Usage;

  procedure Error (Msg : in String) is
  begin
    Basic_Proc.Put_Line_Error ("ERROR: " & Msg & ".");
    Usage;
    Basic_Proc.Set_Error_Exit_Code;
  end Error;


  Port : As.U.Asu_Us;
  File : As.U.Asu_Us;

begin
  Debug.Logger.Init;

  -- Parse keys and options
  Arg_Dscr := Argument_Parser.Parse (Keys);
  if not Arg_Dscr.Is_Ok then
    Error (Arg_Dscr.Get_Error);
    return;
  end if;

  -- Help
  if Arg_Dscr.Is_Set (1) then
    Usage;
    Basic_Proc.Set_Error_Exit_Code;
    return;
  end if;

  -- Version
  if Arg_Dscr.Is_Set (4) then
    Basic_Proc.Put_Line_Output (Version);
    Basic_Proc.Set_Error_Exit_Code;
    return;
  end if;

  -- Port
  if Arg_Dscr.Get_Nb_Occurences (2) /= 1
  or else Arg_Dscr.Get_Option (2) = "" then
    Error ("Missing ""port"" argument");
    return;
  end if;
  Port := As.U.Tus (Arg_Dscr.Get_Option (2));

  -- File
  if Arg_Dscr.Get_Nb_Occurences (3) /= 1
  or else Arg_Dscr.Get_Option (3) = "" then
    Error ("Missing ""file"" argument");
    return;
  end if;
  File := As.U.Tus (Arg_Dscr.Get_Option (3));

  -- No other arg
  if Arg_Dscr.Get_Nb_Occurences (Argument_Parser.No_Key_Index) /= 0
  or else Arg_Dscr.Get_Nb_Embedded_Arguments /= 0 then
    Error ("Invalid arguments");
    return;
  end if;
  Debug.Logger.Log_Debug ("Arguments parsed OK.");

  -- Parse file
  Tree.Parse (File);
  Debug.Logger.Log_Debug ("Tree file parsed OK.");

  -- Check version
  if Tree.Get_Version /= Version then
    Error ("File " & File.Image & " has incorrected version "
         & Tree.Get_Version & ", expecting " & Version);
    return;
  end if;

  -- Init Ios
  Ios.Init (Port);
  Debug.Logger.Log_Debug ("Init OK.");

  -- Handle events
  Events.Handle;

  Debug.Logger.Log_Debug ("Done.");

exception
  when Tree.Parse_Error =>
    Error ("Cannot parse Tree");
  when Ios.Init_Error =>
    Error ("Cannot init TCP connection");
  when Error:others =>
    Basic_Proc.Put_Line_Error ("ERROR: Exception "
       & Ada.Exceptions.Exception_Name (Error) & " raised.");
    Ios.Close;
    raise;
end Tcpchat;

