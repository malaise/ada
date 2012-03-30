with Ada.Exceptions;
with As.U, Basic_Proc, Argument, Argument_Parser, Parser, Event_Mng;
with Clients, Partner;
procedure Tcpipe is

  procedure Usage is
  begin
    Basic_Proc.Put_Line_Error (
     "Usage: " & Argument.Get_Program_Name
               & " <mode> [ <ports> ] [ <target> ]");
    Basic_Proc.Put_Line_Error (
     "  <mode>     ::= <client> | <server>");
    Basic_Proc.Put_Line_Error (
     "  <client>   ::= -c <host>:<port> | --client=""<host>:<port>""");
    Basic_Proc.Put_Line_Error (
     "  <server>   ::= -s <port> | --server=""<port>""");
    Basic_Proc.Put_Line_Error (
     "  <ports>    ::= -p <port_list> | --ports=""<port_list>""");
    Basic_Proc.Put_Line_Error (
     "  <portlist> ::= <port>[{,<port>}]");
    Basic_Proc.Put_Line_Error (
     "  <target> ::= -t <host> | --target=""<host>""");
  end Usage;

  procedure Error (Msg : in String) is
  begin
    Basic_Proc.Put_Line_Error ("ERROR: " & Msg & ".");
    Basic_Proc.Set_Error_Exit_Code;
  end Error;

  -- The keys and descriptor of parsed keys
  Keys : constant Argument_Parser.The_Keys_Type := (
    1 => ('h', As.U.Tus ("help"), False, False),
    2 => ('c', As.U.Tus ("client"), False, True),
    3 => ('s', As.U.Tus ("server"), False, True),
    4 => ('p', As.U.Tus ("ports"), True, True),
    5 => ('t', As.U.Tus ("target"), False, True));
  Arg_Dscr : Argument_Parser.Parsed_Dscr;

  -- Partner address
  Client_Mode : Boolean;
  Partner_Addr : As.U.Asu_Us;

  -- For parsing ports
  function Separing (C : Character) return Boolean is
  begin
    return C = ',';
  end Separing;

  -- For CtrlC
  Break : Boolean := False;
  procedure Break_Cb is
  begin
    Break := True;
  end Break_Cb;

begin
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

  -- No embedded nor trailling args
  if Arg_Dscr.Get_Nb_Embedded_Arguments /= 0
  or else Arg_Dscr.Get_Nb_Embedded_Arguments /= 0 then
    Error ("Invalid arguments");
    return;
  end if;

  -- Parse partner
  -- One and only one mode
  if Arg_Dscr.Is_Set (2) and then Arg_Dscr.Is_Set (3) then
    Error ("Options -c and -s are mutually exclusive");
    return;
  elsif not Arg_Dscr.Is_Set (2) and then not Arg_Dscr.Is_Set (3) then
    Error ("One of options -c or -s is required");
    return;
  end if;

  -- Check address and init
  if Arg_Dscr.Is_Set (2) then
    Client_Mode := True;
    Partner_Addr := As.U.Tus (Arg_Dscr.Get_Option (2));
  else
    Client_Mode := False;
    Partner_Addr := As.U.Tus (Arg_Dscr.Get_Option (3));
  end if;
  if Partner_Addr.Is_Null then
    Error ("Missing partner address");
    return;
  end if;
  begin
    Partner.Init (Client => Client_Mode, Addr => Partner_Addr.Image);
  exception
    when Partner.Invalid_Addr =>
      Error ("Invalid partner address " & Partner_Addr.Image);
      return;
  end;

  -- Parse ports
  for Ports in 1 .. Arg_Dscr.Get_Nb_Occurences (4) loop
    declare
      Iter : Parser.Iterator;
    begin
      Iter.Set (Arg_Dscr.Get_Option (4, Ports), Separing'Unrestricted_Access);
      loop
        declare
          Port : constant String := Iter.Next_Word;
        begin
          exit when Port = "";
          Clients.Accept_Client (Port);
        exception
          when Clients.Invalid_Port =>
            Error ("Invalid port " & Port);
            return;
        end;
      end loop;
    end;
  end loop;

  -- Parse target
  if Arg_Dscr.Is_Set (5) and then Arg_Dscr.Get_Option (5, 1) /= "" then
    begin
      Clients.Set_Target (Arg_Dscr.Get_Option (5, 1));
    exception
      when Clients.Invalid_Target =>
        Error ("Invalid target address " & Arg_Dscr.Get_Option (5, 1));
        return;
    end;
  end if;

  -- Infinite loop
  Event_Mng.Set_Sig_Term_Callback (Break_Cb'Unrestricted_Access);
  loop
    Event_Mng.Wait (Event_Mng.Infinite_Ms);

    pragma Warnings (Off, "variable ""*"" is not modified in loop body");
    exit when Break;
    pragma Warnings (On,  "variable ""*"" is not modified in loop body");

  end loop;

  Partner.Close;
exception
  when Error:others =>
    Basic_Proc.Put_Line_Error ("EXCEPTION "
         & Ada.Exceptions.Exception_Name(Error) & " raised.");
    Partner.Close;
    raise;
end Tcpipe;

