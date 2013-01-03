with Argument, Argument_Parser, Basic_Proc, As.U;
with Utils, Communication, Setup;
procedure Battleship is
   -- The keys
  Keys : constant Argument_Parser.The_Keys_Type := (
   (False, 'h', As.U.Tus ("help"),  False),
   (False, 's', As.U.Tus ("server"),  False));
  Dscr : Argument_Parser.Parsed_Dscr;

  procedure Usage is
  begin
    Basic_Proc.Put_Line_Output ("Usage: " & Argument.Get_Program_Name
      & " [ -s ] <bus_address>");
    Basic_Proc.Put_Line_Output ("  <bus_address>  ::= <ip_address>:<port>");
  end Usage;

  procedure Error (Msg : in String) is
  begin
    Basic_Proc.Put_Line_Error ("ERROR: " & Msg & ".");
    Basic_Proc.Set_Error_Exit_Code;
  end Error;


begin
  Utils.Init;

  -- Parse arguments
  Dscr :=  Argument_Parser.Parse (Keys);
  if not Dscr.Is_Ok then
    -- Parsing error
    Error (Dscr.Get_Error);
    return;
  end if;
  if Dscr.Is_Set (1) then
    -- Help
    Usage;
    return;
  end if;
  if Dscr.Get_Nb_Occurences (Argument_Parser.No_Key_Index) /= 1 then
    Error ("Missing bus address");
    return;
  end if;

  -- Init setup screen and connect
  if not Setup.Init (Dscr.Get_Option (Argument_Parser.No_Key_Index),
                     Dscr.Is_Set (2)) then
    -- Connection cancelled by user
    Communication.Close;
    return;
  end if;

  -- Setup fleet
  Setup.Define;

  -- Done
  Communication.Close;
exception
  when Communication.Init_Error =>
    Error ("Communication initilisation error");
    Communication.Close;
  when Utils.Abort_Game =>
    Basic_Proc.Put_Line_Output ("Aborted by user");
    Communication.Send_End;
    Communication.Close;
end Battleship;

