-- Master mind (graphic or text mode)
with Basic_Proc, Argument, Argument_Parser, As.U, Int_Img, Rnd;
with Common, Action, Mmind_Asc, Screen, Response;
procedure Mmind is
  Version : constant String := "V1.0";
  -- Options
  Text_Mode : Boolean;
  Default_Level : constant Common.Last_Level_Range
                := Common.Last_Level_Range'Succ (Common.Last_Level_Range'First);
  Level : Common.Last_Level_Range := Default_Level;
  Code_Txt : As.U.Asu_Us;
  Show_Codes : Boolean;

  -- Arguments
  procedure Put_Help is
  begin
    Basic_Proc.Put_Line_Error (
      "Usage: " & Argument.Get_Program_Name
      & " [ <text_mode> ] [ <init_level> ] [ <init_code> ] [ <show_codes> ]");
    Basic_Proc.Put_Line_Error (
      "  <text_mode>    ::= -t | --text");
    Basic_Proc.Put_Line_Error (
      "  <init_level>   ::= -l <level> | --level=<level>");
    Basic_Proc.Put_Line_Error (
      "  <level>        ::= 3 | 4 | 5      // Default "
      & Int_Img (Integer (Default_Level)));
    Basic_Proc.Put_Line_Error (
      "  <init_code>    ::= -c <code> | --code=<code>");
    Basic_Proc.Put_Line_Error (
      "  <code>         ::= { <color_letter> } | { <color_number> }");
    Basic_Proc.Put_Line_Error (
      "  <color_letter> ::= { B | T | C | R | M | W | G | Y }");
    Basic_Proc.Put_Line_Error (
      "  <color_number> ::= 1 .. 8         // For text mode");
    Basic_Proc.Put_Line_Error (
      "  <show_codes>   ::= -s | --show");
  end Put_Help;

  procedure Error (Msg : in String) is
  begin
    Basic_Proc.Put_Line_Error (Argument.Get_Program_Name
                             & " ERROR: " & Msg & ".");
    Basic_Proc.Set_Error_Exit_Code;
    Put_Help;
  end Error;

  Keys : constant Argument_Parser.The_Keys_Type := (
   01 => (False, 't', As.U.Tus ("text"),  False),
   02 => (True,  'l', As.U.Tus ("level"), False, True, As.U.Tus ("level")),
   03 => (True,  'c', As.U.Tus ("code"),  False, True, As.U.Tus ("code")),
   04 => (False, 's', As.U.Tus ("show"),  False),
   05 => (False, 'h', As.U.Tus ("help"),  False),
   06 => (False, 'v', As.U.Tus ("version"),  False) );
   Arg_Dscr : Argument_Parser.Parsed_Dscr;

  use type Common.Full_Level_Range;
begin
  -- Parse arguments
  Arg_Dscr := Argument_Parser.Parse (Keys);
  if not Arg_Dscr.Is_Ok then
    Error (Arg_Dscr.Get_Error);
    return;
  end if;

  -- Help
  if Arg_Dscr.Is_Set (05) then
    Put_Help;
    return;
  end if;

  -- Version
  if Arg_Dscr.Is_Set (06) then
    Basic_Proc.Put_Line_Output ("Version: " & Version);
    return;
  end if;

  -- Text mode
  Text_Mode := Arg_Dscr.Is_Set (01);

  -- Level
  if Arg_Dscr.Is_Set (02) then
    begin
      Level := Common.Last_Level_Range'Value (Arg_Dscr.Get_Option (02));
    exception
      when others =>
        Error ("Invalid level " & Arg_Dscr.Get_Option (02));
        return;
    end;
  end if;

  -- Code
  if Arg_Dscr.Is_Set (03) then
    Code_Txt := As.U.Tus (Arg_Dscr.Get_Option (03));
    declare
      Unused_Length : Common.Last_Level_Range;
    begin
      -- Level is maybe not set yet, but still, the code lenght must be in 3..5
      Unused_Length := Common.Last_Level_Range (Code_Txt.Length);
    exception
      when others =>
        Error ("Invalid code length for " & Code_Txt.Image);
        return;
    end;
  end if;

  -- Show codes
  Show_Codes := Arg_Dscr.Is_Set (04);

  -- Check compatibility
  if Show_Codes and then Text_Mode then
    Error ("Incompatible options -t and -s");
    return;
  end if;

  -- Prepare
  Common.Store_Level (Level);
  Common.Set_Level_To_Stored;
  Rnd.Gen.Randomize;

  -- Check code and process the option to set code
  if not Code_Txt.Is_Null then
    if Common.Last_Level_Range (Code_Txt.Length) /= Level then
      Error ("Invalid code length for " & Code_Txt.Image);
      return;
    end if;
    declare
      Code : Response.Color_Array (1 .. Level);
      C : Character;
    begin
      for I in 1 .. Level loop
        C := Code_Txt.Element (Integer(I));
        Code(I) := (
          if Text_Mode then Common.Eff_Color_Range'Value (C & "")
          else Screen.Color_Of (C) );
      end loop;
      Response.New_Code (Code);
    exception
      when others =>
        Error ("Invalid "
          & (if Text_Mode then "text" else "graphic")
          & " code " & Code_Txt.Image);
        return;
    end;
  else
    Response.New_Code;
  end if;

  -- Run
  if Text_Mode then
    -- Ascii Mode
    Mmind_Asc;
  else
    -- Graphic Mode
    Action.Init (Show_Codes);

    while Action.Play loop
      -- For next Play
      Code_Txt.Set_Null;
      Common.Set_Level_To_Stored;
      Response.New_Code;
    end loop;
    Action.End_Action;
  end if;

exception
  when Action.No_Mouse =>
    Basic_Proc.Put_Line_Error ("Sorry, MOUSE not found.");
    return;
end Mmind;

