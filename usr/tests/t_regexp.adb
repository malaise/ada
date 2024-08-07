-- Automatic test or check versus a pattern several sentences,
--  provided as arguments
with Argument, Argument_Parser, Reg_Exp, Int_Img, Str_Util,
     Text_Line, Basic_Proc, As.U;
procedure T_Regexp is
  -- The keys
  Keys : constant Argument_Parser.The_Keys_Type := (
    (False, 'S', As.U.Tus ("silent"), False),
    (False, 'i', As.U.Tus ("case_insensitive"), False),
    (False, 'm', As.U.Tus ("multiline"), False),
    (False, 'd', As.U.Tus ("dot_all"), False),
    (False, 's', As.U.Tus ("strict"), False),
    (False, 'v', As.U.Tus ("version"), False),
    (False, 'h', As.U.Tus ("help"), False));
  Dscr : Argument_Parser.Parsed_Dscr;
  Helps : constant array (Keys'Range) of As.U.Asu_Us := (
    As.U.Tus ("for silent check (exit code only)"),
    As.U.Tus ("for case insensitive"),
    As.U.Tus ("for multiline"),
    As.U.Tus ("for dot matches all"),
    As.U.Tus ("for strict matching (not only contains)"),
    As.U.Tus ("for pcre version"),
    As.U.Tus ("for help"));

  procedure Help is
  begin
    Basic_Proc.Put_Line_Output ("Usage: " & Argument.Get_Program_Name
                        & " <automatic> | <manual> | -h | --help | -v | --version");
    Basic_Proc.Put_Line_Output (" <automatic> ::= -c  |  -f  |  -p [ { <pattern> } ]");
    Basic_Proc.Put_Line_Output ("     -c for successive compilations");
    Basic_Proc.Put_Line_Output ("     -f for successive compilations and frees");
    Basic_Proc.Put_Line_Output ("     -p for compiling all arguments as patterns");
    Basic_Proc.Put_Line_Output (" <manual> ::= [ <option> ] <pattern> { <search_string> }");
    for I in Keys'Range loop
      Basic_Proc.Put_Line_Output ("     " & Argument_Parser.Image (Keys(I))
                                & ", " & Helps(I).Image);
    end loop;
  end Help;

  Silent : Boolean := False;
  Case_Insensitive : Boolean := False;
  Multi_Line : Boolean := False;
  Dot_All : Boolean := False;
  Strict : Boolean := False;
  Start : Natural := 1;
  Ok : Boolean;
  Pattern : Reg_Exp.Compiled_Pattern;
  Arg_Error, Compile_Error : exception;

  procedure Compile_Pattern (Str : in String; Report : in Boolean := True) is
    Ok : Boolean;
  begin
    -- Compile pattern
    Pattern.Compile (Ok, Str,
                     Case_Sensitive => not Case_Insensitive,
                     Multi_Line => Multi_Line,
                     Dot_All => Dot_All);
    if not Ok then
      if not Silent then
        Basic_Proc.Put_Line_Error ("Error compiling pattern >" & Str & "<");
        Basic_Proc.Put_Line_Error (Pattern.Error & ".");
      end if;
      raise Compile_Error;
    elsif Report then
      if not Silent then
        Basic_Proc.Put_Line_Output ("Pattern >" & Str & "< compiled");
      end if;
    end if;
  end Compile_Pattern;


  subtype Match_Result is Natural range 0 .. 50;
  subtype Match_Range is Positive range 1 .. Match_Result'Last;
  Match_Info : Reg_Exp.Match_Array (Match_Range);
  N_Matched : Match_Result;
begin
  if Argument.Get_Nbre_Arg < 1 then
    raise Arg_Error;
  end if;

  if Argument.Get_Nbre_Arg = 1 and then Argument.Get_Parameter = "-c" then
    -- Inifiniyr loop of compile (overwrite pattern)
    Basic_Proc.Put_Line_Output (
      "Infinite loop of silent Compile to check memory... Ctrl C to end");
    loop
      Compile_Pattern ("toto", False);
    end loop;
  elsif Argument.Get_Nbre_Arg = 1 and then Argument.Get_Parameter = "-f" then
    -- Inifiniyr loop of compile + free
    Basic_Proc.Put_Line_Output (
      "Infinite loop of silent Compile/Free to check memory... Ctrl C to end");
    loop
      Compile_Pattern ("titi", False);
      Pattern.Free;
    end loop;
  elsif Argument.Get_Parameter = "-p" then
    -- Compile all args as pattern
    Basic_Proc.Put_Line_Output (
      "Compiling all arguments as patterns.");
    for I in 2 .. Argument.Get_Nbre_Arg loop
      Compile_Pattern (Argument.Get_Parameter (Occurence => I));
    end loop;
    return;
  end if;

  -- Manual modes

  -- Parse arguments
  Dscr :=  Argument_Parser.Parse (Keys);
  if not Dscr.Is_Ok then
    Basic_Proc.Put_Line_Error ("Invalid argument: "  & Dscr.Get_Error & ".");
    raise Arg_Error;
  end if;

  if Dscr.Is_Set (7) then
    -- Help
    Help;
    return;
  end if;
  if Dscr.Is_Set (6) then
    --Version
    Basic_Proc.Put_Line_Output ("PCRE: " & Reg_Exp.Get_Pcre_Version);
    return;
  end if;

  -- At least one non key, not embedded
  if Dscr.Get_Nb_Occurences (Argument_Parser.No_Key_Index ) < 1
  or else Dscr.Get_Nb_Embedded_Arguments /= 0 then
    Basic_Proc.Put_Line_Error ("Invalid argument");
    raise Arg_Error;
  end if;
  Start := Dscr.Get_First_Pos_After_Keys;
  Silent := Dscr.Is_Set (1);
  Case_Insensitive := Dscr.Is_Set (2);
  Multi_Line := Dscr.Is_Set (3);
  Dot_All := Dscr.Is_Set (4);
  Strict := Dscr.Is_Set (5);

  -- Compile 1st arg as pattern
  Compile_Pattern (Argument.Get_Parameter (Occurence => Start), False);
  Start := Start + 1;

  -- Check pattern vs other arguments
  Ok := False;
  for I in Start .. Argument.Get_Nbre_Arg loop
    declare
      Str : constant String
          := Str_Util.Substit (Argument.Get_Parameter (Occurence => I),
                                 "\n",
                                 Text_Line.Line_Feed_Str);
    begin
      Pattern.Exec (Str, N_Matched, Match_Info);
      -- If Strict and if Str matches but not strictly
      --  (Str contains a string that matches but Str has more characters)
      -- then it doesn't match
      if Strict
      and then not Reg_Exp.Strict_Match (Str, Match_Info) then
        N_Matched := 0;
      end if;
    end;

    if not Silent then
      Basic_Proc.Put_Output ("String >"
                      & Argument.Get_Parameter (Occurence => I)
                      & "< ");
    end if;

    if N_Matched = 0 then
      if not Silent then
          Basic_Proc.Put_Line_Output ("does not match");
      end if;
    else
      -- At least one match
      Ok := True;
      if not Silent then
        Basic_Proc.Put_Output ("matches at pos");
        -- List submatches
        for I in Match_Range'(1) .. N_Matched loop
          Basic_Proc.Put_Output (
              " [" & Int_Img(Match_Info(I).First_Offset)
            & "-" & Int_Img(Match_Info(I).Last_Offset_Start)
            & "/" & Int_Img(Match_Info(I).Last_Offset_Stop) & "]");
        end loop;
        Basic_Proc.New_Line_Output;
      end if;
    end if;

  end loop;

  if Ok then
    Basic_Proc.Set_Exit_Code (0);
  else
    Basic_Proc.Set_Exit_Code (1);
  end if;

exception
  when Compile_Error =>
    Basic_Proc.Set_Exit_Code (2);
  when Arg_Error | Argument.Argument_Not_Found =>
    Help;
    Basic_Proc.Set_Exit_Code (3);
end T_Regexp;

