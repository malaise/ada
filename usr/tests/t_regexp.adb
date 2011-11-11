with Argument, Regular_Expressions, Integer_Image, String_Mng, Text_Line,
     Basic_Proc;

procedure T_Regexp is

  procedure Error is
  begin
    Basic_Proc.Put_Line_Output ("Usage: " & Argument.Get_Program_Name
                        & " <automatic> | <manual>");
    Basic_Proc.Put_Line_Output (" <automatic> ::= -c | -f | -p");
    Basic_Proc.Put_Line_Output ("     -c for successive compilations");
    Basic_Proc.Put_Line_Output ("     -f for successive compilations and frees");
    Basic_Proc.Put_Line_Output ("     -p for compiling all arguments as patterns");
    Basic_Proc.Put_Line_Output (" <manual>    ::= [ <option> ] <pattern> { <Search_String> }");
    Basic_Proc.Put_Line_Output ("     -s for silent check (exit code only)");
    Basic_Proc.Put_Line_Output ("     -i for case insensitive");
    Basic_Proc.Put_Line_Output ("     -m for multiline");
    Basic_Proc.Put_Line_Output ("     -d for dot math all");
  end Error;

  Silent : Boolean := False;
  Case_Insensitive : Boolean := False;
  Multi_Line : Boolean := False;
  Dot_All : Boolean := False;
  Start : Natural := 1;
  Ok : Boolean;
  Pattern : Regular_Expressions.Compiled_Pattern;
  Compile_Error : exception;

  procedure Compile_Pattern (Str : in String; Report : in Boolean := True) is
    Ok : Boolean;
  begin
    -- Compile pattern
    Regular_Expressions.Compile (Pattern, Ok, Str,
                                 Case_Sensitive => not Case_Insensitive,
                                 Multi_Line => Multi_Line,
                                 Dot_All => Dot_All);
    if not Ok then
      if not Silent then
        Basic_Proc.Put_Line_Output ("Error compiling pattern >" & Str & "<");
        Basic_Proc.Put_Line_Output (Regular_Expressions.Error (Pattern));
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
  Match_Info : Regular_Expressions.Match_Array (Match_Range);
  N_Matched : Match_Result;
begin
  if Argument.Get_Nbre_Arg < 1 then
    Error;
    return;
  end if;

  if Argument.Get_Nbre_Arg = 1 and then Argument.Get_Parameter = "-c" then
    Basic_Proc.Put_Line_Output (
      "Infinite loop of silent Compile to check memory... Ctrl C to end");
    loop
      Compile_Pattern ("toto", False);
    end loop;
  elsif Argument.Get_Nbre_Arg = 1 and then Argument.Get_Parameter = "-f" then
    Basic_Proc.Put_Line_Output (
      "Infinite loop of silent Compile/Free to check memory... Ctrl C to end");
    loop
      Compile_Pattern ("titi", False);
      Regular_Expressions.Free (Pattern);
    end loop;
  elsif Argument.Get_Parameter = "-p" then
    -- Compile all args as pattern, keep 1st arg as THE pattern
    Basic_Proc.Put_Line_Output (
      "Compiling all arguments as patterns.");
    for I in 2 .. Argument.Get_Nbre_Arg loop
      Compile_Pattern (Argument.Get_Parameter (Occurence => I));
    end loop;
    return;
  end if;

  Start := 1;
  Silent := False;
  Case_Insensitive := False;
  Multi_Line := False;
  Dot_All := False;

  loop
    if Argument.Get_Parameter (Occurence => Start) = "-s" then
      Silent := True;
      Start := Start + 1;
    elsif Argument.Get_Parameter (Occurence => Start) = "-i" then
      Case_Insensitive := True;
      Start := Start + 1;
    elsif Argument.Get_Parameter (Occurence => Start) = "-m" then
      Multi_Line := True;
      Start := Start + 1;
    elsif Argument.Get_Parameter (Occurence => Start) = "-d" then
      Dot_All := True;
      Start := Start + 1;
    else
      exit;
    end if;
  end loop;

  -- Compile 1st args as pattern
  Compile_Pattern (Argument.Get_Parameter (Occurence => Start), False);
  Start := Start + 1;

  -- Check pattern vs other arguments
  Ok := False;
  for I in Start .. Argument.Get_Nbre_Arg loop
    declare
      Str : constant String
          := String_Mng.Replace (Argument.Get_Parameter (Occurence => I),
                                 "\n",
                                 Text_Line.Line_Feed_Str);
    begin
      Regular_Expressions.Exec (Pattern,
                                Str,
                                N_Matched,
                                Match_Info);
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
              " [" & Integer_Image(Match_Info(I).First_Offset)
            & "-" & Integer_Image(Match_Info(I).Last_Offset_Start)
            & "/" & Integer_Image(Match_Info(I).Last_Offset_Stop) & "]");
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
  when Argument.Argument_Not_Found =>
    Error;
    Basic_Proc.Set_Exit_Code (3);
end T_Regexp;

