with Ada.Text_Io;
with Argument, Regular_Expressions, Integer_Image, String_Mng, Text_Line,
     Basic_Proc;

procedure T_Regexp is

  procedure Error is
  begin
    Ada.Text_Io.Put_Line ("Usage: " & Argument.Get_Program_Name
                        & " <automatic> | <manual>");
    Ada.Text_Io.Put_Line (" <automatic> ::= -c | -f | -p");
    Ada.Text_Io.Put_Line ("     -c for successive compilations");
    Ada.Text_Io.Put_Line ("     -f for successive compilations and frees");
    Ada.Text_Io.Put_Line ("     -p for compilind all arguments as patterns");
    Ada.Text_Io.Put_Line (" <manual>    ::= [ <option> ] <pattern> { <Search_String> }");
    Ada.Text_Io.Put_Line ("     -s for silent check (exit code only)");
    Ada.Text_Io.Put_Line ("     -n for no match newline");
    Ada.Text_Io.Put_Line ("     -d for dot math all");
  end Error;

  Silent : Boolean := False;
  Not_Newline : Boolean := False;
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
                                 Case_Sensitive => True,
                                 Match_Newline => not Not_Newline,
                                 Dot_All => Dot_All);
    if not Ok then
      if not Silent then
        Ada.Text_Io.Put_Line ("Error compiling pattern >" & Str & "<");
        Ada.Text_Io.Put_Line (Regular_Expressions.Error (Pattern));
      end if;
      raise Compile_Error;
    elsif Report then
      if not Silent then
        Ada.Text_Io.Put_Line ("Pattern >" & Str & "< compiled");
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
    Ada.Text_Io.Put_Line (
      "Infinite loop of silent Compile to check memory... Ctrl C to end");
    loop
      Compile_Pattern ("toto", False);
    end loop;
  elsif Argument.Get_Nbre_Arg = 1 and then Argument.Get_Parameter = "-f" then
    Ada.Text_Io.Put_Line (
      "Infinite loop of silent Compile/Free to check memory... Ctrl C to end");
    loop
      Compile_Pattern ("titi", False);
      Regular_Expressions.Free (Pattern);
    end loop;
  elsif Argument.Get_Parameter = "-p" then
    -- Compile all args as pattern, keep 1st arg as THE pattern
    Ada.Text_Io.Put_Line (
      "Compiling all arguments as patterns.");
    for I in 2 .. Argument.Get_Nbre_Arg loop
      Compile_Pattern (Argument.Get_Parameter (Occurence => I));
    end loop;
    return;
  end if;

  Start := 1;
  Silent := False;
  Not_Newline := False;
  Dot_All := False;

  loop
    if Argument.Get_Parameter (Occurence => Start) = "-s" then
      Silent := True;
      Start := Start + 1;
    elsif Argument.Get_Parameter (Occurence => Start) = "-n" then
      Not_Newline := True;
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
      Ada.Text_Io.Put ("String >"
                      & Argument.Get_Parameter (Occurence => I)
                      & "< ");
    end if;
    if N_Matched = 0 then
      if not Silent then
          Ada.Text_Io.Put_Line ("does not match");
      end if;
    else
      -- At least one match
      Ok := True;
      if not Silent then
        Ada.Text_Io.Put ("matches at pos");
        -- List submatches
        for I in Match_Range'(1) .. N_Matched loop
          Ada.Text_Io.Put (
              " [" & Integer_Image(Match_Info(I).First_Offset)
            & "-" & Integer_Image(Match_Info(I).Last_Offset_Start)
            & "/" & Integer_Image(Match_Info(I).Last_Offset_Stop) & "]");
        end loop;
        Ada.Text_Io.New_Line;
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

