with Ada.Text_Io;
with Argument, Regular_Expressions, Int_Image;

procedure T_Regexp is

  function Image is new Int_Image (Integer);

  procedure Error is
  begin
    Ada.Text_Io.Put_Line ("Usage: " & Argument.Get_Program_Name
                                    & " <pattern> { <Search_String> }");
  end Error;

  Pattern : Regular_Expressions.Compiled_Pattern;

  procedure Compile_Pattern (Str : in String; Report : in Boolean := True) is
    Ok : Boolean;
  begin
    -- Compile pattern
    Regular_Expressions.Compile (Pattern, Ok, Str);
    if not Ok then
      Ada.Text_Io.Put_Line ("Error compiling pattern >" & Str & "<");
      Ada.Text_Io.Put_Line (Regular_Expressions.Error (Pattern));
    elsif Report then
      Ada.Text_Io.Put_Line ("Pattern >" & Str & "< compiled");
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

  if Argument.Get_Nbre_Arg = 1 and then Argument.Get_Parameter = "-m" then
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
  elsif Argument.Get_Parameter = "-c" then
    -- Compile all args as pattern, keep 1st arg as THE pattern
    Ada.Text_Io.Put_Line (
      "Compiling all arguments as patterns.");
    for I in 2 .. Argument.Get_Nbre_Arg loop
      Compile_Pattern (Argument.Get_Parameter (Occurence => I));
    end loop;
    return;
  end if;


  -- Compile 1st args as pattern
  Compile_Pattern (Argument.Get_Parameter (Occurence => 1), False);

  -- Check pattern vs other arguments
  for I in 2 .. Argument.Get_Nbre_Arg loop
    Regular_Expressions.Exec (Pattern,
                              Argument.Get_Parameter (Occurence => I),
                              N_Matched,
                              Match_Info);
    Ada.Text_Io.Put ("String >"
                    & Argument.Get_Parameter (Occurence => I)
                    & "< ");
    if N_Matched = 0 then
      Ada.Text_Io.Put_Line ("does not match");
    else
      Ada.Text_Io.Put ("matches at pos");
      -- List submatches
      for I in Match_Range'(1) .. N_Matched loop
        if Match_Info(I).Start_Offset > Match_Info(I).End_Offset then
          Ada.Text_Io.Put (" [" & Image(Match_Info(I).Start_Offset)
                          & "-" & Image(Match_Info(I).End_Offset) & "]");
        else
          Ada.Text_Io.Put (" [" & Image(Match_Info(I).Start_Offset)
                          & "-" & Image(Match_Info(I).End_Offset) & "]");
        end if;
      end loop;
      Ada.Text_Io.New_Line;
    end if;

  end loop;

end T_Regexp;

