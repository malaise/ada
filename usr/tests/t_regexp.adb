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
  Ok : Boolean;

  procedure Compile_Pattern (Str : in String; Report : in Boolean := True) is
  begin
    -- Compile pattern
    Regular_Expressions.Compile (Pattern, Ok, Str);
    if not Report then
      return;
    end if;
    if not Ok then
      Ada.Text_Io.Put_Line ("Error compiling pattern >" & Str & "<");
      Ada.Text_Io.Put_Line (Regular_Expressions.Error (Pattern));
    else
      Ada.Text_Io.Put_Line ("Compiled pattern >" & Str & "<");
    end if;
  end Compile_Pattern;

  Match_Info : Regular_Expressions.Match_Array(1 .. 50);
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
  Compile_Pattern (Argument.Get_Parameter (Occurence => 1));

  -- Check pattern vs other arguments
  for I in 2 .. Argument.Get_Nbre_Arg loop
    Regular_Expressions.Exec (Pattern,
                              Argument.Get_Parameter (Occurence => I),
                              Ok,
                              Match_Info);
    Ada.Text_Io.Put ("String >"
                    & Argument.Get_Parameter (Occurence => I)
                    & "< ");
    if not Ok then
      Ada.Text_Io.Put_Line ("does not match");
    else
      Ada.Text_Io.Put ("matches at pos");
      for I in Match_Info'Range loop
        exit when Match_Info(I).Start_Offset >  Match_Info(I).End_Offset;
        Ada.Text_Io.Put (" [" & Image(Match_Info(I).Start_Offset)
                        & "-" & Image(Match_Info(I).End_Offset) & "]");
      end loop;
      Ada.Text_Io.New_Line;
    end if;

  end loop;

end T_Regexp;

