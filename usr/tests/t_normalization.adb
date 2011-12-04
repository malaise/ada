with Ada.Characters.Latin_1;
with Normalization, Get_Float, Async_Stdin, Basic_Proc, Argument, As.U,
     Unbounded_Arrays, Command, Many_Strings, Event_Mng;
procedure T_Normalization is
  F : Float;
  type Delt_Range is delta 0.00001 digits 13
                     range -99_999_999.99999 .. 99_999_999.99999;
  D : Delt_Range;
  function Normal_Delt is new Normalization.Normal_Delt_Dig (Delt_Range);

  Len : Positive;
  Exp : Positive;

  -- Automatic test input and result
  type Test_Rec is record
    F : Float;
    Len : Positive;
    Exp : Positive;
    Res : As.U.Asu_Us;
  end record;
  type Test_Array is array (Positive range <>) of Test_Rec;
  package Test_Uarrays is new Unbounded_Arrays (Test_Rec, Test_Array);
  Tests : Test_Uarrays.Unbounded_Array;


  -- Append a test
  procedure Append (F : in Float; Len : in Positive;
                    Exp : in Positive; Res : in String) is
  begin
    Tests.Append ( Test_Rec'(F, Len, Exp, As.U.Tus (Res)));
  end Append;

  -- Do one automatic test
  function Do_Test (Test : Test_Rec) return Boolean is
    Cmd : Many_Strings.Many_String;
    Flow : aliased Command.Flow_Rec (Command.Str);
    Code : Command.Exit_Code_Range;
    Strlen : Natural;
    use type As.U.Asu_Us;
  begin
    Async_Stdin.Put_Out (Test.F'Img & Test.Len'Img & Test.Exp'Img
                       & " " & Test.Res.Image & " -> ");
    -- Prepare command of test
    Cmd.Set (Argument.Get_Program_Name);
    Cmd.Cat (Test.F'Img);
    Cmd.Cat (Test.Len'Img);
    Cmd.Cat (Test.Exp'Img);
    -- Execute test and test technical result
    Command.Execute (Cmd, True, Command.Both,
                     Flow'Unrestricted_Access, Flow'Unrestricted_Access,
                     Code);
    if Event_Mng.Reset_Default_Signals_Policy then
       -- Command aborted
      Basic_Proc.Put_Line_Error ("Aborted");
      return False;
    end if;
    if Code = Command.Error then
      -- Command returns error
      Basic_Proc.Put_Line_Error ("ERROR: " & Flow.Str.Image);
      return False;
    end if;

    -- Raw mode appends a line feed, remove them
    Strlen := Flow.Str.Length;
    if Strlen >= 1
    and then Flow.Str.Element (Strlen) = Ada.Characters.Latin_1.Lf then
      Flow.Str.Delete (Strlen, Strlen);
    end if;
    Strlen := Flow.Str.Length;

    if Flow.Str = Test.Res then
      Async_Stdin.Put_Line_Out ("OK");
      return True;
    else
      Async_Stdin.Put_Line_Out ("ERROR: Got " & Flow.Str.Image & ".");
      return False;
    end if;
  end Do_Test;

begin

  if Argument.Get_Nbre_Arg = 0 then
    -- No arg => interactive
    loop
      loop
        begin
          Async_Stdin.Put_Out ("F ? : ");
          F := Get_Float.Get_Float (Async_Stdin.Strip_Last_Control (
                Async_Stdin.Get_Line (80, 7)));
          D := Delt_Range (F);
          exit;
        exception
          when others => null;
        end;
      end loop;
      loop
        begin
          Async_Stdin.Put_Out ("Len ? : ");
          Len := Positive'Value (Async_Stdin.Strip_Last_Control (
              Async_Stdin.Get_Line (80, 9)));
          exit;
        exception
          when others => null;
        end;
      end loop;
      loop
        begin
          Async_Stdin.Put_Out ("Exp/Fore ? : ");
          Exp := Positive'Value (Async_Stdin.Strip_Last_Control (
              Async_Stdin.Get_Line (80, 14)));
          exit;
        exception
          when others => null;
        end;
      end loop;

      Async_Stdin.Put_Line_Out ("0         1         2         3         4         5");
      Async_Stdin.Put_Line_Out ("012345678901234567890123456789012345678901234567890");
      begin
        Async_Stdin.Put_Line_Out ('>'
                                & Normalization.Normal_Digits (F, Len, Exp)
                                & '<');
      exception
        when Constraint_Error =>
          Async_Stdin.Put_Line_Out ("Constraint error");
      end;
      begin
        Async_Stdin.Put_Line_Out ('>'
                                & Normalization.Normal_Fixed (F, Len, Exp, '@')
                                & '<');
      exception
        when Constraint_Error =>
          Async_Stdin.Put_Line_Out ("Constraint error");
      end;
      begin
        Async_Stdin.Put_Line_Out ('>' & Normal_Delt (D, Len, Exp, '@') & '<');
      exception
        when Constraint_Error =>
          Async_Stdin.Put_Line_Out ("Constraint error");
      end;
      Async_Stdin.New_Line_Out;
    end loop;
  elsif Argument.Get_Nbre_Arg = 3 then
    -- 3 args => 3 Outputs on the line
    begin
      F := Get_Float.Get_Float (Argument.Get_Parameter (Occurence => 1));
      D := Delt_Range (F);
      Len := Positive'Value (Argument.Get_Parameter (Occurence => 2));
      Exp := Positive'Value (Argument.Get_Parameter (Occurence => 3));
    exception
      when others =>
        Async_Stdin.Put_Line_Err ("Invalid argument");
        raise;
    end;
    begin
      Async_Stdin.Put_Out ('>' & Normalization.Normal_Digits (F, Len, Exp)
                         & '<');
    exception
      when Constraint_Error =>
          Async_Stdin.Put_Line_Out ("Constraint_Error");
    end;
    Async_Stdin.Put_Out (" ");
    begin
      Async_Stdin.Put_Out ('>' & Normalization.Normal_Fixed (F, Len, Exp, '@')
                         & '<');
    exception
      when Constraint_Error =>
          Async_Stdin.Put_Line_Out ("Constraint_Error");
    end;
    Async_Stdin.Put_Out (" ");
    begin
      Async_Stdin.Put_Line_Out ('>' & Normal_Delt (D, Len, Exp, '@') & '<');
    exception
      when Constraint_Error =>
          Async_Stdin.Put_Line_Out ("Constraint_Error");
    end;
    return;
  elsif Argument.Get_Nbre_Arg /= 1
        or else (Argument.Get_Parameter /= "-a"
                 and then Argument.Get_Parameter /= "--auto") then
    -- More than one arg or one arg but not auto => Error
    Async_Stdin.Put_Line_Out ("Usage: " & Argument.Get_Program_Name
      & " [ -a | --auto | <real> <len> <exp_fore> ]");
    Basic_Proc.Set_Error_Exit_Code;
    return;
  end if;

  -- Now we are in automatic mode
  -- Init tests
  Append (3.14159, 10, 3, "> 3.14E+000< >  3.14159@< >  3.14159@<");
  Append (31.4159, 10, 3, "> 3.14E+001< > 31.415899< > 31.41589@<");


  -- Execute tests as long as OK
  for I in 1 .. Tests.Length loop
    exit when not Do_Test (Tests.Element (I));
  end loop;
end T_Normalization;

