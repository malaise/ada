with Ada.Calendar, Ada.Exceptions;
with Normal, Argument, Basic_Proc, Day_Mng, Console, Normalization, As.U;
with Types, File, Euristic;
procedure Hungar is
  Sigma : Float;
  Loc_Ideal_Note, Ideal_Note, Loc_Note : Float;
  Loc_J : Types.Index_Range;
  Nb_Iterations : Positive;
  Max_Iter_Digits : constant := 3;
  Nok_Exit_Code : constant Natural := 2;
  File_Name : As.U.Asu_Us;
  Progress : Boolean;
  Start_Time : Ada.Calendar.Time;
  Start_Arg : Positive;
  -- Max iterations, default is 0 => infinite
  -- Limited with no value is -1 => (Dim^2+1)*10
  Max_Iter : Integer;
  Default_Iter : constant Integer := -1;

  Quit_Error: exception;
  procedure Syntax_Error is
  begin
    Basic_Proc.Put_Line_Output (
      "Syntax error. Usage: hungar [ --progress ] [ --max [=<val> ] <file_name>");
    Basic_Proc.Put_Line_Output ("  Default is no max");
    Basic_Proc.Put_Line_Output ("  Default max is (Dim^2+1)*10");
    raise Quit_Error;
  end Syntax_Error;

begin
  Start_Time := Ada.Calendar.Clock;
  Start_Arg := 1;
  Progress := False;
  Max_Iter := 0;
  if Argument.Get_Nbre_Arg = 0 then
    Syntax_Error;
  end if;
  loop
    if Start_Arg > 3 then
      -- Too many options
      Syntax_Error;
    elsif Argument.Get_Parameter (Occurence => Start_Arg) = "--help" then
      Syntax_Error;
    elsif Argument.Get_Parameter (Occurence => Start_Arg) = "--progress" then
      if Progress then
        -- Option appears twice
        Syntax_Error;
      end if;
      Progress := True;
      Start_Arg := Start_Arg + 1;
    elsif Argument.Get_Parameter (Occurence => Start_Arg) = "--max" then
      if Max_Iter /= 0 then
        -- Option appears twice
        Syntax_Error;
      end if;
      -- No value => default
      Max_Iter := Default_Iter;
      Start_Arg := Start_Arg + 1;
    elsif Argument.Get_Parameter (Occurence => Start_Arg)'Length > 6
    and then Argument.Get_Parameter (Occurence => Start_Arg)(1..6) = "--max="
    then
      if Max_Iter /= 0 then
        -- Option appears twice
        Syntax_Error;
      end if;
      declare
        Str : constant String
            := Argument.Get_Parameter (Occurence => Start_Arg);
      begin
        Max_Iter := Positive'Value (Str(7 .. Str'Last));
      exception
        when others =>
          Syntax_Error;
      end;
      Start_Arg := Start_Arg + 1;
    elsif Start_Arg = Argument.Get_Nbre_Arg then
      -- Last arg is the file name
      Argument.Get_Parameter (File_Name, Occurence => Start_Arg);
      exit;
    else
      Syntax_Error;
    end if;
  end loop;

  Solve:
  declare
    Mattrix : constant not null Types.Mattrix_Rec_Access :=
      new Types.Mattrix_Rec'(File.Read (File_Name.Image));
    Dim : constant Natural := Mattrix.Dim;
    Done : Boolean;
  begin
    -- Max iterations default value
    if Max_Iter = Default_Iter then
      Max_Iter := (Dim * Dim + 1) * 10;
    end if;

    Euristic.Search (Mattrix.all, Max_Iter, Progress, Nb_Iterations, Done);

    if Done then
      Basic_Proc.Put_Line_Output ("Result:");
      Sigma := 0.0;
      Ideal_Note := 0.0;
      for I in 1 .. Dim loop
        if Types."=" (File.Get_Kind, Types.Regret) then
          Loc_Ideal_Note := Float'Last;
        else
          Loc_Ideal_Note := Float'First;
        end if;
        for J in 1 .. Dim loop
          if Types."=" (File.Get_Kind, Types.Regret) then
            -- Lowest note of this row
            if File.Get_Note(I, J) < Loc_Ideal_Note then
              Loc_Ideal_Note := File.Get_Note(I, J);
            end if;
          else
            -- Highest note of this row
            if File.Get_Note(I, J) > Loc_Ideal_Note then
              Loc_Ideal_Note := File.Get_Note(I, J);
            end if;
          end if;
          if Mattrix.Notes(I, J) = 1 then
            -- Affectation found
            Loc_J := J;
          end if;
        end loop;

        -- Affectation
        Basic_Proc.Put_Output ("row " & Normal(I, 3) & " column " & Normal(Loc_J, 3));
        Loc_Note := File.Get_Note(I, Loc_J);
        if Types."=" (File.Get_Kind, Types.Regret) then
          Basic_Proc.Put_Output (" cost: ");
        else
          Basic_Proc.Put_Output (" note: ");
        end if;
        Basic_Proc.Put_Output (Normalization.Normal_Fixed (Loc_Note, 7, 4));
        Sigma := Sigma + Loc_Note;

        -- Ideal minimum cost
        Ideal_Note := Ideal_Note + Loc_Ideal_Note;
        Basic_Proc.Put_Output ("   Ideal: ");
        Basic_Proc.Put_Output (Normalization.Normal_Fixed (
                                       Loc_Ideal_Note, 7, 4));

        -- Loss
        if abs (Loc_Ideal_Note - Loc_Note) > File.Epsilon then
          Basic_Proc.Put_Output (" Loss: ");
          Basic_Proc.Put_Output (Normalization.Normal_Fixed (
              abs (Loc_Ideal_Note - Loc_Note), 7, 4));
        end if;
        Basic_Proc.New_Line_Output;

      end loop;
      Basic_Proc.New_Line_Output;

      -- Total
      if Types."=" (File.Get_Kind, Types.Regret) then
        Basic_Proc.Put_Output ("Total cost: ");
        Basic_Proc.Put_Output(Normalization.Normal_Fixed(Sigma, 8, 5));
        Basic_Proc.Put_Output ("  Ideal cost: ");
        Basic_Proc.Put_Output(Normalization.Normal_Fixed(Ideal_Note, 8, 5));
      else
        Basic_Proc.Put_Output ("Total note: ");
        Basic_Proc.Put_Output(Normalization.Normal_Fixed(Sigma, 8, 5));
        Basic_Proc.Put_Output ("  Ideal note: ");
        Basic_Proc.Put_Output(Normalization.Normal_Fixed(Ideal_Note, 8, 5));
      end if;
      Basic_Proc.Put_Output ("  Total loss: ");
      Basic_Proc.Put_Output (Normalization.Normal_Fixed(
          abs (Ideal_Note - Sigma), 8, 5));
      Basic_Proc.New_Line_Output;
    else
      -- Not done
      Basic_Proc.Put_Line_Output ("No solution found.");
      Basic_Proc.Set_Exit_Code (Nok_Exit_Code);
    end if;
  end Solve;

  Basic_Proc.Put_Output ("Iter: ");
  if Positive'Image(Nb_Iterations)'Length - 1 >= Max_Iter_Digits then
    Basic_Proc.Put_Output (Positive'Image(Nb_Iterations));
  else
    Basic_Proc.Put_Output (Normal (Nb_Iterations, Max_Iter_Digits));
  end if;

  Compute_Elapse:
  declare
    use type Ada.Calendar.Time;
    Dur : Duration;
    Days : Natural;
    Hours : Day_Mng.T_Hours;
    Minutes : Day_Mng.T_Minutes;
    Seconds : Day_Mng.T_Seconds;
    Millisecs : Day_Mng.T_Millisecs;
  begin
    Dur := Ada.Calendar.Clock - Start_Time;
    Days := 0;
    while Dur > Ada.Calendar.Day_Duration'Last loop
      Days := Days + 1;
      Dur := Dur - Ada.Calendar.Day_Duration'Last;
    end loop;
    Day_Mng.Split (Dur, Hours, Minutes, Seconds, Millisecs);
    Display_Elapse:
    declare
      Some_Time_Put : Boolean := False;
      procedure Put_Time (Val : in Natural; Msg : in String) is
      begin
        if Val = 0 and then not Some_Time_Put then
          return;
        end if;
        Some_Time_Put := True;
        Basic_Proc.Put_Output (Natural'Image(Val) & " " & Msg);
        if Val > 1 then
          Basic_Proc.Put_Output ("s");
        end if;
      end Put_Time;
    begin
      Basic_Proc.Put_Output ("    In");
      Put_Time (Days, "day");
      Put_Time (Hours, "hour");
      Put_Time (Minutes, "minute");
      Put_Time (Seconds, "second");
      if Some_Time_Put then
        Basic_Proc.Put_Output (" and");
      end if;
      Put_Time (Millisecs, "millisec");
      Basic_Proc.Put_Line_Output (".");
    end Display_Elapse;
  end Compute_Elapse;
  Console.Sound;

exception
  when File.Read_Error =>
    Basic_Proc.Set_Error_Exit_Code;
  when Quit_Error =>
    Basic_Proc.Set_Error_Exit_Code;
  when Error:others =>
    Basic_Proc.Put_Line_Error ("ERROR: Exception " &
        Ada.Exceptions.Exception_Name (Error) & " raised.");
    Basic_Proc.Set_Error_Exit_Code;
end Hungar;

