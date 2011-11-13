with Ada.Calendar;

with Normal, Argument, Basic_Proc, Day_Mng, Console, Flo_Io;

with Types, File, Euristic;

procedure Hungar is
  Dim : Natural;
  Sigma : Float;
  Loc_Ideal_Note, Ideal_Note, Loc_Note : Float;
  Loc_J : Types.Index_Range;
  Nb_Iterations : Positive;
  Max_Iter_Digits : constant := 3;
  Start_Time : Ada.Calendar.Time;

begin
  if Argument.Get_Nbre_Arg /= 1 then
    Basic_Proc.Put_Line_Output ("Syntax error. Usage : hungar <file_name>");
    return;
  end if;

  Start_Time := Ada.Calendar.Clock;

  Solve:
  declare
    Mattrix : constant Types.Mattrix_Rec_Access :=
      new Types.Mattrix_Rec'(File.Read (Argument.Get_Parameter));
    Done : Boolean;
  begin

    Dim := Mattrix.Dim;

    Euristic.Search (Mattrix.all, Nb_Iterations, Done);

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
        Flo_Io.Put (Loc_Note, 3, 2, 0);
        Sigma := Sigma + Loc_Note;

        -- Ideal minimum cost
        Ideal_Note := Ideal_Note + Loc_Ideal_Note;
        Basic_Proc.Put_Output ("   Ideal: ");
        Flo_Io.Put (Loc_Ideal_Note, 3, 2, 0);

        -- Loss
        if abs (Loc_Ideal_Note - Loc_Note) > File.Epsilon then
          Basic_Proc.Put_Output (" Loss: ");
          Flo_Io.Put (abs (Loc_Ideal_Note - Loc_Note), 3, 2, 0);
        end if;
        Basic_Proc.New_Line_Output;

      end loop;
      Basic_Proc.New_Line_Output;

      -- Total
      if Types."=" (File.Get_Kind, Types.Regret) then
        Basic_Proc.Put_Output ("Total cost: ");
        Flo_Io.Put (Sigma, 6, 2, 0);
        Basic_Proc.Put_Output ("  Ideal cost: ");
        Flo_Io.Put (Ideal_Note, 6, 2, 0);
      else
        Basic_Proc.Put_Output ("Total note: ");
        Flo_Io.Put (Sigma, 6, 2, 0);
        Basic_Proc.Put_Output ("  Ideal note: ");
        Flo_Io.Put (Ideal_Note, 6, 2, 0);
      end if;
      Basic_Proc.Put_Output ("  Total loss: ");
      Flo_Io.Put (abs (Ideal_Note - Sigma), 6, 2, 0);
      Basic_Proc.New_Line_Output;
    else
      -- Not done
      Basic_Proc.Put_Line_Output ("No solution found.");
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
    Millisecs : Day_Mng.T_Millisec;
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
    null;
end Hungar;

