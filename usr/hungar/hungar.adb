with Ada.Calendar;

with Normal;
with Argument;
with My_Io;
with Day_Mng;
with Dos;

with Types;
with File;
with Euristic;

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
    My_Io.Put_Line ("Syntax error. Usage : hungar <file_name>");
    return;
  end if;

  Start_Time := Ada.Calendar.Clock;

  Solve:
  declare
    Mattrix : Types.Mattrix_Rec_Access :=
      new Types.Mattrix_Rec'(File.Read (Argument.Get_Parameter));
  begin

    Dim := Mattrix.Dim;

    Euristic.Search (Mattrix.all, Nb_Iterations);

    My_Io.Put_Line ("Result:");
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
      My_Io.Put ("row " & Normal(I, 3) & " column " & Normal(Loc_J, 3));
      Loc_Note := File.Get_Note(I, Loc_J);
      if Types."=" (File.Get_Kind, Types.Regret) then
        My_Io.Put (" cost: ");
      else
        My_Io.Put (" note: ");
      end if;
      My_Io.Put (Loc_Note, 3, 2, 0);
      Sigma := Sigma + Loc_Note;

      -- Ideal minimum cost
      Ideal_Note := Ideal_Note + Loc_Ideal_Note;
      My_Io.Put ("   Ideal: ");
      My_Io.Put (Loc_Ideal_Note, 3, 2, 0);

      -- Loss
      if abs (Loc_Ideal_Note - Loc_Note) > File.Epsilon then
        My_Io.Put (" Loss: ");
        My_Io.Put (abs (Loc_Ideal_Note - Loc_Note), 3, 2, 0);
      end if;
      My_Io.New_Line;

    end loop;
    My_Io.New_Line;

    -- Total
    if Types."=" (File.Get_Kind, Types.Regret) then
      My_Io.Put ("Total cost: ");
      My_Io.Put(Sigma, 6, 2, 0);
      My_Io.Put ("  Ideal cost: ");
      My_Io.Put(Ideal_Note, 6, 2, 0);
    else
      My_Io.Put ("Total note: ");
      My_Io.Put(Sigma, 6, 2, 0);
      My_Io.Put ("  Ideal note: ");
      My_Io.Put(Ideal_Note, 6, 2, 0);
    end if;
    My_Io.Put ("  Total loss: ");
    My_Io.Put (abs (Ideal_Note - Sigma), 6, 2, 0);
    My_Io.New_Line;
    My_Io.Put ("Iter: ");
    if Positive'Image(Nb_Iterations)'Length - 1 >= Max_Iter_Digits then
      My_Io.Put (Positive'Image(Nb_Iterations));
    else
      My_Io.Put (Normal (Nb_Iterations, Max_Iter_Digits));
    end if;

  end Solve;

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
        My_Io.Put (Natural'Image(Val) & " " & Msg);
        if Val > 1 then
          My_Io.Put ("s");
        end if;
      end Put_Time;
    begin
      My_Io.Put ("    In");
      Put_Time (Days, "day");
      Put_Time (Hours, "hour");
      Put_Time (Minutes, "minute");
      Put_Time (Seconds, "second");
      if Some_Time_Put then
        My_Io.Put (" and");
      end if;
      Put_Time (Millisecs, "millisec");
      My_Io.Put_Line (".");
    end Display_Elapse;
  end Compute_Elapse;
  Dos.Sound;

exception
  when File.Read_Error =>
    null;
end Hungar;

