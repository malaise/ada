with Ada.Calendar;
with Argument, Basic_Proc, Sys_Calls, As.U, Num_Match, Normal, Images;
with Sok_Types, Sok_File;
procedure Scores is

  package Frame_Match is new Num_Match (Sok_Types.Frame_Range);

  procedure Help is
  begin
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
      & " <command> [ <frames> ]");
    Basic_Proc.Put_Line_Error (
      "  <command > ::= --reset | --max-time | --merge <file> | --dump");
    Basic_Proc.Put_Line_Error (
      "  <frames> ::= <num_match_criteria>");
  end Help;

  procedure Error (Msg : in String) is
  begin
    Basic_Proc.Put_Line_Error ("Error: " & Msg & ".");
    Basic_Proc.Set_Error_Exit_Code;
  end Error;

  -- Current command
  type Command_List is (Reset, Max_Time, Merge, Dump);
  Command : Command_List;
  -- Optional file name and frame range
  File : As.U.Asu_Us;
  Frames : As.U.Asu_Us;
  -- Next arg to parse
  Next_Arg : Positive;
  -- Scores to merge
  Score1, Score2 : Sok_Types.Score_Rec;
  -- Has score changed due to merge
  Changed : Boolean;
  -- Max day duration: One day less 1ms
  Max_Dur : constant Ada.Calendar.Day_Duration
          := Ada.Calendar.Day_Duration'Last - 0.001;

begin

  -- Check arguments
  if Argument.Get_Nbre_Arg < 1 then
    Error ("Invalid arguments");
    return;
  else
    if Argument.Get_Parameter = "--help" then
      Help;
      Basic_Proc.Set_Error_Exit_Code;
      return;
    elsif Argument.Get_Parameter = "--reset" then
      Command := Reset;
      Next_Arg := 2;
    elsif Argument.Get_Parameter = "--max-time" then
      Command := Max_Time;
      Next_Arg := 2;
    elsif Argument.Get_Parameter = "--merge" then
      Command := Merge;
      if Argument.Get_Nbre_Arg < 2 then
        Error ("Missing file name");
        return;
      end if;
      -- Get file name
      File := As.U.Tus (Argument.Get_Parameter (Occurence => 2));
      if not Sys_Calls.File_Found (File.Image) then
        Error ("File " & File.Image & " not found");
        return;
      end if;
      Next_Arg := 3;
    elsif Argument.Get_Parameter = "--dump" then
      Command := Dump;
      Next_Arg := 2;
    else
      Error ("Invalid command " & Argument.Get_Parameter);
      return;
    end if;
  end if;

  -- Get optional frame range
  if Argument.Get_Nbre_Arg < Next_Arg then
    -- No Frame range: Any frame matches
    Frames := As.U.Tus ("-");
  elsif Argument.Get_Nbre_Arg = Next_Arg then
    Frames := As.U.Tus (Argument.Get_Parameter (Occurence => Next_Arg));
  else
    Error ("Too many arguments");
    return;
  end if;

  -- Put title if dump
  if Command = Dump then
    Basic_Proc.Put_Line_Output (
        "No                    Time      Moves      Pushes");
  end if;

  -- Do command for matching frames
  for Frame in Sok_Types.Frame_Range loop
    if Frame_Match.Matches (Frame, Frames.Image) then
      -- Reset score of matching frames
      case Command is
        when Reset =>
          Sok_File.Reset_Score (Frame);
        when Max_Time =>
          -- (Re)set time to max
          Score1 := Sok_File.Read_Score (Frame);
          Score1.Day := Natural'Last;
          Score1.Dur := Max_Dur;
          Sok_File.Write_Score (Frame, Score1);
        when Merge =>
          -- Keep best time and score of matching frames
          Score1 := Sok_File.Read_Score (Frame);
          Score2 := Sok_File.Read_Score (Frame, File.Image);
          Changed := False;
          if Score2.Set then
          if not Score1.Set then
            Score1 := Score2;
            Changed := True;
          else
            -- Keep best time
            if Score1.Day > Score2.Day
            or else (Score1.Day = Score2.Day
                     and then Score1.Dur > Score2.Dur) then
              Score1.Day := Score2.Day;
              Score1.Dur := Score2.Dur;
              Changed := True;
            end if;
            -- Keep best score
            if Score1.Pushes > Score2.Pushes
            or else (Score1.Pushes = Score2.Pushes
                     and then Score1.Moves > Score2.Moves) then
              Score1.Moves := Score2.Moves;
              Score1.Pushes := Score2.Pushes;
              Changed := True;
            end if;
          end if;
          end if;
          if Changed then
            Sok_File.Write_Score (Frame, Score1);
          end if;
        when Dump =>
          -- Dump score
          Score1 := Sok_File.Read_Score (Frame);
          -- Frame on 2 chars + 1 sep
          Basic_Proc.Put_Output (Normal (Frame, 2, Gap => '0') & " ");
          -- Time on 23 chars (10+1+12) + 1 sep
          Basic_Proc.Put_Output (Normal (Score1.Day, 10) & "d"
                               & Images.Dur_Image (Score1.Dur) & " ");
          -- Moves and Pushes on 21 chars (10+1+10)
          Basic_Proc.Put_Output (Normal (Score1.Moves, 10) & " "
                               & Normal (Score1.Pushes, 10));
          Basic_Proc.New_Line_Output;
      end case;
    end if;
  end loop;

end Scores;

