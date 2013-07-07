with Argument, Basic_Proc, Sys_Calls, As.U, Num_Match;
with Sok_Types, Sok_File;
procedure Scores is

  package Frame_Match is new Num_Match (Sok_Types.Frame_Range);

  procedure Help is
  begin
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
      & " <command> [ <frames> ]");
    Basic_Proc.Put_Line_Error (
      "  <command > ::= --reset | --merge <file> ");
    Basic_Proc.Put_Line_Error (
      "  <frames> ::= <num_match_criteria>");
  end Help;

  procedure Error (Msg : in String) is
  begin
    Basic_Proc.Put_Line_Error ("Error: " & Msg & ".");
    Basic_Proc.Set_Error_Exit_Code;
  end Error;

  -- Current command
  Reset : Boolean;
  -- Optional file name and frame range
  File : As.U.Asu_Us;
  Frames : As.U.Asu_Us;
  -- Next arg to parse
  Next_Arg : Positive;
  -- Scores to merge
  Score1, Score2 : Sok_Types.Score_Rec;
  -- Has score changed due to merge
  Changed : Boolean;

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
      Reset := True;
      Next_Arg := 2;
    elsif Argument.Get_Parameter = "--merge" then
      Reset := False;
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

  for Frame in Sok_Types.Frame_Range loop
    if Frame_Match.Matches (Frame, Frames.Image) then
      -- Reset score of matching frames
      if Reset then
        Sok_File.Reset_Score (Frame);
      else
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
      end if;
    end if;
  end loop;

end Scores;

