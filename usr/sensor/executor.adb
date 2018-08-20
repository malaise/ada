with Event_Mng, Timers, Any_Def, As.U.Utils, Sys_Calls, Aski;
with Debug, Actions, Rules, Searcher;
package body Executor is

  -- Exit request handler
  Do_Exit : Boolean := False;
  procedure Exit_Handler is
  begin
    Do_Exit := True;
  end Exit_Handler;

  -- The timer callbackl
  function Expire (Dummy_Id : in Timers.Timer_Id;
                   Data : in Timers.Timer_Data) return Boolean is
    Rule : Rules.Rule_Rec;
    Matches : As.U.Utils.Asu_Dyn_List_Mng.List_Type;
    Hist : As.U.Asu_Us;
    Result : As.U.Asu_Us;
    Dummy : Integer;
    use type As.U.Asu_Us;
  begin
     -- Retrieve the filter index from data and read it
     Rule := Rules.Get_Rule (Data.Lint);
     Debug.Log ("Expiration of rule on " & Rule.File.Image);
     -- Search the pattern in the tail of the file
     Searcher.Search (Rule.File.Image, Rule.Tail,
                      Rule.Seconds, Rule.Time_Format,
                      Rule.Pattern,
                      Matches);
     -- Done if no match
     if Matches.Is_Empty then
       return False;
     end if;
     -- If found, check each line of the result in the history
     if Rule.History /= null then
       for I in 1 .. Rule.History.Length loop
         Hist := Rule.History.Look_First (I);
         Matches.Rewind;
         loop
           -- Delete the matching lines that are already known
           if Matches.Access_Current.all = Hist then
             Debug.Log ("  Match " & Matches.Access_Current.Image
                        & " is in history");
             Matches.Delete;
             if Matches.Is_Empty then
               return False;
             end if;
           else
             -- Check next matching line
             exit when not Matches.Check_Move;
             Matches.Move_To;
           end if;
         end loop;
       end loop;
     end if;
     -- Done if all already in history (all deleted)

     -- Save the new lines in history and concat them
     Matches.Rewind;
     loop
       -- Store in history
       if Rule.History /= null then
         Debug.Log ("  Saving " & Matches.Access_Current.Image);
         Rule.History.Push (Matches.Access_Current.all);
       end if;
       Result := Result & Matches.Access_Current.all & Aski.Lf;
       exit when not Matches.Check_Move;
       Matches.Move_To;
     end loop;

     -- Expand the command and execute it
     Debug.Log ("  Action "
              & Actions.Expand (Rule.Action.Image, Result.Image));
     Dummy := Sys_Calls.Call_System (
         Actions.Expand (Rule.Action.Image, Result.Image));
    return False;
  end Expire;

  -- Init the executor (arms timers and sets handlers)
  procedure Init is
    Rule : Rules.Rule_Rec;
    Data : Any_Def.Any (Any_Def.Lint_Kind);
    Appointment : Timers.Delay_Rec  (Timers.Delay_Sec);
    Dummy_Id : Timers.Timer_Id;
  begin
    -- Set sigterm callback
    Event_Mng.Set_Sig_Term_Callback (Exit_Handler'Access);
    -- Arm the timers, each associated to a rule
    Appointment.Delay_Seconds := 0.0;
    for I in 1 .. Rules.Get_Number loop
      -- Retrieve the rule
      Rule := Rules.Get_Rule (I);
      Appointment.Period := Rule.Period;
      -- Arm the timer
      Data.Lint := I;
      Dummy_Id := Timers.Create (Appointment, Expire'Access, Data);
    end loop;
  end Init;

  -- Has exit been requested
  function Exit_Requested return Boolean is (Do_Exit);

end Executor;

