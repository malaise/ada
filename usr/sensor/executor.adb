with Event_Mng, Timers, Any_Def, As.U.Utils, Sys_Calls, Aski;
with Debug, Rules, Filters, Searcher;
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
    Filter : Filters.Filter_Rec;
    Matches : As.U.Utils.Asu_Dyn_List_Mng.List_Type;
    Hist : As.U.Asu_Us;
    Result : As.U.Asu_Us;
    Dummy : Integer;
    use type As.U.Asu_Us;
  begin
     -- Retrieve the filter index from data and read it
     Filter := Filters.Get_Filter (Data.Inte);
     Debug.Log ("Expiration of filter on " & Filter.File.Image);
     -- Search the pattern in the tail of the file
     Searcher.Search (Filter.File.Image, Filter.Tail,
                      Filter.Seconds, Filter.Time_Format,
                      Filter.Pattern,
                      Matches);
     -- Done if no match
     if Matches.Is_Empty then
       return False;
     end if;
     -- If found, check each line of the result in the history
     if Filter.History /= null then
       for I in 1 .. Filter.History.Length loop
         Hist := Filter.History.Look_First (I);
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
       if Filter.History /= null then
         Debug.Log ("  Saving " & Matches.Access_Current.Image);
         Filter.History.Push (Matches.Access_Current.all);
       end if;
       Result := Result & Matches.Access_Current.all & Aski.Lf;
       exit when not Matches.Check_Move;
       Matches.Move_To;
     end loop;

     -- Expand the rule and execute it
     Debug.Log ("  Action "
              & Rules.Expand (Filter.Rule.Image, Result.Image));
     Dummy := Sys_Calls.Call_System (
         Rules.Expand (Filter.Rule.Image, Result.Image));
    return False;
  end Expire;

  -- Init the executor (arms timers and sets handlers)
  procedure Init is
    Filter : Filters.Filter_Rec;
    Data : Any_Def.Any (Any_Def.Inte_Kind);
    Appointment : Timers.Delay_Rec  (Timers.Delay_Sec);
    Dummy_Id : Timers.Timer_Id;
  begin
    -- Set sigterm callback
    Event_Mng.Set_Sig_Term_Callback (Exit_Handler'Access);
    -- Arm the timers, each associated to a filter
    Appointment.Delay_Seconds := 0.0;
    for I in 1 .. Filters.Get_Number loop
      -- Retrieve the filter
      Filter := Filters.Get_Filter (I);
      Appointment.Period := Filter.Period;
      -- Arm the timer
      Data.Inte := I;
      Dummy_Id := Timers.Create (Appointment, Expire'Access, Data);
    end loop;
  end Init;

  -- Has exit been requested
  function Exit_Requested return Boolean is
  begin
    return Do_Exit;
  end Exit_Requested;

end Executor;

