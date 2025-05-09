with Ada.Calendar;
with Event_Mng, Timers, Any_Def, As.U.Utils, Sys_Calls, Aski, Long_Longs,
     Long_Long_Dynamic_List;
with Debug, Actions, Rules, Searcher;
package body Executor is

  -- Start time (to skip previous logs)
  Start_Time : Ada.Calendar.Time;

  -- List or Timers and data
  type Timer_Rec is record
    Tid : Timers.Timer_Id;
    Rule : Long_Longs.Ll_Positive;
  end record;
  package Timer_List_Mng is new Long_Long_Dynamic_List (Timer_Rec);
  package Timer_Mng renames Timer_List_Mng.Dyn_List;
  Timers_List : Timer_Mng.List_Type;

  -- Exit request handler
  Do_Exit : Boolean := False;
  procedure Exit_Handler is
  begin
    Do_Exit := True;
  end Exit_Handler;

  -- Check the lines matching rule
  procedure Check (Rule : Rules.Rule_Rec) is
    Matches : As.U.Utils.Asu_Dyn_List_Mng.List_Type;
    Hist : As.U.Asu_Us;
    use type As.U.Asu_Us, Rules.History_Access;
  begin
     -- Search the pattern in the tail of the file
     Searcher.Search (Rule.File.Image, Start_Time, Rule.Tail,
                      Rule.Aging, Rule.Time_Format,
                      Rule.Pattern,
                      Matches);
     -- Done if no match
     if Matches.Is_Empty then
       return;
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
               -- Done if all already in history (all deleted)
               return;
             end if;
           else
             -- Check next matching line
             exit when not Matches.Check_Move;
             Matches.Move_To;
           end if;
         end loop;
       end loop;
     end if;

     -- Save the new lines in history and append them to the matching
     Matches.Rewind;
     loop
       -- Store in history
       if Rule.History /= null then
         Debug.Log ("  Saving " & Matches.Access_Current.Image);
         Rule.History.Push (Matches.Access_Current.all);
       end if;
       -- Append this new matching line to the list of matching
       Rule.Nb_Match.all := Rule.Nb_Match.all + 1;
       Rule.Matches.all := Rule.Matches.all
                         & Matches.Access_Current.all & Aski.Lf;
       exit when not Matches.Check_Move;
       Matches.Move_To;
     end loop;
  end Check;

  -- Expiration or termination of time
  function Execute (Rule_Num : Long_Longs.Ll_Positive;
                    Flush : Boolean) return Boolean is
    Rule : Rules.Rule_Rec;
    Now : Ada.Calendar.Time;
    use type Ada.Calendar.Time;

    -- Expand the command and execute it if not empty
    procedure Launch (Action, Lines : in String) is
      Expanded : As.U.Asu_Us;
      Dummy : Integer;
    begin
      Expanded := As.U.Tus (Actions.Expand (Action, Lines));
      Debug.Log ("  Launching " & Expanded.Image);
      if not Expanded.Is_Null then
        Dummy := Sys_Calls.Call_System (Expanded.Image);
      end if;
    end Launch;

  begin
    -- Retrieve the filter index from data and read it
    Rule := Rules.Get_Rule (Rule_Num);
    Debug.Log ("Expiration of rule " & Rule.Name.Image
             & " on " & Rule.File.Image);
    -- Search the pattern in the tail of the file
    Check (Rule);
    -- Nothing to do if no match
    if Rule.Nb_Match.all = 0 then
      return False;
    end if;
    -- Check latency
    Now := Ada.Calendar.Clock;
    if Rule.Latency /= 0.0
        and then Rule.Previous.all + Rule.Latency > Now
        and then not Flush then
      -- Within latency, these matches will be processed later on
      Debug.Log ("  Within latency");
      return False;
    end if;
    -- Expand the command and execute it if not empty
    Debug.Log ("  Executing actions on " & Rule.Matches.Image);
    for I in 1 .. Rule.Actions.Length loop
      Launch (Rule.Actions.Element(I).Image, Rule.Matches.Image);
      -- Execute actions triggered by repetitions of action
      Actions.Set_Action (Rule.Actions.Element(I).Image);
      for Repeat of Actions.Occurs (Rule.Actions.Element(I).Image,
                                    Rule.Nb_Match.all) loop
        Debug.Log ("  Repeated action on " & Rule.Matches.Image);
        Launch (Repeat.Image, Rule.Matches.Image);
      end loop;
      Actions.Unset_Action;
    end loop;
    -- Reset result, and reset latency reference
    Rule.Nb_Match.all := 0;
    Rule.Matches.Set_Null;
    Rule.Previous.all := Now;
    return False;
  end Execute;

  -- The timer callback
  function Expire (Dummy_Id : Timers.Timer_Id;
                   Data : Timers.Timer_Data) return Boolean is
  begin
    return Execute (Data.Lint, Flush => False);
  end Expire;

  Purge_Id : Timers.Timer_Id;
  function Purge (Dummy_Id : Timers.Timer_Id;
                  Dummy_Data : Timers.Timer_Data) return Boolean is
  begin
    Actions.Purge;
    return False;
  end Purge;

  -- Init the executor (arms timers and sets handlers)
  procedure Init is
    Rule : Rules.Rule_Rec;
    Data : Any_Def.Any (Any_Def.Lint_Kind);
    Appointment : Timers.Delay_Rec  (Timers.Delay_Sec);
    Max_Period : Timers.Period_Range := 0.0;
    Id : Timers.Timer_Id;
  begin
    -- Init start time
    Start_Time := Ada.Calendar.Clock;
    -- Set sigterm callback
    Event_Mng.Set_Sig_Term_Callback (Exit_Handler'Access);
    -- Arm the timers, each associated to a rule
    Appointment.Delay_Seconds := 0.0;
    for I in 1 .. Rules.Get_Number loop
      -- Retrieve the rule
      Rule := Rules.Get_Rule (I);
      Appointment.Period := Rule.Period;
      -- Update max period
      if Rule.Period > Max_Period then
        Max_Period := Rule.Period;
      end if;
      -- Arm the timer
      Data.Lint := I;
      Id := Timers.Create (Appointment, Expire'Access, Data);
      Timers_List.Insert ( (Id, I) );
    end loop;
    -- Arm a timer for purge
    if Max_Period /= 0.0 then
      Appointment.Period := Max_Period;
      Data.Lint := 0;
      Purge_Id := Timers.Create (Appointment, Purge'Access, Data);
    end if;
  end Init;

  -- Has exit been requested
  function Exit_Requested return Boolean is (Do_Exit);

  -- Terminate actions (flush actions pending on latency)
  procedure Close is
    Timer : Timer_Rec;
    Dummy_Res : Boolean;
  begin
    -- Delete and expire all timers, flushing actions pending on latency
    Timers_List.Rewind (Check_Empty => False);
    while not Timers_List.Is_Empty loop
      Timers_List.Get (Timer);
      Timers.Delete (Timer.Tid);
      Dummy_Res := Execute (Timer.Rule, Flush => True);
    end loop;
    Timers.Delete_If_Exists (Purge_Id);
  end Close;

end Executor;

