with Calendar;
with System;
package body Task_Mng is

  type Task_State_List is (Stopped, Running, Aborted);

  -- The effective period of task and its state
  Real_Period : Duration := Activation_Period;

  -- The current state of the task
  Task_State : Task_State_List := Stopped;

  task The_Task is
    pragma Priority (System.Priority'First);
    -- change task's state
    entry Set_State;
    -- activate
    entry Schedule;
  end The_Task;

  procedure Set_Task_State (New_State : in Task_State_List) is
  begin
    if not The_Task'Callable then
      raise Task_Aborted;
    end if;
    if Task_State = Aborted then
      raise Task_Aborted;
    end if;
    if New_State = Task_State then
      return;
    end if;

    Task_State := New_State;
    if New_State /= Aborted then
      The_Task.Set_State;
    else
      select
        -- Try to warn the task
        The_Task.Set_State;
      or
        delay 1.0;
        -- kill the task which does not answer
        abort The_Task;
      end select;
    end if;
  end Set_Task_State;

  task body The_Task is
    use Calendar;
    -- Next activation date
    Next_Go : Calendar.Time;
  begin
    loop

      select
        accept Set_State;
        -- Tasks's state changes
        if Task_State = Running then
          -- Task starts : activate it immediatly
          Next_Go := Calendar.Clock;
        end if;
      or
        when Task_State = Running =>
          accept Schedule do
            -- Call_back in rendez-vous for re-entrance
            if Calendar.Clock >= Next_Go then
              Next_Go := Next_Go + Real_Period;
              begin
                Call_Back;
              exception
                when others =>
                  Task_State := Aborted;
              end;
            end if;
          end Schedule;
      or
        terminate;
      end select;

    end loop;
  end The_Task;


  -- If period is <= 0 return default at init, previous otherwise
  -- If period is <  minimum return minimum
  -- Else return period
  function Check_Period (New_Period : Duration; Init : Boolean := False)
                        return Duration is
  begin
    if New_Period <= 0.0 then
      if not Init then
        return Real_Period;
      else
        return Def_Period;
      end if;
    elsif New_Period < Min_Period then
      return Min_Period;
    else
      return New_Period;
    end if;
  end Check_Period;


  -- At elaboration, the task is ready but not started.
  -- This call starts effectively the task, eventually with a new period.
  -- A null or negative period is fobidden and discarded (default value).
  -- If the task if already started, its period is updated.
  -- If the task has been aborted, exception is raised.
  procedure Start (New_Period : in Duration := Activation_Period) is
  begin
    Real_Period := Check_Period(New_Period);
    -- Warn task about new state
    Set_Task_State (Running);
  end Start;

  -- When the the task is started, stops it.
  -- If the task is already stopped, no effect.
  -- If the task has been aborted, exception is raised.
  procedure Stop is
  begin
    -- Warn task about new state
    Set_Task_State (Stopped);
  end Stop;

  -- Aborts the task, mandatory for the main program to exit.
  -- If the task is already aborted, exception is raised.
  procedure Abort_Task is
  begin
    -- Try to warn the task
    Set_Task_State (Aborted);
  end Abort_Task;


  -- Returns the current period of activation.
  -- If the task is already aborted, exception is raised.
  function Get_Period return Duration is
  begin
    if Task_State /= Aborted then
      return Real_Period;
    else
      raise Task_Aborted;
    end if;
  end Get_Period;

  procedure Schedule is
  begin
    case Task_State is
      when Stopped =>
        return;
      when Running =>
        The_Task.Schedule;
      when Aborted =>
        return;
    end case;
  end Schedule;

begin
  -- store the initial period
  Real_Period := Check_Period(Activation_Period, True);
end Task_Mng;
