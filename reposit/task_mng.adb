with CALENDAR;
with SYSTEM;
package body TASK_MNG is

  type TASK_STATE_LIST is (STOPPED, RUNNING, ABORTED);

  -- The effective period of task and its state
  REAL_PERIOD : DURATION := ACTIVATION_PERIOD;

  -- The current state of the task
  TASK_STATE : TASK_STATE_LIST := STOPPED;

  task THE_TASK is
    pragma PRIORITY (SYSTEM.PRIORITY'FIRST);
    -- change task's state
    entry SET_STATE;
    -- activate
    entry SCHEDULE;
  end THE_TASK;

  procedure SET_TASK_STATE (NEW_STATE : in TASK_STATE_LIST) is
  begin
    if not THE_TASK'CALLABLE then
      raise TASK_ABORTED;
    end if;
    if TASK_STATE = ABORTED then
      raise TASK_ABORTED;
    end if;
    if NEW_STATE = TASK_STATE then
      return;
    end if;

    TASK_STATE := NEW_STATE;
    if NEW_STATE /= ABORTED then
      THE_TASK.SET_STATE;
    else
      select
        -- Try to warn the task
        THE_TASK.SET_STATE;
      or
        delay 1.0;
        -- kill the task which does not answer
        abort THE_TASK;
      end select;
    end if;
  end SET_TASK_STATE;

  task body THE_TASK is
    use CALENDAR;
    -- Next activation date
    NEXT_GO : CALENDAR.TIME;
  begin
    loop

      select
        accept SET_STATE;
        -- Tasks's state changes
        if TASK_STATE = RUNNING then
          -- Task starts : activate it immediatly
          NEXT_GO := CALENDAR.CLOCK;
        end if;
      or
        when TASK_STATE = RUNNING =>
          accept SCHEDULE do
            -- Call_back in rendez-vous for re-entrance
            if CALENDAR.CLOCK >= NEXT_GO then
              NEXT_GO := NEXT_GO + REAL_PERIOD;
              begin
                CALL_BACK;
              exception
                when others =>
                  TASK_STATE := ABORTED;
              end;
            end if;
          end SCHEDULE;
      or
        terminate;
      end select;

    end loop;
  end THE_TASK;


  -- If period is <= 0 return default at init, previous otherwise
  -- If period is <  minimum return minimum
  -- Else return period
  function CHECK_PERIOD (NEW_PERIOD : DURATION; INIT : BOOLEAN := FALSE)
                        return DURATION is
  begin
    if NEW_PERIOD <= 0.0 then
      if not INIT then
        return REAL_PERIOD;
      else
        return DEF_PERIOD;
      end if;
    elsif NEW_PERIOD < MIN_PERIOD then
      return MIN_PERIOD;
    else
      return NEW_PERIOD;
    end if;
  end CHECK_PERIOD;


  -- At elaboration, the task is ready but not started.
  -- This call starts effectively the task, eventually with a new period.
  -- A null or negative period is fobidden and discarded (default value).
  -- If the task if already started, its period is updated.
  -- If the task has been aborted, exception is raised.
  procedure START (NEW_PERIOD : in DURATION := ACTIVATION_PERIOD) is
  begin
    REAL_PERIOD := CHECK_PERIOD(NEW_PERIOD);
    -- Warn task about new state
    SET_TASK_STATE (RUNNING);
  end START;

  -- When the the task is started, stops it.
  -- If the task is already stopped, no effect.
  -- If the task has been aborted, exception is raised.
  procedure STOP is
  begin
    -- Warn task about new state
    SET_TASK_STATE (STOPPED);
  end STOP;

  -- Aborts the task, mandatory for the main program to exit.
  -- If the task is already aborted, exception is raised.
  procedure ABORT_TASK is
  begin
    -- Try to warn the task
    SET_TASK_STATE (ABORTED);
  end ABORT_TASK;


  -- Returns the current period of activation.
  -- If the task is already aborted, exception is raised.
  function GET_PERIOD return DURATION is
  begin
    if TASK_STATE /= ABORTED then
      return REAL_PERIOD;
    else
      raise TASK_ABORTED;
    end if;
  end GET_PERIOD;

  procedure SCHEDULE is
  begin
    case TASK_STATE is
      when STOPPED =>
        return;
      when RUNNING =>
        THE_TASK.SCHEDULE;
      when ABORTED =>
        return;
    end case;
  end SCHEDULE;

begin
  -- store the initial period
  REAL_PERIOD := CHECK_PERIOD(ACTIVATION_PERIOD, TRUE);
end TASK_MNG;
