with MY_IO, MUTEX_MANAGER, SCHEDULE, ARGUMENT;
use MUTEX_MANAGER;

procedure T_MUT is
  pragma PRIORITY(10);

  N_ARGS : NATURAL;
  N_TASKS : POSITIVE;
  CRITICAL_SECTION_DURATION : constant := 10.0;

  procedure EXEC (MAX_TASK : in POSITIVE) is
    -- Max seems to be 4173 on DEC5000 and 2583 on DEC3100
    CRIT_LOCK : MUTEX;
   
    subtype RANGE_TASK is POSITIVE range 1 .. MAX_TASK;
   
    task type T is
     pragma PRIORITY(10);
      entry NUM(I : in RANGE_TASK);
      entry DONE;
    end T;
   
    TA : array (RANGE_TASK) of T;
   
    package INPUT is
      procedure GET(I : in RANGE_TASK; C : out CHARACTER);
      procedure PUT(S : in STRING; I : in  RANGE_TASK);
    end INPUT;
   
    package body INPUT is
      IN_GET : BOOLEAN := FALSE;
      CURRENT_I : RANGE_TASK;
      IO_LOCK : MUTEX;

      TAB : constant STRING (1..4) := (others => ' ');

      procedure PROMPT(I : in RANGE_TASK; SET : in BOOLEAN) is
      begin
        if SET then 
          -- call by GET
          CURRENT_I := I;
        else
          -- call by PUT
          if not IN_GET then return; end if;
        end if;

        MY_IO.PUT("Task: ");
        MY_IO.PUT(CURRENT_I, 3);
        MY_IO. PUT(" : Bloqued, Immediate, Wait (5s), Terminate ? ");
      end PROMPT;

      procedure GET(I : in RANGE_TASK; C : out CHARACTER)  is
        B : BOOLEAN;
      begin
        B := GET_MUTEX(IO_LOCK, -1.0);
        IN_GET := TRUE;
        PROMPT (I, TRUE);
        MY_IO.GET(C);
        IN_GET := FALSE;
        RELEASE_MUTEX(IO_LOCK);
      end GET;

      procedure PUT(S : in STRING; I : in  RANGE_TASK) is
      begin
        if IN_GET then
          MY_IO.NEW_LINE;
        end if;
        MY_IO.PUT (TAB & S & ' ');
        MY_IO.PUT(I, 3);
        MY_IO.NEW_LINE;
        PROMPT (1, FALSE);
      end PUT;
    end INPUT;

    -- Get action
    -- return FALSE if termination
    -- get mutex then release and return TRUE otherwise
    function CRITICAL(NUM : in RANGE_TASK) return BOOLEAN is
      C : CHARACTER;
      TAB : constant STRING := "    ";
      B : BOOLEAN;
    begin
      INPUT.GET(NUM, C);
   
      if (C = 't') or  (C = 'T') then
        return FALSE;
      elsif (C = 'b') or (C = 'B') then
        B := GET_MUTEX(CRIT_LOCK, -1.0);
      elsif (C = 'i') or (C = 'I') then
        B := GET_MUTEX(CRIT_LOCK, 0.0);
      elsif (C = 'w') or (C = 'W') then
        B := GET_MUTEX(CRIT_LOCK, 3.0);
      else
        return TRUE;
      end if;
      if B then
        INPUT.PUT("Start of critical section for", NUM);
        delay CRITICAL_SECTION_DURATION;
        INPUT.PUT("End   of critical section for", NUM);
        RELEASE_MUTEX(CRIT_LOCK);
      else
        INPUT.PUT("Mutex not free for", NUM);
      end if;
      return TRUE;
    end CRITICAL;
   
    task body T is
      INDEX : RANGE_TASK;
    begin
      -- get name
      accept NUM(I : in RANGE_TASK) do
        INDEX := I;
      end NUM;
      -- work until termination requested in CRITIQUE
      loop
        SCHEDULE;
        exit when not CRITICAL(INDEX);
      end loop;
      -- Ready to end
      accept DONE;
    end T;
   
   
   
  begin -- EXEC
    MY_IO.NEW_LINE(2);
    -- give to each actor it's name
    for I in RANGE_TASK loop
      TA(I).NUM(I);
    end loop;

    -- wait until termination of each actor
    for I in RANGE_TASK loop
      TA(I).DONE;
    end loop;

    MY_IO.NEW_LINE;
    MY_IO.PUT_LINE ("Done.");
    MY_IO.NEW_LINE;
  end EXEC;

  procedure ERROR (S : in STRING) is
  begin
    MY_IO.PUT_LINE (S & ". Syntaxe: t_mut [nbre_tasks]");
  end ERROR;

begin -- T_MUT
  N_ARGS := ARGUMENT.GET_NBRE_ARG;

  if N_ARGS > 1 then
    ERROR ("Syntax error");
  elsif N_ARGS = 1 then
    N_TASKS := POSITIVE'VALUE (ARGUMENT.GET_PARAMETER(1));
    EXEC(N_TASKS);
  else
    EXEC(2);
  end if;

exception
  when CONSTRAINT_ERROR | NUMERIC_ERROR =>
    ERROR ("Wrong argument");
  when STORAGE_ERROR =>
    ERROR ("Argument too big");
  when others =>
    ERROR ("Internal error");
    raise;
end T_MUT;
