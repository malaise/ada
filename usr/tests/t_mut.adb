with My_Io, Mutex_Manager, Schedule, Argument;
use Mutex_Manager;

procedure T_Mut is
  pragma Priority(10);

  N_Args : Natural;
  N_Tasks : Positive;
  Critical_Section_Duration : constant := 10.0;

  procedure Exec (Max_Task : in Positive) is
    -- Max seems to be 4173 on DEC5000 and 2583 on DEC3100
    Crit_Lock : Mutex;
   
    subtype Range_Task is Positive range 1 .. Max_Task;
   
    task type T is
     pragma Priority(10);
      entry Num(I : in Range_Task);
      entry Done;
    end T;
   
    Ta : array (Range_Task) of T;
   
    package Input is
      procedure Get(I : in Range_Task; C : out Character);
      procedure Put(S : in String; I : in  Range_Task);
    end Input;
   
    package body Input is
      In_Get : Boolean := False;
      Current_I : Range_Task;
      Io_Lock : Mutex;

      Tab : constant String (1..4) := (others => ' ');

      procedure Prompt(I : in Range_Task; Set : in Boolean) is
      begin
        if Set then 
          -- call by GET
          Current_I := I;
        else
          -- call by PUT
          if not In_Get then return; end if;
        end if;

        My_Io.Put("Task: ");
        My_Io.Put(Current_I, 3);
        My_Io. Put(" : Bloqued, Immediate, Wait (5s), Terminate ? ");
      end Prompt;

      procedure Get(I : in Range_Task; C : out Character)  is
        B : Boolean;
      begin
        B := Get_Mutex(Io_Lock, -1.0);
        In_Get := True;
        Prompt (I, True);
        My_Io.Get(C);
        In_Get := False;
        Release_Mutex(Io_Lock);
      end Get;

      procedure Put(S : in String; I : in  Range_Task) is
      begin
        if In_Get then
          My_Io.New_Line;
        end if;
        My_Io.Put (Tab & S & ' ');
        My_Io.Put(I, 3);
        My_Io.New_Line;
        Prompt (1, False);
      end Put;
    end Input;

    -- Get action
    -- return FALSE if termination
    -- get mutex then release and return TRUE otherwise
    function Critical(Num : in Range_Task) return Boolean is
      C : Character;
      Tab : constant String := "    ";
      B : Boolean;
    begin
      Input.Get(Num, C);
   
      if (C = 't') or  (C = 'T') then
        return False;
      elsif (C = 'b') or (C = 'B') then
        B := Get_Mutex(Crit_Lock, -1.0);
      elsif (C = 'i') or (C = 'I') then
        B := Get_Mutex(Crit_Lock, 0.0);
      elsif (C = 'w') or (C = 'W') then
        B := Get_Mutex(Crit_Lock, 3.0);
      else
        return True;
      end if;
      if B then
        Input.Put("Start of critical section for", Num);
        delay Critical_Section_Duration;
        Input.Put("End   of critical section for", Num);
        Release_Mutex(Crit_Lock);
      else
        Input.Put("Mutex not free for", Num);
      end if;
      return True;
    end Critical;
   
    task body T is
      Index : Range_Task;
    begin
      -- get name
      accept Num(I : in Range_Task) do
        Index := I;
      end Num;
      -- work until termination requested in CRITIQUE
      loop
        Schedule;
        exit when not Critical(Index);
      end loop;
      -- Ready to end
      accept Done;
    end T;
   
   
   
  begin -- EXEC
    My_Io.New_Line(2);
    -- give to each actor it's name
    for I in Range_Task loop
      Ta(I).Num(I);
    end loop;

    -- wait until termination of each actor
    for I in Range_Task loop
      Ta(I).Done;
    end loop;

    My_Io.New_Line;
    My_Io.Put_Line ("Done.");
    My_Io.New_Line;
  end Exec;

  procedure Error (S : in String) is
  begin
    My_Io.Put_Line (S & ". Syntaxe: t_mut [nbre_tasks]");
  end Error;

begin -- T_MUT
  N_Args := Argument.Get_Nbre_Arg;

  if N_Args > 1 then
    Error ("Syntax error");
  elsif N_Args = 1 then
    N_Tasks := Positive'Value (Argument.Get_Parameter(1));
    Exec(N_Tasks);
  else
    Exec(2);
  end if;

exception
  when Constraint_Error | Numeric_Error =>
    Error ("Wrong argument");
  when Storage_Error =>
    Error ("Argument too big");
  when others =>
    Error ("Internal error");
    raise;
end T_Mut;
