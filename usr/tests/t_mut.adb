with Ada.Text_Io;
with My_Io, Mutex_Manager, Schedule, Argument, Sys_Calls, Upper_Char;
use Mutex_Manager;

procedure T_Mut is
  pragma Priority(10);

  Critical_Section_Duration : constant := 10.0;

  procedure Exec (Mut_Kind : Mutex_Manager.Mutex_Kind;
                  Max_Task : in Positive) is
    Crit_Lock : Mutex (Mut_Kind);

    subtype Range_Task is Positive range 1 .. Max_Task;

    task type T is
     pragma Priority(10);
      entry Num (I : in Range_Task);
      entry Done;
    end T;

    Ta : array (Range_Task) of T;

    package Input is
      procedure Get (I : in Range_Task; K, A : out Character);
      procedure Put (S : in String; I : in  Range_Task);
    end Input;

    package body Input is
      In_Get : Boolean := False;
      Current_I : Range_Task;
      Put_Lock, Get_Lock, Prompt_Lock : Mutex;

      Tab : constant String (1..4) := (others => ' ');

      procedure Prompt (I : in Range_Task; Set : in Boolean) is
        B : Boolean;
        use type Mutex_Manager.Mutex_Kind;
      begin
        B := Get_Mutex (Prompt_Lock, -1.0);
        if Set then
          -- Call by Get: store I for further calls by Put while in get
          Current_I := I;
        end if;

        Ada.Text_Io.Put ("Task: ");
        My_Io.Put (Current_I, 3);
        if Mut_Kind /= Mutex_Manager.Simple then
          Ada.Text_Io.Put (" : Read, Write");
        end if;
        Ada.Text_Io.Put (" : Bloqued, Immediate, Wait (5s), Terminate ? ");
        Ada.Text_Io.Flush;
        Release_Mutex (Prompt_Lock);
      end Prompt;

      procedure Get (I : in Range_Task; K, A : out Character)  is
        B : Boolean;
        C : Character;
      begin
        B := Get_Mutex (Get_Lock, -1.0);
        loop
          Ada.Text_Io.Get_Immediate (C, B);
          exit when not B;
        end loop;
        In_Get := True;
        Prompt (I, True);
        if Mut_Kind /= Mutex_Manager.Simple then
          Ada.Text_Io.Get (K);
          if Upper_Char (K) = 'T' then
            A := K;
            K := 'W';
          else
            Ada.Text_Io.Get (A);
          end if;
        else
          K := 'W';
          Ada.Text_Io.Get (A);
        end if;
        In_Get := False;
        Release_Mutex (Get_Lock);
      end Get;

      procedure Put (S : in String; I : in  Range_Task) is
        B : Boolean;
      begin
        B := Get_Mutex (Put_Lock, -1.0);
        if In_Get then
          Ada.Text_Io.New_Line;
        end if;
        Ada.Text_Io.Put (Tab & S & ' ');
        My_Io.Put (I, 3);
        Ada.Text_Io.New_Line;
        if In_Get then
          Prompt (1, False);
        end if;
        Release_Mutex (Put_Lock);
      end Put;
    end Input;

    -- Get action
    -- return False if termination
    -- get mutex then release and return True otherwise
    function Critical (Num : in Range_Task) return Boolean is
      K, A : Character;
      Action : Mutex_Manager.Access_Kind;
      Waiting : Duration;
      Tab : constant String := "    ";
      B : Boolean;
    begin
      -- Get kind, action
      Input.Get (Num, K, A);

      if Upper_Char (K) = 'R' then
        Action := Mutex_Manager.Read;
      elsif Upper_Char (K) = 'W' then
        Action := Mutex_Manager.Write;
      else
        return True;
      end if;

      if Upper_Char (A) = 'T' then
        return False;
      elsif Upper_Char (A) = 'B' then
        Waiting := -1.0;
      elsif Upper_Char (A) = 'I' then
        Waiting := 0.0;
      elsif Upper_Char (A) = 'W' then
        Waiting := 3.0;
      else
        return True;
      end if;

      B := Get_Mutex (Crit_Lock, Waiting, Action);

      if B then
        Input.Put ("Start of critical section for", Num);
        delay Critical_Section_Duration;
        Input.Put ("End   of critical section for", Num);
        Release_Mutex(Crit_Lock);
      else
        Input.Put ("Mutex not free for", Num);
      end if;
      return True;
    end Critical;

    task body T is
      Index : Range_Task;
    begin
      -- get name
      accept Num (I : in Range_Task) do
        Index := I;
      end Num;
      -- work until termination requested in Critical
      loop
        Schedule;
        exit when not Critical (Index);
      end loop;
      -- Ready to end
      accept Done;
    end T;


  begin -- Exec
    Ada.Text_Io.New_Line(2);
    -- give to each actor it's name
    for I in Range_Task loop
      Ta(I).Num (I);
    end loop;

    -- wait until termination of each actor
    for I in Range_Task loop
      Ta(I).Done;
    end loop;

    Ada.Text_Io.New_Line;
    Ada.Text_Io.Put_Line ("Done.");
    Ada.Text_Io.New_Line;
  end Exec;

  procedure Error (S : in String) is
  begin
    Sys_Calls.Put_Line_Error (S & ".");
    Sys_Calls.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
                  & " <mutex_kind> [ <nb_tasks> ]");
    Sys_Calls.Put_Line_Error ("  <mutex_kind> ::= s | rw | wr");
    Sys_Calls.Set_Error_Exit_Code;
  end Error;

  -- Local to main
  N_Args : Natural;
  N_Tasks : Positive;
  M_Kind : Mutex_Manager.Mutex_Kind;
  use type Mutex_Manager.Mutex_Kind;

begin -- T_Mut
  N_Args := Argument.Get_Nbre_Arg;

  if N_Args < 1 then
    Error ("Argument <mutex_kind> expected.");
  elsif Argument.Get_Parameter (1) = "s" then
    M_Kind := Mutex_Manager.Simple;
  elsif Argument.Get_Parameter (1) = "rw" then
    M_Kind := Mutex_Manager.Read_Write;
  elsif Argument.Get_Parameter (1) = "wr" then
    M_Kind := Mutex_Manager.Write_Read;
  else
    Error ("Invalid argument " & Argument.Get_Parameter (Occurence => 1));
    return;
  end if;

  if N_Args = 1 then
    -- Default Nb of tasks
    if M_Kind = Mutex_Manager.Simple then
      -- 2 tasks on simple mutex
      Exec(M_Kind, 2);
    else
      -- 3 tasks on read/write mutex
      Exec(M_Kind, 3);
    end if;
  elsif N_Args = 2 then
    N_Tasks := Positive'Value (Argument.Get_Parameter (2));
    Exec(M_Kind, N_Tasks);
  else
    Error ("Too many arguments");
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

