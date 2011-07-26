with Ada.Text_Io;
with My_Io, Mutex_Manager, Schedule, Argument, Basic_Proc, Upper_Char;

procedure T_Mut is
  pragma Priority(10);

  Critical_Section_Duration : constant := 10.0;

  procedure Exec (Mut_Kind : Mutex_Manager.Mutex_Kind;
                  Max_Task : in Positive) is
    Crit_Lock : Mutex_Manager.Mutex (Mut_Kind, False);

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
      Put_Lock, Get_Lock, Prompt_Lock : Mutex_Manager.Simple_Mutex;

      Tab : constant String (1..4) := (others => ' ');

      procedure Prompt (I : in Range_Task; Set : in Boolean) is
        use type Mutex_Manager.Mutex_Kind;
      begin
        Prompt_Lock.Get;
        if Set then
          -- Call by Get: store I for further calls by Put while in get
          Current_I := I;
        end if;

        Ada.Text_Io.Put ("Task: ");
        My_Io.Put (Current_I, 3);
        if Mut_Kind /= Mutex_Manager.Simple then
          Ada.Text_Io.Put (" : Read, Write, Terminate");
          Ada.Text_Io.Put (" : Bloqued, Immediate, Wait (3s) ? ");
        else
          Ada.Text_Io.Put (" : Bloqued, Immediate, Wait (3s), Terminate ? ");
        end if;
        Ada.Text_Io.Flush;
        Prompt_Lock.Release;
      end Prompt;

      procedure Get (I : in Range_Task; K, A : out Character)  is
        B : Boolean;
        C : Character;
        S : String (1 .. 256);
        L : Natural;
        use type Mutex_Manager.Mutex_Kind;
      begin
        Get_Lock.Get;
        -- Skip any pending character
        loop
          Ada.Text_Io.Get_Immediate (C, B);
          exit when not B;
        end loop;
        -- Start get
        In_Get := True;
        loop
          Prompt (I, True);
          Ada.Text_Io.Get_Line (S, L);
          if Mut_Kind /= Mutex_Manager.Simple then
            if L = 1 then
              K := Upper_Char (S(1));
              if K = 'T' then
                A := K;
                K := 'W';
                exit;
              end if;
            elsif L = 2 then
              K := Upper_Char (S(1));
              A := Upper_Char (S(2));
              exit;
            end if;
          elsif L = 1 then
            -- Simple mutex
            K := 'W';
            A := Upper_Char (S(1));
            exit;
          end if;
        end loop;
        In_Get := False;
        Get_Lock.Release;
      end Get;

      procedure Put (S : in String; I : in  Range_Task) is
      begin
        Put_Lock.Get;
        if In_Get then
          Ada.Text_Io.New_Line;
        end if;
        Ada.Text_Io.Put (Tab & S & ' ');
        My_Io.Put (I, 3);
        Ada.Text_Io.New_Line;
        if In_Get then
          Prompt (1, False);
        end if;
        Put_Lock.Release;
      end Put;
    end Input;

    -- Get action
    -- return False if termination
    -- get mutex then release and return True otherwise
    function Critical (Num : in Range_Task) return Boolean is
      K, A : Character;
      Action : Mutex_Manager.Access_Kind;
      Waiting : Duration;
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

      B := Crit_Lock.Get (Waiting, Action);

      if B then
        Input.Put ("Start of critical section for", Num);
        delay Critical_Section_Duration;
        Input.Put ("End   of critical section for", Num);
        Crit_Lock.Release;
      else
        Input.Put ("Mutex not free for", Num);
      end if;
      return True;
    end Critical;

    task body T is
      Index : Range_Task;
    begin
      -- Get name
      accept Num (I : in Range_Task) do
        Index := I;
      end Num;
      -- Work until termination requested in Critical
      loop
        Schedule;
        pragma Warnings (Off, "variable ""*"" is not modified in loop body");
        exit when not Critical (Index);
        pragma Warnings (On,  "variable ""*"" is not modified in loop body");
      end loop;
      -- Ready to end
      accept Done;
    end T;


  begin -- Exec
    Ada.Text_Io.New_Line(2);
    -- Give to each actor it's name
    for I in Range_Task loop
      Ta(I).Num (I);
    end loop;

    -- Wait until termination of each actor
    for I in Range_Task loop
      Ta(I).Done;
    end loop;

    Ada.Text_Io.New_Line;
    Ada.Text_Io.Put_Line ("Done.");
    Ada.Text_Io.New_Line;
  end Exec;

  procedure Error (S : in String) is
  begin
    Basic_Proc.Put_Line_Error (S & ".");
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
                  & " <mutex_kind> [ <nb_tasks> ]");
    Basic_Proc.Put_Line_Error ("  <mutex_kind> ::= s | rw | wr");
    Basic_Proc.Set_Error_Exit_Code;
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
    return;
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
    Error ("Too few or too many arguments");
    return;
  end if;

exception
  when Constraint_Error =>
    Error ("Wrong argument");
  when Storage_Error =>
    Error ("Argument too big");
  when others =>
    Error ("Internal error");
    raise;
end T_Mut;

