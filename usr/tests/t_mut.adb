-- Test  a mutex of Mutexes (kind and and number of clients are provided as
--   arguments)
with Basic_Proc, Mutexes, Schedule, Argument, Upper_Char, Normal,
     Sys_Calls,  Key_Pressed;
procedure T_Mut is
  pragma Priority(10);

  Critical_Section_Duration : constant := 10.0;
  Stdin_Is_A_Tty : Boolean;
  Input_Error : exception;

  procedure Exec (Mut_Kind : Mutexes.Mutex_Kind;
                  Max_Task : in Positive) is
    Crit_Lock : Mutexes.Mutex (Mut_Kind, False);

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
      Put_Lock, Get_Lock, Prompt_Lock : Mutexes.Simple_Mutex;

      Tab : constant String (1..4) := (others => ' ');

      procedure New_Line_On_Tty is
      begin
        if Stdin_Is_A_Tty then
          Basic_Proc.New_Line_Output;
        end if;
      end New_Line_On_Tty;
      procedure Put_Char_On_Tty (C : in Character) is
      begin
        if Stdin_Is_A_Tty then
          Basic_Proc.Put_Output (C);
        end if;
      end Put_Char_On_Tty;

      procedure Prompt (I : in Range_Task; Set : in Boolean) is
        use type Mutexes.Mutex_Kind;
      begin
        Prompt_Lock.Get;
        if Set then
          -- Call by Get: store I for further calls by Put while in get
          Current_I := I;
        end if;

        if Stdin_Is_A_Tty then
          if In_Get then
            Basic_Proc.New_Line_Output;
            In_Get := False;
          end if;
          Basic_Proc.Put_Output ("Task: ");
          Basic_Proc.Put_Output (Normal (Current_I, 3));
          if Mut_Kind /= Mutexes.Simple then
            Basic_Proc.Put_Output (" : Read, Write, Terminate");
            Basic_Proc.Put_Output (" : Bloqued, Immediate, Wait (3s) ? ");
          else
            Basic_Proc.Put_Output (" : Bloqued, Immediate, Wait (3s), Terminate ? ");
          end if;
          Basic_Proc.Flush_Output;
        end if;
        Prompt_Lock.Release;
      end Prompt;

      function Get_Char return Character is
        C : Character;
        S : Sys_Calls.Get_Status_List;
        use type Sys_Calls.Get_Status_List;
      begin
        if Stdin_Is_A_Tty then
          C := Key_Pressed.Get_Key (True);
        else
          Sys_Calls.Get_Immediate (Sys_Calls.Stdin, S, C);
          if S = Sys_Calls.None then
            C := Key_Pressed.No_Key;
          elsif S = Sys_Calls.Closed or else S = Sys_Calls.Error then
            C := Key_Pressed.Error_Key;
          end if;
        end if;
        return C;
      end Get_Char;

      procedure Get_Str (K, A : out Character) is
        use type Mutexes.Mutex_Kind;
      begin
        if Mut_Kind = Mutexes.Simple then
          -- Simple mutex, one key Action
          K := 'W';
          A := Upper_Char (Get_Char);
          if A = Key_Pressed.Error_Key then
             raise Input_Error;
          end if;
        else
          -- RW or RW mutex, 'T' or 'R'/'W' then one key Action
          A := Upper_Char (Get_Char);
          if A = 'T' then
            -- Terminate
            K := 'W';
          else
            -- Read or Write
            K := A;
            -- Action
            A := Upper_Char (Key_Pressed.Get_Key (True));
            if A = Key_Pressed.Error_Key then
               raise Input_Error;
            end if;
          end if;
        end if;

        -- Check Kind is R or W
        if K = 'R' or else K = 'W' then
          if Mut_Kind /= Mutexes.Simple then
            -- Put kind if mutex is not simple
            Put_Char_On_Tty (K);
          end if;
        else
          -- Discard
          A := 'D';
          New_Line_On_Tty;
          return;
        end if;
        -- Check Action is T, B, I or W
        if A = 'T' or else A = 'B' or else A = 'I' or else A = 'W' then
          Put_Char_On_Tty (A);
        else
          -- Discard
          A := 'D';
        end if;
        New_Line_On_Tty;
      end Get_Str;

      procedure Get (I : in Range_Task; K, A : out Character)  is
        C : Character;
      begin
        Get_Lock.Get;
        if Stdin_Is_A_Tty then
          -- Skip any pending character on tty
          loop
            C := Key_Pressed.Get_Key;
            if C = Key_Pressed.Error_Key then
               raise Input_Error;
            end if;
            exit when C = Key_Pressed.No_Key;
          end loop;
        end if;

        -- Start get
        Prompt (I, True);
        In_Get := True;
        Get_Str (K, A);
        In_Get := False;
        Get_Lock.Release;
      exception
        when Input_Error =>
          Get_Lock.Release;
          raise;
      end Get;

      procedure Put (S : in String; I : in  Range_Task) is
      begin
        Put_Lock.Get;
        if In_Get then
          Basic_Proc.New_Line_Output;
          In_Get := False;
        end if;
        Basic_Proc.Put_Output (Tab & S & ' ');
        Basic_Proc.Put_Output (Normal (I, 3));
        Basic_Proc.New_Line_Output;
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
      Action : Mutexes.Access_Kind;
      Waiting : Duration;
      B : Boolean;
    begin
      -- Get kind, action
      Input.Get (Num, K, A);

      if K = 'R' then
        Action := Mutexes.Read;
      elsif K = 'W' then
        Action := Mutexes.Write;
      else
        -- Discard
        return True;
      end if;

      if Upper_Char (A) = 'T' then
        -- Terminate
        return False;
      elsif Upper_Char (A) = 'B' then
        Waiting := -1.0;
      elsif Upper_Char (A) = 'I' then
        Waiting := 0.0;
      elsif Upper_Char (A) = 'W' then
        Waiting := 3.0;
      else
        -- Discard
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
      Input.Put ("Termination of", Index);
      accept Done;
    end T;

  begin -- Exec
    -- Give to each actor it's name
    for I in Range_Task loop
      Ta(I).Num (I);
      delay 0.1;
    end loop;

    -- Wait until termination of each actor
    for I in Range_Task loop
      Ta(I).Done;
    end loop;

    Basic_Proc.New_Line_Output;
    Basic_Proc.Put_Line_Output ("Done.");
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
  M_Kind : Mutexes.Mutex_Kind;
  use type Mutexes.Mutex_Kind, Sys_Calls.File_Desc_Kind_List;

begin -- T_Mut
  N_Args := Argument.Get_Nbre_Arg;

  if N_Args < 1 then
    Error ("Argument <mutex_kind> expected.");
    return;
  elsif Argument.Get_Parameter (1) = "s" then
    M_Kind := Mutexes.Simple;
  elsif Argument.Get_Parameter (1) = "rw" then
    M_Kind := Mutexes.Read_Write;
  elsif Argument.Get_Parameter (1) = "wr" then
    M_Kind := Mutexes.Write_Read;
  else
    Error ("Invalid argument " & Argument.Get_Parameter (Occurence => 1));
    return;
  end if;

  Stdin_Is_A_Tty := Sys_Calls.File_Desc_Kind (Sys_Calls.Stdin) = Sys_Calls.Tty;
  if Stdin_Is_A_Tty then
    Key_Pressed.Open;
  end if;

  if N_Args = 1 then
    -- Default Nb of tasks
    if M_Kind = Mutexes.Simple then
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

  if Stdin_Is_A_Tty then
    Key_Pressed.Close;
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

