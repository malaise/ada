-- Test Read-Write mutex with 10 clients, detect deadlock (inactivity)
with Ada.Calendar, Ada.Task_Identification;
with Mutexes, Schedule, Rnd, Normal, Argument, Basic_Proc, Protected_Put,
     Protected_Var, Images;
procedure T_Read_Write is
  pragma Priority(10);

  -- The number of tasks
  subtype Range_Actor is Natural range 0 .. 10;
  Main_Index : constant Range_Actor := 0;
  subtype Range_Task is Positive range 1 .. Range_Actor'Last;

  -- Date of last output of a task
  package Protected_Time is new Protected_Var (Ada.Calendar.Time);
  Last_Time : Protected_Time.Protected_T;
  use type Ada.Calendar.Time;
  -- Delay of inactivity
  Inactivity : constant Duration := 5.0;

  -- Put a task activity
  procedure Put_Line (Index : in Range_Actor; Msg : in String;
                      Output : in Boolean := True) is
  begin
    Last_Time.Set (Ada.Calendar.Clock);
    declare
      Date : constant String := Images.Date_Image (Last_Time.Get);
      Str : constant String
          := (if Index in Range_Task then Normal (Index, 2) else "Main")
           & " " & Msg;
    begin
      if Output then
        Protected_Put.Put_Line_Output (Date & " " & Str);
      else
        Protected_Put.Put_Line_Error (Date & " Error: " & Str);
      end if;
    end;
  end Put_Line;
  type Mustex_Access is access Mutexes.Mutex;
  Lock : Mustex_Access;

  -- The client tasks
  task type T is
   pragma Priority(10);
    entry Num (I : in Range_Task);
    entry Done;
  end T;

  function Image (D : Duration) return String is
    Str : constant String := D'Img;
  begin
    return Str (1 .. 4);
  end Image;

  task body T is
    Index : Range_Task;
    Dur : Duration;
    Kind : Mutexes.Access_Kind;
    subtype Str5 is String (1 .. 5);
    Kind_Strs : constant array (Mutexes.Access_Kind) of Str5 := (
      Mutexes.Read  => " Read",
      Mutexes.Write => "Write");
    Res : Boolean;
  begin
    -- Get name
    accept Num (I : in Range_Task) do
      Index := I;
    end Num;
    Put_Line (Index ,
      "Task started, id "
    & Ada.Task_Identification.Image (Ada.Task_Identification.Current_Task));
    -- Work until termination requested in Critical
    loop
      Schedule;
      -- Wait from -0.1 to 0.5
      Dur := Duration (Rnd.Gen.Int_Random (-1, 5)) / 10.0;
      -- 10% chances to write
      if Rnd.Gen.Int_Random (0, 9) = 0 then
        Kind := Mutexes.Write;
      else
        Kind := Mutexes.Read;
      end if;
      -- Get lock
      Put_Line (Index, "get " & Kind_Strs(Kind) & " " & Image(Dur));
      Res := Lock.Get (Dur, Kind);
      -- Trace result
      if Res then
        Put_Line (Index, "OK");
      else
        Put_Line (Index, "NOK");
      end if;

      if Res then
        -- Got it: Work (wait) from 0.1 to 0.5
        Put_Line (Index, "waiting");
        delay Duration (Rnd.Gen.Int_Random (1, 5)) / 10.0;
        Put_Line (Index, "waited");

        -- Release lock
        Put_Line (Index, "release");
        Lock.Release;
      end if;

      -- 10% chances to terminate
      exit when Rnd.Gen.Int_Random (0, 9) = 0;
    end loop;
    Put_Line (Index, "terminate");
    -- Ready to end
    accept Done;
  end T;

  Tasks : array (Range_Task) of T;
  Runs  : array (Range_Task) of Boolean;
  Nb_Run : Natural;

  -- Log an (init) error
  procedure Error (Msg : in String) is
  begin
    Put_Line (Main_Index, Msg, Output => False);
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
       & " rw | wr");
    Basic_Proc.Set_Error_Exit_Code;
  end Error;

-- The main: activate and monitor the tasks
begin
  if Argument.Get_Nbre_Arg > 1 then
    Error ("Too many arguments");
    return;
  end if;
  if Argument.Get_Nbre_Arg = 0
  or else Argument.Get_Parameter = "rw" then
    Lock := new Mutexes.Mutex (Mutexes.Read_Write, False);
  elsif Argument.Get_Parameter = "wr" then
    Lock := new Mutexes.Mutex (Mutexes.Write_Read, False);
  else
    Error ("Unexpected argument " & Argument.Get_Parameter);
    return;
  end if;

  Rnd.Gen.Randomize;

  -- Give to each actor it's name
  Put_Line (Main_Index, "Starting");
  Nb_Run := 0;
  for I in Range_Task loop
    Tasks(I).Num (I);
    Runs(I) := True;
    Nb_Run := Nb_Run + 1;
  end loop;

  -- Wait until termination of all tasks
  Main: while Nb_Run /= 0 loop

    -- Try to terminate tasks: wait 1s altogether
    for I in Range_Task loop
      if Runs(I) then
        select
          Tasks(I).Done;
          Runs(I) := False;
          Nb_Run := Nb_Run - 1;
        or
          delay 1.0 / Duration(Nb_Run);
        end select;
      end if;
    end loop;

    -- Monitor activity
    if Ada.Calendar.Clock - Last_Time.Get > Inactivity then
      -- Deadlock detected => abort tasks
      Put_Line (Main_Index, "Deadlock detected, aborting");
      Basic_Proc.Set_Error_Exit_Code;
      for I in Range_Task loop
        if Runs(I) then
          abort Tasks(I);
          -- This will exit Main
          Nb_Run := Nb_Run - 1;
        end if;
      end loop;
    end if;
  end loop Main;

  Put_Line (Main_Index, "Done");

end T_Read_Write;

