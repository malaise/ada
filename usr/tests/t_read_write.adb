with Ada.Text_Io;
with Mutex_Manager, Schedule, Rnd, Normal, Argument, Basic_Proc;

procedure T_Read_Write is
  pragma Priority(10);

  Lock : access Mutex_Manager.Mutex;
  Io_Lock : Mutex_Manager.Simple_Mutex;

  subtype Range_Task is Positive range 1 .. 10;

  task type T is
   pragma Priority(10);
    entry Num (I : in Range_Task);
    entry Done;
  end T;

  procedure Put_Line (Index : in Range_Task; Msg : in String) is
  begin
    Io_Lock.Get;
    Ada.Text_Io.Put_Line (Normal (Index, 3) & " " & Msg);
    Io_Lock.Release;
  end Put_Line;

  function Image (D : Duration) return String is
    Str : constant String := D'Img;
  begin
    return Str (1 .. 4);
  end Image;

  task body T is
    Index : Range_Task;
    Dur : Duration;
    Kind : Mutex_Manager.Access_Kind;
    subtype Str5 is String (1 .. 5);
    Kind_Strs : constant array (Mutex_Manager.Access_Kind) of Str5 := (
      Mutex_Manager.Read  => " Read",
      Mutex_Manager.Write => "Write");
    Res : Boolean;
  begin
    -- Get name
    accept Num (I : in Range_Task) do
      Index := I;
    end Num;
    -- Work until termination requested in Critical
    loop
      Schedule;
      -- Wait from -0.1 to 0.5
      Dur := Duration (Rnd.Int_Random (-1, 5)) / 10.0;
      -- 10% chances to write
      if Rnd.Int_Random (0, 9) = 0 then
        Kind := Mutex_Manager.Write;
      else
        Kind := Mutex_Manager.Read;
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
        -- Wait a bit
        delay Duration (Rnd.Int_Random (1, 5)) / 10.0;

        -- Release lock
        Put_Line (Index, "release");
        Lock.Release;
      end if;

      -- 1% chances to terminate
      exit when Rnd.Int_Random (0, 99) = 0;
    end loop;
    Put_Line (Index, "terminate");
    -- Ready to end
    accept Done;
  end T;

  Tasks : array (Range_Task) of T;

  procedure Error (Msg : in String) is
  begin
    Basic_Proc.Put_Line_Error ("Error: " & Msg);
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
       & " rw | wr");
    Basic_Proc.Set_Error_Exit_Code;
  end Error;

begin
  if Argument.Get_Nbre_Arg > 1 then
    Error ("Too many arguments");
    return;
  end if;
  if Argument.Get_Nbre_Arg = 0
  or else Argument.Get_Parameter = "rw" then
    Lock := new Mutex_Manager.Mutex (Mutex_Manager.Read_Write, False);
  elsif Argument.Get_Parameter = "wr" then
    Lock := new Mutex_Manager.Mutex (Mutex_Manager.Write_Read, False);
  else
    Error ("unexpected argument " & Argument.Get_Parameter);
    return;
  end if;

  Rnd.Randomize;
  -- Give to each actor it's name
  for I in Range_Task loop
    Tasks(I).Num (I);
  end loop;

  -- Wait until termination of each actor
  for I in Range_Task loop
    Tasks(I).Done;
  end loop;

end T_Read_Write;

