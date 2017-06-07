with Ada.Calendar, Ada.Task_Identification;
with Mutexes, Schedule, Rnd, Normal, Argument, Basic_Proc, Protected_Put,
     Images;
-- Test Read-Write mutex
procedure T_Read_Write is
  pragma Priority(10);

  Lock : access Mutexes.Mutex;

  subtype Range_Task is Positive range 1 .. 10;

  task type T is
   pragma Priority(10);
    entry Num (I : in Range_Task);
    entry Done;
  end T;

  procedure Put_Line (Index : in Range_Task; Msg : in String) is
  begin
    Protected_Put.Put_Line_Output (
        Images.Date_Image (Ada.Calendar.Clock, Images.Iso_Dot)
      & " " & Normal (Index, 2)
      & " " & Msg);
  end Put_Line;

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
        -- Wait a bit
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
    Lock := new Mutexes.Mutex (Mutexes.Read_Write, False);
  elsif Argument.Get_Parameter = "wr" then
    Lock := new Mutexes.Mutex (Mutexes.Write_Read, False);
  else
    Error ("Unexpected argument " & Argument.Get_Parameter);
    return;
  end if;

  Rnd.Gen.Randomize;

  -- Give to each actor it's name
  Protected_Put.Put_Line_Output ("Starting");
  for I in Range_Task loop
    Tasks(I).Num (I);
  end loop;

  -- Wait until termination of each actor
  for I in Range_Task loop
    Tasks(I).Done;
  end loop;
  Protected_Put.Put_Line_Output ("Done");

end T_Read_Write;

