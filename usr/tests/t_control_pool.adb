with Ada.Calendar;
with Rnd, Event_Mng, Control_Pool, Sys_Calls;
procedure T_Control_Pool is

  subtype Pool_Range is Positive range 1 .. 5;
  package Pool is new Control_Pool (Pool_Range);

  subtype Client_Range is Positive range 1 .. 9;
  task type Client_Task is
    entry Start (Client_No : in Client_Range; Client_Pid : out Sys_Calls.Pid);
  end Client_Task;

  Done : Boolean := False;
  procedure Term_Cb is
  begin
    Done := True;
  end Term_Cb;

  task body Client_Task is
    My_No : Client_Range;
    Pool_No : Pool_Range;
    Got : Boolean;
    T1 : Ada.Calendar.Time;
    Dur1, Dur2 : Duration;
    use type Ada.Calendar.Time;
  begin
    accept Start (Client_No : in Client_Range;
                  Client_Pid : out Sys_Calls.Pid) do
      My_No := Client_No;
      Client_Pid := Sys_Calls.Get_Pid;
    end Start;
    loop
      -- Get a random pool resource
      Pool_No := Rnd.Gen.Int_Random(Pool_Range'First, Pool_Range'Last);
      T1 := Ada.Calendar.Clock;
      Dur1 := Rnd.Gen.Dur_Random (-0.1, 3.0);
      Sys_Calls.Put_Line_Output (My_No'Img & ": getting " & Pool_No'Img & " for " & Dur1'Img);
      Got := Pool.Get(Pool_No, Dur1);
      Dur2 := Ada.Calendar.Clock - T1;
      Sys_Calls.Put_Line_Output (My_No'Img & ": got " & Pool_No'Img & " " & Got'Img
           & " after " & Dur2'Img & " for " & Dur1'Img);
      if Got then
        -- Wait a bit (1 to 5 s) or signal
        Sys_Calls.Put_Line_Output (My_No'Img & ": working on " & Pool_No'Img);
        exit when Event_Mng.Wait(Rnd.Gen.Int_Random(1_000, 5_000));
        Sys_Calls.Put_Line_Output (My_No'Img & ": releasing " & Pool_No'Img);
        Pool.Release (Pool_No);
      end if;
      exit when Done;
    end loop;
    Sys_Calls.Put_Line_Output (My_No'Img & ": exit");
  end Client_Task;

  Clients : array (Client_Range) of Client_Task;
  Pids : array (Client_Range) of Sys_Calls.Pid;

begin
  Rnd.Gen.Randomize;
  Event_Mng. Set_Sig_Term_Callback (Term_Cb'Unrestricted_Access);

  for I in Client_Range loop
    Clients(I).Start(I, Pids(I));
  end loop;

  loop
    exit when Event_Mng.Wait (1_000) and then Done;
  end loop;

end T_Control_Pool;

