with Ada.Text_Io, Ada.Calendar;
with Rnd, Event_Mng, Control_Pool, Sys_Calls;
procedure T_Control_Pool is

  subtype Pool_Range is Positive range 1 .. 5;
  package Pool is new Control_Pool (Pool_Range);

  subtype Client_Range is Positive range 1 .. 9;
  task type Client_Task is
    entry Start (Client_No : in Client_Range; Client_Pid : out Sys_Calls.Pid);
  end Client_Task;

  task body Client_Task is
    My_No : Client_Range;
    Pool_No : Pool_Range;
    Got : Boolean;
    T1 : Ada.Calendar.Time;
    Dur1, Dur2 : Duration;
    use type Ada.Calendar.Time;
  begin
    accept Start (Client_No : in Client_Range; Client_Pid : out Sys_Calls.Pid) do
      My_No := Client_No;
      Client_Pid := Sys_Calls.Get_Pid;
    end Start;
    loop
      -- Get a random pool resource
      Pool_No := Rnd.Int_Random(Pool_Range'First, Pool_Range'Last);
      T1 := Ada.Calendar.Clock;
      Dur1 := Rnd.Dur_Random (1.0, 3.0);
      Ada.Text_Io.Put_Line (My_No'Img & ": getting " & Pool_No'Img & " for " & Dur1'Img);
      Got := Pool.Get(Pool_No, Dur1);
      Dur2 := Ada.Calendar.Clock - T1;
      Ada.Text_Io.Put_Line (My_No'Img & ": got " & Pool_No'Img & " " & Got'Img
           & " after " & Dur2'Img & " for " & Dur1'Img);
      if Got then
        -- Wait a bit (1 to 5 s) or signal
        Ada.Text_Io.Put_Line (My_No'Img & ": working on " & Pool_No'Img);
        exit when Event_Mng.Wait(Rnd.Int_Random(1_000, 5_000));
        Ada.Text_Io.Put_Line (My_No'Img & ": releasing " & Pool_No'Img);
        Pool.Release (Pool_No);
      end if;
    end loop;
    Ada.Text_Io.Put_Line (My_No'Img & ": exit");
  end Client_Task;

  Clients : array (Client_Range) of Client_Task;
  Pids : array (Client_Range) of Sys_Calls.Pid;

begin
  Rnd.Randomize;

  for I in Client_Range loop
    Clients(I).Start(I, Pids(I));
  end loop;

  Event_Mng.Pause (-1);

  Ada.Text_Io.Put_Line ("Aborting");
  for I in Client_Range loop
    abort Clients(I);
  end loop;

end T_Control_Pool;

