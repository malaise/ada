with Ada.Calendar;
with Rnd, Event_Mng, Control_Pool, Images, Protected_Put;
procedure T_Control_Pool is

  -- The pool to control resources
  subtype Pool_Range is Positive range 1 .. 5;
  package Control_Pool_Mng is new Control_Pool (Pool_Range);
  Pool : Control_Pool_Mng.Controlled_Pool_Type;

  -- Protected logger
  procedure Log (Msg : in String) is
  begin
    Protected_Put.Put_Line_Output (Msg);
  end Log;
  -- Detect Ctrl-C and signal to tasks the end of program
  Main_Done : Boolean := False;
  procedure Term_Cb is
  begin
    Log ("Term Cb");
    Main_Done := True;
  end Term_Cb;

  -- (Rounded) image of a duration
  function Dur_Image (Dur : in Duration) return String is
    -- 3 digits after dot and no Plus sign
    (if Dur >= 0.0 then Images.Dur_Image (Dur, 3, False) & "s" else "Infinite");

  -- The clients of resources
  subtype Client_Range is Positive range 1 .. 9;
  task type Client_Task is
    entry Start (Client_No : in Client_Range);
    entry Stop;
    entry Stopped;
  end Client_Task;

  task body Client_Task is
    My_No : Client_Range;
    Pool_No : Pool_Range;
    Got : Boolean;
    T1 : Ada.Calendar.Time;
    Dur1, Dur2, Dur3 : Duration;
    Client_Stop : Boolean := False;

    use type Ada.Calendar.Time;
  begin
    accept Start (Client_No : in Client_Range) do
      My_No := Client_No;
      Log (My_No'Img & ": started");
    end Start;
    loop
      -- Get a random pool resource
      Pool_No := Rnd.Gen.Int_Random(Pool_Range'First, Pool_Range'Last);
      T1 := Ada.Calendar.Clock;
      Dur1 := Rnd.Gen.Dur_Random (-0.1, 3.0);
      Log (My_No'Img & ": getting" & Pool_No'Img & " for " & Dur_Image (Dur1));
      Got := Pool.Get(Pool_No, Dur1);
      Dur2 := Ada.Calendar.Clock - T1;
      Log (My_No'Img & ": got" & Pool_No'Img & " " & Got'Img
           & " after " & Dur_Image (Dur2) & " for " & Dur_Image (Dur1));

      -- Check that no signal, otherwise give up
      select
        accept Stop;
        Client_Stop := True;
      or
        delay 0.0;
      end select;

      if Got and then not Client_Stop and then not Main_Done then
        -- Wait a bit (1 to 5 s) to simulate proteted activity
        Log (My_No'Img & ": working on" & Pool_No'Img);
        Dur3 := Rnd.Gen.Dur_Random (1.0, 5.0);
        Activity: loop
          -- Split actifity into slices of 1s max
          if Dur3 > 1.0 then
            delay 1.0;
            Dur3 := Dur3 - 1.0;
          else
            -- Last slice
            delay Dur3;
            Log (My_No'Img & ": done with" & Pool_No'Img);
            exit Activity;
          end if;
          -- Activity checks for signal from time to time
          select
            accept Stop;
            Client_Stop := True;
          or
            delay 0.0;
          end select;
          if Client_Stop or else Main_Done then
            Log (My_No'Img & ": giving up with" & Pool_No'Img);
            exit Activity;
          end if;
        end loop Activity;
      end if;
      if Got then
        -- Release resource
        Log (My_No'Img & ": releasing" & Pool_No'Img);
        Pool.Release (Pool_No);
      end if;

      -- Exit on signal
      exit when Client_Stop or else Main_Done;
      -- Check that no signal, otherwise give up
      select
        accept Stop;
        Client_Stop := True;
      or
        delay 0.0;
      end select;
      exit when Client_Stop or else Main_Done;
    end loop;

    Log (My_No'Img & ": stopping");
    if not Client_Stop then
      -- Stop must be accepted only once
      accept Stop;
    end if;

    -- Ensures that all clients have finished (no risk of Get/Release)
    accept Stopped;
    Log (My_No'Img & ": stopped");
  end Client_Task;

  Clients : array (Client_Range) of Client_Task;

begin
  Log ("Start");
  -- General init
  Rnd.Gen.Randomize;
  Event_Mng.Set_Sig_Term_Callback (Term_Cb'Unrestricted_Access);

  -- Init clients, each with a specific Id
  for I in Client_Range loop
    Clients(I).Start(I);
  end loop;

  -- Wait until Term_Cb
  loop --## rule line off Loop_While
    exit when Event_Mng.Wait (1_000) and then Main_Done;
  end loop;

  -- Stop clients
  for I in Client_Range loop
    Clients(I).Stop;
  end loop;

  -- Ensure all clients are stopped
  for I in Client_Range loop
    Clients(I).Stopped;
  end loop;

  -- Clear the control pool
  Log ("Clearing");
  Pool.Clear;

  -- The end
  Log ("Done");

end T_Control_Pool;

