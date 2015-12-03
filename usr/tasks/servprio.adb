with Ada.Exceptions;
-- Service with priorities
with Protected_Put, Rnd, Schedule, Text_Line, Mutexes;
use Protected_Put;
procedure Servprio is

  type Urgency is (Critical, Urgent, Normal, Slow);
  subtype Client_Range is Positive range 1 .. 10;

  type Tab is array(Urgency) of Natural;
  Totals, Totalc : Tab := (others => 0);
  Total_Call     : Natural := 0;

  function My_Random is new Rnd.Discr_Random(Urgency);

  File : Text_Line.File_Type;
  Name  : constant String := ("servprio.dat");

  task Server is
    entry Init;
    entry Service( Urgency)(No_Client : in Client_Range);
    entry Stop;
  end Server;

  task type Client is
    entry Init (No : in Client_Range);
    entry Stop (Total : in out Tab);
  end Client;

  Clients : array(Client_Range) of Client;

  Mutex : Mutexes.Mutex (Mutexes.Simple, False);
  procedure Print (Author    : in String;
                   Degree    : in Urgency;
                   Client_No : in Client_Range) is
  begin
    Mutex.Get;
    Text_Line.Put (File, Author);
    Text_Line.Put (File, " of ");
    Text_Line.Put (File, Client_Range'Image (Client_No));
    Text_Line.Put (File, " priority ");
    Text_Line.Put_Line (File, Urgency'Image(Degree));
    Mutex.Release;
  end Print;


  task body Server is
    I : Urgency := Urgency'First;

  begin
    accept Init;

    loop
      Schedule;

      select
        accept Service(I) (No_Client : in Client_Range) do
          Print ("Service", I, No_Client);
          Put_Line_Output ("Service");
          Totals(I) := Totals(I) + 1;
        end Service;
        I := Urgency'First;
      or
        delay 0.0;
        if I /= Urgency'Last then
          I := Urgency'Succ(I);
        else
          I := Urgency'First;
        end if;
      or
        accept Stop;
        exit;
      end select;
    end loop;

  exception
    when Error: others =>
      Put_Line_Output ("Exception in serveur");
      Put_Line_Output (Ada.Exceptions.Exception_Name (Error));
  end Server;


  task body Client is
    No       : Client_Range;
    Prio     : Urgency;
    Max_Loop : constant Positive := 10;
    N_Loop   : Positive;
    Summary   : Tab := (others => 0);
  begin
    accept Init (No : in Client_Range) do
      Client.No := No;
    end Init;

    N_Loop := Rnd.Gen.Int_Random (1, Max_Loop);
    for I in 1 .. N_Loop loop
      Schedule;
      Prio := My_Random (Rnd.Gen.all);
      Print ("                           Request", Prio, No);

      Server.Service(Prio) (No);

      Summary(Prio) := Summary(Prio) + 1;
    end loop;

    accept Stop (Total : in out Tab) do
      for I in Urgency loop
        Total(I) := Total(I) + Summary(I);
      end loop;
    end Stop;

    Mutex.Get;
    Text_Line.Put (File, "                           End of client ");
    Text_Line.Put (File, Client_Range'Image(No));
    Text_Line.Put (File, " , ");
    Text_Line.Put (File, Positive'Image(N_Loop));
    Text_Line.Put_Line (File, " calls");
    Mutex.Release;

  exception
    when Error:others =>
      Put_Line_Output ("Exception in client");
      Put_Line_Output (Ada.Exceptions.Exception_Name (Error));
      raise;
  end Client;

begin
  Rnd.Gen.Randomize;
  Text_Line.Create_All (File, Name);

  for I in Client_Range loop
    Clients(I).Init (I);
  end loop;

  Server.Init;

  for I in Client_Range loop
    Clients(I).Stop (Totalc);
  end loop;

  Server.Stop;

  Text_Line.Put_Line (File, "SUMMARY");
  for I in Urgency loop
    Text_Line.Put (File, Urgency'Image(I));
    Text_Line.Put (File, " -> ");
    Text_Line.Put (File, Positive'Image(Totals(I)));
    Text_Line.Put (File, " & ");
    Text_Line.Put_Line (File, Positive'Image(Totalc(I)));
    Total_Call := Total_Call + Totalc(I);
  end loop;
  Text_Line.Put_Line (File, " TOTAL CALLS : " & Positive'Image(Total_Call));

  Text_Line.Close_All (File);

exception
  when Error: others =>
    Put_Line_Output ("Exception in prog");
    Put_Line_Output (Ada.Exceptions.Exception_Name (Error));
    raise;
end Servprio;

