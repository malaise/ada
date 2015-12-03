-- Two clients share a unique service
with Protected_Put, Normal;
use  Protected_Put;
procedure Ex1 is
  pragma Priority (1);

  subtype Client_Range is Positive range 1 .. 2;

  task Server is
    entry Init;
    entry Service (No_Client : in Client_Range);
    pragma Priority (20);
  end Server;

  task type Client is
    entry Init (No : in Client_Range);
    pragma Priority (10);
  end Client;

  Clients : array (Client_Range) of Client;

  procedure Print (Message : in String; Client_No : in Client_Range) is
  begin
    Put_Line_Output (Message & ' ' & Normal (Client_No, 2));
  end Print;


  task body Server is
  begin
    accept Init;

    loop
      select
        accept Service (No_Client : in Client_Range) do
          Print ("                           Service of", No_Client);
          delay (1.0);
        end Service;
      or
        terminate;
      end select;

    end loop;

  exception
    when others =>
      Put_Line_Output ("Exception in server");
  end Server;


  task body Client is
    No : Client_Range;
  begin
    accept Init (No : in Client_Range) do
      Client.No := No;
    end Init;

    loop
      Print ("Request from", No);
      Server.Service (No);
    end loop;

  exception
    when others =>
      Put_Line_Output ("Exception in client");
  end Client;

begin
  for I in Client_Range loop
    Clients(I).Init(I);
  end loop;

  Server.Init;

exception
  when others =>
    Put_Line_Output ("Exception in main procedure");
end Ex1;

