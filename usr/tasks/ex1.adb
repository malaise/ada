with Basic_Proc, Normal;
use  Basic_Proc;
procedure Ex1 is
  pragma Priority (1);

  subtype Client_Range is Positive range 1..2;

  task Serveur is
    entry Init;
    entry Service (No_Client : in Client_Range);
    pragma Priority (20);
  end Serveur;

  task type Client is
    entry Init (No : in Client_Range);
    pragma Priority (10);
  end Client;

  Clients : array (Client_Range) of Client;

  procedure Print (Message : in String; Client_No : in Client_Range) is
  begin
    Put_Output (Message & ' ');
    Put_Output (Normal (Client_No, 3));
    New_Line_Output;
  end Print;


  task body Serveur is
  begin
    accept Init;

    loop
      select
        accept Service (No_Client : in Client_Range) do
          Print ("                           Service de", No_Client);
          delay (1.0);
        end Service;
      or
        terminate;
      end select;

    end loop;

  exception
    when others =>
      Put_Line_Output ("Exception serveur");
  end Serveur;


  task body Client is
    No : Client_Range;
  begin
    accept Init (No : in Client_Range) do
      Client.No := No;
    end Init;

    loop
      Print ("Requete de", No);
      Serveur.Service (No);
--      Schedule;
    end loop;

  exception
    when others =>
      Put_Line_Output ("Exception client");
  end Client;

begin
  for I in Client_Range loop
    Clients(I).Init(I);
  end loop;

  Serveur.Init;

exception
  when others =>
    Put_Line_Output ("Exception procedure");
end Ex1;

