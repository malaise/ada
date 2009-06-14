with My_Io;
use My_Io;
procedure Garde is

  Cond : Boolean;

  task Serveur is
    entry Init;
    entry Service_True(Client : in Integer);
    entry Service_False(Client : in Integer);
  end Serveur;

  task Client_1 is
    entry Init;
  end Client_1;

  task Client_2 is
    entry Init;
  end Client_2;

  task body Serveur is
  begin
    accept Init;

    loop
      Put("  Serveur sur select : condition ");
      Put_Line(Cond);
      select
        when Cond =>
          accept Service_True(Client : in Integer) do
            Put("  Service TRUE de ");
            Put(Client);
            Put(" condition ");
            Put_Line(Cond);
            delay (0.5);
          end Service_True;
      or
        when  not Cond =>
          accept Service_False(Client : in Integer) do
            Put("  Service FALSE de");
            Put(Client);
            Put(" condition ");
            Put_Line(Cond);
            delay (0.5);
          end Service_False;
      or
        terminate;
      end select;

    end loop;

  exception
    when others =>
      Put_Line("  exception Serveur");
  end Serveur;


  task body Client_1 is
  begin
    accept Init;
    Put_Line("Client 1 requete TRUE");
    Serveur.Service_True(1);
    Put_Line("Client 1 servi TRUE");
  exception
    when others =>
      Put_Line("exception Client 1");
  end Client_1;

  task body Client_2 is
  begin
    accept Init;
    delay 10.0;
    Put_Line(" Client 2 requete FALSE");
    Serveur.Service_False(2);
    Put_Line(" Client 2 servi FALSE");
  exception
    when others =>
      Put_Line("exception Client 2");
  end Client_2;

begin
  Cond := False;
  Put_Line("        Prog condition " & Cond'Img);
  Serveur.Init;
  Client_1.Init;
  Client_2.Init;
  delay 3.0;
  Cond := True;
  Put_Line("        Prog condition " & Cond'Img);
  delay 3.0;
  Put_Line("        fin Prog");
exception
  when others =>
    Put_Line("        exception Prog");
end Garde;

