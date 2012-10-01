with Basic_Proc;
use Basic_Proc;
procedure Garde is

  Cond : Boolean;

  task Serveur is
    entry Init;
    entry Service_True (Client : in Integer);
    entry Service_False (Client : in Integer);
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
      Put_Output ("  Serveur sur select : condition ");
      Put_Line_Output (Cond'Img);
      select
        when Cond =>
          accept Service_True (Client : in Integer) do
            Put_Output ("  Service TRUE de ");
            Put_Output (Client'Img);
            Put_Output (" condition ");
            Put_Line_Output (Cond'Img);
            delay (0.5);
          end Service_True;
      or
        when not Cond =>
          accept Service_False(Client : in Integer) do
            Put_Output ("  Service FALSE de");
            Put_Output (Client'Img);
            Put_Output (" condition ");
            Put_Line_Output (Cond'Img);
            delay (0.5);
          end Service_False;
      or
        terminate;
      end select;

    end loop;

  exception
    when others =>
      Put_Line_Output ("  exception Serveur");
  end Serveur;


  task body Client_1 is
  begin
    accept Init;
    Put_Line_Output ("Client 1 requete TRUE");
    Serveur.Service_True (1);
    Put_Line_Output ("Client 1 servi TRUE");
  exception
    when others =>
      Put_Line_Output ("exception Client 1");
  end Client_1;

  task body Client_2 is
  begin
    accept Init;
    delay 10.0;
    Put_Line_Output (" Client 2 requete FALSE");
    Serveur.Service_False (2);
    Put_Line_Output (" Client 2 servi FALSE");
  exception
    when others =>
      Put_Line_Output ("exception Client 2");
  end Client_2;

begin
  Cond := False;
  Put_Line_Output ("        Prog condition " & Cond'Img);
  Serveur.Init;
  Client_1.Init;
  Client_2.Init;
  delay 3.0;
  Cond := True;
  Put_Line_Output ("        Prog condition " & Cond'Img);
  delay 3.0;
  Put_Line_Output ("        fin Prog");
exception
  when others =>
    Put_Line_Output ("        exception Prog");
end Garde;

