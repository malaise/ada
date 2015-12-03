-- Two clients and a server with a guarded Service
with Protected_Put;
use Protected_Put;
procedure Garde is

  Cond : Boolean;

  task Server is
    entry Init;
    entry Service_True (Client : in Integer);
    entry Service_False (Client : in Integer);
  end Server;

  task Client_1 is
    entry Init;
  end Client_1;

  task Client_2 is
    entry Init;
  end Client_2;

  task body Server is
  begin
    accept Init;

    loop
      Put_Line_Output ("  Server on select : condition " & Cond'Img);
      select
        when Cond =>
          accept Service_True (Client : in Integer) do
            Put_Line_Output ("  Service TRUE of " & Client'Img
                           & " condition " & Cond'Img);
            delay (0.5);
          end Service_True;
      or
        when not Cond =>
          accept Service_False(Client : in Integer) do
            Put_Line_Output ("  Service FALSE of " & Client'Img
                           & " condition " & Cond'Img);
            delay (0.5);
          end Service_False;
      or
        terminate;
      end select;

    end loop;

  exception
    when others =>
      Put_Line_Output ("  exception in Server");
  end Server;


  task body Client_1 is
  begin
    accept Init;
    Put_Line_Output ("Client 1 request TRUE");
    Server.Service_True (1);
    Put_Line_Output ("Client 1 served TRUE");
  exception
    when others =>
      Put_Line_Output ("exception in Client 1");
  end Client_1;

  task body Client_2 is
  begin
    accept Init;
    delay 10.0;
    Put_Line_Output (" Client 2 request FALSE");
    Server.Service_False (2);
    Put_Line_Output (" Client 2 served FALSE");
  exception
    when others =>
      Put_Line_Output ("exception in Client 2");
  end Client_2;

begin
  Cond := False;
  Put_Line_Output ("        Prog condition " & Cond'Img);
  Server.Init;
  Client_1.Init;
  Client_2.Init;
  delay 3.0;
  Cond := True;
  Put_Line_Output ("        Prog condition " & Cond'Img);
  delay 3.0;
  Put_Line_Output ("        end Prog");
exception
  when others =>
    Put_Line_Output ("        exception in Prog");
end Garde;

