with MY_IO, TEXT_IO, SCHEDULE;
use  MY_IO;
procedure EX1 is
  pragma PRIORITY (1);

  subtype CLIENT_RANGE is POSITIVE range 1..2;

  task SERVEUR is
    entry INIT;
    entry SERVICE (NO_CLIENT : in CLIENT_RANGE);
    pragma PRIORITY (20);
  end SERVEUR;

  task type CLIENT is
    entry INIT (NO : in CLIENT_RANGE);
    pragma PRIORITY (10);
  end CLIENT;

  CLIENTS : array (CLIENT_RANGE) of CLIENT;

  procedure PRINT (MESSAGE : in STRING; CLIENT_NO : in CLIENT_RANGE) is
  begin
    PUT (MESSAGE & ' ');
    PUT (CLIENT_NO, 3);
    NEW_LINE;
  end PRINT;


  task body SERVEUR is
  begin
    accept INIT;

    loop
      select
        accept SERVICE (NO_CLIENT : in CLIENT_RANGE) do
          PRINT("                           Service de", NO_CLIENT);
          delay (1.0);
        end SERVICE;
      or
        terminate;
      end select;

    end loop;

  exception
    when others =>
      PUT_LINE ("Exception serveur");
  end SERVEUR;


  task body CLIENT is
    NO : CLIENT_RANGE;
  begin
    accept INIT (NO : in CLIENT_RANGE) do
      CLIENT.NO := NO;
    end INIT;

    loop
      PRINT("Requete de", NO);
      SERVEUR.SERVICE (NO);
--      SCHEDULE;
    end loop;

  exception
    when others =>
      PUT_LINE ("Exception client");
  end CLIENT;

begin
  for I in CLIENT_RANGE loop
    CLIENTS(I).INIT(I);
  end loop;

  SERVEUR.INIT;

exception
  when others =>
    PUT_LINE ("Exception procedure");
end EX1;
