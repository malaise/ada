with MY_IO;
use MY_IO; 
procedure GARDE is 

  COND : BOOLEAN; 

  task SERVEUR is 
    entry INIT; 
    entry SERVICE_TRUE(CLIENT : in INTEGER); 
    entry SERVICE_FALSE(CLIENT : in INTEGER); 
  end SERVEUR; 

  task CLIENT_1 is 
    entry INIT; 
  end CLIENT_1; 

  task CLIENT_2 is 
    entry INIT; 
  end CLIENT_2; 

  task body SERVEUR is 
  begin
    accept INIT; 

    loop
      PUT("  Serveur sur select : condition "); 
      PUT_LINE(COND); 
      select
        when COND => 
          accept SERVICE_TRUE(CLIENT : in INTEGER) do
            PUT("  Service TRUE de "); 
            PUT(CLIENT); 
            PUT(" condition "); 
            PUT_LINE(COND); 
            delay (0.5); 
          end SERVICE_TRUE; 
      or 
        when  not COND => 
          accept SERVICE_FALSE(CLIENT : in INTEGER) do
            PUT("  Service FALSE de"); 
            PUT(CLIENT); 
            PUT(" condition "); 
            PUT_LINE(COND); 
            delay (0.5); 
          end SERVICE_FALSE; 
      or 
        terminate; 
      end select; 

    end loop; 

  exception
    when others => 
      PUT_LINE("  exception Serveur"); 
  end SERVEUR; 


  task body CLIENT_1 is 
  begin
    accept INIT; 
    PUT_LINE("Client 1 requete TRUE"); 
    SERVEUR.SERVICE_TRUE(1); 
    PUT_LINE("Client 1 servi TRUE"); 
  exception
    when others => 
      PUT_LINE("exception Client 1"); 
  end CLIENT_1; 

  task body CLIENT_2 is 
  begin
    accept INIT; 
    delay 10.0; 
    PUT_LINE(" Client 2 requete FALSE"); 
    SERVEUR.SERVICE_FALSE(2); 
    PUT_LINE(" Client 2 servi FALSE"); 
  exception
    when others => 
      PUT_LINE("exception Client 2"); 
  end CLIENT_2; 

begin
  COND := FALSE; 
  PUT_LINE("        Prog condition FALSE"); 
  SERVEUR.INIT; 
  CLIENT_1.INIT; 
  CLIENT_2.INIT; 
  delay 3.0; 
  COND := TRUE; 
  PUT_LINE("        Prog condition TRUE"); 
  delay 3.0; 
  PUT_LINE("        fin Prog"); 
exception
  when others => 
    PUT_LINE("        exception Prog"); 
end GARDE; 
