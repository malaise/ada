with MY_IO;
 
procedure PARA is
 
  RESULT : BOOLEAN;
  ERROR  : exception;
 
  task type COMPUTER is
    entry START(OK : in BOOLEAN);
    entry STOP(OK : out BOOLEAN);
    entry KILL;
  end COMPUTER;

  COMP_1, COMP_2 : COMPUTER;


  task body COMPUTER is
    LOC          : BOOLEAN;
  begin
    WORK :
    loop

      SECURE :
      begin

        TREAT :
        begin

          select
            accept START(OK : in BOOLEAN) do
              LOC := OK;
            end START;
          or
            accept KILL;
            exit WORK;
          end select;

          for I in 1 .. 10 loop
            delay 0.0;
          end loop;
          if not LOC then
            raise ERROR;
          end if;
          accept STOP(OK : out BOOLEAN) do
            OK := LOC;
          end STOP;
        exception
          when ERROR =>
            accept STOP(OK : out BOOLEAN) do
              OK := FALSE;
              raise;
            end STOP;
        end TREAT;

      exception
        when others =>
          null;
      end SECURE;

    end loop WORK;

  end COMPUTER;


begin    -- para
  begin
    for I in 1 .. 10 loop
      COMP_1.START(TRUE);
      COMP_2.START(TRUE);
      COMP_1.STOP(RESULT);
      COMP_2.STOP(RESULT);
    end loop;
    MY_IO.PUT_LINE("Successfull calls achived");
  exception
    when others =>
      MY_IO.PUT_LINE("EXCEPTION " & " raised during successfull calls!");
  end;

  COMP_1.START(FALSE);
  COMP_2.START(FALSE);
  begin
    COMP_1.STOP(RESULT);
  exception
    when ERROR =>
      MY_IO.PUT_LINE("Exception ERROR successfully transmitted from 1");
      raise;
    when TASKING_ERROR =>
      MY_IO.PUT_LINE("Exception TASKING_ERROR successfully transmitted from 1");
      raise;
    when PROGRAM_ERROR =>
      MY_IO.PUT_LINE("Exception PROGRAM_ERROR successfully transmitted from 1");
      raise;
    when STORAGE_ERROR =>
      MY_IO.PUT_LINE("Exception STORAGE_ERROR successfully transmitted from 1");
      raise;
    when CONSTRAINT_ERROR | NUMERIC_ERROR =>
      MY_IO.PUT_LINE(
       "Exception CONSTRAINT - NUMERIC ERROR successfully transmitted from 1");
      raise;
    when others =>
      MY_IO.PUT_LINE("Other EXCEPTION raised during unsuccessfull call to 1!");
      raise;
  end;
  begin
    COMP_2.STOP(RESULT);
  exception
    when ERROR =>
      MY_IO.PUT_LINE("Exception ERROR successfully transmitted from 2");
      raise;
    when others =>
      MY_IO.PUT_LINE("Other EXCEPTION raised during unsuccessfull call to 2!");
      raise;
  end;
  COMP_1.KILL;
  COMP_2.KILL;
end PARA;
