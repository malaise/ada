with My_Io;
 
procedure Para is
 
  Result : Boolean;
  Error  : exception;
 
  task type Computer is
    entry Start(Ok : in Boolean);
    entry Stop(Ok : out Boolean);
    entry Kill;
  end Computer;

  Comp_1, Comp_2 : Computer;


  task body Computer is
    Loc          : Boolean;
  begin
    Work :
    loop

      Secure :
      begin

        Treat :
        begin

          select
            accept Start(Ok : in Boolean) do
              Loc := Ok;
            end Start;
          or
            accept Kill;
            exit Work;
          end select;

          for I in 1 .. 10 loop
            delay 0.0;
          end loop;
          if not Loc then
            raise Error;
          end if;
          accept Stop(Ok : out Boolean) do
            Ok := Loc;
          end Stop;
        exception
          when Error =>
            accept Stop(Ok : out Boolean) do
              Ok := False;
              raise;
            end Stop;
        end Treat;

      exception
        when others =>
          null;
      end Secure;

    end loop Work;

  end Computer;


begin    -- para
  begin
    for I in 1 .. 10 loop
      Comp_1.Start(True);
      Comp_2.Start(True);
      Comp_1.Stop(Result);
      Comp_2.Stop(Result);
    end loop;
    My_Io.Put_Line("Successfull calls achived");
  exception
    when others =>
      My_Io.Put_Line("EXCEPTION " & " raised during successfull calls!");
  end;

  Comp_1.Start(False);
  Comp_2.Start(False);
  begin
    Comp_1.Stop(Result);
  exception
    when Error =>
      My_Io.Put_Line("Exception ERROR successfully transmitted from 1");
      raise;
    when Tasking_Error =>
      My_Io.Put_Line("Exception TASKING_ERROR successfully transmitted from 1");
      raise;
    when Program_Error =>
      My_Io.Put_Line("Exception PROGRAM_ERROR successfully transmitted from 1");
      raise;
    when Storage_Error =>
      My_Io.Put_Line("Exception STORAGE_ERROR successfully transmitted from 1");
      raise;
    when Constraint_Error | Numeric_Error =>
      My_Io.Put_Line(
       "Exception CONSTRAINT - NUMERIC ERROR successfully transmitted from 1");
      raise;
    when others =>
      My_Io.Put_Line("Other EXCEPTION raised during unsuccessfull call to 1!");
      raise;
  end;
  begin
    Comp_2.Stop(Result);
  exception
    when Error =>
      My_Io.Put_Line("Exception ERROR successfully transmitted from 2");
      raise;
    when others =>
      My_Io.Put_Line("Other EXCEPTION raised during unsuccessfull call to 2!");
      raise;
  end;
  Comp_1.Kill;
  Comp_2.Kill;
end Para;
