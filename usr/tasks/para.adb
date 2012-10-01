with Basic_Proc;

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
            accept Start (Ok : in Boolean) do
              Loc := Ok;
            end Start;
          or
            accept Kill;
            exit Work;
          end select;

          delay 0.1;
          if not Loc then
            raise Error;
          end if;
          accept Stop (Ok : out Boolean) do
            Ok := Loc;
          end Stop;
        exception
          when Error =>
            accept Stop (Ok : out Boolean) do
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
      Comp_1.Start (True);
      Comp_2.Start (True);
      Comp_1.Stop (Result);
      Comp_2.Stop (Result);
    end loop;
    Basic_Proc.Put_Line_Output ("Successfull calls achived");
  exception
    when others =>
      Basic_Proc.Put_Line_Output (
          "EXCEPTION " & " raised during successfull calls!");
  end;

  Comp_1.Start (False);
  Comp_2.Start (False);

  begin
    Comp_1.Stop (Result);
  exception
    when Error =>
      Basic_Proc.Put_Line_Output (
          "Exception ERROR successfully transmitted from 1");
    when others =>
      Basic_Proc.Put_Line_Output (
          "Other EXCEPTION raised during unsuccessfull call to 1!");
      raise;
  end;
  begin
    Comp_2.Stop (Result);
  exception
    when Error =>
      Basic_Proc.Put_Line_Output (
          "Exception ERROR successfully transmitted from 2");
    when others =>
      Basic_Proc.Put_Line_Output (
          "Other EXCEPTION raised during unsuccessfull call to 2!");
      raise;
  end;
  Comp_1.Kill;
  Comp_2.Kill;
  Basic_Proc.Put_Line_Output ("Calls to kill achived");
end Para;

