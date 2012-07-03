with Basic_Proc, Init_Manager;
procedure T_Init_Manager is

  package Mng is new Init_Manager (Natural);

  -- Task that injects an update event each second
  task Evt_Gen is
    entry Stop;
  end Evt_Gen;

  task body Evt_Gen is
    I : Natural := 0;
  begin
    loop
      Basic_Proc.Put_Line_Output ("  Sending event:" & I'Img);
      Mng.New_Event (I);
      I := I + 1;
      select
        accept Stop;
        exit;
      or
        delay 0.2;
      end select;
    end loop;
    Basic_Proc.Put_Line_Output ("  Stopped");
  end Evt_Gen;

  -- Handler of events
  procedure Evt_Handle (Event : in Natural) is
  begin
    Basic_Proc.Put_Line_Output ("Handling event:" & Event'Img);
    delay 0.1;
  end Evt_Handle;

begin

  for I in 1 .. 3 loop
    -- Initialize, events shall be bufferized
    if I /= 1 then
      Mng.Set_Handler (null);
    end if;
    Basic_Proc.Put_Line_Output ("Initializing");
    delay 1.0;
    -- End of init, events shall be flushed
    Basic_Proc.Put_Line_Output ("Initialized");
    Mng.Set_Handler (Evt_Handle'Unrestricted_Access);
    delay 1.0;
    Basic_Proc.New_Line_Output;
  end loop;
  Basic_Proc.Put_Line_Output ("Terminating");
  Evt_Gen.Stop;
  delay 0.1;
  Basic_Proc.Put_Line_Output ("Terminated");

end T_Init_Manager;

