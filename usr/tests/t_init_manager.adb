with Ada.Text_Io;
with Init_Manager;
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
      Ada.Text_Io.Put_Line ("  Sending event:" & I'Img);
      Mng.New_Event (I);
      I := I + 1;
      select
        accept Stop;
        exit;
      or
        delay 0.2;
      end select;
    end loop;
    Ada.Text_Io.Put_Line ("  Stopped");
  end Evt_Gen;

  -- Handler of events
  procedure Evt_Handle (Event : in Natural) is
  begin
    Ada.Text_Io.Put_Line ("Handling event:" & Event'Img);
    delay 0.1;
  end Evt_Handle;

begin

  for I in 1 .. 3 loop
    -- Initialize, events shall be bufferized
    if I /= 1 then
      Mng.Set_Handler (null);
    end if;
    Ada.Text_Io.Put_Line ("Initializing");
    delay 2.0;
    -- End of init, events shall be flushed
    Ada.Text_Io.Put_Line ("Initialized");
    Mng.Set_Handler (Evt_Handle'Unrestricted_Access);
    delay 2.0;
  end loop;
  Ada.Text_Io.Put_Line ("Terminating");
  Evt_Gen.Stop;
  delay 0.1;
  Ada.Text_Io.Put_Line ("Terminated");

end T_Init_Manager;

