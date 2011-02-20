with Basic_Proc, Event_Mng;
with Autobus;
procedure T_Autobus is
  Bus : Autobus.Bus_Type;
  Sig : Boolean;
    -- Signal callback
  procedure Signal_Cb is
  begin
    Basic_Proc.Put_Line_Output ("Signal.");
    Sig := True;
  end Signal_Cb;

begin
  Event_Mng.Set_Sig_Term_Callback (Signal_Cb'Unrestricted_Access);
  Bus.Init ("234.7.6.5:21021");

  loop
    Sig := False;
    Event_Mng.Wait (-1);
    exit when Sig;
  end loop;

  Basic_Proc.Put_Line_Output ("Done.");
end T_Autobus;

