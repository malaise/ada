with My_Io, Rnd;
procedure Iole is

  task T1 is
    entry Request;
  end T1;

  task T2;

  task body T1 is
  begin
    loop
      select
        accept Request do
          My_Io.Put_Line ("Request");
        end Request;
      or
        terminate;
      end select;
    end loop;
  end T1;

  task body T2 is
  begin
    loop
      delay Rnd.Dur_Random (0.0, 0.1);
      My_Io.Put_Line ("Event");
    end loop;
  end T2;

begin
  loop
    delay Rnd.Dur_Random (0.0, 0.2);
    T1.Request;
  end loop;
end Iole;
