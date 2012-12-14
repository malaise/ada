with Basic_Proc, Rnd;
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
          Basic_Proc.Put_Line_Output ("Request");
        end Request;
      or
        terminate;
      end select;
    end loop;
  end T1;

  task body T2 is
  begin
    loop
      delay Rnd.Gen.Dur_Random (0.0, 0.1);
      Basic_Proc.Put_Line_Output ("Event");
    end loop;
  end T2;

begin
  Rnd.Gen.Randomize;
  loop
    delay Rnd.Gen.Dur_Random (0.0, 0.2);
    T1.Request;
  end loop;
end Iole;

