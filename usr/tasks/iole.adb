with MY_IO, RND;
procedure IOLE is

  task T1 is
    entry REQUEST;
  end T1;

  task T2;

  task body T1 is
  begin
    loop
      select
        accept REQUEST do
          MY_IO.PUT_LINE ("Request");
        end REQUEST;
      or
        terminate;
      end select;
    end loop;
  end T1;

  task body T2 is
  begin
    loop
      delay RND.DUR_RANDOM (0.0, 0.1);
      MY_IO.PUT_LINE ("Event");
    end loop;
  end T2;

begin
  loop
    delay RND.DUR_RANDOM (0.0, 0.2);
    T1.REQUEST;
  end loop;
end IOLE;
