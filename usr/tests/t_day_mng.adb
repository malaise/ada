with Text_Io, Calendar;
with Day_Mng;

procedure T_Day_Mng is

  package Dur_Io is new Text_Io.Fixed_Io (Calendar.Day_Duration);

  Dur : Calendar.Day_Duration;
  Hours    : Day_Mng.T_Hours;
  Minutes  : Day_Mng.T_Minutes;
  Seconds  : Day_Mng.T_Seconds;
  Millisec : Day_Mng.T_Millisec;

  
begin

  loop

    loop
      begin
        Text_Io.Put ("Enter a duration: ");
        Dur_Io.Get(Dur);
        exit;
      exception
        when others =>
          Text_Io.Put_Line ("Error.");
      end;
    end loop;

    Day_Mng.Split (Dur, Hours, Minutes, Seconds, Millisec);

    Text_Io.Put_Line (Day_Mng.T_Hours'Image(Hours) & " h  "
                    & Day_Mng.T_Minutes'Image(Minutes) & " min  "
                    & Day_Mng.T_Seconds'Image(Seconds) & " sec  "
                    & Day_Mng.T_Millisec'Image(Millisec) & " msec");

    Dur := Day_Mng.Pack (Hours, Minutes, Seconds, Millisec);

    Dur_Io.Put(Dur);
    Text_Io.New_Line;

  end loop;

end T_Day_Mng;

