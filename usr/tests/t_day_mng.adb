with Ada.Exceptions, Ada.Calendar, Ada.Text_Io;
with Basic_Proc, Day_Mng;

procedure T_Day_Mng is

  package Dur_Io is new Ada.Text_Io.Fixed_Io (Ada.Calendar.Day_Duration);

  Str : String (1 .. 21);
  Len : Natural;
  Dur : Ada.Calendar.Day_Duration;
  Hours    : Day_Mng.T_Hours;
  Minutes  : Day_Mng.T_Minutes;
  Seconds  : Day_Mng.T_Seconds;
  Millisec : Day_Mng.T_Millisec;


begin

  Main:
  loop

    loop
      begin
        Basic_Proc.Put_Output ("Enter a duration: ");
        Basic_Proc.Get_Line (Str, Len);
        exit Main when Len = 0;

        Dur_Io.Get(Str(1 .. Len), Dur, Len);
        exit;
      exception
        when Error:others =>
          Basic_Proc.Put_Line_Output ("Exception "
           & Ada.Exceptions.Exception_Name (Error));
      end;
    end loop;

    Basic_Proc.Put_Line_Output(Dur'Img);
    Day_Mng.Split (Dur, Hours, Minutes, Seconds, Millisec);

    Basic_Proc.Put_Line_Output (Day_Mng.T_Hours'Image(Hours) & " h  "
                        & Day_Mng.T_Minutes'Image(Minutes) & " min  "
                        & Day_Mng.T_Seconds'Image(Seconds) & " sec  "
                        & Day_Mng.T_Millisec'Image(Millisec) & " msec");

    Dur := Day_Mng.Pack (Hours, Minutes, Seconds, Millisec);

    Basic_Proc.New_Line_Output;

  end loop Main;

end T_Day_Mng;

