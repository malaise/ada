with Ada.Text_Io;
with Argument, Normal, My_Io, Get_Float;
with Fl_Time, Fl_Get;

procedure Fl is

  Tt, T : Fl_Time.Time_Type;
  Max_Hour_Dig : constant := 9; -- FL_TIME.HOURS_RANGE'WIDTH;
  With_Cost : Boolean;
  Cost, Tmp_Cost : Float;
  use Fl_Time;
begin
  if Argument.Get_Nbre_Arg = 1 and then
     Argument.Get_Parameter = "-c" then
    With_Cost := True;
  elsif Argument.Get_Nbre_Arg = 0 then
    With_Cost := False;
  else
    Ada.Text_Io.Put_Line ("Usage: "
       & Argument.Get_Program_Name & " [ -c ]");
    return;
  end if;

  Ada.Text_Io.Put_Line ("      HOURS and MINUTES additions.");
  Ada.Text_Io.Put_Line ("      ----------------------------");
  Ada.Text_Io.Put_Line (" Syntax of time is [-]hhhhhhh[.mm] (Return);");
  Ada.Text_Io.Put_Line (" Enter 'C'to clear, 'X' or 'Q' to exit.");
  Ada.Text_Io.New_Line (2);

  -- Initialise
  Tt := (True, 0, 0);
  Cost := 0.0;
  Tmp_Cost := 0.0;

  loop
    -- Display
    Ada.Text_Io.Put ("--> ");

    if Tt.Positiv then
      Ada.Text_Io.Put (' ');
    else
      Ada.Text_Io.Put ('-');
    end if;
    Ada.Text_Io.Put (Normal(Integer(Tt.Hours), Max_Hour_Dig + 1));
    Ada.Text_Io.Put ('.');
    Ada.Text_Io.Put (Normal(Integer(Tt.Minutes), 2, Gap => '0'));
    if With_Cost then
       Ada.Text_Io.Put ("     This cost: ");
       My_Io.Put(Tmp_Cost, Fore => 4, Aft => 2, Exp => 0);
       Ada.Text_Io.Put ("  Total cost: ");
       My_Io.Put_Line(Cost, Fore => 5, Aft => 2, Exp => 0);
    else
      Ada.Text_Io.New_Line;
    end if;
      

    -- Get
    begin
      T := Fl_Get.Get_Time;
    exception
      when Fl_Get.Error =>
        T := (True, 0, 0);
        Tmp_Cost := 0.0;
        Ada.Text_Io.Put_Line (Ascii.Bel & "Error.");
      when Fl_Get.Clear =>
        T := (True, 0, 0);
        Tt := (True, 0, 0);
        Tmp_Cost := 0.0;
        Cost := 0.0;
      when Fl_Get.Quit =>
        exit;
    end;

    if With_Cost and then T /= (True, 0, 0) then
      -- Get hourly cost of aircraft
      declare
        Str : String(1 .. 8);
        Len : Natural;
      begin
        Ada.Text_Io.Put(">>");
        Ada.Text_Io.Get_Line(Str, Len);
        Tmp_Cost := Get_Float.Get_Float(Str(1 .. Len));
      exception
        when others =>
          T := (True, 0, 0);
          Tmp_Cost := 0.0;
          Ada.Text_Io.Put_Line (Ascii.Bel & "Error.");
      end;
      -- Cost of the flight
      Tmp_Cost := Tmp_Cost * Float(T.Hours)
                + Tmp_Cost * Float(T.Minutes) / 60.0;
      if not T.Positiv then
        Tmp_Cost := -Tmp_Cost;
      end if;
    end if;

    -- Add / substract
    declare
      St : constant Fl_Time.Time_Type := Tt;
    begin
      Tt := Tt + T;
      if Tt.Hours = 0 and then Tt.Minutes = 0 then
        Tt.Positiv := True;
      end if;
    exception
      when Time_Overflow =>
        Tt := St;
        Ada.Text_Io.Put_Line (Ascii.Bel & "Overflow.");
    end;
    Cost := Cost + Tmp_Cost;

  end loop;

end Fl;

