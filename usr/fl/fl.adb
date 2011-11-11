with Ada.Characters.Latin_1;
with Argument, Normal, My_Io, Get_Float, Basic_Proc;
with Fl_Time, Fl_Get;

procedure Fl is

  Tt, T : Fl_Time.Time_Type;
  Max_Hour_Dig : constant := 9; -- Fl_Time.Hours_Range'Width;
  With_Cost : Boolean;
  Cost, Tmp_Cost : Float;
  use Fl_Time;
  Bell : Character renames Ada.Characters.Latin_1.Bel;
begin
  if Argument.Get_Nbre_Arg = 1 and then
     Argument.Get_Parameter = "-c" then
    With_Cost := True;
  elsif Argument.Get_Nbre_Arg = 0 then
    With_Cost := False;
  else
    Basic_Proc.Put_Line_Error ("Usage: "
       & Argument.Get_Program_Name & " [ -c ]");
    return;
  end if;

  Basic_Proc.Put_Line_Output ("      HOURS and MINUTES additions.");
  Basic_Proc.Put_Line_Output ("      ----------------------------");
  Basic_Proc.Put_Line_Output (" Syntax of time is [-]hhhhhhh[.mm] (Return);");
  Basic_Proc.Put_Line_Output (" Enter 'C'to clear, 'X' or 'Q' to exit.");
  Basic_Proc.New_Line_Output;
  Basic_Proc.New_Line_Output;

  -- Initialise
  Tt := (True, 0, 0);
  Cost := 0.0;
  Tmp_Cost := 0.0;

  loop
    -- Display
    Basic_Proc.Put_Output ("--> ");

    if Tt.Positiv then
      Basic_Proc.Put_Output (' ');
    else
      Basic_Proc.Put_Output ('-');
    end if;
    Basic_Proc.Put_Output (Normal(Integer(Tt.Hours), Max_Hour_Dig + 1));
    Basic_Proc.Put_Output ('.');
    Basic_Proc.Put_Output (Normal(Integer(Tt.Minutes), 2, Gap => '0'));
    if With_Cost then
       Basic_Proc.Put_Output ("     This cost: ");
       My_Io.Put(Tmp_Cost, Fore => 4, Aft => 2, Exp => 0);
       Basic_Proc.Put_Output ("  Total cost: ");
       My_Io.Put_Line(Cost, Fore => 5, Aft => 2, Exp => 0);
    else
      Basic_Proc.New_Line_Output;
    end if;


    -- Get
    begin
      T := Fl_Get.Get_Time;
    exception
      when Fl_Get.Error =>
        T := (True, 0, 0);
        Tmp_Cost := 0.0;
        Basic_Proc.Put_Line_Error (Bell & "Error.");
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
        Basic_Proc.Put_Output (">>");
        Basic_Proc.Get_Input (Str, Len);
        Tmp_Cost := Get_Float.Get_Float (Str(1 .. Len));
      exception
        when others =>
          T := (True, 0, 0);
          Tmp_Cost := 0.0;
          Basic_Proc.Put_Line_Error (Bell & "Error.");
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
        Basic_Proc.Put_Line_Error (Bell & "Overflow.");
    end;
    Cost := Cost + Tmp_Cost;

  end loop;

end Fl;

