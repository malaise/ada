with Argument, Text_Io, Calendar;
procedure Wait is

  Start : constant Calendar.Time := Calendar.Clock;

  package Dur_Io is new Text_Io.Fixed_Io(Duration);
  package Int_Io is new Text_Io.Integer_Io(Integer);

  Dur  : Duration;
  Int  : Integer;
  Last : Positive;

  use Calendar;

begin

  if Argument.Get_Nbre_Arg = 1 then
    begin
      Dur_Io.Get(Argument.Get_Parameter, Dur, Last);
    exception
      when others =>
        Int_Io.Get(Argument.Get_Parameter, Int, Last);
        Dur := Duration(Int);
    end;
  elsif  Argument.Get_Nbre_Arg = 0 then
    Dur := 1.0;
  else
    raise Constraint_Error;
  end if;
  delay Dur - (Calendar.Clock - Start);

exception
  when others => 
    Text_Io.Put_Line("Usage : ""wait [seconds]""     (1.0 by default)."); 
end Wait; 
