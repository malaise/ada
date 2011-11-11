-- Pause for some seconds (default 1.0)
with Ada.Text_Io, Ada.Calendar;
with Argument, Basic_Proc;
procedure Wait is

  Start : constant Ada.Calendar.Time := Ada.Calendar.Clock;

  package Dur_Io is new Ada.Text_Io.Fixed_Io(Duration);
  package Int_Io is new Ada.Text_Io.Integer_Io(Integer);

  Dur  : Duration;
  Int  : Integer;
  Last : Positive;

  use Ada.Calendar;

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
    Dur := 0.0;
  else
    raise Constraint_Error;
  end if;
  delay Dur - (Ada.Calendar.Clock - Start);

exception
  when others =>
    Basic_Proc.Put_Line_Error (
       "Usage : ""wait [seconds]""     (1.0 by default).");
end Wait;

