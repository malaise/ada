with Argument, Normal;
with Errors, Basic_Proc;
package body Args is

  subtype Prio_Range is Positive range 1 .. 255;
  Prio : Prio_Str;

  procedure Usage is
  begin
    Basic_Proc.Put_Line_Error ("Usage: "
        & Argument.Get_Program_Name
        & " -b<bus_name> -p<service_port> -P<prio>");
    raise Errors.Exit_Error;
  end Usage;

  procedure Init is
  begin
    declare
      Dummy_1 : constant String := Get_Bus;
      Dummy_2 : constant String := Get_Client_Port;
      P  : constant Prio_Range
         := Prio_Range'Value (Argument.Get_Parameter(1, "P"));
    begin
      Prio := Normal (P, 3, Gap => '0');
      return;
    end;
  exception
    when Argument.Argument_Not_Found =>
      Basic_Proc.Put_Line_Error ("ERROR. Argument not found");
      Usage;
    when others =>
      Basic_Proc.Put_Line_Error ("ERROR. Invalid argument");
      Usage;
  end Init;

  function Get_Bus return String is (Argument.Get_Parameter(1, "b"));


  function Get_Client_Port return String is (Argument.Get_Parameter(1, "p"));


  function Get_Prio return Prio_Str is (Prio);

end Args;

