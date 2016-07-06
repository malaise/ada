with Ada.Exceptions;
with argument, Basic_Proc;
with Xml_Parser;
procedure Sensor is

  procedure Help is
  begin
    Basic_Proc.Put_Line_Error (Argument.get_Program_Name
        & "<configuration file>");
  end Help;

  -- Xml parsing
  Ctx : Xml_Parser.Ctx_Type;
  Ok : Boolean;
  Root, Node : Xml_Parser.Element_Type;
  

begin

  -- Help mode
  if Argument.Get_Nbre_Param /= 1 then
    Help;
    Basic_Proc.Set_Error_Exit_Code;
    return;
  elsif Argument.Get_Parameter = "-h"
  or else Argument.Get_Parameter = "--help" then
    Help;
    Basic_Proc.Set_Error_Exit_Code;
    return;
  end if;

  -- Parse configuration file
  Ctx.Parse (Argument.Get_Parameter, Ok);
  if not Ok then
    Basic_Proc.Put_Line_Error ("Parse error in config: "
                                 & Ctx.Get_Parse_Error_Message);
    Basic_Proc.Set_Error_Exit_Code;
    return;
  end if;


exception
  when Xml_Parser.File_Error =>
    Basic_Proc.Put_Line_Error (
        "Cannot open config file: " & Argument.Get_Parameter & ".");
    Basic_Proc.Set_Error_Exit_Code;
    return;

end Sensor;


