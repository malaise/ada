with Text_Io;
with Argument, Sys_Calls;
with Debug, Parser, Mcd_Mng;

procedure Mcd is
  Item : Mcd_Mng.Item_Rec;
  The_End : Boolean;
  Invalid_Argument, Argument_Mismatch, Invalid_Register, Emtpy_Register,
                    Empty_Stack : exception;
  Parsing_Error : exception;

begin

  Debug.Init;

  if Argument.Get_Nbre_Arg /= 0 then
    Parser.Print_Help;
    return;
  end if;

  loop 
    begin
      Item := Parser.Next_Item;
      Mcd_Mng.New_Item(Item, The_End);
      exit when The_End;
    exception
      when others =>
        Parser.Dump_Stack;
        raise;
    end;
   end loop;
   
   if not Mcd_Mng.Check_Empty_Stack then
     Sys_Calls.Put_Line_Error ("Warning: The stack was not empty.");
   end if;

exception
  -- Clean mapping of exceptions
  when Mcd_Mng.Invalid_Argument =>
    Sys_Calls.Put_Line_Error ("Error: Invalid argument");
    Sys_Calls.Set_Error_Exit_Code;
  when Mcd_Mng.Argument_Mismatch =>
    Sys_Calls.Put_Line_Error ("Error: Argument mismatch");
    Sys_Calls.Set_Error_Exit_Code;
  when Mcd_Mng.Compute_Error =>
    Sys_Calls.Put_Line_Error ("Error: Compute error");
    Sys_Calls.Set_Error_Exit_Code;
  when Mcd_Mng.Invalid_Register =>
    Sys_Calls.Put_Line_Error ("Error: Invalid register");
    Sys_Calls.Set_Error_Exit_Code;
  when Mcd_Mng.Emtpy_Register =>
    Sys_Calls.Put_Line_Error ("Error: Empty register");
    Sys_Calls.Set_Error_Exit_Code;
  when Mcd_Mng.Empty_Stack =>
    Sys_Calls.Put_Line_Error ("Error: Empty stack");
    Sys_Calls.Set_Error_Exit_Code;
  when Mcd_Mng.String_Len =>
    Sys_Calls.Put_Line_Error ("Error: String length error");
    Sys_Calls.Set_Error_Exit_Code;
  when Parser.Parsing_Error =>
    Sys_Calls.Put_Line_Error ("Error: Parsing error");
    Sys_Calls.Set_Error_Exit_Code;
end Mcd;

