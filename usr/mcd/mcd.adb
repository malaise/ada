with Ada.Text_Io, Ada.Exceptions;
with Argument, Sys_Calls;
with Debug, Mcd_Parser, Mcd_Mng, Io_FLow;

procedure Mcd is
  Item : Mcd_Mng.Item_Rec;
  The_End : Boolean;
  Invalid_Argument, Argument_Mismatch, Invalid_Register, Emtpy_Register,
                    Empty_Stack : exception;
  Parsing_Error : exception;

begin

  Debug.Init;

  begin
    declare
      Str : constant String := Argument.Get_Parameter (1, "h");
    begin
      Mcd_Parser.Print_Help;
      return;
    end;
  exception
    when Argument.Argument_Not_Found =>
      null;
    when others =>
      Mcd_Parser.Print_Help;
      return;
  end;

  loop 
    begin
      Item := Mcd_Parser.Next_Item;
      Mcd_Mng.New_Item(Item, The_End);
      exit when The_End;
    exception
      when others =>
        Mcd_Parser.Dump_Stack;
        raise;
    end;
   end loop;
   
   if not Mcd_Mng.Check_Empty_Stack then
     Sys_Calls.Put_Line_Error ("Warning: The stack was not empty.");
   end if;

   Io_Flow.Close;
   Mcd_Mng.Close;
exception
  -- Clean mapping of exceptions
  when Mcd_Mng.Invalid_Argument =>
    Sys_Calls.Put_Line_Error ("Error: Invalid argument");
    Mcd_Mng.Close;
    Sys_Calls.Set_Error_Exit_Code;
  when Mcd_Mng.Argument_Mismatch =>
    Sys_Calls.Put_Line_Error ("Error: Argument mismatch");
    Mcd_Mng.Close;
    Sys_Calls.Set_Error_Exit_Code;
  when Mcd_Mng.Compute_Error =>
    Sys_Calls.Put_Line_Error ("Error: Compute error");
    Mcd_Mng.Close;
    Sys_Calls.Set_Error_Exit_Code;
  when Mcd_Mng.Invalid_Register =>
    Sys_Calls.Put_Line_Error ("Error: Invalid register");
    Mcd_Mng.Close;
    Sys_Calls.Set_Error_Exit_Code;
  when Mcd_Mng.Emtpy_Register =>
    Sys_Calls.Put_Line_Error ("Error: Empty register");
    Mcd_Mng.Close;
    Sys_Calls.Set_Error_Exit_Code;
  when Mcd_Mng.Empty_Stack =>
    Sys_Calls.Put_Line_Error ("Error: Empty stack");
    Mcd_Mng.Close;
    Sys_Calls.Set_Error_Exit_Code;
  when Mcd_Mng.String_Len =>
    Sys_Calls.Put_Line_Error ("Error: String length error");
    Mcd_Mng.Close;
    Sys_Calls.Set_Error_Exit_Code;
  when Mcd_Mng.File_Error =>
    Sys_Calls.Put_Line_Error ("Error: File IO error");
    Mcd_Mng.Close;
    Sys_Calls.Set_Error_Exit_Code;
  when Mcd_Parser.Parsing_Error =>
    Sys_Calls.Put_Line_Error ("Error: Parsing error");
    Mcd_Mng.Close;
    Sys_Calls.Set_Error_Exit_Code;
  when Io_Flow.Fifo_Error =>
    Sys_Calls.Put_Line_Error ("Error: Fifo error");
    Mcd_Mng.Close;
    Sys_Calls.Set_Error_Exit_Code;
  when Error:others =>
    Sys_Calls.Put_Line_Error ("Error: exception "
                            & Ada.Exceptions.Exception_Name (Error));
    Mcd_Mng.Close;
    Sys_Calls.Set_Error_Exit_Code;
end Mcd;

