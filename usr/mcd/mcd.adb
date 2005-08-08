with Ada.Text_Io, Ada.Exceptions;
with Argument, Sys_Calls, Mixed_Str;
with Debug, Mcd_Parser, Mcd_Mng, Io_Flow;

procedure Mcd is
  Item : Mcd_Mng.Item_Rec;
  The_End : Mcd_Mng.End_Status_List;
  use type Mcd_Mng.End_Status_List;
  Invalid_Argument, Argument_Mismatch, Invalid_Register, Emtpy_Register,
                    Empty_Stack : exception;
  Parsing_Error : exception;

begin

  Debug.Init;

  -- Check at most one arg
  if Argument.Get_Nbre_Arg > 1 then
    Mcd_Parser.Print_Help;
    return;
  end if;

  -- Check for fifo
  begin
    declare
      Str : constant String := Argument.Get_Parameter (1, "f");
    begin
      -- -f. A fifo must be provided
      if Str = "" then
        Mcd_Parser.Print_Help;
        return;
      end if;
      -- A fifo is provided
    end;
  exception
    when Argument.Argument_Not_Found =>
      if Argument.Get_Nbre_Arg /= 0 then
        -- Unrecognised argument
        Mcd_Parser.Print_Help;
        return;
      end if;
    when others =>
      Mcd_Parser.Print_Help;
      return;
  end;

  -- Main loop
  loop 
    begin
      Item := Mcd_Parser.Next_Item;
      Mcd_Mng.New_Item(Item, The_End);
      exit when The_End /= Mcd_Mng.Continue;
    exception
      when others =>
        Mcd_Parser.Dump_Stack;
        raise;
    end;
  end loop;

  -- Normal exit: check empty stack
  if The_End /= Mcd_Mng.Exit_Break
  and then not Mcd_Mng.Check_Empty_Stack then
    Sys_Calls.Put_Line_Error ("Warning: The stack was not empty.");
  end if;

  -- Done
  Io_Flow.Close;
  Mcd_Mng.Close;
  Sys_Calls.Set_Ok_Exit_Code;

exception
  -- Clean mapping of exceptions
  when Mcd_Mng.Invalid_Argument =>
    Sys_Calls.Put_Line_Error ("Mcd error: Invalid argument");
    Mcd_Mng.Close;
    Sys_Calls.Set_Error_Exit_Code;
  when Mcd_Mng.Argument_Mismatch =>
    Sys_Calls.Put_Line_Error ("Mcd error: Argument mismatch");
    Mcd_Mng.Close;
    Sys_Calls.Set_Error_Exit_Code;
  when Mcd_Mng.Compute_Error =>
    Sys_Calls.Put_Line_Error ("Mcd error: Compute error");
    Mcd_Mng.Close;
    Sys_Calls.Set_Error_Exit_Code;
  when Mcd_Mng.Invalid_Register =>
    Sys_Calls.Put_Line_Error ("Mcd error: Invalid register");
    Mcd_Mng.Close;
    Sys_Calls.Set_Error_Exit_Code;
  when Mcd_Mng.Emtpy_Register =>
    Sys_Calls.Put_Line_Error ("Mcd error: Empty register");
    Mcd_Mng.Close;
    Sys_Calls.Set_Error_Exit_Code;
  when Mcd_Mng.Empty_Stack =>
    Sys_Calls.Put_Line_Error ("Mcd error: Empty stack");
    Mcd_Mng.Close;
    Sys_Calls.Set_Error_Exit_Code;
  when Mcd_Mng.String_Len =>
    Sys_Calls.Put_Line_Error ("Mcd error: String length error");
    Mcd_Mng.Close;
    Sys_Calls.Set_Error_Exit_Code;
  when Mcd_Mng.File_Error =>
    Sys_Calls.Put_Line_Error ("Mcd error: File IO error");
    Mcd_Mng.Close;
    Sys_Calls.Set_Error_Exit_Code;
  when Mcd_Parser.Parsing_Error =>
    Sys_Calls.Put_Line_Error ("Mcd error: Parsing error");
    Mcd_Mng.Close;
    Sys_Calls.Set_Error_Exit_Code;
  when Io_Flow.Fifo_Error =>
    Sys_Calls.Put_Line_Error ("Mcd error: Fifo error");
    Mcd_Mng.Close;
    Sys_Calls.Set_Error_Exit_Code;
  when Error:others =>
    Sys_Calls.Put_Line_Error ("Mcd error: exception "
              & Mixed_Str(Ada.Exceptions.Exception_Name (Error))
              & " raised.");
    Mcd_Mng.Close;
    Sys_Calls.Set_Error_Exit_Code;
end Mcd;

