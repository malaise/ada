with Ada.Exceptions;
with Argument, Sys_Calls, Mixed_Str;
with Debug, Mcd_Parser, Mcd_Mng, Io_Flow;

procedure Mcd is
  Item : Mcd_Mng.Item_Rec;
  The_End : Mcd_Mng.End_Status_List;
  use type Mcd_Mng.End_Status_List;
  Invalid_Argument, Argument_Mismatch, Invalid_Register, Emtpy_Register,
                    Empty_Stack : exception;
  Parsing_Error : exception;

  procedure Close is
  begin
    Io_Flow.Close;
  end Close;

  procedure Error (Message : in String) is
  begin
    Sys_Calls.Put_Line_Error (Mixed_Str(Argument.Get_Program_Name) & " error: "
                            & Message & ".");
    Close;
    Sys_Calls.Set_Error_Exit_Code;
  end Error;

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
  Close;
  Sys_Calls.Set_Ok_Exit_Code;

exception
  -- Clean mapping of exceptions
  when Mcd_Mng.Invalid_Argument =>
    Error ("Invalid argument");
  when Mcd_Mng.Argument_Mismatch =>
    Error ("Argument mismatch");
  when Mcd_Mng.Compute_Error =>
    Error ("Compute error");
  when Mcd_Mng.Invalid_Register =>
    Error ("Invalid register");
  when Mcd_Mng.Emtpy_Register =>
    Error ("Empty register");
  when Mcd_Mng.Empty_Stack =>
    Error ("Empty stack");
  when Mcd_Mng.String_Len =>
    Error ("String length error");
  when Mcd_Mng.File_Error =>
    Error ("File IO error");
  when Mcd_Parser.Parsing_Error =>
    Error ("Parsing error");
  when Io_Flow.Fifo_Error =>
    Error ("Fifo error");
  when Except:others =>
    Error ("Exception "
              & Mixed_Str(Ada.Exceptions.Exception_Name (Except))
              & " raised");
end Mcd;

