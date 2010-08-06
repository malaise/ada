with Ada.Exceptions;
with Argument, Basic_Proc, Mixed_Str;
with Debug, Mcd_Parser, Mcd_Mng, Io_Flow;

procedure Mcd is
  Item : Mcd_Mng.Item_Rec;
  The_End : Mcd_Mng.End_Status_List;
  use type Mcd_Mng.End_Status_List;

  procedure Close is
  begin
    Io_Flow.Close;
  end Close;

  procedure Set_Error_Code is
    Error_Exit_Code : constant := 21;
  begin
    Basic_Proc.Set_Exit_Code (Error_Exit_Code);
  end Set_Error_Code;

  procedure Error (Message : in String) is
  begin
    Basic_Proc.Put_Line_Error (Mixed_Str(Argument.Get_Program_Name) & " error: "
                            & Message & ".");
    Mcd_Parser.Dump_Stack;
    Mcd_Mng.Dump_Stack;
    Close;
    Set_Error_Code;
  end Error;

begin

  Basic_Proc.Set_Ok_Exit_Code;
  Debug.Init;

  if Argument.Get_Nbre_Arg = 1
  and then (Argument.Get_Parameter (1) = "-h"
            or else Argument.Get_Parameter (1) = "--help"
            or else Argument.Get_Parameter (1) = "help") then
    Io_Flow.Init (Default => True);
    Mcd_Parser.Print_Help;
    Set_Error_Code;
    return;
  end if;

  -- Let Io_Flow init according to arguments
  begin
    Io_Flow.Init;
  exception
    when others =>
      Io_Flow.Init (Default => True);
      Mcd_Parser.Print_Help;
      raise;
  end;

  -- Main loop
  loop
    begin
      Item := Mcd_Parser.Next_Item;
      Mcd_Mng.New_Item(Item, The_End);
      exit when The_End /= Mcd_Mng.Continue;
    end;
  end loop;

  -- Normal exit: check empty stack
  if The_End /= Mcd_Mng.Exit_Break
  and then not Mcd_Mng.Check_Empty_Stack then
    Basic_Proc.Put_Line_Error ("Warning: The stack was not empty.");
  end if;

  -- Done
  Close;

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
  when Mcd_Mng.Empty_Register =>
    Error ("Empty register");
  when Mcd_Mng.Empty_Stack =>
    Error ("Empty stack");
  when Mcd_Mng.String_Len =>
    Error ("String length error");
  when Mcd_Mng.File_Error =>
    Error ("File IO error");
  when Io_Flow.Init_Error =>
    Error ("Initialization error");
  when Io_Flow.Fifo_Error =>
    Error ("Fifo opening error");
  when Mcd_Parser.Parsing_Error =>
    Error ("Parsing error");
  when Except:others =>
    Error ("Exception "
              & Mixed_Str(Ada.Exceptions.Exception_Name (Except))
              & " raised");
end Mcd;

