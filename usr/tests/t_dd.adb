with Basic_Proc, Argument, Dyn_Data;

procedure T_Dd is

  subtype Data is String (1 .. 1024);
  type Data_Access is access Data;

  package My_Dyn_Data is new Dyn_Data (Data, Data_Access);

  Max_Data : constant := 256;

  Data_Access_Array : array (1 .. Max_Data) of Data_Access;

  Nb_Loop : Natural;

  Abort_Exception : exception;

  procedure One_Cycle is
  begin
    for J in 1 .. Max_Data loop
      Data_Access_Array(J) := My_Dyn_Data.Allocate;
    end loop;

    for J in 1 .. Max_Data loop
      My_Dyn_Data.Free (Data_Access_Array(J));
    end loop;
  end One_Cycle;

begin
  begin
    if Argument.Get_Nbre_Arg = 0 then
      Nb_Loop := 0;
    elsif Argument.Get_Nbre_Arg = 1 then
      Nb_Loop := Natural'Value(Argument.Get_Parameter);
    else
      raise Constraint_Error;
    end if;
  exception
    when others =>
      Basic_Proc.Put_Line_Output ("Wrong argument. Usage : "
      & Argument.Get_Program_Name & " [ <nb_iteration> ]");
      raise Abort_Exception;
  end;

  begin
    One_Cycle;
  exception
    when Storage_Error =>
      Basic_Proc.Put_Line_Output ("The test cannot be performed: "
          & "Even one iteration raises STORAGE_ERROR.");
      Basic_Proc.Put_Line_Output (
            "Lower MAX_DATA in source file, recompile and retry.");
      raise Abort_Exception;
  end;

  Basic_Proc.Put_Line_Output (
        "This test succeeds if no STORAGE_ERROR is raised.");
  Basic_Proc.Put_Output ("Performinig ");
  if Nb_Loop = 0 then
    Basic_Proc.Put_Output (" infinite");
  else
    Basic_Proc.Put_Output (Natural'Image(Nb_Loop));
  end if;
  Basic_Proc.Put_Line_Output (" iterations, each of them consisting in");
  Basic_Proc.Put_Line_Output (" allocating "
                      & Integer'Image(Max_Data)
                      & " objects of "
                      & Integer'Image(Data'Last)
                      & " bytes then freeing them.");


  if Nb_Loop = 0 then
    loop
      One_Cycle;
    end loop;
  else
    for I in 1 .. Nb_Loop loop
      One_Cycle;
    end loop;
  end if;

  Basic_Proc.Put_Line_Output ("Test successful.");
exception
  when Storage_Error =>
    Basic_Proc.Put_Line_Output ("Test has failed.");
  when Abort_Exception =>
    null;
  when others =>
    Basic_Proc.Put_Line_Output ("Unexpected exception was raised.");
    raise;
end T_Dd;

