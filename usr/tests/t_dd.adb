with Ada.Text_Io;
with Argument, Dyn_Data;

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
      Ada.Text_Io.Put_Line ("Wrong argument. Usage : "
      & Argument.Get_Program_Name & " [ <nb_iteration> ]");
      raise Abort_Exception;
  end;

  begin
    One_Cycle;
  exception
    when Storage_Error =>
      Ada.Text_Io.Put_Line ("The test cannot be performed: "
          & "Even one iteration raises STORAGE_ERROR.");
      Ada.Text_Io.Put_Line (
            "Lower MAX_DATA in source file, recompile and retry.");
      raise Abort_Exception;
  end;

  Ada.Text_Io.Put_Line ("This test succeeds if no STORAGE_ERROR is raised.");
  Ada.Text_Io.Put ("Performinig ");
  if Nb_Loop = 0 then
    Ada.Text_Io.Put (" infinite");
  else
    Ada.Text_Io.Put (Natural'Image(Nb_Loop));
  end if;
  Ada.Text_Io.Put_Line (" iterations, each of them consisting in");
  Ada.Text_Io.Put_Line (" allocating "
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

  Ada.Text_Io.Put_Line ("Test successful.");
exception
  when Storage_Error =>
    Ada.Text_Io.Put_Line ("Test has failed.");
  when Abort_Exception =>
    null;
  when others =>
    Ada.Text_Io.Put_Line ("Unexpected exception was raised.");
    raise;
end T_Dd;

