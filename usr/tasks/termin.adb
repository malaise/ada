-- Dependancy between tasks
with Protected_Put;
procedure Termin is
  use Protected_Put;

  type T_Creation is (By_Access, Direct);

  Loops : constant array (T_Creation) of Positive := (4, 2);

  -- priorite du programme principal
  pragma Priority (5);

  -- Kind of task
  task type T_Task is
    entry Init (Name : in T_Creation);
    pragma Priority (1);
  end T_Task;

  type T_Ptr_Task is not null access T_Task;

  Separator : constant String := "-------------";

  task body T_Task is
    Name : T_Creation;
  begin
    accept Init (Name : in T_Creation) do
      T_Task.Name := Init.Name;
    end Init;
    Put_Line_Output ("task " & Name'Img & " started");
    for I in 1..Loops(Name) loop
      delay 0.5;
      Put_Line_Output ("task " & Name'Img & " active");
    end loop;
    Put_Line_Output ("task " & Name'Img & " terminated");
  end T_Task;

begin
  Put_Line_Output ("A task depends from the bloc in which it is declared,"
    & " except if it is accessed,");
  Put_Line_Output (" in which case it depends from the bloc declaring the access type:");
  New_Line_Output;

  declare
    Pa : constant T_Ptr_Task := new T_Task;
    Pb : T_Task;
  begin
    Put_Line_Output (Separator & " Start bloc " & Separator);
    Pa.Init (By_Access);
    Pb.Init (Direct);
  end;
  Put_Line_Output (Separator & " End bloc " & Separator);

end Termin;

