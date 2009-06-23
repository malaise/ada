with Ada.Text_Io;
with Mutex_Manager, Mixed_Str;
procedure T_Recursive_Mutex is

  M : Mutex_Manager.Mutex (Mutex_Manager.Simple, Recursive => True);
  B : Boolean;

  task T is
    entry Start;
    entry Stop;
  end T;


  task body T is
  begin
    accept Start;
    loop
      Ada.Text_Io.Put_Line ("   Task gets mutex");
      M.Get;
      Ada.Text_Io.Put_Line ("   Task has got mutex");
      delay 3.0;
      Ada.Text_Io.Put_Line ("   Task releases mutex");
      M.Release;
      select
        accept Stop;
        exit;
      or
        delay 3.0;
      end select;
    end loop;
  end T;

begin
  -- Get mutex twice
  Ada.Text_Io.Put_Line ("Getting mutex");
  M.Get;
  Ada.Text_Io.Put_Line ("Getting mutex");
  M.Get;

  -- Start task and get mutex a third time
  Ada.Text_Io.Put_Line ("Starting task");
  T.Start;
  Ada.Text_Io.Put_Line ("Getting mutex");
  M.Get;
  Ada.Text_Io.New_Line;

  -- Release fully mutex (task takes it)
  Ada.Text_Io.Put_Line ("Releasing fully mutex");
  M.Release(Fully => True);

  -- Get mutex => Ko, Ko... Ok
  loop
    Ada.Text_Io.Put_Line ("Trying to get mutex");
    B := M.Get (1.0);
    Ada.Text_Io.Put_Line ("Result is " & Mixed_Str (B'Img));
    exit when B;
  end loop;
  Ada.Text_Io.New_Line;

  -- Get mutex a second time then release twice
  Ada.Text_Io.Put_Line ("Getting mutex and waiting 2s");
  M.Get;
  delay 2.0;
  Ada.Text_Io.Put_Line ("Releasing mutex twice");
  M.Release;
  M.Release;

  -- The end
  Ada.Text_Io.Put_Line ("Stopping task");
  T.Stop;
  Ada.Text_Io.Put_Line ("Done");

end T_Recursive_Mutex;

