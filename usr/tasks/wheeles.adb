with My_Io;
procedure Wheeles is

  Nb_Wheeles : constant := 3;
  subtype Wheeles_Range is Positive range 1 .. Nb_Wheeles;

  task type T_Wheele is
    entry Start (No : in Wheeles_Range);
    entry Stop;
  end T_Wheele;

  Wheeles_Array : array (Wheeles_Range) of T_Wheele;

  task body T_Wheele is
    No : Wheeles_Range;
  begin
    accept Start (No : in Wheeles_Range) do
      T_Wheele.No := No;
      My_Io.Put_Line ("Task " & Wheeles_Range'Image(No) & " is starting.");
    end Start;
    My_Io.Put_Line ("Task " & Wheeles_Range'Image(No) & " is started.");

    loop
      My_Io.Put_Line ("Task " & Wheeles_Range'Image(No) & " is running.");
      select
        accept Stop do
          My_Io.Put_Line ("Task " & Wheeles_Range'Image(No) & " is exiting.");
        end Stop;
        exit;
      or
        delay 0.01;
      end select;
    end loop;
    My_Io.Put_Line ("Task " & Wheeles_Range'Image(No) & " is exited.");
  end T_Wheele;


begin
  My_Io.Put_Line ("Starting tasks.");
  for I in Wheeles_Range loop
    Wheeles_Array(I).Start(I);
  end loop;

  loop
    declare
      Str : String (1 .. 132);
      Len : Natural;
    begin
      delay 1.0;
      My_Io.Get_Line (Str, Len);
      exit when Str(1..Len) = "exit";
    exception
      when others => null;
    end;
  end loop;
  My_Io.Put_Line ("Stopping tasks.");

  for I in Wheeles_Range loop
    Wheeles_Array(I).Stop;
  end loop;
  My_Io.Put_Line ("Exiting.");
end Wheeles;
