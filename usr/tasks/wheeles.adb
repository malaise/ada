with Basic_Proc;
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
      Basic_Proc.Put_Line_Output ("Task " & Wheeles_Range'Image(No) & " is starting.");
    end Start;
    Basic_Proc.Put_Line_Output ("Task " & Wheeles_Range'Image(No) & " is started.");

    loop
      Basic_Proc.Put_Line_Output ("Task " & Wheeles_Range'Image(No) & " is running.");
      select
        accept Stop do
          Basic_Proc.Put_Line_Output ("Task " & Wheeles_Range'Image(No) & " is exiting.");
        end Stop;
        exit;
      or
        delay 0.01;
      end select;
    end loop;
    Basic_Proc.Put_Line_Output ("Task " & Wheeles_Range'Image(No) & " is exited.");
  end T_Wheele;


begin
  Basic_Proc.Put_Line_Output ("Starting tasks.");
  for I in Wheeles_Range loop
    Wheeles_Array(I).Start(I);
  end loop;

  loop
    declare
      Str : String (1 .. 132);
      Len : Natural;
    begin
      delay 1.0;
      Basic_Proc.Get_Line (Str, Len);
      exit when Str(1..Len) = "exit";
    exception
      when others => null;
    end;
  end loop;
  Basic_Proc.Put_Line_Output ("Stopping tasks.");

  for I in Wheeles_Range loop
    Wheeles_Array(I).Stop;
  end loop;
  Basic_Proc.Put_Line_Output ("Exiting.");
end Wheeles;
