with Ada.Text_Io;

with Normal;
with Argument;
with My_Io;

with Types;
with File;

procedure T_File is
  Dim : Natural;

begin
  if Argument.Get_Nbre_Arg /= 1 then
    Ada.Text_Io.Put_Line ("Syntax error. Usage : t_file <file_name>");
    return;
  end if;

  declare
    Mattrix : Types.Mattrix_Rec := File.Read (Argument.Get_Parameter);
  begin


    Ada.Text_Io.Put_Line ("Kind is " & Types.Mattrix_Kind_List'Image(File.Get_Kind));
    Dim := Mattrix.Dim;

    for I in 1 .. Mattrix.Dim loop
      for J in 1 .. Mattrix.Dim loop
        Ada.Text_Io.Put (Normal(Mattrix.Notes(I, J), 5) & " ");
      end loop;
      Ada.Text_Io.New_Line;
    end loop;
  end;


  for I in 1 .. Dim loop
    for J in 1 .. Dim loop
      My_Io.Put (File.Get_Note(I, J)); My_Io.Put(" ");
    end loop;
    Ada.Text_Io.New_Line;
  end loop;

exception
  when File.Read_Error =>
    null;
end T_File;

