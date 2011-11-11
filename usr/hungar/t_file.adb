with Normal, Argument, Basic_Proc;
with Types, File;

procedure T_File is
  Dim : Natural;

begin
  if Argument.Get_Nbre_Arg /= 1 then
    Basic_Proc.Put_Line_Error ("Syntax error. Usage : t_file <file_name>");
    return;
  end if;

  declare
    Mattrix : constant Types.Mattrix_Rec := File.Read (Argument.Get_Parameter);
  begin


    Basic_Proc.Put_Line_Output ("Kind is "
                              & Types.Mattrix_Kind_List'Image(File.Get_Kind));
    Dim := Mattrix.Dim;

    for I in 1 .. Mattrix.Dim loop
      for J in 1 .. Mattrix.Dim loop
        Basic_Proc.Put_Output (Normal(Mattrix.Notes(I, J), 5) & " ");
      end loop;
      Basic_Proc.New_Line_Output;
    end loop;
  end;


  for I in 1 .. Dim loop
    for J in 1 .. Dim loop
      Basic_Proc.Put_Output (File.Get_Note(I, J)'Img);
      Basic_Proc.Put_Output (" ");
    end loop;
    Basic_Proc.New_Line_Output;
  end loop;

exception
  when File.Read_Error =>
    null;
end T_File;

