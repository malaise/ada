with Argument, Basic_Proc, File_Hash, Images;
procedure T_File_Hash is
  -- Allocate list on Heap i.o. stack
  type List_Access is access File_Hash.List_Mng.List_Type;
  List_Acc : constant List_Access := new File_Hash.List_Mng.List_Type;
  List : File_Hash.List_Mng.List_Type renames List_Acc.all;
  Line : File_Hash.Line_Rec;
  Found : Boolean;
begin

  if Argument.Get_Nbre_Arg = 0 then
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
                             & " <file> [ { <word> } ]");
    Basic_Proc.Set_Error_Exit_Code;
    return;
  end if;

  File_Hash.Load (Argument.Get_Parameter (1), List);

  for I in 2 .. Argument.Get_Nbre_Arg loop
    Argument.Get_Parameter (Line.Txt, Occurence => I);
    Basic_Proc.Put_Line_Output (Line.Txt.Image & " -> ");
    List.Search_First (Line, Found);
    if Found then
     loop
        List.Read_Current (Line);
        Basic_Proc.Put_Line_Output ("FOUND at line "
                                  & Images.Llint_Image (Line.No)
                                  & " >" & Line.Txt.Image & "<");
        List.Search_Next (Line, Found);
        exit when not Found;
      end loop;
    else
      Basic_Proc.Put_Line_Output ("NOT FOUND");
    end if;
  end loop;

exception
  when File_Hash.Init_Error =>
    Basic_Proc.Put_Line_Error ("ERROR: File "
                             & Argument.Get_Parameter (Occurence => 1)
                             & " not found.");
    Basic_Proc.Set_Error_Exit_Code;
end T_File_Hash;

