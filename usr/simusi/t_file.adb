-- Reads (checks) a pair of simusi data files

with Normalization, Basic_Proc;
use Basic_Proc;

with Common, Data;
use Common, Data;

procedure T_File is
begin

    for I in Manufas'Range loop
      Put_Output (Normalization.Normal_Int (I, 3) & " : "
                & Normalization.Normal_Int (Manufas(I).Start, 3) & " --> "
                & Normalization.Normal_Int (Manufas(I).Stop, 3));
      Put_Output (" +- ");
      Put_Line_Output (Normalization.Normal_Fixed (Manufas(I).Inter, 8, 3));
    end loop;
    for I in Designs'Range loop
      Put_Output (Normalization.Normal_Int (I, 3) & " : "
                & Normalization.Normal_Int (Designs(I).Start, 3) & " --> "
                & Normalization.Normal_Int (Designs(I).Stop, 3) );
      Put_Output (" =  ");
      Put_Output (Normalization.Normal_Fixed (Designs(I).Value, 8, 3));
      Put_Output (" +- ");
      Put_Line_Output (Normalization.Normal_Fixed (Designs(I).Inter,7, 3));
    end loop;

end T_File;

