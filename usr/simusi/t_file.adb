-- Reads (checks) a pair of simusi data files

with Normal, My_Io;
use My_Io;

with Common, Data;
use Common, Data;

procedure T_File is
begin

    for I in Manufas'Range loop
      Put (Normal(I, 3) & " : " & Normal(Manufas(I).Start, 3) & " --> "
         & Normal(Manufas(I).Stop, 3));
      Put (" +- ");
      Put_Line (Manufas(I).Inter, Fore => 3, Aft => 3, Exp => 0);
    end loop;
    for I in Designs'Range loop
      Put (Normal(I, 3) & " : " & Normal(Designs(I).Start, 3) & " --> "
         & Normal(Designs(I).Stop, 3) );
      Put (" =  ");
      Put (Designs(I).Value, Fore => 4, Aft => 3, Exp => 0);
      Put (" +- ");
      Put_Line (Designs(I).Inter, Fore => 3, Aft => 3, Exp => 0);
    end loop;

end T_File;

