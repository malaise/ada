-- Reads (checks) a pair of simusi data files

with NORMAL, MY_IO;
use MY_IO;

with COMMON, DATA;
use COMMON, DATA;

procedure T_FILE is
begin

    for I in MANUFAS'RANGE loop
      PUT (NORMAL(I, 3) & " : " & NORMAL(MANUFAS(I).START, 3) & " --> "
         & NORMAL(MANUFAS(I).STOP, 3));
      PUT (" +- ");
      PUT_LINE (MANUFAS(I).INTER, FORE => 3, AFT => 3, EXP => 0);
    end loop;
    for I in DESIGNS'RANGE loop
      PUT (NORMAL(I, 3) & " : " & NORMAL(DESIGNS(I).START, 3) & " --> "
         & NORMAL(DESIGNS(I).STOP, 3) );
      PUT (" =  ");
      PUT (DESIGNS(I).VALUE, FORE => 4, AFT => 3, EXP => 0);
      PUT (" +- ");
      PUT_LINE (DESIGNS(I).INTER, FORE => 3, AFT => 3, EXP => 0);
    end loop;

end T_FILE;
    
