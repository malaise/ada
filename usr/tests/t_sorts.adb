with My_Io, Sorts, Rnd, Normal;
use My_Io;
procedure T_Sorts is

  subtype Index is Integer range 1 .. 25;

  type Arr is array (Index range <>) of Integer;

  subtype D_Arr is Arr (Index);

  Last : Natural range 0 .. Index'Last;
  Init, Res_Bul, Res_Tas, Res_Rap : D_Arr;

  package Tab_Sorts is new Sorts (Integer, Index, "<", Arr);
  Ok : Boolean;
  Current_Sort : String (1 .. 3);
begin

  loop

    -- init of init and results
    Last := Rnd.Int_Random (0, Index'Last);
    for I in 1 .. Last loop
      Init(I) := Rnd.Int_Random(0, 99);
    end loop;
    Res_Bul(1 .. Last) := Init(1..Last);
    Res_Tas(1 .. Last) := Init(1..Last);
    Res_Rap(1 .. Last) := Init(1..Last);

    Current_Sort := "BUL";
    Tab_Sorts.Bubble_Sort (Res_Bul(1..Last));
    Current_Sort := "TAS";
    Tab_Sorts.Heap_Sort   (Res_Tas(1..Last));
    Current_Sort := "RAP";
    Tab_Sorts.Quick_Sort  (Res_Rap(1..Last));
    Current_Sort := "   ";

    Ok := True;
    for I in 1 .. Last loop
      if Res_Bul(I) /= Res_Tas(I) or else Res_Bul(I) /= Res_Rap(I) then
        Ok := False;
        exit;
      end if;
    end loop;

    if not Ok then
      My_Io.Put_Line ("ERROR:");

      My_Io.Put_Line ("      INIT    BUBBLE      HEAP     QUICK");
      for I in 1 .. Last loop
        My_Io.Put (Normal(Init(I),    10));
        My_Io.Put (Normal(Res_Bul(I), 10));
        My_Io.Put (Normal(Res_Tas(I), 10));
        My_Io.Put (Normal(Res_Rap(I), 10));
        My_Io.New_Line;
      end loop;
      exit;
    else
      for I in 1 .. Last loop
        My_Io.Put (Normal(Init(I), 3));
      end loop;
      My_Io.New_Line;
      for I in 1 .. Last loop
        My_Io.Put (Normal(Res_Rap(I), 3));
      end loop;
      My_Io.Put_Line (" OK");
      My_Io.New_Line;
      delay 1.0;
    end if;

  end loop;
exception
  when others =>
    My_Io.Put_Line ("Exception when sorting with " & Current_Sort & " on:");
    for I in 1 .. Last loop
      My_Io.Put (Normal(Init(I), 3));
    end loop;
    My_Io.New_Line;
    raise;
end T_Sorts;

