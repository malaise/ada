with Basic_Proc, Sorts, Rnd, Normal, Key_Pressed, Argument;
procedure T_Sorts is

  subtype Index is Integer range 1 .. 25;

  type Arr is array (Index range <>) of Integer;

  subtype D_Arr is Arr (Index);

  Last : Natural range 0 .. Index'Last;
  Init, Res_Bul, Res_Tas, Res_Rap : D_Arr;

  package Tab_Sorts is new Sorts (Integer, Index, "<", Arr);
  Ok : Boolean;
  Current_Sort : String (1 .. 3);

  Nb_Loops, Id_Loop : Natural;

begin
  Rnd.Gen.Randomize;

  Nb_Loops := 0;
  if Argument.Get_Nbre_Arg = 1 then
    begin
      Nb_Loops := Natural'Value (Argument.Get_Parameter(1));
    exception
      when Constraint_Error =>
        Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
          & " [ <nb_loops> ]");
        Basic_Proc.Set_Error_Exit_Code;
        return;
    end;
  end if;

  Key_Pressed.Open (False);
  Id_Loop := 0;

  loop

    -- Init of init and results
    Last := Rnd.Gen.Int_Random (0, Index'Last);
    for I in 1 .. Last loop
      Init(I) := Rnd.Gen.Int_Random(0, 99);
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
      Basic_Proc.Put_Line_Error ("ERROR:");

      Basic_Proc.Put_Line_Error ("      INIT    BUBBLE      HEAP     QUICK");
      for I in 1 .. Last loop
        Basic_Proc.Put_Error (Normal(Init(I),    10));
        Basic_Proc.Put_Error (Normal(Res_Bul(I), 10));
        Basic_Proc.Put_Error (Normal(Res_Tas(I), 10));
        Basic_Proc.Put_Error (Normal(Res_Rap(I), 10));
        Basic_Proc.New_Line_Error;
      end loop;
      Basic_Proc.Set_Error_Exit_Code;
      exit;
    else
      for I in 1 .. Last loop
        Basic_Proc.Put_Output (Normal(Init(I), 3));
      end loop;
      Basic_Proc.New_Line_Output;
      for I in 1 .. Last loop
        Basic_Proc.Put_Output (Normal(Res_Rap(I), 3));
      end loop;
      Basic_Proc.Put_Line_Output (" OK");
      Basic_Proc.New_Line_Output;
      Basic_Proc.Flush_Output;

      Id_Loop := Id_Loop + 1;
      exit when Id_Loop = Nb_Loops;
      exit when Key_Pressed.Key_Pressed;
      if Nb_Loops = 0 then
        delay 1.0;
        exit when Key_Pressed.Key_Pressed;
      end if;
    end if;

  end loop;

  Key_Pressed.Close;

exception
  when others =>
    Basic_Proc.Put_Line_Error ("Exception when sorting with "
                             & Current_Sort & " on:");
    for I in 1 .. Last loop
      Basic_Proc.Put_Error (Normal(Init(I), 3));
    end loop;
    Basic_Proc.New_Line_Error;
    Key_Pressed.Close;
    raise;
end T_Sorts;

