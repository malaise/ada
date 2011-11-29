with Normalization, Get_Float, Async_Stdin;
procedure T_Normalization is
  F : Float;
  type Delt_Range is delta 0.00001 digits 13
                     range -99_999_999.99999 .. 99_999_999.99999;
  D : Delt_Range;
  function Normal_Delt is new Normalization.Normal_Delt_Dig (Delt_Range);

  Len : Positive;
  Exp : Positive;
begin

  loop
    loop
      begin
        Async_Stdin.Put_Out ("F ? : ");
        F := Get_Float.Get_Float (Async_Stdin.Strip_Last_Control (
              Async_Stdin.Get_Line (80, 7)));
        exit;
      exception
        when others => null;
      end;
    end loop;
    loop
      begin
        Async_Stdin.Put_Out ("Len ? : ");
        Len := Positive'Value (Async_Stdin.Strip_Last_Control (
            Async_Stdin.Get_Line (80, 9)));
        exit;
      exception
        when others => null;
      end;
    end loop;
    loop
      begin
        Async_Stdin.Put_Out ("Exp/Fore ? : ");
        Exp := Positive'Value (Async_Stdin.Strip_Last_Control (
            Async_Stdin.Get_Line (80, 14)));
        exit;
      exception
        when others => null;
      end;
    end loop;

    Async_Stdin.Put_Line_Out ("0         1         2         3         4         5");
    Async_Stdin.Put_Line_Out ("012345678901234567890123456789012345678901234567890");
    begin
      Async_Stdin.Put_Line_Out ('>' & Normalization.Normal_Digits (F, Len, Exp) & '<');
    exception
      when Constraint_Error =>
        Async_Stdin.Put_Line_Out ("Constraint error");
    end;
    begin
      Async_Stdin.Put_Line_Out ('>' & Normalization.Normal_Fixed (F, Len, Exp, '@') & '<');
    exception
      when Constraint_Error =>
        Async_Stdin.Put_Line_Out ("Constraint error");
    end;
    begin
      D := Delt_Range (F);
      Async_Stdin.Put_Line_Out ('>' & Normal_Delt (D, Len, Exp, '@') & '<');
    exception
      when Constraint_Error =>
        Async_Stdin.Put_Line_Out ("Constraint error");
    end;
    Async_Stdin.New_Line_Out;
  end loop;
end T_Normalization;

