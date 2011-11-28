with My_Io, Normalization;
procedure T_Normalization is
  F : Float;
  type Delt_Range is delta 0.00001 digits 13 range -99_999_999.99999 .. 99_999_999.99999;
  D : Delt_Range;
  function Normal_Delt is new Normalization.Normal_Delt_Dig (Delt_Range);

  Len : Positive;
  Exp : Positive;
begin

  loop
    loop
      begin
        My_Io.Put ("F ? : "); My_Io.Get (F);
        exit;
      exception
        when others => null;
      end;
    end loop;
    loop
      begin
        My_Io.Put ("Len ? : "); My_Io.Get (Len);
        exit;
      exception
        when others => null;
      end;
    end loop;
    loop
      begin
        My_Io.Put ("Exp/Fore ? : "); My_Io.Get (Exp);
        exit;
      exception
        when others => null;
      end;
    end loop;

    My_Io.Put_Line ("0         1         2         3         4         5");
    My_Io.Put_Line ("012345678901234567890123456789012345678901234567890");
    begin
      My_Io.Put_Line ('>' & Normalization.Normal_Digits (F, Len, Exp) & '<');
    exception
      when Constraint_Error =>
        My_Io.Put_Line ("Constraint error");
    end;
    begin
      My_Io.Put_Line ('>' & Normalization.Normal_Fixed (F, Len, Exp, '@') & '<');
    exception
      when Constraint_Error =>
        My_Io.Put_Line ("Constraint error");
    end;
    begin
      D := Delt_Range (F);
      My_Io.Put_Line ('>' & Normal_Delt (D, Len, Exp, '@') & '<');
    exception
      when Constraint_Error =>
        My_Io.Put_Line ("Constraint error");
    end;
    My_Io.New_Line;
  end loop;
end T_Normalization;

