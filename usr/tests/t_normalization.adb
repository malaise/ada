with My_Io, Normalization;
procedure T_Normalization is
  F : Float;
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
        My_Io.Put ("Exp/Int ? : "); My_Io.Get (Exp);
        exit;
      exception
        when others => null;
      end;
    end loop;

    My_Io.Put_Line ("0         1          2         3         4         5");
    My_Io.Put_Line ("0123456789012345678980123456789012345678901234567890");
    My_Io.Put_Line ('>' & Normalization.Normal_Digits (F, Len, Exp) & '<');
    My_Io.Put_Line ('>' & Normalization.Normal_Fixed (F, Len, Exp, '@') & '<');
    My_Io.New_Line;
  end loop;
end T_Normalization;

