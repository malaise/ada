with Ada.Io_Exceptions;
with My_Io, Normal;
procedure T_Normal is
  I : Integer;
  Len : Positive;
  Right : Boolean;
  Gap : Character;
begin

  Main:
  loop
    loop
      begin
        My_Io.Put ("I ? : "); My_Io.Get (I);
        exit;
      exception
        when Ada.Io_Exceptions.End_Error =>
          exit Main;
        when others => null;
      end;
    end loop;
    loop
      begin
        My_Io.Put ("LEN ? : "); My_Io.Get (Len);
        exit;
      exception
        when others => null;
      end;
    end loop;
    loop
      begin
        My_Io.Put ("RIGHT ? : "); My_Io.Get (Right);
        exit;
      exception
        when others => null;
      end;
    end loop;
    loop
      begin
        My_Io.Put ("GAP ? : "); My_Io.Get (Gap);
        exit;
      exception
        when others => null;
      end;
    end loop;

    My_Io.Put_Line ("0         1         2         3         4         5");
    My_Io.Put_Line ("012345678901234567890123456789012345678901234567890");
    My_Io.Put_Line ('>' & Normal (I, Len, Right, Gap) & '<');
    My_Io.New_Line;
  end loop Main;

end T_Normal;
