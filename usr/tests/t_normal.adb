with Basic_Proc, Normal, Get_Int;
procedure T_Normal is
  I : Integer;
  Len : Positive;
  Right : Boolean;
  Gap : Character;
begin

  Main:
  loop
    loop
      Basic_Proc.Put_Output ("I ? : ");
      begin
        I := Get_Int (Basic_Proc.Get_Line);
        exit;
      exception
        when Basic_Proc.End_Error =>
          exit Main;
        when others => null;
      end;
    end loop;
    loop
      Basic_Proc.Put_Output ("LEN ? : ");
      begin
        Len := Get_Int (Basic_Proc.Get_Line);
        exit;
      exception
        when others => null;
      end;
    end loop;
    loop
      Basic_Proc.Put_Output ("RIGHT ? : ");
      begin
        Right := Boolean'Value (Basic_Proc.Get_Line);
        exit;
      exception
        when others => null;
      end;
    end loop;
    loop
      Basic_Proc.Put_Output ("GAP ? : ");
      declare
        Str : constant String := Basic_Proc.Get_Line;
      begin
        if Str'Length = 1 then
          Gap := Str (1);
          exit;
        end if;
      exception
        when others => null;
      end;
    end loop;

    Basic_Proc.Put_Line_Output ("0         1         2         3         4         5");
    Basic_Proc.Put_Line_Output ("012345678901234567890123456789012345678901234567890");
    Basic_Proc.Put_Line_Output ('>' & Normal (I, Len, Right, Gap) & '<');
    Basic_Proc.New_Line_Output;
  end loop Main;

end T_Normal;

