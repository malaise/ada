package body My_Io is

  Error_Message  : constant String := "WRONG FORMAT. Try again";
  Separe_Message : constant String := " : ";

  procedure Put_Line(Item  : in Boolean;
                     Width : in Text_Io.Field := Bool_Io.Default_Width;
                     Set   : in Text_Io.Type_Set := Bool_Io.Default_Setting) is
  begin
    Bool_Io.Put(Item, Width, Set);
    Text_Io.New_Line;
  end Put_Line;

  procedure Put_Line(Item : in Character) is
  begin
    Text_Io.Put(Item);
    Text_Io.New_Line;
  end Put_Line;

  procedure Put_Line(Item  : in Integer;
                     Width : in Text_Io.Field := Int_Io.Default_Width;
                     Base  : in Text_Io.Number_Base := Int_Io.Default_Base) is
  begin
    Int_Io.Put(Item, Width, Base);
    Text_Io.New_Line;
  end Put_Line;

  procedure Put_Line(Item  : in Long_Long_Integer;
                     Width : in Text_Io.Field := Long_Io.Default_Width;
                     Base  : in Text_Io.Number_Base := Long_Io.Default_Base) is
  begin
    Long_Io.Put(Item, Width, Base);
    Text_Io.New_Line;
  end Put_Line;

  procedure Put_Line(Item : in Float;
                     Fore : in Text_Io.Field := Flo_Io.Default_Fore;
                     Aft  : in Text_Io.Field := Flo_Io.Default_Aft;
                     Exp  : in Text_Io.Field := Default_Exp) is
  begin
    Flo_Io.Put(Item, Fore, Aft, Exp);
    Text_Io.New_Line;
  end Put_Line;


  procedure Safe_Get(Prompt : in String;
                     Item   : out Integer) is
    Str                       : String(1 .. Integer'Width);
    Last_Read, Last_Converted : Natural;
  begin
    Put(Prompt & Separe_Message);
    loop
      begin
        Text_Io.Get_Line(Str, Last_Read);
        Int_Io.Get(Str(1 .. Last_Read), Item, Last_Converted);
        if Last_Converted /= Last_Read then
          raise Text_Io.Data_Error;
        end if;
        return;
      exception
        when others =>
          Put(Error_Message & Separe_Message);
      end;
    end loop;
  end Safe_Get;

  procedure Safe_Get(Prompt : in String;
                     Item   : out Long_Long_Integer) is
    Str                       : String(1 .. Long_Long_Integer'Width);
    Last_Read, Last_Converted : Natural;
  begin
    Put(Prompt & Separe_Message);
    loop
      begin
        Text_Io.Get_Line(Str, Last_Read);
        Long_Io.Get(Str(1 .. Last_Read), Item, Last_Converted);
        if Last_Converted /= Last_Read then
          raise Text_Io.Data_Error;
        end if;
        return;
      exception
        when others =>
          Put(Error_Message & Separe_Message);
      end;
    end loop;
  end Safe_Get;

  procedure Safe_Get(Prompt : in String;
                     Item   : out Float) is
    Str                       : String(1 .. 132);
    Last_Read, Last_Converted : Natural;
    Int_Val                   : Long_Long_Integer;
  begin
    Put(Prompt & Separe_Message);
    loop
      begin
        begin
          Text_Io.Get_Line(Str, Last_Read);
          Flo_Io.Get(Str(1 .. Last_Read), Item, Last_Converted);
          if Last_Converted /= Last_Read then
            raise Text_Io.Data_Error;
          end if;
          return;
        exception
          when others =>
            Long_Io.Get(Str(1 .. Last_Read), Int_Val, Last_Converted);
            if Last_Converted /= Last_Read then
              raise Text_Io.Data_Error;
            end if;
            Item := Float(Int_Val);
            return;
        end;
      exception
        when others =>
          Put(Error_Message & Separe_Message);
      end;
    end loop;
  end Safe_Get;

end My_Io;
