with Ada.Text_Io;

package body Fl_Get is

  function Parse_Minutes (Str : String) return Fl_Time.Minutes_Range is
  begin
    if Str'Length = 1 then
      raise Error;
    else
      return Fl_Time.Minutes_Range'Value (Str);
    end if;
  end Parse_Minutes;

  function Get_Time return Fl_Time.Time_Type is
    Max_Hour_Dig : constant := 9; -- Fl_Time.Hours_Range'Width;
    Str : String(1 .. Max_Hour_Dig + 2 + 1);
    F, L, P :  Natural;
    Time : Fl_Time.Time_Type := (True, 0, 0);
  begin
    Ada.Text_Io.Put ('>');
    Ada.Text_Io.Get_Line (Str, L);
    if L = 0 then raise Error; end if;
    if L = 1 then
      if Str(1) = '.' then raise Error; end if;
      if Str(1) = '-' then raise Error; end if;
      if Str(1) = 'x' or else Str(1) = 'X' then raise Quit;  end if;
      if Str(1) = 'q' or else Str(1) = 'Q' then raise Quit;  end if;
      if Str(1) = 'c' or else Str(1) = 'C' then raise Clear; end if;
    end if;

    if Str(1) = '-' then
      Time.Positiv := False;
      F := 2;
    else
      Time.Positiv := True;
      F := 1;
    end if;

    P := 0;
    for I in F .. L loop
      case Str(I) is
        when '0'..'9' =>
          null;
        when '.' =>
          if P = 0 then
            P := I;
          else
            raise Error;
          end if;
        when others =>
          raise Error;
      end case;
    end loop;

    if P = 0 then
      -- no dot : only hours
      Time.Hours := Fl_Time.Hours_Range'Value (Str (F .. L));
      Time.Minutes := 0;
    elsif P = L then
      -- dot at last : only hours
      Time.Hours := Fl_Time.Hours_Range'Value (Str ( F.. P - 1));
      Time.Minutes := 0;
    elsif P = F then
      -- dot as first : only minutes
      Time.Hours := 0;
      Time.Minutes := Parse_Minutes (Str (P + 1 .. L));
    else
      -- dot somewhere
      Time.Hours := Fl_Time.Hours_Range'Value (Str (F .. P - 1));
      Time.Minutes := Parse_Minutes (Str (P + 1 .. L));
    end if;

    return Time;

  exception
    when Quit | Clear => raise;
    when others => raise Error;
  end Get_Time;

end Fl_Get;

