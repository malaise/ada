package body Fleet is

  function Length (Ship : Ship_List) return Ship_Len_Range is
  begin
    case Ship is
      when Carrier => return 5;
      when Battleship => return 4;
      when Cruiser => return 3;
      when Sub1 | Sub2 => return 2;
    end case;

  end Length;

end Fleet;

