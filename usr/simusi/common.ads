package Common is

  -- Max number of lines
  Max_Line : constant := 1_000;
  -- 2 lines -> 1 cote
  Max_Cote : constant := Max_Line - 1;


  subtype Line_Range is Positive range 1 .. Max_Line;
  subtype Cote_Range is Positive range 1 .. Max_Cote;

  subtype Pos_Float is Float range 0.0 .. Float'Last;

  type Cote_Kind is (Manufa, Design);

  type Cote_Rec (Kind : Cote_Kind) is record
    -- Lines of cote
    Start, Stop : Line_Range;
    -- Interval of cote
    Inter    : Pos_Float;
    case Kind is
      when Manufa =>
        null;
      when Design =>
        Value : Pos_Float;
    end case;
  end record;

  subtype Manufa_Cote_Rec is Cote_Rec(Manufa);
  subtype Design_Cote_Rec is Cote_Rec(Design);


  type Manufa_Array is array (Cote_Range range <>) of Manufa_Cote_Rec;
  type Design_Array is array (Cote_Range range <>) of Design_Cote_Rec;
end Common;

