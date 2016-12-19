with As.U;
with Utils;
package Fleet is

  -- List of ships
  type Ship_List is (Carrier, Battleship, Cruiser, Sub1, Sub2);
  subtype Ship_Kind_List is Ship_List range Carrier .. Sub1;
  Ship_Names : array (Ship_List) of As.U.Asu_Us;

  -- Length of a ship
  subtype Ship_Len_Range is Positive range 1 .. 5;
  Length : constant array (Ship_List) of Ship_Len_Range := (
      Carrier => 5,
      Battleship => 4,
      Cruiser => 3,
      Sub1 | Sub2 => 2);

  -- Position of our ships
  type Pos_List is array (Ship_Len_Range) of Utils.Coord;
  My_Ships : array (Ship_List) of Pos_List;

end Fleet;

