with Common;
package Response is


  procedure New_Code;


  type Color_Array is array (Common.Level_Range range <>) of
   Common.Eff_Color_Range;
  type Color_Rec (Level : Common.Last_Level_Range := Common.Min_Level)
   is record
    Color : Color_Array (1 .. Level);
  end record;

  function Get_Code return Color_Rec;

  type Response_Rec is record
    Placed_Ok : Natural;
    Colors_Ok  : Natural;
  end record;

  function Respond (Propal : Color_Rec) return Response_Rec;

end Response;