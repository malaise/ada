with Common;
package Response is

  type Color_Array is array (Common.Full_Level_Range range <>)
                            of Common.Eff_Color_Range;
  Empty : constant Color_Array (1 ..0) := (others => <>);

  -- Set a new code (random by default)
  -- Raises Constraint_Error if the code provided is not compatible with
  -- Common Level
  procedure New_Code (Init : in Color_Array := Empty);

  -- The secret code
  type Color_Rec (Level : Common.Last_Level_Range
                        := Common.Min_Level) is record
    Color : Color_Array (1 .. Level);
  end record;
  function Get_Code return Color_Rec;

  -- The answer to a proposal
  type Response_Rec is record
    Placed_Ok : Natural;
    Colors_Ok  : Natural;
  end record;
  function Respond (Propal : Color_Rec) return Response_Rec;

end Response;

