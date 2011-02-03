-- Is a point within or outside a polygon
-- Is a polygon crossed (some semgments cross each other)
package Polygon_Mng is

  -- Representation of a point
  type Float_Point_Rec is record
    X : Float;
    Y : Float;
  end record;
  -- Representation of a polygon
  type Float_Points_Array is array (Positive range <>) of Float_Point_Rec;

  -- Representations of a point
  type Int_Point_Rec is record
    X : Integer;
    Y : Integer;
  end record;
  -- Representation of a polygon
  type Int_Points_Array is array (Positive range <>) of Int_Point_Rec;

  -- Relationships between points and areas
  type Belonging_Results is (Out_Of_Area, Summit, Boundary, Inside_Area);

  procedure Belong_To_Area
      (Polygon        : in Float_Points_Array;
       Point_To_Check : in Float_Point_Rec;
       Accuracy       : in Float;
       Result         : out Belonging_Results);

  function Is_Crossed (Polygon : Float_Points_Array) return Boolean;

  procedure Belong_To_Area
      (Polygon        : in Int_Points_Array;
       Point_To_Check : in Int_Point_Rec;
       Accuracy       : in Float;
       Result         : out Belonging_Results);

  function Is_Crossed (Polygon : Int_Points_Array) return Boolean;

  Not_A_Polygon : exception;

end Polygon_Mng;

