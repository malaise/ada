package body Polygon_Mng is

  procedure Belong_To_Area (Polygon        : in Float_Points_Array;
                            Point_To_Check : in Float_Point_Rec;
                            Accuracy       : in Float;
                            Result         : out Belonging_Results) is


    Isi, Isf, Isc          : Integer;
    A1, A2, A3, B1, B2, B3 : Float;
    P, C, E1, E2           : Float;
    D, Y1                  : Float;
    Accuracy_2             : Float;
    Iti                    : Integer;
    Temp_Result            : Belonging_Results;

  begin
    -- At least three points are required to define a polygon
    if Polygon'Length < 3 then
      raise Not_A_Polygon;
    end if;

    -- Check to see if the point is inside or outside of the polygon
    Temp_Result := Out_Of_Area;
    Iti := 0;
    Accuracy_2 := Accuracy ** 2;
    Isf := 0;
    A1 := Polygon(Polygon'First).X - Point_To_Check.X;
    B1 := Polygon(Polygon'First).Y - Point_To_Check.Y;
    if A1 > 0.0 then
      Isf := 1;
    elsif A1 < 0.0 then
      Isf := -1;
    end if;
    Isi := Isf;
    Isc := Isf;
    for I in Polygon'Range loop
      if I /= Polygon'Last then
        A2 := Polygon(I + 1).X - Point_To_Check.X;
        B2 := Polygon(I + 1).Y - Point_To_Check.Y;
      else
        -- last segment : the second point of the segment is the first point of
        --                the polygon
        A2 := Polygon(Polygon'First).X - Point_To_Check.X;
        B2 := Polygon(Polygon'First).Y - Point_To_Check.Y;
      end if;
      A3 := A2 - A1;
      B3 := B2 - B1;
      P := A2 * B1 - A1 * B2;
      C := A3 ** 2 + B3 ** 2;
      E1 := A1 ** 2 + B1 ** 2;
      E2 := A2 ** 2 + B2 ** 2;
      if E1 <= Accuracy_2 or else E2 <= Accuracy_2 then
        Temp_Result := Summit;
        exit;
      end if;
      if C + E1 > E2 and then C + E2 > E1 then
        D := P ** 2 / C;
        if D <= Accuracy_2 then
          Temp_Result := Boundary;
          exit;
        end if;
      end if;
      if A2 > 0.0 then
        Isf := 1;
      elsif A2 < 0.0 then
        Isf := -1;
      end if;
      --
      if Isi = 0 then
        Isi := Isf;
      end if;
      A1 := A2;
      B1 := B2;
      if Isc /= Isf then
        if Isc /= 0 then
          Y1 := P / A3;
          if Y1 > 0.0 then
            Iti := Iti + 1;
          end if;
        end if;
        Isc := Isf;
      end if;

    end loop;

    if Temp_Result = Out_Of_Area then
      if Isf /= Isi then
        if B2 > 0.0 then
          Iti := Iti + 1;
        end if;
      end if;
      if Iti /= Iti / 2 * 2 then
        Temp_Result := Inside_Area;
      end if;
    end if;
    Result := Temp_Result;

  end Belong_To_Area;

  function "-" (Pl, Pr : Float_Point_Rec) return Float_Point_Rec is
  begin
    return (X => Pl.X - Pr.X, Y=> Pl.Y - Pr.Y);
  end "-";

  procedure Segments_Intersect (Ps1, Qs1, Ps2, Qs2 : in  Float_Point_Rec;
                                Intersect          :    out Boolean;
                                I_Point            :    out Float_Point_Rec) is

    Det                   : Float;
    Coef1, Coef2          : Float;
    Vect1, Vect2, Vectq12 : Float_Point_Rec;

  begin
    Vect1 := Ps1 - Qs1;
    Vect2 := Ps2 - Qs2;
    I_Point := (X => 0.0, Y => 0.0);
    Det := Vect1.Y * Vect2.X - Vect1.X * Vect2.Y;

    -- If the determinant is null (segments parallel)
    if abs Det < 0.5 then
      Intersect := False;
    else
      Vectq12 := Qs1 - Qs2;
      Coef1 := (Vect2.Y * Vectq12.X - Vect2.X * Vectq12.Y) / Det;
      Coef2 := (Vect1.Y * Vectq12.X - Vect1.X * Vectq12.Y) / Det;
      if       Coef1 < 1.0 and then Coef1 > 0.0
      and then Coef2 < 1.0 and then Coef2 > 0.0 then
        Intersect := True;
        I_Point.X := Vect1.X * Coef1 + Qs1.X;
        I_Point.Y := Vect1.Y * Coef1 + Qs1.Y;
      else
        Intersect := False;
      end if;
    end if;

  end Segments_Intersect;

  function Is_Crossed (Polygon : Float_Points_Array) return Boolean is

    Intersect : Boolean;
    I_Point   : Float_Point_Rec;

  begin
    -- At least three points are required to define a polygon
    if Polygon'Length < 3 then
      raise Not_A_Polygon;
    end if;

    for Next_Point_Nb in Polygon'First + 2 .. Polygon'Last - 1 loop
      for Previous_Point_Nb in Polygon'First .. Next_Point_Nb - 2 loop
        Segments_Intersect (Ps1       => Polygon(Next_Point_Nb),
                            Qs1       => Polygon(Next_Point_Nb + 1),
                            Ps2       => Polygon(Previous_Point_Nb),
                            Qs2       => Polygon(Previous_Point_Nb + 1),
                            Intersect => Intersect,
                            I_Point   => I_Point);
        if Intersect then
          return True;
        end if;
      end loop;
    end loop;

    for Point_Nb in Polygon'First + 1 .. Polygon'Last - 2 loop
      Segments_Intersect (Ps1       => Polygon(Polygon'Last),
                          Qs1       => Polygon(Polygon'First),
                          Ps2       => Polygon(Point_Nb),
                          Qs2       => Polygon(Point_Nb + 1),
                          Intersect => Intersect,
                          I_Point   => I_Point);
      if Intersect then
        return True;
      end if;
    end loop;

    return False;

  end Is_Crossed;

  function To_Float (Point : Int_Point_Rec) return Float_Point_Rec is
  begin
    return (X => Float(Point.X), Y => Float(Point.Y));
  end To_Float;

  procedure To_Float (Int_Polygon : in Int_Points_Array; Float_Polygon : out Float_Points_Array) is
    J : Positive;
  begin
    for I in Int_Polygon'Range loop
      J := I - Int_Polygon'First + Float_Polygon'First;
      Float_Polygon (J) := To_Float(Int_Polygon(I));
    end loop;
  end To_Float;

  procedure Belong_To_Area (Polygon        : in Int_Points_Array;
                            Point_To_Check : in Int_Point_Rec;
                            Accuracy       : in Float;
                            Result         : out Belonging_Results) is
    Float_Polygon : Float_Points_Array(1 .. Polygon'Length);
    Float_Point_To_Check : Float_Point_Rec;
  begin
    To_Float (Polygon, Float_Polygon);
    Float_Point_To_Check := To_Float (Point_To_Check);
    Belong_To_Area (Float_Polygon, Float_Point_To_Check, Accuracy, Result);
  end  Belong_To_Area;

  function Is_Crossed (Polygon : Int_Points_Array) return Boolean is
    Float_Polygon : Float_Points_Array(1 .. Polygon'Length);
  begin
    To_Float (Polygon, Float_Polygon);
    return Is_Crossed (Float_Polygon);
  end Is_Crossed;

end Polygon_Mng;

