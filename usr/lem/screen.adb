with Con_Io, My_Math;
package body Screen is

  -- First and Last X on screen for space
  First_X : Con_Io.Graphics.X_Range;
  Last_X : Con_Io.Graphics.X_Range;
  -- First and Last Y on screen for space
  First_Y : Con_Io.Graphics.Y_Range;
  Last_Y : Con_Io.Graphics.Y_Range;

  -- X and Y factor for space to screen conversion
  X_Factor : My_Math.Real;
  Y_Factor : My_Math.Real;

  -- Screen coordinate
  type Coordinate_Rec is record
    X : Con_Io.Graphics.X_Range;
    Y : Con_Io.Graphics.Y_Range;
  end record;

  -- Space to screen conversion
  function X_To_Screen (X_Pos : Space.Position_Range) return Con_Io.Graphics.X_Range is
    use type My_Math.Real;
  begin
    return First_X + Integer(My_Math.Trunc(My_Math.Real(X_Pos) * X_Factor));
  end X_To_Screen;
  function Y_To_Screen (Y_Pos : Space.Position_Range) return Con_Io.Graphics.Y_Range is
    use type My_Math.Real;
  begin
    return First_Y + Integer(My_Math.Trunc(My_Math.Real(Y_Pos) * Y_Factor));
  end Y_To_Screen;
  function To_Screen (Position : Space.Position_Rec)
           return Coordinate_Rec is
    use type My_Math.Real;
  begin
    return (X => X_To_Screen (Position.X_Pos),
            Y => Y_To_Screen (Position.Y_Pos) );
  end To_Screen;

  -- Screen position gauge names (thrust, speeds and fuel)
  Th, Vs, Fu, Hs : Coordinate_Rec;

  -- Reset screen. Display titles and moon ground
  procedure Init (Ground : in Moon.Ground_Array) is
    use Con_Io.Graphics;
    use type My_Math.Real;
  begin
    -- Reset screen
    Con_Io.Init;
    Con_Io.Reset_Term;
    -- Compute space
    -- Space last X leaves "TT VV" (5) for Thrust and V speed
    First_X := 1;
    Last_X := X_Max - Font_Width * 5 - 2;
    -- Space first Y leaves "H" for H speed and F for fuel
    First_Y := Font_Height * 2 + 2;
    Last_Y := Y_Max - 1;
    -- Compute conversion factors
    X_Factor := My_Math.Real (Last_X - First_X) / My_Math.Real (Space.X_Max);
    Y_Factor := My_Math.Real (Last_Y - First_Y) / My_Math.Real (Space.Y_Max);
    -- Compute position of gauge names
    Th := (Last_X + 2, First_Y + 1);
    Vs := (Th.X + Con_Io.Graphics.Font_width * 3, Th.Y);
    Fu := (First_X + 1, First_Y - 2 - Con_Io.Graphics.Font_Height);
    Hs := (Fu.X, Fu.Y - Con_Io.Graphics.Font_Height);
    -- Draw ground
    Refresh (Ground);
  end Init;

  -- Put "constant" info
  procedure Refresh (Ground : in Moon.Ground_Array) is
    J : Positive;
    Screen_Point : Coordinate_Rec;
    Screen_Ground : Con_Io.Natural_Array (1 .. (Ground'Length + 2) * 2);
    use type My_Math.Real;
  begin
    -- Draw ground
    J := 1;
    -- 1, 1
    Screen_Ground(J) := First_X;
    J := J + 1;
    Screen_Ground(J) := First_Y;
    J := J + 1;
    for I in Ground'Range loop
      -- Convert this ground point to screen
      Screen_Point := To_Screen (Ground (I));
      -- Store X then Y
      Screen_Ground(J) := Screen_Point.X;
      J := J + 1;
      Screen_Ground(J) := Screen_Point.Y;
      J := J + 1;
    end loop;
    -- Max, 1
    Screen_Ground(J) := Last_X;
    J := J + 1;
    Screen_Ground(J) := First_Y;
    J := J + 1;
    -- Put ground
    Con_Io.Set_Foreground (Con_Io.Light_Gray);
    Con_Io.Graphics.Fill_Area (Screen_Ground);
    -- Frame
    Con_Io.Set_Foreground (Con_Io.Red);
    Con_Io.Graphics.Draw_Rectangle (First_X - 1, First_Y - 1,
                                    Last_X + 1, Last_Y + 1);
    -- Letters
    Con_Io.Graphics.Put ("Th", Th.X, Th.Y);
    Con_Io.Graphics.Put ("Vs", Vs.X, Vs.Y);
    Con_Io.Graphics.Put ("Fu:", Fu.X, Fu.Y);
    Con_Io.Graphics.Put ("Hs:", Hs.X, Hs.Y);
  end Refresh;

  -- Update lem and its speed and Y_thrust
  procedure Update (Prev_Pos, New_Pos : in Lem_Position;
                    Speed : in Lem.Speed_Rec;
                    Y_Thrust : in Lem.Y_Thrust_Range;
                    Fuel     : in Lem.Fuel_Range) is
    procedure Draw_Lem (Pos : Space.Position_Rec) is
      -- Lower left and upper right of LEM
      -- Bottom and top of left and right feet
      Ll, Ur, Blf, Tlf, Brf, Trf : Coordinate_Rec;
      --   +--------+
      --   |        |
      --   +---+----+
      --     /    \
      --    /      \
      use type My_Math.Real;
      Height : constant Con_Io.Graphics.Y_Range
             := Integer(My_Math.Trunc(My_Math.Real(Lem.Width) / 2.0 * Y_Factor));
      use type Space.Position_Range;
    begin
      -- Compute Lower left and Upper right corners (Pos is LEM center)
      Ll := To_Screen ( (Pos.X_Pos - Lem.Width / 2.0,
                         Pos.Y_Pos) );
      Ur := (X => X_To_Screen (Pos.X_Pos + Lem.Width / 2.0),
             Y => Ll.Y + Height);
      -- Compute left and right legs bottom and top
      Blf := (X => Ll.X,
              Y => Ll.Y - Height);
      Brf := (X => Ur.X,
              Y => Blf.Y);
      Tlf := (X => X_To_Screen (Pos.X_Pos - Lem.Width / 4.0),
              Y => Ll.Y);
      Trf := (X => X_To_Screen (Pos.X_Pos + Lem.Width / 4.0),
              Y => Ll.Y); 
      -- Fill body
      Con_Io.Graphics.Fill_Rectangle (Ll.X, Ll.Y, Ur.X, Ur.Y);
      -- Draw legs
      Con_Io.Graphics.Draw_Line (Tlf.X, Tlf.Y, Blf.X, Blf.Y);
      Con_Io.Graphics.Draw_Line (Tlf.X + 1, Tlf.Y, Blf.X + 1, Blf.Y);
      Con_Io.Graphics.Draw_Line (Trf.X, Trf.Y, Brf.X, Brf.Y);
      Con_Io.Graphics.Draw_Line (Trf.X - 1, Trf.Y, Brf.X - 1, Brf.Y);
    end Draw_Lem;
  begin
    if Prev_Pos.Set then
      -- Hide prev pos
      Con_Io.Set_Foreground (Con_Io.Get_Background);
      Draw_Lem (Prev_Pos.Pos);
    end if;
    if New_Pos.Set then
      -- Show prev pos
      Con_Io.Set_Foreground (Con_Io.Cyan);
      Draw_Lem (New_Pos.Pos);
    end if;
    -- Show Y thrust, speeds and fuel
    -- @@@
    Con_Io.Flush;
  end Update;


  -- Put game end
  -- subtype End_Reason_List is Flight.Status_List
  --                            range Flight.Landed .. Flight.Lost;
  procedure Put_End (Reason : in End_Reason_List) is
  begin
    -- @@@
    null;
  end Put_End;

end Screen;

