-- Program to test package complex
--
with Text_Io;
with PragmARC.Complex;
procedure Cmplxtst is
   type Real is digits 10;

   package Cmplx is new PragmARC.Complex (Supplied_Real => Real);

   Left  : constant Cmplx.Number := Cmplx.To_Complex ( 5.42, 5.16);
   Right : constant Cmplx.Number := Cmplx.To_Complex (-5.74, 6.52);
   Neg_1 : constant Cmplx.Number := Cmplx.To_Complex (-1.00, 0.00);
   Num_I : constant Cmplx.Number := Cmplx.To_Complex ( 0.00, 1.00);

   package Real_Io is new Text_Io.Float_Io (Num => Real);

   procedure Put (Item : in Cmplx.Number) is
      -- null;
   begin -- Put
      Text_Io.Put ('(');
      Real_Io.Put (Cmplx.Real_Part (Item) );
      Text_Io.Put (',');
      Real_Io.Put (Cmplx.Imag_Part (Item) );
      Text_Io.Put (')');
   end Put;

   use type Cmplx.Number;
begin -- Cmplxtst
   Put (Item => Left);
   Text_Io.Put (" + ");
   Put (Item => Right);
   Text_Io.Put_Line (" = (-0.32, 11.68)");
   Text_Io.Put ("     ");
   Put (Item => Left + Right);
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Put (Item => Left);
   Text_Io.Put (" - ");
   Put (Item => Right);
   Text_Io.Put_Line (" = (11.16, -1.36)");
   Text_Io.Put ("     ");
   Put (Item => Left - Right);
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Put (Item => Left);
   Text_Io.Put (" * ");
   Put (Item => Right);
   Text_Io.Put_Line (" = (-64.754, 5.72)");
   Text_Io.Put ("     ");
   Put (Item => Left * Right);
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Put (Item => Left);
   Text_Io.Put (" / ");
   Put (Item => Right);
   Text_Io.Put_Line (" = (0.0335603, -0.8608338)");
   Text_Io.Put ("     ");
   Put (Item => Left / Right);
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ('|');
   Put (Item => Left);
   Text_Io.Put_Line ("| = 7.4834484");
   Text_Io.Put ("     ");
   Real_Io.Put (abs Left);
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ('|');
   Put (Item => Right);
   Text_Io.Put_Line ("| = 8.6866564");
   Text_Io.Put ("     ");
   Real_Io.Put (abs Right);
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Raise_I : for Power in Integer range 2 .. 4 loop
      Text_Io.Put_Line ("i **" & Integer'Image (Power));
      Text_Io.Put ("     ");
      Put (Item => Num_I ** Power);
      Text_Io.New_Line;
      Text_Io.Skip_Line;
   end loop Raise_I;

   Text_Io.Put ("sqrt [");
   Put (Item => Left);
   Text_Io.Put_Line ("] = (2.5400244, 1.0157383)");
   Text_Io.Put ("     ");
   Put (Item => Cmplx.Sqrt (Left) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("sqrt [");
   Put (Item => Right);
   Text_Io.Put_Line ("] = (1.2138073, 2.685764)");
   Text_Io.Put ("     ");
   Put (Item => Cmplx.Sqrt (Right) );
   Text_Io.New_Line;
   Text_Io.Skip_Line;

   Text_Io.Put ("sqrt [");
   Put (Item => Neg_1);
   Text_Io.Put_Line ("] = (0.0, 1.0)");
   Text_Io.Put ("     ");
   Put (Item => Cmplx.Sqrt (Neg_1) );
   Text_Io.Skip_Line;
end Cmplxtst;
--
-- Copyright (C) 2000 by PragmAda Software Engineering.  All rights reserved.
--
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- Foundation; either version 2, or (at your option) any later version.
-- This software is distributed in the hope that it will be useful, but WITH
-- OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software Foundation, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA.
