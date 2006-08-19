-- Test PragmARC.Images
--
with Ada.Text_IO;
with PragmARC.Images;

use Ada.Text_IO;
use PragmARC;
procedure Test_Images is
   type Mod_Int is mod 2 ** Integer'Size;

   function Image is new Images.Signed_Image  (Integer);
   function Image is new Images.Modular_Image (Mod_Int);
   function Image is new Images.Float_Image   (Float);

   Middle : constant Mod_Int := Mod_Int'Last / 2;
begin -- Test_Images
   All_Fills : for Fill in Boolean loop
      All_Bases : for Base in Images.Number_Base loop
         Put_Line ('>' & Image (Integer'First,  0, Fill, Base) & '<');
         Put_Line ('>' & Image (Integer'(0),    0, Fill, Base) & '<');
         Put_Line ('>' & Image (Integer'Last,   0, Fill, Base) & '<');
         Put_Line ('>' & Image (Integer'First, 17, Fill, Base) & '<');
         Put_Line ('>' & Image (Integer'(0),   17, Fill, Base) & '<');
         Put_Line ('>' & Image (Integer'Last,  17, Fill, Base) & '<');
         Put_Line ('>' & Image (Integer'First, 35, Fill, Base) & '<');
         Put_Line ('>' & Image (Integer'(0),   35, Fill, Base) & '<');
         Put_Line ('>' & Image (Integer'Last,  35, Fill, Base) & '<');

         Put_Line ('>' & Image (Mod_Int'First,  0, Fill, Base) & '<');
         Put_Line ('>' & Image (Middle,         0, Fill, Base) & '<');
         Put_Line ('>' & Image (Mod_Int'Last,   0, Fill, Base) & '<');
         Put_Line ('>' & Image (Mod_Int'First, 17, Fill, Base) & '<');
         Put_Line ('>' & Image (Middle,        17, Fill, Base) & '<');
         Put_Line ('>' & Image (Mod_Int'Last,  17, Fill, Base) & '<');
         Put_Line ('>' & Image (Mod_Int'First, 35, Fill, Base) & '<');
         Put_Line ('>' & Image (Middle,        35, Fill, Base) & '<');
         Put_Line ('>' & Image (Mod_Int'Last,  35, Fill, Base) & '<');
      end loop All_Bases;
      
      Put_Line ('>' & Image (Float'First, Zero_Filled => Fill) & '<');
      Put_Line ('>' & Image (Float'First, 5, 5, 5, Fill)       & '<');
      Put_Line ('>' & Image (Float'First, 5, 5, 0, Fill)       & '<');
      Put_Line ('>' & Image (-1.0, Zero_Filled => Fill)        & '<');
      Put_Line ('>' & Image (-1.0, 5, 5, 5, Fill)              & '<');
      Put_Line ('>' & Image (-1.0, 5, 5, 0, Fill)              & '<');
      Put_Line ('>' & Image ( 0.0, Zero_Filled => Fill)        & '<');
      Put_Line ('>' & Image ( 0.0, 5, 5, 5, Fill)              & '<');
      Put_Line ('>' & Image ( 0.0, 5, 5, 0, Fill)              & '<');
      Put_Line ('>' & Image ( 1.0, Zero_Filled => Fill)        & '<');
      Put_Line ('>' & Image ( 1.0, 5, 5, 5, Fill)              & '<');
      Put_Line ('>' & Image ( 1.0, 5, 5, 0, Fill)              & '<');
      Put_Line ('>' & Image (Float'Last, Zero_Filled => Fill)  & '<');
      Put_Line ('>' & Image (Float'Last, 5, 5, 5, Fill)        & '<');
      Put_Line ('>' & Image (Float'Last, 5, 5, 0, Fill)        & '<');
   end loop All_Fills;
end Test_Images;
--
-- Copyright (C) 2006 by Jeffrey R. Carter
--
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- Foundation; either version 2, or (at your option) any later version.
-- This software is distributed in the hope that it will be useful, but WITH
-- OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software Foundation, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA.
