-- Test Binary_Searcher
--
with PragmARC.Assignment;
with PragmARC.Three_Way;
with PragmARC.Binary_Searcher;
with PragmARC.Sort_Quick_In_Place;
with Ada.Text_Io;
with Ada.Numerics.Float_Random;

use Ada;
use Ada.Numerics;
use PragmARC;
procedure Bs_Test is
   type List is array (Positive range <>) of Float;

   function Compare is new Three_Way.Compare (Float);

   package Searcher is new Binary_Searcher (Float, Positive, List);

   procedure Assign is new Assignment (Float);

   procedure Sort is new Sort_Quick_In_Place (Float, Positive, List);

   L : List (1 .. 100);
   R : Searcher.Result_Info;
   F : Float;
   G : Float_Random.Generator;

   use type Searcher.Outside_Id;
begin -- Bs_Test
   Float_Random.Reset (G);

   Several : for I in 1 .. 10 loop
      L := (others => Float_Random.Random (G) );
      Sort (Set => L);

      if I = 8 then
         F := L (L'Last / 2);
      elsif I = 9 then
         F := L (L'First) - 1.0;
      elsif I = 10 then
         F := L (L'Last) + 1.0;
      else
         F := Float_Random.Random (G);
      end if;

      R := Searcher.Search (F, L);

      Text_Io.Put_Line (Float'Image (F) & ' ' & Boolean'Image (R.Found) );

      if R.Found then
         Text_Io.Put_Line (Integer'Image (R.Location) & Float'Image (L (R.Location) ) );
      else
         Text_Io.Put_Line (Boolean'Image (R.Unfound.Between) );

         if R.Unfound.Between then
            Text_Io.Put_Line (Integer'Image (R.Unfound.Close) & Float'Image (L (R.Unfound.Close) ) &
               Float'Image (L (R.Unfound.Close + 1) ) );
         else
            Text_Io.Put_Line (Searcher.Outside_Id'Image (R.Unfound.Location) );

            if R.Unfound.Location = Searcher.First then
               Text_Io.Put_Line (Float'Image (L (L'First) ) );
            else
               Text_Io.Put_Line (Float'Image (L (L'Last) ) );
            end if;
         end if;
      end if;

      Text_Io.Skip_Line;
   end loop Several;
end Bs_Test;
--
-- Copyright (C) 2001 by PragmAda Software Engineering.  All rights reserved.
--
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- Foundation; either version 2, or (at your option) any later version.
-- This software is distributed in the hope that it will be useful, but WITH
-- OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software Foundation, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA.