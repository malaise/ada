-- Test PragmARC.Wrapping
--
with PragmARC.Wrapping;
with Ada.Text_IO;

use Ada.Text_IO;
procedure Wrapping_Test is 
   package Integer_Wrapping is new PragmARC.Wrapping (Item => Integer);
   use Integer_Wrapping;

   type Color_ID is (Red, Orange, Yellow, Green, Blue, Purple); 

   package Color_Wrapping is new PragmARC.Wrapping (Item => Color_ID);
   use Color_Wrapping;
   
   package Character_Wrapping is new PragmARC.Wrapping (Item => Character);
   use Character_Wrapping;
begin -- Wrapping_Test
   Put_Line (Integer'Image (Wrap_Pred (Integer'First) ) );
   Put_Line (Integer'Image (Wrap_Succ (Integer'Last) ) );
   Put_Line (Integer'Image (Wrap_Pred (0) ) );
   Put_Line (Integer'Image (Wrap_Succ (0) ) );
   Put_Line (Color_ID'Image (Wrap_Pred (Color_ID'First) ) );
   Put_Line (Color_ID'Image (Wrap_Succ (Color_ID'Last) ) );
   Put_Line (Color_ID'Image (Wrap_Pred (Yellow) ) );
   Put_Line (Color_ID'Image (Wrap_Succ (Yellow) ) );
   Put_Line (Integer'Image (Character'Pos (Wrap_Pred (Character'First) ) ) );
   Put_Line (Integer'Image (Character'Pos (Wrap_Succ (Character'Last) ) ) );
   Put_Line (Integer'Image (Character'Pos (Wrap_Pred ('M') ) ) );
   Put_Line (Integer'Image (Character'Pos (Wrap_Succ ('M') ) ) );
end Wrapping_Test;
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
