-- Mine Detector Game
-- Copyright (C) 2004 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Encapsulates the operations on the mine field for Mine_Detector game
--
-- V4.4 2004 Aug 01
--
with PragmARC.Protected_Option;
package Field.Operations is
   procedure Reset; -- Reset the mine field to its initial condition

   procedure Mark (Cell : in Cell_Location); -- Mark a cell as having a mine, or unmark a marked cell

   procedure Step (Cell : in Cell_Location); -- Step on a cell

   type Game_State_ID is (In_Progress, Won, Lost);
   
   function Game_State return Game_State_ID;
   
   Auto_Marking : PragmARC.Protected_Option.Handle;
   -- Enables and disables automatic marking
   -- May be enabled or disabled at any time and takes effect immediately
   
   Extended_Stepping : PragmARC.Protected_Option.Handle;
   -- Enables and disables extended automatic stepping (automatic stepping after marking)
   -- May be enabled or disabled at any time and takes effect immediately
end Field.Operations;
--
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- Foundation; either version 2, or (at your option) any later version.
-- This software is distributed in the hope that it will be useful, but WITH
-- OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software Foundation, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA.