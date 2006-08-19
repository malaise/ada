-- Mine Detector Game
-- Copyright (C) 2006 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Provide the user interface
--
-- V5.0 2006 Feb 01
--
with Field;
package User_If is
   procedure Display_Count (Count    : in Field.Valid_Count;
                            Stepped  : in Boolean;
                            Cell     : in Field.Cell_Location);

   procedure Display_Mark (Cell : in Field.Cell_Location); -- Display a marked cell

   procedure Display_Mine (Cell : in Field.Cell_Location); -- Display a mine.

   procedure Display_Blank (Cell : in Field.Cell_Location); -- Display a blank cell

   procedure Display_To_Go (To_Go : in Integer); -- Display # of mines still to mark

   procedure Reset_Screen; -- Return to start of game condition
end User_If;
--
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- Foundation; either version 2, or (at your option) any later version.
-- This software is distributed in the hope that it will be useful, but WITH
-- OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software Foundation, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA.
