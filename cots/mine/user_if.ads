-- Mine Detector Game
-- Copyright (C) 2016 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Provide the user interface
--
-- V1.0/GTK3 2016 Feb 15
--
with Field;
package User_If is
  procedure Init;

  procedure Display_Count (Count    : in Field.Valid_Count;
                           Stepped  : in Boolean;
                           Cell     : in Field.Cell_Location);

  -- Display a marked cell
  procedure Display_Mark (Cell : in Field.Cell_Location);

  -- Display a mine
  procedure Display_Mine (Cell : in Field.Cell_Location);

  -- Display a blank cell
  procedure Display_Blank (Cell : in Field.Cell_Location);

  -- Display # of mines still to mark
  procedure Display_To_Go (To_Go : in Integer);

  -- Return to start of game condition
  procedure Reset_Screen;

  -- Get auto-marking state
  function Auto_Marking return Boolean;

  -- Get extended-stepping (after mark) state
  function Extended_Stepping return Boolean;
end User_If;
--
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- Foundation; version 2.
-- This software is distributed in the hope that it will be useful, but WITH
-- OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software Foundation, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA.

