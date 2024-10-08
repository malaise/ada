-- Mine Detector Game
-- Copyright (C) 2016 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Encapsulates the definition of the mine field
--
-- V1.0/GTK3 2016 Feb 15
--
package Field is
  -- Size of the mine field
  subtype Valid_Row    is Positive range 1 .. 16;
  subtype Valid_Column is Positive range 1 .. 30;

  type Cell_Location is record
    Row    : Valid_Row    := 1;
    Column : Valid_Column := 1;
  end record;

  -- Count of # of mines in a cell & its neighbors
  subtype Valid_Count is Natural range 0 .. 9;
end Field;
--
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- Foundation; version 2.
-- This software is distributed in the hope that it will be useful, but WITH
-- OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software Foundation, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA.

