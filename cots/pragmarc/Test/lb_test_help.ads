with PragmARC.Assignment;
with PragmARC.List_Bounded_Unprotected;
package LB_Test_Help is
   procedure Assign is new PragmARC.Assignment (Element => Positive);

   package Int_List is new PragmARC.List_Bounded_Unprotected (Element => Positive);

   procedure Put (Item : in out Positive; Context : in out Integer; Pos : in Int_List.Position; Continue : out Boolean);
end LB_Test_Help;
--
-- Copyright (C) 2002 by PragmAda Software Engineering.  All rights reserved.
--
-- This is free software; you can redistribute it and/or modify it under
-- terms of the GNU General Public License as published by the Free Software
-- Foundation; either version 2, or (at your option) any later version.
-- This software is distributed in the hope that it will be useful, but WITH
-- OUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
-- for more details. Free Software Foundation, 59 Temple Place - Suite
-- 330, Boston, MA 02111-1307, USA.