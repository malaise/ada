with Ada.Text_IO;

use Ada;
package body Bag_Test_Help is

   ---------
   -- Put --
   ---------

   procedure Put (Item : in out Positive; Context : in out Int_Bag.Context_Data'Class; Continue : out Boolean) is
   begin
      Continue := True;
      Text_IO.Put_Line (Item => Integer'Image (Item) );
   end Put;

end Bag_Test_Help;
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
--
-- As a special exception, if other files instantiate generics from this
-- unit, or you link this unit with other files to produce an executable,
-- this unit does not by itself cause the resulting executable to be
-- covered by the GNU General Public License. This exception does not
-- however invalidate any other reasons why the executable file might be
-- covered by the GNU Public License.