-- Test PragmARC.List_Unbounded_Unprotected.Assign
--
with Ada.Text_IO;
with Lasgn_Help;

use Ada.Text_IO;
use Lasgn_Help;
procedure Lasgn is
   First   : Int_List.Handle;
   Second  : Int_List.Handle;
   New_Pos : Int_List.Position;
   Dummy   : Integer := 0;

   Off : constant Int_List.Position := Int_List.Off_List (First);

   procedure Put
      (Item : in out Positive; Context : in out Integer; Pos : in Int_List.Position; Continue : out Boolean)
   is
      -- null;
   begin -- Put
      Put_Line (Item => Integer'Image (Item) );
      Continue := True;
   end Put;

   procedure Put_All is new Int_List.Iterate (Context_Data => Integer, Action => Put);
begin -- Lasgn
   Fill : for I in 1 .. 10 loop
      if I rem 2 = 0 then
         Int_List.Insert (Into => First, Item => I, Before => Off, New_Pos => New_Pos);
      else
         Int_List.Append (Into => First, Item => I, After => Off, New_Pos => New_Pos);
      end if;
   end loop Fill;

   Put_All (Over => First, Context => Dummy);

   Int_List.Assign (To => Second, From => First);

   Put_All (Over => Second, Context => Dummy);
end Lasgn;
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