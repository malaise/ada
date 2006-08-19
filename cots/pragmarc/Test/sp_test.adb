-- Test PragmARC.Safe_Pointers
--
with Ada.Text_IO;
with SP_Test_Help;
procedure SP_Test is
   use SP_Test_Help;
   
   P1 : Int_Ptr.Safe_Pointer := Int_Ptr.Allocate (23); -- skiddoo.
   P2 : Int_Ptr.Safe_Pointer := P1;
   
   use Ada.Text_IO;
   use type Int_Ptr.Safe_Pointer;
begin -- SP_Test
   Put_Line (Item => "P1 contains" & Integer'Image (Int_Ptr.Get (P1) ) );
   Put_Line (Item => "P2 contains" & Integer'Image (Int_Ptr.Get (P2) ) );
   Put_Line (Item => "P1 = P2 is " & Boolean'Image (P1 = P2) );
   
   declare
      P3 : Int_Ptr.Safe_Pointer := P2;
   begin
      Put_Line (Item => "P3 contains" & Integer'Image (Int_Ptr.Get (P3) ) );
      Put_Line (Item => "P1 = P3 is " & Boolean'Image (P1 = P3) );
      Int_Ptr.Put (Pointer => P3, Value => 42); -- life, the universe, and everything.
      Put_Line (Item => "P1 contains" & Integer'Image (Int_Ptr.Get (P1) ) );
      Put_Line (Item => "P2 contains" & Integer'Image (Int_Ptr.Get (P2) ) );
      Put_Line (Item => "P3 contains" & Integer'Image (Int_Ptr.Get (P3) ) );
      Put_Line (Item => "P1 = P2 is " & Boolean'Image (P1 = P2) );
      Put_Line (Item => "P1 = P3 is " & Boolean'Image (P1 = P3) );
   end;
   
   Int_Ptr.Put (Pointer => P2, Value => 71); -- really dirty.
   Put_Line (Item => "P1 contains" & Integer'Image (Int_Ptr.Get (P1) ) );
   Put_Line (Item => "P2 contains" & Integer'Image (Int_Ptr.Get (P2) ) );
   Put_Line (Item => "P1 = P2 is " & Boolean'Image (P1 = P2) );

   P2 := Int_Ptr.Allocate (3); -- pi.
   Put_Line (Item => "P1 contains" & Integer'Image (Int_Ptr.Get (P1) ) );
   Put_Line (Item => "P2 contains" & Integer'Image (Int_Ptr.Get (P2) ) );
   Put_Line (Item => "P1 = P2 is " & Boolean'Image (P1 = P2) );
end SP_Test;
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
