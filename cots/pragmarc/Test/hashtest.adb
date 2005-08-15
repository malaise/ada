with Ada.Text_IO;
with PragmARC.Hash_Fast_Variable_Length;

use Ada;
use PragmARC;
procedure Hashtest is
   Input  : String (1 .. 255);
   Last   : Natural;
   Result : Hash_Fast_Variable_Length.Byte;

   function To_Byte (Item : Character) return Hash_Fast_Variable_Length.Byte is
      -- null;
   begin -- to_byte
      return Character'Pos (Item);
   end To_Byte;

   function String_Hash is new Hash_Fast_Variable_Length.Hash
     (Element => Character, Index => Positive, String => String, To_Byte => To_Byte)
   ;
begin -- Hashtest
   All_Strings : loop
      Text_Io.Put ("Enter a string to hash:  ");
      Text_Io.Get_Line (Input, Last);

      exit All_Strings when Last < Input'First;

      Result := String_Hash (Input (Input'First .. Last) );
      Text_Io.Put_Line (Hash_Fast_Variable_Length.Byte'Image (Result) );
      Text_Io.New_Line;
   end loop All_Strings;
end Hashtest;
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
