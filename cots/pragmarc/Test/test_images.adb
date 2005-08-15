with Ada.Text_IO;
with PragmARC.Images;

use Ada.Text_IO;
use PragmARC;
procedure Test_Images is
   type Mod_Int is mod 2 ** Integer'Size;

   function Image is new Images.Signed_Image  (Integer);
   function Image is new Images.Modular_Image (Mod_Int);

   Middle : constant Mod_Int := Mod_Int'Last / 2;
begin -- Test_Images
   All_Fills : for Fill in Boolean loop
      All_Bases : for Base in Images.Number_Base loop
         Put_Line ('>' & Image (Integer'First,  0, Fill, Base) & '<');
         Put_Line ('>' & Image (Integer'(0),    0, Fill, Base) & '<');
         Put_Line ('>' & Image (Integer'Last,   0, Fill, Base) & '<');
         Put_Line ('>' & Image (Integer'First, 17, Fill, Base) & '<');
         Put_Line ('>' & Image (Integer'(0),   17, Fill, Base) & '<');
         Put_Line ('>' & Image (Integer'Last,  17, Fill, Base) & '<');
         Put_Line ('>' & Image (Integer'First, 35, Fill, Base) & '<');
         Put_Line ('>' & Image (Integer'(0),   35, Fill, Base) & '<');
         Put_Line ('>' & Image (Integer'Last,  35, Fill, Base) & '<');

         Put_Line ('>' & Image (Mod_Int'First,  0, Fill, Base) & '<');
         Put_Line ('>' & Image (Middle,         0, Fill, Base) & '<');
         Put_Line ('>' & Image (Mod_Int'Last,   0, Fill, Base) & '<');
         Put_Line ('>' & Image (Mod_Int'First, 17, Fill, Base) & '<');
         Put_Line ('>' & Image (Middle,        17, Fill, Base) & '<');
         Put_Line ('>' & Image (Mod_Int'Last,  17, Fill, Base) & '<');
         Put_Line ('>' & Image (Mod_Int'First, 35, Fill, Base) & '<');
         Put_Line ('>' & Image (Middle,        35, Fill, Base) & '<');
         Put_Line ('>' & Image (Mod_Int'Last,  35, Fill, Base) & '<');
      end loop All_Bases;
   end loop All_Fills;
end Test_Images;
