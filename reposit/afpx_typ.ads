with Generic_Con_Io, Con_Io, Text_Handler, Directory;
package Afpx_Typ is

  -- Version of Afpx
  Afpx_Version : constant Float := 4.2;

  -- Files path
  Dest_Path : Text_Handler.Text (Directory.Max_Dir_Name_Len + 1);

  -- Files name
  Dscr_File_Name : constant String := "AFPX.DSC";
  Fld_File_Name  : constant String := "AFPX.FLD";
  Init_File_Name : constant String := "AFPX.INI";

  -- Descriptor, field index
  type Descriptor_Range is new Positive range 1 .. 50;
  type Absolute_Field_Range is new Natural range 0 .. 200;
  subtype Field_Range is Absolute_Field_Range
          range 1 .. Absolute_Field_Range 'Last;

  -- The maximum length of a color name
  Max_Color_Name_Len : constant := 80;
  subtype Color_Name is String (1 .. Max_Color_Name_Len);
  No_Color : constant Color_Name := (others => ' ');
  type Color_Names is array (Generic_Con_Io.Effective_Colors) of Color_Name;

  -- A descriptor
  type Dscr_Rec is record
    -- To be checked prior to loading
    Version    : Float;
    -- Screen size
    Size : Con_Io.Full_Square;
    -- To generate refresh. True in file if used
    Modified   : Boolean;
    -- In the file: index of the dscr for fields and init.
    -- In memory, its No
    Dscr_Index : Descriptor_Range;
    -- Nb of fields of the dscr
    Nb_Fields  : Absolute_Field_Range;
    -- Name of Colors
    Colors : Color_Names;
    -- Background color
    Background : Con_Io.Effective_Basic_Colors;
  end record;
  type Descriptors_Array is array (Descriptor_Range) of Dscr_Rec;

  -- Field kind
  type Field_Kind_List is (Put, Button, Get);

  -- Field colors
  type Colors_Rec is record
    Foreground : Con_Io.Effective_Colors;
    Blink_Stat : Con_Io.Effective_Blink_Stats;
    Background : Con_Io.Effective_Basic_Colors;
    Selected   : Con_Io.Effective_Basic_Colors;
  end record;

  -- Width and height of a field
  subtype Height_Range is Positive range 1 .. Con_Io.Full_Row_Range_Last + 1;
  subtype Width_Range  is Positive range 1 .. Con_Io.Full_Col_Range_Last + 1;

  -- Characters of the fields
  Max_Init_Len : constant Integer :=
   (Con_Io.Full_Row_Range_Last + 1) * (Con_Io.Full_Col_Range_Last + 1);
  subtype Char_Str_Range is Positive range 1 .. Max_Init_Len;

  -- A field
  type Field_Rec is record
    -- First (0) field is the List.
    --   Set if Kind is Button, and not if Kind is Put
    -- Others are fields
    Kind : Field_Kind_List;
    -- Field activation
    Activated : Boolean;
    -- Field protection for Get / Button fields
    Isprotected : Boolean;
    -- Upper left, lower_right corners of the field
    Upper_Left, Lower_Right : Con_Io.Full_Square;
    -- Width, height
    Height : Height_Range;
    Width  : Width_Range;
    -- Colors
    Colors : Colors_Rec;
    -- Index in Char_Str of start of field content
    Char_Index : Char_Str_Range;
  end record;

  -- The array of fields of current descriptor
  type Fields_Array is array (Absolute_Field_Range) of Field_Rec;

  -- The init/current characters of current descriptor
  subtype Char_Str is Wide_String (Char_Str_Range);


  -- Check is square (relative to field) is in field
  function In_Field (Field  : in Field_Rec;
                     Square : in Con_Io.Full_Square) return Boolean;

  -- Check is square (absolute) is in field
  function In_Field_Absolute (Field  : in Field_Rec;
                              Square : in Con_Io.Full_Square) return Boolean;

  -- Make Generic_Con_Io.Colors_Definition from Dscr Color_Names
  function To_Def (Names : Color_Names) return Generic_Con_Io.Colors_Definition;
end Afpx_Typ;

