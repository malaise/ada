with Con_Io, Text_Handler, Directory;
package Afpx_Typ is

  -- Version of Afpx
  Afpx_Version : constant Float := 2.1;

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

  -- A descriptor
  type Dscr_Rec is record
    -- To be checked prior to loading
    Version    : Float;
    -- To generate refresh. True in file if used
    Modified   : Boolean;
    -- In the file: index of the dscr for fields and init.
    -- In memory, its No
    Dscr_Index : Descriptor_Range;
    -- Nb of fields of the dscr
    Nb_Fields  : Absolute_Field_Range;
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
  subtype Height_Range is Positive range 1 .. Con_Io.Row_Range_Last + 1;
  subtype Width_Range  is Positive range 1 .. Con_Io.Col_Range_Last + 1;

  -- Characters of the fields
  Max_Init_Len : constant Integer :=
   (Con_Io.Row_Range_Last + 1) * (Con_Io.Col_Range_Last + 1);
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
    Upper_Left, Lower_Right : Con_Io.Square;
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

  -- The init/current charcters of current descriptor
  subtype Char_Str is String (Char_Str_Range);


  -- Check is square (relative to field) is in field
  function In_Field (Field  : in Field_Rec;
                     Square : in Con_Io.Square) return Boolean;

  -- Check is square (absolute) is in field
  function In_Field_Absolute (Field  : in Field_Rec;
                              Square : in Con_Io.Square) return Boolean;

end Afpx_Typ;

