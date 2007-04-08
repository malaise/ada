with Ada.Direct_Io, Ada.Text_Io;
with Environ, Text_Handler;
separate (Afpx)
package body Af_Dscr is

  -- Direct_io of descriptors, fields, init strings
  package Dscr_Io is new Ada.Direct_Io (Afpx_Typ.Descriptors_Array);
  Dscr_File : Dscr_Io.File_Type;
  package Fld_Io  is new Ada.Direct_Io (Afpx_Typ.Fields_Array);
  Fld_File : Fld_Io.File_Type;
  package Init_Io is new Ada.Direct_Io (Afpx_Typ.Char_Str);
  Init_File : Init_Io.File_Type;

  -- Has a descriptor been set
  Dscr_Set : Boolean := False;

  -- Colors from file
  Init_Colors : array (Afpx_Typ.Absolute_Field_Range) of Afpx_Typ.Colors_Rec;

  -- Chars from file
  Init_Str : Afpx_Typ.Char_Str;

  -- Descriptor read
  Dscrs : Afpx_Typ.Descriptors_Array;

  -- Load the screen size, raise No_Descriptor if failure of version check
  function Load_Size return Con_Io.Full_Square is
  begin
    -- Even when not used, each dscr has the screen size
    return Dscrs(1).Size;
  end Load_Size;
  
  -- Load a descriptor
  procedure Load_Dscr (Dscr_No : in Afpx_Typ.Descriptor_Range) is
    Dscr_Index : Afpx_Typ.Descriptor_Range;
  begin

    -- Check it is defined
    if not Dscrs(Dscr_No).Modified then
      raise No_Descriptor;
    end if;

    -- Save index of descriptor for field and init files
    Dscr_Index := Dscrs(Dscr_No).Dscr_Index;

    -- Copy descriptor, read fields and chars
    Current_Dscr := Dscrs(Dscr_No);
    Current_Dscr.Dscr_Index := Dscr_No;

    Fld_Io.Read  (Fld_File , Fields,   Fld_Io.Positive_Count(Dscr_Index));
    Init_Io.Read (Init_File, Init_Str, Init_Io.Positive_Count(Dscr_Index));

    -- Save colors, copy chars
    for I in
    Afpx_Typ.Absolute_Field_Range'First .. Dscrs(Dscr_No).Nb_Fields loop
      Init_Colors(I) := Fields(I).Colors;
    end loop;
    Chars := Init_Str;

    Dscr_Set := True;

  end Load_Dscr;

  -- Release a descriptor. Check will raise No_Descriptor
  procedure Release_Dscr is
  begin
    Dscr_Set := False;
  end Release_Dscr;

  -- Check if a descriptor i in use
  procedure Check is
  begin
    if not Dscr_Set then
      raise No_Descriptor;
    end if;
  end Check;


  -- Check if a descriptor i in use and if field is valid
  procedure Check (Field_No : in Afpx_Typ.Absolute_Field_Range) is
    use type Afpx_Typ.Absolute_Field_Range;
  begin

    if Field_No = Lfn then
      -- A list in the descriptor?
      if Has_List then
        return;
      end if;
    else
      Check;
       -- This field in the descriptor?
      if Field_No <= Current_Dscr.Nb_Fields then
        return;
      end if;
    end if;
    -- No such field/list
    raise Invalid_Field;
  end Check;

  -- Check if descriptor has the list field active
  function Has_List return Boolean is
    use type Afpx_Typ.Field_Kind_List;
  begin
    Check;
    return Fields(Lfn).Kind = Afpx_Typ.Button;
  end Has_List;


  -- Load a field
  procedure Load_Field (Field_No : in Afpx_Typ.Absolute_Field_Range;
        Load_Colors : in Boolean;
        Load_Chars  : in Boolean) is
    Field : constant Afpx_Typ.Field_Rec := Fields(Field_No);
    Nb_Chars : constant Positive := Field.Height * Field.Width;
    use Afpx_Typ;
  begin
    Check (Field_No);
    Fields(Field_No).Activated := True;
    Fields(Field_No).Isprotected := False;
    if Load_Colors then
      Fields(Field_No).Colors := Init_Colors(Field_No);
    end if;

    if Load_Chars and then Field_No /= 0 then
      -- Copy the nb_chars from init_str to char_str
      for I in Field.Char_Index .. Field.Char_Index + Nb_Chars - 1 loop
        Chars(I) := Init_Str(I);
      end loop;
    end if;
  exception
    when others =>
      raise Afpx_Internal_Error;
  end Load_Field;

begin
  -- Try to getenv data dir then to open the files
  declare
    File_Dir_Env_Name : constant String := "AFPX_DATA_DIR";
    Default_Path : constant String := ".";
  begin
    Text_Handler.Set (Afpx_Typ.Dest_Path, Default_Path);
    Environ.Get_Txt (File_Dir_Env_Name, Afpx_Typ.Dest_Path);
    Text_Handler.Append (Afpx_Typ.Dest_Path, "/");

    Dscr_Io.Open (Dscr_File, Dscr_Io.In_File,
                  Text_Handler.Value(Afpx_Typ.Dest_Path) & Afpx_Typ.Dscr_File_Name);
    Fld_Io.Open  (Fld_File,  Fld_Io.In_File,
                  Text_Handler.Value(Afpx_Typ.Dest_Path) & Afpx_Typ.Fld_File_Name);
    Init_Io.Open (Init_File, Init_Io.In_File,
                  Text_Handler.Value(Afpx_Typ.Dest_Path) & Afpx_Typ.Init_File_Name);
  exception
    when others =>
      Ada.Text_Io.Put ("AFPX ERROR: Can't read descriptors. For info, "
                      & File_Dir_Env_Name & " is ");
      if Environ.Is_Set (File_Dir_Env_Name) then
        Ada.Text_Io.Put_Line (Text_Handler.Value(Afpx_Typ.Dest_Path));
      else
        Ada.Text_Io.Put_Line ("not set.");
      end if;
      raise Afpx_File_Not_Found;
  end;

  -- Read first descriptor
  begin
    Dscr_Io.Read (Dscr_File, Dscrs, 1);
  exception
    when others =>
      raise Afpx_File_Read_Error;
  end;

  -- Check Afpx version
  if Dscrs(1).Version /= Afpx_Typ.Afpx_Version then
    raise Afpx_File_Version_Error;
  end if;
end Af_Dscr;
