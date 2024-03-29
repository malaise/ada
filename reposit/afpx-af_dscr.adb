with Ada.Direct_Io;
with Basic_Proc;
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
  Init_Colors : array (Absolute_Field_Range) of Afpx_Typ.Colors_Rec;

  -- Half offsets from file
  Half_Row_Offset : array (Absolute_Field_Range) of Boolean;
  Half_Col_Offsets : array (Absolute_Field_Range)
                     of Afpx_Typ.Half_Col_Offsets_Array;

  -- Descriptor read
  Dscrs : Afpx_Typ.Descriptors_Array;

  -- Load Afpx files if needed
  procedure Load_Files;

  -- Load the screen size, raise No_Descriptor if failure of version check
  function Get_Size return Con_Io.Square is
  begin
    -- Load Afpx files if needed
    Load_Files;

    -- Even when not used, each dscr has the screen size
    return Dscrs(1).Size;
  end Get_Size;


  -- Check if a descriptor exists
  function Is_Defined (Dscr_No : in Descriptor_Range) return Boolean is
  begin
    -- Load Afpx files if needed
    Load_Files;

    -- Check it is defined
    return Dscrs(Dscr_No).Modified;
  end Is_Defined;

  -- Load a descriptor
  procedure Load_Dscr (Dscr_No : in Descriptor_Range) is
    Dscr_Index : Descriptor_Range;
  begin
    -- Load Afpx files if needed
    Load_Files;

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

    -- Save colors and half offset, copy chars
    for I in Absolute_Field_Range'First .. Dscrs(Dscr_No).Nb_Fields loop
      Init_Colors(I) := Fields(I).Colors;
      Half_Row_Offset(I) := Fields(I).Half_Row_Offset;
      Half_Col_Offsets(I) := Fields(I).Half_Col_Offsets;
    end loop;
    Chars := Init_Str;

    Dscr_Set := True;

  end Load_Dscr;

  -- Release a descriptor. Check will raise No_Descriptor
  procedure Release_Dscr is
  begin
    Dscr_Set := False;
  end Release_Dscr;

  -- Check if a descriptor is in use
  procedure Check is
  begin
    if not Dscr_Set then
      raise No_Descriptor;
    end if;
  end Check;

  -- Check if a descriptor is being used
  function Is_Set return Boolean is (Dscr_Set);

  -- Check if a descriptor is in use and if field is valid
  procedure Check (Field_No : in Absolute_Field_Range) is
    use type Absolute_Field_Range;
  begin

    Check;
    if Field_No = Lfn then
      -- A list in the descriptor?
      if Has_List then
        return;
      end if;
    else
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
    use type Field_Kind_List;
  begin
    Check;
    return Fields(Lfn).Kind = Button_Field;
  end Has_List;

  -- Load a field
  procedure Load_Field (Field_No : in Absolute_Field_Range;
        Load_Colors : in Boolean;
        Load_Chars  : in Boolean;
        Reset_Activation : in Boolean;
        Reset_Protection : in Boolean) is
    Field : constant Afpx_Typ.Field_Rec := Fields(Field_No);
    Nb_Chars : constant Positive := Field.Height * Field.Width;
    use type Absolute_Field_Range;
  begin
    Check (Field_No);
    if Reset_Activation then
      Fields(Field_No).Activated := True;
    end if;
    if Reset_Protection then
      Fields(Field_No).Isprotected := False;
    end if;
    if Load_Colors then
      Fields(Field_No).Colors := Init_Colors(Field_No);
    end if;

    if Load_Chars and then Field_No /= 0 then
      -- Copy the nb_chars from init_str to char_str
      for I in Field.Char_Index .. Field.Char_Index + Nb_Chars - 1 loop
        Chars(I) := Init_Str(I);
      end loop;
      Fields(Field_No).Half_Row_Offset := Half_Row_Offset(Field_No);
      Fields(Field_No).Half_Col_Offsets := Half_Col_Offsets(Field_No);
    end if;
  exception
    when others =>
      raise Afpx_Internal_Error;
  end Load_Field;

  -- Try to getenv data dir then to open the files
  Files_Loaded : Boolean := False;
  procedure Load_Files is
  begin
    if Files_Loaded then
      return;
    end if;

    declare
      File_Dir_Env_Name : constant String := "AFPX_DATA_DIR";
      Default_Path : constant String := ".";
    begin
      Afpx_Typ.Dest_Path := As.U.Tus (Default_Path);
      Environ.Get_Us (File_Dir_Env_Name, Afpx_Typ.Dest_Path);
      Afpx_Typ.Dest_Path.Append ("/");

      Dscr_Io.Open (Dscr_File, Dscr_Io.In_File,
                    Afpx_Typ.Dest_Path.Image & Afpx_Typ.Dscr_File_Name);
      Fld_Io.Open  (Fld_File,  Fld_Io.In_File,
                    Afpx_Typ.Dest_Path.Image & Afpx_Typ.Fld_File_Name);
      Init_Io.Open (Init_File, Init_Io.In_File,
                    Afpx_Typ.Dest_Path.Image & Afpx_Typ.Init_File_Name);
    exception
      when others =>
        Basic_Proc.Put_Error ("AFPX ERROR: Can't read descriptors. For info, "
                        & File_Dir_Env_Name & " is ");
        if Environ.Is_Set (File_Dir_Env_Name) then
          Basic_Proc.Put_Line_Error (Afpx_Typ.Dest_Path.Image);
        else
          Basic_Proc.Put_Line_Error ("not set.");
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
    Files_Loaded := True;
  end Load_Files;

begin
  -- Load files (and check) at elaboration, so as early as possible
  Load_Files;
end Af_Dscr;

