with Text_Io;
with Afpx_Typ;
package body Afpx is

  Lfn : constant Afpx_Typ.Absolute_Field_Range
      := Afpx_Typ.Absolute_Field_Range(List_Field_No);
  Afpx_Internal_Error : exception;

  package Af_Dscr is
   
    -- Current descriptor
    Current_Dscr : Afpx_Typ.Dscr_Rec;
   
    -- The fields of current descriptor
    Fields : Afpx_Typ.Fields_Array;
   
    -- Characters of the fields
    Chars : Afpx_Typ.Char_Str;
   
    -- Load a descriptor rais NO_DESCRIPTOR if invalid no
    --  failure of version check
    procedure Load_Dscr (Dscr_No : in Afpx_Typ.Descriptor_Range);
   
    -- Check if a descriptor has been used (raise NO_DESCRPTOR)
    procedure Check;
   
    -- Check if a descriptor has been used (raise NO_DESCRPTOR)
    --  and if FIELD_NO is valid in it (raise INVALID_FIELD)
    procedure Check (Field_No : in Afpx_Typ.Absolute_Field_Range);
   
    -- Load a field's characters and/or colors from init
    procedure Load_Field (Field_No : in Afpx_Typ.Absolute_Field_Range;
                          Load_Colors : in Boolean;
                          Load_Chars  : in Boolean);
   
  end Af_Dscr;


  function In_Field (Field_No : in Afpx_Typ.Absolute_Field_Range;
  Square : in Con_Io.Square) return Boolean is
  begin
    return Afpx_Typ.In_Field (Af_Dscr.Fields(Field_No), Square);
  end In_Field;

  function In_Field_Absolute (Field_No : in Afpx_Typ.Absolute_Field_Range;
  Square : in Con_Io.Square) return Boolean is
  begin
    return Afpx_Typ.In_Field_Absolute (Af_Dscr.Fields(Field_No), Square);
  end In_Field_Absolute;

  package body Af_Dscr is separate;


  function Next_Field (Field_No : Afpx_Typ.Absolute_Field_Range)
                      return Afpx_Typ.Absolute_Field_Range is
    Ret_No : Afpx_Typ.Absolute_Field_Range;
    use Afpx_Typ;
  begin
    Ret_No := Field_No;
    loop
      if Ret_No /= Af_Dscr.Current_Dscr.Nb_Fields then
        Ret_No := Ret_No + 1;
      elsif Af_Dscr.Fields(Lfn).Kind = Afpx_Typ.Button then
        Ret_No := 0;
      else
        Ret_No := 1;
      end if;
      exit when Af_Dscr.Fields(Ret_No).Activated;
    end loop;
    return Ret_No;
  end Next_Field;

  function Prev_Field (Field_No : Afpx_Typ.Absolute_Field_Range)
                      return Afpx_Typ.Absolute_Field_Range is
    Ret_No : Afpx_Typ.Absolute_Field_Range;
    use Afpx_Typ;
  begin
    Ret_No := Field_No;
    loop
      if Ret_No = 0 then
        Ret_No := Af_Dscr.Current_Dscr.Nb_Fields;
      elsif Ret_No /= 1 then
        Ret_No := Ret_No - 1;
      elsif Af_Dscr.Fields(Lfn).Kind = Afpx_Typ.Button then
        Ret_No := 0;
      else
        Ret_No := Af_Dscr.Current_Dscr.Nb_Fields;
      end if;
      exit when Af_Dscr.Fields(Ret_No).Activated;
    end loop;
    return Ret_No;
  end Prev_Field;

  function Next_Get_Field (Field_No : Afpx_Typ.Field_Range)
                           return Afpx_Typ.Field_Range is
    Fn : Afpx_Typ.Absolute_Field_Range;
    use Afpx_Typ;
  begin
    Fn := Field_No;
    loop
      Fn := Next_Field(Fn);
      exit when Fn /= 0
          and then Af_Dscr.Fields(Fn).Kind = Afpx_Typ.Get
          and then not Af_Dscr.Fields(Fn).Isprotected;
    end loop;
    return Fn;
  end Next_Get_Field;

  function Prev_Get_Field (Field_No : Afpx_Typ.Field_Range)
                          return Afpx_Typ.Field_Range is
    Fn : Afpx_Typ.Absolute_Field_Range;
    use Afpx_Typ;
  begin
    Fn := Field_No;
    loop
      Fn := Prev_Field(Fn);
      exit when Fn /= 0
          and then Af_Dscr.Fields(Fn).Kind = Afpx_Typ.Get
          and then not Af_Dscr.Fields(Fn).Isprotected;
    end loop;
    return Fn;
  end Prev_Get_Field;

  function Next_Mouse_Field (Field_No : Afpx_Typ.Field_Range)
                            return Afpx_Typ.Field_Range is
    Fn : Afpx_Typ.Absolute_Field_Range;
    use Afpx_Typ;
  begin
    Fn := Field_No;
    loop
      Fn := Next_Field(Fn);
      exit when Fn /= 0 and then Af_Dscr.Fields(Fn).Kind = Afpx_Typ.Button
          and then not Af_Dscr.Fields(Fn).Isprotected;
    end loop;
    return Fn;
  end Next_Mouse_Field;

  -- All the actions related to screen, keyboard and mouse
  package Af_Ptg is
   
    -- States of a field (or a row in list)
    -- Normal   => Foreground, Background
    -- Clicked  => Background, Foreground
    -- Selected => Foreground, Selected
    type State_List is (Normal, Clicked, Selected);
   
    -- Sets Foreground and background according to state
    procedure Set_Colors (Field : in Afpx_Typ.Field_Rec;
                          State : in State_List;
                          Foreground : out Con_Io.Effective_Colors;
                          Background : out Con_Io.Effective_Basic_Colors);
   
    -- Put a whole field in attribute
    procedure Put_Field (Field_No : in Afpx_Typ.Field_Range;
                         State    : in State_List);
   
    -- Put a whole row of a field in attribute
    procedure Put_Row (Field_No : in Afpx_Typ.Field_Range;
                       Row      : in Con_Io.Row_Range;
                       State    : in State_List);
   
    -- Put a string somewhere in a field
    procedure Put_Str (Field_No : in Afpx_Typ.Field_Range;
                       Pos      : in Con_Io.Square;
                       Str      : in String;
                       State    : in State_List);
   
    -- Erase a field (screen_background, screen_background)
    procedure Erase_Field (Field_No : in Afpx_Typ.Absolute_Field_Range);
   
    -- The put_then get
    procedure Ptg (Cursor_Field : in out Afpx_Typ.Field_Range;
                   Cursor_Col   : in out Con_Io.Col_Range;
                   Result       : out Result_Rec;
                   Redisplay    : in Boolean;
                   Get_Active   : in Boolean);
   
  end Af_Ptg;

  package Af_List is
   
    -- Open / Re-open the list window
    procedure Open;
   
    -- Display the list, starting from FIRST_ITEM
    -- Has to be called each time the list changes
    --  or colors are modified
    procedure Display (First_Item_Id : in Positive);
   
    -- Update the list due to an action
    procedure Update (Action : in List_Action_List);
   
    -- Set the current item (selected_color) of the list
    procedure Set_Selected (Item_Id : in Positive);
   
    -- The current status of the list
    type Status_Rec is record
      -- The number of items diplayed
      -- (width if list_length >= width), list_length otherwise
      Nb_Rows : Natural;
      -- First and last items displayed in the window
      Id_Top    : Natural;
      Id_Bottom : Natural;
      -- Item selected
      Id_Selected : Natural;
    end record;
   
    function Get_Status return Status_Rec;
   
    -- Set current item of list according to ID_SELECTED
    procedure Set_Current;
   
    -- Put a row in a state
    procedure Put (Row : in Con_Io.Row_Range; State : in Af_Ptg.State_List);
   
    -- Is an ID, a row displayed
    function Id_Displayed (Id : Positive) return Boolean;
    function Row_Displayed (Row : Con_Io.Row_Range) return Boolean;
   
    -- ROW <-> Item ID
    function To_Row (Id : Positive) return Con_Io.Row_Range;
    function To_Id  (Row : Con_Io.Row_Range) return Positive;
   
    Not_Opened, Not_Displayed : exception;
  end Af_List;

  package body Af_Ptg is separate;
  package body Af_List is separate;



  -- Set current descriptor (read descriptor description)
  procedure Use_Descriptor (Descriptor_No : in Descriptor_Range;
  Clear_Screen : in Boolean := True) is
  begin
    Con_Io.Init;
    Af_Dscr.Load_Dscr (Afpx_Typ.Descriptor_Range (Descriptor_No));
    Af_List.Open;
    Af_Dscr.Current_Dscr.Modified := True;
    if Clear_Screen then
      Con_Io.Clear (Con_Io.Screen);
    end if;
  end Use_Descriptor;

  -- Clear the content of a field
  procedure Clear_Field (Field_No : in Field_Range) is
    Fn : constant Afpx_Typ.Field_Range := Afpx_Typ.Field_Range(Field_No);
    Field : Afpx_Typ.Field_Rec;
    Field_Size : Positive;
  begin
    Af_Dscr.Check(Fn);
    Field := Af_Dscr.Fields(Fn);
    Field_Size := Field.Height * Field.Width;
    -- Copy the nb_chars from init_str to char_str
    for I in Field.Char_Index .. Field.Char_Index + Field_Size - 1 loop
      Af_Dscr.Chars(I) := ' ';
    end loop;
    Af_Dscr.Current_Dscr.Modified := True;
   end Clear_Field;

  -- Reset the field from initial definition in file
  procedure Reset_Field (Field_No : in Absolute_Field_Range;
  Reset_Colors : in Boolean := True;
  Reset_String : in Boolean := True) is
    Fn : constant Afpx_Typ.Field_Range := Afpx_Typ.Field_Range(Field_No);
  begin
    Af_Dscr.Check(Fn);
    Af_Dscr.Load_Field (Fn, Reset_Colors, Reset_String);
    Af_Dscr.Current_Dscr.Modified := True;
  end Reset_Field;

  -- Field width
  function Get_Field_Width (Field_No : in Absolute_Field_Range)
                           return Width_Range is
    Fn : constant Afpx_Typ.Absolute_Field_Range
       := Afpx_Typ.Absolute_Field_Range(Field_No);
  begin
    Af_Dscr.Check(Fn);
    return Af_Dscr.Fields(Fn).Width;
  end Get_Field_Width;

  -- Field size
  procedure Get_Field_Size (Field_No : in Absolute_Field_Range;
                            Height : out Height_Range;
                            Width  : out Width_Range) is
    Fn : constant Afpx_Typ.Absolute_Field_Range
       := Afpx_Typ.Absolute_Field_Range(Field_No);
  begin
    Af_Dscr.Check(Fn);
    Height := Af_Dscr.Fields(Fn).Height;
    Width  := Af_Dscr.Fields(Fn).Width;
  end Get_Field_Size;

  -- Encode a string in a row of a field
  procedure Encode_Field (Field_No : in Field_Range;
                          From_Pos : in Con_Io.Square;
                          Str      : in String) is
    Fn : constant Afpx_Typ.Field_Range := Afpx_Typ.Field_Range(Field_No);
    Field : Afpx_Typ.Field_Rec;
    Init_Index : Afpx_Typ.Char_Str_Range;
  begin
    Af_Dscr.Check(Fn);
    Field := Af_Dscr.Fields(Fn);
    -- Check that square is in field
    if not Afpx_Typ.In_Field (Field, From_Pos) then
      raise Invalid_Square;
    end if;
   
    -- Check that FROM_POS.COL + STR is length compatible with field width
    if Str'Length /= 0
        and then not Afpx_Typ.In_Field (Field,
            (From_Pos.Row, From_Pos.Col + Str'Length - 1)) then
      raise String_Too_Long;
    end if;
    -- Copy in init string
    Init_Index := Field.Char_Index
          + From_Pos.Row * Field.Width
          + From_Pos.Col;
    Af_Dscr.Chars (Init_Index .. Init_Index + Str'Length - 1) := Str;
    Af_Dscr.Current_Dscr.Modified := True;
  end Encode_Field;

  procedure Encode_Field (Field_No : in Field_Range;
    From_Pos : in Con_Io.Square;
    Str      : in Str_Txt) is
  begin
    Encode_Field (Field_No, From_Pos, Text_Handler.Value (Str));
  end Encode_Field;



  -- Decode the content of a row of a field
  procedure Decode_Field (Field_No : in Field_Range;
                          Row      : in Con_Io.Row_Range;
                          Str      : in out Str_Txt) is
    Fn : constant Afpx_Typ.Field_Range := Afpx_Typ.Field_Range(Field_No);
    Field : Afpx_Typ.Field_Rec;
    Init_Index : Afpx_Typ.Char_Str_Range;
  begin
    Af_Dscr.Check(Fn);
    Field := Af_Dscr.Fields(Fn);
    -- Check that row is in field
    if not Afpx_Typ.In_Field (Field, (Row, 0)) then
      raise Invalid_Row;
    end if;
    -- Copy in init string
    Init_Index := Field.Char_Index + Row * Field.Width;
    -- Return FIELD.WIDTH characters
    Text_Handler.Set (Str,
         Af_Dscr.Chars (Init_Index .. Init_Index + Field.Width - 1));
  end Decode_Field;

  -- Decode the content of a row of a field
  function Decode_Field (Field_No : Field_Range; Row : Con_Io.Row_Range)
  return String is
    Str : Str_Txt;
  begin
    Decode_Field (Field_No, Row, Str);
    return Text_Handler.Value (Str);
  end Decode_Field;


  -- Get field colors
  procedure Get_Field_Colors (Field_No : in Absolute_Field_Range;
                              Foreground : out Con_Io.Effective_Colors;
                              Blink_Stat : out Con_Io.Effective_Blink_Stats;
                              Background : out Con_Io.Effective_Basic_Colors;
                              Selected   : out Con_Io.Effective_Basic_Colors) is
    Fn : constant Afpx_Typ.Absolute_Field_Range
       := Afpx_Typ.Absolute_Field_Range(Field_No);
    Field : Afpx_Typ.Field_Rec;
  begin
    Af_Dscr.Check(Fn);
    Field := Af_Dscr.Fields(Fn);
    Foreground := Field.Colors.Foreground;
    Blink_Stat := Field.Colors.Blink_Stat;
    Background := Field.Colors.Background;
    Selected   := Field.Colors.Selected;
  end Get_Field_Colors;

  -- Set field colors
  procedure Set_Field_Colors (Field_No   : in Absolute_Field_Range;
                 Foreground : in Con_Io.Colors       := Con_Io.Current;
                 Blink_Stat : in Con_Io.Blink_Stats  := Con_Io.Current;
                 Background : in Con_Io.Basic_Colors := Con_Io.Current;
                 Selected   : in Con_Io.Basic_Colors := Con_Io.Current) is
    Fn : constant Afpx_Typ.Absolute_Field_Range
       := Afpx_Typ.Absolute_Field_Range(Field_No);
    Field : Afpx_Typ.Field_Rec;
    use Con_Io;
    use Afpx_Typ;
  begin
    Af_Dscr.Check(Fn);
    Field := Af_Dscr.Fields(Fn);
    -- Check FOREGROUND is BASIC_COLORS for list, get and button fields
    if Foreground not in Con_Io.Basic_Colors
        and then Field.Kind /= Afpx_Typ.Put then
      raise Invalid_Color;
    end if;
    -- Check BLINK_STAT is CURRENT except for PUT
    if Blink_Stat /= Con_Io.Current
        and then (Field.Kind /= Afpx_Typ.Put) then
      raise Invalid_Color;
    end if;
   
    -- Check SELECTED is CURRENT for put and button fields
    if Selected /= Con_Io.Current
        and then (Field.Kind = Afpx_Typ.Put
                  or else Field.Kind = Afpx_Typ.Button) then
      raise Invalid_Color;
    end if;
   
    -- Copy colors if not current
    if Foreground /= Con_Io.Current then
      Field.Colors.Foreground := Foreground;
    end if;
    if Blink_Stat /= Con_Io.Current then
      Field.Colors.Blink_Stat := Blink_Stat;
    end if;
    if Background /= Con_Io.Current then
      Field.Colors.Background := Background;
    end if;
    if Selected /= Con_Io.Current then
      Field.Colors.Selected := Selected;
    end if;
    -- Affect if fields_array
    Af_Dscr.Fields(Fn) := Field;
    Af_Dscr.Current_Dscr.Modified := True;
  end Set_Field_Colors;

  -- Activate/Desactivate a field for further put_then_get
  procedure Set_Field_Activation (Field_No : in Absolute_Field_Range;
                                  Activate : in Boolean) is
    Fn : constant Afpx_Typ.Absolute_Field_Range
       := Afpx_Typ.Absolute_Field_Range(Field_No);
  begin
    Af_Dscr.Check(Fn);
    Af_Dscr.Fields(Fn).Activated := Activate;
    Af_Dscr.Current_Dscr.Modified := True;
  end Set_Field_Activation;

  procedure Get_Field_Activation (Field_No : in Absolute_Field_Range;
                                  Activate : out Boolean) is
    Fn : constant Afpx_Typ.Absolute_Field_Range
       := Afpx_Typ.Absolute_Field_Range(Field_No);
  begin
    Af_Dscr.Check(Fn);
    Activate := Af_Dscr.Fields(Fn).Activated;
  end Get_Field_Activation;

  -- Protect/Unprotect a GET for further put_then_gets
  -- A non active field is not displayed by put_then get
  -- All fields are activated by default (when USE_DESCRIPTOR or RESET_FIELD)
  -- Exceptions : NO_DESCRIPTOR, INVALID_FIELD
  procedure Set_Field_Protection (Field_No : in Absolute_Field_Range;
                                  Protect  : in Boolean) is
    Fn : constant Afpx_Typ.Absolute_Field_Range
       := Afpx_Typ.Absolute_Field_Range(Field_No);
    use Afpx_Typ;
  begin
    Af_Dscr.Check(Fn);
    if Af_Dscr.Fields(Fn).Kind = Afpx_Typ.Put then
      raise Invalid_Field;
    end if;
    Af_Dscr.Fields(Fn).Isprotected := Protect;
    Af_Dscr.Current_Dscr.Modified := True;
  end Set_Field_Protection;

  procedure Get_Field_Protection (Field_No : in Absolute_Field_Range;
                                  Protect  : out Boolean) is
    Fn : constant Afpx_Typ.Absolute_Field_Range
       := Afpx_Typ.Absolute_Field_Range(Field_No);
     use Afpx_Typ;
  begin
    Af_Dscr.Check(Fn);
    if Af_Dscr.Fields(Fn).Kind = Afpx_Typ.Put then
      raise Invalid_Field;
    end if;
    Protect := Af_Dscr.Fields(Fn).Isprotected;
  end Get_Field_Protection;

  -- Erase all the fields of the descriptor from the screen
  procedure Erase is
    Background : constant Con_Io.Effective_Basic_Colors
               := Con_Io.Get_Background (Con_Io.Screen);
    use Afpx_Typ;
  begin
    Af_Dscr.Check;
    if Af_Dscr.Fields(Lfn).Kind = Afpx_Typ.Button then
      Af_Ptg.Erase_Field (Lfn);
    end if;
    for I in 1 .. Af_Dscr.Current_Dscr.Nb_Fields loop
      Af_Ptg.Erase_Field (I);
    end loop;
    Con_Io.Flush;
  end Erase;

  -- Put all the fields of the descriptor on the screen
  procedure Put is
    use Afpx_Typ;
  begin
    Af_Dscr.Check;
    -- Check no list active in descriptor
    if Af_Dscr.Fields(Lfn).Kind = Afpx_Typ.Button then
      if Af_Dscr.Fields (Lfn).Activated then
        raise List_In_Put;
      else
        Af_Ptg.Erase_Field (Lfn);
      end if;
    end if;
   
    -- Put all fields
    for I in 1 .. Af_Dscr.Current_Dscr.Nb_Fields loop
      if Af_Dscr.Fields (I).Activated then
        Af_Ptg.Put_Field (I, Af_Ptg.Normal);
      else
        Af_Ptg.Erase_Field (I);
      end if;
    end loop;
    Con_Io.Flush;
  end Put;

  -- Computes next cursor field after current one:
  function Next_Cursor_Field (From : Absolute_Field_Range)
                             return Absolute_Field_Range is
    Ret_No : Afpx_Typ.Absolute_Field_Range;
    use Afpx_Typ;
  begin
    Ret_No := Afpx_Typ.Absolute_Field_Range(From);
    loop
      if Ret_No /= Af_Dscr.Current_Dscr.Nb_Fields then
        Ret_No := Ret_No + 1;
      else
        Ret_No := 1;
      end if;
      if Af_Dscr.Fields(Ret_No).Kind = Afpx_Typ.Get
          and then Af_Dscr.Fields(Ret_No).Activated
          and then not Af_Dscr.Fields(Ret_No).Isprotected then
        return Absolute_Field_Range(Ret_No);
      elsif Ret_No = Afpx_Typ.Absolute_Field_Range(From) then
        return 0;
      elsif From = 0 and then Ret_No = Af_Dscr.Current_Dscr.Nb_Fields then
        return 0;
      end if;
    end loop;
  end Next_Cursor_Field;

  -- Computes previous cursor field before current one:
  function Prev_Cursor_Field (From : Absolute_Field_Range)
                             return Absolute_Field_Range is
  Ret_No : Afpx_Typ.Absolute_Field_Range;
  use Afpx_Typ;
  begin
    Ret_No := Afpx_Typ.Absolute_Field_Range(From);
    loop
      if Ret_No /= 1 then
        Ret_No := Ret_No - 1;
      else
        Ret_No := Af_Dscr.Current_Dscr.Nb_Fields;
      end if;
      if Af_Dscr.Fields(Ret_No).Kind = Afpx_Typ.Get
          and then Af_Dscr.Fields(Ret_No).Activated
          and then not Af_Dscr.Fields(Ret_No).Isprotected then
        return Absolute_Field_Range(Ret_No);
      elsif Ret_No = Afpx_Typ.Absolute_Field_Range(From) then
        return 0;
      elsif From = 0 and then Ret_No = 1 then
        return 0;
      end if;
    end loop;
  end Prev_Cursor_Field;

  procedure Update_List (Action : in List_Action_List) is
  begin
    Af_Dscr.Check(Lfn);
    Af_List.Update(Action);
 end Update_List;


  -- Print the fields and the list, then gets
  procedure Put_Then_Get (
              Cursor_Field : in out Field_Range;
              Cursor_Col   : in out Con_Io.Col_Range;
              Result       : out Result_Rec;
              Redisplay    : in Boolean := False) is
    Some_Get : Boolean;
    Cf : Afpx_Typ.Field_Range := Afpx_Typ.Field_Range(Cursor_Field);
    use Afpx_Typ;
  begin
    Af_Dscr.Check;
    -- Check if some active get field in the descriptor
    Some_Get := False;
    for I in 1 .. Af_Dscr.Current_Dscr.Nb_Fields loop
      if Af_Dscr.Fields(I).Kind = Afpx_Typ.Get
          and then Af_Dscr.Fields (I).Activated
          and then not Af_Dscr.Fields (I).Isprotected then
        Some_Get := True;
        exit;
      end if;
    end loop;
    -- Check cursor pos if some get field active
    if Some_Get then
      Af_Dscr.Check (Cf);
      if Af_Dscr.Fields(Cf).Kind /= Afpx_Typ.Get
          or else  not Af_Dscr.Fields(Cf).Activated
          or else      Af_Dscr.Fields(Cf).Isprotected then
        raise Invalid_Field;
      end if;
      if Cursor_Col >= Af_Dscr.Fields(Cf).Width then
        raise Invalid_Col;
      end if;
    end if;
   
    Af_Ptg.Ptg (Cf, Cursor_Col, Result, Redisplay, Some_Get);
    Cursor_Field := Field_Range(Cf);
  end Put_Then_Get;

end Afpx;

