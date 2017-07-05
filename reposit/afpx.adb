with Environ, Language;
package body Afpx is

  Console : aliased Con_Io.Console;
  Af_Con_Io : Con_Io.Window;
  Size : Con_Io.Square;

  Lfn : constant Absolute_Field_Range := List_Field_No;
  Afpx_Internal_Error : exception;

  package Af_Dscr is

    -- Current descriptor
    Current_Dscr : Afpx_Typ.Dscr_Rec;

    -- The fields of current descriptor
    Fields : Afpx_Typ.Fields_Array;

    -- Characters of the fields
    Chars : Afpx_Typ.Char_Str;
    -- Load the screen size
    function Load_Size return Con_Io.Square;

    -- Load a descriptor, raise No_Descriptor if invalid No
    procedure Load_Dscr (Dscr_No : in Descriptor_Range);

    -- Release a descriptor. Check will raise No_Descriptor
    procedure Release_Dscr;

    -- Check if a descriptor is being used (raise No_Descriptor)
    procedure Check;

    -- Check if a descriptor is being used
    function Is_Set return Boolean;

    -- Check if a descriptor has been used (raise No_Descriptor)
    --  and if Field_No is valid in it (raise Invalid_Field)
    procedure Check (Field_No : in Absolute_Field_Range);

    -- Check if a descriptor has been used (raise No_Descriptor)
    -- and it has the list field active
    function Has_List return Boolean;

    -- Load a field's characters and/or colors from init
    procedure Load_Field (Field_No : in Absolute_Field_Range;
                          Load_Colors : in Boolean;
                          Load_Chars  : in Boolean);

  end Af_Dscr;


  function In_Field_Absolute (Field_No : in Absolute_Field_Range;
                              Square : in Con_Io.Square) return Boolean is
  begin
    return Afpx_Typ.In_Field_Absolute (Af_Dscr.Fields(Field_No), Square);
  end In_Field_Absolute;

  package body Af_Dscr is separate;


  function Next_Field (Field_No : Absolute_Field_Range)
                      return Absolute_Field_Range is
    Ret_No : Absolute_Field_Range;
    use type Absolute_Field_Range;
  begin
    Ret_No := Field_No;
    loop
      if Ret_No /= Af_Dscr.Current_Dscr.Nb_Fields then
        Ret_No := Ret_No + 1;
      elsif Af_Dscr.Has_List then
        Ret_No := List_Field_No;
      else
        Ret_No := 1;
      end if;
      exit when Af_Dscr.Fields(Ret_No).Activated;
    end loop;
    return Ret_No;
  end Next_Field;

  function Prev_Field (Field_No : Absolute_Field_Range)
                      return Absolute_Field_Range is
    Ret_No : Absolute_Field_Range;
    use type Absolute_Field_Range;
  begin
    Ret_No := Field_No;
    loop
      if Ret_No = 0 then
        Ret_No := Af_Dscr.Current_Dscr.Nb_Fields;
      elsif Ret_No /= 1 then
        Ret_No := Ret_No - 1;
      elsif Af_Dscr.Has_List then
        Ret_No := List_Field_No;
      else
        Ret_No := Af_Dscr.Current_Dscr.Nb_Fields;
      end if;
      exit when Af_Dscr.Fields(Ret_No).Activated;
    end loop;
    return Ret_No;
  end Prev_Field;

  function Next_Get_Field (Field_No : Field_Range) return Field_Range is
    Fn : Absolute_Field_Range;
    use type Absolute_Field_Range, Field_Kind_List;
  begin
    Fn := Field_No;
    loop
      Fn := Next_Field(Fn);
      exit when Fn /= 0
          and then Af_Dscr.Fields(Fn).Kind = Get_Field
          and then not Af_Dscr.Fields(Fn).Isprotected;
    end loop;
    return Fn;
  end Next_Get_Field;

  function Prev_Get_Field (Field_No : Field_Range) return Field_Range is
    Fn : Absolute_Field_Range;
    use type Absolute_Field_Range, Field_Kind_List;
  begin
    Fn := Field_No;
    loop
      Fn := Prev_Field(Fn);
      exit when Fn /= 0
          and then Af_Dscr.Fields(Fn).Kind = Get_Field
          and then not Af_Dscr.Fields(Fn).Isprotected;
    end loop;
    return Fn;
  end Prev_Get_Field;


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
                          Background : out Con_Io.Effective_Colors);

    -- Set double-click delay
    Default_Double_Click_Delay : constant Double_Click_Delay_Range := 0.500;
    procedure Set_Double_Click_Delay (
      Double_Click_Delay : in Double_Click_Delay_Range);

    -- Put a whole field in attribute
    procedure Put_Fld (Field_No : in Field_Range;
                       State    : in State_List);

    -- Erase a field (screen_background, screen_background)
    procedure Erase_Fld (Field_No : in Absolute_Field_Range);

    -- Force redisplay at next Ptg
    procedure Redisplay;

    -- The put_then get
    procedure Ptg (Get_Handle    : in out Get_Handle_Rec;
                   Result        : out Result_Rec;
                   Right_Select  : in Boolean;
                   Get_Active    : in Boolean;
                   Cursor_Col_Cb : access
      function (Cursor_Field : Field_Range;
                New_Field : Boolean;
                Pointer_Col : Con_Io.Col_Range;
                Offset : Con_Io.Col_Range;
                Enter_Field_Cause : Enter_Field_Cause_List;
                Str : Unicode_Sequence) return Con_Io.Col_Range := null;
                   List_Change_Cb : access
      procedure (Action : in List_Change_List;
                 Status : in List_Status_Rec) := null);

  end Af_Ptg;

  package Af_List is

    -- Open / Re-open the list window
    procedure Open;

    -- Display the list, starting from First_Item
    -- Has to be called each time the list changes
    --  or colors are modified
    procedure Display (First_Item_Id : in Positive);

    -- Update the list due to an action, re-display the list or not
    -- True if update was possible (some change)
    function Update (Action : List_Action_List; Display : Boolean)
                    return Boolean;

    -- Set the current item (selected_color) of the list
    procedure Set_Selected (Button : in List_Button_List; Item_Id : in Natural);

    -- Get current status of the list (what is shown, what is selected)
    function Get_Status return List_Status_Rec;

    -- Percent of position of list in field
    -- 0 when list is shorter than field (including empty list)
    -- 1 => Top, 100 => Bottom
    function Get_Percent return Percent_Range;

    -- Get position in list corresponding to Percent
    function Get_Index (Percent : Percent_Range) return Natural;


    -- Set current item of list according to Ids_Selected(Left)
    procedure Set_Current;

    -- Put a row in a state, move to next (if possible and requested)
    procedure Put (Row : in Con_Io.Row_Range;
                   State : in Af_Ptg.State_List;
                   Move : in Boolean);

    -- Is an Id, a row displayed
    function Id_Displayed (Id : Positive) return Boolean;
    function Row_Displayed (Row : Con_Io.Row_Range) return Boolean;

    -- Row <-> Item Id
    function To_Row (Id : Positive) return Con_Io.Row_Range;
    function To_Id  (Row : Con_Io.Row_Range) return Positive;

    Not_Opened : exception;
  end Af_List;

  package body Af_Ptg is separate;
  package body Af_List is separate;

  -- No call to Ptg from a Ptg/Event callback.
  In_Ptg : Boolean := False;
  -- No "active" call in a Put_Then_Get callback
  procedure Check_Ptg is
  begin
    if In_Ptg then
      raise In_Put_Then_Get;
    end if;
  end Check_Ptg;

  -- Width and height of the screen
  procedure Get_Screen_Size (Height : out Height_Range;
                             Width  : out Width_Range) is
  begin
    Height := Size.Row + 1;
    Width := Size.Col + 1;
  end Get_Screen_Size;

  -- Local procedure to init double click dlay from ENV
  procedure Init_Double_Click_Delay_From_Env is
    Ms : Positive;
    Double_Click_Delay : Double_Click_Delay_Range;
  begin
    Ms := Positive'Value (Environ.Getenv ("AFPX_DOUBLE_CLICK_DELAY"));
    Double_Click_Delay := Duration (Ms) / 1000.0;
    Af_Ptg.Set_Double_Click_Delay (Double_Click_Delay);
  exception
    when others =>
      -- Invalid ENV value => discard
      null;
  end Init_Double_Click_Delay_From_Env;

  -- Set current descriptor (read descriptor description)
  procedure Use_Descriptor (Descriptor_No : in Descriptor_Range;
                            Clear_Screen : in Boolean := True) is
  begin
    Check_Ptg;
    Af_Dscr.Load_Dscr (Descriptor_No);
    if not Console.Is_Open then
      -- Done only once at first descriptor
      if Af_Dscr.Current_Dscr.Colors (Con_Io.Effective_Colors'First)
         /= Afpx_Typ.No_Color then
        -- If necessary, Set the colors when using the first descriptor
        Con_Io.Set_Colors (Afpx_Typ.To_Def (Af_Dscr.Current_Dscr.Colors));
      end if;
      -- Done only once at first descriptor
      Size := Af_Dscr.Load_Size;
      -- Create console and screen
      Console.Open (
              Font_No => 1,
              Row_Last => Size.Row,
              Col_Last => Size.Col,
              Def_Back => Af_Dscr.Current_Dscr.Background);
      Af_Con_Io.Set_To_Screen (Console'Access);
      -- Init double screen delay from ENV
      Init_Double_Click_Delay_From_Env;
    end if;
    -- Done at each descriptor
    Af_List.Open;
    Redisplay;
    Af_Con_Io.Set_Background (Af_Dscr.Current_Dscr.Background);
    if Clear_Screen then
      Af_Con_Io.Clear;
    end if;
  end Use_Descriptor;

  -- Get current descriptor no
  function Get_Descriptor return Descriptor_Range is
  begin
    Af_Dscr.Check;
    return Af_Dscr.Current_Dscr.Dscr_Index;
  end Get_Descriptor;

  -- Reset current descriptor (as if Use_Descriptor)
  procedure Reset_Descriptor (Clear_Screen : in Boolean := True) is
  begin
    Af_Dscr.Check;
    Use_Descriptor (Af_Dscr.Current_Dscr.Dscr_Index, Clear_Screen);
  end Reset_Descriptor;

  -- Close the Console
  procedure Release_Descriptor is
  begin
    Check_Ptg;
    Af_Dscr.Check;
    Console.Close;
    Af_Dscr.Release_Dscr;
  end Release_Descriptor;

  -- Is a descriptor in use
  function Is_Descriptor_Set return Boolean is
  begin
    return Af_Dscr.Is_Set;
  end Is_Descriptor_Set;

  -- The Con_Io Console used by Afpx
  function Get_Console return Con_Io.Console is
  begin
    Af_Dscr.Check;
    return Console;
  end Get_Console;

  -- Suspend and resume the descriptor
  procedure Suspend is
  begin
    Check_Ptg;
    Af_Dscr.Check;
    if Is_Suspended then
      raise Suspended;
    end if;
    Console.Suspend;
  end Suspend;

  procedure Resume is
  begin
    Check_Ptg;
    Af_Dscr.Check;
    if not Is_Suspended then
      raise Suspended;
    end if;
    Console.Resume;
  end Resume;

  function Is_Suspended return Boolean is
  begin
    Af_Dscr.Check;
    return Console.Is_Suspended;
  end Is_Suspended;

  --Get descriptor background color
  function Get_Descriptor_Background return Con_Io.Effective_Colors is
  begin
    Af_Dscr.Check;
    return Af_Dscr.Current_Dscr.Background;
  end Get_Descriptor_Background;

  -- Check if current descriptor defines a list
  -- Exceptions : No_Descriptor (no Descriptor in use)
  function Has_List return Boolean is
  begin
    Af_Dscr.Check;
    return Af_Dscr.Has_List;
  end Has_List;

  -- Returns the number of fields of current descriptor
  -- Exceptions : No_Descriptor (no Descriptor in use)
  function Nb_Fields return Absolute_Field_Range is
  begin
    Af_Dscr.Check;
    return Absolute_Field_Range(Af_Dscr.Current_Dscr.Nb_Fields);
  end Nb_Fields;

  -- Clear the content of a field
  procedure Clear_Field (Field_No : in Field_Range) is
    Fn : constant Field_Range := Field_No;
    Field : Afpx_Typ.Field_Rec;
    Field_Size : Positive;
  begin
    Af_Dscr.Check(Fn);
    Field := Af_Dscr.Fields(Fn);
    Field_Size := Field.Height * Field.Data_Len;
    -- Copy the nb_chars from init_str to char_str
    for I in Field.Char_Index .. Field.Char_Index + Field_Size - 1 loop
      Af_Dscr.Chars(I) := Con_Io.Space;
    end loop;
    Af_Dscr.Current_Dscr.Modified := True;
    Af_Dscr.Fields(Fn).Modified := True;
   end Clear_Field;

  -- Reset the field from initial definition in file
  procedure Reset_Field (Field_No : in Absolute_Field_Range;
                         Reset_Colors : in Boolean := True;
                         Reset_String : in Boolean := True) is
    Fn : constant Absolute_Field_Range := Field_No;
  begin
    Af_Dscr.Check(Fn);
    Af_Dscr.Load_Field (Fn, Reset_Colors, Reset_String);
    Af_Dscr.Current_Dscr.Modified := True;
    Af_Dscr.Fields(Fn).Modified := True;
  end Reset_Field;

  -- Field Height
  function Get_Field_Height (Field_No : Absolute_Field_Range)
                             return Height_Range is
    Fn : constant Absolute_Field_Range := Field_No;
  begin
    Af_Dscr.Check(Fn);
    return Af_Dscr.Fields(Fn).Height;
  end Get_Field_Height;

  -- Field width
  function Get_Field_Width (Field_No : in Absolute_Field_Range)
                            return Width_Range is
    Fn : constant Absolute_Field_Range := Field_No;
  begin
    Af_Dscr.Check(Fn);
    return Af_Dscr.Fields(Fn).Width;
  end Get_Field_Width;

  -- Field size
  procedure Get_Field_Size (Field_No : in Absolute_Field_Range;
                            Height : out Height_Range;
                            Width  : out Width_Range) is
    Fn : constant Absolute_Field_Range := Field_No;
  begin
    Af_Dscr.Check(Fn);
    Height := Af_Dscr.Fields(Fn).Height;
    Width  := Af_Dscr.Fields(Fn).Width;
  end Get_Field_Size;

  -- Data len
  function Get_Data_Len (Field_No : Absolute_Field_Range)
                         return Width_Range is
    Fn : constant Absolute_Field_Range := Field_No;
    use type Field_Kind_List;
  begin
    Af_Dscr.Check(Fn);
    if Af_Dscr.Fields(Fn).Kind /= Get_Field then
      raise Invalid_Field;
    end if;
    return Af_Dscr.Fields(Fn).Data_Len;
  end Get_Data_Len;

  -- Offset
  procedure Set_Offset (Field_No : in Absolute_Field_Range;
                        Offset : in Con_Io.Col_Range := 0) is
    Fn : constant Absolute_Field_Range := Field_No;
    use type Field_Kind_List;
  begin
    Af_Dscr.Check(Fn);
    if Af_Dscr.Fields(Fn).Kind /= Get_Field then
      raise Invalid_Field;
    end if;
    if Offset > Af_Dscr.Fields(Fn).Data_Len - Af_Dscr.Fields(Fn).Width then
      raise Invalid_Col;
    end if;
    Af_Dscr.Fields(Fn).Offset := Offset;
  end Set_Offset;

  -- Get field kind
  -- Exceptions : No_Descriptor, Invalid_Field
  -- type Field_Kind_List is (Put, Button, Get);
  function Get_Field_Kind (Field_No : in Field_Range) return Field_Kind_List is
    Fn : constant Absolute_Field_Range := Field_No;
  begin
    Af_Dscr.Check(Fn);
    return Af_Dscr.Fields(Fn).Kind;
  end Get_Field_Kind;

  function Is_Put_Kind    (Field_No : in Field_Range) return Boolean is
    use type Field_Kind_List;
  begin
    return Get_Field_Kind (Field_No) = Put_Field;
  end Is_Put_Kind;

  function Is_Button_Kind (Field_No : in Field_Range) return Boolean is
    use type Field_Kind_List;
  begin
    return Get_Field_Kind (Field_No) = Button_Field;
  end Is_Button_Kind;

  function Is_Get_Kind    (Field_No : in Field_Range) return Boolean is
    use type Field_Kind_List;
  begin
    return Get_Field_Kind (Field_No) = Get_Field;
  end Is_Get_Kind;

  -- Encode a string in a row of a field
  procedure Encode_Field (Field_No : in Field_Range;
                          From_Pos : in Con_Io.Square;
                          Str      : in String) is
  begin
    Encode_Field (Field_No, From_Pos, Language.String_To_Unicode (Str));
  end Encode_Field;

  procedure Encode_Wide_Field (Field_No : in Field_Range;
                               From_Pos : in Con_Io.Square;
                               Str      : in Wide_String) is
    Ustr : Unicode_Sequence (Str'Range);
  begin
    for I in Ustr'Range loop
      Ustr(I) := Language.Wide_To_Unicode (Str(I));
    end loop;
    Encode_Field (Field_No, From_Pos, Ustr);
  end Encode_Wide_Field;

  procedure Encode_Field (Field_No : in Field_Range;
                          From_Pos : in Con_Io.Square;
                          Str      : in Unicode_Sequence) is
    Fn : constant Field_Range := Field_No;
    Field : Afpx_Typ.Field_Rec;
    Init_Index : Afpx_Typ.Char_Str_Range;
  begin
    Af_Dscr.Check(Fn);
    Field := Af_Dscr.Fields(Fn);
    -- Check that square is in field
    if not Afpx_Typ.In_Field (Field, From_Pos) then
      raise Invalid_Square;
    end if;

    -- Check that From_Pos.Col + Wstr length is compatible with field width
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
    Af_Dscr.Fields(Fn).Modified := True;
  end Encode_Field;

  procedure Encode_Field (Field_No : in Field_Range;
                          From_Pos : in Con_Io.Square;
                          Str      : in As.U.Asu_Us) is
  begin
    Encode_Field (Field_No, From_Pos, Str.Image);
  end Encode_Field;

  -- Decode the content of a row of a field
  function Decode_Field (Field_No : Field_Range;
                         Row      : Con_Io.Row_Range;
                         Adjust   : Boolean := True) return String is
    Str : constant String
        := Language.Unicode_To_String (Decode_Field (Field_No, Row));
    Fn : constant Absolute_Field_Range := Field_No;
    Width : constant Width_Range := Af_Dscr.Fields(Fn).Data_Len;
  begin
    if not Adjust or else Str'Length = Width then
      return Str;
    else
      declare
        -- Trunc length if <= Width
        Trunc : constant String := Language.Adjust (Str, Width);
        Pad : constant String (1 .. Width - Trunc'Length)
            := (others => ' ');
      begin
        return Trunc & Pad;
      end;
    end if;
  end Decode_Field;

  function Decode_Wide_Field (Field_No : Field_Range;
                              Row      : Con_Io.Row_Range)
                              return Wide_String is
    Ustr : constant Unicode_Sequence := Decode_Field (Field_No, Row);
  begin
    return Language.Copy (Ustr);
  end Decode_Wide_Field;

  function Decode_Field (Field_No : Field_Range;
                         Row      : Con_Io.Row_Range)
                         return Unicode_Sequence is
    Fn : constant Field_Range := Field_No;
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
    Init_Index := Field.Char_Index + Row * Field.Data_Len;
    -- Return characters in a String (1 .. N)
    declare
      Str : constant Unicode_Sequence (1 .. Field.Data_Len)
           := Af_Dscr.Chars (Init_Index .. Init_Index + Field.Data_Len - 1);
    begin
      return Str;
    end;
  end Decode_Field;

  procedure Decode_Field (Field_No : in Field_Range;
                          Row      : in Con_Io.Row_Range;
                          Str      : in out As.U.Asu_Us;
                          Adjust   : in Boolean := True) is
  begin
    Str := As.U.Tus (Decode_Field (Field_No, Row, Adjust));
  end Decode_Field;

  -- Get field colors
  procedure Get_Field_Colors (Field_No : in Absolute_Field_Range;
                              Foreground : out Con_Io.Effective_Colors;
                              Background : out Con_Io.Effective_Colors;
                              Selected   : out Con_Io.Effective_Colors) is
    Fn : constant Absolute_Field_Range := Field_No;
    Field : Afpx_Typ.Field_Rec;
  begin
    Af_Dscr.Check(Fn);
    Field := Af_Dscr.Fields(Fn);
    Foreground := Field.Colors.Foreground;
    Background := Field.Colors.Background;
    Selected   := Field.Colors.Selected;
  end Get_Field_Colors;

  -- Set field colors
  procedure Set_Field_Colors (Field_No   : in Absolute_Field_Range;
                 Foreground : in Con_Io.Colors       := Con_Io.Current;
                 Background : in Con_Io.Colors := Con_Io.Current;
                 Selected   : in Con_Io.Colors := Con_Io.Current) is
    Fn : constant Absolute_Field_Range := Field_No;
    Field : Afpx_Typ.Field_Rec;
    use type Field_Kind_List, Con_Io.Colors;
  begin
    Af_Dscr.Check(Fn);
    Field := Af_Dscr.Fields(Fn);

    -- Check Selected is Current for put and button fields
    if Selected /= Con_Io.Current
        and then (Field.Kind = Put_Field
                  or else Field.Kind = Button_Field) then
      raise Invalid_Color;
    end if;

    -- Copy colors if not current
    if Foreground /= Con_Io.Current then
      Af_Dscr.Fields(Fn).Colors.Foreground := Foreground;
    end if;
    if Background /= Con_Io.Current then
      Af_Dscr.Fields(Fn).Colors.Background := Background;
    end if;
    if Selected /= Con_Io.Current then
      Af_Dscr.Fields(Fn).Colors.Selected := Selected;
    end if;
    -- Affect if fields_array
    Af_Dscr.Current_Dscr.Modified := True;
    Af_Dscr.Fields(Fn).Modified := True;
  end Set_Field_Colors;

  -- Activate/Desactivate a field for further put_then_get
  procedure Set_Field_Activation (Field_No : in Absolute_Field_Range;
                                  Activate : in Boolean) is
    Fn : constant Absolute_Field_Range := Field_No;
    use type Absolute_Field_Range, Field_Kind_List;
  begin
    if Fn = List_Field_No
    or else Af_Dscr.Fields(Fn).Kind = Get_Field then
      Check_Ptg;
    end if;
    Af_Dscr.Check(Fn);
    Af_Dscr.Fields(Fn).Activated := Activate;
    Af_Dscr.Current_Dscr.Modified := True;
    Af_Dscr.Fields(Fn).Modified := True;
  end Set_Field_Activation;

  function Get_Field_Activation (Field_No : in Absolute_Field_Range)
                                 return Boolean is
    Fn : constant Absolute_Field_Range := Field_No;
  begin
    Af_Dscr.Check(Fn);
    return Af_Dscr.Fields(Fn).Activated;
  end Get_Field_Activation;

  -- Protect/Unprotect a Get for further put_then_gets
  -- A non active field is not displayed by put_then get
  -- All fields are activated by default (when Use_Descriptor or Reset_Field)
  -- Exceptions : No_Descriptor, Invalid_Field
  procedure Set_Field_Protection (Field_No : in Absolute_Field_Range;
                                  Protect  : in Boolean) is
    Fn : constant Absolute_Field_Range := Field_No;
    use type Absolute_Field_Range, Field_Kind_List;
  begin
    if Fn = List_Field_No
    or else Af_Dscr.Fields(Fn).Kind = Get_Field then
      Check_Ptg;
    end if;
    Af_Dscr.Check(Fn);
    if Af_Dscr.Fields(Fn).Kind = Put_Field then
      raise Invalid_Field;
    end if;
    Af_Dscr.Fields(Fn).Isprotected := Protect;
    Af_Dscr.Current_Dscr.Modified := True;
    Af_Dscr.Fields(Fn).Modified := True;
  end Set_Field_Protection;

  function Get_Field_Protection (Field_No : in Absolute_Field_Range)
                                 return Boolean is
    Fn : constant Absolute_Field_Range := Field_No;
    use type Absolute_Field_Range, Field_Kind_List;
  begin
    Af_Dscr.Check(Fn);
    if Af_Dscr.Fields(Fn).Kind = Put_Field then
      raise Invalid_Field;
    end if;
    return Af_Dscr.Fields(Fn).Isprotected;
  end Get_Field_Protection;

  -- Erase all the fields of the descriptor from the screen
  procedure Erase is
  begin
    Check_Ptg;
    Af_Dscr.Check;
    -- Check no list active in descriptor
    if Af_Dscr.Has_List then
      if Af_Dscr.Fields (Lfn).Activated then
        raise List_In_Put;
      end if;
      Af_Ptg.Erase_Fld (Lfn);
    end if;
    for I in 1 .. Af_Dscr.Current_Dscr.Nb_Fields loop
      Af_Ptg.Erase_Fld (I);
    end loop;
    Console.Flush;
  end Erase;

  -- Put all the fields of the descriptor on the screen
  procedure Put is
  begin
    Check_Ptg;
    Af_Dscr.Check;
    -- Check no list active in descriptor
    if Af_Dscr.Has_List then
      if Af_Dscr.Fields (Lfn).Activated then
        raise List_In_Put;
      end if;
      Af_Ptg.Erase_Fld (Lfn);
    end if;

    -- Put all fields
    for I in 1 .. Af_Dscr.Current_Dscr.Nb_Fields loop
      if Af_Dscr.Fields (I).Activated then
        Af_Ptg.Put_Fld (I, Af_Ptg.Normal);
      else
        Af_Ptg.Erase_Fld (I);
      end if;
    end loop;
    Console.Flush;
  end Put;

  -- Computes next cursor field after current one:
  function Next_Cursor_Field (From : Absolute_Field_Range)
                             return Absolute_Field_Range is
    Start_No, Ret_No : Absolute_Field_Range;
    use type Absolute_Field_Range, Field_Kind_List;
  begin
    Af_Dscr.Check;
    -- Empty descriptor (or only a list)
    if Af_Dscr.Current_Dscr.Nb_Fields = 0 then
      return 0;
    end if;
    -- Start at 1 if From is 0 or above Nb
    Start_No := From;
    if Start_No = 0 or else Start_No > Af_Dscr.Current_Dscr.Nb_Fields then
      Start_No := 1;
    end if;
    -- Loop from From up to From, start at 1 if From is above Nb
    Ret_No := Start_No;
    loop
      -- Next field
      if Ret_No < Af_Dscr.Current_Dscr.Nb_Fields then
        Ret_No := Ret_No + 1;
      else
        Ret_No := 1;
      end if;
      -- Check it is a cursor field
      if Af_Dscr.Fields(Ret_No).Kind = Get_Field
          and then Af_Dscr.Fields(Ret_No).Activated
          and then not Af_Dscr.Fields(Ret_No).Isprotected then
        return Ret_No;
      elsif Ret_No = Start_No then
        -- All field checked without finding a cursor field
        return 0;
      end if;
    end loop;
  end Next_Cursor_Field;

  -- Computes previous cursor field before current one:
  function Prev_Cursor_Field (From : Absolute_Field_Range)
                             return Absolute_Field_Range is
    Ret_No : Absolute_Field_Range;
    use type Absolute_Field_Range, Field_Kind_List;
  begin
    Af_Dscr.Check;
    Ret_No := From;
    loop
      if Ret_No /= 1 then
        Ret_No := Ret_No - 1;
      else
        Ret_No := Af_Dscr.Current_Dscr.Nb_Fields;
      end if;
      if Af_Dscr.Fields(Ret_No).Kind = Get_Field
          and then Af_Dscr.Fields(Ret_No).Activated
          and then not Af_Dscr.Fields(Ret_No).Isprotected then
        return Ret_No;
      elsif Ret_No = From then
        return 0;
      elsif From = 0 and then Ret_No = 1 then
        return 0;
      end if;
    end loop;
  end Prev_Cursor_Field;

  -- Encode a string in a line for the list
  -- Exceptions : String_Too_Long
  procedure Encode_Line (Line : in out Line_Rec;
                         Str  : in String) is
  begin
    Encode_Line (Line, Language.String_To_Unicode (Str));
  end Encode_Line;

  procedure Encode_Line (Line : in out Line_Rec;
                         Str  : in Unicode_Sequence) is
  begin
    if Str'Length > Af_Dscr.Fields(Lfn).Width then
      raise String_Too_Long;
    end if;
    Line.Len := Str'Length;
    Line.Str (1 .. Line.Len) := Str;
  end Encode_Line;

  procedure Encode_Wide_Line (Line : in out Line_Rec;
                              Str  : in Wide_String) is
    Ustr : Unicode_Sequence (Str'Range);
  begin
    for I in Ustr'Range loop
      Ustr(I) := Language.Wide_To_Unicode (Str(I));
    end loop;
    Encode_Line (Line, Ustr);
  end Encode_Wide_Line;

  procedure Update_List (Action : in List_Action_List) is
    Dummy : Boolean;
  begin
    Af_Dscr.Check(Lfn);
    if not Af_Dscr.Fields(Lfn).Activated then
      raise Invalid_Field;
    end if;
    Dummy := Af_List.Update (Action, Display => False);
  end Update_List;

  -- Returns the index (from 0 to Str'Length-1) of the first character of Str
  --  or, if Significant, the index preceeding first significant character
  --  (skipping heading spaces and htabs).
  -- This can be usefully called by Cursor_Set_Col_Cb.
  function First_Index (Str : Unicode_Sequence; Significant : Boolean)
                        return Con_Io.Col_Range is
    N : Natural;
  begin
    if not Significant then
      return 0;
    end if;
    -- Locate first significant character
    N := 0;
    for I in Str'Range loop
      if Str (I) /= Con_Io.Space and then Str(I) /= Con_Io.Htab then
        N := I;
        exit;
      end if;
    end loop;
    if N = 0 then
      -- Not found: All is space/tab
      return 0;
    elsif N = Str'First then
      -- First char is significant, stay on it
      return 0;
    else
      -- Set to space before N
      return N - Str'First - 1;
    end if;
  end First_Index;

  -- Returns the index (from 0 to Str'Length-1) of the last character of Str
  --  or, if Significant, the index following last significant character
  --  (skipping trailing spaces and htabs).
  -- This can be usefully called by Cursor_Set_Col_Cb.
  function Last_Index (Str : Unicode_Sequence; Significant : Boolean)
                       return Con_Io.Col_Range is
    N : Natural;
  begin
    if not Significant then
      return Str'Length - 1;
    end if;
    -- Locate last significant character
    N := 0;
    for I in reverse Str'Range loop
      if Str (I) /= Con_Io.Space and then Str(I) /= Con_Io.Htab then
        N := I;
        exit;
      end if;
    end loop;
    if N = 0 then
      -- Not found: All is space/tab
      return 0;
    elsif N = Str'Last then
      -- Last character is significant. That's it.
      return Str'Length - 1;
    else
      -- Set to space after N
      return N - Str'First + 1;
    end if;
  end Last_Index;

   -- Percent of position of list in list field
  -- 0 when list is shorter than field (including empty list)
  -- 1 => Top, 100 => Bottom
  function Get_List_Percent return Percent_Range renames Af_List.Get_Percent;

   -- Get position in list corresponding to Percent
  function Get_List_Index (Percent : Percent_Range) return Natural
           renames Af_List.Get_Index;

  procedure Set_Double_Click_Delay (
    Double_Click_Delay : in Double_Click_Delay_Range)
    renames Af_Ptg.Set_Double_Click_Delay;


  -- Force redisplay at next Put_Then_Get
  procedure Redisplay is
  begin
    Af_Ptg.Redisplay;
  end Redisplay;

  -- Reset Handle (all fields except Cursor_Field)
  procedure Reset (Handle : in out Get_Handle_Rec) is
    Cursor_Field : constant Absolute_Field_Range := Handle.Cursor_Field;
  begin
    Handle := (others => <>);
    Handle.Cursor_Field := Cursor_Field;
  end Reset;

  -- Print the fields and the list, then gets
  procedure Put_Then_Get (Get_Handle    : in out Get_Handle_Rec;
                          Result        : out Result_Rec;
                          Right_Select  : in Boolean := False;
                          Cursor_Col_Cb : access
       function (Cursor_Field : Field_Range;
                 New_Field : Boolean;
                 Pointer_Col : Con_Io.Col_Range;
                 Offset : Con_Io.Col_Range;
                 Enter_Field_Cause : Enter_Field_Cause_List;
                 Str : Unicode_Sequence) return Con_Io.Col_Range := null;
                          List_Change_Cb : access
       procedure (Action : in List_Change_List;
                  Status : in List_Status_Rec) := null) is
    Some_Get : Boolean;
    Cf : Field_Range;
    use type Absolute_Field_Range, Field_Kind_List;
  begin
    -- No call to Put_Then_Get while syspended
    if Is_Suspended then
      raise Suspended;
    end if;
    Check_Ptg;
    -- Now we are in put then get until the end
    In_Ptg := True;
    Af_Dscr.Check;
    -- Check if some active get field in the descriptor
    Some_Get := (for some I in 1 .. Af_Dscr.Current_Dscr.Nb_Fields =>
        Af_Dscr.Fields(I).Kind = Get_Field
        and then Af_Dscr.Fields (I).Activated
        and then not Af_Dscr.Fields (I).Isprotected);
    -- Check cursor pos if some get field active
    if Some_Get then
      if Get_Handle.Cursor_Field = List_Field_No then
        Cf := Next_Cursor_Field (List_Field_No);
      else
        Cf := Get_Handle.Cursor_Field;
      end if;
      Af_Dscr.Check (Cf);
      if Af_Dscr.Fields(Cf).Kind /= Get_Field
          or else  not Af_Dscr.Fields(Cf).Activated
          or else      Af_Dscr.Fields(Cf).Isprotected then
        raise Invalid_Field;
      end if;
      if Get_Handle.Cursor_Col >= Af_Dscr.Fields(Cf).Width then
        raise Invalid_Col;
      end if;
    else
      Cf := 1;
    end if;
    Get_Handle.Cursor_Field := Cf;

    Af_Ptg.Ptg (Get_Handle, Result, Right_Select,
                Some_Get, Cursor_Col_Cb, List_Change_Cb);
    In_Ptg := False;
  exception
    when others =>
      In_Ptg := False;
      raise;
  end Put_Then_Get;

  -- Propose (mouse) selection to to other applications
  -- Clears if empty string
  procedure Set_Selection (Selection : in String) is
  begin
    Af_Dscr.Check;
    Console.Set_Selection (Selection);
  end Set_Selection;

  -- Ring a bell on screen
  procedure Bell (Repeat : in Positive := 1) is
  begin
    Af_Dscr.Check;
    Console.Bell (Repeat);
  end Bell;

end Afpx;

