with Afpx, Text_Handler;
with Points, File;
package Screen is

  -- The get field
  Get_Fld           : constant Afpx.Field_Range := 9;
  -- The scrool buttons
  subtype List_Scroll_Fld_Range is Afpx.Field_Range range 11 .. 16;
  -- The Ok/Cancel buttons
  Ok_Button_Fld     : constant Afpx.Field_Range := 18;
  Cancel_Button_Fld : constant Afpx.Field_Range := 19;
  -- The exit/back button
  Exit_Button_Fld : constant Afpx.Field_Range := 17;

  -- The F(xxxxx)= field
  Yfx_Put_Fld : constant Afpx.Field_Range := 30;

  -- All the menues dependant fields
  subtype Menu_Fld_Range is Afpx.Field_Range range 20 .. 31;

  -- Return width of Get field
  function Get_Get_Width return Afpx.Width_Range;

  -- Max width of degree
  Max_Degree_Width : constant := 4;

  -- Put a title
  type S_Action_List is (Data, Read_Points, Write_Points, New_Points,
      Modify_1, Add_1, Suppress_1, Approximate, Sort_Points,
      Get_Degree, Polynom, Y_F_X, Scales, Boundaries, Curve, Exit_Approx);
  procedure Put_Title (S_Action : in S_Action_List; Option : in Boolean := False);

  -- Truncate head of string:  "> " & truncated head
  -- Or or padds with spaces
  function Procuste (Str : String; Len : Positive) return String;

  -- Put file name
  procedure Put_File (File_Name : in File.F_T_File_Name);

  -- Scroll the list
  procedure Scroll (Fld_No : in List_Scroll_Fld_Range);

  -- Put/hide info. Display error. Confirm
  type S_Message_List is (
      I_Clear, I_File_Name, I_X, I_Y, I_Xmin, I_Ymin, I_Xmax, I_Ymax,
      I_Degree, I_Scales, I_Wait, 
      C_File_Exists, C_Delete_Point, C_Go_On, C_Data_Lost,
      E_Done, E_File_Not_Found, E_Io_Error, E_File_Name,
      E_No_Data, E_Wrong_Degree, E_Wrong_Coordinate,
      E_Resolution_Problem, E_Curve_Problem, E_Curve_Active,
      E_Too_Many_Points);

  subtype S_Info_List is S_Message_List range I_Clear .. I_Wait;
  subtype S_Confirm_List is S_Message_List range C_File_Exists .. C_Data_Lost;
  subtype S_Error_List is S_Message_List
                          range E_Done .. E_Too_Many_Points;

  procedure Inform  (Msg : in S_Info_List);
  function  Confirm (Msg : S_Confirm_List; Alert : Boolean;
                     Subtitle : Boolean := False) return Boolean; 
  procedure Error   (Msg : in S_Error_List; Subtitle : in Boolean := False);


  -- Update number and status of points
  procedure Put_Point_Status;

  -- Init for file search, for get coordinate...
  procedure Init_For_Get (Cursor_Field : out Afpx.Field_Range;
                          Subtitle : in Boolean := False);

  -- Init screen for main menu1
  procedure Init_For_Main1 (Cursor_Field : out Afpx.Field_Range);

  -- Store current file_name for further menus
  -- Put stored file
  procedure Store_File;
  procedure Put_File;

  -- Init screen for main menu2
  procedure Init_For_Main2 (Cursor_Field : out Afpx.Field_Range);

  -- Init screen for main submenu21
  procedure Init_For_Main21 (Cursor_Field : out Afpx.Field_Range);

end Screen;

