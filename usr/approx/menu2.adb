with Afpx, Curve, My_Math;
with Points, Screen, Set_Points_List, Dialog, Resol, Afpx_Xref;
package body Menu2 is

  type Restore_List is (None, Partial, List, Full);
  Get_Handle : Afpx.Get_Handle_Rec;

  package Curve_Data is
    Nb_Points : Natural;
    The_Points : Points.P_T_The_Points(1 .. Points.Max_Number);
    The_Degree : Resol.R_T_Degree;
    The_Solution : Resol.Vector (1 .. Resol.R_T_Degree'Last + 1);
    The_Bounds : Curve.T_Boundaries;
    The_Bounds_Set : Boolean;
    function Curve_F_X (X : Points.P_T_Coordinate)
                       return Points.P_T_Coordinate;
    procedure Curve_Draw (Boundaries : in Curve.T_Boundaries;
                          Points : in Curve.T_The_Points);
  end Curve_Data;

  task Curve_Task is
    -- Start computation, Solution is computed on main stack
    entry Start (Solution : in Resol.Vector; Ok : out Boolean);
    entry Stopped;
  end Curve_Task;

  package Menu21 is

    -- Return the bounds previoulsy set (if set)
    function Bounds_Set return Boolean;
    procedure Reset_Bounds;
    procedure Get_Bounds (Set : out Boolean; Bounds : out Curve.T_Boundaries);

    -- Interactive selection of bounds
    procedure Main_Screen;

  end Menu21;
  package body Menu21 is separate;

  procedure Do_Restore (Restore : in Restore_List) is
    Activate_No_Curve : constant Boolean := Curved_Stopped;
  begin
    case Restore is
      when None =>
        null;
      when Partial =>
        Afpx.Use_Descriptor(Afpx_Xref.Compute.Dscr_Num, False);
        Get_Handle.Cursor_Field := Screen.Init_For_Main2;
        Screen.Put_File;
      when List =>
        -- polynom display needs reset of list
        Afpx.Use_Descriptor(Afpx_Xref.Compute.Dscr_Num, False);
        Set_Points_List;
        Get_Handle.Cursor_Field := Screen.Init_For_Main2;
        Screen.Put_File;
      when Full =>
        Afpx.Use_Descriptor(Afpx_Xref.Compute.Dscr_Num, True);
        Set_Points_List;
        Get_Handle.Cursor_Field := Screen.Init_For_Main2;
        Screen.Put_File;
    end case;
    -- Activate or not according to curve activity
    -- Back
    Afpx.Set_Field_Activation (Screen.Exit_Button_Fld, Activate_No_Curve);
    -- Set degree
    Afpx.Set_Field_Activation (Afpx_Xref.Compute.Set_Degree, Activate_No_Curve);
    -- Draw
    Afpx.Set_Field_Activation (Afpx_Xref.Compute.Draw, Activate_No_Curve);
    -- Set/View bounds
    if Activate_No_Curve then
      Afpx.Encode_Field(Afpx_Xref.Compute.Bounds, (1, 1), " Set");
    else
      Afpx.Encode_Field(Afpx_Xref.Compute.Bounds, (1, 1), "View");
    end if;
  end Do_Restore;

  function F_X (X : Points.P_T_Coordinate;
                    Polynom : Resol.Vector) return Points.P_T_Coordinate is
     Y : Points.P_T_Coordinate := 0.0;
     Bubble : Points.P_T_Coordinate := 1.0;
     use type My_Math.Real;
  begin
     -- Y = F(X) from vector
     for Deg of Polynom loop
       Y := Y + Deg * Bubble;
       Bubble := Bubble * X;
     end loop;
     return Y;
   end F_X;

  procedure Compute_Xy (Point : out Points.P_T_One_Point;  Ok : out Boolean) is
   Lp : Points.P_T_One_Point;
   Set : Boolean;
  begin
    Screen.Put_Title(Screen.Y_F_X);
    -- Get X
    Set := False;
    Lp := (0.0, 0.0);
    Dialog.Read_Coordinate (Screen.I_X, Set, Lp.X, Subtitle => True);
    Afpx.Set_Field_Activation (Screen.Get_Fld, False);
    if not Set then
      Ok := False;
      return;
    end if;

    -- Compute Y
    Screen.Inform(Screen.I_Wait);
    Ok := True;
    begin
      declare
        -- Resolution of problem
        Solution : constant Resol.Vector
                 := Resol.R_Resolution (Points.P_The_Points);
      begin
        Lp.Y := F_X(Lp.X, Solution);
      end;
    exception
      when others =>
        Screen.Error (Screen.E_Resolution_Problem);
        Ok := False;
    end;
    Screen.Inform(Screen.I_Clear);
    Point := Lp;
  end Compute_Xy;

  package body Curve_Data is
    function Curve_F_X (X : Points.P_T_Coordinate)
                    return Points.P_T_Coordinate is
    begin
      return F_X (X, The_Solution(1 .. The_Degree + 1));
    end Curve_F_X;
    procedure My_Draw is new Curve.Draw (Curve_F_X);
    procedure Curve_Draw (Boundaries : in Curve.T_Boundaries;
                          Points : in Curve.T_The_Points) is
    begin
      My_Draw (Boundaries, Points);
    end Curve_Draw;
  end Curve_Data;

  task body Curve_Task is
    Draw_It : Boolean;
    use Curve_Data;
  begin
    loop
      select
        accept Start (Solution : in Resol.Vector; Ok : out Boolean) do
          begin
            Nb_Points := Points.P_Nb;
            The_Points(1 .. Nb_Points) := Points.P_The_Points;
            The_Degree := Resol.R_Degree;
            The_Solution (1 .. The_Degree + 1) := Solution;
            Menu21.Get_Bounds (The_Bounds_Set, The_Bounds);
            Draw_It := The_Bounds_Set;
          exception
            when others =>
              Draw_It := False;
          end;
          if Draw_It then
            Draw_It := Curve.Init;
          end if;
          Ok := Draw_It;
        end Start;
      or
        accept Stopped do
          Draw_It := False;
        end Stopped;
      or
        terminate;
      end select;

      if Draw_It then
        begin
           Curve_Draw (The_Bounds, The_Points(1 .. Nb_Points));
        exception
          when others =>
            -- Draw error
            null;
        end;
      end if;
    end loop;
  end Curve_Task;

  function Curved_Stopped return Boolean is
   Ok : Boolean;
  begin
    select
      Curve_Task.Stopped;
      Ok := True;
    or
      -- Wait a bit to let Curve_Task be ready to accept Stopped
      delay 0.1;
      Ok := False;
    end select;
    return Ok;
  end Curved_Stopped;

  procedure Draw_Curve is
    Ok : Boolean;
  begin
    Screen.Put_Title(Screen.Curve);
    Screen.Inform(Screen.I_Wait);
    Curve_Task.Start (Resol.R_Resolution (Points.P_The_Points), Ok);
    Screen.Inform(Screen.I_Clear);
    -- Accept started if start Ok
    if not Ok then
      Screen.Error (Screen.E_Curve_Problem);
    end if;
  exception
    when others =>
      Screen.Error (Screen.E_Resolution_Problem);
  end Draw_Curve;


  procedure Main_Screen (Data_Changed : in Boolean) is
    Ptg_Result : Afpx.Result_Rec;
    Restore : Restore_List;

    use Afpx;

  begin
    Afpx.Use_Descriptor(Afpx_Xref.Compute.Dscr_Num);
    -- Try to keep previous data
    if Data_Changed then
      -- Or reset degree to max
      if Points.P_Nb - 1 < Resol.R_T_Degree'Last then
        Resol.R_Set_Degree(Points.P_Nb - 1);
      else
        Resol.R_Set_Degree(Resol.R_T_Degree'Last);
      end if;
      Resol.R_Points_Modification;
      Menu21.Reset_Bounds;
    end if;
    Get_Handle.Cursor_Field := Screen.Init_For_Main2;
    Screen.Put_File;

    -- Update Nb of points and save_status
    Screen.Put_Point_Status;

    Get_Handle.Cursor_Col := 0;
    Get_Handle.Insert := False;
    Restore := None;

    loop
      Do_Restore (Restore);


      Afpx.Put_Then_Get (Get_Handle, Ptg_Result);
      Restore := None;
      case Ptg_Result.Event is
        when Afpx.Keyboard =>
          case Ptg_Result.Keyboard_Key is
            when Afpx.Return_Key =>
              null;
            when Afpx.Escape_Key =>
              if not Curved_Stopped then
                Screen.Error (Screen.E_Curve_Active);
                Restore := Partial;
              else
                return;
              end if;
            when Afpx.Break_Key =>
              raise Screen.Exit_Requested;
          end case;
        when Afpx.Mouse_Button =>
          case Ptg_Result.Field_No is
            when Screen.List_Scroll_Fld_Range =>
              Screen.Scroll(Ptg_Result.Field_No);
            when Screen.Exit_Button_Fld =>
              return;
            when Afpx_Xref.Compute.Set_Degree =>
              Dialog.Read_Degree;
              Restore := Partial;
            when Afpx_Xref.Compute.Polynom =>
              -- Display polynom
              Screen.Put_Title(Screen.Polynom, True);
              Screen.Inform(Screen.I_Wait);
              -- Display
              begin
                Dialog.Put_Polynom (Resol.R_Resolution(Points.P_The_Points));
              exception
                when others =>
                  Screen.Error (Screen.E_Resolution_Problem);
              end;
              Restore := List;
            when Afpx_Xref.Compute.Yfx =>
              -- Y=f(x)
              declare
                Point : Points.P_T_One_Point;
                Ok : Boolean;
              begin
                loop
                  Compute_Xy (Point, Ok);
                  if Ok then
                    Ok := Dialog.Put_Yfx (Point);
                  end if;
                  exit when not Ok;
                end loop;
              end;
              Restore := Partial;
            when Afpx_Xref.Compute.Bounds =>
              -- Set boudaries
              Menu21.Main_Screen;
              Restore := Full;
            when Afpx_Xref.Compute.Draw =>
              -- Draw
              -- Set bounds if needed
              if not Menu21.Bounds_Set then
                Menu21.Main_Screen;
                Restore := Full;
              else
                Restore := None;
              end if;
              if Menu21.Bounds_Set then
                -- Restore for wait/error
                Do_Restore(Restore);
                Draw_Curve;
              end if;
              Restore := Full;
            when others =>
              null;
          end case;
        when Afpx.Fd_Event | Afpx.Timer_Event | Afpx.Signal_Event
           | Afpx.Refresh =>
          null;
      end case;
    end loop;

  end Main_Screen;

end Menu2;

