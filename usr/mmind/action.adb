with Rnd, Con_Io;
with Common, Screen, Response;
package body Action is


  Level : Common.Last_Level_Range;

  Playing : Boolean;

  Cur_Selection : Screen.Selection_Rec;
  Last_Click : Screen.Selection_Rec;

  First_Free : Common.Propal_Range;

  procedure Init is
  begin
    Screen.Init;
    Rnd.Randomize;
    Level := Common.Get_Level;
  end Init;

  procedure End_Action is
  begin
    null;
  end End_Action;

  procedure Update_Try (Propal : in Common.Propal_Range) is
    Prop_State : constant Common.Propal_State_Rec
               := Common.Get_Propal_State (Propal);
    use Common;
  begin
    for I in Common.Level_Range
     range Common.Level_Range'First .. Level loop
      if Prop_State.Propal_Color(I) = 0 then
        Common.Set_Try_State (Propal, Common.Not_Set);
        Screen.Put_Try (Propal, Screen.Cannot_Try);
        return;
      end if;
    end loop;
    Common.Set_Try_State (Propal, Common.Can_Try);
    Screen.Put_Try (Propal, Screen.Can_Try);
  end Update_Try;


  procedure Treat_Click is separate;

  procedure Treat_Release (Go_On, Exit_Game : out Boolean) is separate;


  -- True if start again, False if exit
  function Play return Boolean is
    Clicked : Boolean := False;
    Go_On, Exit_Game : Boolean;
    Scr : constant Con_Io.Window := Con_Io.Get_Screen (Screen.Console'Access);
  begin

    -- Start new game - playing
    Level := Common.Get_Level;
    Common.Reset_State;
    Screen.Init (Level);
    Screen.Set_Mouse_Default_Color;

    First_Free := Common.Propal_Range'First;
    Response.New_Code;

    Playing := True;
    Screen.Put_Start_Giveup (Start => False, Selected => False);
    Screen.Put_Help (Screen.Released);
    Screen.Put_Current_Level (Level);


    Main:
    loop

      declare
        Str : Con_Io.Unicode_Sequence (1 .. 0);
        Last : Natural;
        Stat : Con_Io.Curs_Mvt;
        Pos : Positive;
        Ins : Boolean;
        Mouse_Status : Con_Io.Mouse_Event_Rec;
        use Screen, Con_Io;
      begin
        Wait_Event:
        loop
          Scr.Get (Str, Last, Stat, Pos, Ins, Echo => False);
          if Stat = Con_Io.Mouse_Button then
            Screen.Console.Get_Mouse_Event (Mouse_Status);
            if Mouse_Status.Button = Con_Io.Left then
              -- exit on new event
              exit Wait_Event when Clicked
                 xor (Mouse_Status.Status = Con_Io.Pressed);
            end if;
          elsif Stat = Con_Io.Refresh then
            Screen.Init (Level);
            -- Put colors
            declare
              Propal : Common.Propal_State_Rec(Level);
              Placed_Ok, Colors_Ok : Natural;
              use Common;
            begin
              for I in Common.Propal_Range loop
                Propal := Common.Get_Propal_State(I);
                for J in 1 .. Level loop
                  Screen.Put_Color (I, J, Propal.Propal_Color(J));
                end loop;
                if Propal.Try = Common.Can_Try then
                  Screen.Put_Try (I, Screen.Can_Try);
                elsif Propal.Try = Common.Answered then
                  Common.Get_Answer (I, Placed_Ok, Colors_Ok);
                  Screen.Put_Answer (I, Placed_Ok, Colors_Ok);
                end if;
              end loop;
            end;
            if Playing then
              Screen.Put_Start_Giveup (Start => False, Selected => False);
              Screen.Put_Help (Screen.Released);
              Screen.Put_Current_Level (Level);
            else
              declare
                Code : Response.Color_Rec(Level);
              begin
                Code := Response.Get_Code;
                for J in 1 .. Level loop
                  Screen.Put_Secret_Color(J, Code.Color(J));
                end loop;
              end;
              Screen.Put_Start_Giveup (Start => True, Selected => False);
              Screen.Put_Help (Screen.Start);
              Screen.Put_Current_Level (Common.Get_Stored_Level);
            end if;
          elsif Stat = Con_Io.Break then
            Screen.Clear;
            End_Action;
            return False;
          end if;
        end loop Wait_Event;
        Clicked := not Clicked;
        Screen.Get_Selected ( Where => (Row => Mouse_Status.Row,
                                        Col => Mouse_Status.Col),
                              What => Cur_Selection);
      end;


      if Clicked then
        Treat_Click;
      else
        Treat_Release (Go_On, Exit_Game);
        exit Main when not Go_On;
      end if;

    end loop Main;

    if Exit_Game then
      Screen.Clear;
      End_Action;
    end if;

    return not Exit_Game;
  exception
    when others =>
      Scr.Move;
      return Exit_Game;
  end Play;

end Action;

