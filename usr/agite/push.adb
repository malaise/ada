with Ada.Exceptions;
with As.U.Utils, Con_Io, Afpx.List_Manager, Basic_Proc, Unicode;
with Utils.X, Git_If, Afpx_Xref, Error;
package body Push is

  procedure Set (Line : in out Afpx.Line_Rec;
                 From : in As.U.Asu_Us) is
  begin
    Afpx.Encode_Line (Line,
         Utils.Normalize (From.Image,
            Afpx.Get_Field_Width (Afpx.List_Field_No)) );
  exception
    when Error:others =>
      Basic_Proc.Put_Line_Error ("Exception "
          & Ada.Exceptions.Exception_Name (Error)
          & " raised in push on " & From.Image);
  end Set;

  procedure Init_List is new Afpx.List_Manager.Init_List (
    As.U.Asu_Us, As.U.Utils.Asu_List_Mng, Set);

  -- Afpx line list with "origin"
  Origin : Afpx.Line_Rec;
  function Match (Current, Criteria : Afpx.Line_Rec) return Boolean is
    use type Unicode.Unicode_Sequence;
  begin
    return Current.Str(4 .. Current.Len) = Criteria.Str(4 .. Criteria.Len);
  end Match;
  function Search is new Afpx.Line_List_Mng.Search (Match);

  -- The references
  References : Git_If.Reference_Mng.List_Type;

  -- Push
  function Do_Push return Boolean is
  begin
    References.Move_At (Afpx.Line_List.Get_Position);
    if Git_If.Do_Push (References.Access_Current.Image) then
      return True;
    else
      Error ("Pushing to", References.Access_Current.Image, "");
      return False;
    end if;
  end Do_Push;

  -- Handle the Push
  procedure Handle (Root : in String) is
    -- Afpx stuff
    Cursor_Field : Afpx.Field_Range;
    Cursor_Col   : Con_Io.Col_Range;
    Insert       : Boolean;
    Ptg_Result   : Afpx.Result_Rec;
    use type Afpx.Absolute_Field_Range;

    procedure Init is
    begin
      Afpx.Use_Descriptor (Afpx_Xref.Push.Dscr_Num);
      Cursor_Field := 1;
      Cursor_Col := 0;
      Insert := False;
      -- Encode Root
      Afpx.Encode_Field (Afpx_Xref.Push.Root, (0, 0),
          Utils.Normalize (Root, Afpx.Get_Field_Width (Afpx_Xref.Push.Root)));
      -- Encode current branch
      Afpx.Clear_Field (Afpx_Xref.Push.Branch);
      Afpx.Encode_Field (Afpx_Xref.Push.Branch, (0, 0),
          Utils.X.Branch_Image (Git_If.Current_Branch,
              Afpx.Get_Field_Width (Afpx_Xref.Push.Branch)));

      -- Get list of references
      Git_If.List_References (References);
      Init_List (References);
      if References.Is_Empty then
        Afpx.Set_Field_Activation (Afpx_Xref.Push.Push, False);
      else
        -- Move to "origin" or top
        if not Search (Afpx.Line_List, Origin,
                       From => Afpx.Line_List_Mng.Absolute) then
          Afpx.Line_List.Rewind;
        end if;
      end if;

    end Init;

  begin
    -- Reset Afpx list
    Afpx.Line_List.Delete_List (False);
    Afpx.Encode_Line (Origin, "origin");

    -- Init Afpx
    Init;

    -- Main loop
    loop
      Afpx.Put_Then_Get (Cursor_Field, Cursor_Col, Insert, Ptg_Result, True);

      case Ptg_Result.Event is
        when Afpx.Keyboard =>
          case Ptg_Result.Keyboard_Key is
            when Afpx.Return_Key =>
              null;
            when Afpx.Escape_Key =>
              -- Back
              return;
            when Afpx.Break_Key =>
              raise Utils.Exit_Requested;
          end case;

        when Afpx.Mouse_Button =>
          case Ptg_Result.Field_No is
            when Utils.X.List_Scroll_Fld_Range'First ..
                 Utils.X.List_Scroll_Fld_Range'Last =>
              -- Scroll list
              Afpx.List_Manager.Scroll(
                  Ptg_Result.Field_No
                - Utils.X.List_Scroll_Fld_Range'First
                + 1);
            when Afpx_Xref.Push.Push =>
              if Do_Push then
                -- Push OK
                return;
              else
                -- Push KO
                Init;
              end if;
            when Afpx_Xref.Push.Back =>
              -- Back button
              return;
            when others =>
              null;
          end case;

       when Afpx.Fd_Event | Afpx.Timer_Event | Afpx.Signal_Event
          | Afpx.Refresh =>
         null;
      end case;
    end loop;

  end Handle;

end Push;

