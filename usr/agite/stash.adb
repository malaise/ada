with Ada.Exceptions;
with As.U, Directory, Afpx.Utils, Str_Util, Basic_Proc,
     Language, Images;
with Git_If, Utils.X, Afpx_Xref, Confirm, Error;
package body Stash is

  -- List width
  List_Width : Afpx.Width_Range;

  procedure Set (Line : in out Afpx.Line_Rec;
                 From : in  Git_If.Stash_Entry_Rec) is
  begin
    Afpx.Utils.Encode_Line ("",
        Images.Integer_Image (From.Num)
      & " " & From.Branch.Image & " " & From.Name.Image, "", List_Width, Line,
        False);
  exception
    when Error:others =>
      Basic_Proc.Put_Line_Error ("Exception "
          & Ada.Exceptions.Exception_Name (Error)
          & " raised in stash on " & From.Name.Image);
  end Set;

  procedure Init_List is new Afpx.Utils.Init_List (
    Git_If.Stash_Entry_Rec, Git_If.Stash_Mng, Set, False);

  -- Root path
  Root : As.U.Asu_Us;

  -- Afpx Ptg stuff
  Get_Handle : Afpx.Get_Handle_Rec;

  -- Init screen
  procedure Init is
  begin
    Afpx.Use_Descriptor (Afpx_Xref.Stash.Dscr_Num);
    -- Encode Root
    Utils.X.Encode_Field (Root.Image, Afpx_Xref.Stash.Root);
    -- Reset Ptg stuff
    Get_Handle := (others => <>);
  end Init;

  -- The stashes
  Stashes : Git_If.Stash_List;

  -- Re assess the list of stashes
  procedure Reread (Keep : in Boolean) is
    Pos : Natural;
  begin
    -- Save current selection
    if Keep and then not Afpx.Line_List.Is_Empty then
      Pos := Afpx.Line_List.Get_Position;
    else
      Pos := 0;
    end if;

    -- Get list of changes
    Git_If.List_Stashes (Stashes);

    -- Encode the list
    Init_List (Stashes);

    -- Move back to the same entry as before (if possible)
    if not  Afpx.Line_List.Is_Empty then
      if Pos /= 0 then
        if Pos <= Afpx.Line_List.List_Length then
          Afpx.Line_List.Move_At (Pos);
        else
          Afpx.Line_List.Rewind (Where => Afpx.Line_List_Mng.Prev);
        end if;
      else
        Afpx.Line_List.Rewind;
      end if;
    end if;
    -- Center
    Afpx.Update_List (Afpx.Center_Selected);

    -- Encode current branch
    Utils.X.Encode_Branch (Afpx_Xref.Commit.Branch);
    -- Set field activity
    Afpx.Utils.Protect_Field (Afpx_Xref.Stash.Del, Afpx.Line_List.Is_Empty);
    Afpx.Utils.Protect_Field (Afpx_Xref.Stash.Apply, Afpx.Line_List.Is_Empty);
    Afpx.Utils.Protect_Field (Afpx_Xref.Stash.Pop, Afpx.Line_List.Is_Empty);
    Afpx.Utils.Protect_Field (Afpx_Xref.Stash.Rename, Afpx.Line_List.Is_Empty);
  end Reread;

  -- Stash operations
  type Stash_Oper_List is (Stash_Addapl, Stash_Addrst, Stash_Apl,
                           Stash_Pop, Stash_Del, Stash_Rename);
  function Do_Stash (Oper : in Stash_Oper_List) return Boolean is
    Name, Str : As.U.Asu_Us;
    Num : Git_If.Stash_Number;
    Loc_Oper : Stash_Oper_List := Oper;
    Index : Positive;
    Message : As.U.Asu_Us;
    Result : Boolean;
  begin
    -- Recover argument
    if Oper = Stash_Addapl or else Oper = Stash_Addrst
    or else Oper = Stash_Rename then
       -- Recover name from Get field
       Afpx.Decode_Field (Afpx_Xref.Stash.Name, 0, Name);
       Name := As.U.Tus (Str_Util.Strip (Name.Image));
       Afpx.Clear_Field (Afpx_Xref.Stash.Name);
       Afpx.Reset (Get_Handle);
    end if;

    if Oper = Stash_Addapl or else Oper = Stash_Addrst then
      Str := Name;
    else
      -- Recover num
      declare
        Line : constant Afpx.Line_Rec := Afpx.Line_List.Access_Current.all;
      begin
        Str := As.U.Tus (Str_Util.Strip (Language.Unicode_To_String (
            Line.Str(1 .. Line.Len))));
        Index := Str_Util.Locate (Str.Image, " ");
        Num := Git_If.Stash_Number'Value (Str.Slice (1, Index));
        Str := As.U.Tus (Images.Integer_Image (Num));
      end;
    end if;

    -- Confirm except for addapl
    if Oper /= Stash_Addapl then
      Result := Confirm (
          "Stash",
          "Ready to "
        & (case Oper is
            when Stash_Addapl => "",
            when Stash_Addrst => "add",
            when Stash_Apl    => "apply",
            when Stash_Pop    => "apply and delete",
            when Stash_Del    => "del",
            when Stash_Rename => "rename")
        & " stash: " & Str.Image
        & (if Oper = Stash_Addrst then " and reset"
           elsif Oper = Stash_Rename then " as " & Name.Image
           else ""));
      Init;
      Reread (True);
      if not Result then
        -- Cancel
        return False;
      end if;
    end if;

    -- Do stash operation
    case Oper is
      when Stash_Addapl =>
        Loc_Oper := Stash_Addrst;
        Message := As.U.Tus (Git_If.Add_Stash (Name.Image));
        if Message.Is_Null then
          Loc_Oper := Stash_Apl;
          Message := As.U.Tus (Git_If.Apply_Stash (0));
        end if;
      when Stash_Addrst => Message := As.U.Tus (Git_If.Add_Stash (Name.Image));
      when Stash_Apl    => Message := As.U.Tus (Git_If.Apply_Stash (Num));
      when Stash_Pop    => Message := As.U.Tus (Git_If.Pop_Stash (Num));
      when Stash_Del    => Message := As.U.Tus (Git_If.Drop_Stash (Num));
      when Stash_Rename => Message := As.U.Tus (Git_If.Rename_Stash (
                                                  Num, Name.Image));
    end case;

    -- Handle error
    if Message.Is_Null then
      -- OK
      Reread (False);
      return True;
    else
      Error ("Stash "
        & (case Loc_Oper is
              when Stash_Addrst => "adding",
              when Stash_Addapl => "",
              when Stash_Apl    => "applying",
              when Stash_Pop    => "popping",
              when Stash_Del    => "deleting",
              when Stash_Rename => "renaming"),
        Name.Image, Message.Image);
      Init;
      Reread (True);
      return False;
    end if;
  end Do_Stash;

  procedure Do_Stash (Oper : in Stash_Oper_List) is
    Dummy : Boolean;
  begin
    Dummy := Do_Stash (Oper);
  end Do_Stash;

  -- Handle the commits
  procedure Handle (Root : in String) is
    Ptg_Result   : Afpx.Result_Rec;
    use type Afpx.Field_Range;
  begin

    -- Move to root
    Stash.Root := As.U.Tus (Root);
    Directory.Change_Current (Root);

    -- Init Afpx
    Init;

    -- Reset Afpx list
    Afpx.Line_List.Delete_List (False);

    -- List width
    List_Width := Afpx.Get_Field_Width (Afpx.List_Field_No);

    -- Encode Changes
    Reread (False);

    -- Main loop
    loop
      Afpx.Put_Then_Get (Get_Handle, Ptg_Result);

      case Ptg_Result.Event is
        when Afpx.Keyboard =>
          case Ptg_Result.Keyboard_Key is
            when Afpx.Return_Key =>
              -- Add stash and apply
              Do_Stash (Stash_Addapl);
              return;
            when Afpx.Escape_Key =>
              -- Back
              return;
            when Afpx.Break_Key =>
              raise Utils.Exit_Requested;
          end case;

        when Afpx.Mouse_Button =>
          case Ptg_Result.Field_No is
            when Utils.X.List_Scroll_Fld_Range =>
              -- Scroll list
              Afpx.Utils.Scroll(
                  Ptg_Result.Field_No
                - Utils.X.List_Scroll_Fld_Range'First
                + 1);

            -- Stash operations
            when Afpx_Xref.Stash.Add_Apply =>
              -- Add stash and apply
              if Do_Stash (Stash_Addapl) then
                return;
              end if;
            when Afpx_Xref.Stash.Add_Reset =>
              -- Add stash (and reset)
              if Do_Stash (Stash_Addrst) then
                return;
              end if;
            when Afpx_Xref.Stash.Apply
               | Afpx.List_Field_No =>
              -- Apply stash
              if Do_Stash (Stash_Apl) then
                return;
              end if;
            when Afpx_Xref.Stash.Pop =>
              -- Pop stash
              if Do_Stash (Stash_Pop) then
                return;
              end if;
            when Afpx_Xref.Stash.Del =>
              -- Delete stash
              Do_Stash (Stash_Del);
              Reread (False);
            when Afpx_Xref.Stash.Rename =>
              -- Rename stash
              Do_Stash (Stash_Rename);
              Reread (False);

            when Afpx_Xref.Stash.Back =>
              -- Back button
              return;
            when others =>
              null;
          end case;

        when Afpx.Fd_Event | Afpx.Timer_Event | Afpx.Signal_Event =>
          null;
        when Afpx.Refresh =>
         -- Encode current branch
         Utils.X.Encode_Branch (Afpx_Xref.Commit.Branch);
      end case;
    end loop;

  end Handle;

end Stash;

