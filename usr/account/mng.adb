with Ada.Calendar, Ada.Characters.Latin_1;
with As.B, Dynamic_List, Directory, Afpx, Select_File, Normal, Text_Line,
     Environ, Sys_Calls, Images, Language, Perpet, Con_Io;
with Oper_Def, File_Mng, Oper_Dyn_List_Mng, Screen, Unit_Format, Afpx_Xref;

-- Manage the whole acount status
package body Mng is

  -- Sorted operations
  package Oper_List_Mng renames Oper_Dyn_List_Mng.Dyn_List;
  Oper_List : Oper_List_Mng.List_Type;

  -- Sort by date
  procedure Sort is new Oper_List_Mng.Sort (Oper_Def.Before);

  -- Sort by abs(amount)
  procedure Sort_Amounts is new Oper_List_Mng.Sort (Oper_Def.Smaller);

  -- Name and status of current account
  Account_Name : As.B.Asb_Bs(Directory.Max_Dir_Name_Len);
  Account_Saved : Boolean := True;

  -- The amount in first record of file (entered)
  Root_Amount : Oper_Def.Amount_Range;

  -- The amounts computed
  Amounts : Screen.Amounts_Array;

  -- Callback for selection when Loading/saving file
  Loading : Boolean;
  procedure Init_Select_File is
  begin
    Afpx.Clear_Field(Screen.Account_Name_Fld);
    if Loading then
      Afpx.Encode_Field(Afpx_Xref.Selection.Title, (0, 0),
                        "Loading an account");
    else
      Afpx.Encode_Field(Afpx_Xref.Selection.Title, (0, 0),
                        "Saving an account");
    end if;
  end Init_Select_File;
  package Asf is new Select_File(Afpx_Xref.Selection.Dscr_Num,
                                 Init_Select_File);

  -- Selection list
  type Sel_Rec is record
    No : Oper_Range;
    Deleted : Boolean := False;
  end record;
  package Sel_Dyn_List_Mng is new Dynamic_List(Sel_Rec);
  package Sel_List_Mng renames Sel_Dyn_List_Mng.Dyn_List;
  Sel_List : Sel_List_Mng.List_Type;

  -- Set current in sel list from Afpx selected
  procedure Set_Current (No : in Oper_Nb_Range) is
  begin
    if No in Oper_Range then
      Sel_List.Move_At(No);
    end if;
  end Set_Current;


  -- Builds the Afpx line from oper
  function Oper_To_Line (No : Oper_Range;
                         Oper : Oper_Def.Oper_Rec) return Afpx.Line_Rec is
    Line : Afpx.Line_Rec;
    Sep : constant String := "|";
    Wsep : constant Wide_String := Language.String_To_Wide (Sep);
  begin
    Line.Len := Afpx.Get_Field_Width(8);
    Line.Str := (others => Language.Char_To_Unicode (' '));
    Line.Str(1 .. 33) := Language.Copy (
                Normal(No, 4) & Sep                                     -- 5
              & Unit_Format.Short_Date_Image(Oper.Date) & Sep           -- 9
              & Unit_Format.Short_Image(Oper.Amount) & Sep              -- 10
              & Unit_Format.Short_Status_Image(Oper.Status) & Sep       -- 4
              & Unit_Format.Short_Kind_Image(Oper.Kind) & Sep);         -- 5
    Line.Str(34 .. 71) := Language.Copy (
           Oper.Destination(1 .. 10) & Wsep
         & Oper.Comment(1 .. 16) & Wsep
         & Oper.Reference);
    return Line;
  end Oper_To_Line;

  package List_Util is
    -- Build initial selection with all opers
    procedure Reset_Selection;

    -- Move in oper list to currently selected in sel list
    procedure Move_To_Current;

    -- These work only if lists are not modified between calls
    procedure Save_Pos (Move_To_First : in Boolean := True);
    procedure Restore_Pos;

    -- Don't use selection list between insert and get
    procedure Insert_Amount (Amount : in Oper_Def.Amount_Range);
    function Get_Amount return Oper_Def.Amount_Range;
  end List_Util;

  package body List_Util is

    procedure Reset_Selection is
    begin
      Sel_List.Delete_List(Deallocate => False);
      for I in 1 .. Oper_List.List_Length loop
        Sel_List.Insert((No => I, Deleted => False));
      end loop;
      Screen.Set_Sublist(False);
    end Reset_Selection;


    procedure Move_To_Current is
      Sel : Sel_Rec;
    begin
      Sel_List.Read(Sel, Sel_List_Mng.Current);
      Oper_List.Move_At(Sel.No);
    end Move_To_Current;

    Loc_Pos : Oper_Range;

    procedure Save_Pos (Move_To_First : in Boolean := True) is
    begin
      Loc_Pos := Sel_List.Get_Position;
      if Move_To_First then
        Sel_List.Rewind;
      end if;
    end Save_Pos;

    procedure Restore_Pos is
    begin
      Sel_List.Move_At(Loc_Pos);
    end Restore_Pos;


    procedure Insert_Amount (Amount : in Oper_Def.Amount_Range) is
      Oper : Oper_Def.Oper_Rec;
    begin
      Oper.Amount := Amount;
      Oper_List.Rewind (False);
      Oper_List.Insert(Oper, Oper_List_Mng.Prev);
    end Insert_Amount;

    function Get_Amount return Oper_Def.Amount_Range is
      Oper : Oper_Def.Oper_Rec;
    begin
      Oper_List.Rewind;
      Oper_List.Get(Oper, Oper_List_Mng.Next);
      return Oper.Amount;
    end Get_Amount;

  end List_Util;


  -- Reset the Afpx list from the sel list
  procedure Reset_List is
    Oper : Oper_Def.Oper_Rec;
  begin
    Afpx.Line_List.Delete_List;
    if Sel_List.Is_Empty then
      return;
    end if;

    -- Save pos and move to beginning of selection
    List_Util.Save_Pos;
    loop
      List_Util.Move_To_Current;
      Oper_List.Read(Oper, Oper_List_Mng.Current);
      Afpx.Line_List.Insert(Oper_To_Line(Oper_List.Get_Position, Oper));
      exit when not Sel_List.Check_Move;
      Sel_List.Move_To;
    end loop;
    List_Util.Restore_Pos;
    Afpx.Line_List.Move_At(Sel_List.Get_Position);
  end Reset_List;

  -- Compute amounts from all account operations
  procedure Compute_Amounts is
    use type Oper_Def.Amount_Range, Oper_Def.Status_List, Oper_Def.Kind_List,
             Screen.Amount_List;

    -- Add a value to an amount, if not already in overflow.
    procedure Add_Amount (Kind : in Screen.Amount_List;
                          Value : in Oper_Def.Amount_Range) is
    begin
      if not Amounts(Kind).Overflow then
      begin
        Amounts(Kind).Amount := Amounts(Kind).Amount + Value;
      exception
        when Constraint_Error =>
          Amounts(Kind).Overflow := True;
        end;
      end if;
    end Add_Amount;

    Oper : Oper_Def.Oper_Rec;

  begin
    -- Initial values
    Amounts(Screen.Real).Amount    := Root_Amount;
    Amounts(Screen.Account).Amount := Root_Amount;
    Amounts(Screen.Defered).Amount := 0.0;
    Amounts(Screen.Saved).Amount  := 0.0;
    for I in Screen.Amount_List loop
      Amounts(I).Overflow := False;
    end loop;

    if Oper_List.Is_Empty then
      return;
    end if;

    -- All operations
    Oper_List.Rewind;
    loop
      Oper_List.Read(Oper, Oper_List_Mng.Current);
      Add_Amount (Screen.Real, Oper.Amount);
      if Oper.Status = Oper_Def.Entered then
        Add_Amount(Screen.Account, Oper.Amount);
      end if;
      if Oper.Kind = Oper_Def.Savings then
        Add_Amount(Screen.Saved, Oper.Amount);
      end if;
      if Oper.Status = Oper_Def.Defered then
        Add_Amount(Screen.Defered, Oper.Amount);
      end if;
      exit when not Oper_List.Check_Move;
      Oper_List.Move_To;
    end loop;
  end Compute_Amounts;

  -- Ecode amounts values
  procedure Encode_Amounts is
  begin
    Screen.Encode_Summary(Amounts);
  end Encode_Amounts;

  -- Refresh all, to be called each time an oper or the account changes
  type List_Update_List is (Bottom, Center, Unchanged);
  procedure Refresh_Screen (List_Update : in List_Update_List) is
  begin
    Screen.Encode_File_Name(Account_Name.Image);
    Screen.Encode_Nb_Oper(Oper_List.List_Length,
                          Sel_List.List_Length);
    Screen.Encode_Saved(Account_Saved);
    Reset_List;
    if List_Update = Bottom then
      Afpx.Update_List(Afpx.Bottom);
    elsif List_Update = Center then
      Afpx.Update_List(Afpx.Center_Selected);
    end if;
    Encode_Amounts;
    Screen.Update_To_Unit;
    Screen.Allow_Edit(not Sel_List.Is_Empty);
  end Refresh_Screen;

  -- Adjust operation after copy
  procedure Adjust_Copy (Oper : in out Oper_Def.Oper_Rec) is
    Oper_Time : Ada.Calendar.Time;
    Dummy_Seconds : Ada.Calendar.Day_Duration;
    Max_Days : Ada.Calendar.Day_Number;
    use type Oper_Def.Kind_List, Perpet.Duration_Rec;
  begin
    -- Adjust Status: Credit is defered, others are not entered
    if Oper.Kind = Oper_Def.Credit then
      Oper.Status := Oper_Def.Defered;
    else
      Oper.Status := Oper_Def.Not_Entered;
    end if;

    -- Increment month of Transfers and Savings
    if Oper.Kind = Oper_Def.Transfer
    or else Oper.Kind = Oper_Def.Savings then
      Oper_Time := Ada.Calendar.Time_Of (Oper.Date.Year,
                                         Oper.Date.Month,
                                         Oper.Date.Day, 0.0);
      Oper_Time := Oper_Time + (Years => 0, Months => 1);
      Ada.Calendar.Split (Oper_Time, Oper.Date.Year,
                                     Oper.Date.Month,
                                     Oper.Date.Day, Dummy_Seconds);
      -- Adjust days if new month does not have enough days
      Max_Days := Perpet.Nb_Days_Month(Oper.Date.Year, Oper.Date.Month);
      if Oper.Date.Day > Max_Days then
        Oper.Date.Day := Max_Days;
      end if;
    end if;

  end Adjust_Copy;

  -- Copy selected operations
  procedure Copy_Selection is
    Oper : Oper_Def.Oper_Rec;
    Copied_List : Sel_List_Mng.List_Type;
  begin
    -- Sanity
    if Sel_List.Is_Empty then
      return;
    end if;
    if Oper_List.List_Length + Sel_List.List_Length
       > Oper_Def.Oper_Range'Last then
      return;
    end if;
    -- Copy all selected operations
    Sel_List.Rewind;
    loop
      -- Read current operation
      List_Util.Move_To_Current;
      Oper_List.Read (Oper, Oper_List_Mng.Current);
      -- Adjust status and date if needed
      Adjust_Copy (Oper);
      -- Append in list of operations
      Oper_List.Rewind (True, Oper_List_Mng.Prev);
      Oper_List.Insert (Oper);
      -- Save this operation ref
      Copied_List.Insert ( (No => Oper_List.List_Length, Deleted => False) );
      -- Next selected operation if any
      exit when not Sel_List.Check_Move;
      Sel_List.Move_To;
    end loop;
    -- Merge selection lists: new selection is previous + copies
    Sel_List.Insert_Copy (Copied_List);
    -- Recompute
    Account_Saved := False;
    Compute_Amounts;
    Refresh_Screen(Bottom);
  end Copy_Selection;

  -- Load from file
  procedure Load (File_Name : in String) is
    Loaded_Name : As.B.Asb_Bs(Account_Name.Max);
    Can_Write : Boolean;
  begin
    if not Account_Saved
    and then not Screen.Confirm_Action(Screen.Overwrite_Account) then
      -- User discards overwritting current account
      return;
    end if;

    if File_Name /= "" then
      -- Store file name
      begin
        Loaded_Name.Set (File_Name);
      exception
        when Constraint_Error =>
          Screen.Ack_Error(Screen.File_Name_Too_Long);
          Refresh_Screen(Unchanged);
          return;
      end;
    else
      -- Let user select file
      Loading := True;
      Loaded_Name.Set (Asf.Get_File("", True, False));
      Screen.Reset;
      Screen.Set_Sublist(False);
      Refresh_Screen(Bottom);
    end if;

    if not Loaded_Name.Is_Null then
      -- Load
      begin
        File_Mng.Load(Loaded_Name.Image, Oper_List, Can_Write);
      exception
        when File_Mng.F_Access_Error =>
          Screen.Ack_Error(Screen.File_Access);
          return;
        when File_Mng.F_Io_Error =>
          Screen.Ack_Error(Screen.File_Io);
          return;
      end;
      -- Get root amount
      Root_Amount := List_Util.Get_Amount;
      -- Build initial selection with all
      Sort(Oper_List);
      List_Util.Reset_Selection;
      -- Set data
      Account_Name.Set (Loaded_Name);
      Account_Saved := True;
      Compute_Amounts;
      -- Set screen
      Refresh_Screen(Bottom);
      if not Can_Write then
        Screen.Ack_Error(Screen.File_Read_Only);
      end if;
    else
      -- User cancelled selection
      null;
    end if;

  end Load;

  procedure Save (Mode : Save_Mode_List) is
    Tmp_Name : As.B.Asb_Bs(Directory.Max_Dir_Name_Len);
  begin
    -- The save button is also used for Copy_All when in Sublist
    if Screen.Is_Sublist then
      Copy_Selection;
      return;
    end if;
    if Mode = Rescue then
      List_Util.Insert_Amount(Root_Amount);
      File_Mng.Save("Tmp", Oper_List);
      Root_Amount := List_Util.Get_Amount;
      return;
    end if;

    -- Confirm file overwritting
    --  or select file
    Loading := False;
    if Account_Name.Is_Null
    or else not Screen.Confirm_Action(Screen.Overwrite_File) then
      -- User discards overwritting
      if Mode = Cancel then
        return;
      end if;
      Tmp_Name.Set (Asf.Get_File("", False, False));
      Screen.Reset;
      Screen.Set_Sublist(False);
      Refresh_Screen(Center);
      if Tmp_Name.Is_Null then
        -- User discards selecting new file name
        return;
      end if;
      Account_Name.Set (Tmp_Name);
      Screen.Encode_File_Name (Account_Name.Image);
    end if;
    -- Insert root amount
    List_Util.Insert_Amount(Root_Amount);
    -- Save
    begin
      File_Mng.Save(Account_Name.Image, Oper_List);
    exception
      when File_Mng.F_Access_Error =>
        Screen.Ack_Error(Screen.File_Access);
        Root_Amount := List_Util.Get_Amount;
        return;
      when File_Mng.F_Io_Error =>
        Screen.Ack_Error(Screen.File_Io);
        Root_Amount := List_Util.Get_Amount;
        return;
    end;
    Root_Amount := List_Util.Get_Amount;
    -- Update data and screen
    Account_Saved := True;
    Refresh_Screen(Center);
  end Save;

  procedure Clear is
  begin
    if not Account_Saved
    and then not Screen.Confirm_Action(Screen.Overwrite_Account) then
      return;
    end if;
    -- Set data
    Account_Name.Set_Null;
    Sel_List.Delete_List(Deallocate => False);
    Oper_List.Delete_List;
    Root_Amount := 0.0;
    Account_Saved := True;
    Compute_Amounts;
    -- Set screen
    Screen.Set_Sublist(False);
    Refresh_Screen(Bottom);
  end Clear;

  -- Print account
  procedure Print is
    Pfn : constant String := "Account.lpt";
    Pf : Text_Line.File_Type;
    Oper : Oper_Def.Oper_Rec;
    Sep : constant Character := '|';
    Form_Feed : constant String := Ada.Characters.Latin_1.Ff & "";
    Index : Oper_Range;
    Line : Positive;
    Lines_Per_Page : Positive;
    Page_Title : constant String
    --     --1234 123456789  123456789012 123 1234 12345678901234567890 12345678901234567890 1234567890
       := "    No|   Date   |   Amount   |Ent|Kind|Destination         |Comment             |Reference";
    Overflow : constant Unit_Format.Amount_Str := "Overflow    ";
  begin
    -- Get lines per page
    declare
      Min_Lines_Per_Page : constant Positive := 10;
      Default_Lines_Per_Page : constant Positive := 60;
    begin
      Lines_Per_Page := Default_Lines_Per_Page;
      Environ.Get_Pos("ACCOUNT_LPR_LINES_PER_PAGE", Lines_Per_Page);
      if Lines_Per_Page < Min_Lines_Per_Page then
        Lines_Per_Page := Min_Lines_Per_Page;
      end if;
    exception
      when others =>
        Lines_Per_Page := Default_Lines_Per_Page;
    end;

    -- Create file
    begin
      Pf.Create_All(Pfn);
    exception
      when others =>
        Screen.Ack_Error(Screen.File_Access);
        Refresh_Screen(Center);
        return;
    end;
    Pf.Put_Line("Account: " & Account_Name.Image
               & "     at: " & Images.Date_Image(Ada.Calendar.Clock) (1 .. 16));
    Pf.Put_Line(Page_Title);
    Line := 3;

    if not Oper_List.Is_Empty then
      Oper_List.Rewind;
      Index := 1;
      loop
        Oper_List.Read(Oper, Oper_List_Mng.Current);
        Pf.Put_Line("  " & Normal(Index, 4) & Sep
                   & Unit_Format.Date_Image(Oper.Date) & Sep
                   & Unit_Format.Image(Oper.Amount, False) & Sep
                   & Unit_Format.Short_Status_Image(Oper.Status) & Sep
                   & Unit_Format.Short_Kind_Image(Oper.Kind) & Sep
                   & Language.Wide_To_String (Oper.Destination) & Sep
                   & Language.Wide_To_String (Oper.Comment) & Sep
                   & Language.Wide_To_String (Oper.Reference)) ;
        exit when not Oper_List.Check_Move;
        Oper_List.Move_To;
        Index := Index + 1;
        if Line = Lines_Per_Page then
          Pf.Put(Form_Feed);
          Pf.Put_Line(Page_Title);
          Line := 2;
        else
          Line := Line + 1;
        end if;
      end loop;
    end if;
    -- Print summary
    if Line = Lines_Per_Page then
      Pf.Put(Form_Feed);
      Line := 2;
    end if;

    Pf.Put("Real: ");
    if not Amounts(Screen.Real).Overflow then
      Pf.Put (Unit_Format.Image(Amounts(Screen.Real).Amount, False));
    else
      Pf.Put (Overflow);
    end if;
    Pf.Put (" Account: ");
    if not Amounts(Screen.Account).Overflow then
      Pf.Put (Unit_Format.Image(Amounts(Screen.Account).Amount, False));
    else
      Pf.Put (Overflow);
    end if;
    Pf.Put (" Defered: ");
    if not Amounts(Screen.Defered).Overflow then
      Pf.Put (Unit_Format.Image(Amounts(Screen.Defered).Amount, False));
    else
      Pf.Put (Overflow);
    end if;
    Pf.Put (" Saved: ");
    if not Amounts(Screen.Saved).Overflow then
      Pf.Put (Unit_Format.Image(Amounts(Screen.Saved).Amount, False));
    else
      Pf.Put (Overflow);
    end if;
    Pf.New_Line;

    Pf.Put(Form_Feed);
    Pf.Flush;

    -- Print
    declare
      Val : String(1 .. 256);
      Len : Natural;
      Dummy : Integer;
      pragma Unreferenced (Dummy);
    begin
      Len := 3;
      Val (1 .. Len) := "lpr";

      Environ.Get_Str("ACCOUNT_LPR_COMMAND", Val, Len);
      Dummy := Sys_Calls.Call_System (Val(1..Len) & " " & Pfn);
    end;

    -- Delete & close
    Pf.Close_All;
    Sys_Calls.Unlink (Pfn);

  exception
    when others =>
      Screen.Ack_Error(Screen.File_Io);
      Refresh_Screen(Center);
      return;
  end Print;

  -- Update the displayed amounts
  procedure Change_Unit is
    use type Unit_Format.Units_List;
  begin
    Unit_Format.Switch_Unit;
    -- Redisplay
    Refresh_Screen(Center);
  end Change_Unit;

  -- Sort
  procedure Sort (By_Date : in Boolean) is
  begin
    if By_Date then
      Sort(Oper_List);
    else
      Sort_Amounts(Oper_List);
    end if;
    List_Util.Reset_Selection;
    Refresh_Screen(Bottom);
  end Sort;

  -- Deletion management
  package Deletion is
    -- Flag currently selected operation as deleted or not
    procedure Flag_Deleted;
    procedure Flag_Undeleted;

    -- Get number of flagged operations
    function Get_Nb_Deleted return Oper_Nb_Range;

    -- Delete all flagged operation
    procedure Commit_Deletions;

    -- Cancel all flagged operation
    procedure Cancel_Deletions;

  end Deletion;
  package body Deletion is separate;

  -- The generic edition of an operation
  package Edition is
    type Edit_List is (Create, Modify, Copy, Delete);
    procedure Edit (Edit_Type : in Edit_List);
  end Edition;
  package body Edition is separate;

  -- Update status of operation
  procedure Update_State is
    Oper : Oper_Def.Oper_Rec;
    Prev_Status : Oper_Def.Status_List;
    use type Oper_Def.Status_List;
  begin
    List_Util.Move_To_Current;
    Oper_List.Read(Oper, Oper_List_Mng.Current);
    Prev_Status := Oper.Status;
    case Oper.Status is
      when Oper_Def.Entered =>
        if Oper_Def.Kind_Can_Be(Oper.Kind, Oper_Def.Defered) then
          Oper.Status := Oper_Def.Defered;
        elsif Oper_Def.Kind_Can_Be(Oper.Kind, Oper_Def.Not_Entered) then
          Oper.Status := Oper_Def.Not_Entered;
        end if;
      when Oper_Def.Not_Entered | Oper_Def.Defered =>
        if Oper_Def.Kind_Can_Be(Oper.Kind, Oper_Def.Entered) then
          Oper.Status := Oper_Def.Entered;
        end if;
    end case;
    if Oper.Status /= Prev_Status then
      Oper_List.Modify(Oper, Oper_List_Mng.Current);
      Account_Saved := False;
      Compute_Amounts;
      Refresh_Screen(Unchanged);
    end if;
  end Update_State;

  -- Create a new operation
  procedure Add_Oper is
  begin
    if Oper_List.List_Length /= Oper_Def.Oper_Range'Last then
      Edition.Edit(Edition.Create);
      Screen.Reset;
      Compute_Amounts;
      Refresh_Screen(Bottom);
    end if;
  end Add_Oper;

  -- Edit an operation
  procedure Edit_Oper is
  begin
    Edition.Edit(Edition.Modify);
    Screen.Reset;
    Compute_Amounts;
    Refresh_Screen(Center);
  end Edit_Oper;

  -- Copy an operation
  procedure Copy_Oper is
  begin
    if Oper_List.List_Length /= Oper_Def.Oper_Range'Last then
      Edition.Edit(Edition.Copy);
      Screen.Reset;
      Compute_Amounts;
      Refresh_Screen(Bottom);
    end if;
  end Copy_Oper;

  -- Delete an operation
  procedure Del_Oper is
  begin
    Edition.Edit(Edition.Delete);
    Screen.Reset;
    Compute_Amounts;
    Refresh_Screen(Center);
  end Del_Oper;

  -- Remove all entered operations up to current
  -- Update root amount
  procedure Garbage_Collect is
    Pos : Positive;
    Oper : Oper_Def.Oper_Rec;
    Tmp_Amount : Oper_Def.Amount_Range;
    use type Oper_Def.Status_List, Oper_Def.Amount_Range;
  begin
    if Oper_List.Is_Empty then
      return;
    end if;
    -- Get number of oper to check and start from the beginning
    Pos := Sel_List.Get_Position;
    Sel_List.Rewind;
    -- Check up to pos included
    Tmp_Amount := Root_Amount;
    for I in 1 .. Pos loop
      List_Util.Move_To_Current;
      Oper_List.Read(Oper, Oper_List_Mng.Current);
      -- Remove if entered
      if Oper.Status = Oper_Def.Entered then
        begin
          Tmp_Amount := Tmp_Amount + Oper.Amount;
        exception
          when Constraint_Error =>
            -- Overflow on root amount. Cancel.
            Deletion.Cancel_Deletions;
            Screen.Ack_Error(Screen.Capacity_Error);
            Refresh_Screen(Center);
            return;
        end;
        Deletion.Flag_Deleted;
        Account_Saved := False;
      end if;
      -- Done when orig pos is processed
      exit when I = Pos;
      -- Move to next
      Sel_List.Move_To;
    end loop;
    Root_Amount := Tmp_Amount;
    Deletion.Commit_Deletions;
    Compute_Amounts;
    Refresh_Screen(Center);
  end Garbage_Collect;

  -- Make a sub selection of operations
  procedure Search is separate;

  -- Reset selection to the full list
  procedure Show_All is
  begin
    List_Util.Reset_Selection;
    Refresh_Screen(Bottom);
  end Show_All;

  -- Get data
  function Is_Saved return Boolean is
  begin
    return Account_Saved;
  end Is_Saved;

end Mng;

