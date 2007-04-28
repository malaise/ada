with Ada.Text_Io, Ada.Calendar;
with Text_Handler, Dynamic_List, Directory, Afpx, Select_File, Normal,
     Environ, Sys_Calls, Date_Image, Language;
with File_Mng, Oper_Dyn_List_Mng, Screen, Unit_Format;

-- Manage the whole acount status
package body Mng is

  -- Sorted operations
  package Oper_List_Mng renames Oper_Dyn_List_Mng.Dyn_List;
  Oper_List : Oper_List_Mng.List_Type;
  procedure Sort is new Oper_List_Mng.Sort (Oper_Def.Before);

  -- Sort by abs(amount)
  function Smaller (Oper_1, Oper_2 : Oper_Def.Oper_Rec) return Boolean is
    use type Oper_Def.Amount_Range;
  begin
    return abs(Oper_1.Amount) < abs(Oper_2.Amount);
  end Smaller;
  procedure Sort_Amounts is new Oper_List_Mng.Sort (Smaller);

  -- Name and status of current account
  Account_Name : Text_Handler.Text(Directory.Max_Dir_Name_Len);
  Account_Saved : Boolean := True;

  -- Are we working with sublist or all selection
  In_Sublist : Boolean := False;

  -- The amount in first record of file (entered)
  Root_Amount : Oper_Def.Amount_Range;

  -- The amounts computed
  Amounts : Screen.Amounts_Array;

  -- Callback for selection when Loading/saving file
  Loading : Boolean;
  procedure Init_Select_File is
  begin
    Afpx.Clear_Field(1);
    if Loading then
      Afpx.Encode_Field(1, (0, 0), "Loading an account");
    else
      Afpx.Encode_Field(1, (0, 0), "Saving an account");
    end if;
  end Init_Select_File;
  function Account_Select_File is new Select_File(Init_Select_File);

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
      Sel_List_Mng.Move_To(Sel_List, Sel_List_Mng.Next, No - 1, False);
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
    Line.Str := (others => ' ');
    Line.Str(1 .. 34) := Language.String_To_Wide (
                Normal(No, 4) & Sep                                     -- 5
              & Unit_Format.Short_Date_Image(Oper.Date) & Sep           -- 9
              & Unit_Format.Short_Image(Oper.Amount) & Sep              -- 10
              & ' ' & Unit_Format.Short_Status_Image(Oper.Status) & Sep -- 5
              & Unit_Format.Short_Kind_Image(Oper.Kind) & Sep);         -- 5
    Line.Str(35 .. 71) :=
           Oper.Destination(1 .. 10) & Wsep
         & Oper.Comment(1 .. 15) & Wsep
         & Oper.Reference;
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
      Sel_List_Mng.Delete_List(Sel_List, Deallocate => False);
      for I in 1 .. Oper_List_Mng.List_Length(Oper_List) loop
        Sel_List_Mng.Insert(Sel_List, (No => I, Deleted => False));
      end loop;
      In_Sublist := False;
      Screen.Sublist(In_Sublist);
    end Reset_Selection;


    procedure Move_To_Current is
      Sel : Sel_Rec;
    begin
      Sel_List_Mng.Read(Sel_List, Sel, Sel_List_Mng.Current);
      Oper_List_Mng.Move_To(Oper_List, Oper_List_Mng.Next, Sel.No - 1, False);
    end Move_To_Current;


    Loc_Pos : Oper_Range;

    procedure Save_Pos (Move_To_First : in Boolean := True) is
    begin
      Loc_Pos := Sel_List_Mng.Get_Position(Sel_List);
      if Move_To_First then
        Sel_List_Mng.Rewind(Sel_List);
      end if;
    end Save_Pos;

    procedure Restore_Pos is
    begin
      Sel_List_Mng.Move_To(Sel_List, Sel_List_Mng.Next, Loc_Pos-1, False);
    end Restore_Pos;


    procedure Insert_Amount (Amount : in Oper_Def.Amount_Range) is
      Oper : Oper_Def.Oper_Rec;
    begin
      Oper.Amount := Amount;
      if not Oper_List_Mng.Is_Empty(Oper_List) then
        Oper_List_Mng.Rewind(Oper_List);
      end if;
      Oper_List_Mng.Insert(Oper_List, Oper, Oper_List_Mng.Prev);
    end Insert_Amount;

    function Get_Amount return Oper_Def.Amount_Range is
      Oper : Oper_Def.Oper_Rec;
    begin
      Oper_List_Mng.Rewind(Oper_List);
      Oper_List_Mng.Get(Oper_List, Oper, Oper_List_Mng.Next);
      return Oper.Amount;
    end Get_Amount;

  end List_Util;


  -- Reset the Afpx list from the sel list
  procedure Reset_List is
    Oper : Oper_Def.Oper_Rec;
  begin
    Afpx.Line_List_Mng.Delete_List(Afpx.Line_List);
    if Sel_List_Mng.Is_Empty(Sel_List) then
      return;
    end if;

    -- Save pos and move to beginning of selection
    List_Util.Save_Pos;
    loop
      List_Util.Move_To_Current;
      Oper_List_Mng.Read(Oper_List, Oper, Oper_List_Mng.Current);
      Afpx.Line_List_Mng.Insert(Afpx.Line_List,
                  Oper_To_Line(Oper_List_Mng.Get_Position(Oper_List), Oper));
      exit when not Sel_List_Mng.Check_Move(Sel_List);
      Sel_List_Mng.Move_To(Sel_List);
    end loop;
    List_Util.Restore_Pos;
    Afpx.Line_List_Mng.Move_To(Afpx.Line_List,
                     Afpx.Line_List_Mng.Next,
                     Sel_List_Mng.Get_Position(Sel_List) - 1, False);
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
        when Constraint_Error | Numeric_Error =>
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

    if Oper_List_Mng.Is_Empty(Oper_List) then
      return;
    end if;

    -- All operations
    Oper_List_Mng.Rewind(Oper_List);
    loop
      Oper_List_Mng.Read(Oper_List, Oper, Oper_List_Mng.Current);
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
      exit when not Oper_List_Mng.Check_Move(Oper_List);
      Oper_List_Mng.Move_To(Oper_List);
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
    Screen.Encode_File_Name(Text_Handler.Value(Account_Name));
    Screen.Encode_Nb_Oper(Oper_List_Mng.List_Length(Oper_List),
                          Sel_List_Mng.List_Length(Sel_List));
    Screen.Encode_Saved(Account_Saved);
    Reset_List;
    if List_Update = Bottom then
      Afpx.Update_List(Afpx.Bottom);
    elsif List_Update = Center then
      Afpx.Update_List(Afpx.Center);
    end if;
    Encode_Amounts;
    Screen.Update_To_Unit;
    Screen.Allow_Edit(not Sel_List_Mng.Is_Empty(Sel_List));
  end Refresh_Screen;

  -- Load from file
  procedure Load (File_Name : in String) is
    Loaded_Name : Text_Handler.Text(Account_Name.Max_Len);
    Oper : Oper_Def.Oper_Rec;
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
        Text_Handler.Set(Loaded_Name, File_Name);
      exception
        when Constraint_Error =>
          Screen.Ack_Error(Screen.File_Name_Too_Long);
          Refresh_Screen(Unchanged);
          return;
      end;
    else
      -- Let user select file
      Loading := True;
      Text_Handler.Set(Loaded_Name, Account_Select_File(2, "", True));
      Screen.Reset;
      Refresh_Screen(Bottom);
    end if;

    if not Text_Handler.Empty(Loaded_Name) then
      -- Load
      begin
        File_Mng.Load(Text_Handler.Value(Loaded_Name), Oper_List, Can_Write);
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
      Text_Handler.Set (Account_Name, Loaded_Name);
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
    Tmp_Name : Text_Handler.Text(Directory.Max_Dir_Name_Len);
  begin
    if Mode = Rescue then
      List_Util.Insert_Amount(Root_Amount);
      File_Mng.Save("Tmp", Oper_List);
      Root_Amount := List_Util.Get_Amount;
      return;
    end if;

    -- Confirm file overwritting
    --  or select file
    Loading := False;
    if Text_Handler.Empty(Account_Name)
    or else not Screen.Confirm_Action(Screen.Overwrite_File) then
      -- User discards overwritting
      if Mode = Cancel then
        return;
      end if;
      Text_Handler.Set(Tmp_Name, Account_Select_File(2, "", False));
      Screen.Reset;
      Refresh_Screen(Center);
      if Text_Handler.Empty(Tmp_Name) then
        -- User discards selecting new file name
        return;
      end if;
      Text_Handler.Set(Account_Name, Tmp_Name);
      Screen.Encode_File_Name(Text_Handler.Value(Account_Name));
    end if;
    -- Insert root amount
    List_Util.Insert_Amount(Root_Amount);
    -- Save
    begin
      File_Mng.Save(Text_Handler.Value(Account_Name), Oper_List);
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
    Oper : Oper_Def.Oper_Rec;
  begin
    if not Account_Saved
    and then not Screen.Confirm_Action(Screen.Overwrite_Account) then
      return;
    end if;
    -- Set data
    Text_Handler.Empty(Account_Name);
    Sel_List_Mng.Delete_List(Sel_List, Deallocate => False);
    Oper_List_Mng.Delete_List(Oper_List);
    Root_Amount := 0.0;
    Account_Saved := True;
    Compute_Amounts;
    -- Set screen
    In_Sublist := False;
    Screen.Sublist(In_Sublist);
    Refresh_Screen(Bottom);
  end Clear;

  -- Print account
  procedure Print is
    use Ada.Text_Io;
    Pfn : constant String := "Account.lpt";
    Pf : File_Type;
    Oper : Oper_Def.Oper_Rec;
    Sep : constant Character := '|';
    Index : Oper_Range;
    Line : Positive;
    Lines_Per_Page : Positive;
    Page_Title : constant String
    --     --1234 123456789  123456789012 1234 1234 12345678901234567890 12345678901234567890 1234567890
       := "    No|   Date   |   Amount   |Stat|Kind|Destination         |Comment             |Reference";
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
      Create(Pf, Out_File, Pfn);
    exception
      when others =>
        Screen.Ack_Error(Screen.File_Access);
        Refresh_Screen(Center);
        return;
    end;
    Put_Line(Pf, "Account: " & Text_Handler.Value(Account_Name)
               & "     at: " & Date_Image(Ada.Calendar.Clock) (1 .. 16));
    Put_Line(Pf, Page_Title);
    Line := 3;

    if not Oper_List_Mng.Is_Empty(Oper_List) then
      Oper_List_Mng.Rewind(Oper_List);
      Index := 1;
      loop
        Oper_List_Mng.Read(Oper_List, Oper, Oper_List_Mng.Current);
        Put_Line(Pf, "  " & Normal(Index, 4) & Sep
                   & Unit_Format.Date_Image(Oper.Date) & Sep
                   & Unit_Format.Image(Oper.Amount, False) & Sep
                   & ' ' & Unit_Format.Short_Status_Image(Oper.Status) & Sep
                   & Unit_Format.Short_Kind_Image(Oper.Kind) & Sep
                   & Language.Wide_To_String (Oper.Destination) & Sep
                   & Language.Wide_To_String (Oper.Comment) & Sep
                   & Language.Wide_To_String (Oper.Reference)) ;
        exit when not Oper_List_Mng.Check_Move(Oper_List);
        Oper_List_Mng.Move_To(Oper_List);
        Index := Index + 1;
        if Line = Lines_Per_Page then
          New_Page(Pf);
          Put_Line(Pf, Page_Title);
          Line := 2;
        else
          Line := Line + 1;
        end if;
      end loop;
    end if;
    -- Print summary
    if Line = Lines_Per_Page then
      New_Page(Pf);
      Line := 2;
    end if;

    Put(Pf, "Real: ");
    if not Amounts(Screen.Real).Overflow then
      Put (Pf, Unit_Format.Image(Amounts(Screen.Real).Amount, False));
    else
      Put (Pf, Overflow);
    end if;
    Put (Pf, " Account: ");
    if not Amounts(Screen.Account).Overflow then
      Put (Pf, Unit_Format.Image(Amounts(Screen.Account).Amount, False));
    else
      Put (Pf, Overflow);
    end if;
    Put (Pf, " Defered: ");
    if not Amounts(Screen.Defered).Overflow then
      Put (Pf, Unit_Format.Image(Amounts(Screen.Defered).Amount, False));
    else
      Put (Pf, Overflow);
    end if;
    Put (Pf, " Saved: ");
    if not Amounts(Screen.Saved).Overflow then
      Put (Pf, Unit_Format.Image(Amounts(Screen.Saved).Amount, False));
    else
      Put (Pf, Overflow);
    end if;
    New_Line (Pf);

    Line := Line + 1;
    New_Page(Pf);
    Flush(Pf);

    -- Print
    declare
      Val : String(1 .. 256);
      Len : Natural;
      Dummy : Integer;
    begin
      Len := 3;
      Val (1 .. Len) := "lpr";

      Environ.Get_Str("ACCOUNT_LPR_COMMAND", Val, Len);
      Dummy := Sys_Calls.Call_System (Val(1..Len) & " " & Pfn);
    end;

    -- Delete & close
    Delete(Pf);

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
    Oper_List_Mng.Read(Oper_List, Oper, Oper_List_Mng.Current);
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
      Oper_List_Mng.Modify(Oper_List, Oper, Oper_List_Mng.Current);
      Account_Saved := False;
      Compute_Amounts;
      Refresh_Screen(Unchanged);
    end if;
  end Update_State;

  -- Create a new operation
  procedure Add_Oper is
  begin
    Edition.Edit(Edition.Create);
    Screen.Reset;
    Compute_Amounts;
    Refresh_Screen(Bottom);
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
    Edition.Edit(Edition.Copy);
    Screen.Reset;
    Compute_Amounts;
    Refresh_Screen(Bottom);
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
    if Oper_List_Mng.Is_Empty(Oper_List) then
      return;
    end if;
    -- Get number of oper to check and start from the beginning
    Pos := Sel_List_Mng.Get_Position(Sel_List);
    Sel_List_Mng.Rewind(Sel_List);
    -- Check up to pos included
    Tmp_Amount := Root_Amount;
    for I in 1 .. Pos loop
      List_Util.Move_To_Current;
      Oper_List_Mng.Read(Oper_List, Oper, Oper_List_Mng.Current);
      -- Remove if entered
      if Oper.Status = Oper_Def.Entered then
        begin
          Tmp_Amount := Tmp_Amount + Oper.Amount;
        exception
          when Constraint_Error | Numeric_Error =>
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
      Sel_List_Mng.Move_To(Sel_List);
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
    In_Sublist := False;
    Screen.Sublist(In_Sublist);
    List_Util.Reset_Selection;
    Refresh_Screen(Bottom);
  end Show_All;

  -- Get data
  function Is_Saved return Boolean is
  begin
    return Account_Saved;
  end Is_Saved;

end Mng;

