with Ada.Calendar;
with As.U, As.B, Perpet, Normal, Dir_Mng;
with Pers_Def;
package body Mesu_Nam is

  -- The result. File name (or file template)
  -- subtype File_Name_Str is String (1 .. 14);


  -- "YYyyMmDd" or "        "
  -- subtype File_Date_Str is String (1 .. 8);

  -- from "00" to "99" or "  "
  -- subtype File_No_Str is String (1 .. 2);

  -- from "000" to "999" or "   "
  -- subtype File_Pid_Str is String (1 .. 3);

  function Valid_Date (Date : File_Date_Str) return Boolean is
    Year  : Ada.Calendar.Year_Number;
    Month : Ada.Calendar.Month_Number;
    Day  : Ada.Calendar.Day_Number;
  begin
    Year  := Ada.Calendar.Year_Number'Value(Date(1..4));
    Month := Ada.Calendar.Month_Number'Value(Date(5..6));
    Day   := Ada.Calendar.Day_Number'Value(Date(7..8));
    return Day <= Perpet.Nb_Days_Month (Year, Month);
  exception
    when others =>
      return False;
  end Valid_Date;

  function Valid_No (No : String) return Boolean is
  begin
    return No'Length = 2 and then No >= "00" and then No <= "99";
  exception
    when others =>
      return False;
  end Valid_No;

  function Valid_Pid (Pid : in String) return Boolean is
    Dummy_Pid_Val : Pers_Def.Pid_Range;
  begin
    if Pid'Length /= 3 then
      return False;
    end if;
    Dummy_Pid_Val := Pers_Def.Pid_Range'Value(Pid);
    return True;
  exception
    when others =>
      return False;
  end Valid_Pid;

  -- Check wether fields are valid
  function Valid_File_Def (Date : File_Date_Str := Wild_Date_Str;
                            No   : File_No_Str   := Wild_No_Str;
                            Pid  : File_Pid_Str  := Wild_Pid_Str)
   return Boolean is
  begin
    if Date /= Wild_Date_Str and then not Valid_Date (Date) then
      return False;
    elsif No /= Wild_No_Str and then not Valid_No (No) then
      return False;
    elsif Pid /= Wild_Pid_Str and then not Valid_Pid (Pid) then
      return False;
    else
      return True;
    end if;
  exception
    when others =>
      return False;
   end Valid_File_Def;

  -- Build a file name (or a template if some " ")
  -- May raise File_Name_Error if some fields have wrong format
  --  or date is not valid
  function Build_File_Name (Date : File_Date_Str := Wild_Date_Str;
                            No   : File_No_Str   := Wild_No_Str;
                            Pid  : File_Pid_Str  := Wild_Pid_Str)
   return File_Name_Str is
  begin
    if not Valid_File_Def (Date, No, Pid) then
      raise File_Name_Error;
    end if;
    return Date & No & "." & Pid;
  end Build_File_Name;

  -- Check wether fields are valid
  function Valid_File_Name (File_Name : File_Name_Str) return Boolean is
    Date : File_Date_Str;
  begin
    if File_Name(11) /= '.' then
      return False;
    end if;

    Date := File_Name (1 .. 8);
    if Date /= "????????" then
      if not Valid_Date (Date) then
        return False;
      end if;
    end if;
    if File_Name(9 .. 10) /= Wild_No_Str
    and then not Valid_No (File_Name(9 .. 10)) then
       return False;
    end if;
    if File_Name(12 .. 14) /= Wild_Pid_Str
    and then not Valid_Pid (File_Name(12 .. 14)) then
      return False;
    end if;
    return True;
  exception
    when others =>
      return False;
  end Valid_File_Name;


  -- Split a file name (or a template)
  -- May raise File_Name_Error if some fields have wrong format
  --  or date is not valid
  procedure Split_File_Name (File_Name : in File_Name_Str;
                             Date      : out File_Date_Str;
                             No        : out File_No_Str;
                             Pid       : out File_Pid_Str)  is
  begin
    if not Valid_File_Name (File_Name => File_Name) then
      raise File_Name_Error;
    end if;
    Date := File_Name(1 .. 8);
    No := File_Name(9 .. 10);
    Pid := File_Name(12 .. 14);
  end Split_File_Name;

  -- Find first file_no_str available for given date and pid
  -- May return Wild_No_Str if no more_slot available
  -- May raise File_Name_Error if date or pid has wild
  function Find_Slot (Date : File_Date_Str;
                      Pid  : File_Pid_Str) return File_No_Str is
    File_Template : File_Name_Str;
    List : Dir_Mng.File_List_Mng.List_Type;
    File : Dir_Mng.File_Entry_Rec;
    L_Date : File_Date_Str;
    L_No   : File_No_Str;
    L_Pid  : File_Pid_Str;
    Ret_No : File_No_Str;
    File_Name : File_Name_Str;

    function Less_Than (L, R : Dir_Mng.File_Entry_Rec) return Boolean is
      use type As.U.Asu_Us;
    begin
      return L.Name < R.Name;
    end Less_Than;

    procedure My_Sort is new Dir_Mng.File_List_Mng.Sort(Less_Than);

  begin
    -- Check no wild_char
    if      As.B.Locate (As.B.Tbs (Date), Wild_Char & "" ) /= 0
    or else As.B.Locate (As.B.Tbs (Pid),  Wild_Char & "" ) /= 0 then
      raise File_Name_Error;
    end if;
    -- build date??.pid
    File_Template := Build_File_Name (Date => Date, Pid => Pid);
    -- Search list of matching files
    Dir_Mng.List_Dir (List, "", File_Template);

    -- Sort (by no because same date and pid)
    My_Sort (List);
    -- Look for lowest
    Ret_No := "00";
    if List.Is_Empty then
      return Ret_No;
    end if;
    List.Rewind;
    -- loop in list
    loop
      -- Get file name
      List.Read (File, Dir_Mng.File_List_Mng.Current);
      File_Name := File.Name.Image;
      Split_File_Name(File_Name, L_Date, L_No, L_Pid);

      if L_No /= Ret_No then
        -- Ret_no is an empty slot
        exit;
      elsif Ret_No = "99" then
        -- No free slot
        Ret_No := Wild_No_Str;
        exit;
      else
        -- Next slot
        Ret_No := Normal(Integer'Value(Ret_No) + 1, 2, Gap => '0');
        if List.Check_Move then
          -- Go to next entry
          List.Move_To;
        else
          -- End of list
          exit;
        end if;
      end if;

    end loop;

    -- Done. Garbage collect!
    List.Delete_List;
    return Ret_No;
  end Find_Slot;

end Mesu_Nam;

