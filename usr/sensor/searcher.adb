with Ada.Calendar, Ada.Exceptions;
with Bloc_Io, Aski, Long_Longs, Trace, Date_Text;
with Debug;
package body Searcher is

  -- The blocs of file
  package Char_Io is new Bloc_Io (Character);
  Bloc_Size : constant := 1024;

  -- Dump list (tail or result)
  Dump_Severity : constant Trace.Severities := 16#20#;
  procedure Dump_List (Title : in String;
                       List : in out As.U.Utils.Asu_Dyn_List_Mng.List_Type) is
  begin
    Debug.Logger.Log (Dump_Severity, "Dump " & Title);
    if List.Is_Empty then
      return;
    end if;
    List.Rewind;
    loop
      Debug.Logger.Log (Dump_Severity,  " >" & List.Access_Current.Image & "<");
      exit when not List.Check_Move;
      List.Move_To;
    end loop;
  end Dump_List;

  -- from a Time_Rec to a Calendar.Time
  function Time_Of (Date : Date_Text.Date_Rec) return Ada.Calendar.Time is
  begin
    return Date_Text.Pack (Date);
  end Time_Of;

  -- Search the Pattern in the Tail last lines of File
  -- Clear and set the list to the matching lines
  procedure Search (File_Name : in String;
                    Tail      : in Filters.Tail_Length;
                    Seconds   : in Filters.Tail_Length;
                    Time_Fmt  : in As.U.Asu_Us;
                    Pattern   : access Reg_Exp.Compiled_Pattern;
                    Matches   : in out As.U.Utils.Asu_Dyn_List_Mng.List_Type) is
    -- Input blocs
    File : Char_Io.File_Type;
    Chars, Prev : Char_Io.Element_Array (1 .. Bloc_Size);
    -- Offset from which to read
    Offset : Char_Io.Positive_Count;
    -- How many blocs
    Size : Char_Io.Count;
    Blocs_Nb : Char_Io.Count;
    Last_Bloc_Len, Len : Char_Io.Count;
    -- How many LFs
    Lfs : Long_Longs.Ll_Natural;
    -- Indexes of Lfs in blocs
    Stop : Char_Io.Positive_Count;
    In_Prev : Boolean;
    -- Tempo buffer
    Buffer : As.U.Asu_Us;
    Moved : Boolean;
    -- Length of time
    Time_Len : Natural;
    -- Time of record
    Time : Date_Text.Date_Rec;
    Current_Time, Line_Time, Ref_Time : Ada.Calendar.Time;
    -- Current Year
    use type Char_Io.Count, Char_Io.Element_Array, Ada.Calendar.Time;
  begin
    -- Init out parameter
    Matches.Delete_List;

    -- Open file, compute nb of blocs and nb of chars in last bloc
    Char_Io.Open  (File, Char_Io.In_File, File_Name);
    Size := Char_Io.Size (File);
    if Size rem Bloc_Size = 0 then
      Blocs_Nb := Size / Bloc_Size;
      Last_Bloc_Len := Bloc_Size;
    else
      Blocs_Nb := Size / Bloc_Size + 1;
      Last_Bloc_Len := Size rem Bloc_Size;
    end if;

    -- Count linefeed (newline) backwards and store tailing lines
    Lfs := 0;
    Stop := 1;
    Len := Bloc_Size;
    Offset := Char_Io.Size (File) + 1;
    Prev(Bloc_Size) := Aski.Lf;
    Scan:
    for I in reverse 1 .. Blocs_Nb loop
      -- Stop is in previous bloc
      In_Prev := True;
      -- Set reading index and adjust size of last bloc read
      if I = 1 then
        Offset := 1;
        Len := Last_Bloc_Len;
      else
        Offset := Offset - Bloc_Size;
      end if;
      -- Read
      Char_Io.Read (File, Chars, Offset);
      -- Locate LFs
      for J in reverse 1 .. Len loop
        if Chars(J) = Aski.Lf then
          -- Got a Lf
          if In_Prev then
            -- Concat with head of previous bloc
            Buffer := As.U.Tus (String (Chars(J + 1 .. Len)
                                     & Prev(1 .. Stop - 1)));
          else
            -- Extract line
            Buffer := As.U.Tus (String (Chars(J + 1 .. Stop - 1)));
          end if;
          if not Buffer.Is_Null then
            -- Insert in chrono order
            Matches.Insert (Buffer, As.U.Utils.Asu_Dyn_List_Mng.Prev);
          end if;
          -- Done
          In_Prev := False;
          Stop := J;
          Lfs := Lfs + 1;
          -- First detected Lf (at end of file) will generate an dummy empty
          --  line, so we take Tail+1 lines
          exit Scan when Tail /= 0 and then Lfs > Tail;
        end if;
      end loop;
      Prev := Chars;
    end loop Scan;

    -- Dump this tail
    if Debug.Logger.Is_On (Dump_Severity) then
      Dump_List ("Tail", Matches);
    end if;

    -- Time filter
    if Seconds /= 0 then
      if Matches.Is_Empty then
        return;
      end if;
      -- The length of the time in the line of text
      Time_Len := Date_Text.Length (Time_Fmt.Image);
      -- Current and reference time (above which we drop)
      Current_Time := Ada.Calendar.Clock;
      Ref_Time := Current_Time - Duration (Seconds);
      Debug.Logger.Log_Debug ("Time format >" & Time_Fmt.Image
                            & "< len: " & Time_Len'Img);

      -- Remove all lines that are before ref
      Matches.Rewind;
      loop
        -- Extract date
        Time := Date_Text.Scan (
            Matches.Access_Current.Slice (1, Time_Len),
            Time_Fmt.Image);
        -- Fix year if it is not set
        if Time.Years = Ada.Calendar.Year_Number'First then
          Time.Years := Ada.Calendar.Year (Current_Time);
          Line_Time := Time_Of (Time);
          -- Adjust year to -1  if result is above current time
          if Line_Time > Current_Time then
            Time.Years := Time.Years - 1;
            Line_Time := Time_Of (Time);
          end if;
        end if;
        -- Remove lines before ref time
        if Line_Time >= Ref_Time then
          -- Keep
          Moved := Matches.Check_Move;
          if Moved then
            Matches.Move_To;
          end if;
        else
          -- Drop
          Debug.Logger.Log_Debug (" Old " & Matches.Access_Current.Image);
          Matches.Delete (Moved => Moved);
        end if;
        exit when not Moved;
      end loop;
      if Debug.Logger.Is_On (Dump_Severity) then
        Dump_List ("In time", Matches);
      end if;
    end if;

    -- Remove all lines that do not match
    if Matches.Is_Empty then
      return;
    end if;
    Matches.Rewind;
    loop
      if Pattern.Match (Matches.Access_Current.Image, False) then
        Moved := Matches.Check_Move;
        if Moved then
          Matches.Move_To;
        end if;
      else
        Debug.Logger.Log_Debug (" Unmatching " & Matches.Access_Current.Image);
        Matches.Delete (Moved => Moved);
      end if;
      exit when not Moved;
    end loop;

    -- Dump the matching
    if Debug.Logger.Is_On (Dump_Severity) then
      Dump_List ("Matching", Matches);
    end if;

    Matches.Rewind (Check_Empty => False);
  exception
    when Error: others =>
      Debug.Logger.Log_Debug ("Exception in Search "
                            & Ada.Exceptions.Exception_Name (Error));
      Matches.Rewind (Check_Empty => False);
  end Search;

end Searcher;

