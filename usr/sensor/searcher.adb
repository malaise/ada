with Bloc_Io, Aski, Long_Longs, Trace;
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
      Debug.Logger.Log (Dump_Severity, List.Access_Current.Image);
      exit when not List.Check_Move;
      List.Move_To;
    end loop;
  end Dump_List;

  -- Search the Pattern in the Tail last lines of File
  -- Clear and set the list to the matching lines
  procedure Search (File_Name : in String;
                    Tail    : in Filters.Tail_Length;
                    Pattern : access Regular_Expressions.Compiled_Pattern;
                    Matches : in out As.U.Utils.Asu_Dyn_List_Mng.List_Type) is
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
    use type Char_Io.Count, Char_Io.Element_Array;
  begin
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

    -- Count linefeed (newline) backwards and store lines
    Matches.Delete_List;
    Lfs := 0;
    Stop := 1;
    Len := Bloc_Size;
    Offset := Char_Io.Size (File) + 1;
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
        if Chars(I) = Aski.Lf then
          -- Got a Lf
          if In_Prev then
            -- Concat with head of previous bloc
            Buffer := As.U.Tus (String (Chars(J + 1 .. Len)
                                     & Prev(1 .. Stop - 1)));
          else
            -- Extract line
            Buffer := As.U.Tus (String (Chars(J + 1 .. Stop - 1)));
          end if;
          Matches.Insert (Buffer);
          In_Prev := False;
          Stop := J;
        end if;
      end loop;
      Prev := Chars;
    end loop;

    -- Dump this tail
    if Debug.Logger.Is_On (Dump_Severity) then
      Dump_List ("Tail", Matches);
    end if;

  end Search;

end Searcher;

