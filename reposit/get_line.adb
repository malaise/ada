with Ada.Characters.Latin_1;
with Sys_Calls;
package body Get_Line is

  F : Text_Line.File_Type;
  Fd : Sys_Calls.File_Desc;
  Current_Line : As.U.Utils.Asu_Ua.Unbounded_Array;
  Current_Line_No : Count;
  Cur : Positive;
  Current_Whole_Line : Line_Txt;
  First_Word : Line_Txt;
  Parsed : Boolean;
  Buff : As.U.Asu_Us;
  Word : Word_Txt;

  -- Opens the file. Exceptions are the one of Ada.Text_Io.Open (In_File)
  -- Loads the first line
  procedure Open (File_Name : in String) is
  begin
    Current_Line_No := 0;
    if F.Is_Open then
      raise Status_Error;
    end if;
    Fd := Sys_Calls.Open (File_Name, Sys_Calls.In_File);
    F.Open (Text_Line.In_File, Fd);
    Read_Next_Line;
  exception
    when Sys_Calls.Name_Error =>
      raise Name_Error;
    when Sys_Calls.System_Error =>
      raise Io_Error;
  end Open;

  procedure Close is
  begin
    if not F.Is_Open then
      raise Status_Error;
    end if;
    F.Close;
    Sys_Calls.Close (Fd);
  end Close;

  function Is_Separator (C : Character) return Boolean is
  begin
    return C = ' ' or else C = Ada.Characters.Latin_1.Ht;
  end Is_Separator;

  -- Next word of Buff (from Cur). "" if no more word.
  function Get_Next_Word return String is
    F, L : Positive;
    In_Word : Boolean := True;
  begin
    if Cur > Buff.Length then
      return "";
    end if;
    F := Cur;
    for I in Cur .. Buff.Length loop
      if Is_Separator (Buff.Element (I)) then
        if In_Word then
          L := I;
          In_Word := False;
        end if;
      else
        if not In_Word then
          Cur := I;
          return Buff.Slice (F, L - 1);
        end if;
      end if;
    end loop;
    Cur := Buff.Length + 1;
    if In_Word then
      return Buff.Slice (F, Buff.Length);
    else
      return "";
    end if;
  end Get_Next_Word;

  -- Reset Cur and parse leading spaces
  procedure Reset_Word is
  begin
    Current_Line := As.U.Utils.Asu_Ua.Null_Unbounded_Array;
    for I in 1 .. Buff.Length loop
      if Is_Separator (Buff.Element (I)) then
        null;
      else
        Cur := I;
        return;
      end if;
    end loop;
    Cur := Buff.Length + 1;
  end Reset_Word;

  -- Current line number
  function Get_Line_No return Positive_Count is
  begin
    if not F.Is_Open then
      raise Status_Error;
    end if;
    return Current_Line_No;
  end Get_Line_No;

  -- Get next line
  procedure Read_Next_Line is
  begin
    Parsed := False;
    if not F.Is_Open then
      raise Status_Error;
    end if;

    loop
      -- Get line from file, may raise End_Error
      Buff := F.Get;
      if Buff.Is_Null then
        raise End_Error;
      end if;
      Current_Line_No := Current_Line_No + 1;

      -- Strip tailing Lf is there is
      if Buff.Element (Buff.Length) = Text_Line.Line_Feed_Char then
        Buff.Delete (Buff.Length, Buff.Length);
      end if;

      -- Store the line as it is in Current_Whole_Line
      Current_Whole_Line := Buff;

      -- Remove trailing spaces
      while not Buff.Is_Null
      and then Is_Separator (Buff.Element (Buff.Length)) loop
        Buff.Delete (Buff.Length, Buff.Length);
      end loop;

      -- Remove leading spaces
      Reset_Word;

      -- Parse first word
      First_Word := As.U.Tus (Get_Next_Word);

      -- Done when no check of comments
      exit when Comment = "";

      -- Done when not empty
      --  and then neither first word nor first letters are the comment
      exit when not First_Word.Is_Null
      and then First_Word.Image /= Comment
      and then
       (Current_Whole_Line.Length < Comment'Length
        or else Current_Whole_Line.Slice (1, Comment'Length) /= Comment);
    end loop;

  end Read_Next_Line;


  -- Get the whole line (not parsed)
  procedure Get_Whole_Line (Line : in out Line_Txt) is
  begin
    if not F.Is_Open then
      raise Status_Error;
    end if;
    Line := Current_Whole_Line;
  end Get_Whole_Line;


  -- Get the first significant word of the line (not parsed)
  function Get_First_Word return String is
  begin
    if not F.Is_Open then
      raise Status_Error;
    end if;
    return First_Word.Image;
  end Get_First_Word;

  -- Internal
  procedure Parse_Words is
  begin
    if Parsed then
      return;
    end if;
    Reset_Word;
    -- Parse words
    loop
      -- Check word length
      Word := As.U.Tus (Get_Next_Word);

      -- Check no more word in line
      if Word.Is_Null then
        exit;
      end if;

      -- Store word
      Current_Line.Append (Word);
    end loop;
    Parsed := True;
  end Parse_Words;


  -- Number of words in currently loaded line
  function Get_Word_Number return Word_Count is
  begin
    if not F.Is_Open then
      raise Status_Error;
    end if;
    Parse_Words;
    return Current_Line.Length;
  end Get_Word_Number;

  -- Words of the currently loaded line
  function Get_Words return Line_Array is
  begin
    if not F.Is_Open then
      raise Status_Error;
    end if;
    Parse_Words;
    return Current_Line.To_Array;
  end Get_Words;

  procedure Get_Words (Line : in out As.U.Utils.Asu_Ua.Unbounded_Array) is
  begin
   if not F.Is_Open then
      raise Status_Error;
    end if;
    Parse_Words;
    Line := Current_Line;
  end Get_Words;

end Get_Line;

