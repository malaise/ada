package body Get_Line is


  F : Text_Io.File_Type;
  Current_Line : Line_Array;
  Nb_Words : Word_Count;
  Current_Line_No : Text_Io.Count;
  Cur : Positive;
  Current_Whole_Line : Line_Txt;
  First_Word : Line_Txt;
  Parsed : Boolean;
  Buff : String (1 .. Max_Line_Len+1);
  Word : Word_Txt;
  Last : Natural;

  -- Opens the file. Exceptions are the one of TEXT_IO.OPEN (IN_FILE)
  -- Loads the first line
  procedure Open (File_Name : in String) is
  begin
    Current_Line_No := 0;
    Text_Io.Open (F, Text_Io.In_File, File_Name);
    Read_Next_Line;
  end Open;

  procedure Close is
  begin
    Text_Io.Close (F);
  end Close;

  -- Next word of BUFF (from CUR). "" if no more word.
  function Get_Next_Word return String is
    F, L : Positive;
    In_Word : Boolean := True;
  begin
    if Cur > Last then
      return "";
    end if;
    F := Cur;
    for I in Cur .. Last loop
      if Buff(I) = ' ' or Buff(I) = Ascii.Ht then
        if In_Word then
          L := I;
          In_Word := False;
        end if;
      else
        if not In_Word then
            Cur := I;
            return Buff (F .. L-1);
        end if;
      end if;
    end loop;
    Cur := Last + 1;
    if In_Word then
      return Buff (F .. Last);
    else
      return "";
    end if;
  end Get_Next_Word;

  -- Reset CUR and parse leading spaces
  procedure Reset_Word is
  begin
    Nb_Words := 0;
    for I in 1 .. Last loop
      if Buff(I) = ' ' or Buff(I) = Ascii.Ht then
        null;
      else
        Cur := I;
        return;
      end if;
    end loop;
    Cur := Last + 1;
  end Reset_Word;

  -- Current line number
  function Get_Line_No return Text_Io.Positive_Count is
  begin
    if not Text_Io.Is_Open (F) then
      raise Not_Open;
    end if;
    return Current_Line_No;
  end Get_Line_No;

  -- Get next line
  procedure Read_Next_Line is
  begin
    Parsed := False;
    if not Text_Io.Is_Open (F) then
      raise Not_Open;
    end if;

    loop
      -- Get line from file
      begin
        Text_Io.Get_Line (F, Buff, Last);
      exception
        when Text_Io.End_Error =>
          raise No_More_Line;
      end;

      Current_Line_No := Text_Io. "+" (Current_Line_No, 1);

      -- Check got line length
      if Last = Buff'Last then
        raise Line_Too_Long;
      end if;

      -- Store the line as it is in CURRENT_WHOLE_LINE
      Text_Handler.Set (Current_Whole_Line, Buff(1 .. Last));

      -- Remove trailing spaces
      while Last > 0
       and then (Buff(Last) = ' ' or else Buff(Last) = Ascii.Ht) loop
        Last := Last - 1;
      end loop;

      -- Remove leading spaces
      Reset_Word;

      -- Parse first word
      Text_Handler.Set (First_Word, Get_Next_Word);

      -- Done when no check of comments
      -- else go on if empty or comment
      exit when Comment = Ascii.Nul
      or else (not Text_Handler.Empty(First_Word)
               and then Text_Handler.Value(First_Word)(1) /= Comment);
    end loop;

  end Read_Next_Line;


  -- Get the whole line (not parsed)
  procedure Get_Whole_Line (Line : in out Line_Txt) is
  begin
    Text_Handler.Set (Line, Current_Whole_Line);
  end Get_Whole_Line;


    -- Get the first significant word of the line (not parsed)
  function Get_First_Word return String is
  begin
    if not Text_Io.Is_Open (F) then
      raise Not_Open;
    end if;
    return Text_Handler.Value(First_Word);
  end Get_First_Word;



  procedure Parse_Words is
  begin
    if Parsed then
      return;
    end if;
    Reset_Word;
    -- Parse words
    loop
      -- Check word length
      begin
        Text_Handler.Set (Word, Get_Next_Word);
      exception
        when Constraint_Error =>
          raise Word_Too_Long;
      end;

      -- Check no more word in line
      if Text_Handler.Length(Word) = 0 then
        exit;
      end if;

      -- Check word count
      if Nb_Words = Word_Range'Last then
        raise Too_Many_Words;
      end if;

      -- Store word
      Nb_Words := Nb_Words + 1;
      Text_Handler.Set (Current_Line(Nb_Words), Word);
    end loop;
    Parsed := True;
  end Parse_Words;


  -- Number of words in currently loaded line
  function Get_Word_Number return Word_Count is
  begin
    if not Text_Io.Is_Open (F) then
      raise Not_Open;
    end if;
    Parse_Words;
    return Nb_Words;
  end Get_Word_Number;


  -- Words of the currently loaded line
  procedure Get_Words (Line : in out Line_Array) is
  begin
    if not Text_Io.Is_Open (F) then
      raise Not_Open;
    end if;
    Parse_Words;
    for I in 1 .. Nb_Words loop
      Text_Handler.Set (Line(I), Current_Line(I));
    end loop;
    for I in Integer(Nb_Words) + 1 .. Word_Range'Last loop
      Text_Handler.Set (Line(I), "");
    end loop;
  end Get_Words;


end Get_Line;

