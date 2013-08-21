with As.U.Utils, Basic_Proc, My_Math, Get_Line, Gets;

package body File is

  Max_Dim : constant := Types.Max_Dim;

  File_Read : Boolean := False;

  -- Input mattrix splitted in rows to reduce size of object
  type Float_Cell_Range is digits 5 range 0.00 .. 100.00;
  type Input_Row_Tab is array (1 .. Max_Dim) of Float_Cell_Range;
  type Input_Row_Tab_Access is access Input_Row_Tab;
  type Input_Mattrix_Tab is array (1 .. Max_Dim) of Input_Row_Tab_Access;
  Input_Mattrix : constant Input_Mattrix_Tab
                := (others => new Input_Row_Tab);

  Loc_Kind : Types.Mattrix_Kind_List;

  function Read (File_Name : String) return Types.Mattrix_Rec is

    package My_Get_Line is new Get_Line(Comment => "#");
    Line  : As.U.Utils.Asu_Ua.Unbounded_Array;
    Dim : Positive;
    F   : Float_Cell_Range;

    procedure Read_Next_Significant_Line is
    begin
      My_Get_Line.Read_Next_Line;
      My_Get_Line.Get_Words (Line);
    end Read_Next_Significant_Line;

    use My_Math;
  begin
    -- Open file
    begin
      My_Get_Line.Open (File_Name);
    exception
      when others =>
        Basic_Proc.Put_Line_Error ("ERROR opening file " & File_Name);
        raise Read_Error;
    end;

    -- Read mattrix kind
    begin
      My_Get_Line.Get_Words (Line);
    exception
      when My_Get_Line.End_Error =>
        Basic_Proc.Put_Line_Error ("ERROR in file " & File_Name
                          & ". File is empty.");
        My_Get_Line.Close;
        raise Read_Error;
    end;
    if My_Get_Line.Get_Word_Number /= 1 then
      Basic_Proc.Put_Line_Error ("ERROR in file " & File_Name
            & ", only one word, WISH or REGRET, allowed in first line.");
      My_Get_Line.Close;
      raise Read_Error;
    end if;
    begin
      Loc_Kind := Types.Mattrix_Kind_List'Value (Line.Element (1).Image);
    exception
      when others =>
        Basic_Proc.Put_Line_Error ("ERROR in file " & File_Name
              & ", only one word, WISH or REGRET, allowed in first line.");
        My_Get_Line.Close;
        raise Read_Error;
    end;

    -- Compute dimension from second line : nb of words
    begin
      Read_Next_Significant_Line;
      Dim := My_Get_Line.Get_Word_Number;
    exception
      when My_Get_Line.End_Error =>
        Basic_Proc.Put_Line_Error ("ERROR in file " & File_Name & ", no mattrix.");
        My_Get_Line.Close;
        raise Read_Error;
      when others =>
        Basic_Proc.Put_Line_Error ("ERROR in file " & File_Name
                          & ", impossible to compute mattrix size.");
        My_Get_Line.Close;
        raise Read_Error;
    end;

    -- Load matrix and vector from file.
    for I in 1 .. Dim loop
      -- Parse current line in matrix and vector
      begin
        for J in 1 .. Dim loop
          F := Float_Cell_Range (Gets.Get_Int_Float (Line.Element(J).Image));
          if F > 100.00
          or else My_Math.Frac (My_Math.Real (F)) * 100.0 > 100.0 then
            My_Get_Line.Close;
            raise Read_Error;
          end if;
          Input_Mattrix(I).all(J) := F;
        end loop;
      exception
        when others =>
          Basic_Proc.Put_Line_Error ("ERROR, when reading data at line "
                & My_Get_Line.Count'Image (My_Get_Line.Get_Line_No)
                & " of file " & File_Name);
          My_Get_Line.Close;
          raise Read_Error;
      end;

      if I /= Dim then
        -- read next not empty line
        Read_Next_Significant_Line;

        -- Check number of words
        if My_Get_Line.Get_Word_Number /= Dim then
          Basic_Proc.Put_Line_Error ("ERROR in file. Wrong number of words at line "
                & My_Get_Line.Count'Image(My_Get_Line.Get_Line_No)
                & " of file " & File_Name);
          My_Get_Line.Close;
          raise Read_Error;
        end if;
      end if;

    end loop;

    -- Check nothing else in file
    begin
      Read_Next_Significant_Line;
      Basic_Proc.Put_Line_Error ("ERROR. Unexpected data at line "
            & My_Get_Line.Count'Image (My_Get_Line.Get_Line_No)
            & " of file " & File_Name);
      My_Get_Line.Close;
      raise Read_Error;
    exception
      when My_Get_Line.End_Error =>
        -- Ok, go on
        My_Get_Line.Close;
    end;

    declare
     Loc_Mattrix : Types.Mattrix_Tab (1 .. Dim, 1 .. Dim);
    begin
      -- Affect to Integers
      for I in 1 .. Dim loop
        for J in 1 .. Dim loop
          Loc_Mattrix(I, J) := Types.Cell_Range(
             My_Math.Round (My_Math.Real((Input_Mattrix(I).all(J))) * 100.0));
        end loop;
      end loop;

      if Types."=" (Loc_Kind, Types.Wish) then
        -- Make a regret mattrix by substracting each to 10_000
        for I in 1 .. Dim loop
          for J in 1 .. Dim loop
            Loc_Mattrix(I, J) := 10_000 - Loc_Mattrix(I, J);
          end loop;
        end loop;
      end if;

      File_Read := True;
      return (Dim => Dim, Notes => Loc_Mattrix);
    end;

  end Read;

  function Get_Kind return Types.Mattrix_Kind_List is
  begin
    if not File_Read then
      raise File_Not_Read;
    end if;
    return Loc_Kind;
  end Get_Kind;

  function Get_Note (Row, Col : Positive) return Float is
  begin
    if not File_Read then
      raise File_Not_Read;
    end if;
    return Float(Input_Mattrix(Row).all(Col));
  end Get_Note;

end File;

