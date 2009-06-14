with Ada.Exceptions, Ada.Text_Io;

with Text_Handler, Get_Line;

with Space.Board, Pieces, Image;

package body Set_Up is

  -- WPe1 = White Pawn at e1
  package My_Get_Line is new Get_Line (4, 132, 132, "#");
  Line : My_Get_Line.Line_Array;

  -- Load_Error : exception;
  function Load (File_Name : in String) return Space.Color_List is
    Char : Character;
    Str2 : Image.Square_Str;
    Decoded_Id : Space.Board.Orig_Piece_Id (True);
    Decoded_Square : Space.Square_Coordinate;
    Orig_Id :  Space.Board.Orig_Piece_Id;

    use type Pieces.Piece_Kind_List;
    use type Space.Color_List, Space.Board.Orig_Piece_Id;
  begin
    Open:
    begin
      My_Get_Line.Open (File_Name);
    exception
      when Ada.Text_Io.Name_Error =>
        Ada.Text_Io.Put_Line ("Error. Cannot find set-up file " & File_Name);
        return Space.White;
      when My_Get_Line.No_More_Line =>
        Ada.Text_Io.Put_Line ("Warning. Empty set-up file " & File_Name);
        My_Get_Line.Close;
        return Space.White;
      when Error : others =>
        Ada.Text_Io.Put_Line ("Error. Cannot open set-up file " & File_Name);
        Ada.Text_Io.Put_Line ("Exception " & Ada.Exceptions.Exception_Name (Error));
        raise Load_Error;
    end Open;

    One_Line:
    loop
      My_Get_Line.Get_Words (Line);

      One_Word:
      for I in 1 .. My_Get_Line.Get_Word_Number loop
        if Text_Handler.Length (Line(I)) /= 4 then
          Ada.Text_Io.Put_Line ("Error. Invalid definition " & Text_Handler.Value (Line(I))
           & " at line " & Ada.Text_Io.Positive_Count'Image (My_Get_Line.Get_Line_No) );
           raise Load_Error;
        end if;

        -- Read color char
        Char := Text_Handler.Value (Line(I))(1);
        if Char = 'W' then
          Decoded_Id.Id.Color := Space.White;
        elsif Char = 'B' then
          Decoded_Id.Id.Color := Space.Black;
        else
          Ada.Text_Io.Put_Line ("Error. Invalid color " & Char
           & " at line " & Ada.Text_Io.Positive_Count'Image (My_Get_Line.Get_Line_No) );
           raise Load_Error;
        end if;

        -- Read kind char
        Char := Text_Handler.Value (Line(I))(2);
        if Char = 'P' then
          Decoded_Id.Id.Kind := Pieces.Pawn;
        else
          Decoded_Id.Id.Kind := Image.Piece_Value (Char);
          if Decoded_Id.Id.Kind = Pieces.Pawn then
            Ada.Text_Io.Put_Line ("Error. Invalid piece " & Char
             & " at line " & Ada.Text_Io.Positive_Count'Image (My_Get_Line.Get_Line_No) );
            raise Load_Error;
          end if;
        end if;

        -- Read square
        Str2 :=  Text_Handler.Value (Line(I))(3 ..4);
        begin
          Decoded_Square := Image.Square_Value (Str2);
        exception
          when Image.Value_Error =>
            Ada.Text_Io.Put_Line ("Error. Invalid square " & Str2
             & " at line " & Ada.Text_Io.Positive_Count'Image (My_Get_Line.Get_Line_No) );
            raise Load_Error;
        end;

        -- Check piece may not have moved (same Orig_Piece_Id)
        Orig_Id := Space.Board.Orig_Piece_Id_At (Decoded_Square);
        begin
          Space.Board.Create_Piece (Decoded_Id.Id.Kind, Decoded_Id.Id.Color, Decoded_Square,
                                    Has_Moved => Orig_Id /= Decoded_Id);
        exception
          when others =>
            Ada.Text_Io.Put_Line ("Error. Non empty square " & Text_Handler.Value (Line(I))
             & " at line " & Ada.Text_Io.Positive_Count'Image (My_Get_Line.Get_Line_No) );
            raise Load_Error;
        end;
      end loop One_Word;

      Try_Line:
      begin
        My_Get_Line.Read_Next_Line;
      exception
        when My_Get_Line.No_More_Line =>
          My_Get_Line.Close;
          return Decoded_Id.Id.Color;
      end Try_Line;

    end loop One_Line;

  exception
    when Load_Error =>
      raise;
    when Error : others =>
      Ada.Text_Io.Put_Line ("Error reading file " & File_Name
         & " at line " & Ada.Text_Io.Positive_Count'Image (My_Get_Line.Get_Line_No)
         & " exception " &  Ada.Exceptions.Exception_Name (Error));
      begin
        My_Get_Line.Close;
      exception
        when others => null;
      end;
      raise;
  end Load;

end Set_Up;

