-- One argument : file name
-- Reads this file describing a linear system (get_line & get_float)
-- Solve linear system and put solution

with Ada.Text_Io;
with Text_Handler, Argument, Normal, Syslin, Flo_Io, Get_Line, Get_Float;

procedure T_Syslin is

  package My_Syslin is new Syslin(Float);

  Max_Line_Len : constant := 1024;
  Max_Word_Nb  : constant := 500;
  Max_Word_Len : constant := 15;

  -- Matrix dimension
  Dim : Positive := 1;

  File_Error : exception;


begin
  -- Check syntax
  if Argument.Get_Nbre_Arg /= 1 then
    Ada.Text_Io.Put_Line ("ERROR. Syntax : t_syslin <file_name>");
    return;
  end if;

  declare
    package My_Get_Line is new Get_Line (
      Max_Word_Len => Max_Word_Len,
      Max_Word_Nb  => Max_Word_Nb,
      Max_Line_Len => Max_Line_Len,
      Comment => "#");

    Line  : My_Get_Line.Line_Array;
    Whole_Line : My_Get_Line.Line_Txt;

    procedure Read_Next_Significant_Line is
    begin
      My_Get_Line.Read_Next_Line;
      My_Get_Line.Get_Words (Line);
      My_Get_Line.Get_Whole_Line (Whole_Line);
    end Read_Next_Significant_Line;

  begin

    -- open file
    begin
      My_Get_Line.Open (Argument.Get_Parameter);
    exception
      when others =>
        Ada.Text_Io.Put_Line ("ERROR opening file " & Argument.Get_Parameter & ".");
        raise;
    end;

    -- Compute dimension from first line : nb of words - 1
    begin
      My_Get_Line.Get_Words (Line);
      My_Get_Line.Get_Whole_Line (Whole_Line);
      Dim := My_Get_Line.Get_Word_Number - 1;
      if Dim = 0 then
        Ada.Text_Io.Put_Line ("ERROR in file " & Argument.Get_Parameter
                          & " only one word in first line.");
        raise File_Error;
      end if;
    exception
      when Ada.Text_Io.End_Error =>
        Ada.Text_Io.Put_Line ("ERROR in file " & Argument.Get_Parameter
                          & " file is empty.");
        raise File_Error;
    end;

    -- Load matrix and vector from file.
    declare
      Matrix : My_Syslin.Matrix(1..Dim, 1..Dim);
      Vector : My_Syslin.Vector(1..Dim);
      Solution : My_Syslin.Vector(1..Dim);
    begin
      for I in 1 .. Dim loop
        -- Parse current line in matrix and vector
        begin
          for J in 1 .. Dim loop
            Matrix (I, J) := Get_Float.Get_Float(Text_Handler.Value(Line(J)));
          end loop;
          Vector (I) := Get_Float.Get_Float(Text_Handler.Value(Line(Dim+1)));
        exception
          when others =>
            Ada.Text_Io.Put_Line ("ERROR, when reading data at line "
                              & Integer'Image(My_Get_Line.Get_Word_Number) & ".");
            raise File_Error;
        end;
        Ada.Text_Io.Put_Line (">" & Text_Handler.Value(Whole_Line) & "<");

        if I /= Dim then
          -- read next not empty line
          Read_Next_Significant_Line;

          -- Check number of words
          if My_Get_Line.Get_Word_Number /= Dim + 1 then
            Ada.Text_Io.Put_Line ("ERROR in file. Wrong number of words at line "
                              & Integer'Image(My_Get_Line.Get_Word_Number) & ".");
            raise File_Error;
          end if;
        end if;

      end loop;

      -- Check nothing else in file
      begin
        Read_Next_Significant_Line;
        Ada.Text_Io.Put_Line ("ERROR. Unexpected data at line "
                          & Integer'Image(My_Get_Line.Get_Word_Number) & ".");
        My_Get_Line.Close;
        raise File_Error;
      exception
        when My_Get_Line.No_More_Line =>
          My_Get_Line.Close;
      end;

      -- Solve
      begin
        Solution :=  My_Syslin.Gauss(Matrix, Vector);
      exception
        when My_Syslin.Discriminent_Error =>
          Ada.Text_Io.Put_Line ("Unable to solve: Discriminent is nul.");
          return;
        when My_Syslin.Dimension_Error =>
          Ada.Text_Io.Put_Line ("Unable to solve: ERROR in dimensions.");
          return;
        when others =>
          Ada.Text_Io.Put_Line ("ERROR solving linear system.");
          raise;
      end;

      -- Put solution
      for I in 1 .. Dim loop
        Ada.Text_Io.Put ("X(" & Normal(I, 3) & ") = ");
        Flo_Io.Put (Solution(I), Aft => 6);
        Ada.Text_Io.New_Line;
      end loop;

    end;
  end;
  -- Done
end T_Syslin;
