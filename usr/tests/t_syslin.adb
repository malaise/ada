-- One argument : file name
-- Reads this file describing a linear system (get_line & get_float)
-- Solve linear system and put solution

with Basic_Proc, As.U.Utils, Argument, Normal, Syslin, Images, Get_Line,
     Gets;

procedure T_Syslin is

  package My_Syslin is new Syslin(Float);
  function My_Image is new Images.Flo_Image(Float);

  -- Matrix dimension
  Dim : Positive := 1;

  File_Error : exception;


begin
  -- Check syntax
  if Argument.Get_Nbre_Arg /= 1 then
    Basic_Proc.Put_Line_Output ("ERROR. Syntax : t_syslin <file_name>");
    return;
  end if;

  declare
    package My_Get_Line is new Get_Line (Comment => "#");

    Whole_Line : My_Get_Line.Line_Txt;
    Line : As.U.Utils.Asu_Ua.Unbounded_Array;

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
        Basic_Proc.Put_Line_Output ("ERROR opening file "
                                  & Argument.Get_Parameter & ".");
        raise;
    end;

    -- Compute dimension from first line : nb of words - 1
    begin
      My_Get_Line.Get_Words (Line);
      My_Get_Line.Get_Whole_Line (Whole_Line);
      Dim := My_Get_Line.Get_Word_Number - 1;
    exception
      when Constraint_Error =>
        Basic_Proc.Put_Line_Output ("ERROR in file " & Argument.Get_Parameter
                          & " only one word in first line.");
        raise File_Error;
      when Basic_Proc.End_Error =>
        Basic_Proc.Put_Line_Output ("ERROR in file " & Argument.Get_Parameter
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
            Matrix (I, J) := Gets.Get_Int_Or_Float(Line.Element (J).Image);
          end loop;
          Vector (I) := Gets.Get_Int_Or_Float(Line.Element (Dim+1).Image);
        exception
          when others =>
            Basic_Proc.Put_Line_Output ("ERROR, when reading data at line "
                          & Integer'Image(My_Get_Line.Get_Word_Number) & ".");
            raise File_Error;
        end;
        Basic_Proc.Put_Line_Output (">" & Whole_Line.Image & "<");

        if I /= Dim then
          -- read next not empty line
          Read_Next_Significant_Line;

          -- Check number of words
          if My_Get_Line.Get_Word_Number /= Dim + 1 then
            Basic_Proc.Put_Line_Output (
                "ERROR in file. Wrong number of words at line "
              & Integer'Image(My_Get_Line.Get_Word_Number) & ".");
            raise File_Error;
          end if;
        end if;

      end loop;

      -- Check nothing else in file
      begin
        Read_Next_Significant_Line;
        Basic_Proc.Put_Line_Output ("ERROR. Unexpected data at line "
                          & Integer'Image(My_Get_Line.Get_Word_Number) & ".");
        My_Get_Line.Close;
        raise File_Error;
      exception
        when My_Get_Line.End_Error =>
          My_Get_Line.Close;
      end;

      -- Solve
      begin
        Solution :=  My_Syslin.Gauss(Matrix, Vector);
      exception
        when My_Syslin.Null_Determinant =>
          Basic_Proc.Put_Line_Output ("Unable to solve: Determinant is nul.");
          return;
        when My_Syslin.Dimension_Error =>
          Basic_Proc.Put_Line_Output ("Unable to solve: ERROR in dimensions.");
          return;
        when others =>
          Basic_Proc.Put_Line_Output ("ERROR solving linear system.");
          raise;
      end;

      -- Put solution
      for I in 1 .. Dim loop
        Basic_Proc.Put_Output ("X(" & Normal(I, 3) & ") = ");
        Basic_Proc.Put_Output (My_Image (Solution(I)));
        Basic_Proc.New_Line_Output;
      end loop;

    end;
  end;
  -- Done
end T_Syslin;

