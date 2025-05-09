with Arbitrary.Limited_List, Argument, Basic_Proc;
-- Pascal triangle with arbitrary precision numbers
procedure Arbipas is

  -- Prev/Current line
  procedure Set (To : out Arbitrary.Number; Val : in Arbitrary.Number) is
  begin
    To := Val;
  end Set;
  package Arbi_List_Mng is new Arbitrary.Limited_List (Arbitrary.Number, Set);
  Arbi_List : Arbi_List_Mng.List_Type;


  procedure Put_Number (N : Arbitrary.Number) is
    Str : constant String := N.Image;
  begin
    Basic_Proc.Put_Output (Str(2 .. Str'Last));
  end Put_Number;

  procedure Put_Line (N : Natural) is
    C : Arbitrary.Number;
    Done : Boolean;
  begin
    -- Init
    Arbi_List.Rewind;
    Basic_Proc.Put_Output (N'Img & ": ");
    loop
      Arbi_List.Read (C, Arbi_List_Mng.Next, Done);
      Put_Number (C);
      Basic_Proc.Put_Output (" ");
      exit when not Done;
    end loop;
    -- Add last one
    Basic_Proc.New_Line_Output;
  end Put_Line;

  procedure Compute_Line is
    P, C, N : Arbitrary.Number;
    Done : Boolean;
    use type Arbitrary.Number;
  begin
    -- Init
    Arbi_List.Rewind;
    P := Arbitrary.Zero;
    loop
      -- Compute New = Previous + Current
      Arbi_List.Read (N, Arbi_List_Mng.Current);
      C := P + N;
      -- Save new and shift
      Arbi_List.Modify (C, Arbi_List_Mng.Next, Done);
      P := N;
      exit when not Done;
    end loop;
    -- Add last one
    Arbi_List.Insert (Arbitrary.One);
  end Compute_Line;

  -- Number and current line no
  Nb_Line : Natural;
  No_Line : Natural;
begin

  -- Parse Argument
  begin
    if Argument.Get_Nbre_Arg = 0 then
      Nb_Line := 0;
    elsif  Argument.Get_Nbre_Arg = 1 then
      Nb_Line := Positive'Value (Argument.Get_Parameter);
    else
      raise Constraint_Error;
    end if;
  exception
    when Constraint_Error =>
      Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
       & " [ <nb_Line> ]");
      return;
  end;

  -- First line
  No_Line := 0;
  Arbi_List.Insert (Arbitrary.One);
  Put_Line (No_Line);

  while Nb_Line = 0 or else No_Line < Nb_Line loop
    No_Line := No_Line + 1;
    Compute_Line;
    Put_Line (No_Line);
  end loop;

end Arbipas;

