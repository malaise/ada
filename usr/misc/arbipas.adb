with Ada.Text_Io;
with Arbitrary, Dynamic_List, Argument;
-- Pascal triangle with arbitrary precision numbers
procedure Arbipas is

  -- Prev/Current line
  package Arbi_Dyn_List_Mng is new Dynamic_List (Arbitrary.Number);
  package Arbi_List_Mng renames Arbi_Dyn_List_Mng.Dyn_List;
  Arbi_List : Arbi_List_Mng.List_Type;


  procedure Put_Number (N : Arbitrary.Number) is
    Str : constant String := Arbitrary.Image (N);
  begin
    Ada.Text_Io.Put (Str(2 .. Str'Last));
  end Put_Number;

  procedure Put_Line (N : Natural) is
    C : Arbitrary.Number;
    Done : Boolean;
  begin
    -- Init
    Arbi_List_Mng.Rewind (Arbi_List);
    Ada.Text_Io.Put (N'Img & ": ");
    loop
      Arbi_List_Mng.Read (Arbi_List, C, Arbi_List_Mng.Next, Done);
      Put_Number (C);
      Ada.Text_Io.Put (" ");
      exit when not Done;
    end loop;
    -- Add last one
    Ada.Text_Io.New_Line;
  end Put_Line;

  procedure Compute_Line is
    P, C, N : Arbitrary.Number;
    Done : Boolean;
    use type Arbitrary.Number;
  begin
    -- Init
    Arbi_List_Mng.Rewind (Arbi_List);
    P := Arbitrary.Zero;
    loop
      -- Compute New = Previous + Current
      Arbi_List_Mng.Read (Arbi_List, N, Arbi_List_Mng.Current);
      C := P + N;
      -- Save new and shift
      Arbi_List_Mng.Modify (Arbi_List, C, Arbi_List_Mng.Next, Done);
      P := N;
      exit when not Done;
    end loop;
    -- Add last one
    Arbi_List_Mng.Insert (Arbi_List, Arbitrary.One);
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
      Ada.Text_Io.Put_Line ("Usage: " & Argument.Get_Program_Name
       & " [ <nb_Line> ]");
      return;
  end;

  -- First line
  No_Line := 0;
  Arbi_List_Mng.Insert (Arbi_List, Arbitrary.One);
  Put_Line (No_Line);

  while Nb_Line = 0 or else No_Line < Nb_Line loop
    No_Line := No_Line + 1;
    Compute_Line;
    Put_Line (No_Line);
  end loop;

end Arbipas;

