-- Pascal Triangle
with Basic_Proc, Images;
use Basic_Proc;
-- Compute elements of the 33 first lines lignes of the Pascal's triangle
procedure Pascal is

  -- Item computed and printed
  Element      : Long_Long_Integer;
  -- Table of results
  No_Line_Max : constant Integer := 33;
  Index_Max   : constant Integer := No_Line_Max + 1;
  subtype Typ_No_Line is Integer range 0 .. No_Line_Max;
  subtype Typ_Index is Integer range 1 .. Index_Max;
  Table : array(Typ_No_Line, Typ_Index) of Long_Long_Integer :=
   (others => (others => 0));

begin
  Put_Output ("Computation of the ");
  Put_Output (Images.Integer_Image(No_Line_Max));
  Put_Line_Output (" first lines of the PASCAL triangle:");
  New_Line_Output;

  Principale : for No_Line in Typ_No_Line loop

    -- Line number for computation
    Put_Output (Images.Integer_Image (No_Line));
    Put_Line_Output (" : ");

    Put_Output ("   ");
    for Index in Typ_Index range Typ_Index'First .. Typ_No_Line'Succ(
      No_Line) loop

      -- 6 elements per line of screen
      if ((Index - 1) mod 6 = 0) and then (Index /= 1) then
        New_Line_Output;
        Put_Output ("-> ");
      end if;

      -- Elements of the line, separated by '/'
      if Index = Typ_Index'First then
        Element := 1;
      else
        Element := Table(Typ_No_Line'Pred(No_Line), Typ_Index'Pred(Index)) +
                      Table(Typ_No_Line'Pred(No_Line), Index);
      end if;
      Table(No_Line, Index) := Element;
      Put_Output (Images.Long_Image (Element));
      if Index /= Typ_Index'Succ(No_Line) then
        Put_Output ('/');
      end if;
    end loop;

    -- End of processing
    New_Line_Output;
    New_Line_Output;
  end loop Principale;

exception

  -- One item is too big
  when others =>
    Put_Line_Output ("PROBLEM: Level is too big.");
    New_Line_Output;
end Pascal;

