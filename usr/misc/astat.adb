-- For each file provided as argument format and put the number of Ada
--   statements of this file, then put the formated total:
--   statements, then % of comments then number of lines
-- If -q (--quiet), only put the total number of statments, not formated
-- If -s (--sort), sort by crescent number of statements
with Ada.Exceptions;
with Argument, Argument_Parser, As.U, Basic_Proc, Long_Long_Limited_List;
with One_File_Statements;
procedure Astat is
  -- Arguments and options
  Args : Argument_Parser.Parsed_Dscr;
  Java_Syntax : Boolean := False;
  Details : Boolean := True;
  Sort : Boolean := False;

  -- What is stored for each file
  type Cell_Rec is record
    Name : As.U.Asu_Us;
    Metric : One_File_Statements.Metrics;
  end record;
  Cell : Cell_Rec;
  procedure Set (To : out Cell_Rec; Val : in Cell_Rec) is
  begin
    To := Val;
  end Set;
  -- Storage
  package Cell_List_Mng is new Long_Long_Limited_List (Cell_Rec, Set);
  Cell_List : Cell_List_Mng.List_Type;
  -- Sorting
  function Less_Than (El1, El2 : Cell_Rec) return Boolean is
    (El1.Metric.Statements < El2.Metric.Statements);
  procedure Sort_Cells is new Cell_List_Mng.Sort (Less_Than);

  -- Global statistics
  Max_Len : Positive;
  Total : One_File_Statements.Metrics;

  Keys : constant Argument_Parser.The_Keys_Type := (
    1 => (False, 'h', As.U.Tus ("help"),  False),
    2 => (False, 'q', As.U.Tus ("quiet"), False),
    3 => (False, 'j', As.U.Tus ("java"),  False),
    4 => (False, 's', As.U.Tus ("sort"),  False) );

  procedure Usage is
  begin
    Basic_Proc.Put_Line_Output ("Usage: " & Argument.Get_Program_Name
      & " [ -q | --quiet ] [ -j | --java] [ -s | --sort ] { <file_name> }");
  end Usage;

 Abort_Error : exception;
  procedure Error (Msg : in String) is
  begin
    Basic_Proc.Put_Line_Error ("ERROR: " & Msg);
    raise Abort_Error;
  end Error;

  use type Cell_List_Mng.Ll_Natural;

begin
  ------------------
  -- Parse arguments
  ------------------
  -- General validity
  Args := Argument_Parser.Parse (Keys);
  if not Args.Is_Ok then
    Error ("Invalid arguments: " & Args.Get_Error);
  end if;
  if Args.Get_Nb_Embedded_Arguments /= 0
  or else Args.Get_Nb_Occurences (Argument_Parser.No_Key_Index) = 0 then
    Error ("Invalid arguments");
  end if;
  -- Options
  if Args.Is_Set (1) then
    -- Help
    Usage;
    Basic_Proc.Set_Error_Exit_Code;
    return;
  end if;
  if Args.Is_Set (2) then
    Details := False;
  end if;
  if Args.Is_Set (3) then
    Java_Syntax := True;
  end if;
  if Args.Is_Set (4) then
    Sort := True;
  end if;

  -- Store names and stats, and compute stats and longest file name
  Max_Len := 1;
  for Arg in 1 .. Args.Get_Nb_Occurences(Argument_Parser.No_Key_Index) loop
    -- Appen current file
    Cell.Name.Set (Args.Get_Option (Argument_Parser.No_Key_Index, Arg));
    Cell.Metric := One_File_Statements.Count_Statements_Of_File (
        Cell.Name.Image, Java_Syntax);
    Cell_List.Insert (Cell);
    -- Update totals
    Total.Statements := Total.Statements + Cell.Metric.Statements;
    Total.Comments := Total.Comments + Cell.Metric.Comments;
    Total.Lines := Total.Lines + Cell.Metric.Lines;
    if Cell.Name.Length > Max_Len then
      Max_Len := Cell.Name.Length;
    end if;
  end loop;

  -- Put header of list
  if Details then
    One_File_Statements.Put_Header (Max_Len);
  end if;
  if  Cell_List.Is_Empty then
    return;
  end if;

  -- Sort
  if Sort then
    Sort_Cells (Cell_List);
  end if;

  -- Put all files
  if Details then
    Cell_List.Rewind;
    loop
      One_File_Statements.Put_File (Cell_List.Access_Current.Name.Image,
                                    Cell_List.Access_Current.Metric,
                                    Max_Len);
      exit when Cell_List.Get_Position = Cell_List.List_Length;
      Cell_List.Move_To;
    end loop;
  end if;

  -- Total
  One_File_Statements.Put_Total (Total, Details, Max_Len);

exception
  when Abort_Error =>
    Basic_Proc.Set_Error_Exit_Code;
  when Error: others =>
    Basic_Proc.Put_Line_Error ("Unexpected exception: "
      & Ada.Exceptions.Exception_Name (Error) & " raised.");
    Basic_Proc.Set_Error_Exit_Code;
    raise;
end Astat;

