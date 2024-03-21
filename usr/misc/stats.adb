with Basic_Proc, Argument,
     As.U.Utils, Str_Util.Regex, Images, Normalization,
     Unbounded_Arrays, My_Math, Long_Longs, Scanner,
     Text_Line, Any_Def, Long_Long_Limited_Pool;
procedure Stats is

  -- Help and error messages
  procedure Usage is
  begin
    Basic_Proc.Put_Line_Error (
        "Usage: " & Argument.Get_Program_Name & " <scanner_format> <columns>");
    Basic_Proc.Put_Line_Error (
        "  <scanner_format>  // With %l and %r significant for statistics");
    Basic_Proc.Put_Line_Error (
        "  <columns>  ::= <<column> [ { ,<column> } ]");
  end Usage;

  procedure Error (Msg : in String) is
  begin
    Basic_Proc.Put_Line_Error ("ERROR: " & Msg & ".");
    Usage;
    Basic_Proc.Set_Error_Exit_Code;
  end Error;

  -- Arguments
  Format, Columns_Text : As.U.Asu_Us;

  -- Number of columns specified
  Nb_Columns : Positive;

  -- Column numbers
  type Positive_Array is array (Positive range <>) of Positive;
  package Positive_Uarrays is new Unbounded_Arrays (Positive, Positive_Array);
  Columns_Array : Positive_Uarrays.Unb_Array;

begin

  -- Help
  if Argument.Get_Nbre_Arg = 1 and then (Argument.Get_Parameter = "-h"
      or else Argument.Get_Parameter = "--help") then
    Usage;
    return;
  end if;
  -- One or two arguments
  if Argument.Get_Nbre_Arg /= 1 and then Argument.Get_Nbre_Arg /= 2 then
    Error ("Invalid arguments");
    return;
  end if;

  -- Get scanner format and optional columns
  Argument.Get_Parameter (Format, 1);
  if Argument.Get_Nbre_Arg = 2 then
    Argument.Get_Parameter (Columns_Text,  2);
  end if;

  -- Check scanner format
  declare
    -- Dummy format length
    Dummy_Len : Natural;
  begin
    Dummy_Len := Scanner.Length (Format.Image);
  exception
    when Scanner.Unknown_Length =>
      -- A format without fixed length is valid
      null;
    when Scanner.Invalid_Format =>
      Error ("Invalid format");
      return;
  end;

  -- Parse and check the columns
  if Columns_Text.Is_Null then
    return;
  end if;
  declare
    -- Split according to ','
    Columns_Texts : constant As.U.Utils.Asu_Array
                  := Str_Util.Regex.Split_Sep (Columns_Text.Image, ",");
  begin
    if Columns_Texts'Length = 0 then
      Error ("Invalid columns");
      return;
    end if;
    -- Store column numbers
    Nb_Columns := Columns_Texts'Length;
    for C of Columns_Texts loop
      Columns_Array.Append (Positive'Value (C.Image));
    end loop;
  exception
    when Constraint_Error =>
      -- Not a positive
      Error ("Invalid columns");
      return;
  end;

  -- Depending on the number of columns
  declare
    -- The column number
    Columns : constant Positive_Array := Columns_Array.To_Array;
    C : Positive;
    -- The totals, averages, and deviations of each column
    type Real_Array is array (1 .. Nb_Columns) of My_Math.Real;
    Val, Totals, Averages, Deviations : Real_Array := (others => 0.0);
    V, Nbr : My_Math.Real;

    -- Input flow and Current line of input
    Input : Text_Line.File_Type;
    Line : As.U.Asu_Us;

    -- The scanned sequence of Anys
    Scanned : Scanner.Any_Sequence;
    Anany : Any_Def.Any;

    -- The pool of arrays of values
    procedure Set (To : out Real_Array; Val : in Real_Array) is
    begin
      To := Val;
    end Set;
    package Line_Pool is new Long_Long_Limited_Pool (Real_Array,
                                                     Lifo => False, Set => Set);
    Pool : Line_Pool.Pool_Type;
    Nbu : Line_Pool.Ll_Natural;

    use type Any_Def.Any_Kind_List, Long_Longs.Llu_Natural, My_Math.Real;
  begin
    -- Scan stdin and parse
    Input.Open_All (Text_Line.In_File);
    loop
      Line := Input.Get;
      exit when Line.Is_Null;
      Text_Line.Trim (Line);
      Scanned := Scanner.Scan (Line.Image, Format.Image);
      -- Parse the Anys at indexes matching the columns (must be int or real)
      -- Store each value and update totals per column
      for I in Columns'Range loop
        C := Columns(I);
        Anany := Scanned.Element (C);
        if Anany.Kind = Any_Def.Lint_Kind then
          Val(I) := My_Math.Real (Anany.Lint);
        elsif Anany.Kind = Any_Def.Real_Kind then
          Val(I) := Anany.Real;
        else
          Error ("Invalid content " & Any_Def.Image (Anany) & ", at line "
              & Long_Longs.Image (Pool.Length + 1));
          return;
        end if;
        Totals(I) := Totals(I) + Val(I);
      end loop;
      Pool.Push (Val);
    end loop;
    Input.Close_All;

    -- Compute averages
    Nbu:= Pool.Length;
    Nbr:= My_Math.Real (Nbu);
    for I in 1 .. Nb_Columns loop
      Averages(I) := Totals(I) / Nbr;
    end loop;

    -- Compute Deviations
    while not Pool.Is_Empty loop
      Pool.Pop (Val);
      for I in 1 .. Nb_Columns loop
        V := Val(I) - Averages(I);
        Deviations(I) := Deviations(I) + V * V;
      end loop;
    end loop;

    -- Display results
    Basic_Proc.Put_Line_Output ("Nb lines: " & Long_Longs.Image (Nbu));
    for I in 1 .. Nb_Columns loop
       Deviations(I) := Deviations(I) / Nbr;
       Basic_Proc.Put_Line_Output ( "Col " & Images.Integer_Image (I)
           & ", Avg: " & Normalization.Normal_Fixed (Averages(I), 13, 10)
           & ", Dev: " & Normalization.Normal_Digits (Deviations(I), 11, 3));
    end loop;
  end;

end Stats;

