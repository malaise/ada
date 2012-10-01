with Normalization, Basic_Proc, Arg_Parsing;
package body Display is
  use Common;

  Not_Found : exception;
  Nb_Cote_Line : constant := 12;

  Ok : Boolean := True;

  procedure Put_Val (Val : in Common.Pos_Float) is
  begin
    Basic_Proc.Put_Output (Normalization.Normal_Fixed (Val, 8, 3));
  end Put_Val;

  procedure Put_It (It : in Common.Pos_Float) is
  begin
    Basic_Proc.Put_Output (Normalization.Normal_Fixed (It, 7, 3));
  end Put_It;

  function Other_Kind (Kind : Common.Cote_Kind) return Common.Cote_Kind is
  begin
    if Kind = Common.Manufa then
      return Common.Design;
    else
      return Common.Manufa;
    end if;
  end Other_Kind;

  procedure Search (Kind : in Common.Cote_Kind;
                    Start, Stop : in Data.Eff_Line_Range;
                    Cote_No : out Data.Eff_Cote_Range;
                    Plus : out Boolean) is
  begin
    if Kind = Common.Manufa then
      for I in Data.Eff_Cote_Range loop
        if Data.Manufas(I).Start = Start
        and then Data.Manufas(I).Stop = Stop then
          Cote_No := I;
          Plus := True;
          return;
        elsif Data.Manufas(I).Start = Stop
        and then Data.Manufas(I).Stop = Start then
          Cote_No := I;
          Plus := False;
          return;
        end if;
      end loop;
      raise Not_Found;
    else
      for I in Data.Eff_Cote_Range loop
        if Data.Designs(I).Start = Start
        and then Data.Designs(I).Stop = Stop then
          Cote_No := I;
          Plus := True;
          return;
        elsif Data.Designs(I).Start = Stop
        and then Data.Designs(I).Stop = Start then
          Cote_No := I;
          Plus := False;
          return;
        end if;
      end loop;
      raise Not_Found;
    end if;
  end Search;

  function Char (Kind : in Common.Cote_Kind) return Character is
  begin
    if Kind = Common.Manufa then
      return 'M';
    else
      return 'S';
    end if;
  end Char;

  procedure Print (Kind : in Common.Cote_Kind;
                   Cote : in Data.Eff_Cote_Range;
                   Way  : in Way_Vector) is
    Cote_No : Data.Eff_Cote_Range;
    Plus : Boolean;
    Nb_Printed : Natural := 0;
    Val : Float;
    Last : constant Natural := Way'Last;
  begin
    Basic_Proc.Put_Output (Char(Other_Kind(Kind))
                      & Normalization.Normal_Int (Cote, 3, True, '0') & ": ");

    Val := 0.0;
    for I in Data.Eff_Line_Range range 2 .. Last loop
      Search (Kind, Way(I-1), Way(I), Cote_No, Plus);
      if Arg_Parsing.Verbose then
        if Nb_Printed >= Nb_Cote_Line
        and then (Nb_Printed - 1) mod (Nb_Cote_Line - 1) = 0 then
          Basic_Proc.New_Line_Output;
          Basic_Proc.Put_Output ("      ");
        end if;
        if Plus then
          Basic_Proc.Put_Output ("+");
        else
          Basic_Proc.Put_Output ("-");
        end if;
        Basic_Proc.Put_Output (Char(Kind)
              & Normalization.Normal_Int (Cote_No, 3, True, '0') & " ");
      end if;
      Nb_Printed := Nb_Printed + 1;
      if Kind = Common.Manufa then
        -- Check Inter
        Val := Val + abs(Data.Manufas(Cote_No).Inter);
      else
        -- Add Value
        if Plus then
          Val := Val + Data.Designs(Cote_No).Value;
        else
          Val := Val - Data.Designs(Cote_No).Value;
        end if;
      end if;
    end loop;
    if Arg_Parsing.Verbose then
      Basic_Proc.New_Line_Output;
    end if;

    Basic_Proc.Put_Output (" --> ");
    if Kind = Common.Manufa then
      Basic_Proc.Put_Output ("IT specified: ");
      Put_It (Data.Designs(Cote).Inter);
      Basic_Proc.Put_Output ("   IT done: ");
      Put_It (Val);
      if Val > Data.Designs(Cote).Inter then
        Basic_Proc.Put_Line_Output (" NOT OK");
        Ok := False;
      else
        Basic_Proc.New_Line_Output;
      end if;
    else
      Basic_Proc.Put_Output ("Value : ");
      Put_Val (Val);
      Basic_Proc.Put_Output (" +/- ");
      Put_It (Data.Manufas(Cote).Inter);
      Basic_Proc.New_Line_Output;
    end if;

  end Print;

  procedure Put_No_Way (Kind : in Common.Cote_Kind;
                        Cote : in Data.Eff_Cote_Range) is
  begin
    Basic_Proc.Put_Error ("No way for ");
    Basic_Proc.Put_Line_Error (Char(Other_Kind(Kind))
           & Normalization.Normal_Int (Cote, 3, True, '0'));
    Ok := False;
  end Put_No_Way;

  procedure Print_Tittle (Kind : in Common.Cote_Kind) is
  begin
    if Kind = Common.Manufa then
      Basic_Proc.Put_Line_Output ("Check of feasibility");
      Basic_Proc.Put_Line_Output ("--------------------");
    else
      Basic_Proc.Put_Line_Output ("Resolution");
      Basic_Proc.Put_Line_Output ("----------");
    end if;
  end Print_Tittle;

  procedure Print_Result is
  begin
    if Ok then
      Basic_Proc.Put_Line_Output ("Simulation successful.");
    else
      Basic_Proc.Put_Line_Output ("Simulation FAILED.");
    end if;
  end Print_Result;

end Display;

