with My_Io, Normal, Sys_Calls, Arg_Parsing;
package body Display is
  use Common;

  Not_Found : exception;
  Nb_Cote_Line : constant := 12;

  Ok : Boolean := True;

  procedure Put_Val (Val : in Common.Pos_Float) is
  begin
    My_Io.Put (Val, Fore => 4, Aft => 3, Exp => 0);
  end Put_Val;

  procedure Put_It (It : in Common.Pos_Float) is
  begin
    My_Io.Put (It, Fore => 3, Aft => 3, Exp => 0);
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
    My_Io.Put (Char(Other_Kind(Kind)) & Normal(Cote, 3, True, '0') & ": ");

    Val := 0.0;
    for I in Data.Eff_Line_Range range 2 .. Last loop
      Search (Kind, Way(I-1), Way(I), Cote_No, Plus);
      if Arg_Parsing.Verbose then
        if Nb_Printed >= Nb_Cote_Line
        and then (Nb_Printed - 1) mod (Nb_Cote_Line - 1) = 0 then
          My_Io.New_Line;
          My_Io.Put ("      ");
        end if;
        if Plus then
          My_Io.Put ("+");
        else
          My_Io.Put ("-");
        end if;
        My_Io.Put (Char(Kind) & Normal(Cote_No, 3, True, '0') & " ");
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
      My_Io.New_Line;
    end if;

    My_Io.Put (" --> ");
    if Kind = Common.Manufa then
      My_Io.Put ("IT specified: ");
      Put_It (Data.Designs(Cote).Inter);
      My_Io.Put("   IT done: ");
      Put_It (Val);
      if Val > Data.Designs(Cote).Inter then
        My_Io.Put_Line (" NOT OK");
        Ok := False;
      else
        My_Io.New_Line;
      end if;
    else
      My_Io.Put ("Value : ");
      Put_Val (Val); 
      My_Io.Put (" +/- ");
      Put_It (Data.Manufas(Cote).Inter);
      My_Io.New_Line;
    end if;
    
  end Print;

  procedure Put_No_Way (Kind : in Common.Cote_Kind;
                        Cote : in Data.Eff_Cote_Range) is
  begin
    Sys_Calls.Put_Error ("No way for ");
    Sys_Calls.Put_Line_Error (Char(Other_Kind(Kind)) & Normal(Cote, 3, True, '0'));
    Ok := False;
  end Put_No_Way;

  procedure Print_Tittle (Kind : in Common.Cote_Kind) is
  begin
    if Kind = Common.Manufa then
      My_Io.Put_Line ("Check of feasibility");
      My_Io.Put_Line ("--------------------");
    else
      My_Io.Put_Line ("Resolution");
      My_Io.Put_Line ("----------");
    end if;
  end Print_Tittle;

  procedure Print_Result is
  begin
    if Ok then
      My_Io.Put_Line ("Simulation successful.");
    else
      My_Io.Put_Line ("Simulation FAILED.");
    end if;
  end Print_Result;

end Display;

