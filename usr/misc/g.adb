-- Small game. Player shall compute the remainig if division by 3 of a random
-- number and add -1, 0 or 1 so that the remaining is 0.
-- Then do the same on the quotien of this division... until the quotien
-- becomes 0.
with Ada.Calendar;
with Aski, Rnd, Clear_Screen, Basic_Proc, Normal, Key_Pressed;
procedure G is
  -- Generated number
  subtype Number is Natural range 0 .. 999_999_999;
  Num : Number;

  -- Result of division of current value
  type Div_Res is (Minus_1, Zero, Plus_1);

  Got_Res, Res : Div_Res;
  Success : Boolean;

  Start_Time : Ada.Calendar.Time;
  Time_Spent : Natural;

  function Rand return Positive is
    Ret : Positive;
  begin
    Ret := Rnd.Gen.Int_Random (1, 9);
    for I in 1 .. 8 loop
      Ret := Ret * 10 + Rnd.Gen.Int_Random (0, 9);
    end loop;
    return Ret;
  end Rand;

  procedure Div (N : in Number;
                 New_N : out Number; Res : out Div_Res) is
    N0 : Natural;
  begin
    N0 := N rem 3;
    if N0 = 0 then
      Res := Zero;
      New_N := N / 3;
    elsif N0 = 1 then
      Res := Minus_1;
      New_N := (N-1) / 3;
    elsif N0 = 2 then
      Res := Plus_1;
      New_N := (N+1) / 3;
    else
      raise Program_Error;
    end if;
    return;
  end Div;

  Get_Error : exception;
  function Get_Char return Character is
    Char : Character;
  begin
    Char := Key_Pressed.Get_Key (True);
    if Char = Key_Pressed.Error_Key then
      raise Get_Error;
    else
      return Char;
    end if;
  end Get_Char;

  function Handle_Arrow (Char : in Character) return Boolean is
    Lchar : Character;
  begin
    Lchar := Char;
    if Lchar = Aski.Esc then
      Lchar := Get_Char;
      if Lchar = '[' then
        Lchar := Get_Char;
        if Lchar = 'D' then
          Lchar := '4';
        elsif Lchar = 'C'then
          Lchar := '6';
        elsif Lchar = 'B'then
          Lchar := '5';
        else
          Lchar := ' ';
        end if;
      end if;
    end if;

    if Lchar = '4' then
      Got_Res := Minus_1;
      return True;
    elsif Lchar = '6' then
      Got_Res := Plus_1;
      return True;
    elsif Lchar = '5' then
      Got_Res := Zero;
      return True;
    end if;
    return False;
  end Handle_Arrow;

begin
  Rnd.Gen.Randomize;
  Key_Pressed.Open;

  Game:
  loop
    Num := Rand;
    Success := True;
    Basic_Proc.Put_Output ("   ");
    Start_Time := Ada.Calendar.Clock;

    Party:
    loop

      Get:
      loop
        Basic_Proc.Put_Output ("--> ");
        Basic_Proc.Put_Output (Normal (Num, 10));
        exit Party when Num = 0;
        Basic_Proc.Put_Output (
         "  '<-' -1   'V' 0   '->' +1   'q' quit ? ");

        declare
          Char : Character;
        begin
          Char := Get_Char;
          Basic_Proc.New_Line_Output;
          if Char = 'q' or else Char = 'Q' then
            Clear_Screen;
            exit Game;
          end if;
          -- Done when valid arrow
          exit Get when Handle_Arrow (Char);
        end;
        Basic_Proc.Put_Output ("ERR");
      end loop Get;

      Div (Num, Num, Res);
      if Res /= Got_Res then
        Success := False;
        case Res is
          when Zero =>
            Basic_Proc.Put_Line_Output (" Error, it was  0");
          when Plus_1 =>
            Basic_Proc.Put_Line_Output (" Error, it was +1");
          when Minus_1 =>
            Basic_Proc.Put_Line_Output (" Error, it was -1");
        end case;
      end if;
      case Res is
        when Zero =>
          Basic_Proc.Put_Output (" 0 ");
        when Plus_1 =>
          Basic_Proc.Put_Output ("+1 ");
        when Minus_1 =>
          Basic_Proc.Put_Output ("-1 ");
      end case;
    end loop Party;

    Basic_Proc.Put_Output ("   ");
    if Success then
      Time_Spent := Positive (Ada.Calendar."-" (Ada.Calendar.Clock,
                                                Start_Time) );
      Basic_Proc.Put_Line_Output (" Perfect, in" & Time_Spent'Img
                    & " seconds!");
    else
      Basic_Proc.Put_Line_Output (" Some errors...");
    end if;
    Basic_Proc.New_Line_Output;
  end loop Game;

  Key_Pressed.Close;
exception
  when others =>
    Key_Pressed.Close;
end G;

