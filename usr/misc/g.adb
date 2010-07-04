with Ada.Text_Io, Ada.Calendar, Ada.Characters.Latin_1;
with My_Io, U_Rand, Clear_Screen;
procedure G is
  -- generated number
  subtype Number is Natural range 0 .. 999_999_999;
  Num : Number;

  -- result of division of current value
  type Div_Res is (Minus_1, Zero, Plus_1);

  Got_Res, Res : Div_Res;
  Success : Boolean;

  Start_Time : Ada.Calendar.Time;
  Time_Spent : Natural;

  procedure Init is
    N : Positive;
  begin
    N := Positive (Ada.Calendar.Seconds(Ada.Calendar.Clock));
    N := (N mod (U_Rand.Seed_Range_2'Last)) + 1;
    U_Rand.Start (New_L => N);
  end Init;

  function Rand return Positive is
    subtype Digit is Natural range 0 .. 9;
    subtype R_Digit is Float range
     Float (Digit'First) .. Float (Digit'Last + 1);
    Ret : Positive;
    function Trunc (R : in R_Digit) return Digit is
      D : Natural;
    begin
      D := Natural (R);
      if Float (D) > R then
        D := D - 1;
      end if;
      return D;
    end Trunc;
    function New_Digit (Allow_0 : in Boolean) return Digit is
      R : R_Digit;
      D : Digit;
    begin
      loop
        R := U_Rand.Next * R_Digit'Last;
        D := Trunc (R);
        exit when Allow_0 or else D /= 0;
      end loop;
      return D;
    end New_Digit;

  begin
    Ret := New_Digit (Allow_0 => False);
    for I in 1 .. 8 loop
      Ret := Ret * 10 + New_Digit (Allow_0 => True);
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

begin
  -- Clear_Screen;
  Init;

  Game:
  loop
    Num := Rand;
    Success := True;
    My_Io.Put ("   ");
    Start_Time := Ada.Calendar.Clock;

    Party:
    loop

      Get:
      loop
        My_Io.Put ("--> ");
        My_Io.Put (Num, 10);
        exit Party when Num = 0;
        My_Io.Put (
         "  '<-' -1   'V' 0   '->' +1   'q' quit ? ");

        declare
          Char : Character;
        begin
          Ada.Text_Io.Get_Immediate (Char);
          Ada.Text_Io.New_Line;
          if Char = 'q' or else Char = 'Q' then
            Clear_Screen;
            return;
          end if;
          if Char = Ada.Characters.Latin_1.Esc then
            Ada.Text_Io.Get_Immediate (Char);
            if Char = '[' then
              Ada.Text_Io.Get_Immediate (Char);
              if Char = 'D' then
                Char := '4';
              elsif Char = 'C'then
                Char := '6';
              elsif Char = 'B'then
                Char := '5';
              else
                Char := ' ';
              end if;
            end if;
          end if;

          if Char = '4' then
            Got_Res := Minus_1;
            exit Get;
          elsif Char = '6' then
            Got_Res := Plus_1;
            exit Get;
          elsif Char = '5' then
            Got_Res := Zero;
            exit Get;
          end if;
        end;

        My_Io.Put ("ERR");
      end loop Get;

      Div (Num, Num, Res);
      if Res /= Got_Res then
        Success := False;
        case Res is
          when Zero =>
            My_Io.Put_Line (" Error, it was  0");
          when Plus_1 =>
            My_Io.Put_Line (" Error, it was +1");
          when Minus_1 =>
            My_Io.Put_Line (" Error, it was -1");
        end case;
      end if;
      case Res is
        when Zero =>
          My_Io.Put (" 0 ");
        when Plus_1 =>
          My_Io.Put ("+1 ");
        when Minus_1 =>
          My_Io.Put ("-1 ");
      end case;
    end loop Party;

    My_Io.Put ("   ");
    if Success then
      Time_Spent := Positive (Ada.Calendar."-" (Ada.Calendar.Clock,
                                                Start_Time) );
      My_Io.Put_Line (" Perfect, in" & Time_Spent'Img
                    & " seconds!");
    else
      My_Io.Put_Line (" Some errors...");
    end if;
    My_Io.New_Line;
  end loop Game;

end G;

