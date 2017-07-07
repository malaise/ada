with Images, As.U, Trace.Loggers;
with Definition;
package body Coder is

  function Limage (L : Types.Lid) return String is
    (Images.Integer_Image (Natural (L)));
  function Image (L : Types.Lid) return String is
    (Limage (L) & "=" & Types.Letter_Of (L));

  -- Debug
  Logger : Trace.Loggers.Logger;
  Text : As.U.Asu_Us;
  procedure Put (Str : in String) is
  begin
    if Logger.Debug_On then
      Text.Append (Str);
    end if;
  end Put;

  procedure Putl (Str : in String) is
  begin
    Logger.Log_Debug (Text.Image & Str);
    Text.Set_Null;
  end Putl;

  -- The machine
  Machine : Definition.Definition_Rec;

  -- These are local optimizations, constants
  -- Is last rotor fixed
  Last_Fixed : Boolean;
  -- Reverted scramblers of rotors
  Reverted : array (Definition.Rotors_Id_Range) of Definition.Scrambler_Type;

  -- Init machine from arguments and config files
  procedure Init is
    No_Carry : constant Definition.Carries_Array := (others => False);
    use type Definition.Carries_Array;
  begin
    -- Init debug
    Logger.Init;
    -- Init definition
    Definition.Read_Definition (Machine);
    -- Optim: Store if last rotor has no carry (=> it does not turn)
    Last_Fixed := Machine.Nb_Rotors >= 1
                  and then Machine.Rotors(Machine.Nb_Rotors).Carries = No_Carry;
    -- Optim: Store reverted rotors
    for I in 1 .. Machine.Nb_Rotors loop
      Reverted(I) := Machine.Rotors(I).Scrambler.Revert;
    end loop;
  end Init;

  -- Kind of movement: No movement, simple rotation,
  --  rotation with carry on initial pos
  type State_List is (Steady, Move, Move_Carry);
  -- Move the rotors
  procedure Move is
    Prev_State, Curr_State : State_List;
    Curr_Carry : Boolean;
    use type Types.Lid;
  begin
    -- First rotor always moves, simulate it as if previous moved with carry
    Prev_State := Move_Carry;
    Put ("Moving:" );
    for I in 1 .. Machine.Nb_Rotors loop
      -- Is initial position with the carry
      Curr_Carry := Machine.Rotors(I).Carries(Machine.Rotors(I).Position);
      -- Move current rotor either because previous rotor moved and had carry
      -- or because current rotor has the carry and next rotor can move
      -- This causes the "doublestepping"
      -- But last rotor may be fixed anyway
      if I = Machine.Nb_Rotors and then Last_Fixed then
        -- (Last) rotor with no carry does not turn, except if alone
        Curr_State := Steady;
      elsif Prev_State = Move_Carry then
        Curr_State := Move;
      elsif Prev_State = Move
            and then Curr_Carry
            and then (I /= Machine.Nb_Rotors - 1 or else not Last_Fixed) then
        Curr_State := Move;
      else
        Curr_State := Steady;
      end if;
      if Curr_State = Move then
        Curr_State := (if Curr_Carry then Move_Carry else Move);
        Machine.Rotors(I).Position := Machine.Rotors(I).Position + 1;
        Put (I'Img);
      end if;
      Prev_State := Curr_State;
    end loop;
    Putl("");

    -- Dump current rotors
    Put ("Rotors: ");
    for I in reverse 1 .. Machine.Nb_Rotors loop
      Put (Image (Machine.Rotors(I).Position) & " ");
    end loop;
    Putl("");
  end Move;

  -- Encode a letter
  function Encode (L : Types.Letter) return Types.Letter is
    X : Types.Lid;
    use type Types.Lid;
  begin
    -- Move the rotors
    Move;
    -- Init Lid
    X := Types.Id_Of (L);
    Put ("Encoding " & Image (X) & ": ");
    -- Encode through the switches
    X := Machine.Switches.Translate (X);
    Put ("S->" & Image (X) & ", ");
    -- Encode through the rotors
    for I in 1 .. Machine.Nb_Rotors loop
      X := X - Machine.Rotors(I).Offset + Machine.Rotors(I).Position;
      X := Machine.Rotors(I).Scrambler.Translate (X)
         + Machine.Rotors(I).Offset - Machine.Rotors(I).Position;
      Put ("R" & Images.Integer_Image (I) & "->" & Image (X) & ", ");
    end loop;
    -- Encode through the reflector
    X := X + Machine.Reflector.Position;
    X := Machine.Reflector.Scrambler.Translate (X);
    X := X - Machine.Reflector.Position;
    Put ("F->" & Image (X) & ", ");
    -- Encode backwards through the rotors
    for I in reverse 1 .. Machine.Nb_Rotors loop
      X := X - Machine.Rotors(I).Offset + Machine.Rotors(I).Position;
      X := Reverted(I).Translate (X)
         + Machine.Rotors(I).Offset - Machine.Rotors(I).Position;
      Put ("R" & Images.Integer_Image (I) & "->" & Image (X) & ", ");
    end loop;
    -- Encode through the switches
    X := Machine.Switches.Translate (X);
    Putl ("S->" & Image (X));

    -- Done
    return Types.Letter_Of (X);
  end Encode;

end Coder;

