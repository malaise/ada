with Definition;
package body Coder is

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
    Definition.Read_Definition (Machine);
    -- Optim: Store if last rotor has no carry (=> it does not turn)
    Last_Fixed := Machine.Rotors(Machine.Nb_Rotors).Carries = No_Carry;
    -- Optim: Store reverted rotors
    for I in 1 .. Machine.Nb_Rotors loop
      Reverted(I) := Machine.Rotors(I).Scrambler.Revert;
    end loop;
  end Init;

  -- Move the rotors
  procedure Move is
    Next_Moves, Curr_Moves : Boolean;
    use type Types.Lid;
  begin
    -- Next_Moves is set if current carry is set (so next rotor moves)
    -- First rotor always moves
    Next_Moves := True;
    for I in 1 .. Machine.Nb_Rotors loop
      -- Does current rotor move
      Curr_Moves := Next_Moves;
      -- Will next rotor move
      Next_Moves := I /= Machine.Nb_Rotors
         and then Machine.Rotors(I).Carries(Machine.Rotors(I).Offset
                                            + Machine.Rotors(I).Position);
      -- Move current rotor either because previous rotor has the carry
      --  or because current rotor has the carry and moves next rotor
      -- This causes the "doubelstepping"
      -- But last rotor may be fixed anyway
      if (Curr_Moves or else Next_Moves)
      and then (I /= Machine.Nb_Rotors or else not Last_Fixed) then
        Machine.Rotors(I).Position := Machine.Rotors(I).Position + 1;
      end if;
    end loop;
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
    -- Encode through the switches
    X := Machine.Switches.Translate (X);
    -- Encode through the rotors
    for I in 1 .. Machine.Nb_Rotors loop
      X := X + Machine.Rotors(I).Offset + Machine.Rotors(I).Position;
      X := Machine.Rotors(I).Scrambler.Translate (X);
    end loop;
    -- Encode through the reflector
    X := X + Machine.Reflector.Position;
    X := Machine.Reflector.Scrambler.Translate (X);
    -- Encode backwards through the rotors
    for I in 1 .. Machine.Nb_Rotors loop
      X := Reverted(I).Translate (X);
      X := X - Machine.Rotors(I).Offset - Machine.Rotors(I).Position;
    end loop;
    -- Encode through the switches
    X := Machine.Switches.Translate (X);

    -- Done
    return Types.Letter_Of (X);
  end Encode;

end Coder;

