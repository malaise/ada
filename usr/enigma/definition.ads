-- Parses command line and returns enigma definition
with Types;
package Definition is

  -- A switch definition
  type Switch_Range is new Natural range 0 .. 26;
  subtype Switch_Index is Switch_Range range 1 .. Switch_Range'Last;
  type Letter_Pair_Array is array (Switch_Range range <>)
                                  of Types.Letter_Pair_T;

  type Switch_Definition (Nb_Switches : Switch_Range := 0) is record
    Switch : Letter_Pair_Array (1 .. Nb_Switches);
  end record;

  -- A scrambler is defined by an index (1 .. 9) and an offset
  type Scrambler_Range is new Natural range 0 .. 9;
  subtype Scrambler_Index is Scrambler_Range range 1 .. Scrambler_Range'Last;
  type Scrambler_Definition is record
    Scrambler : Scrambler_Index; -- The selected scrambler
    Offset    : Types.Letter;    -- The offset of this scrambler;
  end record;

  -- Array of 0 to 8 jammers
  subtype Jammers_Range is Scrambler_Range range 0 .. Scrambler_Range'Last - 1;
  subtype Jammers_Index is Jammers_Range range 1 .. Jammers_Range'Last;
  type Jammers_Array is array (Jammers_Index range <>) of Scrambler_Definition;
  type Jammers_Definition (Nb_Jammers : Jammers_Range := 0) is record
    Jammers      : Jammers_Array (1 .. Nb_Jammers);
  end record;

  -- The complete definition
  type Def_Rec is record
    Switch  : Switch_Definition;
    Jammers : Jammers_Definition;
    Back    : Scrambler_Definition;
  end record;

  -- Exception if definition is not correct
  Invalid_Definition : exception;

  -- Parse args and fill Def
  procedure Read_Definition (Def : out Def_Rec);
 
  -- Get initial start byte offset
  function Read_Start_Byte return Positive;

end Definition;

