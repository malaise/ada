-- Parses configuration file and command line and returns enigma definition
with Types;
package Definition is

  -- Load and check the configuration (definition of rotors and reflectors)
  -- Parse arguments and check definition of switches, rotors and reflector
  -- Logs problem on stdout
  procedure Load_Configuration;
  Invalid_Configuration : exception;
  Invalid_Definition : exception;


  -- Generic definition of the associations of letters
  -- The first of each pair must be unique,
  -- The second of each pair must be unique
  type Scrambler_Type is tagged private;

  -- Translate a letter through a scrambler
  function Translate (Scrambler : Scrambler_Type; A_Letter : Types.Lid)
           return Types.Lid;

  -- Return the revert scrambler
  function Revert (Scrambler : Scrambler_Type) return Scrambler_Type;

  -- Initial definition of the machine
  -- Possible carries
  type Carries_Array is array (Types.Lid) of Boolean;

  -- The definition of a rotor: scrambler + carries + offset + position
  type Rotor_Def_Rec is record
    Scrambler : Scrambler_Type;
    Carries   : Carries_Array  := (others => False);
    Offset : Types.Lid := 0;
    Position : Types.Lid := 0;
  end record;

  -- The possible number of rotors
  subtype Rotors_Nb_Range is Natural range 0 .. 4;
  subtype Rotors_Id_Range is Positive range 1 .. Rotors_Nb_Range'Last;
  type Rotors_Array is array (Rotors_Id_Range range <>) of Rotor_Def_Rec;

  -- The definition of the reflector: Scrambler (symetric) + position
  type Reflector_Def_Rec is record
    Scrambler : Scrambler_Type;
    Position : Types.Lid := 0;
  end record;

  type Definition_Rec (Nb_Rotors : Rotors_Nb_Range := 0) is record
    -- The (optional) switches
    Switches : Scrambler_Type;
    -- The (optional) Rotors
    Rotors : Rotors_Array(1 .. Nb_Rotors);
    -- The mandatory Reflector
    Reflector : Reflector_Def_Rec;
  end record;

  -- Parse arguments and fill Def
  procedure Read_Definition (Def : out Definition_Rec);

  -- Get initial start byte offset (1 by default)
  function Read_Start_Byte return Positive;

  -- Get last byte offset (0 if none)
  function Read_Last_Byte return Natural;

private

  -- The E -> D mapping
  type Mapping_Array is array (Types.Lid) of Types.Lid;
  type Scrambler_Type is tagged record
    Mapping : Mapping_Array := (others => 0);
  end record;

  -- Initialize a scrambler
  procedure Set (Scrambler : out Scrambler_Type; To : in String);

  Default_Scrambler : constant Scrambler_Type := (Mapping => (others => 0));

end Definition;

