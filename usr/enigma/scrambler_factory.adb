package body Scrambler_Factory is

  -- Create a new scrambler
  -- Initializes En/Decoding to identity;
  function Create return Scrambler_Type is
    Scrambler : Scrambler_Type;
  begin
    for I in Types.Lid'Range loop
      Scrambler.Encoding (I) := I;
      Scrambler.Decoding (I) := I;
    end loop;
    return Scrambler;
  end Create;

  -- Set an association
  procedure Set (Scrambler : in out Scrambler_Type;
                 Association : in Types.Lid_Pair_T) is
    -- New and old ids
    New_E, New_D, Old_E, Old_D : Types.Lid;
  begin
    -- Set ids
    New_E := Association.E;
    New_D := Association.D;
    Old_E := Scrambler.Encoding (New_E);
    Old_D := Scrambler.Decoding (New_D);
    -- Set new connection
    Scrambler.Encoding (New_E) := New_D;
    Scrambler.Decoding (New_D) := New_E;
    -- Reconnect isolated slots
    Scrambler.Encoding (Old_D) := Old_E;
    Scrambler.Decoding (Old_E) := Old_D;
  end Set;

  -- Encode a letter
  function Encode (Scrambler : in Scrambler_Type;
                   X : Types.Lid) return Types.Lid is
  begin
    return Scrambler.Encoding (X);
  end Encode;

  -- Decode a letter
  function Decode (Scrambler : in Scrambler_Type;
                   X : Types.Lid) return Types.Lid is
  begin
    return Scrambler.Decoding (X);
  end Decode;

  ------------------------------------------------------------
  use type Types.Lid;

  -- Create a new plate
  function Create return Plate_Type is
    Scrambler : Scrambler_Type := Create;
  begin
    return (Scrambler with Init_Offset => 0);
  end Create;

  -- Set the offset
  procedure Set_Offset (Plate : in out Plate_Type;
                        Offset : in Types.Lid) is
  begin
    Plate.Init_Offset := Offset;
  end Set_Offset;

  -- Encode a letter
  function Encode (Plate : in Plate_Type;
                   X : Types.Lid) return Types.Lid is
  begin
    return Encode (Scrambler_Type(Plate), X + Plate.Init_Offset);
  end Encode;

  -- Decode a letter
  function Decode (Plate : in Plate_Type;
                   X : Types.Lid) return Types.Lid is
  begin
    return Decode (Scrambler_Type(Plate), X - Plate.Init_Offset);
  end Decode;

  ------------------------------------------------------------

  -- Create a new jammer
  function Create return Jammer_Type is
    Plate : Plate_Type := Create;
  begin
    -- Init the global offset with the plate initial offset
    return (Plate with Global_Offset => Plate.Init_Offset,
                       Increment     => 0);
  end Create;
  
  -- Increment the jammer
  -- Set Carry to True each 26 increments since creation
  procedure Increment (Jammer : in out Jammer_Type;
                       Carry : out Boolean) is
  begin
    Jammer.Global_Offset := Jammer.Global_Offset + 1;
    Jammer.Increment := Jammer.Increment + 1;
    Carry := Jammer.Increment = 0;
  end Increment;

  -- Encode a letter
  function Encode (Jammer : in Jammer_Type;
                   X : Types.Lid) return Types.Lid is
  begin
    return Encode (Scrambler_Type(Jammer), X + Jammer.Global_Offset);
  end Encode;


  -- Decode a letter
  function Decode (Jammer : in Jammer_Type;
                   X : Types.Lid) return Types.Lid is
  begin
    return Decode (Scrambler_Type(Jammer), X - Jammer.Global_Offset);
  end Decode;


end Scrambler_Factory;

