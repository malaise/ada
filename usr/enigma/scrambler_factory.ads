with Types;
package Scrambler_Factory is

  -----------------
  -- A scrambler --
  -----------------
  type Scrambler_Type is tagged private;
  subtype Switch_Type is Scrambler_Type;

  -- Create a new scrambler
  function Create return Scrambler_Type;

  -- Set an association
  procedure Set (Scrambler : in out Scrambler_Type;
                 Association : in Types.Lid_Pair_T);

  -- Encode a letter
  function Encode (Scrambler : in Scrambler_Type;
                   X : Types.Lid) return Types.Lid;

  -- Decode a letter
  function Decode (Scrambler : in Scrambler_Type;
                   X : Types.Lid) return Types.Lid;

  -------------
  -- A plate -- (scrambler with offset)
  -------------
  type Plate_Type is new Scrambler_Type with private;
  subtype Back_Type is Plate_Type;

  -- Create a new plate
  function Create return Plate_Type;

  -- Set the offset
  procedure Set_Offset (Plate : in out Plate_Type;
                        Offset : Types.Lid);

  -- Encode a letter
  function Encode (Plate : in Plate_Type;
                   X : Types.Lid) return Types.Lid;

  -- Decode a letter
  function Decode (Plate : in Plate_Type;
                   X : Types.Lid) return Types.Lid;

  --------------
  -- A jammer -- (plate with increment)
  --------------
  type Jammer_Type is new Plate_Type with private;

  -- Create a new jammer
  function Create return Jammer_Type;

  -- Increment the jammer
  -- Set Carry to True each 26 increments since creation
  procedure Increment (Jammer : in out Jammer_Type;
                       Carry : out Boolean);

  -- Encode a letter
  function Encode (Jammer : in Jammer_Type;
                   X : Types.Lid) return Types.Lid;

  -- Decode a letter
  function Decode (Jammer : in Jammer_Type;
                   X : Types.Lid) return Types.Lid;

private
  type Translation_Array is array (Types.Lid) of Types.Lid;

  type Scrambler_Type is tagged record
    Encoding : Translation_Array;
    Decoding : Translation_Array;
  end record;

  type Plate_Type is new Scrambler_Type with record
    Init_Offset : Types.Lid;
  end record;

  type Jammer_Type is new Plate_Type with record
    Global_Offset : Types.Lid;
    Increment : Types.Lid;
  end record;

end Scrambler_Factory;

