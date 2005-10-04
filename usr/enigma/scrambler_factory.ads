with Types, Definition;
package Scrambler_Factory is

  -- Init from conf
  procedure Init;

  -----------------
  -- A scrambler --
  -----------------
  type Scrambler_Type is private;
  -- A switch is a scrambler
  subtype Switch_Type is Scrambler_Type;

  -- Create a new scrambler
  function Create return Scrambler_Type;

  -- Set an association
  procedure Set (Scrambler : in out Scrambler_Type;
                 Association : in Types.Lid_Pair_T);

  -- Encode a letter
  function Encode (Scrambler : Scrambler_Type;
                   X : Types.Lid) return Types.Lid;

  -- Decode a letter
  function Decode (Scrambler : Scrambler_Type;
                   X : Types.Lid) return Types.Lid;

  -------------
  -- A back  -- (plate)
  -------------
  type Back_Type is private;

  -- Get the back
  function Get (Scambler_Id : Definition.Scrambler_Index) return Back_Type;

  -- Set the offset
  procedure Set_Offset (Back : in out Back_Type;
                        Offset : Types.Lid);

  -- Encode a letter
  function Encode (Back : Back_Type;
                   X : Types.Lid) return Types.Lid;

  --------------
  -- A jammer --
  --------------
  type Jammer_Type is private;

  -- Get a jammer
  function Get (Scambler_Id : Definition.Scrambler_Index) return Jammer_Type;

  -- Set the offset
  procedure Set_Offset (Jammer : in out Jammer_Type;
                        Offset : Types.Lid);

  -- Increment the jammer
  -- Set Carry to True each 26 increments since creation
  procedure Increment (Jammer : in out Jammer_Type;
                       Carry : out Boolean);

  -- Encode a letter
  function Encode (Jammer : Jammer_Type;
                   X : Types.Lid) return Types.Lid;

  -- Decode a letter
  function Decode (Jammer : Jammer_Type;
                   X : Types.Lid) return Types.Lid;

  ----------------
  -- Exceptions --
  ----------------
  -- Error loading and parsing conf
  Config_Error : exception;

  -- Getting the back twice
  Getting_Back_Twice : exception;

  -- The requested scrambler for back is not symetrical
  Asymetric_Back : exception;

  -- Error using a scrambler not defined in conf
  Unknown_Scrambler : exception;

  -- Error using twice the same scrambler
  Scrambler_In_Use : exception;

private
  type Translation_Array is array (Types.Lid) of Types.Lid;

  type Scrambler_Type is record
    Encoding : Translation_Array;
    Decoding : Translation_Array;
  end record;

  type Back_Type is record
    Scrambler : Scrambler_Type := Create;
    Init_Offset : Types.Lid := Types.Lid'First;
  end record;

  type Jammer_Type is record
    Scrambler : Scrambler_Type := Create;
    Global_Offset : Types.Lid := Types.Lid'First;
    Increment : Types.Lid := Types.Lid'First;
  end record;

end Scrambler_Factory;

