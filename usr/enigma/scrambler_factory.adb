package body Scrambler_Factory is

  -- A scrambler as it is managed internally
  type Scrambler_Cell is record
    Defined : Boolean;
    Used : Boolean;
    Scrambler : Scrambler_Type;
  end record;

  -- The scramblers to manage
  Scramblers : array (Definition.Scrambler_Index) of Scrambler_Cell;

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

  -- Get the back
  Back_Got : Boolean := False;
  function Get (Scambler_Id : Definition.Scrambler_Index) return Back_Type is
    Back : Back_Type;
    Encoding_Lid : Types.Lid;
  begin
    if Back_Got then
      raise Getting_Back_Twice;
    end if;
    if not Scramblers(Scambler_Id).Defined then
      raise Unknown_Scrambler;
    end if;
    if Scramblers(Scambler_Id).Used then
      raise Scrambler_In_Use;
    end if;
    for I in Types.Lid'Range loop
      Encoding_Lid := Scramblers(Scambler_Id).Scrambler.Encoding(I);
      if Scramblers(Scambler_Id).Scrambler.Encoding(Encoding_Lid) /= I then
        -- Encoding(Encoding(I)) /= I, not symetric
        raise Asymetric_Back;
      end if;
    end loop;
    Back := (Scrambler   => Scramblers(Scambler_Id).Scrambler,
             Init_Offset => 0);
    Scramblers(Scambler_Id).Used := True;
    Back_Got := True;
    return Back;
  end Get;

  -- Set the offset
  procedure Set_Offset (Back : in out Back_Type;
                        Offset : in Types.Lid) is
  begin
    Back.Init_Offset := Offset;
  end Set_Offset;

  -- Encode a letter
  function Encode (Back : in Back_Type;
                   X : Types.Lid) return Types.Lid is
  begin
    return Encode (Back.Scrambler, X + Back.Init_Offset)
         - Back.Init_Offset;
  end Encode;

  ------------------------------------------------------------

  -- Get a jammer
  function Get (Scambler_Id : Definition.Scrambler_Index) return Jammer_Type is
    Jammer : Jammer_Type;
  begin
    if not Scramblers(Scambler_Id).Defined then
      raise Unknown_Scrambler;
    end if;
    if Scramblers(Scambler_Id).Used then
      raise Scrambler_In_Use;
    end if;
    Jammer := (Scrambler     => Scramblers(Scambler_Id).Scrambler,
               Global_Offset => 0,
               Increment     => 0,
               Carry_Offset  => 0);
    Scramblers(Scambler_Id).Used := True;
    return Jammer;
  end Get;

  -- Set the initial value for the carry (see Increment)
  procedure Set_Carry (Jammer : in out Jammer_Type;
                       Carry_Offset : in Types.Lid) is
  begin
    Jammer.Carry_Offset := Carry_Offset;
  end Set_Carry;
  
  -- Increment the jammer
  -- Set Carry to True after Carry_Offset then after each 26 increments
  procedure Increment (Jammer : in out Jammer_Type;
                       Carry : out Boolean) is
  begin
    Jammer.Global_Offset := Jammer.Global_Offset + 1;
    Jammer.Increment := Jammer.Increment + 1;
    -- Carry when increment is modulo Carry_Offset
    Carry := Jammer.Increment = Jammer.Carry_Offset;
  end Increment;

  -- Set the offset
  procedure Set_Offset (Jammer : in out Jammer_Type;
                        Offset : Types.Lid) is
  begin
    Jammer.Global_Offset := Offset;
  end Set_Offset;

  -- Encode a letter
  function Encode (Jammer : in Jammer_Type;
                   X : Types.Lid) return Types.Lid is
  begin
    return Encode (Jammer.Scrambler, X + Jammer.Global_Offset);
  end Encode;


  -- Decode a letter
  function Decode (Jammer : in Jammer_Type;
                   X : Types.Lid) return Types.Lid is
  begin
    return Decode (Jammer.Scrambler, X) - Jammer.Global_Offset;
  end Decode;

  --------------------
  -- Initialisation --
  --------------------
  -- Init the scrambler
  procedure Init is separate;

end Scrambler_Factory;

