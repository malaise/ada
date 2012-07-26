package C_Types is

  -- Basic types
  type Bool is new Boolean;
  for Bool'Size use 32;
  for Bool use (False => 0, True => 1);

  subtype Char is Character;

  type Short is new Integer range -(2 ** 15) .. 2 ** 15 - 1;
  for Short'Size use 16;

  subtype Int is Integer;

  subtype Long is Long_Integer;
  subtype Long64 is Long_Long_Integer;

  type  Double is new Long_Float;

  -- Unsigned types
  type Uint8 is new Integer range 0 .. 2 ** 8 - 1;
  for Uint8'Size use 8;

  type Uint16 is new Integer range 0 .. 2 ** 16 - 1;
  for Uint16'Size use 16;

  type Uint32 is new Long_Long_Integer range 0 .. 2 ** 32 - 1;
  for Uint32'Size use 32;

  -- Other types
  subtype Time_T is Long;
  subtype Suseconds_T is Long;
  type Timeval_T is record
    Tv_Sec : Time_T;
    Tv_Usec : Suseconds_T;
  end record;

  subtype Size_T is Long;
  subtype Off_T is Long64;

  subtype Mode_T is Uint32;

  subtype Uid_T is Uint32;

  subtype Pid_T is Int;

end C_Types;

