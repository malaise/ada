package C_Types is

  -- Basic types
  type Bool is new Boolean;
  for Bool'Size use 32;
  for Bool use (False => 0, True => 1);

  subtype Long is Integer;


  -- Other types
  subtype Time_T is Long;
  subtype Suseconds_T is Long;
  type Timeval_T is record
    Tv_Sec : Time_T;
    Tv_Usec : Suseconds_T;
  end record;
  subtype Size_T is Long;

end C_Types;


