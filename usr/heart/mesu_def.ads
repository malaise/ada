with Pers_Def;
package Mesu_Def is

  -- Date in string YyyyMmDd
  subtype Date_Str is String (1 .. 8);

  -- Delta in seconds between 2 samplings
  type Sampling_Delta_Range is new Positive range 1 .. 120;

  -- Comment for a mesure
  subtype Comment_Str is String (1 .. 20);

  -- Number of samplings
  subtype Sample_Nb_Range is Positive range 1 .. 100;

  -- Values
  type Sample_Array is array (Sample_Nb_Range range <>) of
   Pers_Def.Bpm_Range;
  subtype Max_Sample_Array is Sample_Array (Sample_Nb_Range);

  -- A mesure
  type Mesure_Rec is record
    Pid : Pers_Def.Pid_Range;
    -- YYYYMMDD
    Date : Date_Str := (others => ' ');
    Sampling_Delta : Sampling_Delta_Range := 120;
    Comment : Comment_Str := (others => ' ');
    -- Time zones for the mesure
    Tz : Pers_Def.Person_Tz_Array := (others => Pers_Def.Bpm_Range'First);
    Samples : Max_Sample_Array := (others => Pers_Def.Bpm_Range'First);
  end record;

end Mesu_Def;
