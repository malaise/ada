with Unbounded_Arrays;
with Pers_Def;
package Mesu_Def is

  -- Date in string YyyyMmDd
  subtype Date_Str is String (1 .. 8);

  -- Time in string HhMm
  subtype Time_Str is String (1 .. 4);

  -- Comment for a mesure
  subtype Comment_Str is String (1 .. 20);

  -- Array of samples
  type Sample_Array is array (Positive range <>) of Pers_Def.Bpm_Range;
  package Sample_Array_Mng is new Unbounded_Arrays (Pers_Def.Bpm_Range,
                                                    Sample_Array);
  subtype Unb_Sample_Array is Sample_Array_Mng.Unb_Array;

  -- A mesure
  type Mesure_Rec is record
    Pid : Pers_Def.Pid_Range := Pers_Def.Pid_Range'First;
    -- YYYYMMDD
    Date : Date_Str := (others => ' ');
    -- HHMM
    Time : Time_Str := (others => '0');
    Sampling_Delta : Pers_Def.Sampling_Delta_Range
                   := Pers_Def.Default_Sampling_Delta;
    Comment : Comment_Str := (others => ' ');
    -- Time zones for the mesure
    Tz : Pers_Def.Person_Tz_Array := (others => Pers_Def.Bpm_Range'First);
    Samples : Unb_Sample_Array;
  end record;

end Mesu_Def;

