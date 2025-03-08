with Dynamic_List;
package Pers_Def is

  -- Name and activity of a person
  subtype Person_Name_Str is String (1 .. 20);
  subtype Person_Activity_Str is String (1 .. 10);

  -- Unique Id of a person.
  type Pid_Range is new Natural range 000 .. 999;

  -- Heart rate. 0 in person time zones if not set
  type Bpm_Range is new Natural range 0 .. 250;
  subtype Set_Bpm_Range is Bpm_Range range
   Bpm_Range'Succ(Bpm_Range'First) .. Bpm_Range'Last;

  -- Delta in seconds between 2 samplings
  type Sampling_Delta_Range is new Natural range 0 .. 120;
  Default_Sampling_Delta : constant Sampling_Delta_Range := 120;

  -- 6 time zones for a person
  type Person_Tz_Array is array (1 .. 6) of Bpm_Range;

  -- A person
  type Person_Rec is record
    Name : Person_Name_Str := (others => ' ');
    Activity : Person_Activity_Str := (others => ' ');
    Pid : Pid_Range := Pid_Range'First;
    Tz : Person_Tz_Array := (others => Bpm_Range'First);
    Sampling_Delta : Sampling_Delta_Range := Default_Sampling_Delta;
  end record;

  -- A list of person (all set)
  package Person_Dyn_List_Mng is new Dynamic_List (Element_Type => Person_Rec);
  package Person_List_Mng renames Person_Dyn_List_Mng.Dyn_List;
  subtype Person_List is Person_List_Mng.List_Type;
  The_Persons : Person_List;

  -- Exception on Ctrl-C (and window manager closing request)
  Exit_Requested : exception;

end Pers_Def;

