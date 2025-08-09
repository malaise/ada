with Ada.Calendar;
with Afpx;
with Pers_Def, Mesu_Def, Mesu_Nam;
package Str_Mng is

  -- Parse spaces from a string
  --  remove heading spaces
  --  remove multi spaces between words
  --  add spaces at the end
  procedure Parse (Str : in out String);

  -- True if the string is only spaces
  function Is_Spaces (Str : String) return Boolean;

  -- Has the str some spaces
  function Has_Spaces (Str : String) return Boolean;

  -- True if a parsed string has spaces in the middle
  function Has_Holes (Str : String) return Boolean;

  subtype Sampling_Str is String (1 .. 3);
  -- 0 <-> spaces
  -- others <-> value
  function To_Str (Sampling : Pers_Def.Sampling_Delta_Range)
           return Sampling_Str;
  function To_Sampling (Str : Sampling_Str)
           return Pers_Def.Sampling_Delta_Range;

  subtype Bpm_Str is String (1 .. 3);
  -- 0 <-> spaces
  -- others <-> value
  function To_Str (Bpm : Pers_Def.Bpm_Range) return Bpm_Str;
  function To_Bpm (Str : Bpm_Str) return Pers_Def.Bpm_Range;

  function Pid_Str (Pid : Pers_Def.Pid_Range) return Mesu_Nam.File_Pid_Str;

  -- Times and dates
  subtype Str2 is String (1 .. 2);
  subtype Str4 is String (1 .. 4);

  type Date_Str_Rec is record
    Day   : Str2 := (others => ' ');
    Month : Str2 := (others => ' ');
    Year  : Str4 := (others => ' ');
  end record;

  -- Time is from 0000 to 2359
  -- Set to "0000" by default
  procedure Check_Time (Input : in Str4;
                        Output : out Str4;
                        Valid : out Boolean);

  -- An input date can be before or after
  -- Check its validity and build date YYyyNnDd
  procedure Check_Date (Input  : in Date_Str_Rec;
                        After  : in Boolean;
                        Output : out Mesu_Def.Date_Str;
                        Valid  : out Boolean);

  -- Build a rec
  procedure To_Rec (Date : in Mesu_Def.Date_Str;
                    Rec  : out Date_Str_Rec);

  -- A printed date is Dd/Mm/YYyy
  subtype Printed_Date_Str is String (1 .. 10);
  function To_Printed_Date_Str (Date : Mesu_Def.Date_Str)
           return Printed_Date_Str;
  function To_Date_Str (Printed_Date : Printed_Date_Str)
           return Mesu_Def.Date_Str;

  -- A printed time is Hh:Mm/Mm/YYyy
  subtype Printed_Time_Str is String (1 .. 5);
  function To_Printed_Time_Str (Time : Mesu_Def.Time_Str)
           return Printed_Time_Str;
  function To_Time_Str (Printed_Time : Printed_Time_Str)
           return Mesu_Def.Time_Str;

  -- Current_date - nb month
  subtype Offset_Range is Natural range 0 .. Ada.Calendar.Month_Number'Last;

  function Current_Date (Offset : Offset_Range := 0) return Mesu_Def.Date_Str;
  function Current_Date_Printed (Offset : Offset_Range := 0)
  return Printed_Date_Str;
  procedure Current_Date_Rec (Date_Rec : out Date_Str_Rec;
                              Offset   : in Offset_Range := 0);


  -- From a person rec to person in list
  procedure Format_Person_To_List (Person    : in Pers_Def.Person_Rec;
                                   List_Pers : out Afpx.Line_Rec);
  procedure Format_List_To_Person (List_Pers : in Afpx.Line_Rec;
                                   Person    : out Pers_Def.Person_Rec);

  -- From a mesure rec to person in list
  procedure Format_Mesure_To_List (Person    : in Pers_Def.Person_Rec;
                                   Mesure    : in Mesu_Def.Mesure_Rec;
                                   List_Mesu : out Afpx.Line_Rec);
  procedure Format_List_To_Mesure (List_Mesu : in Afpx.Line_Rec;
                                   File_Name : out Mesu_Nam.File_Name_Str);

end Str_Mng;
