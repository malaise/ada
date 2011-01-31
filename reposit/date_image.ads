-- Return String image "YYyy/Mm/Dd Hh:Mm:Ss.mmm" of a time
-- Alternatively uses  "YYyy-Mm-DdTHh:Mm:Ss.mmm", the ISO 8601 format
-- 23 characters in both cases
with Ada.Calendar;
function Date_Image (Date : Ada.Calendar.Time;
                     Iso  : Boolean := False) return String;

