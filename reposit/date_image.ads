-- Return String image "YYyy/Mm/Dd Hh:Mm:Ss.mmm" of a time
-- 23 characters
with Ada.Calendar;
function Date_Image (Date : Ada.Calendar.Time) return String;

