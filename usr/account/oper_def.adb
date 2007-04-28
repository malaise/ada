with Language;
package body Oper_Def is

  function Current_Date return Date_Rec is
    Date : Date_Rec;
    Secs : Ada.Calendar.Day_Duration;
  begin
    Ada.Calendar.Split(Ada.Calendar.Clock, Date.Year, Date.Month, Date.Day, Secs);
    return Date;
  end Current_Date;

  -- Sort by date, amount, kind, destination
  -- DO NOT SORT BY STATUS, or a double click on sub list will
  --  mess all up.
  function Before (Oper_1, Oper_2 : Oper_Rec) return Boolean is
  begin
    if Oper_1.Date.Year /= Oper_2.Date.Year then
      return Oper_1.Date.Year < Oper_2.Date.Year;
    elsif Oper_1.Date.Month /= Oper_2.Date.Month then
      return Oper_1.Date.Month < Oper_2.Date.Month;
    elsif Oper_1.Date.Day /= Oper_2.Date.Day then
      return Oper_1.Date.Day < Oper_2.Date.Day;
    elsif Oper_1.Amount /= Oper_2.Amount then
      return Oper_1.Amount < Oper_2.Amount;
    elsif Oper_1.Kind /= Oper_2.Kind then
      return Oper_1.Kind < Oper_2.Kind;
    else
      return Oper_1.Destination < Oper_2.Destination;
    end if;
  end Before;

  -- To be adapted temporaly when importing
  procedure Convert (Read_Oper : in Read_Oper_Rec;
                     Oper      : out Oper_Rec) is
  begin
    Oper := Oper_Rec (Read_Oper);
  end Convert;

end Oper_Def;

