with Ada.Calendar;
with As.U, Long_Longs, Timers, Queues, Reg_Exp;
package Rules is

  -- Circular list to store history
  package Hist_Mng is new Queues.Circ (As.U.Asu_Us);

  -- Tail length
  subtype Tail_Length is Long_Longs.Ll_Natural;

  -- A rule
  type Pattern_Access is access all Reg_Exp.Compiled_Pattern;
  type History_Access is access Hist_Mng.Circ_Type;
  type Time_Access is access Ada.Calendar.Time;
  type Natural_Access is access Natural;
  type Asu_Access is access As.U.Asu_Us;
  type Rule_Rec is record
    File : As.U.Asu_Us;
    Period : Timers.Period_Range;
    Tail : Tail_Length;
    History : History_Access;
    Action : As.U.Asu_Us;
    Aging : Duration;
    Time_Format : As.U.Asu_Us;
    Pattern : Pattern_Access;
    Latency : Duration;
    -- Internal data for handling the check of a rule
    Previous : Time_Access;
    Nb_Match : Natural_Access;
    Matches : Asu_Access;
  end record;

  -- Store a rule
  procedure Store (Rule : Rule_Rec);

  -- Get the number of stored rules
  function Get_Number return Long_Longs.Ll_Natural;

  -- Retrieve a rule
  No_Rule : exception;
  function Get_Rule (Number : in Long_Longs.Ll_Positive) return Rule_Rec;

end Rules;

