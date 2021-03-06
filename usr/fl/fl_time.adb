package body Fl_Time is

  Max_Min : constant Integer := Integer (Minutes_Range'Last) + 1;

  function Abs_Time (Val : Time_Type) return Time_Type is
    ((Positiv => True,
      Hours   => Val.Hours,
      Minutes => Val.Minutes));

  function "-" (Val : Time_Type) return Time_Type is
    ((Positiv => not Val.Positiv,
      Hours   => Val.Hours,
      Minutes => Val.Minutes));

  function "<" (Left, Right : Time_Type) return Boolean is
    (if Left.Positiv = Right.Positiv then
       -- Same sign
      (if Left.Positiv then Left.Hours < Right.Hours or else
         (Left.Hours = Right.Hours and then Left.Minutes < Right.Minutes)
       else Left.Hours > Right.Hours or else
         (Left.Hours = Right.Hours and then Left.Minutes > Right.Minutes))
    else Right.Positiv);

  function "+" (Left, Right : Time_Type) return Time_Type is
    Result : Time_Type;
  begin
    if Left.Positiv = Right.Positiv then
      Result.Minutes := Minutes_Range (
       (Integer (Left.Minutes) + Integer (Right.Minutes)) mod Max_Min);
      Result.Hours := Hours_Range (
       (Integer (Left.Minutes) + Integer (Right.Minutes)) / Max_Min);
      Result.Hours := Result.Hours + Left.Hours + Right.Hours;
      Result.Positiv := Left.Positiv;
      return Result;
    elsif Left.Positiv then
      return Left - Abs_Time(Right);
    else
      return - (Abs_Time(Left) - Right);
    end if;
  exception
    when Constraint_Error =>
      raise Time_Overflow;
  end "+";

  function "-" (Left, Right : Time_Type) return Time_Type is
    Result : Time_Type;
  begin
    if Left.Positiv = Right.Positiv then
      if not (Abs_Time(Left) < Abs_Time(Right)) then --## rule line off Logical_Not

        if Left.Minutes >= Right.Minutes then
          Result.Minutes := Left.Minutes - Right.Minutes;
          Result.Hours := Left.Hours - Right.Hours;
        else
          Result.Minutes := Minutes_Range (
             Max_Min
           + Integer (Left.Minutes)
           - Integer (Right.Minutes));
          Result.Hours := Left.Hours - Right.Hours - 1;
        end if;
        Result.Positiv := Left.Positiv;
        return Result;
      else
        return - (Right - Left);
      end if;
    else
      return Left + (- Right);
    end if;
  exception
    when Constraint_Error =>
      raise Time_Overflow;
  end "-";

end Fl_Time;

