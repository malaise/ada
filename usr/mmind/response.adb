with Rnd;
package body Response is


  Secret : Color_Rec;

  function Color_Random is new Rnd.Discr_Random (Common.Eff_Color_Range);

  procedure New_Code is
    Current_Level : Common.Last_Level_Range := Common.Get_Level;
  begin
    Secret := (Level => Current_Level,
              Color => (others => Common.Eff_Color_Range'First) );

    for I in Common.Level_Range
     range Common.Level_Range'First .. Current_Level loop
      Secret.Color(I) := Color_Random;
    end loop;
  end New_Code;


  function Respond (Propal : Color_Rec) return Response_Rec is

    subtype Column_Range is Common.Level_Range range
     Common.Level_Range'First .. Secret.Level;
    Seen_Code, Seen_Propal : array (Column_Range) of Boolean
                         := (others => False);
    Response : Response_Rec := (Placed_Ok => 0, Colors_Ok=> 0);
    use Common;
  begin
    if Secret.Level /= Propal.Level then raise Constraint_Error; end if;

    for Col in Column_Range loop
      if Secret.Color(Col) = Propal.Color(Col) then
        Response.Placed_Ok := Response.Placed_Ok + 1;
        Seen_Code(Col) := True;
        Seen_Propal(Col) := True;
      end if;
    end loop;

    for Col_Propal in Column_Range loop
      if not Seen_Propal (Col_Propal) then
        -- not a black
        for Col_Code in Column_Range loop
          if not Seen_Code(Col_Code) and then
           Secret.Color(Col_Code) = Propal.Color(Col_Propal) then
            Response.Colors_Ok := Response.Colors_Ok + 1;
            Seen_Code(Col_Code) := True;
            Seen_Propal(Col_Propal) := True;
            exit;
          end if;
        end loop;
      end if;
    end loop;

    return Response;
  end Respond;



  function  Get_Code return Color_Rec is
  begin
    return Secret;
  end Get_Code;

end Response;
