with Text_Handler, Upper_Str, Lower_Str;
separate(Mcd_Mng)

package body Strings is

  procedure Check_Chrs (X : in Item_Rec) is
  begin
    if X.Kind /= Chrs then
      raise Invalid_Argument;
    end if;
  end Check_Chrs;

  procedure Check_Inte (X : in Item_Rec) is
  begin
    if X.Kind /= Inte then
      raise Invalid_Argument;
    end if;
  end Check_Inte;

  function Strcat (S1, S2 : Item_Rec) return Item_Rec is
    Res : Item_Rec(Chrs);
  begin
    Check_Chrs(S1);
    Check_Chrs(S2);

    if S1.Val_Len + S2.Val_Len > Chars_Text'Length then
      raise String_Len;
    end if;
    Res.Val_Len := S1.Val_Len + S2.Val_Len;
    Res.Val_Text(1 .. Res.Val_Len) := S1.Val_Text(1 .. S1.Val_Len)
                                    & S2.Val_Text(1 .. S2.Val_Len);
    return Res;
  exception
    when Constraint_Error =>
      raise String_Len;
  end Strcat;

  function Strsub (S, I1, I2 : Item_Rec) return Item_Rec is
    Res : Item_Rec(Chrs);
  begin
    Check_Chrs(S);
    Check_Inte(I1);
    Check_Inte(I2);

    -- Empty string
    if I2.Val_Inte = 0 or else I2.Val_Inte < I1.Val_Inte then
      Res.Val_Len := 0;
      return Res;
    end if;


    if I1.Val_Inte < 1 or else I1.Val_Inte > My_Math.Inte(S.Val_Len) then
      raise Argument_Mismatch;
    end if;
    if I2.Val_Inte < 1 or else I2.Val_Inte > My_Math.Inte(S.Val_Len) then
      raise Argument_Mismatch;
    end if;
    Res.Val_Len := Natural(I2.Val_Inte) - Natural(I1.Val_Inte) + 1;
    Res.Val_Text(1 .. Res.Val_Len) :=
            S.Val_Text(Natural(I1.Val_Inte) .. Natural(I2.Val_Inte));
    return Res;

  end Strsub;

  function Strloc (Occ, Pat, S : Item_Rec) return Item_Rec is
    Res : Item_Rec(Inte);
  begin
    Check_Chrs(S);
    Check_Chrs(Pat);
    Check_Inte(Occ);

    if Occ.Val_Inte < 1 or else Occ.Val_Inte > My_Math.Inte(Positive'Last) then
      raise Argument_Mismatch;
    end if;

    declare
      Txt : Text_Handler.Text(Text_Handler.Max_Len_Range(S.Val_Len));
    begin
      Text_Handler.Set(Txt, S.Val_Text(1 .. S.Val_Len));
      Res.Val_Inte := My_Math.Inte(
          Text_Handler.Locate(Within    => Txt,
                              Fragment  => Pat.Val_Text(1 .. Pat.Val_Len),
                              Occurence => Positive(Occ.Val_Inte)) );
    end;
    return Res;
  end Strloc;

  function Strrep (I, Pat, S : Item_Rec) return Item_Rec is
    Res : Item_Rec(Chrs);
  begin
    Check_Chrs(S);
    Check_Chrs(Pat);
    Check_Inte(I);

    if I.Val_Inte < 1 or else I.Val_Inte > My_Math.Inte(S.Val_Len) then
      raise Argument_Mismatch;
    end if;

    declare
      Txt : Text_Handler.Text(Chars_Text'Length);
    begin
      Text_Handler.Set(Txt, S.Val_Text(1 .. S.Val_Len));
      Text_Handler.Amend(To => Txt, 
                         By => Pat.Val_Text(1 .. Pat.Val_Len),
                         Position => Text_Handler.Max_Len_Range(I.Val_Inte) );
      Res.Val_Len := Text_Handler.Length(Txt);
      Res.Val_Text(1 .. Res.Val_Len) := Text_Handler.Value(Txt);
    end;
    return Res;
  exception
    when Constraint_Error =>
      raise String_Len;
  end Strrep;

  function Strlen (S : Item_Rec) return Item_Rec is
  begin
    Check_Chrs(S);
    return (Kind => Inte, Val_Inte => My_Math.Inte(S.Val_Len));
  end Strlen;

  function Strupp (S : Item_Rec) return Item_Rec is
    Res : Item_Rec(Chrs);
  begin
    Check_Chrs(S);
    Res.Val_Len := S.Val_Len;
    Res.Val_Text(1 .. Res.Val_Len) :=
      Upper_Str(S.Val_Text(1 .. S.Val_Len));
    return Res;
  end Strupp;

  function Strlow (S : Item_Rec) return Item_Rec is
    Res : Item_Rec(Chrs);
  begin
    Check_Chrs(S);
    Res.Val_Len := S.Val_Len;
    Res.Val_Text(1 .. Res.Val_Len) :=
      Lower_Str(S.Val_Text(1 .. S.Val_Len));
    return Res;
  end Strlow;

end Strings;

