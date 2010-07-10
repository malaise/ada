with Upper_Str, Lower_Str, Mixed_Str, String_Mng;
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
    use type Asu_Us;
  begin
    Check_Chrs(S1);
    Check_Chrs(S2);

    Res.Val_Text := S1.Val_Text & S2.Val_Text;
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
      return Res;
    end if;

    if I1.Val_Inte < 1
    or else I1.Val_Inte > My_Math.Inte(Asu.Length (S.Val_Text)) then
      raise Argument_Mismatch;
    end if;
    if I2.Val_Inte < 1
    or else I2.Val_Inte > My_Math.Inte(Asu.Length (S.Val_Text)) then
      raise Argument_Mismatch;
    end if;
    Res.Val_Text := Asu_Uslice (S.Val_Text, Positive(I1.Val_Inte),
                                            Natural(I2.Val_Inte));
    return Res;
  end Strsub;

  function Strloc (S, Occ, Pat : Item_Rec) return Item_Rec is
    Res : Item_Rec(Inte);
  begin
    Check_Chrs(S);
    Check_Chrs(Pat);
    Check_Inte(Occ);

    if Occ.Val_Inte < 1 or else Occ.Val_Inte > My_Math.Inte(Positive'Last) then
      raise Invalid_Argument;
    end if;

    Res.Val_Inte := My_Math.Inte(
            String_Mng.Locate (Asu_Ts (S.Val_Text),
                               Asu_Ts (Pat.Val_Text),
                               Occurence => Positive(Occ.Val_Inte)));
    return Res;
  end Strloc;

  function Strrep (S, I, Pat : Item_Rec) return Item_Rec is
    Res : Item_Rec(Chrs);
  begin
    Check_Chrs(S);
    Check_Chrs(Pat);
    Check_Inte(I);

    if I.Val_Inte < 1
    or else I.Val_Inte > My_Math.Inte(Asu.Length (S.Val_Text)) then
      raise Argument_Mismatch;
    end if;

    Res.Val_Text := Asu.Overwrite (S.Val_Text, Positive (I.Val_Inte),
                                   Asu_Ts (Pat.Val_Text));
    return Res;
  end Strrep;

  function Strlen (S : Item_Rec) return Item_Rec is
  begin
    Check_Chrs(S);
    return (Kind => Inte, Val_Inte => My_Math.Inte(Asu.Length (S.Val_Text)));
  end Strlen;

  function Strupp (S : Item_Rec) return Item_Rec is
    Res : Item_Rec(Chrs);
  begin
    Check_Chrs(S);
    Res.Val_Text := Asu_Tus (Upper_Str (Asu_Ts (S.Val_Text)));
    return Res;
  end Strupp;

  function Strlow (S : Item_Rec) return Item_Rec is
    Res : Item_Rec(Chrs);
  begin
    Check_Chrs(S);
    Res.Val_Text := Asu_Tus (Lower_Str (Asu_Ts (S.Val_Text)));
    return Res;
  end Strlow;

  function Strmix (S : Item_Rec) return Item_Rec is
    Res : Item_Rec(Chrs);
  begin
    Check_Chrs(S);
    Res.Val_Text := Asu_Tus (Mixed_Str (Asu_Ts (S.Val_Text)));
    return Res;
  end Strmix;

end Strings;

