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
    use type As.U.Asu_Us;
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
    or else I1.Val_Inte > My_Math.Inte(S.Val_Text.Length) then
      raise Argument_Mismatch;
    end if;
    if I2.Val_Inte < 1
    or else I2.Val_Inte > My_Math.Inte(S.Val_Text.Length) then
      raise Argument_Mismatch;
    end if;
    Res.Val_Text := S.Val_Text.Uslice (Positive(I1.Val_Inte),
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
            String_Mng.Locate (S.Val_Text.Image,
                               Pat.Val_Text.Image,
                               Occurence => Positive(Occ.Val_Inte)));
    return Res;
  end Strloc;

  function Strrep (S, I, J, Sub : Item_Rec) return Item_Rec is
    Res : Item_Rec(Chrs);
  begin
    Check_Chrs(S);
    Check_Chrs(Sub);
    Check_Inte(I);
    Check_Inte(J);

    -- May raise Index_Error if Low > Source.Length+1 or High > Source.Length
    if I.Val_Inte < 1
    or else I.Val_Inte > My_Math.Inte(S.Val_Text.Length) + 1
    or else J.Val_Inte < 1
    or else J.Val_Inte > My_Math.Inte(S.Val_Text.Length) then
      raise Invalid_Argument;
    end if;
    Res := S;
    Res.Val_Text.Replace (Positive (I.Val_Inte), Natural (J.Val_Inte),
                          Sub.Val_Text.Image);
    return Res;
  end Strrep;

  function Strins (S, I, Sub : Item_Rec) return Item_Rec is
    Res : Item_Rec(Chrs);
  begin
    Check_Chrs(S);
    Check_Chrs(Sub);
    Check_Inte(I);

    if I.Val_Inte < 1
    or else I.Val_Inte > My_Math.Inte(S.Val_Text.Length) then
      raise Argument_Mismatch;
    end if;
    Res := S;
    Res.Val_Text.Insert (Positive (I.Val_Inte),
                         Sub.Val_Text.Image);
    return Res;
  end Strins;

  function Strovw (S, I, Sub : Item_Rec) return Item_Rec is
    Res : Item_Rec(Chrs);
  begin
    Check_Chrs(S);
    Check_Chrs(Sub);
    Check_Inte(I);

    if I.Val_Inte < 1
    or else I.Val_Inte > My_Math.Inte(S.Val_Text.Length) then
      raise Argument_Mismatch;
    end if;
    Res := S;
    Res.Val_Text.Overwrite (Positive (I.Val_Inte),
                            Sub.Val_Text.Image);
    return Res;
  end Strovw;

  function Strdel (S, I, J : Item_Rec) return Item_Rec is
    Res : Item_Rec(Chrs);
  begin
    Check_Chrs(S);
    Check_Inte(I);
    Check_Inte(J);

    if I.Val_Inte < 1
    or else J.Val_Inte > My_Math.Inte(S.Val_Text.Length) then
      raise Argument_Mismatch;
    end if;
    Res := S;
    Res.Val_Text.Delete (Positive (I.Val_Inte),
                         Natural (J.Val_Inte));
    return Res;
  end Strdel;

  function Strlen (S : Item_Rec) return Item_Rec is
  begin
    Check_Chrs(S);
    return (Kind => Inte, Val_Inte => My_Math.Inte(S.Val_Text.Length));
  end Strlen;

  function Strupp (S : Item_Rec) return Item_Rec is
    Res : Item_Rec(Chrs);
  begin
    Check_Chrs(S);
    Res.Val_Text := As.U.Tus (Upper_Str (S.Val_Text.Image));
    return Res;
  end Strupp;

  function Strlow (S : Item_Rec) return Item_Rec is
    Res : Item_Rec(Chrs);
  begin
    Check_Chrs(S);
    Res.Val_Text := As.U.Tus (Lower_Str (S.Val_Text.Image));
    return Res;
  end Strlow;

  function Strmix (S : Item_Rec) return Item_Rec is
    Res : Item_Rec(Chrs);
  begin
    Check_Chrs(S);
    Res.Val_Text := As.U.Tus (Mixed_Str (S.Val_Text.Image));
    return Res;
  end Strmix;

end Strings;

