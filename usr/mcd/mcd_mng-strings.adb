with Upper_Str, Lower_Str, Mixed_Str;
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
    if X.Kind /= Inte or else abs X.Val_Inte > My_Math.Inte(Integer'Last) then
      raise Invalid_Argument;
    end if;
  end Check_Inte;

  procedure Check_Pos (X : in Item_Rec) is
  begin
    if X.Kind /= Inte or else X.Val_Inte < 0
    or else X.Val_Inte > My_Math.Inte(Integer'Last) then
      raise Invalid_Argument;
    end if;
  end Check_Pos;

  procedure Check_Notnull (X : in Item_Rec) is
  begin
    if X.Val_Inte = 0 then
      raise Invalid_Argument;
    end if;
  end Check_Notnull;

  function Strnull (S : Item_Rec) return Item_Rec is
    Res : Item_Rec(Bool);
  begin
    Check_Chrs(S);
    Res.Val_Bool := S.Val_Text.Is_Null;
    return Res;
  end Strnull;

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
    Check_Pos(I1);
    Check_Notnull(I1);
    Check_Pos(I2);

    Res := S;
    Res.Val_Text := S.Val_Text.Uslice (Positive(I1.Val_Inte),
                                       Natural(I2.Val_Inte));
    return Res;
  exception
    when As.Index_Error =>
      raise Argument_Mismatch;
  end Strsub;

  function Strloc (S, Occ, Pat : Item_Rec) return Item_Rec is
    Res : Item_Rec(Inte);
  begin
    Check_Chrs(S);
    Check_Chrs(Pat);
    Check_Inte(Occ);
    if Occ.Val_Inte = 0 then
      -- Occ = 0 => Result = 0
      Res.Val_Inte := 0;
    else
      Res.Val_Inte := My_Math.Inte(
            S.Val_Text.Locate (Pat.Val_Text.Image,
                               Forward => Occ.Val_Inte > 0,
                               Occurence => Positive(abs Occ.Val_Inte)));
    end if;
    return Res;
  end Strloc;

  function Strins (S, I, Sub : Item_Rec) return Item_Rec is
    Res : Item_Rec(Chrs);
  begin
    Check_Chrs(S);
    Check_Chrs(Sub);
    Check_Pos(I);
    Check_Notnull(I);

    Res := S;
    Res.Val_Text.Insert (Positive (I.Val_Inte),
                         Sub.Val_Text.Image);
    return Res;
  exception
    when As.Index_Error =>
      raise Argument_Mismatch;
  end Strins;

  function Strovw (S, I, Sub : Item_Rec) return Item_Rec is
    Res : Item_Rec(Chrs);
  begin
    Check_Chrs(S);
    Check_Chrs(Sub);
    Check_Pos(I);
    Check_Notnull(I);

    Res := S;
    Res.Val_Text.Overwrite (Positive (I.Val_Inte),
                            Sub.Val_Text.Image);
    return Res;
  exception
    when As.Index_Error =>
      raise Argument_Mismatch;
  end Strovw;

  function Strdel (S, I, J : Item_Rec) return Item_Rec is
    Res : Item_Rec(Chrs);
  begin
    Check_Chrs(S);
    Check_Pos(I);
    Check_Notnull(I);
    Check_Pos(J);

    Res := S;
    Res.Val_Text.Delete (Positive (I.Val_Inte),
                         Natural (J.Val_Inte));
    return Res;
  exception
    when As.Index_Error =>
      raise Argument_Mismatch;
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

