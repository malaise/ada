package Types is
  type Mattrix_Kind_List is (Wish, Regret);

  -- 0.00 to 100.00 multiplied by 100
  subtype Cell_Range is Natural range 0 .. 10_000;

  Max_Dim : constant := 256;
  subtype Index_Range is Natural range 0 .. Max_Dim;
  type Mattrix_Tab is array (Index_Range range <>, Index_Range range <>)
                            of Cell_Range;

  type Mattrix_Rec (Dim : Index_Range := 0) is record
    Notes : Mattrix_Tab(1 .. Dim, 1 .. Dim);
  end record;
  type Mattrix_Rec_Access is access Mattrix_Rec;

end Types;

