with Basic_Proc, Ndbm, Normal;

procedure T_Ndbm is

  subtype Key is Natural;
  subtype Data is String (1 .. 10);

  package My_Ndbm is new Ndbm (Key, Data);
  Db : My_Ndbm.Database;

  K : Key;
  D : Data;

  Nb_Op : Natural;

begin

  Db.Open ("data/test_dbm");

  -- Infinite loop
  for N in 1 .. 1_000 loop

    -- Fill
    for I in 1 .. 1_000 loop
      Db.Write (I, Normal(I, 10));
    end loop;

    -- Read
    Nb_Op := 0;
    K := Db.First_Key;
    loop
      D := Db.Read (K);
      Nb_Op := Nb_Op + 1;
      Basic_Proc.Put_Line_Output (D);
      begin
        K := Db.Next_Key;
      exception
        when My_Ndbm.No_Data =>
           Basic_Proc.Put_Line_Output ("No more to read, "
                                & Natural'Image(Nb_Op)
                                & " done.");
           exit;
      end;
    end loop;

    -- Delete
    Nb_Op := 0;
    loop
      begin
        K := Db.First_Key;
        begin
          Db.Delete (K);
          Nb_Op := Nb_Op + 1;
        exception
          when My_Ndbm.No_Data =>
            Basic_Proc.Put_Line_Output ("Delete Not Ok ????!!!!");
        end;
      exception
        when My_Ndbm.No_Data =>
          Basic_Proc.Put_Line_Output ("No more to delete, "
                                & Natural'Image(Nb_Op)
                                & " done.");
          exit;
      end;
    end loop;

    begin
      K := Db.First_Key;
      Basic_Proc.Put_Line_Output ("First Key Ok ????!!!!");
    exception
      when My_Ndbm.No_Data =>
        Basic_Proc.Put_Line_Output ("First Key -> No data");
    end;

    begin
      D := Db.Read (K);
      Basic_Proc.Put_Line_Output ("Read Ok ????!!!!");
    exception
      when My_Ndbm.No_Data =>
        Basic_Proc.Put_Line_Output ("Read -> No data");
    end;

    delay 1.0;

  end loop;

  Db.Close;

end T_Ndbm;

