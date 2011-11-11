with Basic_Proc, Ndbm, Normal;

procedure T_Ndbm is

  subtype Key is Natural;
  subtype Data is String (1 .. 10);

  package My_Ndbm is new Ndbm (Key, Data, "data/test_dbm");

  K : Key;
  D : Data;

  Nb_Op : Natural;

begin

  My_Ndbm.Open;

  -- Infinite loop
  for N in 1 .. 1_000 loop

    -- Fill
    for I in 1 .. 1_000 loop
      My_Ndbm.Write (I, Normal(I, 10));
    end loop;

    -- Read
    Nb_Op := 0;
    K := My_Ndbm.First_Key;
    loop
      D := My_Ndbm.Read (K);
      Nb_Op := Nb_Op + 1;
      Basic_Proc.Put_Line_Output (D);
      begin
        K := My_Ndbm.Next_Key;
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
        K := My_Ndbm.First_Key;
        begin
          My_Ndbm.Delete (K);
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
      K := My_Ndbm.First_Key;
      Basic_Proc.Put_Line_Output ("First Key Ok ????!!!!");
    exception
      when My_Ndbm.No_Data =>
        Basic_Proc.Put_Line_Output ("First Key -> No data");
    end;

    begin
      D := My_Ndbm.Read (K);
      Basic_Proc.Put_Line_Output ("Read Ok ????!!!!");
    exception
      when My_Ndbm.No_Data =>
        Basic_Proc.Put_Line_Output ("Read -> No data");
    end;

    delay 1.0;

  end loop;

  My_Ndbm.Close;

end T_Ndbm;

