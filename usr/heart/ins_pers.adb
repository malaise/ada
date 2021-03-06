with As.B, Argument, Basic_Proc, Normal;
with Pers_Def, Pers_Fil, Pers_Mng;

procedure Ins_Pers is
  Person, Got_Person : Pers_Def.Person_Rec;
  Nn, Na, Nt : Positive;
  Name_Txt : As.B.Asb_Bs(Person.Name'Length);
  Acti_Txt : As.B.Asb_Bs(Person.Activity'Length);
  Pos : Natural;
  C : Character;
  List_Length : Natural;

  procedure Put (Person : in Pers_Def.Person_Rec;
                 Display_Pid : in Boolean := False) is
  begin
    Basic_Proc.Put_Output ("NAME: >" & Person.Name & "<");
    Basic_Proc.Put_Output ("  ACTIVITY: >" & Person.Activity & "<");
    if Display_Pid then
      Basic_Proc.Put_Line_Output ("  PID: " & Normal(Integer(Person.Pid), 3) );
    else
      Basic_Proc.New_Line_Output;
    end if;
    Basic_Proc.Put_Output (" Training Zones: ");
    for Tz of Person.Tz loop
      Basic_Proc.Put_Output (Normal(Integer(Tz), 4));
    end loop;
    Basic_Proc.New_Line_Output;
  end Put;

begin

  if Argument.Get_Nbre_Arg = 1 and then Argument.Get_Parameter = "-l" then
    -- Load list
    Pers_Fil.Load;
    if Pers_Def.The_Persons.Is_Empty then
      Basic_Proc.Put_Line_Output ("The list is empty.");
    else
      List_Length := Pers_Def.The_Persons.List_Length;
      for I in 1 .. List_Length loop
        if I /= List_Length then
          Pers_Def.The_Persons.Read (Person);
        else
          -- Do not move after reading last person
          Pers_Def.The_Persons.Read (Person, Pers_Def.Person_List_Mng.Current);
        end if;
        Put(Person, True);
      end loop;
    end if;
    Basic_Proc.New_Line_Output;
    Basic_Proc.Put_Line_Output ("Done.");
    return;
  end if;

  -- Parse arguments in person record
  Nn := Argument.Get_Position (Param_Key => "n");
  if Nn /= 1 then
    raise Constraint_Error;
  end if;
  Na := Argument.Get_Position (Param_Key => "a");
  Nt := Argument.Get_Position (Param_Key => "t");
  if      Argument.Get_Parameter (Param_Key => "n") /= ""
  or else Argument.Get_Parameter (Param_Key => "a") /= ""
  or else Argument.Get_Parameter (Param_Key => "t") /= "" then
    raise Constraint_Error;
  end if;

  if Nt < Na or else Nt /= Argument.Get_Nbre_Arg - Person.Tz'Length then
    raise Constraint_Error;
  end if;
  for I in 2 .. Na - 1 loop
    Name_Txt.Append (String'(Argument.Get_Parameter (I)));
    if I /= Na - 1 then
      Name_Txt.Append (' ');
    end if;
  end loop;
  Person.Name(1 .. Name_Txt.Length) := Name_Txt.Image;
  for I in Na + 1 .. Nt - 1 loop
    Acti_Txt.Append (String'(Argument.Get_Parameter (I)));
    if I /= Nt - 1 then
      Acti_Txt.Append (' ');
    end if;
  end loop;
  Person.Activity(1 .. Acti_Txt.Length) := Acti_Txt.Image;
  for I in Nt + 1 .. Argument.Get_Nbre_Arg loop
    Person.Tz(I-Nt) := Pers_Def.Bpm_Range'Value(Argument.Get_Parameter(I));
  end loop;

  -- Load list
  Pers_Fil.Load;
  -- Check wether this person exists
  Pers_Mng.Search (Pers_Def.The_Persons, Person.Name, Person.Activity, Pos);
  if Pos = 0 then
    -- Display and ask confirm
    Put (Person);
    Basic_Proc.Put_Output ("This person will be inserted. OK? (Y/N) : ");
    Basic_Proc.Get (C);
    if C /= 'Y' and then C /= 'y' then
      Basic_Proc.Put_Line_Output ("Aborted");
      return;
    end if;
    -- Insert
    Pers_Mng.Insert (Pers_Def.The_Persons, Person);
    Basic_Proc.New_Line_Output;
    Basic_Proc.Put_Line_Output ("Done. PID is " & Normal(Integer(Person.Pid), 3) );
  else
    Pers_Def.The_Persons.Read (Got_Person, Pers_Def.Person_List_Mng.Current);
    Person.Pid := Got_Person.Pid;
    Basic_Proc.Put_Line_Output ("The person in list");
    Put (Got_Person, True);
    Basic_Proc.New_Line_Output;
    Basic_Proc.Put_Line_Output ("Will be replaced by");
    Put (Person, True);
    Basic_Proc.Put_Output ("OK? (Y/N) : ");
    Basic_Proc.Get (C);
    if C /= 'Y' and then C /= 'y' then
      Basic_Proc.Put_Line_Output ("Aborted");
      return;
    end if;
    Pers_Def.The_Persons.Modify (Person, Pers_Def.Person_List_Mng.Current);
    Basic_Proc.New_Line_Output;
    Basic_Proc.Put_Line_Output ("Done.");
  end if;
  -- Save list
  Pers_Fil.Save;
  -- Ok. Display Pid.

exception
  when others =>
    Basic_Proc.Put_Line_Output ("USAGE : " & Argument.Get_Program_Name
              & " -n <person name> -a <activity> -t <6 training zones>");
    Basic_Proc.Put_Line_Output (" or   : " & Argument.Get_Program_Name & " -l");
    Basic_Proc.Put_Line_Output ("Each training zone must be between 0 and 250");
    raise;
end Ins_Pers;

