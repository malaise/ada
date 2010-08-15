with Afpx, Con_Io, Upper_Str;
with Pers_Def, Str_Mng, Mesu_Mng, Pers_Mng, Pers_Fil;
package body Pers_Lis is

  procedure Build_List is
    Person : Pers_Def.Person_Rec;
    Line   : Afpx.Line_Rec;
  begin
    -- Encode list of persons
    Afpx.Line_List.Delete_List;
    if not Pers_Def.The_Persons.Is_Empty then
      Pers_Def.The_Persons.Rewind;
      loop
        Pers_Def.The_Persons.Read (Person, Pers_Def.Person_List_Mng.Current);
        Str_Mng.Format_Person_To_List (Person, Line);
        Afpx.Line_List.Insert (Line);
        exit when not Pers_Def.The_Persons.Check_Move;
        Pers_Def.The_Persons.Move_To;
      end loop;
      -- End of list
      Afpx.Line_List.Rewind (True, Afpx.Line_List_Mng.Prev);
    end if;
  end Build_List;


  procedure Set_Protection (Field : in Afpx.Field_Range;
                            Protect : in Boolean) is
  begin
    Afpx.Set_Field_Protection (Field, Protect);
    if Protect then
      Afpx.Set_Field_Colors (Field, Foreground => Con_Io.Color_Of ("Cyan"),
                                    Background => Con_Io.Color_Of ("Black"));
    else
      Afpx.Reset_Field(Field, Reset_Colors=>True, Reset_String=>False);
    end if;
  end Set_Protection;

  procedure List (Exit_Program : out Boolean) is

    First_Field  : Afpx.Field_Range;

    Cursor_Field : Afpx.Field_Range;
    Cursor_Col   : Con_Io.Col_Range;
    Insert       : Boolean;
    Ptg_Result   : Afpx.Result_Rec;
    Redisplay    : Boolean;

    List_Empty : Boolean;
    type State_List is (In_List, In_Create, In_Edit, In_Delete);
    State : State_List;
    Act : Boolean;

    Person : Pers_Def.Person_Rec;
    Pos : Natural;
    Ok : Boolean;
    Moved : Boolean;
    use Afpx;
    use Pers_Def.Person_List_Mng;

    procedure Encode_Person is
    begin
      Afpx.Encode_Field (11, (00, 00), Person.Name);
      Afpx.Encode_Field (13, (00, 00), Person.Activity);
      for I in 1 .. 6 loop
        Afpx.Encode_Field (Afpx.Field_Range (I + 16), (00, 00),
                           Str_Mng.To_Str(Person.Tz(I)) );
      end loop;
    end Encode_Person;

    -- Check a field
    procedure Check_Field (Current_Field : in out Afpx.Absolute_Field_Range;
                           Ok : out Boolean) is
      Locok : Boolean;
      Tz_S  : Str_Mng.Bpm_Str;
      Tz    : Pers_Def.Bpm_Range;
      use Pers_Def;
    begin
      case Current_Field is

        when 11 =>
          -- In name : not empty
          Person.Name := Upper_Str (Afpx.Decode_Field (11, 00));
          Str_Mng.Parse (Person.Name);
          Locok := not Str_Mng.Is_Spaces (Person.Name);
          if Locok then
            Afpx.Encode_Field (11, (00, 00), Person.Name);
            Current_Field := 13;
          end if;

        when 13 =>
          -- In activity : not empty
          Person.Activity := Upper_Str (Afpx.Decode_Field (13, 00));
          Str_Mng.Parse (Person.Activity);
          Locok := not Str_Mng.Is_Spaces (Person.Activity);
          if Locok then
            Afpx.Encode_Field (13, (00, 00), Person.Activity);
            Current_Field := 17;
          end if;

        when 17 | 18 | 19 | 20 | 21 | 22 =>
          Locok := True;
          Tz_S := Afpx.Decode_Field (Current_Field, 00);
          begin
            Tz := Str_Mng.To_Bpm(Tz_S);
          exception
            when others =>
              Locok := False;
          end;
          if Locok then
            Person.Tz (Integer(Current_Field) - 16) := Tz;
          end if;
          if Locok then
            Locok := Tz /= Pers_Def.Bpm_Range'First;
          end if;
          if Locok then
            -- Tz must be crescent
            if Current_Field /= 17
            and then Tz /= Pers_Def.Bpm_Range'First then
              Locok := Tz > Person.Tz (Integer(Current_Field) - 17);
            end if;
          end if;
          if Locok then
            Afpx.Encode_Field (Current_Field, (00, 00), Str_Mng.To_Str(Tz) );
            if Current_Field = 22 then
              Current_Field := First_Field;
            else
              Current_Field := Current_Field + 1;
            end if;
          end if;

        when others =>
          null;
      end case;

      Cursor_Col := 0;
      Insert := False;
      Ok := Locok;

    end Check_Field;

    function Compute return Boolean is
      Tzr_S, Tzm_S  : Str_Mng.Bpm_Str;
      Tzr, Tzd : Pers_Def.Bpm_Range;
      use type Pers_Def.Bpm_Range;
    begin
      -- Last field must not be empty and valid
      -- Other fields can be filled
      Cursor_Field := 22;
      Cursor_Col := 0;
      Insert := False;
      Tzm_S := Afpx.Decode_Field (22, 00);
      Person.Tz(6) := Str_Mng.To_Bpm(Tzm_S);
      if Person.Tz(6) = Pers_Def.Bpm_Range'First then
        -- TZ6 is empty
        return False;
      end if;
      Tzr_S := Afpx.Decode_Field (16, 00);
      Tzr := Str_Mng.To_Bpm(Tzr_S);
      if Tzr = Pers_Def.Bpm_Range'First then
        -- Rest rate is empty: 50% .. 90% of Max rate (Tz(6))
        for I in 1 .. 5 loop
          Person.Tz(I) := Person.Tz(6) * (Pers_Def.Bpm_Range(I) + 4) / 10;
        end loop;
      else
        -- Rest rate is set: Karvonen:
        -- Max rate + 50% .. 90% of (Max rate - Rest rate)
        Tzd := Person.Tz(6) - Tzr;
        for I in 1 .. 5 loop
          Person.Tz(I) := Tzr + Tzd * (Pers_Def.Bpm_Range(I) + 4) / 10;
        end loop;
      end if;
      Afpx.Clear_Field (16);
      return True;
    exception
      when others => return False;
    end Compute;

   use type Pers_Def.Bpm_Range;

   begin
    Exit_Program := False;

    Afpx.Use_Descriptor(2);

    State := In_List;

    Cursor_Field := 01;
    Cursor_Col := 0;
    Insert := False;
    Redisplay := False;

    Build_List;

    loop

      List_Empty := Afpx.Line_List.Is_Empty;
      -- List and menu buttons, only in list
      Act := State = In_List;
      Afpx.Set_Field_Activation (03, Act);
      Afpx.Set_Field_Activation (04, Act);
      Afpx.Set_Field_Activation (06, Act);
      -- Delete/edit if not empty and in list
      Act := Act and then not List_Empty;
      Afpx.Set_Field_Activation (07, Act);
      Afpx.Set_Field_Activation (08, Act);
      -- Edit if edit
      Act := State /= In_List;
      for I in Afpx.Field_Range'(10) .. 25 loop
        Afpx.Set_Field_Activation (I, Act);
      end loop;
      -- Un protect person name & activity if in create
      Act := State = In_Create;
      Set_Protection (11, not Act);
      Set_Protection (13, not Act);
      -- Un protect other fields if in create or edit
      Act := State = In_Create or else State = In_Edit;
      Set_Protection (16, not Act);
      Set_Protection (17, not Act);
      Set_Protection (18, not Act);
      Set_Protection (19, not Act);
      Set_Protection (20, not Act);
      Set_Protection (21, not Act);
      Set_Protection (22, not Act);
      -- Compute if in edit or create
      Afpx.Set_Field_Activation (23, Act);
      -- Confirm if Valid
      Afpx.Set_Field_Activation (09, State = In_Delete);
      -- Lock list if not in list
      Afpx.Set_Field_Protection (Afpx.List_Field_No, State /= In_List);

      Afpx.Encode_Field (02, (00, 00), Str_Mng.Current_Date_Printed);

      Afpx.Put_Then_Get (Cursor_Field, Cursor_Col, Insert,
                         Ptg_Result, Redisplay);
      Redisplay := False;

      -- Move in persons list according to Afpx selection
      if not List_Empty then
        Pers_Def.The_Persons.Move_At(Afpx.Line_List.Get_Position);
      end if;

      case Ptg_Result.Event is

        when Refresh =>
          Redisplay := True;
        when Fd_Event | Afpx.Timer_Event | Afpx.Signal_Event =>
          null;
        when Keyboard =>

          case Ptg_Result.Keyboard_Key is
            when Return_Key =>
              -- Check field and go to next if Ok
              Check_Field (Cursor_Field, Ok);
            when Escape_Key =>
              -- Clear current field
              if Cursor_Field = 11  then
                Afpx.Clear_Field (11);
                Afpx.Clear_Field (13);
                Cursor_Field := 11;
              else
                Afpx.Clear_Field (Cursor_Field);
              end if;
              Cursor_Col := 0;
              Insert := False;
            when Break_Key =>
              Exit_Program := True;
              exit;
          end case;

        when Mouse_Button =>

          case Ptg_Result.Field_No is
            when 04 =>
              -- Back to records
              exit;
            when 05 =>
              -- Exit
              Exit_Program := True;
              exit;
            when 06 =>
              -- Create
              State := In_Create;
              First_Field := 11;
              Cursor_Field := First_Field;
              Cursor_Col := 0;
              Insert := False;
              Person.Name := (others => ' ');
              Person.Activity := (others => ' ');
              Person.Tz := (others => Pers_Def.Bpm_Range'First);
              Encode_Person;
            when 00 | 08 =>
              -- Edit
              State := In_Edit;
              First_Field := 17;
              Cursor_Field := First_Field;
              Cursor_Col := 0;
              Insert := False;
              Read (Pers_Def.The_Persons, Person,
                    Pers_Def.Person_List_Mng.Current);
              Encode_Person;
            when 07 =>
              -- Delete
              State := In_Delete;
              Read (Pers_Def.The_Persons, Person,
                    Pers_Def.Person_List_Mng.Current);
              Encode_Person;
            when 23 =>
              -- Compute
              Ok := Compute;
              if Ok then
                -- Decode name & activity, then rencode all
                Cursor_Field := 11;
                Check_Field (Cursor_Field, Ok);
                Cursor_Field := 13;
                Check_Field (Cursor_Field, Ok);
                Encode_Person;
                Cursor_Field := 17;
                Cursor_Col := 0;
                Insert := False;
              end if;
            when 24 =>
              -- Valid
              if State /= In_Delete then
                Cursor_Field := First_Field;
                loop
                  Check_Field (Cursor_Field, Ok);
                  exit when not Ok or else Cursor_Field = First_Field;
                end loop;
              else
                Ok := True;
              end if;
              if Ok then
                if State = In_Create then
                  -- In create : insert person in list (uniq)
                  begin
                    Pers_Mng.Insert (Pers_Def.The_Persons, Person);
                    Build_List;
                  exception
                    when others =>
                      Cursor_Field := First_Field;
                      Ok := False;
                  end;
                elsif State = In_Edit then
                  -- In edit : update person in list
                  Modify (Pers_Def.The_Persons, Person,
                          Pers_Def.Person_List_Mng.Current);

                else
                  -- In delete : delete records, delete person
                  Mesu_Mng.Delete_All (Person);
                  -- Delete all has changed persons list
                  Pers_Mng.Search (Pers_Def.The_Persons, Person.Pid, Pos);
                  Pers_Def.The_Persons.Delete (Moved => Moved);
                  Build_List;
                end if;
              end if;
              if Ok then
                Pers_Fil.Save;
                State := In_List;
              end if;
            when 25 =>
              -- Cancel
              State := In_List;
            when others =>
              null;
          end case;
      end case;

    end loop;

    Afpx.Line_List.Delete_List;

  end List;

end Pers_Lis;

