with Afpx, Con_Io, Normal, Upper_Str, My_Math;
with Pers_Def, Str_Mng, Mesu_Mng, Pers_Mng, Pers_Fil;
package body Pers_Lis is

  procedure Build_List is
    Person : Pers_Def.Person_Rec;
    Line   : Afpx.Line_Rec;
    use Pers_Def.Person_List_Mng;
  begin
    -- Encode list of persons
    Afpx.Line_List_Mng.Delete_List (Afpx.Line_List);
    if not Pers_Def.Person_List_Mng.Is_Empty (Pers_Def.The_Persons) then
      Rewind (Pers_Def.The_Persons);
      loop
        Read (Pers_Def.The_Persons, Person, Pers_Def.Person_List_Mng.Current);
        Str_Mng.Format_Person_To_List (Person, Line);
        Afpx.Line_List_Mng.Insert (Afpx.Line_List, Line);
        exit when not Check_Move (Pers_Def.The_Persons);
        Move_To (Pers_Def.The_Persons);
      end loop;
      -- End of list
      Afpx.Line_List_Mng.Rewind (Afpx.Line_List, Afpx.Line_List_Mng.Prev);
    end if;
  end Build_List;


  procedure Set_Protection (Field : in Afpx.Field_Range;
                            Protect : in Boolean) is
  begin
    Afpx.Set_Field_Protection (Field, Protect);
    if Protect then
      Afpx.Set_Field_Colors (Field, Foreground => Con_Io.Cyan,
                                    Background => Con_Io.Black);
    else
      Afpx.Reset_Field(Field, Reset_Colors=>True, Reset_String=>False);
    end if;
  end Set_Protection;

  procedure List (Exit_Program : out Boolean) is

    First_Field  : Afpx.Field_Range;

    Cursor_Field : Afpx.Field_Range;
    Cursor_Col   : Con_Io.Col_Range;
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
        Afpx.Encode_Field (Afpx.Field_Range (I + 15), (00, 00),
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
            Current_Field := 16;
          end if;

        when 16 | 17 | 18 | 19 | 20 | 21 =>
          Tz_S := Afpx.Decode_Field (Current_Field, 00);
          begin
            Tz := Str_Mng.To_Bpm(Tz_S);
          exception
            when others =>
              Locok := False;
          end;
          if Locok then
            Person.Tz (Integer(Current_Field) - 15) := Tz;
          end if;
          if Locok then
            Locok := Tz /= Pers_Def.Bpm_Range'First;
          end if;
          if Locok then
            -- Tz must be crescent
            if Current_Field /= 16
            and then Tz /= Pers_Def.Bpm_Range'First then
              Locok := Tz > Person.Tz (Integer(Current_Field) - 16);
            end if;
          end if;
          if Locok then
            Afpx.Encode_Field (Current_Field, (00, 00), Str_Mng.To_Str(Tz) );
            if Current_Field = 21 then
              Current_Field := First_Field;
            else
              Current_Field := Current_Field + 1;
            end if;
          end if;

        when others =>
          null;
      end case;

      Cursor_Col := 0;
      Ok := Locok;

    end Check_Field;

    function Check_Compute return Boolean is
      subtype Tz_Range is Integer range Pers_Def.Person_Tz_Array'First ..
                                        Pers_Def.Person_Tz_Array'Last;

      function Get_Tz (N : Tz_Range) return Pers_Def.Bpm_Range is
        Tz_S  : Str_Mng.Bpm_Str;
      begin
        Tz_S := Afpx.Decode_Field (Afpx.Field_Range(N) + 15, 00);
        return Str_Mng.To_Bpm(Tz_S);
      end Get_Tz;

      use Pers_Def;
    begin
      Cursor_Col := 0;
      -- Last field must not be empty and valid
      Cursor_Field := 21;
      Person.Tz(6) := Get_Tz (6);
      if Person.Tz(6) = Pers_Def.Bpm_Range'First then
        return False;
      end if;

      -- First field must be empty or valid
      Cursor_Field := 16;
      Person.Tz(1) := Get_Tz (1);

      -- First value > last value
      if Person.Tz(1) >= Person.Tz(6) then
        return False;
      end if;

      -- Other fields must be empty
      for I in 2 .. 5 loop
        Cursor_Field := Afpx.Field_Range(15 + I);
        if Get_Tz(I) /= 0 then
          return False;
        end if;
      end loop;

      return True;

    exception
      when others => return False;
    end Check_Compute;



   begin
    Exit_Program := False;

    Afpx.Use_Descriptor(2);

    State := In_List;

    Cursor_Field := 01;
    Cursor_Col := 0;
    Redisplay := False;

    Build_List;

    loop

      List_Empty := Afpx.Line_List_Mng.List_Length(Afpx.Line_List) = 0;
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
      for I in Afpx.Field_Range'(10) .. 23 loop
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
      -- Compute if in edit or create
      Afpx.Set_Field_Activation (24, Act);
      -- Confirm if Valid
      Afpx.Set_Field_Activation (09, State = In_Delete);


      Afpx.Encode_Field (02, (00, 00), Str_Mng.Current_Date_Printed);

      Afpx.Put_Then_Get (Cursor_Field, Cursor_Col, Ptg_Result, Redisplay);
      Redisplay := False;

      -- Move in persons list according to Afpx selection
      Pers_Def.Person_List_Mng.Move_To(
              Pers_Def.The_Persons, Next,
              Afpx.Line_List_Mng.Get_Position(Line_List) - 1,
              False);
      

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
              Person.Name := (others => ' ');
              Person.Activity := (others => ' ');
              Person.Tz := (others => Pers_Def.Bpm_Range'First);
              Encode_Person;
            when 00 | 08 =>
              -- Edit
              State := In_Edit;
              First_Field := 16;
              Cursor_Field := First_Field;
              Cursor_Col := 0;
              Read (Pers_Def.The_Persons, Person,
                    Pers_Def.Person_List_Mng.Current);
              Encode_Person;
            when 07 =>
              -- Delete
              State := In_Delete;
              Read (Pers_Def.The_Persons, Person,
                    Pers_Def.Person_List_Mng.Current);
              Encode_Person;
            when 22 =>
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
                  Delete (Pers_Def.The_Persons, Done => Moved);
                  Build_List;
                end if;
              end if;
              if Ok then
                Pers_Fil.Save;
                State := In_List;
              end if;
            when 23 =>
              -- Cancel
              State := In_List;
            when 24 =>
              -- Compute
              Ok := Check_Compute;
              if Ok then
                declare
                  Rest_Rate : Pers_Def.Bpm_Range;
                  Delta_Rate : My_Math.Real;
                  Percent : My_Math.Real;
                  use My_Math;
                  use Pers_Def;
                begin
                  Rest_Rate := Person.Tz(1);
                  Delta_Rate := My_Math.Real (Person.Tz(6) - Person.Tz(1));
                  -- Rest_Rate + 50% .. 90% of Delta
                  for I in 1 .. 5 loop
                    Percent := My_Math.Real (50 + (I - 1) * 10) / 100.0;
                    Person.Tz(I) :=
                       Pers_Def.Bpm_Range(My_Math.Trunc(Delta_Rate * Percent))
                     + Rest_Rate;
                  end loop;
                end;
                -- Decode name & activity, then rencode all
                Cursor_Field := 11;
                Check_Field (Cursor_Field, Ok);
                Cursor_Field := 13;
                Check_Field (Cursor_Field, Ok);
                Encode_Person;

                Cursor_Field := 16;
                Cursor_Col := 0;
              end if;
            when others =>
              null;
          end case;
      end case;

    end loop;

    Afpx.Line_List_Mng.Delete_List (Afpx.Line_List);

  end List;

end Pers_Lis;
