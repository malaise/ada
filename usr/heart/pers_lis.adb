with Afpx, Con_Io, Upper_Str;
with Pers_Def, Str_Mng, Mesu_Mng, Pers_Mng, Pers_Fil, Afpx_Xref;
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
      Afpx.Line_List.Rewind (Afpx.Line_List_Mng.Prev);
    end if;
  end Build_List;


  procedure Set_Protection (Field : in Afpx.Field_Range;
                            Protect : in Boolean) is
  begin
    Afpx.Set_Field_Protection (Field, Protect);
    if Protect then
      Afpx.Set_Field_Colors (Field,
                             Foreground => Con_Io.Color_Of ("Black"),
                             Background => Con_Io.Color_Of ("Light_Grey"));
    else
      Afpx.Reset_Field(Field, Reset_Colors=>True, Reset_String=>False);
    end if;
  end Set_Protection;

  procedure List is

    First_Field : Afpx.Field_Range;

    Get_Handle : Afpx.Get_Handle_Rec;
    Ptg_Result : Afpx.Result_Rec;

    List_Empty : Boolean;
    type State_List is (In_List, In_Create, In_Edit, In_Delete);
    State : State_List;
    Act : Boolean;

    Person : Pers_Def.Person_Rec;
    Pos : Natural;
    Ok : Boolean;
    Moved : Boolean;
    use type Afpx.Field_Range;
    use Pers_Def.Person_List_Mng;

    procedure Encode_Person is
      Dummy_Pos : Integer;
    begin
      Pers_Mng.Expand (Pers_Def.The_Persons,
                       Person.Name, Person.Activity, Dummy_Pos);
      Afpx.Encode_Field (Afpx_Xref.Activity.Person, (00, 00), Person.Name);
      Afpx.Encode_Field (Afpx_Xref.Activity.Activity, (00, 00),
                         Person.Activity);
      Afpx.Encode_Field (Afpx_Xref.Activity.Sampling, (00, 00),
                         Str_Mng.To_Str (Person.Sampling_Delta));

      for I in Afpx.Absolute_Field_Range'(0) .. 5 loop
        Afpx.Encode_Field (Afpx.Field_Range (Afpx_Xref.Activity.Tz1 + I),
                           (00, 00),
                           Str_Mng.To_Str(Person.Tz(Natural(I + 1))) );
      end loop;
    end Encode_Person;

    -- Check a field
    procedure Check_Field (Current_Field : in out Afpx.Absolute_Field_Range;
                           Ok : out Boolean; Expand : in Boolean) is
      Locok : Boolean;
      Pos_Pers : Integer;
      Delta_S : Str_Mng.Sampling_Str;
      Tz_S  : Str_Mng.Bpm_Str;
      Tz    : Pers_Def.Bpm_Range;
      use type Pers_Def.Bpm_Range;
    begin
      case Current_Field is

        when Afpx_Xref.Activity.Person | Afpx_Xref.Activity.Activity =>
          -- In name or activity
          Person.Name     := Afpx.Decode_Field (Afpx_Xref.Activity.Person, 00);
          Person.Activity :=
                  Afpx.Decode_Field (Afpx_Xref.Activity.Activity, 00);

          if Str_Mng.Is_Spaces (Person.Name) then
            if Str_Mng.Is_Spaces (Person.Activity) then
              -- Name & activity empty : KO, remain in current field
              Locok := False;
            else
              -- Name emtpy but activity set : err name
              Current_Field := Afpx_Xref.Activity.Person;
              Locok := False;
            end if;
          elsif Expand then
            -- Name not empty => Expand
            Pers_Mng.Expand (Pers_Def.The_Persons,
                             Person.Name, Person.Activity, Pos_Pers);

            if Pos_Pers = 0 then
              -- One person found (no activity), already expanded
              Afpx.Encode_Field (Afpx_Xref.Activity.Person, (00, 00),
                                 Person.Name);
              Current_Field := Afpx_Xref.Activity.Activity;
              Locok := True;
            elsif Pos_Pers > 0 then
              -- One person and activity found, already expanded, show all
              Pers_Def.The_Persons.Read (Person,
                                         Pers_Def.Person_List_Mng.Current);
              Encode_Person;
              -- Activity will need to be modified
              Current_Field := Afpx_Xref.Activity.Activity;
              Locok := True;
            else
              -- Error in name or activity, remain in current field
              Locok := False;
            end if;
          elsif Current_Field = Afpx_Xref.Activity.Person then
            -- Just for check
            Locok := not Str_Mng.Is_Spaces (Person.Name);
            if Locok then
              Afpx.Encode_Field (
                  Afpx_Xref.Activity.Person, (00, 00), Upper_Str (Person.Name));
              Current_Field := Afpx_Xref.Activity.Activity;
            end if;
          elsif Current_Field = Afpx_Xref.Activity.Activity then
            -- Just for check
            Str_Mng.Parse (Person.Activity);
            Locok := not Str_Mng.Is_Spaces (Person.Activity);
            if Locok then
              Afpx.Encode_Field (Afpx_Xref.Activity.Activity, (00, 00),
                  Upper_Str (Person.Activity));
              Current_Field := Afpx_Xref.Activity.Sampling;
            end if;
          end if;

        when Afpx_Xref.Activity.Sampling =>
          Locok := True;
          Delta_S := Afpx.Decode_Field(Afpx_Xref.Activity.Sampling, 00);
          begin
            Person.Sampling_Delta := Str_Mng.To_Sampling (Delta_S);
          exception
            when others =>
              Locok := False;
          end;
          if Locok then
            Afpx.Encode_Field (Afpx_Xref.Activity.Sampling, (00, 00),
                               Str_Mng.To_Str (Person.Sampling_Delta));
            Current_Field := Afpx_Xref.Activity.Tz1;
          end if;

        when Afpx_Xref.Activity.Tz1 .. Afpx_Xref.Activity.Tz6 =>
          Locok := True;
          Tz_S := Afpx.Decode_Field (Current_Field, 00);
          begin
            Tz := Str_Mng.To_Bpm(Tz_S);
          exception
            when others =>
              Locok := False;
          end;
          if Locok then
            Person.Tz(Integer(Current_Field - Afpx_Xref.Activity.Tz1 + 1)) := Tz;
          end if;
          if Locok then
            Locok := Tz /= Pers_Def.Bpm_Range'First;
          end if;
          if Locok then
            -- Tz must be crescent
            if Current_Field /= Afpx_Xref.Activity.Tz1
            and then Tz /= Pers_Def.Bpm_Range'First then
              Locok := Tz > Person.Tz(Integer(Current_Field
                                    - Afpx_Xref.Activity.Tz1));
            end if;
          end if;
          if Locok then
            Afpx.Encode_Field (Current_Field, (00, 00), Str_Mng.To_Str(Tz) );
            if Current_Field = Afpx_Xref.Activity.Tz6 then
              Current_Field := First_Field;
            else
              Current_Field := Current_Field + 1;
            end if;
          end if;

        when others =>
          null;
      end case;

      Get_Handle.Cursor_Col := 0;
      Get_Handle.Insert := False;
      Ok := Locok;

    end Check_Field;

    function Compute return Boolean is
      Tzr_S, Tzm_S  : Str_Mng.Bpm_Str;
      Tzr, Tzd : Pers_Def.Bpm_Range;
      use type Pers_Def.Bpm_Range;
    begin
      -- Last field must not be empty and valid
      -- Other fields can be filled
      Get_Handle.Cursor_Field := Afpx_Xref.Activity.Tz6;
      Get_Handle.Cursor_Col := 0;
      Get_Handle.Insert := False;
      Tzm_S := Afpx.Decode_Field (Afpx_Xref.Activity.Tz6, 00);
      Person.Tz(6) := Str_Mng.To_Bpm(Tzm_S);
      if Person.Tz(6) = Pers_Def.Bpm_Range'First then
        -- TZ6 is empty
        return False;
      end if;
      Tzr_S := Afpx.Decode_Field (Afpx_Xref.Activity.Rest, 00);
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
      Afpx.Clear_Field (Afpx_Xref.Activity.Rest);
      return True;
    exception
      when others => return False;
    end Compute;

   begin

    Afpx.Use_Descriptor(Afpx_Xref.Activity.Dscr_Num);

    State := In_List;

    Get_Handle := (others => <>);

    Build_List;

    loop

      List_Empty := Afpx.Line_List.Is_Empty;
      -- Lock list if not in list
      Afpx.Set_Field_Protection (Afpx.List_Field_No, State /= In_List);
      -- Title
      Afpx.Clear_Field (Afpx_Xref.Activity.Title);
      case State is
        when In_List =>
          Afpx.Reset_Field (Afpx_Xref.Activity.Title, False);
        when In_Create =>
          Afpx.Encode_Field (Afpx_Xref.Activity.Title, (0, 0),
                             "Creation of an Activity");
       when In_Edit =>
          Afpx.Encode_Field (Afpx_Xref.Activity.Title, (0, 0),
                             "Edition of an Activity");
       when In_Delete =>
          Afpx.Encode_Field (Afpx_Xref.Activity.Title, (0, 0),
                             "Deletion of an Activity");
      end case;
      -- List and menu buttons, only in list
      Act := State = In_List;
      Afpx.Set_Field_Activation (Afpx_Xref.Activity.Records, Act);
      Afpx.Set_Field_Activation (Afpx_Xref.Activity.Create, Act);
      Afpx.Set_Field_Activation (Afpx_Xref.Activity.Quit, Act);
      -- Delete/clone/edit if not empty and in list
      Act := Act and then not List_Empty;
      Afpx.Set_Field_Activation (Afpx_Xref.Activity.Delete, Act);
      Afpx.Set_Field_Activation (Afpx_Xref.Activity.Clone, Act);
      Afpx.Set_Field_Activation (Afpx_Xref.Activity.Edit, Act);
      -- Edit if edit
      Act := State /= In_List;
      for I in Afpx_Xref.Activity.Person_Title
            .. Afpx_Xref.Activity.Cancel loop
        Afpx.Set_Field_Activation (I, Act);
      end loop;
      -- Un-protect person name & activity if in create
      Act := State = In_Create;
      Set_Protection (Afpx_Xref.Activity.Person, not Act);
      Set_Protection (Afpx_Xref.Activity.Activity, not Act);
      -- Un protect other fields if in create or edit
      Act := State = In_Create or else State = In_Edit;
      Set_Protection (Afpx_Xref.Activity.Rest, not Act);
      Set_Protection (Afpx_Xref.Activity.Sampling, not Act);
      for I in Afpx_Xref.Activity.Tz1 .. Afpx_Xref.Activity.Tz6 loop
        Set_Protection (I, not Act);
      end loop;
      -- Compute if in edit or create
      Afpx.Set_Field_Activation (Afpx_Xref.Activity.Compute, Act);
      -- Confirm if Valid
      Afpx.Set_Field_Activation (Afpx_Xref.Activity.Confirm, State = In_Delete);

      Afpx.Encode_Field (Afpx_Xref.Activity.Date, (00, 00),
                         Str_Mng.Current_Date_Printed);

      Afpx.Put_Then_Get (Get_Handle, Ptg_Result);

      -- Move in persons list according to Afpx selection
      if not List_Empty then
        Pers_Def.The_Persons.Move_At (Positive (Afpx.Line_List.Get_Position));
      end if;

      case Ptg_Result.Event is

        when Afpx.Fd_Event | Afpx.Timer_Event | Afpx.Signal_Event
           | Afpx.Refresh =>
          null;
        when Afpx.Keyboard =>

          case Ptg_Result.Keyboard_Key is
            when Afpx.Return_Key =>
              -- Check field and go to next if Ok
              Check_Field (Get_Handle.Cursor_Field, Ok, True);
            when Afpx.Escape_Key =>
              -- Clear current field
              if Get_Handle.Cursor_Field = Afpx_Xref.Activity.Person  then
                Afpx.Clear_Field (Afpx_Xref.Activity.Person);
                Afpx.Clear_Field (Afpx_Xref.Activity.Activity);
                Get_Handle.Cursor_Field := Afpx_Xref.Activity.Person;
              else
                Afpx.Clear_Field (Get_Handle.Cursor_Field);
              end if;
              Get_Handle.Cursor_Col := 0;
              Get_Handle.Insert := False;
            when Afpx.Break_Key =>
              raise Pers_Def.Exit_Requested;
          end case;

        when Afpx.Mouse_Button =>

          case Ptg_Result.Field_No is
            when Afpx_Xref.Activity.Records =>
              -- Back to records
              exit;
            when Afpx_Xref.Activity.Quit =>
              -- Exit
              raise Pers_Def.Exit_Requested;
            when Afpx_Xref.Activity.Create =>
              -- Create
              State := In_Create;
              First_Field := Afpx_Xref.Activity.Person;
              Get_Handle.Cursor_Field := First_Field;
              Get_Handle.Cursor_Col := 0;
              Get_Handle.Insert := False;
              Person.Name := (others => ' ');
              Person.Activity := (others => ' ');
              Person.Tz := (others => Pers_Def.Bpm_Range'First);
              Encode_Person;
            when Afpx_Xref.Activity.Clone =>
              -- Clone
              State := In_Create;
              First_Field := Afpx_Xref.Activity.Activity;
              Get_Handle.Cursor_Field := First_Field;
              Get_Handle.Cursor_Col := 0;
              Get_Handle.Insert := False;
              Read (Pers_Def.The_Persons, Person,
                    Pers_Def.Person_List_Mng.Current);
              Encode_Person;
            when Afpx.List_Field_No | Afpx_Xref.Activity.Edit =>
              -- Edit
              State := In_Edit;
              First_Field := Afpx_Xref.Activity.Sampling;
              Get_Handle.Cursor_Field := First_Field;
              Get_Handle.Cursor_Col := 0;
              Get_Handle.Insert := False;
              Read (Pers_Def.The_Persons, Person,
                    Pers_Def.Person_List_Mng.Current);
              Encode_Person;
            when Afpx_Xref.Activity.Delete =>
              -- Delete
              State := In_Delete;
              Read (Pers_Def.The_Persons, Person,
                    Pers_Def.Person_List_Mng.Current);
              Encode_Person;
            when Afpx_Xref.Activity.Compute =>
              -- Compute
              Ok := Compute;
              if Ok then
                -- Decode name, activity & sampling delta, then rencode all
                Get_Handle.Cursor_Field := Afpx_Xref.Activity.Person;
                Check_Field (Get_Handle.Cursor_Field, Ok, False);
                Get_Handle.Cursor_Field := Afpx_Xref.Activity.Activity;
                Check_Field (Get_Handle.Cursor_Field, Ok, False);
                Get_Handle.Cursor_Field := Afpx_Xref.Activity.Sampling;
                Check_Field (Get_Handle.Cursor_Field, Ok, False);
                Encode_Person;
                Get_Handle.Cursor_Field := Afpx_Xref.Activity.Tz1;
                Get_Handle.Cursor_Col := 0;
                Get_Handle.Insert := False;
              end if;
            when Afpx_Xref.Activity.Valid =>
              -- Valid
              if State /= In_Delete then
                Get_Handle.Cursor_Field := First_Field;
                loop
                  Check_Field (Get_Handle.Cursor_Field, Ok, False);
                  exit when not Ok
                  or else Get_Handle.Cursor_Field = First_Field;
                end loop;
              else
                Ok := True;
              end if;
              if Ok then
                if State = In_Create then
                  -- In create : insert person in list (uniq)
                  Encode_Person;
                  begin
                    Pers_Mng.Insert (Pers_Def.The_Persons, Person);
                    Build_List;
                  exception
                    when others =>
                      Get_Handle.Cursor_Field := First_Field;
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
            when Afpx_Xref.Activity.Cancel =>
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

