with Ada.Text_Io;
with Argument, Con_Io, Afpx, Basic_Proc, Language, Many_Strings, String_Mng;
with Common, Command;
procedure Xwords is

  procedure Error is
  begin
    Ada.Text_Io.Put_Line ("Usage : " & Argument.Get_Program_Name & " [ -l ]");
    Basic_Proc.Set_Error_Exit_Code;
  end Error;

  -- Log option
  Log : Boolean := False;

  -- Afpx stuff
  Cursor_Field : Afpx.Field_Range;
  Cursor_Col   : Con_Io.Col_Range;
  Insert       : Boolean;
  Ptg_Result   : Afpx.Result_Rec;
  Redisplay    : Boolean;
  Afpx_Item    : Afpx.Line_Rec;
  Status_Ok    : Boolean;

  -- Fields
  Clear_Fld : constant Afpx.Field_Range := 3;
  Get_Fld : constant Afpx.Field_Range := 4;
  Search_Fld : constant Afpx.Field_Range := 5;
  Add_Word_Fld : constant Afpx.Field_Range := 8;
  Add_Noun_Fld : constant Afpx.Field_Range := 9;
  Del_Word_Fld : constant Afpx.Field_Range := 10;
  Del_Noun_Fld : constant Afpx.Field_Range := 11;
  History_Fld : constant Afpx.Field_Range := 12;
  Clear_List_Fld : constant Afpx.Field_Range := 13;
  Lmng_Fld : constant Afpx.Field_Range := 14;
  Exit_Fld : constant Afpx.Field_Range := 21;

  -- History of search requests
  History : Command.Res_List;
  Done : Boolean;

  -- A line of text
  Line : Common.Asu_Us;

  -- Us to Afpx line
  function Us2Afpx (Us : Common.Asu_Us) return Afpx.Line_Rec is
    Rec : Afpx.Line_Rec;
    List_Width : constant Afpx.Width_Range
               := Afpx.Get_Field_Width (Afpx.List_Field_No);
  begin
    Rec.Len := Common.Asu.Length (Us);
    -- Procuste
    Rec.Str := (others => ' ');
    if Rec.Len > List_Width then
      Rec.Len := List_Width;
    end if;
    Rec.Str (1 .. Rec.Len) := Language.String_To_Wide (
               Common.Asu_Ts (Us)(1 .. Rec.Len));
    return Rec;
  end Us2Afpx;

  -- Build and launch a Words command
  procedure Do_Command (Num : Afpx.Field_Range) is
    Result : Command.Res_List;
    Com, Arg : Common.Asu_Us;
    use type Afpx.Field_Range, Common.Asu_Us;
  begin
    -- Clear result
    Afpx.Line_List.Delete_List (Deallocate => False);

    -- Build command and execute it
    declare
      Str : constant String := Afpx.Decode_Field (Get_Fld, 0);
      Last : constant Natural := String_Mng.Parse_Spaces (Str, False);
    begin
      Arg := Common.Asu_Tus (Str (1 .. Last));
    end;
    case Num is
      when Search_Fld =>
        Com := Common.Asu_Tus ("ws");
      when Add_Word_Fld | Add_Noun_Fld =>
        Com := Common.Asu_Tus ("wa");
      when Del_Word_Fld | Del_Noun_Fld =>
        Com := Common.Asu_Tus ("wd");
      when others =>
        Status_Ok := False;
        return;
    end case;
    if Num = Add_Noun_Fld or else Num = Del_Noun_Fld then
      Arg := Common.Asu_Tus (Many_Strings.Cat ("-noun", Common.Asu_Ts (Arg)));
    end if;
    Command.Exec (Common.Asu_Ts (Com), Common.Asu_Ts (Arg),
                  Status_Ok, Result);

    -- Store in history if search
    if Num = Search_Fld then
      History.Insert (Arg);
    end if;

    -- Log request if needed
    if Log then
      Line := Com;
      for I in 1 .. Many_Strings.Nb (Common.Asu_Ts (Arg)) loop
        Line := Line & " " & Many_Strings.Nth (Common.Asu_Ts (Arg), I);
      end loop;
      Ada.Text_Io.Put_Line (Common.Asu_Ts (Line));
    end if;

    -- Encode result
    if not Result.Is_Empty then
      Result.Rewind;
      loop
        Result.Read (Line, Done => Done);
        Afpx.Line_List.Insert (Us2Afpx (Line));
        if Log then
          Ada.Text_Io.Put_Line (Common.Asu_Ts (Line));
        end if;
        exit when not Done;
      end loop;

      -- Move to Top
      Afpx.Line_List.Rewind;
      Afpx.Update_List(Afpx.Top);
    end if;
  end Do_Command;

  use type Afpx.Field_Range;
begin
  -- Parse option for Log
  if Argument.Get_Nbre_Arg > 1 then
    Error;
    return;
  elsif Argument.Get_Nbre_Arg = 1 then
    if Argument.Get_Parameter (1) = "-l" then
      Log := True;
    else
      Error;
      return;
    end if;
  end if;

  Afpx.Use_Descriptor (1);
  Cursor_Field := Get_Fld;
  Cursor_Col := 0;
  Insert := False;
  Redisplay := False;

  Status_Ok := True;

  loop
    -- Color of result list according to result
    if Status_Ok then
      Afpx.Reset_Field (Afpx.List_Field_No, Reset_String => False);
    else
      Afpx.Set_Field_Colors (Afpx.List_Field_No, Background => Con_Io.Red);
    end if;

    Afpx.Put_Then_Get (Cursor_Field, Cursor_Col, Insert, Ptg_Result, Redisplay);
    Redisplay := False;

    case Ptg_Result.Event is
      when Afpx.Keyboard =>
        case Ptg_Result.Keyboard_Key is
          when Afpx.Return_Key =>
            Do_Command (Search_Fld);
          when Afpx.Escape_Key =>
            Afpx.Clear_Field (Get_Fld);
          when Afpx.Break_Key =>
            exit;
        end case;
      when Afpx.Mouse_Button =>
        case Ptg_Result.Field_No is

          -- Double click in list => copy in get fld
          when Afpx.List_Field_No =>
            Afpx.Line_List.Read (Afpx_Item, Afpx.Line_List_Mng.Current);
            Afpx.Encode_Wide_Field (Get_Fld, (0, 0),
               Afpx_Item.Str (1 .. Afpx.Get_Field_Width (Afpx.List_Field_No)));

          -- Clear get
          when Clear_Fld =>
            Afpx.Clear_Field (Get_Fld);

          -- Words commands
          when Search_Fld .. Del_Noun_Fld =>
            Do_Command (Ptg_Result.Field_No);

          -- History
          when History_Fld =>
            -- Put history of search in list
            Afpx.Line_List.Delete_List (Deallocate => False);
            if not History.Is_Empty then
              History.Rewind;
              loop
                History.Read (Line, Done => Done);
                Afpx.Line_List.Insert (Us2Afpx (Line));
                exit when not Done;
              end loop;
              -- Move to Bottom
              Afpx.Line_List.Rewind (Afpx.Line_List_Mng.Prev);
              Afpx.Update_List(Afpx.Bottom);
              History.Rewind (Command.Res_Mng.Dyn_List.Prev);
            end if;
            Status_Ok := True;
          -- Clear list
          when Clear_List_Fld =>
            Afpx.Line_List.Delete_List (Deallocate => False);
            Status_Ok := True;

          -- List management
          when Lmng_Fld + 0 =>
            Afpx.Update_List(Afpx.Top);
          when Lmng_Fld + 1 =>
            Afpx.Update_List(Afpx.Page_Up);
          when Lmng_Fld + 2 =>
            Afpx.Update_List(Afpx.Up);
          when Lmng_Fld + 3 =>
            Afpx.Update_List(Afpx.Center);
          when Lmng_Fld + 4 =>
            Afpx.Update_List(Afpx.Down);
          when Lmng_Fld + 5 =>
            Afpx.Update_List(Afpx.Page_Down);
          when Lmng_Fld + 6 =>
            Afpx.Update_List(Afpx.Bottom);

          -- Exit
          when Exit_Fld =>
            exit;
          when others =>
            null;
        end case;

      when Afpx.Refresh =>
        Redisplay := True;
      when others =>
        null;
    end case;
  end loop;

  -- Done
  Afpx.Release_Descriptor;

exception
  when Command.Terminate_Request =>
    Afpx.Release_Descriptor;
end Xwords;

