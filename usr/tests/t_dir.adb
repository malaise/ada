with Ada.Calendar;
with As.U, My_Io, Int_Io, Directory, Sys_Calls, Argument, Day_Mng, Normal;
procedure T_Dir is
  File_Name : As.U.Asu_Us;
  Dir_Name : As.U.Asu_Us;
  Fstat : Sys_Calls.File_Stat_Rec;
  Max_Len : constant := 50;
  Pad : constant String(1 .. Max_Len) := (others => ' ');

  function Get_New_Dir return String is
    Str : String(1 .. 1024);
    Len : Natural;
  begin
    My_Io.Put ("Enter new directory: ");
    My_Io.Get_Line (Str, Len);
    return Str(1 .. Len);
  end  Get_New_Dir;

  procedure Put_Rights (Rights : in Natural) is
    Str : String(1 .. 7) := (others => ' ');
    Zstr : String(1 .. 4) := (others => '0');
    F,L : Natural;
  begin
    My_Io.Put (" ");
    Int_Io.Put (Str, Rights, Base => 8);
    for I in Str'Range loop
      if Str(I) = '#' then
        F := I+1;
        exit;
      end if;
    end loop;
    for I in reverse Str'Range loop
      if Str(I) = '#' then
        L := I - 1;
        exit;
      end if;
    end loop;

    Zstr(4 - L + F .. 4) := Str(F .. L);
    My_Io.Put(Zstr);
  end Put_Rights;

  procedure Put_Id (Id : in Natural) is
  begin
    My_Io.Put (" " & Normal (Id, 4));
  end Put_Id;

  procedure Put_Size (Size : in Sys_Calls.Size_T) is
  begin
    My_Io.Put(" " & Normal (Integer(Size), 10));
  end Put_Size;

  procedure Put_Date (Mtime : in Sys_Calls.Time_T) is
    T : Ada.Calendar.Time;
    Year   : Ada.Calendar.Year_Number;
    Month  : Ada.Calendar.Month_Number;
    Day    : Ada.Calendar.Day_Number;
    Dur    : Ada.Calendar.Day_Duration;
    Hours    : Day_Mng.T_Hours;
    Minutes  : Day_Mng.T_Minutes;
    Seconds  : Day_Mng.T_Seconds;
    Millisec : Day_Mng.T_Millisec;
  begin
    T := Sys_Calls.Time_Of (Mtime);
    Ada.Calendar.Split (T, Year, Month, Day, Dur);
    Day_Mng.Split (Dur, Hours, Minutes, Seconds, Millisec);
    My_Io.Put (" " &
               Normal (Year, 4, Gap =>'0') & '/' &
               Normal (Month, 2, Gap =>'0') & '/' &
               Normal (Day, 2, Gap =>'0') & ' '  &
               Normal (Hours, 2, Gap =>'0') & ':'  &
               Normal (Minutes, 2, Gap =>'0') & ':'  &
               Normal (Seconds, 2, Gap =>'0') );
  end Put_Date;

  use type Sys_Calls.File_Kind_List;
begin
  if Argument.Get_Nbre_Arg = 1 then
    Directory.Create (Argument.Get_Parameter);
    My_Io.Put_Line ("Created dir " & Argument.Get_Parameter);
  end if;

  loop

    My_Io.Put_Line ("PWD ->" & Directory.Get_Current & "<");

    declare
      Dsc : Directory.Dir_Desc;
    begin
      if Argument.Get_Nbre_Arg /= 0 then
        Argument.Get_Parameter (Dir_Name);
      else
        Directory.Get_Current (Dir_Name);
      end if;
      Dsc.Open (Dir_Name.Image);
      if Dir_Name.Image = "/" then
        Dir_Name.Set_Null;
      end if;
      loop
        Dsc.Next_Entry (File_Name);
        My_Io.Put ("  ---->" & File_Name.Image & "< ");
        My_Io.Put (Pad(1 .. Max_Len - File_Name.Length));
        begin
          Fstat := Sys_Calls.File_Stat (Dir_Name.Image & '/' & File_Name.Image);
          Put_Id (Fstat.User_Id);
          Put_Id (Fstat.Group_Id);
          Put_Rights (Fstat.Rights);
          Put_Date (Fstat.Modif_Time);
          My_Io.Put (" " & Fstat.Kind'Img);
          if Fstat.Kind = Sys_Calls.File then
            Put_Size (Fstat.Size);
            My_Io.New_Line;
          elsif Fstat.Kind = Sys_Calls.Link then
            My_Io.New_Line;
            My_Io.Put_Line ("    ++++>" & Directory.Read_Link (
                Dir_Name.Image & '/' & File_Name.Image) & '<');
          else
            My_Io.New_Line;
          end if;
        exception
          when Directory.Name_Error =>
            My_Io.Put_Line ("???? ???");
          when Directory.Access_Error =>
            My_Io.Put_Line ("!!!! !!!");
          when Directory.Recursive_Link =>
            My_Io.Put_Line ("#### ###");
        end;
      end loop;
    exception
      when Directory.End_Error =>
        null;
    end;
    if Argument.Get_Nbre_Arg /= 0 then
      exit;
    end if;

    My_Io.New_Line;
    loop
      begin
        Directory.Change_Current (Get_New_Dir);
        exit;
      exception
        when Directory.Name_Error =>
          My_Io.Put_Line ("-> Not found.");
        when Directory.Access_Error =>
          My_Io.Put_Line ("-> Permission.");
      end;
    end loop;

  end loop;

  if Argument.Get_Nbre_Arg = 1 then
    Directory.Remove (Argument.Get_Parameter);
    My_Io.Put_Line ("Removed dir " & Argument.Get_Parameter);
  end if;
end T_Dir;

