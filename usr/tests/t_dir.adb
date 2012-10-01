with Ada.Calendar, Ada.Text_Io;
with As.U, Basic_Proc, Directory, Sys_Calls, Argument, Day_Mng, Normal, Normal_Long;
procedure T_Dir is
  package Int_Io is new Ada.Text_Io.Integer_Io (Integer);
  File_Name : As.U.Asu_Us;
  Dir_Name : As.U.Asu_Us;
  Fstat : Sys_Calls.File_Stat_Rec;
  Max_Len : constant := 50;
  Pad : constant String(1 .. Max_Len) := (others => ' ');

  function Get_New_Dir return String is
  begin
    Basic_Proc.Put_Output ("Enter new directory: ");
    return Basic_Proc.Get_Line;
  end  Get_New_Dir;

  procedure Put_Rights (Rights : in Natural) is
    Str : String(1 .. 7) := (others => ' ');
    Zstr : String(1 .. 4) := (others => '0');
    F,L : Natural;
  begin
    Basic_Proc.Put_Output (" ");
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
    Basic_Proc.Put_Output(Zstr);
  end Put_Rights;

  procedure Put_Id (Id : in Natural) is
  begin
    Basic_Proc.Put_Output (" " & Normal (Id, 4));
  end Put_Id;

  procedure Put_Size (Size : in Sys_Calls.Off_T) is
  begin
    Basic_Proc.Put_Output(" " & Normal_Long (Size, 12));
  exception
    when Constraint_Error => Basic_Proc.Put_Output(" " & "xxxxxxxxxx");
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
    use type Ada.Calendar.Time;
  begin
    T := Sys_Calls.Time_Of (Mtime) + Sys_Calls.Gmt_Offset;
    Ada.Calendar.Split (T, Year, Month, Day, Dur);
    Day_Mng.Split (Dur, Hours, Minutes, Seconds, Millisec);
    Basic_Proc.Put_Output (" " &
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
    Basic_Proc.Put_Line_Output ("Created dir " & Argument.Get_Parameter);
  end if;

  loop

    Basic_Proc.Put_Line_Output ("PWD ->" & Directory.Get_Current & "<");

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
        Basic_Proc.Put_Output ("  ---->" & File_Name.Image & "< ");
        Basic_Proc.Put_Output (Pad(1 .. Max_Len - File_Name.Length));
        begin
          Fstat := Sys_Calls.File_Stat (Dir_Name.Image & '/' & File_Name.Image);
          Put_Id (Fstat.User_Id);
          Put_Id (Fstat.Group_Id);
          Put_Rights (Fstat.Rights);
          Put_Date (Fstat.Modif_Time);
          Basic_Proc.Put_Output (" " & Fstat.Kind'Img);
          if Fstat.Kind = Sys_Calls.File then
            Put_Size (Fstat.Size);
            Basic_Proc.New_Line_Output;
          elsif Fstat.Kind = Sys_Calls.Link then
            Basic_Proc.New_Line_Output;
            Basic_Proc.Put_Line_Output ("    ++++>" & Directory.Read_Link (
                Dir_Name.Image & '/' & File_Name.Image) & '<');
          else
            Basic_Proc.New_Line_Output;
          end if;
        exception
          when Directory.Name_Error =>
            Basic_Proc.Put_Line_Output ("???? ???");
          when Directory.Access_Error =>
            Basic_Proc.Put_Line_Output ("!!!! !!!");
          when Directory.Recursive_Link =>
            Basic_Proc.Put_Line_Output ("#### ###");
        end;
      end loop;
    exception
      when Directory.End_Error =>
        null;
    end;
    if Argument.Get_Nbre_Arg /= 0 then
      exit;
    end if;

    Basic_Proc.New_Line_Output;
    loop
      begin
        Directory.Change_Current (Get_New_Dir);
        exit;
      exception
        when Directory.Name_Error =>
          Basic_Proc.Put_Line_Output ("-> Not found.");
        when Directory.Access_Error =>
          Basic_Proc.Put_Line_Output ("-> Permission.");
      end;
    end loop;

  end loop;

  if Argument.Get_Nbre_Arg = 1 then
    Directory.Remove (Argument.Get_Parameter);
    Basic_Proc.Put_Line_Output ("Removed dir " & Argument.Get_Parameter);
  end if;
end T_Dir;

