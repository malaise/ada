with Ada.Text_Io;
with Text_Handler, Argument, Basic_Proc, Get_Line, Get_Float;
pragma Elaborate (Argument);
with Arg_Parsing;
package body File is

  package Cote_Get_Line is new Get_Line (
    Max_Word_Len => 20,
    Max_Word_Nb  => 4,
    Max_Line_Len => 80,
    Comment      => "#");

  Line : Cote_Get_Line.Line_Array;

  subtype Loc_Cote_Range is Natural range 0 .. Max_Cote;
  type Cote_Line_Array is array (Cote_Range) of Ada.Text_Io.Count;
  Cote_Line : Cote_Line_Array := (others => 0);

  Load_Ok : Boolean := False;
  Manufas : Manufa_Array(1 .. Max_Cote);
  Nb_Manufa : Loc_Cote_Range;
  Designs : Design_Array(1 .. Max_Cote);
  Nb_Design : Loc_Cote_Range;

  type Result_List is (Ok, End_Of_File, File_Access, File_Format, File_Length,
           Line_Nb, Start_Stop, Duplicate, No_Cote);

  procedure Read_Next_Significant_Line is
  begin
    Cote_Get_Line.Read_Next_Line;
    Cote_Get_Line.Get_Words (Line);
  end Read_Next_Significant_Line;

  procedure Error (Filename : in String; Result : in Result_List;
                   Cote_No : in Loc_Cote_Range := 0;
                   No2 : in Natural := 0) is
  begin
    if Result = Ok or else Result = End_Of_File then
      return;
    end if;
    Basic_Proc.Put_Error ( "Error in file " & Filename);
    if Cote_No /= 0 then
      Basic_Proc.Put_Line_Error (
       " at line " & Ada.Text_Io.Count'Image(Cote_Line(Cote_No)));
    else
      Basic_Proc.New_Line_Error;
    end if;
    case Result is
      when Ok | End_Of_File =>
        null;
      when File_Access =>
        Basic_Proc.Put_Line_Error ("File not found or not readable or empty.");
      when File_Format =>
        Basic_Proc.Put_Line_Error ("Wrong format.");
      when File_Length =>
        Basic_Proc.Put_Line_Error ("File too long.");
      when Line_Nb =>
        Basic_Proc.Put_Line_Error ("Invalid line number.");
      when Start_Stop =>
        Basic_Proc.Put_Line_Error ("Start equal stop.");
      when Duplicate =>
        Basic_Proc.Put_Line_Error ("Cote already exists at line "
          & Ada.Text_Io.Count'Image(Cote_Line(No2)));
      when No_Cote =>
        Basic_Proc.Put_Line_Error ("Line " & Natural'Image(No2) & " has no cote");
    end case;
    raise Load_Error;
  end Error;

  procedure Open (Filename : in String) is
  begin
    Cote_Get_Line.Open (Filename);
    Cote_Get_Line.Get_Words (Line);
  exception
    when others =>
      Error (Filename, File_Access);
  end Open;

  procedure Load_Cote (Kind : in Cote_Kind;
                       Nb_Cote : in out Loc_Cote_Range;
                       Cote : in out Cote_Rec;
                       Result : out Result_List) is
  begin
    -- Except for first (done in open) get new line and parse
    if Nb_Cote /= 0 then
      begin
        Read_Next_Significant_Line;
      exception
        when Cote_Get_Line.No_More_Line =>
          Result := End_Of_File;
          return;
      end;
      Cote_Get_Line.Get_Words (Line);
    end if;
    -- Got a new cote: Check number of cotes
    if Nb_Cote = Max_Cote then
      Result := File_Length;
      return;
    end if;
    Nb_Cote := Nb_Cote + 1;
    Cote_Line(Nb_Cote) := Cote_Get_Line.Get_Line_No;
    -- Got a new cote: Check number of words
    if Kind = Manufa then
      if Cote_Get_Line.Get_Word_Number /= 3 then
        Result := File_Format;
        return;
      end if;
    else
      if Cote_Get_Line.Get_Word_Number /= 4 then
        Result := File_Format;
        return;
      end if;
    end if;
    -- Parse line
    Cote.Start := Line_Range'Value(Text_Handler.Value(Line(1)));
    Cote.Stop  := Line_Range'Value(Text_Handler.Value(Line(2)));
    if Kind = Design then
      Cote.Value := Get_Float.Get_Float(Text_Handler.Value(Line(3)));
      Cote.Inter := Get_Float.Get_Float(Text_Handler.Value(Line(4)));
    else
      Cote.Inter := Get_Float.Get_Float(Text_Handler.Value(Line(3)));
    end if;
    -- Error if start=stop. Set start < stop
    if Cote.Start = Cote.Stop then
      Result := Start_Stop;
      return;
    elsif Cote.Start > Cote.Stop then
      declare
        Tmp : Cote_Range;
      begin
        Tmp := Cote.Start;
        Cote.Start := Cote.Stop;
        Cote.Stop := Tmp;
      end;
    end if;

    Result := Ok;
  exception
    when others =>
      Result := File_Format;
  end Load_Cote;

  -- Load both sets of cotes from two files
  -- And checks
  procedure Load_Cotes is
    Line_Usage : array (Line_Range) of Boolean;
    Result : Result_List;
    Manufa_Cote : Manufa_Cote_Rec;
    Design_Cote : Design_Cote_Rec;

    function Are_Dup (C1, C2 : in Cote_Rec) return Boolean is
    begin
      return  C1.Start = C2.Start and then C1.Stop = C2.Stop;
    end Are_Dup;
  begin
    begin
      Arg_Parsing.Check;
    exception
      when others =>
        Basic_Proc.Put_Line_Error ("Error. Usage: "
          & Argument.Get_Program_Name
          & " [ -v ] <manufacturing_file> <specification_file>");
        raise Load_Error;
    end;
    -- Load and check Manufas
    Nb_Manufa := 0;
    Open (Arg_Parsing.Manufa_File_Name);
    loop
      Load_Cote (Manufa, Nb_Manufa, Manufa_Cote, Result);
      exit when Result = End_Of_File;
      Error (Arg_Parsing.Manufa_File_Name, Result, Nb_Manufa);
      Manufas(Nb_Manufa) := Manufa_Cote;
    end loop;
    Cote_Get_Line.Close;

    -- Load and check Designs
    Nb_Design := 0;
    Open (Arg_Parsing.Design_File_Name);
    loop
      Load_Cote (Design, Nb_Design, Design_Cote, Result);
      exit when Result = End_Of_File;
      Error (Arg_Parsing.Design_File_Name, Result, Nb_Design);
      Designs(Nb_Design) := Design_Cote;
    end loop;
    Cote_Get_Line.Close;

    -- Check same number of cotes
    if Nb_Manufa /= Nb_Design then
      Basic_Proc.Put_Line_Error ( "Error: files " & Arg_Parsing.Manufa_File_Name
                           & " and " & Arg_Parsing.Design_File_Name
                           & " don't have the same number of cotes.");
      raise Load_Error;
    end if;

    -- Check duplicates
    for I in 2 .. Nb_Manufa loop
      for J in 1 .. I - 1 loop
        if Are_Dup (Manufas(I), Manufas(J)) then
          Error (Arg_Parsing.Manufa_File_Name, Duplicate, I, J);
        end if;
      end loop;
    end loop;

    -- Check duplicates
    for I in 2 .. Nb_Design loop
      for J in 1 .. I - 1 loop
        if Are_Dup (Designs(I), Designs(J)) then
          Error (Arg_Parsing.Design_File_Name, Duplicate, I, J);
        end if;
      end loop;
    end loop;

    -- Check line nos and line usage
    Line_Usage := (others => False);
    for I in 1 .. Nb_Manufa loop
      if      Manufas(I).Start > Nb_Manufa + 1
      or else Manufas(I).Stop  > Nb_Manufa + 1 then
        Error (Arg_Parsing.Manufa_File_Name, Line_Nb, I);
      end if;
      Line_Usage(Manufas(I).Start) := True;
      Line_Usage(Manufas(I).Stop)  := True;
    end loop;
    for I in 1 .. Nb_Manufa + 1 loop
      if not Line_Usage(I) then
        Error (Arg_Parsing.Manufa_File_Name, No_Cote, 0, I);
      end if;
    end loop;

    -- Check line nos and line usage
    Line_Usage := (others => False);
    for I in 1 .. Nb_Design loop
      if      Designs(I).Start > Nb_Design + 1
      or else Designs(I).Stop  > Nb_Design + 1 then
        Error (Arg_Parsing.Design_File_Name, Line_Nb, I);
      end if;
      Line_Usage(Designs(I).Start) := True;
      Line_Usage(Designs(I).Stop)  := True;
    end loop;
    for I in 1 .. Nb_Design + 1 loop
      if not Line_Usage(I) then
        Error (Arg_Parsing.Design_File_Name, No_Cote, 0, I);
      end if;
    end loop;

    Load_Ok := True;
  end Load_Cotes;

  function Get_Nb_Cote return Cote_Range is
  begin
    if not Load_Ok then
      raise Load_Error;
    end if;
    return Nb_Manufa;
  end Get_Nb_Cote;

  function Get_Manufa return Manufa_Array is
  begin
    if not Load_Ok then
      raise Load_Error;
    end if;
    return Manufas(1 .. Nb_Manufa);
  end Get_Manufa;

  function Get_Design return Design_Array is
  begin
    if not Load_Ok then
      raise Load_Error;
    end if;
    return Designs(1 .. Nb_Design);
  end Get_Design;

begin
  Load_Cotes;
end File;

