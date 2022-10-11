with Ada.Command_Line, Ada.Text_Io;
with Olc;
procedure T_Olc is

  procedure Help is
  begin
    Ada.Text_Io.Put_Line (
        "Usage: " & Ada.Command_Line.Command_Name & " <command>");
    Ada.Text_Io.Put_Line (
        "  <commnd> ::= <encode> | <decode> | <center> | <status> |"
                    & " <shorten> | <nearest> | <help>");
    Ada.Text_Io.Put_Line ("  <encode>  ::= -c <coord> <precision>");
    Ada.Text_Io.Put_Line ("  <coord>   ::= <lat> <lon>");
    Ada.Text_Io.Put_Line ("  <decode>  ::= -d <code>");
    Ada.Text_Io.Put_Line ("  <center>  ::= -C <coord> <coord>");
    Ada.Text_Io.Put_Line ("  <status>  ::= -S <code>");
    Ada.Text_Io.Put_Line ("  <shorten> ::= -s <code> <coord>");
    Ada.Text_Io.Put_Line ("  <nearest> ::= -n <code> <coord>");
    Ada.Text_Io.Put_Line ("  <help>    ::= -h");
  end Help;

  function Positive_Image (P : Integer) return String is
    Str : constant String := P'Img;
  begin
    return Str(2 .. Str'Last);
  end Positive_Image;

  procedure Encode (C : in Olc.Coordinate;
                    P : in Olc.Precision_Range) is
    Code : constant String := Olc.Encode (C, P);
  begin
    Ada.Text_Io.Put_Line (Code);
  end Encode;

  procedure Decode (C : Olc.Code_Type) is
    Se, Nw : Olc.Coordinate;
  begin
    Olc.Decode (C, Se, Nw);
    Ada.Text_Io.Put_Line (Se.Lat'Img & " " & Se.Lon'Img
                        & " " & Nw.Lat'Img & " " & Nw.Lon'Img);
  end Decode;

  procedure Center (C : in Olc.Code_Type) is
    Se, Nw, Ce: Olc.Coordinate;
  begin
    Olc.Decode (C, Se, Nw);
    Ce := Olc.Center_Of (Se, Nw);
    Ada.Text_Io.Put_Line (Ce.Lat'Img & " " & Ce.Lon'Img);
  end Center;

  procedure Status (C : in Olc.Code_Type) is
  begin
    if not Olc.Is_Valid (C) then
      Ada.Text_Io.Put_Line ("Invalid");
      return;
    end if;
    Ada.Text_Io.Put (Positive_Image (Olc.Precision_Of (C)) & " ");
    if Olc.Is_Short (C) then
      Ada.Text_Io.Put_Line ("Short");
      return;
    end if;
    if Olc.Is_Full (C) then
      Ada.Text_Io.Put_Line ("Full");
    else
      Ada.Text_Io.Put_Line ("Not_full");
    end if;
  end Status;

  procedure Shorten (Code : in Olc.Code_Type;
                     Coord : in Olc.Coordinate) is
    Short : constant String := Olc.Shorten (Code, Coord);
  begin
    Ada.Text_Io.Put_Line (Short);
  end Shorten;

  procedure Nearest (Code : in Olc.Code_Type;
                     Coord : in Olc.Coordinate) is
    Full : constant String := Olc.Nearest (Code, Coord);
  begin
    Ada.Text_Io.Put_Line (Full);
  end Nearest;

begin
  if Ada.Command_Line.Argument (1) = "-c" then
    Encode (
        C => (Lat => Olc.Real'Value (Ada.Command_Line.Argument (2)),
              Lon => Olc.Real'Value (Ada.Command_Line.Argument (3))),
        P => Olc.Precision_Range'Value (Ada.Command_Line.Argument (4)));
  elsif Ada.Command_Line.Argument (1) = "-d" then
    Decode (Ada.Command_Line.Argument (2));
  elsif Ada.Command_Line.Argument (1) = "-C" then
    Center (Ada.Command_Line.Argument (2));
  elsif Ada.Command_Line.Argument (1) = "-S" then
    Status (Ada.Command_Line.Argument (2));
  elsif Ada.Command_Line.Argument (1) = "-s" then
    Shorten (
        Ada.Command_Line.Argument (2),
        (Lat => Olc.Real'Value (Ada.Command_Line.Argument (3)),
         Lon => Olc.Real'Value (Ada.Command_Line.Argument (4))));
  elsif Ada.Command_Line.Argument (1) = "-n" then
    Nearest (
        Ada.Command_Line.Argument (2),
        (Lat => Olc.Real'Value (Ada.Command_Line.Argument (3)),
         Lon => Olc.Real'Value (Ada.Command_Line.Argument (4))));
  elsif Ada.Command_Line.Argument (1) = "-h" then
    Help;
  else
    raise Constraint_Error;
  end if;
exception
  when others =>
    Ada.Text_Io.Put_Line ("ERROR: Invalid argument");
    Help;
    Ada.Command_Line.Set_Exit_Status (1);
end T_Olc;

