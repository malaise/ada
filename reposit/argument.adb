with Ada.Command_Line, Ada.Characters.Latin_1;
with Str_Util;
package body Argument is

  Key_Prefix : constant Character := '-';

  Path_Separator : constant Character := '/';

  function Not_Key return String is
  begin
    return "" & Ada.Characters.Latin_1.Del;
  end Not_Key;

  function Any_Arg return String is
  begin
    return "" & Ada.Characters.Latin_1.Nul;
  end Any_Arg;

  function Any_Key return String is
  begin
    return " ";
  end Any_Key;

  package Loc_Arg is
    -- Return the number of arguments of current program (0 if no argument)
    function Count return Natural;
    -- Return the Nth argument of current program (program name if Pos = 0)
    function Data (Pos : Natural) return String;
  end Loc_Arg;

  package body Loc_Arg is
    -- Return the number of arguments of current program (0 if no argument)
    function Count return Natural is
    begin
      return Ada.Command_Line.Argument_Count;
    end Count;

    -- Return the Nth argument of current program (program name if Pos = 0)
    function Data (Pos : Natural) return String is
    begin
      if Pos = 0 then
        return Ada.Command_Line.Command_Name;
      else
        return Ada.Command_Line.Argument(Pos);
      end if;
    end Data;
  end Loc_Arg;

  use Loc_Arg;

  -- The common "heart" procedure
  procedure Get_Param_And_Pos (
   Parameter : out As.U.Asu_Us;
   Position : out Natural;
   Occurence : in Natural := 1;
   Param_Key : in String := Any_Arg) is

    Comform_Occurence : Natural := 0;
    First_Char : Positive;
  begin
    -- Init result for case of error
    Position := 0;

    -- test if occurence is 0
    if Occurence = 0 then
      if Param_Key /= Any_Arg and then Param_Key /= Not_Key then
        raise Argument_Not_Found;
      end if;
      -- affect string
      Parameter := As.U.Tus (Data(0));
      Position := 0;
      return;
    end if;

    -- Compute 1st char of argument to return
    if Param_Key=Any_Arg or else Param_Key=Not_Key then
      First_Char := 1;
    elsif Param_Key = Any_Key then
      -- any key : start after '-'
      First_Char := 2;
    else
      -- specific key : start after -<key>
      First_Char := Param_Key'Length + 2;
    end if;

    -- analyse arguments of command line,
    for I in 1 .. Count loop
      -- Check if argument conforms
      if Param_Key = Any_Arg then
        -- any parameter comforms
        Comform_Occurence := Comform_Occurence + 1;
      elsif Param_Key = Not_Key and then Data(I)(1) /= Key_Prefix then
        -- any parameter not preceeded by separator comforms
        Comform_Occurence := Comform_Occurence + 1;
      elsif Param_Key = Any_Key and then Data(I)(1) = Key_Prefix then
        -- any parameter preceeded by separator comforms
        Comform_Occurence := Comform_Occurence + 1;
      elsif Data(I)(1) = Key_Prefix
       and then Data(I)'Length >= Param_Key'Length + 1
       and then Data(I)(2 .. Param_Key'Length+1) = Param_Key then
        -- Check that first char is Prefix
        -- and that length of parameter is >= than '-'<key>
        -- and that argument after '-' matches Param_Key
        Comform_Occurence := Comform_Occurence + 1;
      end if;

      if Comform_Occurence = Occurence then
        -- Comforming occurence is found. Affect string (First_Char..Len)
        Parameter := As.U.Tus (Data(I)(First_Char .. Data(I)'Length));
        Position := I;
        return;
      end if;
      -- Next argument
    end loop;
    raise Argument_Not_Found;
  exception
    -- Propagate the cause of problem.
    when Argument_Not_Found =>
      raise;
    when others =>
      -- In case of other problem : not found.
      raise Argument_Not_Found;
  end Get_Param_And_Pos;


  function Get_Parameter (
   Occurence : in Natural := 1;
   Param_Key : in String := Any_Arg) return String is
    Str : As.U.Asu_Us;
    Position : Natural;
  begin
    Get_Param_And_Pos (Str, Position, Occurence, Param_Key);
    return Str.Image;
  end Get_Parameter;

  procedure Get_Parameter (
   Parameter : out String;
   Param_Length : out Natural;
   Occurence : in Natural := 1;
   Param_Key : in String := Any_Arg) is
    Position : Natural;
  begin
    Get_Param_And_Pos (Parameter, Param_Length, Position, Occurence, Param_Key);
  end Get_Parameter;

  procedure Get_Parameter (
   Parameter : out As.U.Asu_Us;
   Occurence : in Natural := 1;
   Param_Key : in String := Any_Arg) is
    Position : Natural;
  begin
    Get_Param_And_Pos (Parameter, Position, Occurence, Param_Key);
  end Get_Parameter;


  procedure Get_Param_And_Pos (
   Parameter : out String;
   Param_Length : out Natural;
   Position : out Natural;
   Occurence : in Natural := 1;
   Param_Key : in String := Any_Arg) is
    Str : As.U.Asu_Us;
  begin
    Get_Param_And_Pos (Str, Position, Occurence, Param_Key);
    if Parameter'Length < Str.Length then
      raise Argument_Too_Long;
    end if;
    Str_Util.Copy (Str.Image, Parameter);
    Param_Length := Str.Length;
  end Get_Param_And_Pos;


  function Is_Set (
   Occurence : in Natural := 1;
   Param_Key : in String := Any_Arg) return Boolean is
    Str : As.U.Asu_Us;
    Pos : Natural;
  begin
    if Occurence = 0
    and then (Param_Key = Any_Arg or else Param_Key = Not_Key) then
      return True;
    end if;
    Get_Param_And_Pos (Str, Pos, Occurence, Param_Key);
    return True;
  exception
    when Argument_Not_Found =>
      return False;
  end Is_Set;

 function Get_Position (
   Occurence : in Natural := 1;
   Param_Key : in String := Any_Arg) return Natural is
    Str : As.U.Asu_Us;
    Pos : Natural;
  begin
    Get_Param_And_Pos (Str, Pos, Occurence, Param_Key);
    return Pos;
  end Get_Position;

  function Get_Nbre_Arg return Natural is
  begin
    return Count;
  end Get_Nbre_Arg;

  -- Analyse of argument(0)

  -- Path of program from Arg(0) (with last /)
  function Last_Delimiter (Path_Prog : String) return Natural is
  begin
    for I in reverse Path_Prog'Range loop
      if Path_Prog(I) = Path_Separator then
        return I;
      end if;
    end loop;
    return 0;
  end Last_Delimiter;


  function Get_Program_Path return String is
    Str : As.U.Asu_Us;
    Pos : Natural;
    Len : Natural;
  begin
    -- Program path and name
    Get_Param_And_Pos (Str, Pos, 0, Any_Arg);
    Len := Last_Delimiter (Str.Image);
    return Str.Slice (1, Len);
  end Get_Program_Path;

  procedure Get_Program_Path (Path : out String; Path_Length : out Natural) is
    Str : As.U.Asu_Us;
    Pos : Natural;
    Len : Natural;
  begin
    Get_Param_And_Pos (Str, Pos, 0, Any_Arg);
    Len := Last_Delimiter (Str.Image);
    Str_Util.Copy (Str.Slice (1, Len), Path);
    Path_Length := Len;
  end Get_Program_Path;

  -- Name of program from Argument(0)
  function Get_Program_Name return String is
    Str : As.U.Asu_Us;
    Pos : Natural;
    Len : Natural;
    Start : Natural;
  begin
    -- Program name
    Get_Param_And_Pos (Str, Pos, 0, Any_Arg);
    Len := Str.Length;
    Start := Last_Delimiter(Str.Image) + 1;

    return Str.Slice (Start, Len);
  end Get_Program_Name;

  procedure Get_Program_Name (Name : out String;
                              Name_Length : out Natural) is
    Str : As.U.Asu_Us;
    Pos : Natural;
    Len : Natural;
    Start : Natural;
  begin
    Get_Param_And_Pos (Str, Pos, 0, Any_Arg);
    Len := Str.Length;
    Start := Last_Delimiter(Str.Image) + 1;

    Str_Util.Copy (Str.Slice (Start, Len), Name);
    Name_Length := Len - Start + 1;
  end Get_Program_Name;


end Argument;

