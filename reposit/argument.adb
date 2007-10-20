with Ada.Characters.Latin_1;
with Loc_Arg, String_Mng;
use Loc_Arg;
package body Argument is

  Key_Prefix : constant Character := '-';

  Path_Separator : constant Character := '/';

  Str : String (1 .. Max_Len_Arg);

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


  function Get_Parameter (
   Occurence : in Natural := 1;
   Param_Key : in String := Any_Arg) return String is
    Len : Natural;
  begin
    Get_Parameter (Str, Len, Occurence, Param_Key);
    return Str (1 .. Len);
  end Get_Parameter;

  procedure Get_Parameter (
   Parameter : out String;
   Param_Length : out Natural;
   Occurence : in Natural := 1;
   Param_Key : in String := Any_Arg) is
    Position : Natural;
  begin
    Get_Param_And_Pos (Parameter, Param_Length, Position,
     Occurence, Param_Key);
  end Get_Parameter;

  procedure Get_Parameter (
   Parameter : in out Text_Handler.Text;
   Occurence : in Natural := 1;
   Param_Key : in String := Any_Arg) is
    Position : Natural;
  begin
    Get_Param_And_Pos (Parameter, Position,
     Occurence, Param_Key);
  end;



  procedure Get_Param_And_Pos (
   Parameter : out String;
   Param_Length : out Natural;
   Position : out Natural;
   Occurence : in Natural := 1;
   Param_Key : in String := Any_Arg) is

    Comform_Occurence : Natural := 0;
    In_Occurence : Natural := Occurence;
    First_Char : Positive;
  begin
    -- Init result for case of error
    Param_Length := 0;
    Position := 0;

    -- test if occurence is 0
    if In_Occurence=0 then
      if Param_Key /= Any_Arg and then Param_Key /= Not_Key then
        raise Argument_Not_Found;
      end if;
      if Parameter'Length < Data(0)'Length then
        raise Argument_Too_Long;
      end if;
      -- affect string
      Param_Length := Data(0)'Length;
      String_Mng.Copy (Data(0), Parameter);
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

      if Comform_Occurence = In_Occurence then
        -- Comforming occurence is found. Check length
        if Parameter'Length < Data(I)'Length - (First_Char-1) then
          raise Argument_Too_Long;
        else
          -- Affect string (First_Char..Len)
          Param_Length := Data(I)'Length - (First_Char-1);
          String_Mng.Copy (Data(I)(First_Char .. Data(I)'Length), Parameter);
          Position := I;
          return;
        end if;
      end if;
      -- Next argument
    end loop;
    raise Argument_Not_Found;
  exception
    -- Propagate the cause of problem.
    when Argument_Too_Long | Argument_Not_Found =>
      raise;
    when others =>
      -- In case of other problem : not found.
      raise Argument_Not_Found;
  end Get_Param_And_Pos;

  procedure Get_Param_And_Pos (
   Parameter : in out Text_Handler.Text;
   Position : out Natural;
   Occurence : in Natural := 1;
   Param_Key : in String := Any_Arg) is
    Str : String (1..Parameter.Max_Len);
    Len : Natural;
  begin
    Get_Param_And_Pos (Str, Len, Position, Occurence, Param_Key);
    Text_Handler.Set (Parameter, Str(1 .. Len));
  end Get_Param_And_Pos;

  function Is_Set (
   Occurence : in Natural := 1;
   Param_Key : in String := Any_Arg) return Boolean is
    Len : Natural;
    Pos : Natural;
  begin
    if Occurence = 0
    and then (Param_Key = Any_Arg or else Param_Key = Not_Key) then
      return True;
    end if;
    Get_Param_And_Pos (Str, Len, Pos, Occurence, Param_Key);
    return True;
  exception
    when Argument_Not_Found =>
      return False;
  end Is_Set;

 function Get_Position (
   Occurence : in Natural := 1;
   Param_Key : in String := Any_Arg) return Natural is
    Len : Natural;
    Pos : Natural;
  begin
    Get_Param_And_Pos (Str, Len, Pos, Occurence, Param_Key);
    return Pos;
  end Get_Position;

  function Get_Nbre_Arg return Natural is
  begin
    return Count;
  end Get_Nbre_Arg;

  -- Analyse of argument(0)

  -- Path of program from Arg(0) (wirh last /)
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
    Len : Natural;
  begin
    -- Program path
    Get_Program_Path (Str, Len);
    return Str (1 .. Len);
  end Get_Program_Path;

  procedure Get_Program_Path (Path : out String; Path_Length : out Natural) is
    Len : Natural;
  begin
    -- Program path and name
    Get_Parameter (Str, Len, 0, Any_Arg);
    Len := Last_Delimiter(Str (1 .. Len));

    Path_Length := Len;
    String_Mng.Copy (Str(1 .. Len), Path);
  end Get_Program_Path;

  procedure Get_Program_Path (Path : in out Text_Handler.Text) is
  begin
    Text_Handler.Set (Path, Get_Program_Path);
  end Get_Program_Path;

  -- Name of program from Argument(0)
  function Get_Program_Name return String is
    Len : Natural;
  begin
    -- Program name
    Get_Program_Name (Str, Len);
    return Str (Str'First .. Len);
  end Get_Program_Name;

  procedure Get_Program_Name (Name : out String;
                              Name_Length : out Natural) is
    Len : Natural;
    Start : Natural;
  begin
    -- Program path and name
    Get_Parameter (Str, Len, 0, Any_Arg);
    Start := Last_Delimiter(Str (1 .. Len)) + 1;

    Name_Length := Len - Start + 1;
    String_Mng.Copy (Str(Start .. Len), Name);
  end Get_Program_Name;


  procedure Get_Program_Name (Name : in out Text_Handler.Text) is
  begin
    Text_Handler.Set (Name, Get_Program_Name);
  end Get_Program_Name;

end Argument;

