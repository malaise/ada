with Ada.Calendar;

with Big_Con_Io, Normal, Lower_Str, Lower_Char, Upper_Char, Day_Mng;

with Pieces, Space.Board, Image;

package body Screen is

  package Con_Io renames Big_Con_Io;

  Screen_Row_Offset : constant Con_Io.Row_Range := 3;
  Screen_Col_Offset : constant Con_Io.Col_range := 5;

  Fore_White : constant Con_Io.Effective_Colors := Con_Io.Orange;
  Fore_Black : constant Con_Io.Effective_Colors := Con_Io.Red;
  Back_White : constant Con_Io.Effective_Basic_Colors := Con_Io.Cyan;
  Back_Black : constant Con_Io.Effective_Basic_Colors := Con_Io.Black;
  Main_Back  : constant Con_Io.Effective_Basic_Colors := Con_Io.Brown;


  -- Foreground color of messages, moves and for get
  function Fore (Color : Space.Color_List) return Con_Io.Effective_Colors is
    use type Space.Color_List;
  begin
    if Color = Space.White then
      return Fore_White;
    else
      return Back_Black;
    end if;
  end Fore;

  -- Movement history
  package Moves is
    procedure Put_Moves;
    procedure Add_Move (Color  : in Space.Color_List;
                        Action : in Game.Action_Rec;
                        Result : in Game.Move_Status_List);
  end Moves;
  package body Moves is separate;

  function To_Con_Io_Square (Color : in Space.Color_List;
                             Square : Space.Square_Coordinate)
                             return Con_Io.Square is
    use type Space.Color_List;
  begin
    if Color = Space.White then 
      return (Row => Space.Row_Range'Pos(Space.Row_Range'Last)
                   - Space.Row_Range'Pos(Square.Row)
                   + Screen_Row_Offset,
              Col => Space.Col_Range'Pos(Square.Col) + Screen_Col_Offset);
    else
      return (Row => Space.Row_Range'Pos(Square.Row) + Screen_Row_Offset - 1,
              Col => Space.Col_Range'Pos(Space.Col_Range'Last)
                   - Space.Col_Range'Pos(Square.Col)
                   + Screen_Col_Offset);
    end if;
  end To_Con_Io_Square;

  procedure Display_Square (Color : in Space.Color_List;
                            Square : in Space.Square_Coordinate) is
    Back : Con_Io.Effective_Basic_Colors;
    Fore : Con_Io.Effective_Colors;
    Id   : Pieces.Piece_Id;

    Char : Character;
    use type Space.Color_List, Pieces.Piece_Access, Pieces.Piece_Kind_List;
  begin
    -- get background
    if Space.Color_Of_Square(Square) = Space.White then
      Back := Back_White;
    else
      Back := Back_Black;
    end if;

    -- Set Foreground and character
    if Space.Board.Piece_At(Square) = null then
      -- Empty square
      Fore := Back;
      Char := ' ';
    else
      -- Set Foreground
      Id := Pieces.Id_Of(Space.Board.Piece_At(Square).all);
      if Id.Kind = Pieces.Pawn then
        Char := 'i';
      else
         Char := Image.Piece_Image(Id.Kind)(1);
      end if;

      if Id.Color = Space.White then
        Fore := Fore_White;
      else
        Fore := Fore_Black;
      end if;
    end if;

    -- Move and put 
    Con_Io.Move(To_Con_Io_Square(Color, Square));
    Con_Io.Put(Char,
               Foreground => Fore,
               Blink_Stat => Con_Io.Blink,
               Background => Back,
               Move => False);

  end Display_Square;


  -- Redisplay the board
  procedure Display_Board (Color : in Space.Color_List) is
    use type Space.Color_List;
    Con_Row : Con_Io.Row_Range;
    Con_Col : Con_Io.Col_Range;

    procedure Put (Row : in Space.Row_Range) is
    begin
      Con_Io.Put(Normal(Integer(Row), 1), Move => False);
    end Put;
    procedure Put (Col : in Space.Col_Range) is
    begin
      Con_Io.Put(Lower_Str(Space.Col_Range'Image(Col)), Move => False);
    end Put;

  begin
    Con_Io.Init;
    Con_Io.Set_Background (Main_Back);
    Con_Io.Set_Foreground (Con_Io.White);
    Con_Io.Clear;
    for Row in Space.Row_Range loop
      for Col in Space.Col_Range loop
        Display_Square (Color, (Col, Row) );
      end loop;
    end loop;
    for Row in Space.Row_Range loop
      if Color = Space.White then
        Con_Row := Space.Row_Range'Pos(Space.Row_Range'Last)
                 - Space.Row_Range'Pos(Row)
                 + Screen_Row_Offset;
      else
        Con_Row := Space.Row_Range'Pos(Row) + Screen_Row_Offset - 1;
      end if;
      Con_Io.Move(Con_Row, Screen_Col_Offset - 2);
      Put (Row);
      Con_Io.Move(Row => Con_Row,
                  Col => Screen_Col_Offset
                       + Space.Col_Range'Pos(Space.Col_Range'Last)
                       + 2);
      Put (Row);
    end loop;
    for Col in Space.Col_Range loop
      if Color = Space.White then
        Con_Col := Space.Col_Range'Pos(Col) + Screen_Col_Offset;
      else
        Con_Col := Space.Col_Range'Pos(Space.Col_Range'Last)
                 - Space.Col_Range'Pos(Col)
                 + Screen_Col_Offset;
      end if;
      Con_Io.Move(Row => Space.Row_Range'Pos(Space.Row_Range'Last)
                       + Screen_Row_Offset + 1,
                  Col => Con_Col);
      Put (Col);
      Con_Io.Move(Screen_Row_Offset - 2, Con_Col);
      Put (Col);
    end loop;
    Moves.Put_Moves;
    Con_Io.Flush;        
  end Display_Board;


  -- Update some squares of the board
  -- type Update_Array is array (Positive range <> of Space.Square_Coordinate);
  procedure Update_Board (Color : in Space.Color_List;
                          Squares : in Space.Square_Array) is
  begin
    for N in Squares'Range loop
      Display_Square(Color, Squares(N));
    end loop;
    Con_Io.Flush;        
  end Update_Board;

  -- Get, Ack, Wait
  procedure Erase is
    Erase_Str :  constant String (1 .. 80) := (others => ' ');
  begin
    Con_Io.Move (23, 1);
    Con_Io.Put (Erase_Str, Foreground => Main_Back);
  end Erase;

  Start_Time : Ada.Calendar.Time;

  procedure Put_Time (Color : Space.Color_List) is
    Hours   : Day_Mng.T_Hours := 0;
    Minutes : Day_Mng.T_Minutes := 0;
    Secs    : Day_Mng.T_Seconds := 0;
    Millisecs : Day_Mng.T_Millisec := 0;
    use Ada.Calendar;
  begin
    Con_Io.Move (2, 65);
    -- Not more than one day :-)
    begin
      Day_Mng.Split (Ada.Calendar.Clock-Start_Time, Hours, Minutes, Secs, Millisecs);
    exception
      when Constraint_Error | Ada.Calendar.Time_Error =>
        Start_Time := Ada.Calendar.Clock;
    end;
    Con_Io.Put (Normal(Hours, 2, Gap => '0') & 'h'
        & ' ' & Normal(Minutes, 2, Gap => '0') & 'm'
        & ' ' & Normal(Secs, 2, Gap => '0') & 's',
        Foreground => Fore (Color) );
  end Put_Time;


  procedure Erase_Time (Color : Space.Color_List) is
  begin
    Con_Io.Move (2, 65);
    Con_Io.Put ("               ", Foreground => Main_Back);
  end Erase_Time;

  procedure Reset_Time is
  begin
    Start_Time := Ada.Calendar.Clock;
  end Reset_Time;
  
  function Get (Color : Space.Color_List) return Players.Action_Rec is
    Str  : String (1 .. 5);
    Last : Natural;
    Stat : Con_Io.Curs_Mvt;
    Pos  : Positive;
    Ins  : Boolean;

    Conv_Ok : Boolean;
    From, To : Space.Square_Coordinate;
    Promo : Pieces.Piece_Kind_List;

    Action : Players.Action_Rec;

    use type Con_Io.Curs_Mvt, Space.Color_List;
  begin
    Str := (others => ' ');
    Ins := False;
    Pos := 1;

    Put_Time (Color);

    loop
      Con_Io.Move (23, 1);
      Con_Io.Put ("Move:", Foreground => Fore (Color));

      Con_Io.Move (23, 7);
      Con_Io.Put_Then_Get (Str, Last, Stat, Pos, Ins,
             Foreground => Fore (Color),
             Time_Out => (Con_Io.Delay_Sec, 0.1));
      Put_Time (Color);
      if Stat = Con_Io.Ret then
        if Str(1) = ' ' then
          Str(1 .. Str'Length-1) := Str(2 .. Str'Length);
          Str(Str'Length) := ' ';
        end if;

        if Lower_Str (Str) = "exit " then
          Erase;
          return (Valid => False);
        end if;
        begin
          From.Col := Space.Col_Range'Value(Str(1..1));
          From.Row := Space.Row_Range'Value(Str(2..2));
          To.Col := Space.Col_Range'Value(Str(3..3));
          To.Row := Space.Row_Range'Value(Str(4..4));
          Conv_Ok := True;
        exception
          when others =>
            Conv_Ok := False;
        end;
        if Conv_Ok then
          if Str(5) = ' ' then
            Promo := Pieces.Pawn;
            Conv_Ok := True;
          else
            Conv_Ok := False;
            for Piece in Pieces.Promotion_Piece_List loop
              if Upper_Char(Str(5)) = Image.Piece_Image(Piece)(1) then
                Promo := Piece;
                Conv_Ok := True;
                exit;
              end if;
            end loop;
          end if;
        end if;

        if Conv_Ok then
          Action := Players.Find_Action (Color, From, To, Promo);
          if Action.Valid then
            Erase;
            return Action;
          end if;
        end if;
      elsif Stat = Con_Io.Refresh then
        Display_Board (Color);
      elsif Stat = Con_Io.Break then
        Erase;
        return (Valid => False);
      end if;
    end loop;

  end Get;

  procedure Put (Color : in Space.Color_List;
                 Message : in String; Ack : in Boolean := False) is
    Str  : String (1 .. 0);
    Last : Natural;
    Stat : Con_Io.Curs_Mvt;
    Pos  : Positive;
    Ins  : Boolean;

    Timeout : Con_Io.Delay_Rec;

    use type Con_Io.Curs_Mvt, Space.Color_List;
  begin
    if Ack then
      Timeout := Con_Io.Infinite_Delay;
      Erase_Time (Color);
    else
      Timeout := (Con_Io.Delay_Sec, 1.0);
      Put_Time (Color);
    end if;
    Ins := False;

    loop
      Con_Io.Move (23, 1);
      Con_Io.Put (Message, Foreground => Fore (Color) );

      Pos := 1;
      Con_Io.Put_Then_Get (Str, Last, Stat, Pos, Ins,
             Time_Out => Timeout);
      if not Ack then
        Put_Time (Color);
      end if;
      if Stat = Con_Io.Ret or else Stat = Con_Io.Timeout then
        Erase;
        return;
      elsif Stat = Con_Io.Refresh then
        Display_Board (Color);
      elsif Stat = Con_Io.Break then
        Erase;
        return;
      end if;
    end loop;
  end Put;

  -- Wait a bit
  procedure Wait (Color : Space.Color_List; Delay_Ms : in Natural) is
    Str  : String (1 .. 0);
    Last : Natural;
    Stat : Con_Io.Curs_Mvt;
    Pos  : Positive;
    Ins  : Boolean;

    Timeout : Con_Io.Delay_Rec;

    use type Con_Io.Curs_Mvt, Space.Color_List;
  begin
    Timeout := (Con_Io.Delay_Sec, Duration (Delay_Ms) / 1000.0);
    Ins := False;
    Put_Time (Color);

    loop
      Con_Io.Move (23, 1);

      Pos := 1;
      Con_Io.Put_Then_Get (Str, Last, Stat, Pos, Ins,
             Time_Out => Timeout);
      Put_Time (Color);
      if Stat = Con_Io.Timeout or else Stat = Con_Io.Break then
        return;
      elsif Stat = Con_Io.Refresh then
        Display_Board (Color);
      end if;
    end loop;
  end Wait;

  procedure Put_Move (Color  : in Space.Color_List;
                      Action : in Game.Action_Rec;
                      Result : in Game.Move_Status_List) is
  begin
    Moves.Add_Move (Color, Action, Result);
    Moves.Put_Moves;
  end Put_Move;
    
  procedure Close is
  begin
    Con_Io.Destroy;
  end Close;

end Screen;

