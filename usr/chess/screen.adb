with Ada.Calendar;

with Big_Con_Io, Normal, Lower_Str, Lower_Char, Upper_Char, Day_Mng, Timers;

with Pieces, Space.Board, Image, Debug;

package body Screen is

  -- Text or Graphic
  -- Graphic_Mode : constant Boolean := False;
  Graphic_Mode : constant Boolean := True;

  package Con_Io renames Big_Con_Io;

  -- Are we waiting for promotion selection
  Getting_Promotion : Boolean := False;

  Fore_White : constant Con_Io.Effective_Colors := Con_Io.Orange;
  Fore_Black : constant Con_Io.Effective_Colors := Con_Io.Red;
  Back_White : constant Con_Io.Effective_Basic_Colors := Con_Io.Cyan;
  Back_Black : constant Con_Io.Effective_Basic_Colors := Con_Io.Black;

  Main_Fore : constant Con_Io.Effective_Colors := Con_Io.White;
  Main_Back : constant Con_Io.Effective_Basic_Colors := Con_Io.Brown;


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
                        Action : in Game.Valid_Action_Rec;
                        Result : in Game.Move_Status_List);
  end Moves;
  package body Moves is separate;

  -- Used by text and graphic internally
  type Square_Result_Rec (Valid : Boolean := False) is record
    case Valid is
      when True =>
        Square : Space.Square_Coordinate;
      when False =>
        null;
    end case;
  end record;

  -- Mouse event
  type Mouse_Event_List is (Discard, Release_Out, Click, Release);
  type Mouse_Event_Rec (Kind : Mouse_Event_List := Discard) is record
    case Kind is
      when Click | Release =>
        Square : Space.Square_Coordinate;
      when Discard | Release_Out =>
        null;
    end case;
  end record;

  package Text is
    procedure Init_Board (Color : in Space.Color_List);
    procedure Display_Square (Color : in Space.Color_List;
                              Square : in Space.Square_Coordinate);
    function Get_Mouse_Event (Color : Space.Color_List) return Mouse_Event_Rec;
    procedure Display_Promotion (Move_Color : in Space.Color_List);
    function Get_Promotion (Click : in Boolean) return Pieces.Piece_Kind_List;
  end Text;
  package body Text is separate;
  package Graphic is
    procedure Init_Board (Color : in Space.Color_List);
    procedure Display_Square (Color : in Space.Color_List;
                              Square : in Space.Square_Coordinate);
    function Get_Mouse_Event (Color : Space.Color_List) return Mouse_Event_Rec;
    procedure Display_Promotion (Move_Color : in Space.Color_List);
    function Get_Promotion (Click : in Boolean) return Pieces.Piece_Kind_List;
  end Graphic;
  package body Graphic is separate;
 
  procedure Init_Board (Color : in Space.Color_List) is
  begin
    if Graphic_Mode then
      Graphic.Init_Board (Color);
    else
      Text.Init_Board (Color);
    end if;
  end Init_Board;
  procedure Display_Square (Color : in Space.Color_List;
                            Square : in Space.Square_Coordinate) is
  begin
    if Graphic_Mode then
      Graphic.Display_Square (Color, Square);
    else
      Text.Display_Square (Color, Square);
    end if;
  end Display_Square;

  function Get_Mouse_Event (Color : Space.Color_List) return Mouse_Event_Rec is
  begin
    if Graphic_Mode then
      return Graphic.Get_Mouse_Event (Color);
    else
      return Text.Get_Mouse_Event (Color);
    end if;
  end Get_Mouse_Event;

  procedure Display_Promotion (Move_Color : in Space.Color_List) is
  begin
    if Graphic_Mode then
      Graphic.Display_Promotion (Move_Color);
    else
      Text.Display_Promotion (Move_Color);
    end if;
  end Display_Promotion;

  function Get_Promotion (Click : in Boolean) return Pieces.Piece_Kind_List is
  begin
    if Graphic_Mode then
      return Graphic.Get_Promotion (Click);
    else
      return Text.Get_Promotion (Click);
    end if;
  end Get_Promotion;


  -- Redisplay the board
  procedure Display_Board (Color : in Space.Color_List) is
  begin
    Con_Io.Init;
    Con_Io.Set_Background (Main_Back);
    Con_Io.Set_Foreground (Main_Fore);
    Con_Io.Clear;
    Init_Board (Color);
    for Row in Space.Row_Range loop
      for Col in Space.Col_Range loop
        Display_Square (Color, (Col, Row) );
      end loop;
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
  end Update_Board;


  -- Time
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
        Foreground => Fore (Color),
        Background => Main_Back );
  end Put_Time;


  procedure Erase_Time (Color : Space.Color_List) is
  begin
    Con_Io.Move (2, 65);
    Con_Io.Put ("               ", Foreground => Main_Back, Background => Main_Back);
  end Erase_Time;

  procedure Reset_Time is
  begin
    Start_Time := Ada.Calendar.Clock;
  end Reset_Time;
  
  -- Get, Ack, Wait
  procedure Erase is
    Erase_Str :  constant String (1 .. 50) := (others => ' ');
  begin
    Con_Io.Move (23, 1);
    Con_Io.Put (Erase_Str, Foreground => Main_Back);
  end Erase;

  -- Mouse 
  Clicked_Pos, Released_Pos : Square_Result_Rec;
  Clicked_Promo, Released_Promo : Pieces.Piece_Kind_List;
  procedure Manage_Mouse (Disp_Color : in Space.Color_List;
                          Move_Color : Space.Color_List;
                          From, To : out Square_Result_Rec;
                          Promo    : out Pieces.Piece_Kind_List) is
    Mouse_Event : Mouse_Event_Rec;
    Piece : Pieces.Piece_Access;

    use type Space.Row_Range, Space.Color_List;
    use type Pieces.Piece_Access, Pieces.Piece_Kind_List;
  begin
    From := (Valid => False);
    To := (Valid => False);
    Promo := Pieces.Pawn;

    if not Getting_Promotion then
      Mouse_Event := Get_Mouse_Event (Disp_Color);
      case Mouse_Event.Kind is
        when Click =>
          Clicked_Pos := (True, Mouse_Event.Square);
        when Release =>
          -- Check click is valid and there is a piece
          if not Clicked_Pos.Valid then
            return;
          end if;
          Piece :=  Space.Board.Piece_At(Clicked_Pos.Square);
          if Piece = null then
            Clicked_Pos := (Valid => False);
            return;
          end if;
          -- OK. Store release and check promotion
          Released_Pos := (True, Mouse_Event.Square);
          if Pieces.Id_Of(Piece.all).Kind = Pieces.Pawn
          and then ( (Move_Color = Space.White
                      and then Released_Pos.Square.Row = 8)
                    or else (Move_Color = Space.Black
                             and then Released_Pos.Square.Row = 1) ) then
            -- Put promotion
            Getting_Promotion := True;
            Display_Promotion (Move_Color);
            Clicked_Promo := Pieces.Pawn;
            Released_Promo := Pieces.Pawn;
          else
            -- Normal move
            From := Clicked_Pos;
            To := Released_Pos;
            return;
          end if;
        when Discard | Release_Out =>
          Clicked_Pos := (Valid => False);
      end case;
    else
      -- Promotion
      if Clicked_Promo = Pieces.Pawn then
        Clicked_Promo := Get_Promotion (True);
      else
        Released_Promo := Get_Promotion (False);
        if Clicked_Promo /= Pieces.Pawn and then Clicked_Promo = Released_Promo then
          Getting_Promotion := False;
          Display_Promotion (Move_Color);
          From := Clicked_Pos;
          To := Released_Pos;
          Promo := Clicked_Promo;
        else
          Clicked_Promo := Pieces.Pawn;
          Released_Promo := Pieces.Pawn;
        end if;
      end if;
    end if;
  end Manage_Mouse;



  function Get (Disp_Color : Space.Color_List;
                Move_Color : Space.Color_List) return Players.Action_Rec is
    Str  : String (1 .. 5);
    Last : Natural;
    Stat : Con_Io.Curs_Mvt;
    Pos  : Positive;
    Ins  : Boolean;

    Conv_Ok : Boolean;
    From, To : Space.Square_Coordinate;
    Mouse_From, Mouse_To : Square_Result_Rec;
    Promo : Pieces.Piece_Kind_List;

    Action : Players.Action_Rec;

    use type Con_Io.Curs_Mvt, Space.Color_List;
  begin
    Clicked_Pos := (Valid => False);
    Getting_Promotion := False;
    Display_Promotion (Move_Color);

    Str := (others => ' ');
    Ins := False;
    Pos := 1;

    Put_Time (Move_Color);

    loop
      Con_Io.Set_Foreground (Fore (Move_Color));
      Con_Io.Set_Background (Main_Back);
      Con_Io.Move (23, 1);
      Con_Io.Put ("Move:");

      Con_Io.Move (23, 7);
      Con_Io.Put_Then_Get (Str, Last, Stat, Pos, Ins,
             Time_Out => (Timers.Delay_Sec, Con_Io.No_Period, 0.1));
      Put_Time (Move_Color);
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
          Action := Players.Find_Action (Move_Color, From, To, Promo);
          if Action.Valid then
            Erase;
            Getting_Promotion := False;
            Display_Promotion (Move_Color);
            return Action;
          end if;
        end if;
      elsif Stat = Con_Io.Mouse_Button then
        Manage_Mouse (Disp_Color, Move_Color, Mouse_From, Mouse_To, Promo);
        if Mouse_From.Valid then
          Action := Players.Find_Action (Move_Color,
                        Mouse_From.Square, Mouse_To.Square, Promo);
          if Action.Valid then
            Erase;
            Getting_Promotion := False;
            Display_Promotion (Move_Color);
            return Action;
          end if;
        end if;
      elsif Stat = Con_Io.Refresh then
        Display_Board (Disp_Color);
        Display_Promotion (Move_Color);
      elsif Stat = Con_Io.Break then
        Erase;
        return (Valid => False);
      end if;
    end loop;

  end Get;

  procedure Put (Disp_Color : in Space.Color_List;
                 Move_Color : in Space.Color_List;
                 Message : in String; Ack : in Boolean := False) is
    Str  : String (1 .. 0);
    Last : Natural;
    Stat : Con_Io.Curs_Mvt;
    Pos  : Positive;
    Ins  : Boolean;

    Timer : Timers.Timer_Id;

    use type Con_Io.Curs_Mvt, Space.Color_List;
  begin
    if Ack then
      Erase_Time (Move_Color);
    else
      Put_Time (Move_Color);
      -- Start a single shot timer (no callback)
      Timer := Timers.Create ( (Timers.Delay_Sec, Con_Io.No_Period, 1.0), null);
    end if;
    Ins := False;

    loop
      Con_Io.Set_Background (Main_Back);
      Con_Io.Set_Foreground (Fore (Move_Color));
      Con_Io.Move (23, 1);
      Con_Io.Put (Message);

      Pos := 1;
      Con_Io.Put_Then_Get (Str, Last, Stat, Pos, Ins);
      if not Ack then
        Put_Time (Move_Color);
      end if;
      if Stat = Con_Io.Ret or else Stat = Con_Io.Timer_Event then
        Erase;
        exit;
      elsif Stat = Con_Io.Refresh then
        Display_Board (Disp_Color);
        Display_Promotion (Move_Color);
      elsif Stat = Con_Io.Break then
        Erase;
        exit;
      end if;
    end loop;
    begin
      Timers.Delete (Timer);
    exception
      when Timers.Invalid_Timer =>
        null;
    end;
  end Put;

  -- Wait until fd event
  procedure Wait_Event (Disp_Color : in Space.Color_List;
                        Move_Color : in Space.Color_List) is
    Str  : String (1 .. 0);
    Last : Natural;
    Stat : Con_Io.Curs_Mvt;
    Pos  : Positive;
    Ins  : Boolean;

    Timeout : Con_Io.Delay_Rec;

    use type Con_Io.Curs_Mvt, Space.Color_List;
  begin
    Timeout := (Timers.Delay_Sec, Con_Io.No_Period, 0.5);
    Ins := False;
    Put_Time (Move_Color);

    loop
      Con_Io.Set_Background (Main_Back);
      Con_Io.Set_Foreground (Main_Back);
      Con_Io.Move (23, 1);

      Pos := 1;
      Con_Io.Put_Then_Get (Str, Last, Stat, Pos, Ins,
             Time_Out => Timeout);
      Put_Time (Move_Color);
      if Stat = Con_Io.Fd_Event then
        return;
      elsif Stat = Con_Io.Refresh then
        Display_Board (Disp_Color);
        Display_Promotion (Move_Color);
      end if;
    end loop;
  end Wait_Event;

  -- Wait for a moment
  procedure Wait (Delay_Sec : in Duration) is
    Str  : String (1 .. 0);
    Last : Natural;
    Stat : Con_Io.Curs_Mvt;
    Pos  : Positive;
    Ins  : Boolean;

    Timeout : Con_Io.Delay_Rec;

    use type Con_Io.Curs_Mvt;
  begin
    Timeout := (Timers.Delay_Sec, Con_Io.No_Period, Delay_Sec);
    Ins := False;

    loop
      Con_Io.Set_Background (Main_Back);
      Con_Io.Set_Foreground (Main_Back);
      Con_Io.Move (23, 1);

      Pos := 1;
      Con_Io.Put_Then_Get (Str, Last, Stat, Pos, Ins,
             Time_Out => Timeout);
      if Stat = Con_Io.Timeout then
        return;
      end if;
    end loop;
  end Wait;

  procedure Put_Move (Color  : in Space.Color_List;
                      Action : in Game.Valid_Action_Rec;
                      Result : in Game.Move_Status_List) is
  begin
    Moves.Add_Move (Color, Action, Result);
    Moves.Put_Moves;
    Con_Io.Flush;
  end Put_Move;
    
  procedure Close is
  begin
    Con_Io.Destroy;
  end Close;

end Screen;

