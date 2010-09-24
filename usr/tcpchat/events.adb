with Ada.Calendar;
with As.U; use As.U;
with Basic_Proc, Regular_Expressions, Command, Date_Image;
with Tree, Ios;
package body Events is

  -- Report progress
  procedure Put_Line (Str : in String) is
  begin
    Basic_Proc.Put_Line_Output (Date_Image (Ada.Calendar.Clock)
                              & " => " & Str & ".");
  end Put_Line;

  -- Output flow of called command
  Flow : aliased Command.Flow_Rec(Command.Str);

  -- Are we in a chat, if yes then the timeout of root (chats) select
  --  applies. If not, no need to reset connection when no chat selected
  In_Chat : Boolean := False;

  -- Reset connection and move on root
  procedure Reset is
  begin
    In_Chat := False;
    Ios.Reset;
    Tree.Chats.Move_Root;
  end Reset;

  -- Handle events
  procedure Handle is
    use Tree;
    Node, Child : Node_Rec;
    Timeout : Integer;
    Next : Position_Access;
    Event : Ios.Event_Type;
    Disconnection : Boolean;
    use type Ios.Event_Kind_List;
  begin
    loop
      -- Where are we?
      Node := Chats.Read;
      case Node.Kind is
        when Selec =>
          -- If we are root
          --  Cancel previous chat timeout if we are in chat
          --  No select timeout if we are not in chat (no useless reset)
          Timeout := Node.Timeout;
          if not Chats.Has_Father then
            if In_Chat then
              Ios.Stop_Global_Timer;
            else
              Timeout := Infinite_Ms;
            end if;
          end if;
          -- Read a sentence
          Event := Ios.Read (Timeout);
          case Event.Kind is
            when Ios.Exit_Requested =>
              Put_Line ("Exit requested");
              return;
            when Ios.Disconnection =>
              if In_Chat then
                Put_Line ("Disconnection");
              end if;
              Reset;
            when Ios.Global_Timeout =>
              Put_Line ("Timeout on chat script");
              Reset;
            when Ios.Local_Timeout =>
              Put_Line ("Timeout on select");
              Reset;
            when Ios.Got_Sentence =>
              -- Dispatch to child, avoid Next
              Next := Node.Next.all;
              Children:
              for I in 1 .. Chats.Children_Number loop
                if I = 1 then
                  Chats.Move_Child;
                else
                  Chats.Move_Brother (False);
                end if;
                Child := Chats.Read;
                if Child.Next.all = Next then
                  -- Last child is Next => no match
                  Put_Line ("No match on select");
                  Reset;
                  exit Children;
                elsif Child.Kind = Default
                or else (Child.Kind = Read and then
                         Regular_Expressions.Match (
                           Criteria => Asu_Ts (Child.Text),
                           Str      => Asu_Ts (Event.Sentence),
                           Strict   => True) ) then
                  -- This read child matches (or is default)
                  if Child.Kind = Read
                  and then not Asu_Is_Null (Child.Text) then
                    -- This is the start of a new chat
                    Put_Line ("Starting chat " & Asu_Ts (Child.Name));
                    Ios.Start_Global_Timer (Child.Timeout);
                  end if;
                  -- Move to the child of this select entry
                  Set_Position (Child.Next.all);
                  In_Chat := True;
                  exit Children;
                elsif not Chats.Has_Brother (False) then
                  -- No more child
                  Put_Line ("No match on select");
                  Reset;
                  exit Children;
                end if;
              end loop Children;
          end case;

        when Read =>
          Event := Ios.Read (Node.Timeout);
          case Event.Kind is
            when Ios.Exit_Requested =>
              Put_Line ("Exit requested");
              return;
            when Ios.Disconnection =>
              Put_Line ("Disconnection");
              Reset;
            when Ios.Global_Timeout =>
              Put_Line ("Timeout on chat script");
              Reset;
            when Ios.Local_Timeout =>
              Put_Line ("Timeout on Read");
              Reset;
            when Ios.Got_Sentence =>
              -- Check match
              if Regular_Expressions.Match (
                   Criteria => Asu_Ts (Node.Text),
                   Str      => Asu_Ts (Event.Sentence),
                   Strict   => True) then
                Set_Position (Node.Next.all);
              else
                Put_Line ("Read mismatch");
                Reset;
              end if;
          end case;

        when Default =>
          -- Should not occur
          null;

        when Skip =>
          Event := Ios.Read (Node.Timeout);
          case Event.Kind is
            when Ios.Exit_Requested =>
              Put_Line ("Exit requested");
              return;
            when Ios.Disconnection =>
              Put_Line ("Disconnection");
              Reset;
            when Ios.Global_Timeout =>
              Put_Line ("Timeout on chat script");
              Reset;
            when Ios.Local_Timeout =>
              Put_Line ("Timeout on Read");
              Reset;
            when Ios.Got_Sentence =>
              -- Skip
              Set_Position (Node.Next.all);
          end case;

        when Wait =>
          Event := Ios.Wait (Node.Timeout);
          case Event.Kind is
            when Ios.Exit_Requested =>
              Put_Line ("Exit requested");
              return;
            when Ios.Disconnection =>
              Put_Line ("Disconnection");
              Reset;
            when Ios.Global_Timeout =>
              Put_Line ("Timeout on chat script");
              Reset;
            when Ios.Local_Timeout =>
              Set_Position (Node.Next.all);
            when Ios.Got_Sentence =>
              -- Should not occur
              null;
          end case;

        when Send =>
          Ios.Send (Node.Text, Disconnection);
          if Disconnection then
            Put_Line ("Disconnection");
            Reset;
          else
            Set_Position (Node.Next.all);
          end if;

        when Call =>
          declare
            Exit_Code : Command.Exit_Code_Range;
          begin
            Command.Execute (
              Cmd => Asu_Ts (Node.Text),
              Use_Sh => True,
              Mix_Policy => Command.Only_Out,
              Out_Flow => Flow'Access,
              Err_Flow => null,
              Exit_Code => Exit_Code);
            if Exit_Code = 0 then
              -- Command OK, send its out flow
              Ios.Send (Flow.Str, Disconnection);
              if Disconnection then
                Put_Line ("Disconnection");
                Reset;
              else
                -- Call and send OK
                Set_Position (Node.Next.all);
              end if;
            else
              Put_Line ("Command error");
              Reset;
            end if;
          exception
            when Command.Terminate_Request =>
              Put_Line ("Exit requested");
              return;
            when Command.Spawn_Error =>
              Put_Line ("Command error");
              Reset;
          end;
        when Close =>
          Reset;
          Put_Line ("Closed");
      end case;
    end loop;
  end Handle;

end Events;

