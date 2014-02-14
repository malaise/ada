with Ada.Calendar;
with As.U, Basic_Proc, Command, Many_Strings, Images, Mixed_Str, Trilean,
     Directory, Argument, Trees;
with Variables, Tree, Ios, Matcher, Debug;
package body Events is

  -- Report progress
  procedure Put_Line (Str : in String) is
  begin
    Basic_Proc.Put_Line_Error (Images.Date_Image (Ada.Calendar.Clock)
                              & " => " & Str & ".");
  end Put_Line;

  -- Output flow of called command
  Flow : aliased Command.Flow_Rec(Command.Str);

  -- Are we in a chat, if yes then the timeout of root (chats) select
  --  applies. If not, no need to reset connection when no chat selected
  In_Chat : Boolean := False;

  -- Initial directory
  Home_Dir : constant As.U.Asu_Us := As.U.Tus (Directory.Get_Current);

  -- Reset connection, variables, directory and move on root
  procedure Reset is
  begin
    In_Chat := False;
    Ios.Reset;
    Variables.Reset;
    Directory.Change_Current (Home_Dir.Image);
    Tree.Chats.Move_Root;
  end Reset;

  -- See if node has an error handler
  -- True if it has 2 children or if it has one child that is not Next
  function Has_Error_Handler (Node : in Tree.Node_Rec) return Boolean is
    Nb_Children : constant Natural := Tree.Chats.Children_Number;
    Child_Position : Tree.Position_Access;
    use type Tree.Position_Access;
  begin
    if Nb_Children = 0 then
      -- No child => no error handler
      return False;
    elsif Nb_Children = 2 then
      -- 2 children => an error handler
      return True;
    else
      -- One child, get its position
      Tree.Chats.Move_Child;
      Child_Position := Tree.Position_Access(Tree.Chats.Get_Position);
      Tree.Chats.Move_Father;
      -- Child is the error handler if it is not Next
      return Child_Position /= Node.Next.all;
    end if;
  end Has_Error_Handler;

  -- See if a variable must be set
  -- (IfUnset not set or Var not set)
  function Set_Var (Node : in Tree.Node_Rec) return Boolean is
    Name : As.U.Asu_Us;
  begin
    -- On Set and Eval Ifunset is in fact a boolean
    if not Trilean.Tri2Boo (Node.Ifunset) then
      -- IfUnset = flase => always set var
      return True;
    end if;
    -- See if Var is set
    Name := Variables.Expand (Node.Assign(Node.Assign'First).Name,
                              Variables.Local_Only);
    return not Variables.Is_Set (Name);
  end Set_Var;

  -- Handle events
  Internal_Error : exception;
  procedure Handle is
    use Tree;
    Node, Child : Node_Rec;
    Current_Timeout : Integer;
    Children_Number : Trees.Child_Range;
    Next : Position_Access;
    Event : Ios.Event_Type;
    Disconnection : Boolean;
    Variable : As.U.Asu_Us;
    use type Ios.Event_Kind_List;
  begin
    Put_Line (Argument.Get_Program_Name & " V" & Tree.Get_Version & " ready");
    Main : loop
      begin
        -- Where are we?
        Node := Chats.Read;
        Debug.Logger.Log_Debug ("Node is " & Mixed_Str (Node.Kind'Img));
        case Node.Kind is
          when Nop =>
            -- no operation (empty "if" or "expect"
            Set_Position (Node.Next.all);

          when Selec =>
            -- If we are root
            --  Keep previous chat timeout if we are in chat
            --  No select timeout if we are not in chat (no useless reset)
            Current_Timeout := Node.Timeout;
            if not Chats.Has_Father and then not In_Chat then
              Current_Timeout := Infinite_Ms;
            end if;

            -- Read a sentence
            Event := Ios.Read (Current_Timeout);
            case Event.Kind is
              when Ios.Exit_Requested =>
                Put_Line ("Exit requested");
                exit Main;
              when Ios.Fatal_Error =>
                Put_Line ("Fatal error");
                exit Main;
              when Ios.Disconnection =>
                if In_Chat then
                  Put_Line ("Disconnection");
                end if;
                Reset;
              when Ios.Global_Timeout =>
                Put_Line ("Timeout on chat script");
                Reset;
              when Ios.Local_Timeout =>
                Debug.Logger.Log_Debug ("Select: timeout");
                Next := Node.Next.all;
                -- For reset if no timeout
                Children_Number := Chats.Children_Number;
                if Children_Number <= 1 then
                  -- 0! or 1 (expect) entry
                  In_Chat := False;
                else
                  -- See if last child is a timeout
                  Chats.Move_Child (False);
                  Child := Chats.Read;
                  if Position_Access(Chats.Get_Position) = Next
                  and then Children_Number > 2 then
                    -- last child is next, go to prev if any
                    Chats.Move_Brother;
                    Child := Chats.Read;
                  else
                    In_Chat := False;
                  end if;

                  if In_Chat and then Child.Kind = Timeout then
                    Debug.Logger.Log_Debug ("Selec timeout");
                    -- Move to the child of this select entry
                    Set_Position (Child.Next.all);
                  end if;
                end if;
                if not In_Chat then
                  -- No timeout found
                  Debug.Logger.Log_Debug ("No timeout on select");
                  Put_Line ("Timeout on select");
                  Reset;
                end if;

              when Ios.Got_Sentence =>
                -- Dispatch to child, avoid Next
                Debug.Logger.Log_Debug ("Selec got: " & Event.Sentence.Image);
                Next := Node.Next.all;
                Selec_Children:
                for I in 1 .. Chats.Children_Number loop
                  if I = 1 then
                    Chats.Move_Child;
                  else
                    Chats.Move_Brother (False);
                  end if;
                  Child := Chats.Read;
                  Debug.Logger.Log_Debug ("Selec trying: " & Child.Text.Image);
                  if Position_Access(Chats.Get_Position) = Next then
                    -- Current (last) child is Next => no match
                    Put_Line ("No match on select");
                    Reset;
                    exit Selec_Children;
                  elsif Child.Kind = Default
                  or else (Child.Kind = Read and then
                           Matcher.Match (Child, Event.Sentence) ) then
                    if Child.Kind = Default then
                      Debug.Logger.Log_Debug ("Selec default");
                    else
                      Debug.Logger.Log_Debug ("Selec match: "
                                            & Child.Text.Image);
                    end if;
                    -- This read child matches (or is default)
                    if Child.Kind = Read
                    and then not Child.Name.Is_Null then
                      -- This is the start of a new chat
                      Put_Line ("Starting chat " & Child.Name.Image);
                      In_Chat := True;
                      Ios.Stop_Global_Timer;
                      Ios.Start_Global_Timer (Child.Timeout);
                    end if;
                    -- Move to the child of this select entry
                    Set_Position (Child.Next.all);
                    exit Selec_Children;
                  elsif not Chats.Has_Brother (False) then
                    -- No more child
                    Put_Line ("No match on select");
                    Reset;
                    exit Selec_Children;
                  end if;
                end loop Selec_Children;
            end case;

          when Cond =>
            -- Dispatch to child, avoid Next
            Next := Node.Next.all;
            Cond_Children:
            for I in 1 .. Chats.Children_Number loop
              if I = 1 then
                Chats.Move_Child;
              else
                Chats.Move_Brother (False);
              end if;
              Child := Chats.Read;
              -- Resolv variable
              if Child.Next.all = Next then
                -- Last child is Next => no match
                Debug.Logger.Log_Debug ("Cond no match");
                Set_Position (Next);
                exit Cond_Children;
              elsif Child.Kind = Condif then
                Variable := Child.Assign(Child.Assign'First).Name;
                Debug.Logger.Log_Debug ("Condif trying: " & Variable.Image
                                      & " match " & Child.Text.Image);
                if Matcher.Match (Child, Variable) then
                  Debug.Logger.Log_Debug ("Condif match");
                  -- Move to the child of this select entry
                  Set_Position (Child.Next.all);
                  exit Cond_Children;
                elsif Chats.Has_Brother (False) then
                  -- The only case when we continue
                  Debug.Logger.Log_Debug ("Condif not match");
                else
                  -- No more child
                  Debug.Logger.Log_Debug ("Cond no match");
                  Set_Position (Next);
                  exit Cond_Children;
                end if;
              elsif Child.Kind = Condelse then
                Debug.Logger.Log_Debug ("Condelse");
                -- Move to the child of this select entry
                Set_Position (Child.Next.all);
                exit Cond_Children;
              else
                -- This child is the next
                Debug.Logger.Log_Debug ("Cond no match");
                Set_Position (Next);
                exit Cond_Children;
              end if;
            end loop Cond_Children;
          when Condif | Condelse =>
            -- Should not occur
            raise Internal_Error;

          when Repeat =>
            -- Resolv variable
            Variable := Node.Assign(Node.Assign'First).Name;
            -- Repeat has 2 children, the first instruction of the loop
            --  and the next instruction after the loop
            -- See if variable content matches
            if Matcher.Match (Node, Variable) then
              -- Match, go to the first child of the loop
              Debug.Logger.Log_Debug ("Repeat true: " & Node.Text.Image);
              Chats.Move_Child (True);
            else
              Debug.Logger.Log_Debug ("Repeat false: " & Node.Text.Image);
              -- No mach, move to end of loop
              Chats.Move_Child (False);
            end if;

          when Read =>
            Event := Ios.Read (Node.Timeout);
            case Event.Kind is
              when Ios.Exit_Requested =>
                Put_Line ("Exit requested");
                exit Main;
              when Ios.Fatal_Error =>
                Put_Line ("Fatal error");
                exit Main;
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
                Debug.Logger.Log_Debug ("Read got: " & Event.Sentence.Image);
                -- Check match
                if Matcher.Match (Node, Event.Sentence) then
                  Set_Position (Node.Next.all);
                else
                  Put_Line ("Read mismatch");
                  Reset;
                end if;
            end case;

          when Default | Timeout =>
            -- Should not occur
            raise Internal_Error;

          when Skip =>
            Event := Ios.Read (Node.Timeout);
            case Event.Kind is
              when Ios.Exit_Requested =>
                Put_Line ("Exit requested");
                exit Main;
              when Ios.Fatal_Error =>
                Put_Line ("Fatal error");
                exit Main;
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
                Debug.Logger.Log_Debug ("Skip got: " & Event.Sentence.Image);
                Set_Position (Node.Next.all);
            end case;

          when Wait =>
            Event := Ios.Wait (Node.Timeout);
            case Event.Kind is
              when Ios.Exit_Requested =>
                Put_Line ("Exit requested");
                exit Main;
              when Ios.Fatal_Error =>
                Put_Line ("Fatal error");
                exit Main;
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
            Ios.Send (Variables.Expand (Node.Text, Variables.Local_Env),
                      Disconnection);
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
                Cmd => Many_Strings.Set (As.U.Asu_Us'(
                    Variables.Expand (Node.Text, Variables.Local_Env))),
                Use_Shell => True,
                Mix_Policy => Command.Only_Out,
                Out_Flow => Flow'Access,
                Err_Flow => null,
                Exit_Code => Exit_Code);
              if Exit_Code = 0 then
                -- Command OK, send its out flow
                Debug.Logger.Log_Debug ("Call got: " & Flow.Str.Image);
                Ios.Send (Flow.Str, Disconnection);
                if Disconnection then
                  Put_Line ("Disconnection");
                  Reset;
                else
                  -- Call and send OK
                  Set_Position (Node.Next.all);
                end if;
              elsif Has_Error_Handler (Node) then
                -- First child is error handler
                Debug.Logger.Log_Debug ("Call handling error");
                Chats.Move_Child;
              else
                Put_Line ("Command error");
                Reset;
              end if;
            exception
              when Command.Terminate_Request =>
                Put_Line ("Exit requested");
                exit Main;
              when Command.Spawn_Error =>
                Put_Line ("Command error");
                Reset;
            end;

          when Eval =>
            if Set_Var (Node) then
              declare
                Exit_Code : Command.Exit_Code_Range;
              begin
                Command.Execute (
                  Cmd => Many_Strings.Set (As.U.Asu_Us'(
                      Variables.Expand (Node.Text, Variables.Local_Env))),
                  Use_Shell => True,
                  Mix_Policy => Command.Only_Out,
                  Out_Flow => Flow'Access,
                  Err_Flow => null,
                  Exit_Code => Exit_Code);
                if Exit_Code = 0 then
                  Debug.Logger.Log_Debug ("Eval got: " & Flow.Str.Image);
                  -- Command OK, load the variable
                  if Matcher.Match (Node, Flow.Str) then
                    Set_Position (Node.Next.all);
                  else
                    Put_Line ("Invalid evaluation");
                  end if;
                elsif Has_Error_Handler (Node) then
                  -- First child is error handler
                  Debug.Logger.Log_Debug ("Eval handling error");
                  Chats.Move_Child;
                else
                  Put_Line ("Command error");
                  Reset;
                end if;
              exception
                when Command.Terminate_Request =>
                  Put_Line ("Exit requested");
                  exit Main;
                when Command.Spawn_Error =>
                  Put_Line ("Command error");
                  Reset;
              end;
            else
              -- Skip
              Set_Position (Node.Next.all);
            end if;

          when Set =>
            -- Set or Assign
            begin
              if Set_Var (Node) then
                -- Load the variable
                if Matcher.Match (Node,
                                  Variables.Expand (Node.Text,
                                                    Variables.Local_Env)) then
                  Set_Position (Node.Next.all);
                else
                  raise Matcher.Match_Error;
                end if;
              else
                -- Skip
                Set_Position (Node.Next.all);
              end if;
            exception
              when Variables.Expand_Error | Matcher.Match_Error =>
                if Has_Error_Handler (Node) then
                  Debug.Logger.Log_Debug ("Invalid evaluation "
                                        & Node.Text.Image);
                  Chats.Move_Child;
                else
                  Put_Line ("Invalid evaluation");
                  Reset;
                end if;
            end;

          when Chdir =>
            declare
              Target : constant String
                     := Variables.Expand (Node.Text, Variables.Local_Env);
            begin
              if Target = "" then
                -- Go to initial dir
                Directory.Change_Current (Home_Dir.Image);
              else
                -- Go to target dir
                Directory.Change_Current (Target);
              end if;
              Set_Position (Node.Next.all);
            exception
              when Directory.Name_Error | Directory.Access_Error =>
                if Has_Error_Handler (Node) then
                  Debug.Logger.Log_Debug ("Chdir handling error");
                  Chats.Move_Child;
                else
                  Put_Line ("ERROR changing current directory");
                  Reset;
                end if;
            end;

          when Log =>
            Put_Line ("Log: " & Variables.Expand (Node.Text,
                                                  Variables.Local_Env));
            Set_Position (Node.Next.all);

          when Close =>
            Reset;
            Put_Line ("Closed");
        end case;
      exception
        when Matcher.Match_Error =>
          Put_Line ("ERROR matching expression");
          Reset;
        when Variables.Expand_Error =>
          Put_Line ("ERROR expanding expression");
          Reset;
      end;
    end loop Main;

    Ios.Close;
  end Handle;

end Events;

