with Ada.Calendar;
with As.U, Basic_Proc, Command, Many_Strings, Images, Mixed_Str,
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

  -- See if a variable must be set
  -- (IfUnset not set or Var not set)
  function Set_Var (Node : in Tree.Node_Rec) return Boolean is
    Name : As.U.Asu_Us;
  begin
    -- On Set and Eval Ifunset is in fact a boolean
    if not Node.Ifunset then
      -- IfUnset = flase => always set var
      return True;
    end if;
    -- See if Var is set
    Name := Variables.Expand (Node.Expression, Variables.Local_Only);
    return not Variables.Is_Set (Name);
  end Set_Var;

  -- Find next valid node
  procedure Find_Next is
    Node : Tree.Node_Rec;
  begin
    Node := Tree.Chats.Read;
    Tree.Set_Position (Node.Next.all);
  end Find_Next;

  -- Go to first child if any, otherwise Find_Next
  procedure Try_Child is
  begin
    if Tree.Chats.Children_Number /= 0 then
      Tree.Chats.Move_Child;
    else
      Find_Next;
    end if;
  end Try_Child;

  -- Handle events
  procedure Handle is
    -- Current node and next child
    Node, Child : Tree.Node_Rec;
    Current_Timeout : Integer;
    Children_Number : Trees.Child_Range;
    Event : Ios.Event_Type;
    Disconnection : Boolean;
    Select_Chats : As.U.Asu_Us;
    Variable : As.U.Asu_Us;
    Dummy : Boolean;
    use type Tree.Node_Kind, Ios.Event_Kind_List;

    function Handle_One return Boolean is
    begin
      case Node.Kind is
        when Tree.Selectn =>
          -- If we are root (chats)
          --  Keep previous chat timeout only if we are in chat,
          --  otherwise reset
          Current_Timeout := Node.Timeout;
          if not Tree.Chats.Has_Father and then not In_Chat then
            Current_Timeout := Tree.Infinite_Ms;
          end if;
          Select_Chats := As.U.Tus (if Tree.Chats.Has_Father then "select"
                                    else "chats");

          -- Read a sentence
          Event := Ios.Read (Current_Timeout);
          case Event.Kind is
            when Ios.Exit_Requested =>
              Put_Line ("Exit requested");
              return True;
            when Ios.Input_Error =>
              Put_Line ("Input error");
              return True;
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
              -- For reset if no timeout
              Children_Number := Tree.Chats.Children_Number;
              if Children_Number <= 1 then
                -- 0! or 1 (expect) entry
                In_Chat := False;
              else
                -- See if last child is a timeout
                Tree.Chats.Move_Child (False);
                Child := Tree.Chats.Read;

                if In_Chat and then Child.Kind = Tree.Timeout then
                  Debug.Logger.Log_Debug ("Select timeout");
                  -- Remain in this timeout entry
                end if;
              end if;
              if not In_Chat then
                -- No timeout found
                Debug.Logger.Log_Debug ("No timeout on select");
                Put_Line ("Timeout on " & Select_Chats.Image);
                Reset;
              end if;

            when Ios.Got_Sentence =>
              -- Dispatch to a child, there is at least one Expect
              Debug.Logger.Log_Debug ("Select got: " & Event.Sentence.Image);
              Selec_Children:
              for I in 1 .. Tree.Chats.Children_Number loop
                if I = 1 then
                  Tree.Chats.Move_Child;
                else
                  Tree.Chats.Move_Brother (False);
                end if;
                Child := Tree.Chats.Read;
                Debug.Logger.Log_Debug ("Select trying: "
                                      & Child.Critext.Image);
                if Child.Kind = Tree.Timeout then
                  -- Current (last) child is Timeout => no match
                  Put_Line ("No match on " & Select_Chats.Image);
                  Reset;
                  exit Selec_Children;
                elsif Child.Kind = Tree.Default
                or else (Child.Kind = Tree.Expect and then
                         Matcher.Match (Child, Event.Sentence) ) then
                  if Child.Kind = Tree.Default then
                    Debug.Logger.Log_Debug ("Select "
                       & (if Child.Kind = Tree.Default then "default"
                          else "match: " & Child.Critext.Image));
                  end if;
                  -- This Expect child matches (or is default)
                  if Child.Kind = Tree.Expect
                  and then not Child.Name.Is_Null then
                    -- This is the start of a new chat
                    Put_Line ("Starting chat " & Child.Name.Image);
                    In_Chat := True;
                    Ios.Stop_Global_Timer;
                    Ios.Start_Global_Timer (Child.Timeout);
                  end if;
                  -- Remain on this select entry
                  exit Selec_Children;
                elsif not Tree.Chats.Has_Brother (False) then
                  -- No more child
                  Put_Line ("No match on " & Select_Chats.Image);
                  Reset;
                  exit Selec_Children;
                end if;
              end loop Selec_Children;
          end case;

        when Tree.Cond =>
          -- Dispatch to a child
          Cond_Children:
          for I in 1 .. Tree.Chats.Children_Number loop
            if I = 1 then
              Tree.Chats.Move_Child;
            else
              Tree.Chats.Move_Brother (False);
            end if;
            Child := Tree.Chats.Read;
            -- Resolv variable
            if Child.Kind = Tree.Condif then
              Variable := Child.Expression;
              Debug.Logger.Log_Debug ("Condif trying: " & Variable.Image
                                    & " match " & Child.Critext.Image);
              if Matcher.Match (Child) then
                Debug.Logger.Log_Debug ("Condif match");
                -- Remain on this matching Condif child
                exit Cond_Children;
              elsif Tree.Chats.Has_Brother (False) then
                -- The only case when we continue
                Debug.Logger.Log_Debug ("Condif not match");
              else
                -- No more child
                Debug.Logger.Log_Debug ("Cond no match");
                Find_Next;
                exit Cond_Children;
              end if;
            elsif Child.Kind = Tree.Condelse then
              Debug.Logger.Log_Debug ("Condelse");
              -- Remain on this matching Condelse child
              exit Cond_Children;
            end if;
          end loop Cond_Children;

        when Tree.Repeat =>
          -- Resolv variable
          Variable := Node.Expression;
          Debug.Logger.Log_Debug ("Repeat checking: " & Variable.Image
                                    & " match " & Node.Critext.Image);
          -- See if variable content matches
          if Matcher.Match (Node) then
            -- Match, go to the first child of the loop
            Debug.Logger.Log_Debug ("Repeat yes");
            Tree.Chats.Move_Child;
          else
            Debug.Logger.Log_Debug ("Repeat no");
            -- No match, move to next instruction
            Find_Next;
          end if;

        when Tree.Loopn =>
          Tree.Chats.Move_Child;

        when Tree.Exitn =>
          Find_Next;

        when Tree.Read =>
          Event := Ios.Read (Node.Timeout);
          case Event.Kind is
            when Ios.Exit_Requested =>
              Put_Line ("Exit requested");
              return True;
            when Ios.Input_Error =>
              Put_Line ("Input error");
              return True;
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
                Find_Next;
              else
                Put_Line ("Read mismatch");
                Reset;
              end if;
          end case;

        when Tree.Get =>
          Event := Ios.Read (Node.Timeout);
          case Event.Kind is
            when Ios.Exit_Requested =>
              Put_Line ("Exit requested");
              return True;
            when Ios.Input_Error =>
              Put_Line ("Input error");
              return True;
            when Ios.Disconnection =>
              Put_Line ("Disconnection");
              Reset;
            when Ios.Global_Timeout =>
              Put_Line ("Timeout on chat script");
              Reset;
            when Ios.Local_Timeout =>
              Put_Line ("Timeout on Get");
              Reset;
            when Ios.Got_Sentence =>
              -- Get
              Debug.Logger.Log_Debug ("Get got: " & Event.Sentence.Image);
              -- Assign
              begin
                if Set_Var (Node) then
                  -- Load the variable
                  if Matcher.Match (Node, Event.Sentence) then
                    Find_Next;
                  else
                    raise Matcher.Match_Error;
                  end if;
                else
                  -- Skip
                  Find_Next;
                end if;
              exception
                when Variables.Expand_Error | Matcher.Match_Error =>
                  if Tree.Chats.Children_Number /= 0 then
                    Debug.Logger.Log_Debug ("Invalid evaluation "
                                          & Node.Critext.Image);
                    Tree.Chats.Move_Child;
                  else
                    Put_Line ("Invalid evaluation");
                    Reset;
                  end if;
              end;
          end case;

        when Tree.Skip =>
          Event := Ios.Read (Node.Timeout);
          case Event.Kind is
            when Ios.Exit_Requested =>
              Put_Line ("Exit requested");
              return True;
            when Ios.Input_Error =>
              Put_Line ("Input error");
              return True;
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
              Find_Next;
          end case;

        when Tree.Wait =>
          Event := Ios.Wait (Node.Timeout);
          case Event.Kind is
            when Ios.Exit_Requested =>
              Put_Line ("Exit requested");
              return True;
            when Ios.Input_Error =>
              Put_Line ("Input error");
              return True;
            when Ios.Disconnection =>
              Put_Line ("Disconnection");
              Reset;
            when Ios.Global_Timeout =>
              Put_Line ("Timeout on chat script");
              Reset;
            when Ios.Local_Timeout =>
              Find_Next;
            when Ios.Got_Sentence =>
              -- Should not occur
              null;
          end case;

        when Tree.Send =>
          begin
            Ios.Send (Variables.Expand (Node.Critext, Variables.Local_Env),
                      Disconnection);
            if Disconnection then
              Put_Line ("Disconnection");
              Reset;
            else
              Find_Next;
            end if;
          exception
            when Ios.Output_Error =>
              Put_Line ("Output error");
              Reset;
          end;
        when Tree.Call =>
          declare
            Exit_Code : Command.Exit_Code_Range;
          begin
            Command.Execute (
              Cmd => Many_Strings.Set (As.U.Asu_Us'(
                  Variables.Expand (Node.Critext, Variables.Local_Env))),
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
                Find_Next;
              end if;
            elsif Tree.Chats.Children_Number /= 0 then
              -- First child is error handler
              Debug.Logger.Log_Debug ("Call handling error");
              Tree.Chats.Move_Child;
            else
              Put_Line ("Command error");
              Reset;
            end if;
          exception
            when Command.Terminate_Request =>
              Put_Line ("Exit requested");
              return True;
            when Command.Spawn_Error =>
              Put_Line ("Command error");
              Reset;
          end;

        when Tree.Eval =>
          if Set_Var (Node) then
            declare
              Exit_Code : Command.Exit_Code_Range;
            begin
              Command.Execute (
                Cmd => Many_Strings.Set (As.U.Asu_Us'(
                    Variables.Expand (Node.Critext, Variables.Local_Env))),
                Use_Shell => True,
                Mix_Policy => Command.Only_Out,
                Out_Flow => Flow'Access,
                Err_Flow => null,
                Exit_Code => Exit_Code);
              if Exit_Code = 0 then
                Debug.Logger.Log_Debug ("Eval got: " & Flow.Str.Image);
                -- Command OK, load the variable
                if not Matcher.Match (Node, Flow.Str) then
                  Put_Line ("Invalid evaluation");
                end if;
                Find_Next;
              elsif Tree.Chats.Children_Number /= 0 then
                -- First child is error handler
                Debug.Logger.Log_Debug ("Eval handling error");
                Tree.Chats.Move_Child;
              else
                Put_Line ("Command error");
                Reset;
              end if;
            exception
              when Command.Terminate_Request =>
                Put_Line ("Exit requested");
                return True;
              when Command.Spawn_Error =>
                Put_Line ("Command error");
                Reset;
            end;
          else
            -- Skip
            Find_Next;
          end if;

        when Tree.Set =>
          -- Set or Assign
          begin
            if Set_Var (Node) then
              -- Load the variable
              if Matcher.Match (Node) then
                Find_Next;
              else
                raise Matcher.Match_Error;
              end if;
            else
              -- Skip
              Find_Next;
            end if;
          exception
            when Variables.Expand_Error | Matcher.Match_Error =>
              if Tree.Chats.Children_Number /= 0 then
                Debug.Logger.Log_Debug ("Invalid evaluation "
                                      & Node.Critext.Image);
                Tree.Chats.Move_Child;
              else
                Put_Line ("Invalid evaluation");
                Reset;
              end if;
          end;

        when Tree.Parse =>
          Dummy := Matcher.Match (Node);
          Find_Next;

        when Tree.Chdir =>
          declare
            Target : constant String
                   := Variables.Expand (Node.Critext, Variables.Local_Env);
          begin
            if Target = "" then
              -- Go to initial dir
              Directory.Change_Current (Home_Dir.Image);
            else
              -- Go to target dir
              Directory.Change_Current (Target);
            end if;
            Find_Next;
          exception
            when Directory.Name_Error | Directory.Access_Error =>
              if Tree.Chats.Children_Number /= 0 then
                Debug.Logger.Log_Debug ("Chdir handling error");
                Tree.Chats.Move_Child;
              else
                Put_Line ("ERROR changing current directory");
                Reset;
              end if;
          end;

        when Tree.Log =>
          Put_Line ("Log: " & Variables.Expand (Node.Critext,
                                                Variables.Local_Env));
          Find_Next;

        when Tree.Close =>
          Reset;
          Put_Line ("Closed");

        when Tree.Expect | Tree.Default | Tree.Timeout
           | Tree.Condif | Tree.Condelse =>
          -- Move to next instruction
          Try_Child;
      end case;
      return False;
    end Handle_One;

  begin
    Put_Line (Argument.Get_Program_Name & " V" & Tree.Get_Version & " ready");
    Main : loop
      begin
        -- Where are we?
        Node := Tree.Chats.Read;

        Debug.Logger.Log_Debug ("Node is " & Mixed_Str (Node.Kind'Img)
                              & " with Critext " & Node.Critext.Image);
        exit Main when Handle_One;
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

