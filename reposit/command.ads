-- Executes a command (fork/exec) and retrieves its outputs
with Ada.Strings.Unbounded;
with Dynamic_List;
package Command is

  -- Asu stuff
  package Asu renames Ada.Strings.Unbounded;
  subtype Asu_Us is Asu.Unbounded_String;

  -- Output and error flow, either a list (one item perline) or a Asu_Us
  type Flow_Format_Kind_List is (Str, List);
  subtype Line_Type is Asu_Us;
  package Res_Mng is new Dynamic_List (Line_Type);
  subtype Res_List is Res_Mng.Dyn_List.List_Type;
  type Flow_Rec (Kind : Flow_Format_Kind_List) is record
    case Kind is
      when Str =>
        Str : Asu_Us;
      when List =>
        List : Res_List;
    end case;
  end record;
  type Flow_Access is access all Flow_Rec;

  -- Strategy to set/merge/propagate flows
  -- Set each flow on its Flow_Rec
  -- Set only Out_Flow to output flow (error flow is put on stderr)
  -- Propagate each flow
  type Flow_Mixing_Policies is (Both, Only_Out, None);

  -- Kind of exit code
  subtype Exit_Code_Range is Integer range -1 .. Integer'Last;
  Error : constant Exit_Code_Range := -1;

  -- Call command, which have to follow Many_Strings format
  -- If use_Sh, then issue a 'sh -c "Cmd"'
  -- Report or propagate output/error flow with proper kind
  -- Set resulting exit code
  ---------------------------------------------------------------------------
  -- Because it waits for asynchronous exit of child, this function uses   --
  --  Event_Mng.Wait, which sets signal handlers. As a consequence:        --
  --  * Non interactive programs shall call                                --
  --    Event_Mng.Reset_Default_Signal_Policy after using this function    --
  --  * X11 programs shall Suspend ALL the X objects X_Line/Con_Io/Afpx    --
  --    before calling this function, then Resume the X objects.           --
  ---------------------------------------------------------------------------
  procedure Execute (Cmd : in String;
                     Use_Sh : in Boolean;
                     Mix_Policy : in Flow_Mixing_Policies;
                     Out_Flow : in Flow_Access;
                     Err_Flow : in Flow_Access;
                     Exit_Code : out Exit_Code_Range);

  -- Terminate requested by Control C
  Terminate_Request : exception;

  -- Could not spawn command or /bin/sh
  Spawn_Error : exception;

end Command;

