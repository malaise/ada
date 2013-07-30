-- Executes a command (fork/exec) and retrieves its outputs
with As.U, Dynamic_List, Many_Strings;
package Command is

  -- Output and error flow,
  --  either a list: one item per line without LineFeed, not rewinded
  --  or a raw Asu_Us
  type Flow_Format_Kind_List is (Str, List);
  subtype Line_Type is As.U.Asu_Us;
  package Res_Mng is new Dynamic_List (Line_Type);
  subtype Res_List is Res_Mng.Dyn_List.List_Type;
  type Flow_Rec (Kind : Flow_Format_Kind_List) is record
    case Kind is
      when Str =>
        Str : As.U.Asu_Us;
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

  -- Call command and wait for its termination
  -- If not Use_Shell, then Cmd must be the program then its arguments
  --   concatenated in a Many_String
  -- If Use_Shell, then issue a 'sh -c "Cmd"'. Cmd can then be either a
  --   Many_String as above or the program and arguments separated by spaces
  -- Report or propagate output/error flow with proper kind
  -- Set resulting exit code
  -- If Use_Shell, the shell can be specified to another than the default
  ---------------------------------------------------------------------------
  -- Because it waits for the asynchronous exit of the child, this         --
  --  function uses Event_Mng.Wait internally. As a consequence:           --                         --
  --  * X11 programs shall Suspend ALL the X objects X_Line/Con_Io/Afpx    --
  --    before calling this function, then Resume the X objects afterwards,--
  --  * This function is protected agains parallel execution with a mutex. --
  ---------------------------------------------------------------------------
  Default_Shell : constant String := "/bin/sh";
  procedure Execute (Cmd : in Many_Strings.Many_String;
                     Use_Shell : in Boolean;
                     Mix_Policy : in Flow_Mixing_Policies;
                     Out_Flow : in Flow_Access;
                     Err_Flow : in Flow_Access;
                     Exit_Code : out Exit_Code_Range;
                     Shell : in String := Default_Shell);

  -- Terminate requested by Control C
  Terminate_Request : exception;

  -- Could not spawn command or /bin/sh
  Spawn_Error : exception;

end Command;

