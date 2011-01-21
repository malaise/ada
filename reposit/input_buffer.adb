-- Bufferize input flow (strings) until separator is found
-- Report the received string (text between separators)
with Ada.Unchecked_Deallocation;
with String_Mng;
package body Input_Buffer is

  -- Initialise the buffer Buf with the sentence delimiter and the notifier
  procedure Set (Buf : in out Buffer;
                 Notifier : in Sentence_Notifier;
                 Delimiter : in String := Line_Feed) is
  begin
    if Notifier = null or else Delimiter = "" then
      raise Constraint_Error;
    end if;
    if Buf.Acc = null then
      Buf.Acc := new Buffer_Rec;
    end if;
    Buf.Acc.Notif := Notifier;
    Buf.Acc.Delim := As.U.Tus (Delimiter);
    Buf.Acc.Text.Set_Null;
    Buf.Acc.Susp := False;
  end Set;

  -- INTERNAL: Raise Status_Error if Buf is not set
  procedure Check_Set (Buf : in Buffer) is
  begin
    if not Is_Set (Buf) then
      raise Status_Error;
    end if;
  end Check_Set;

  -- INTERNAL: Invoque the notifier if Buf is not suspended
  procedure Flush (Buf : in Buffer) is
    Ind : Natural;
    Head : As.U.Asu_Us;
  begin
    if Buf.Acc.Susp then
      return;
    end if;
    -- Several notifs
    loop
      Ind := String_Mng.Locate (Buf.Acc.Text.Image, Buf.Acc.Delim.Image);
      exit when Ind = 0;
      -- Move to end of sentence (end of delim)
      Ind := Ind + Buf.Acc.Delim.Length - 1;
      Head := Buf.Acc.Text.Uslice (1, Ind);
      -- Del head
      Buf.Acc.Text.Delete (1, Ind);
      -- Finally notify (after the delete so that exception in Notifier
      --  don't lead to infinite loop if ignored by push/resume.)
      Buf.Acc.Notif (Head.Image);
    end loop;
  end Flush;

  -- Push text in buffer Buf
  -- Can lead the notifier to be invoqued once or several times
  procedure Push (Buf : in Buffer; Text : in String) is
  begin
    Check_Set (Buf);
    Buf.Acc.Text.Append (Text);
    Flush (Buf);
  end Push;
  procedure Push (Buf : in Buffer; Char : in Character) is
  begin
    Check_Set (Buf);
    Buf.Acc.Text.Append (Char);
    Flush (Buf);
  end Push;

  -- Get the tail of the buffer Buf (text not ending with Delimiter)
  function Tail (Buf : Buffer) return String is
  begin
    Check_Set (Buf);
    return Buf.Acc.Text.Image;
  end Tail;

  -- Suspend the buffer Buf
  procedure Suspend (Buf : in Buffer) is
  begin
    Check_Set (Buf);
    Buf.Acc.Susp := True;
  end Suspend;

  -- Resume the buffer Buf
  procedure Resume (Buf : in Buffer) is
  begin
    Check_Set (Buf);
    Buf.Acc.Susp := False;
    Flush (Buf);
  end Resume;

  -- Is the buffer Buf suspended?
  function Is_Suspended (Buf : Buffer) return Boolean is
  begin
    Check_Set (Buf);
    return Buf.Acc.Susp;
  end Is_Suspended;

  -- Is the buffer Buf set?
  function Is_Set (Buf : Buffer) return Boolean is
  begin
    return Buf.Acc /= null and then Buf.Acc.Notif /= null;
  end Is_Set;

  -- Copy the buffer Src_Buf to Dest_Buf
  procedure Copy (Dest_Buf : in out Buffer; Src_Buf : in Buffer) is
  begin
    Finalize (Dest_Buf);
    if Is_Set (Src_Buf) then
      Dest_Buf.Acc := new Buffer_Rec'(Src_Buf.Acc.all);
    end if;
  end Copy;


  --  Free Acc
  procedure Free is new Ada.Unchecked_Deallocation
         (Object => Buffer_Rec,
          Name   => Buffer_Rec_Access);
  overriding procedure Finalize (Buf : in out Buffer) is
  begin
    if Is_Set (Buf) then
      Free (Buf.Acc);
    end if;
  end Finalize;

end Input_Buffer;

