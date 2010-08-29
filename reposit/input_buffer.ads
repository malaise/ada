-- Bufferize input flow (strings) until separator is found
-- Report the received string (text between separators)
-- This is the same as Text_Line input except that the input flow is ourself
with Ada.Finalization;
with As.U; use As.U;
with Text_Line;
package Input_Buffer is

  -------------------------
  -- GENERAL DEFINITIONS --
  -------------------------
  -- Default separator
  Line_Feed : constant String := Text_Line.Line_Feed_Str;

  -- Callback invoqued when a sentence has been identified in input flow
  --  (text ending by separator)
  type Sentence_Notifier is access procedure (Sentence : in String);

  -- A buffer
  type Buffer is tagged limited private;

  ----------------------
  -- BASIC OPERATIONS --
  ----------------------
  -- Initialise the buffer Buf with the sentence delimiter and the notifier
  -- Previous content of Buf is overwritten
  -- May raise Constraint_Error if Delimiter is empty or Notifier is null
  procedure Set (Buf : in out Buffer;
                 Notifier : in Sentence_Notifier;
                 Delimiter : in String := Line_Feed);

  -- Push text in buffer Buf
  -- Can lead the notifier to be invoqued once or several times
  -- May raise Status_Error if Buf is not set
  procedure Push (Buf : in Buffer; Text : in String);
  procedure Push (Buf : in Buffer; Char : in Character);

  ----------------------
  -- OTHER OPERATIONS --
  ----------------------
  -- Get the tail of the buffer Buf (text not ending with Delimiter)
  -- Beware that there may be Delimiters in the tail if the buffer is
  --  suspended
  -- May raise Status_Error if Buf is not set
  function Tail (Buf : Buffer) return String;

  -- Suspend the buffer Buf
  -- It will store the text push but not notify
  -- No effect if already suspended
  -- May raise Status_Error if Buf is not set
  procedure Suspend (Buf : in Buffer);

  -- Resume the buffer Buf
  -- Can lead the notifier to be invoqued once or several times
  -- No effect if not suspended
  -- May raise Status_Error if Buf is not set
  procedure Resume (Buf : in Buffer);

  -- Is the buffer Buf suspended?
  -- May raise Status_Error if Buf is not set
  function Is_Suspended (Buf : Buffer) return Boolean;

  ---------------
  -- UTILITIES --
  ---------------
  -- Is the buffer Buf set?
  function Is_Set (Buf : Buffer) return Boolean;

  -- Copy the buffer Src_Buf to Dest_Buf
  -- Previous content of Dest_Buf is overwritten
  -- Src_Buf may not be set, which deletes Dest_Buf
  procedure Copy (Dest_Buf : in out Buffer; Src_Buf : in Buffer);

  Status_Error : exception;
private

  type Buffer_Rec is record
    Notif : Sentence_Notifier := null;
    Delim : Asu_Us;
    Text : Asu_Us;
    Susp : Boolean := False;
  end record;

  Default_Rec : Buffer_Rec;
  Init_Rec : constant Buffer_Rec := Default_Rec;

  type Buffer_Rec_Access is access Buffer_Rec;
  type Buffer is limited new Ada.Finalization.Limited_Controlled with record
    Acc : Buffer_Rec_Access := null;
  end record;

   --  Free Acc
  overriding procedure Finalize (Buf : in out Buffer);

end Input_Buffer;

