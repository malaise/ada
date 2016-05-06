-- PragmAda Reusable Component (PragmARC)
-- Copyright (C) 2016 by PragmAda Software Engineering.  All rights reserved.
-- **************************************************************************
--
-- Text I/O to handle text files from multiple platforms
-- Line terminators must be encoded as a sequence of 1 or 2 characters
-- No column, line, or page counting; no column or page operations
-- End_Of_File works correctly
--
-- History:
-- 2016 Mar 01     J. Carter          Use Sequential_IO so no extra EOLs when a file is closed
-- 2016 Feb 15     J. Carter          V1.0--Initial version
--
with Ada.Sequential_IO;

package PragmARC.Text_IO is
   type EOL_ID is (DOS_Windows_EOL, Mac_EOL, Unix_EOL);
   -- DOS_Windows_EOL = CR-LF
   -- Mac_EOL         = CR
   -- Unix_EOL        = LF

   procedure Set_Line_Terminator (EOL : in EOL_ID);
   -- Sets the line terminator for output to EOL
   -- The default output EOL is DOS_Windows_EOL

   type File_Handle is tagged limited private;

   package Character_IO is new Ada.Sequential_IO (Element_Type => Character);

   -- File modes and forms are interpreted the same as for Character_IO

   function In_File     return Character_IO.File_Mode renames Character_IO.In_File;
   function Out_File    return Character_IO.File_Mode renames Character_IO.Out_File;
   function Append_File return Character_IO.File_Mode renames Character_IO.Append_File;

   procedure Create (File : in out File_Handle;
                     Name : in     String                 := "";
                     Mode : in     Character_IO.File_Mode := Out_File;
                     Form : in     String                 := "");
   -- Creates a file named Name with mode Mode and form Form accessible through File
   -- File becomes open
   -- The default Name creates a temporary file
   -- May raise the same exceptions as Character_IO.Create

   procedure Open (File : in out File_Handle;
                   Name : in     String;
                   Mode : in     Character_IO.File_Mode := In_File;
                   Form : in     String                 := "");
   -- Opens the file named Name with mode Mode and form Form accessbile through File
   -- File becomes open
   -- May raise the same exceptions as Character_IO.Open

   procedure Close (File : in out File_Handle);
   -- Closes open file File; File must be open
   -- May raise the same exceptions as Character_IO.Close

   function Is_Open (File : File_Handle) return Boolean;
   -- Returns True if File is open; False otherwise

   procedure New_Line (File : in out File_Handle; Spacing : in Positive := 1);
   -- Adds Spacing EOLs to File
   -- File mode must be Out_File or Append_File

   -- An input EOL is a CR-LF pair, or a single CR not followed by an LF, or a single LF not preceeded by a CR

   procedure Skip_Line (File : in out File_Handle; Spacing : in Positive := 1);
   -- Skips Spacing EOLs in File
   -- File mode must be In_File

   function End_Of_Line (File : File_Handle) return Boolean;
   -- Returns True if the next thing in File is an EOL; False otherwise
   -- File mode must be In_File

   function End_Of_File (File : File_Handle) return Boolean;
   -- Returns True if the next thing in File is an EOF; False otherwise
   -- File mode must be In_File
   -- May raise the same exceptions as Character_IO.End_Of_File

   procedure Get (File : in out File_Handle; Item : out Character);
   -- Skips any EOLs in File and then reads a single Character from File into Item
   -- May raise the same exceptions as Character_IO.Read

   procedure Put (File : in out File_Handle; Item : in Character);
   -- Writes Item to File
   -- May raise the same exceptions as Character_IO.Write

   procedure Get (File : in out File_Handle; Item : out String);
   -- Gets Item'Length Characters from File into Item
   -- May raise the same exceptions as Get for Character

   procedure Put (File : in out File_Handle; Item : in String);
   -- Puts the Characters in Item to File
   -- May raise the same exceptions as Put

   function Get_Line (File : File_Handle) return String;
   -- Gets Characters from File until an EOL is encountered
   -- Skips the EOL
   -- May raise the same exceptions as Get

   procedure Get_Line (File : in out File_Handle; Item : out String; Last : out Natural);
   -- Gets Characters from File until Item is filled or an EOL is encountered
   -- The index of the last position filled in Item is put in Last
   -- If an EOL is encountered, skips the EOL
   -- If End_Of_Line (File), Last will be Item'First - 1
   -- May raise the same exceptions as procedure Get

   procedure Put_Line (File : in out File_Handle; Item : in String);
   -- Puts Item to File followed by EOL
   -- May raise the same exceptions as Put and New_Line
private -- PragmARC.Text_IO
   type Handle_Ptr is access all File_Handle;

   type Rosen_Trick (Ptr : Handle_Ptr) is limited null record;

   type File_Handle is tagged limited record
      Handle : Rosen_Trick (Ptr => File_Handle'Unchecked_Access);
      File   : Character_IO.File_Type;
      Buffer : Character;
      Empty  : Boolean := True;
   end record;
end PragmARC.Text_IO;
