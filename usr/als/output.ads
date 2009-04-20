with Ada.Strings.Unbounded;
with Entities, Lister;
package Output is

  -- Set (store) sorting and format style
  type Sort_Kind_List is (Alpha, Time, Size);
  type Format_Kind_List is (Simple, One_Row, Long, Long_Human);
  procedure Set_Style (Sort_Kind : in Sort_Kind_List;
                       Revert : in Boolean;
                       Format_Kind : in Format_Kind_List;
                       Put_Path : in Boolean;
                       Classify : in Boolean;
                       Separator : Ada.Strings.Unbounded.Unbounded_String);

  -- Sort list and put according to style
  procedure Put (List : in out Entities.Entity_List);

  -- Put a dir name ("Dir:")
  procedure Put_Dir (Name : in String);

  -- Put a New_line
  procedure New_Line;

  -- Put Total size, no new lin
  procedure Put_Size (Size : in Lister.Size_Type);

end Output;

