with As.U;
with Entities, Lister;
package Output is

  -- Set (store) sorting and format style
  type Sort_Kind_List is (Alpha, Len, None, Size, Time);
  type Format_Kind_List is (Simple, One_Row, Long, Long_Human);
  procedure Set_Style (Sort_Kind   : in Sort_Kind_List;
                       Revert      : in Boolean;
                       Format_Kind : in Format_Kind_List;
                       Put_Path    : in Boolean;
                       Full_Path   : in Boolean;
                       Classify    : in Boolean;
                       Date_Iso    : in Boolean;
                       Quiet       : in Boolean;
                       Separator   : in As.U.Asu_Us);

  -- Sort list and put according to style
  procedure Put (List : in out Entities.Entity_List;
                 Append_New_Line : in Boolean);

  -- Put a dir name ("Dir:")
  procedure Put_Dir (Name : in String);

  -- Put a New_line
  procedure New_Line;

  -- Put Total size, no new lin
  procedure Put_Line_Size (Number : in Natural; Size : in Lister.Size_Type);

end Output;

