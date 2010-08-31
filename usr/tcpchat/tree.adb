with Basic_Proc, Xml_Parser;
with Debug;
package body Tree is

  -- Parse file and build tree
  procedure Parse (File_Name : in Asu_Us) is
    Ctx : Xml_Parser.Ctx_Type;
    Ok : Boolean;
  begin
    begin
      Ctx.Parse (Asu_Ts (File_Name), Ok);
      if not Ok then
        Basic_Proc.Put_Line_Error ("XML ERROR: "
             & Ctx.Get_Parse_Error_Message & ".");
        raise Parse_Error;
      end if;
    exception
      when Xml_Parser.File_Error =>
        Basic_Proc.Put_Line_Error ("XML ERROR: Cannot access to file "
          & Asu_Ts (File_Name));
        raise Parse_Error;
    end;
    Debug.Log ("File " & Asu_Ts (File_Name) & " parsed OK.");

    -- Build Tree
    -- @@@
      
  end Parse;


  -- Kind of node
  -- type Node_Kind is (Selec, Read, Default, Skip, Wait, Send, Call, Exec, Close);

  -- Infinite timeout
  -- Infinite_Ms : constant Integer := Event_Mng.Infinite_Ms;

  -- Node
  -- type Node_Rec;

  -- type Node_Access is access all Node_Rec;

  -- type Node_Rec is record
  --   Kind : Node_Kind := Close;
    -- For chat (kind Read)
  --   Name : Asu_Us;
    -- For Read, Send, Call or Exec
  --   Text : Asu_Us;
    -- For Selec, Read, Skip, Wait
  --   Timeout : Integer := Infinite_Ms;
    -- Next node
  --   Next : Node_Access := null;
  -- end record;

end Tree;

