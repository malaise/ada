with Xml_Parser;

separate (Channels)

package body File is

  Ctx : Xml_Parser.Ctx_Type;
  Chn : Xml_Parser.Element_Type;
  Nod : Xml_Parser.Element_Type;

  procedure Open (File_Name : in String; Channel_Name : in String) is
    Root : Xml_Parser.Element_Type;
    Ok : Boolean;
  begin
    Chn := Xml_Parser.No_Node;
    -- Open and parse file
    Ctx.Parse (File_Name, Ok);
    if not Ok then
      raise File_Error;
    end if;

    -- Look for requested Channel if found
    Root := Ctx.Get_Root_Element;
    declare
      Channels : constant Xml_Parser.Nodes_Array
               := Ctx.Get_Children (Root);
      Name : Xml_Parser.Attribute_Rec;
    begin
      for I in Channels'Range loop
        Name := Ctx.Get_Attribute (Channels(I), 1);
        if Asu_Ts (Name.Value) = Channel_Name then
          Chn := Channels(I);
          Nod := Xml_Parser.No_Node;
          exit;
        end if;
      end loop;
    end;
  exception
    when others =>
      raise File_Error;
  end Open;

  procedure Close is
  begin
    Ctx.Clean;
    Chn := Xml_Parser.No_Node;
  exception
     when others =>
      raise File_Error;
  end Close;

  function Next_Host return Tcp_Util.Remote_Host is
    Host : Tcp_Util.Remote_Host;
    Txt : Xml_Parser.Text_Type;
    use type Xml_Parser.Element_Type;
  begin
    if Chn = Xml_Parser.No_Node then
      raise File_Error;
    end if;
    begin
      if Nod = Xml_Parser.No_Node then
        -- First call => First Host
        Nod := Ctx.Get_Child (Chn, 1);
      else
        -- Next Host
        Nod := Ctx.Get_Brother (Nod);
      end if;
    exception
      when Xml_Parser.Invalid_Index | Xml_Parser.No_Brother =>
        -- No (more) Host
        raise End_Error;
    end;

    -- The Text node
    Txt := Ctx.Get_Child (Nod, 1);
    Host.Name := Ctx.Get_Text (Txt);
    return Host;

  exception
    when End_Error =>
      raise;
    when others =>
      raise File_Error;

  end Next_Host;


end File;

