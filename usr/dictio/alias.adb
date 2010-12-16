with Parser, Text_Handler;
with Parse, Names, Dictio_Debug;
package body Alias is

  subtype Tmp_Txt is Text_Handler.Text(Data_Base.Item_Data'Length);

  -- Get the highest alias for a name
  -- For a.b.c.d, look for a, then a.b, then a.b.c...
  -- Return Empty if no alias found
  Len_Error : exception;
  procedure Best_Alias (Name : in out Tmp_Txt) is
    Look_Item, Got_Item : Data_Base.Item_Rec;
    Iter : Parser.Iterator;
    Searching : Boolean;
    Tail : Tmp_Txt;
    use type Data_Base.Item_Rec;
  begin

    -- Init parsing
    Parser.Set (Iter, Name.Value, Names.Is_Sep'Access);
    Name.Empty;
    Searching := True;

    loop
      -- Get next word of Name
      declare
        Word : constant String := Parser.Next_Word (Iter);
      begin
        exit when Word = "";
        if Searching then
          -- Append word to pattern if searching
          Name.Append (Word);
        else
          -- Append word to the tail that will be appended to alias
          Tail.Append (Word);
        end if;
      end;

      if Searching then
        -- Look for alias
        Look_Item := Data_Base.No_Item;
        Look_Item.Name(1 .. Name.Length) := Name.Value;

        Data_Base.Get (Look_Item.Name, Data_Base.Alias_Kind, Got_Item);
        if Got_Item.Data_Len /= 0 then
          -- Got an active alias
          Searching := False;
        else
          -- Prepare to append next word
          Name.Append (Names.Sep);
        end if;
      end if;

    end loop;

    -- Finish
    Parser.Del (Iter);

    if Searching then
      -- Nothing found
      Name.Empty;
    else
      -- Found. Append tail to found alias
      Name.Set (Got_Item.Data(1 .. Got_Item.Data_Len));
      if not Tail.Is_Empty then
        Name.Append (Names.Sep);
        Name.Append (Tail);
      end if;
    end if;

  exception
    when Constraint_Error =>
      -- While appending
      Name.Empty;
      raise Len_Error;
  end Best_Alias;


  -- Resolves an alias
  procedure Resolve (Item : in out Data_Base.Item_Rec) is
    Ini_Txt, Cur_Txt, Got_Txt : Tmp_Txt;
    use type Text_Handler.Text;
  begin
    -- Do not resolve aliases
    if Item.Kind = Data_Base.Alias_Kind then
      return;
    end if;

    if Dictio_Debug.Level_Array(Dictio_Debug.Client_Alias) then
      Dictio_Debug.Put ("Client-alias.resolve: " & Parse(Item.Name));
    end if;

    -- Resolve aliases one by one
    Ini_Txt.Set (Parse(Item.Name));
    Cur_Txt.Set (Ini_Txt);
    loop
      -- Get the highest alias
      Got_Txt.Set (Cur_Txt);
      Best_Alias (Got_Txt);
      -- No alias
      exit when Got_Txt.Is_Empty;
      -- Loop detected
      if Got_Txt = Ini_Txt then
        Dictio_Debug.Put ("Client-alias.resolving loop: " & Ini_Txt.Value);
        Cur_Txt.Empty;
        exit;
      end if;
      -- Switch to this one
      Cur_Txt.Set (Got_Txt);
      if Dictio_Debug.Level_Array(Dictio_Debug.Client_Alias) then
        Dictio_Debug.Put ("Client-alias.resolving: " & Cur_Txt.Value);
      end if;
    end loop;

    -- Accept final alias if one got and not too long
    if not Cur_Txt.Is_Empty
    and then Cur_Txt.Length <= Item.Name'Length then
      Item.Name := (others => ' ');
      Item.Name(1 .. Cur_Txt.Length) := Cur_Txt.Value;
    end if;

    if Dictio_Debug.Level_Array(Dictio_Debug.Client_Alias) then
      Dictio_Debug.Put ("Client-alias.resolved: " & Parse(Item.Name));
    end if;

  exception
    when Len_Error =>
      -- Keep Item unchanged
      if Dictio_Debug.Level_Array(Dictio_Debug.Client_Alias) then
        Dictio_Debug.Put ("Client-alias.resolve len error on: " & Parse(Item.Name));
      end if;
  end Resolve;

end Alias;

