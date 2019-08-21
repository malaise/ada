with Basic_Proc, Dyn_Data;
pragma Elaborate_All (Dyn_Data);
package body Hashing is

  -- The function to hash at default size default
  function Def_Max_Hash_Func (Key : String) return Def_Max_Hash_Range is
    use type Hash_Function.Hash_Range;
  begin
    return Hash_Function.Hash (Key) rem Def_Max_Hash_Value;
  end Def_Max_Hash_Func;

  package body Sized_Hash is

    -- Call Hash and trunk
    function Hash_Def_Func (Key : String) return Hash_Range is
      use type Hash_Function.Hash_Range;
    begin
      return Hash_Function.Hash (Key) rem Hash_Max;
    end Hash_Def_Func;

    package body Hash_Mng is

      Not_Found_Rec : constant Found_Rec
                    := (Found => False);

      -- To manage the linked cells
      package Dyn_Hash is new Dyn_Data (Cell_Rec, Cell_Access);

      -- To store association Key <-> Index
      procedure Store (Table : in out Hash_Table;
                       Index : in Hash_Range;
                       Data  : in Data_Access;
                       Where : in Where_Insert_List := Last) is
        Ca : Cell_Access;
        Cu : Cell_Access;
        use type Depth_Range;
      begin
        Ca := Dyn_Hash.Allocate ((Data => Data, Prev => null, Next => null));

        Cu := Table.Arr(Index).Current;
        if Table.Arr(Index).First = null then
          if Where = After_Curr or else Where = Before_Curr then
            raise Not_Found;
          end if;
        else
          case Where is
            when First =>
              Ca.Next := Table.Arr(Index).First;
            when Last =>
              Ca.Prev := Table.Arr(Index).Last;
            when After_Curr =>
              if Cu = null then
                raise Not_Found;
              end if;
              Ca.Next := Cu.Next;
              Ca.Prev := Cu;
            when Before_Curr =>
              if Cu = null then
                raise Not_Found;
              end if;
              Ca.Next := Cu;
              Ca.Prev := Cu.Prev;
          end case;
        end if;
        -- Link brothers (or first/last) to ca
        if Ca.Prev /= null then
          Ca.Prev.Next := Ca;
        else
          Table.Arr(Index).First := Ca;
        end if;
        if Ca.Next /= null then
          Ca.Next.Prev := Ca;
        else
          Table.Arr(Index).Last := Ca;
        end if;
        -- Count items in branch
        if Table.Arr(Index).Depth = Depth_Range'Last then
          raise Too_Many;
        end if;
        Table.Arr(Index).Depth := Table.Arr(Index).Depth + 1;
      end Store;

      procedure Store (Table : in out Hash_Table;
                       Key   : in String;
                       Data  : in Data_Access;
                       Where : in Where_Insert_List := Last) is
      begin
        Store (Table, Hash_Func(Key), Data, Where);
      end Store;

      -- To remove a stored association Key <-> Index
      procedure Remove (Table : in out Hash_Table;
                        Index : in Hash_Range) is
        Cu : Cell_Access;
        use type Depth_Range;
      begin
        Cu := Table.Arr(Index).Current;
        -- Empty or not found
        if Table.Arr(Index).First = null or else Cu = null then
          raise Not_Found;
        end if;

        -- Reset current
        Table.Arr(Index).Current := null;

        -- Disconnect
        if Cu.Next /= null then
          Cu.Next.Prev := Cu.Prev;
        else
          Table.Arr(Index).Last := Cu.Prev;
        end if;
        if Cu.Prev /= null then
          Cu.Prev.Next := Cu.Next;
        else
          Table.Arr(Index).First := Cu.Next;
        end if;

        -- Get rid of current
        Dyn_Hash.Free (Cu);

        Table.Arr(Index).Depth := Table.Arr(Index).Depth - 1;
      end Remove;

      procedure Remove (Table : in out Hash_Table;
                        Key   : in String) is
        I : constant Hash_Range := Hash_Func(Key);
      begin
        Remove (Table, I);
      end Remove;

      -- To reset finding index for matching Key
      procedure Reset_Find (Table : in out Hash_Table; Index : in Hash_Range) is
      begin
        Table.Arr(Index).Current := null;
      end Reset_Find;

      procedure Reset_Find (Table : in out Hash_Table;
                            Key   : in String) is
      begin
        Reset_Find (Table, Hash_Func(Key));
      end Reset_Find;

      -- Depth
      function Depth (Table : Hash_Table; Index : Hash_Range)
                     return Depth_Range is
        (Table.Arr(Index).Depth);
      function Depth (Table : Hash_Table; Key   : String)
                     return Depth_Range is
        (Table.Arr(Hash_Func(Key)).Depth);

      -- To get next Index matching Key
      procedure Find_Next (Table     : in out Hash_Table;
                           Index     : in Hash_Range;
                           Found     : out Found_Rec;
                           Direction : in Direction_List := Forward) is
      begin
        Found := Find_Next (Table, Index, Direction);
      end Find_Next;

      function Find_Next (Table     : in out Hash_Table;
                          Index     : in Hash_Range;
                          Direction : in Direction_List := Forward)
                return Found_Rec is
        Cu : Cell_Access;
      begin
        if Table.Arr(Index).Current = null then
          Cu := (if Direction = Forward then Table.Arr(Index).First
                 else Table.Arr(Index).Last);
        else
          Cu := (if Direction = Forward then Table.Arr(Index).Current.Next
                 else Table.Arr(Index).Current.Prev);
        end if;

        Table.Arr(Index).Current := Cu;

        return (if Cu = null then Not_Found_Rec
                else (Found => True, Data => Cu.Data));
      end Find_Next;

      procedure Find_Next (Table     : in out Hash_Table;
                           Key       : in String;
                           Found     : out Found_Rec;
                           Direction : in Direction_List := Forward) is
      begin
        Found := Find_Next (Table, Hash_Func(Key), Direction);
      end Find_Next;

      function Find_Next (Table     : in out Hash_Table;
                          Key       : in String;
                          Direction : in Direction_List := Forward)
               return Found_Rec is
        (Find_Next (Table, Hash_Func(Key), Direction));

      -- To re-read data previously found at Index or Key
      procedure Re_Read (Table : in out Hash_Table;
                         Index : in Hash_Range;
                         Found : out Found_Rec) is
      begin
        Found := Re_Read (Table, Index);
      end Re_Read;

      function Re_Read (Table : in out Hash_Table;
                        Index : in Hash_Range) return Found_Rec is
        (if Table.Arr(Index).First = null
            or else Table.Arr(Index).Current = null then
              -- Empty or not found
              Not_Found_Rec
         else
           (Found => True, Data => Table.Arr(Index).Current.Data));

      procedure Re_Read (Table : in out Hash_Table;
                         Key   : in String;
                         Found : out Found_Rec) is
      begin
        Found := Re_Read (Table, Hash_Func(Key));
      end Re_Read;

      function Re_Read (Table : in out Hash_Table;
                        Key   : in String) return Found_Rec is
        (Re_Read (Table, Hash_Func(Key)));

      -- Dump hash value of key and lists all data found for key
      procedure Dump (Table     : in Hash_Table;
                      Index     : in Hash_Range;
                      Put       : not null access
                                      procedure (Data : in Data_Access);
                      Direction : in Direction_List := Forward) is
        Ca : Cell_Access := Table.Arr(Index).First;
      begin
        Basic_Proc.Put_Line_Output ("Hash " & Hash_Range'Image(Index));
        Ca := (if Direction = Forward then Table.Arr(Index).First
               else Table.Arr(Index).Last);
        if Ca = null then
          Basic_Proc.Put_Line_Output (" No data found");
        end if;
        while Ca /= null loop
          Basic_Proc.Put_Output (" Data found "
              & (if Ca = Table.Arr(Index).Current then " => " else " -> "));

          Put (Ca.Data);
          Basic_Proc.New_Line_Output;
          Ca := (if Direction = Forward then Ca.Next else Ca.Prev);
        end loop;
      end Dump;

      procedure Dump (Table     : in Hash_Table;
                      Key       : in String;
                      Put       : not null access
                                      procedure (Data : in Data_Access);
                      Direction : in Direction_List := Forward) is
        I : constant Hash_Range := Hash_Func(Key);
      begin
        Dump (Table, I, Put, Direction);
      end Dump;

      procedure Clear_All (Table : in out Hash_Table) is
        Ca, Cn : Cell_Access;
      begin
        for Hn of Table.Arr loop
          Ca := Hn.First;
          Hn.First := null;
          Hn.Current := null;

          while Ca /= null loop
            Cn := Ca.Next;
            Dyn_Hash.Free (Ca);
            Ca := Cn;
          end loop;
        end loop;
      end Clear_All;

    end Hash_Mng;

  end Sized_Hash;

end Hashing;

