with My_Io;
with Dyn_Data;
pragma Elaborate (Dyn_Data);
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
                       Key   : in String;
                       Data  : in Data_Access) is
        I : constant Hash_Range := Hash_Func(Key);
        Ca : Cell_Access;
      begin
        Ca := Dyn_Hash.Allocate ((Data => Data, Next  => null));

        -- Append
        if Table.Arr(I).First = null then
          Table.Arr(I).First := Ca;
          Table.Arr(I).Last := Ca;
        else
          Table.Arr(I).Last.Next := Ca;
          Table.Arr(I).Last := Ca;
        end if;

      end Store;

      -- To remove a stored association Key <-> Index
      procedure Remove (Table : in out Hash_Table;
                        Index : in Hash_Range) is
        Cp : Cell_Access;
        Cu : Cell_Access;
        Cf : Cell_Access;
      begin
        Cu := Table.Arr(Index).Current;
        Cf := Table.Arr(Index).First;
        -- Empty or not found
        if Cf = null or else Cu = null then
          raise Not_Found;
        end if;

        -- Reset current
        Table.Arr(Index).Current := null;

        if Cf = Cu then
          -- Special case when current is first
          Table.Arr(Index).First := Cf.Next;
          Cp := null;
        else
          -- Find previous of current and chain it
          Cp := Cf;
          while Cp.Next /= Cu loop
            Cp := Cp.Next;
          end loop;
          Cp.Next := Cu.Next;
        end if;

        -- Handle case when current is last
        if Table.Arr(Index).Last = Cu then
          Table.Arr(Index).Last := Cp;
        end if;

        -- Get rid of current
        Dyn_Hash.Free (Cu);

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

      -- To get next Index matching Key
      procedure Find_Next (Table : in out Hash_Table;
                           Index : in Hash_Range;
                           Found : out Found_Rec) is
        Cu : Cell_Access;
      begin
        if Table.Arr(Index).Current = null then
          Cu := Table.Arr(Index).First;
        else
          Cu := Table.Arr(Index).Current.Next;
        end if;

        Table.Arr(Index).Current := Cu;

        if Cu = null then
          Found := Not_Found_Rec;
        else
          Found := (Found => True, Data => Cu.Data);
        end if;
      end Find_Next;

      procedure Find_Next (Table : in out Hash_Table;
                           Key   : in String;
                           Found : out Found_Rec) is
      begin
        Find_Next (Table, Hash_Func(Key), Found);
      end Find_Next;

      -- To re-read data previously found at Index or Key
      procedure Re_Read (Table : in out Hash_Table;
                         Index : in Hash_Range;
                         Found : out Found_Rec) is
      begin
        if Table.Arr(Index).First = null
        or else Table.Arr(Index).Current = null then
          -- Empty or not found
          Found := Not_Found_Rec;
        else
          Found := (Found => True, Data => Table.Arr(Index).Current.Data);
        end if;
      end Re_Read;

      procedure Re_Read (Table : in out Hash_Table;
                         Key   : in String;
                         Found : out Found_Rec) is
      begin
        Re_Read (Table, Hash_Func(Key), Found);
      end Re_Read;


      -- Dump hash value of key and lists all data found for key
      procedure Dump (Table : in Hash_Table;
                      Index : in Hash_Range) is
        Ca : Cell_Access := Table.Arr(Index).First;
      begin
        My_Io.Put_Line ("Hash " & Hash_Range'Image(Index));
        if Ca = null then
          My_Io.Put_Line (" No data found");
        end if;
        while Ca /= null loop
          My_Io.Put (" Data found ");
          if Ca = Table.Arr(Index).Current then
            My_Io.Put (" => ");
          else
            My_Io.Put (" -> ");
          end if;

          Dump (Ca.Data);
          My_Io.New_Line;
          Ca := Ca.Next;
        end loop;
      end Dump;

      procedure Dump (Table : in Hash_Table;
                      Key   : in String) is
        I : constant Hash_Range := Hash_Func(Key);
      begin
        Dump (Table, I);
      end Dump;

      procedure Clear_All (Table : in out Hash_Table) is
        Ca, Cn : Cell_Access;
      begin
        for I in Hash_Range loop
          Ca := Table.Arr(I).First;
          Table.Arr(I).First := null;
          Table.Arr(I).Current := null;

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

