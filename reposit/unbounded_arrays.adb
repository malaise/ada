with Ada.Unchecked_Deallocation;

package body Unbounded_Arrays is

  procedure Free (X : in out Array_Access) is
    procedure Deallocate is
       new Ada.Unchecked_Deallocation (Element_Array, Array_Access);
  begin
    --  Do not try to free statically allocated null array
    if X /= Null_Unbounded_Array.Reference then
       Deallocate (X);
    end if;
  end Free;

  procedure Check_Indexes (Low : Positive;
                           High : Positive;
                           Last : Natural;
                           Allow_Empty : in Boolean) is
  begin
    if Low <= Last and then High <= Last then
      -- Both within range => OK
      return;
    end if;
    if Allow_Empty and then Low = Last + 1 and then High <= Last then
      -- Empty indexes can be one index above range
      return;
    end if;
    raise Index_Error;
  end Check_Indexes;

  procedure Check_Index (Index : Positive;
                         Last : Natural;
                         Allow_Append : in Boolean) is
  begin
    if Index <= Last then
      -- Index within range
      return;
    end if;
    if Allow_Append and then Index = Last + 1 then
      -- Append = insert before last + 1
      return;
    end if;
    raise Index_Error;
  end Check_Index;

  -- Length and element
  function Length (Source : Unbounded_Array) return Natural is
  begin
    return Source.Reference.all'Length;
  end Length;

  function Element (Source : Unbounded_Array;
                    Index  : Positive) return Element_Type is
  begin
    if Index > Source.Reference.all'Last then
      raise Index_Error;
    end if;
    return Source.Reference.all(Index);
  end Element;

  procedure Replace_Element (Source : in out Unbounded_Array;
                             Index  : in Positive;
                             By     : in Element_Type) is
  begin
    if Index > Source.Reference.all'Last then
      raise Index_Error;
    end if;
    Source.Reference.all(Index) := By;
  end Replace_Element;

  -- Conversions
  function To_Unbounded_Array (Source : Element_Type) return Unbounded_Array is
    Res : Unbounded_Array;
  begin
    Res.Reference := new Element_Array (1 .. 1);
    Res.Reference(1) := Source;
    return Res;
  end To_Unbounded_Array;

  function To_Unbounded_Array (Source : Element_Array) return Unbounded_Array is
    Res : Unbounded_Array;
  begin
    Res.Reference := new Element_Array (1 .. Source'Length);
    Res.Reference.all := Source;
    return Res;
  end To_Unbounded_Array;

  function To_Array (Source : Unbounded_Array) return Element_Array is
  begin
    return Source.Reference.all;
  end To_Array;

  -- Prepend
  procedure Prepend (Source   : in out Unbounded_Array;
                     New_Item : in Unbounded_Array) is

  begin
    Prepend (Source, New_Item.Reference.all);
  end Prepend;

  procedure Prepend (Source   : in out Unbounded_Array;
                     New_Item : in Element_Array) is
    Acc : constant Array_Access
        := new Element_Array (1 .. New_Item'Length
                                 + Source.Reference.all'Length);

  begin
    Acc.all(1 .. New_Item'Length) := New_Item;
    Acc.all( New_Item'Length + 1 ..  Acc.all'Last) := Source.Reference.all;
    Free (Source.Reference);
    Source.Reference := Acc;
  end Prepend;

  procedure Prepend (Source   : in out Unbounded_Array;
                     New_Item : in Element_Type) is
    Acc : constant Array_Access
        := new Element_Array (1 .. 1 + Source.Reference.all'Length);
  begin
    Acc.all(1) := New_Item;
    Acc.all(2 ..  Acc.all'Last) := Source.Reference.all;
    Free (Source.Reference);
    Source.Reference := Acc;
  end Prepend;

  -- Append
  procedure Append (Source   : in out Unbounded_Array;
                    New_Item : in Unbounded_Array) is

  begin
    Append (Source, New_Item.Reference.all);
  end Append;

  procedure Append (Source   : in out Unbounded_Array;
                    New_Item : in Element_Array) is
    Acc : constant Array_Access
        := new Element_Array (1 .. Source.Reference.all'Length
                                 + New_Item'Length);

  begin
    Acc.all(1 ..  Source.Reference.all'Length) := Source.Reference.all;
    Acc.all(Source.Reference.all'Length + 1 .. Acc.all'Last) := New_Item;
    Free (Source.Reference);
    Source.Reference := Acc;
  end Append;

  procedure Append (Source   : in out Unbounded_Array;
                    New_Item : in Element_Type) is
    Acc : constant Array_Access
        := new Element_Array (1 .. Source.Reference.all'Length + 1);
  begin
    Acc.all(1 ..  Source.Reference.all'Length) := Source.Reference.all;
    Acc.all(Acc.all'Last) := New_Item;
    Free (Source.Reference);
    Source.Reference := Acc;
  end Append;

  function "&" (Left  : Unbounded_Array;
                Right : Unbounded_Array) return Unbounded_Array is
    Res : Unbounded_Array := Left;
  begin
    Append (Res, Right.Reference.all);
    return Res;
  end "&";

  function "&" (Left  : Unbounded_Array;
                Right : Element_Array) return Unbounded_Array is
    Res : Unbounded_Array := Left;
  begin
    Append (Res, Right);
    return Res;
  end "&";

  function "&" (Left  : Element_Array;
                Right : Unbounded_Array) return Unbounded_Array is
    Res : Unbounded_Array := To_Unbounded_Array (Left);
  begin
    Append (Res, Right.Reference.all);
    return Res;
  end "&";

  function "&" (Left  : Unbounded_Array;
                Right : Element_Type) return Unbounded_Array is
    Res : Unbounded_Array := Left;
  begin
    Append (Res, Right);
    return Res;
  end "&";

  function "&" (Left  : Element_Type;
                Right : Unbounded_Array) return Unbounded_Array is
    Res : Unbounded_Array;
  begin
    Res.Reference := new Element_Array (1 .. Right.Reference.all'Length + 1);
    Res.Reference.all(1) := Left;
    Res.Reference.all(2 .. Res.Reference.all'Length) := Right.Reference.all;
    return Res;
  end "&";

  -- Replace
  procedure Replace (Source   : in out Unbounded_Array;
                     Low      : in Positive;
                     High     : in Natural;
                     By       : in Unbounded_Array) is
  begin
    Replace (Source, Low, High, By.Reference.all);
  end Replace;

  procedure Replace (Source   : in out Unbounded_Array;
                     Low      : in Positive;
                     High     : in Natural;
                     By       : in Element_Array) is
    Acc : Array_Access;
  begin
    Check_Indexes (Low, High, Source.Reference.all'Length, True);
    Acc := new Element_Array (1 .. Source.Reference.all'Length
                                 - (High - Low + 1)
                                 + By'Length);
    Acc.all(1 ..  Low - 1) := Source.Reference.all (1 ..  Low - 1);
    Acc.all(Low .. Low + By'Length - 1) := By;
    Acc.all(Low + By'Length .. Acc.all'Last) :=
              Source.Reference.all(High + 1 .. Source.Reference.all'Last);
    Free (Source.Reference);
    Source.Reference := Acc;
  end Replace;

  -- Insert
  procedure Insert (Source   : in out Unbounded_Array;
                    Before   : in Positive;
                    New_Item : in Unbounded_Array) is
  begin
    Insert (Source, Before, New_Item.Reference.all);
  end Insert;

  procedure Insert (Source   : in out Unbounded_Array;
                    Before   : in Positive;
                    New_Item : in Element_Array) is
    Acc : Array_Access;
  begin
    Check_Index (Before, Source.Reference.all'Length, True);
    Acc := new Element_Array (1 .. Source.Reference.all'Length
                                 + New_Item'Length);
    Acc.all(1 ..  Before - 1) := Source.Reference.all (1 ..  Before - 1);
    Acc.all(Before .. Before + New_Item'Length - 1) := New_Item;
    Acc.all(Before + New_Item'Length .. Acc.all'Last) :=
              Source.Reference.all(Before .. Source.Reference.all'Last);
    Free (Source.Reference);
    Source.Reference := Acc;
  end Insert;

  -- Delete
  procedure Delete (Source  : in out Unbounded_Array;
                    From    : in Positive;
                    Through : in Natural) is
    Acc : Array_Access;
  begin
    Check_Indexes (From, Through, Source.Reference'Length, True);
    Acc := new Element_Array (1 .. Source.Reference.all'Length
                                 - (Through - From + 1));
    Acc.all(1 ..  From - 1) := Source.Reference.all (1 ..  From - 1);
    Acc.all(From .. Acc.all'Last) :=
              Source.Reference.all(Through + 1 .. Source.Reference.all'Last);
    Free (Source.Reference);
    Source.Reference := Acc;
  end Delete;

  -- Slice
  function Slice (Source : Unbounded_Array;
                  Low    : Positive;
                  High   : Natural) return Element_Array is
  begin
    Check_Indexes (Low, High, Source.Reference.all'Length, True);
    return Source.Reference.all(Low .. High);
  end Slice;

  function Unbounded_Slice (Source : Unbounded_Array;
                            Low    : Positive;
                            High   : Natural) return Unbounded_Array is
  begin
    Check_Indexes (Low, High, Source.Reference.all'Length, True);
    return To_Unbounded_Array (Source.Reference.all(Low .. High));
  end Unbounded_Slice;

  -- Comparisons
  function "=" (Left  : Unbounded_Array;
                Right : Unbounded_Array) return Boolean is
  begin
    return Left.Reference.all = Right.Reference.all;
  end "=";
  function "=" (Left  : Unbounded_Array;
                Right : Element_Array) return Boolean is
  begin
    return Left.Reference.all = Right;
  end "=";
  function "=" (Left  : Element_Array;
                Right : Unbounded_Array) return Boolean is
  begin
    return Left = Right.Reference.all;
  end "=";

  -- Life cycle
  overriding procedure Initialize (Object : in out Unbounded_Array) is
  begin
    Object.Reference := Null_Array'Access;
  end Initialize;
  overriding procedure Adjust (Object : in out Unbounded_Array) is
  begin
    if Object.Reference /= Null_Array'Access then
      -- Real copy
      Object.Reference := new Element_Array'(Object.Reference.all);
    end if;
  end Adjust;
  overriding procedure Finalize (Object : in out Unbounded_Array) is
  begin
    Free (Object.Reference);
    Object.Reference := Null_Array'Access;
  end Finalize;

end Unbounded_Arrays;

