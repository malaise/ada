----------------------------------------------------------------------
-- Bubble, Heapsort and Quicksort generic sorting                   --
-- Instanciation:                                                   --
--   define type typ_object of elements to sort (not limited)       --
--               typ_index of indexes of array (discret)        --
--               typ_array of array NOT CONTRAINED              --
--           one comparison function ("<") of two elements          --
--  package My_Sort is new Sorts                                    --
--    (Typ_Object, Typ_Index, Comparison, Typ_Array);               --
--                                                                  --
--  The procedures sort any slice of the array in crescent order    --
--    (inducted by the comparison fonction)                         --
--  The exception Sort_Error is raised in any exception             --
--   circumstance (i.e. memory overflow)                            --
-- WARNING : It is mandarory for the "<" function to be strict      --
--   (A < A MUST return False), otherwise Sort_Error may be raised  --
----------------------------------------------------------------------

generic

  type Typ_Object is private;
  type Typ_Index is (<>);
  with function "<" (A, B: Typ_Object) return Boolean;
  type Typ_Array is array (Typ_Index range <>) of Typ_Object;

package Sorts is

  procedure Bubble_Sort (Slice : in out Typ_Array); -- Bubble sort
  procedure Heap_Sort   (Slice : in out Typ_Array); -- Heapsort
  procedure Quick_Sort  (Slice : in out Typ_Array); -- Quicksort

  Sort_Error : exception;

end Sorts;

