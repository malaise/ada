----------------------------------------------------------------------
-- Creation: 25/09/1988                           -- Pascal MALAISE --
-- Last update : 14/02/1989                                         --
----------------------------------------------------------------------
-- Bubble, Heapsort and Quicksort generic sorting                   --
-- Instanciation:                                                   --
--   define type typ_object of elements to sort (not limited)       --
--                   typ_index of indexes of array (discret)        --
--                   typ_array of array NOT CONTRAINED              --
--           one comparison function ("<") of two elements          --
--  package MY_SORT is new SORTS                                    --
--    (typ_object, typ_index, comparison, typ_array);               --
--                                                                  --
--  The procedures sort in crescent order (inducted by the          --
--   comparison fonction) any slice of the array                    --
--  The exception Sort_Error is raised in any exception             --
--   circumstance (i.e. memory overflow)                            --
-- WARNING : It is mandarory for the "<" function to be strict      --
--       (A < A MUST return False), other Sort_Error may be raised  --
----------------------------------------------------------------------

generic

  type Typ_Object is private;
  type Typ_Index is (<>);
  with function "<" (A, B: Typ_Object) return Boolean;
  type Typ_Array is array (Typ_Index range <>) of Typ_Object;

package Sorts is

  procedure Bubble_Sort (Slice : in out Typ_Array); -- bubble sort
  procedure Heap_Sort   (Slice : in out Typ_Array); -- heapsort
  procedure Quick_Sort  (Slice : in out Typ_Array); -- quicksort

  Sort_Error : exception;

end Sorts;

