----------------------------------------------------------------------
-- Creation: 25/09/1988                            --Pascal MALAISE --
-- Last update : 14/02/1989                                         --
----------------------------------------------------------------------
-- tris BUBBLE, HEAPSORT and QUICKSORT generics                     --
-- INSTANCIATION:                                                   --
--   define type typ_object of elements to sort (not limited)       --
--                   typ_index of indexes of array (discret)        --
--                   typ_array of array NOT CONTRAINED              --
--           one comparison function ("<") of two elements          --
--  package MY_SORT is new SORTS                                    --
--    (typ_object, typ_index, comparison, typ_array);               --
--                                                                  --
--  The procedures sort in crescent order (inducted by the          --
--   comparison fonction) any slice of the array                    --
--  The exception SORT_ERROR is raised in any exception             --
--   circumstance (i.e. memory overflow)                            --
-- WARNING : It is mandarory for the "<" function to be strict      --
--       (A < A MUST return FALSE)                                  --
----------------------------------------------------------------------

generic

  type TYP_OBJECT is private;
  type TYP_INDEX is (<>);
  with function "<" (A, B: TYP_OBJECT) return BOOLEAN;
  type TYP_ARRAY is array (TYP_INDEX range <>) of TYP_OBJECT;

package SORTS is

  procedure BUBBLE_SORT (SLICE : in out TYP_ARRAY); -- bubble sort
  procedure HEAP_SORT   (SLICE : in out TYP_ARRAY); -- heapsort
  procedure QUICK_SORT  (SLICE : in out TYP_ARRAY); -- quicksort

  SORT_ERROR : exception;

end SORTS;
