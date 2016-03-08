-- Puts an int I in a string of fixed length Len.
-- If I fits in Len, then it is aligned at Right or left, and Len is
--  completed (left or Right) with the Gap character
-- If I is longer, then it is rounded to fit in Len - 1 and a
--  warning char ('!') is prepended (if Right) or appended (if left)
with Normalization;
function Normal is new Normalization.Normal_Gen (Integer);

