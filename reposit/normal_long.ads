-- Puts an Integer in a string of fixed length.
-- If I is shorter than max, it is aligned at right or left
-- If I is longer, it is rounded if possible (or truncated)

-- I : Integer value to put in the returned string
-- Len : Number of characters of the returned string
-- Right : If string is shorter than Len character, align it at Right
--   or at left (not Right) and fill with Gap
-- Gap : When string is shorter than len, fill empty positions with Gap
with Normal_Gen;
function Normal_Long is new Normal_Gen (Long_Long_Integer);

