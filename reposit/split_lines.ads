-- Given a Parser.Iterator, extract the successive Next_Words and append
--  them (separated by a space) as long as <word> { ' ' <word> } does not
--  exceed Len, then append a new Asu_Us, up to Max Asu_Us
-- If a <word> is longer than Len, then append it alone in a As_Us
-- When reaching Max then set last Asu_Us to the full tail of text
with Parser, As.U.Utils;
function Split_Lines (Iter : Parser.Iterator;
                      Len  : Positive;
                      Max : Natural := 0) return As.U.Utils.Asu_Ua.Unb_Array;
