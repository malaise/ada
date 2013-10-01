-- Given a Parser.Iterator, extract the successive Next_Words and append
--  them (separated by a space) as long as <word> { ' ' <word> } does not
--  exceed Len, then append a new Asu_Us
-- If a <word> is longer than Len, then append it alone in a As_Us
with Parser, As.U.Utils;
function Split_Lines (Iter : Parser.Iterator; Len : Positive)
               return As.U.Utils.Asu_Ua.Unb_Array;
