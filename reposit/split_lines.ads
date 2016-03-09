-- Given a Parser.Iterator, extract the successive Next_Words and append
--  them (separated by a space) as long as <word> { ' ' <word> } does not
--  exceed Len, then append a new Asu_Us starting by Indent, up to Max Asu_Us
-- If a <word> is longer than Len, then append it alone in a As_Us
-- if Max > 0 then keep the Max first lines
-- if Max < 0 then keep the abs(Max) last lines
with Parser, As.U.Utils;
function Split_Lines (Iter   : in out Parser.Iterator;
                      Len    : Positive;
                      Indent : String;
                      Max    : Integer := 0)
         return As.U.Utils.Asu_Ua.Unb_Array;
