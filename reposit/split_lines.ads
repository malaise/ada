-- Given a Parser.Iterator, extract the successive Next_Words and append
--  them (separated by a space) as long as <word> { ' ' <word> } does not
--  exceed Len, then append a new Asu_Us, up to Max Asu_Us
-- If a <word> is longer than Len, then append it alone in a As_Us
-- if Max > 0 then keep the Max first lines and, when reaching Max then set
--  the last Asu_Us to the full tail of text if not Cut
-- if Max < 0 then keep the abs(Max) last lines and set the first Asu_Us
--  to the full head of text if not Cut
-- if Cut then the extra lines are skipped
with Parser, As.U.Utils;
function Split_Lines (Iter : Parser.Iterator;
                      Len  : Positive;
                      Max  : Integer := 0;
                      Cut  : Boolean := True)
                      return As.U.Utils.Asu_Ua.Unb_Array;
