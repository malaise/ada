with Game;
-- From the action and the result (Cr being column and row)
-- Move      -> Cr-Cr
-- Take      -> CrXCr
-- EnPassant -> CrXCrep
-- Castle    -> o-o
--           or o-o-o
-- Promotion -> Cr-Cr=K    (K is one letter: kind of new piece)
--           or CrXCr=K
-- Plus extra:
-- Check        +
-- Checkmate    ++
-- Stalemate    ==
-- -- Which make 9 characters max
function Image (Action : Game.Action_Rec;
                Result : Game.Move_Status_List) return String;

