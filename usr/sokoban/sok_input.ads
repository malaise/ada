package SOK_INPUT is

  type KEY_LIST is (LEFT, RIGHT, UP, DOWN, UNDO, ESC, NEXT, REFRESH);

  function GET_KEY return KEY_LIST;

  BREAK_REQUESTED : exception;

  procedure END_OF_PROGRAM;

end SOK_INPUT;
