with TEXT_IO;
with DYNAMIC_LIST, NORMAL, RND;
procedure T_DL is
  package MY_LIST is new DYNAMIC_LIST(ELEMENT_TYPE => INTEGER);
  procedure MY_SEARCH is new MY_LIST.SEARCH;   -- ("=" of INTEGER)
  procedure MY_SORT is new MY_LIST.SORT("<");  -- ("<" of INTEGER)

  LIST : MY_LIST.LIST_TYPE;
  ITEM : INTEGER;

  procedure PUT (I : in INTEGER; NEW_LINE : in BOOLEAN := FALSE) is
  begin
    TEXT_IO.PUT (NORMAL (I, 2, GAP => '0') & ' ');
    if NEW_LINE then TEXT_IO.NEW_LINE; end if;
  end PUT;

  procedure DUMP is
    POS : NATURAL;
  begin
    if MY_LIST.LIST_LENGTH(LIST) = 0 then
      TEXT_IO.NEW_LINE;
      return;
    end if;

    POS := MY_LIST.GET_POSITION (LIST);
    MY_LIST.MOVE_TO (LIST, MY_LIST.NEXT, 0, FALSE);
    loop
      MY_LIST.READ (LIST, ITEM, MY_LIST.NEXT);
      PUT (ITEM);
    end loop;
  exception
    when MY_LIST.NOT_IN_LIST =>
      MY_LIST.READ (LIST, ITEM, MY_LIST.CURRENT);
      PUT (ITEM, TRUE);
      MY_LIST.MOVE_TO (LIST, MY_LIST.NEXT, POS-1, FALSE);
  end DUMP;

begin

  -- add 10 elements to the list
  TEXT_IO.PUT_LINE("Adds 10 elements");
  for I in 1 .. 10 loop
    MY_LIST.INSERT(LIST, I, MY_LIST.NEXT);
  end loop;

  -- read 5 elements from list in reverse
  TEXT_IO.PUT("Reads 5 elements from the last one: ");
  MY_LIST.MOVE_TO(LIST, MY_LIST.PREV, 0, FALSE);
  for I in 1 .. 5 loop
    MY_LIST.READ(LIST, ITEM, MY_LIST.PREV);
    PUT(ITEM);
  end loop;
  TEXT_IO.NEW_LINE;

  -- dump
  TEXT_IO.PUT("List length: ");
  PUT(MY_LIST.LIST_LENGTH(LIST), TRUE);

  -- delete 5th
  TEXT_IO.PUT_LINE("Deletes the current");
  MY_LIST.DELETE(LIST);

  -- list length
  TEXT_IO.PUT("List length: ");
  PUT(MY_LIST.LIST_LENGTH(LIST), TRUE);

  -- read 7 elements from first
  TEXT_IO.PUT("Reads 7 elements from the first one: ");
  MY_LIST.MOVE_TO(LIST, MY_LIST.NEXT, 0, FALSE);
  for I in 1 .. 7 loop
    MY_LIST.READ(LIST, ITEM);
    PUT(ITEM);
  end loop;
  TEXT_IO.NEW_LINE;

  -- add 50 before current
  TEXT_IO.PUT_LINE("Adds the element 50 before current position");
  MY_LIST.INSERT(LIST, 50, MY_LIST.PREV);

  -- list length
  TEXT_IO.PUT("List length: ");
  PUT(MY_LIST.LIST_LENGTH(LIST), TRUE);

  -- read 9 elements from the last
  TEXT_IO.PUT("Reads 9 elements from the last one: ");
  MY_LIST.MOVE_TO(LIST, MY_LIST.PREV, 0, FALSE);
  for I in 1 .. 9 loop
    MY_LIST.READ(LIST, ITEM, MY_LIST.PREV);
    PUT(ITEM);
  end loop;
  TEXT_IO.NEW_LINE;

  -- permute 1st and 4th elements, then search 3 from last
  TEXT_IO.PUT_LINE("Permute 1st and 4th elements, then seach 3 from last");
  MY_LIST.PERMUTE (LIST, 0, 3, MY_LIST.NEXT, FALSE);
  MY_SEARCH (LIST, 3, MY_LIST.PREV, 1, FALSE);

  -- get pos from first and current item
  TEXT_IO.PUT("Get current pos from first: ");
  PUT(MY_LIST.GET_POSITION (LIST));
  TEXT_IO.PUT(" Get current item: ");
  MY_LIST.GET (LIST, ITEM);
  PUT(ITEM, TRUE);

  -- dump
  TEXT_IO.PUT("List (length: ");
  PUT (MY_LIST.LIST_LENGTH(LIST), FALSE);
  TEXT_IO.PUT (") : ");
  DUMP;

  -- search 50 from first
  TEXT_IO.PUT_LINE("Seach 50 from first");
  begin
    MY_SEARCH(LIST, 50, MY_LIST.NEXT, 1, FALSE);
  exception
    when MY_LIST.NOT_IN_LIST => TEXT_IO.PUT_LINE ("NOT IN LIST");
  end;

  -- dump the list
  begin
    loop
      TEXT_IO.PUT("Pos from first: ");
      PUT (MY_LIST.GET_POSITION (LIST), FALSE);
      TEXT_IO.PUT("Pos from last: ");
      PUT (MY_LIST.GET_POSITION (LIST, MY_LIST.FROM_LAST), FALSE);
      TEXT_IO.PUT("Current item, go to next: ");
      MY_LIST.READ(LIST, ITEM);
      PUT(ITEM, TRUE);
    end loop;
  exception
    when MY_LIST.NOT_IN_LIST =>
      TEXT_IO.PUT_LINE("NOT IN LIST");
  end;

  TEXT_IO.PUT("Pos from first: ");
  PUT(MY_LIST.GET_POSITION (LIST), FALSE);
  TEXT_IO.PUT("Pos from last: ");
  PUT(MY_LIST.GET_POSITION (LIST, MY_LIST.FROM_LAST), FALSE);
  TEXT_IO.PUT("Current item, stay: ");
  MY_LIST.READ(LIST, ITEM, MY_LIST.CURRENT);
  PUT(ITEM, TRUE);

  TEXT_IO.PUT("Current item, stay: ");
  MY_LIST.READ(LIST, ITEM, MY_LIST.CURRENT);
  PUT(ITEM, TRUE);

  TEXT_IO.PUT_LINE("Delete fully the list");
  MY_LIST.DELETE_LIST (LIST);

  TEXT_IO.PUT("Get current pos from first: ");
  begin
    PUT(MY_LIST.GET_POSITION(LIST), TRUE);
  exception
    when MY_LIST.EMPTY_LIST => TEXT_IO.PUT_LINE("EMPTY LIST");
  end;

  -- list length
  TEXT_IO.PUT("List length: ");
  PUT(MY_LIST.LIST_LENGTH(LIST), TRUE);

  TEXT_IO.PUT ("Make the following random list: ");
  for I in 1 .. RND.INT_RANDOM (0, 10) loop
    MY_LIST.INSERT (LIST, RND.INT_RANDOM(0, 50));
  end loop;
  DUMP;
  MY_SORT(LIST);
  TEXT_IO.PUT ("After sorting it: ");
  DUMP;


end T_DL;
