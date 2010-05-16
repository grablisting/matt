--Matt Forbes
--Feb 1, 2010
--Generic Linked List package body

with Ada.Unchecked_Deallocation;

package body Generic_Linked_List is

    --free memory that a list was using
    procedure free is new Ada.Unchecked_Deallocation(list, listPtr);

    --getter function so we can keep list implementation private
    function value(aList : listPtr) return Element_t is
    begin
	return aList.value;
    end value;

    function next(aList : listPtr) return listPtr is
    begin
	return aList.next;
    end next;
    
    --constructor
    function new_list(Item : Element_t) return listPtr is
	nList : listPtr;
    begin
	nList := new list;
	nList.next := NULL;
	return nList;
    end new_list;

    function insert_before(aList : listPtr; Item : Element_t) return listPtr is
	nList : listPtr;
    begin
	if aList = null then
	    return new_list(Item);
	end if;
	nList := new list;
	nList.value := Item;
	nList.next := aList;
	return nList;
    end insert_before;

    function insert_after(aList : listPtr; Item : Element_t) return listPtr is
	nList : listPtr;
    begin
	if aList = null then
	    return new_list(Item);
	end if;
	nList := new list;
	nList.value := Item;
	nList.next := aList.next;
	aList.next := nList;
	return nList;
    end insert_after;

    procedure insert_last(aList : in out listPtr; Item : Element_t)
    is
	iter : listPtr;
    begin
	if(aList = null) then
	    aList := new_list(Item);
	else
	    iter := aList;
	    while(iter.next /= null) loop
		iter := iter.next;
	    end loop;
	    iter := insert_after(iter, Item);
	end if;
    end insert_last;

    procedure remove(aList : in out listPtr; rList : out listPtr) is
	tmp : listPtr;
    begin
	if(aList = NULL) then
	    raise null_list;
	end if;
	tmp := aList.next;
	free(aList);
	rList := tmp;
    end remove;

    procedure remove_after(aList : in out listPtr; rList : out listPtr) is
	tmp : listPtr;
    begin
	if(aList = NULL) then
	    raise null_list;
	end if;
	tmp := aList.next.next;
	free(aList.next);
	aList.next := tmp;
	rList := aList;
    end remove_after;

    procedure clear(aList : in out listPtr) is
	tmp, next : listPtr;
    begin
	next := aList;
	while(next /= NULL) loop
	    tmp := next.next;
	    free(next);
	    next := tmp;
	end loop;
    end clear;

end Generic_Linked_list;
