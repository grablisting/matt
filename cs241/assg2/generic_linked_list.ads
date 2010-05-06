--Matt Forbes
--Feb 1, 2010
--Generic Linked List specification

generic
    type Element_t is private;
package Generic_Linked_List is

    type list is private;
    type listPtr is access list;
    
    null_list : exception;
    
    --returns an empty list
    function new_list(Item : Element_T) return listPtr;

    --gets the value from a list
    function value(aList : listPtr) return Element_t;

    --gets the next listPtr from a list
    function next(aList : listPtr) return listPtr;

    --insert an element before this list, and return a pointer to the new list
    function insert_before(aList : listPtr; Item : Element_t) return listPtr;

    --insert an element after this list, and return the new list
    function insert_after(aList : listPtr; Item : Element_t) return listPtr;

    procedure insert_last(aList : in out listPtr; Item : Element_t);

    --remove this list and return the value it was pointing to
    procedure remove(aList : in out listPtr; rList : out listPtr);

    --remove after this list and return the original list
    procedure remove_after (aList : in out listPtr; rList : out listPtr);

    --delete this list recursively
    procedure clear(aList : in out listPtr);

private
    type list is 
	record
	    value : Element_t;
	    next : listPtr;
	end record;

end Generic_Linked_List;
