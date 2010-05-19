// Matt Forbes
// CSCI 347 - Assignment 4
// Main program that calls assembly subprogram substr

#include <stdio.h>
#include <stdlib.h>
#include "cdecl.h"

#define STR_SIZE 256

// Declare assembly subprogram prototype
int PRE_CDECL substr(char *str, char *sub) POST_CDECL;

int main(int argc, char **argv)
{
	char *str, *sub;
	int subpos;

	// allocate space for strings
	str = (char *)malloc(sizeof(char) * STR_SIZE);
	sub = (char *)malloc(sizeof(char) * STR_SIZE);

	// get string, substring
	printf("%s", "Enter a string: ");
	scanf("%s", str);

	printf("%s", "Enter a sub string: ");
	scanf("%s", sub);

	// call substr from assembly
	subpos = substr(str, sub);

	// print out appropriate message
	if(subpos > -1)
		printf("Found match at position: %d\n", subpos);
	else
		printf("Did not find match\n");

	// clean up
	free(str);
	free(sub);

	return 0;
}
