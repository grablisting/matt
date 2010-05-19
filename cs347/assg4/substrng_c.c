#include <stdio.h>
#include <stdlib.h>
#include "cdecl.h"

#define STR_SIZE 256

int PRE_CDECL substr(char *str, char *sub) POST_CDECL;

int main(int argc, char **argv)
{
	char *str, *sub;
	size_t strsize = STR_SIZE;
	int subpos;
	int a;

	str = (char *)malloc(sizeof(char) * STR_SIZE);
	sub = (char *)malloc(sizeof(char) * STR_SIZE);

	printf("%s", "Enter a string: ");
	a = getline(&str, &strsize, stdin);
	str[a-1] = 0;

	printf("%s", "Enter a sub string: ");
	a = getline(&sub, &strsize, stdin);
	sub[a-1] = 0;

	subpos = substr(str, sub);

	if(subpos > -1)
		printf("Found match at position: %d\n", subpos);
	else
		printf("Did not find match\n");

	free(str);
	free(sub);

	return 0;
}
