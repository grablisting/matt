#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#define TEXT_SIZE 256

int h(int ascii)
{
    return ascii % TEXT_SIZE;
}

int substr(char *text, char *sub)
{
    int subLen=0, subHash=0, subAscii = 0;
    int i=0, first=0, equal=0;
    int ascii=0;
    char *iter = sub;
    char *c, *d;

    while(*iter)
    {
	subAscii += *iter;
	iter++;
	subLen++;
    }

    subHash = h(subAscii);

    first = text[subLen-1];
    for( i=0; i<subLen; i++)
    {
	ascii += text[i];
    }

    iter = text;
    while(*(iter+subLen-1))
    {
	ascii -= first;
	ascii += *(iter+subLen-1);

	first = *iter;

	if(h(ascii) == subHash)
	{
	    c = iter;
	    d = sub;
	    equal = 1;
	    for( i=0; i<subLen; i++)
	    {
		if(*c != *d)
		{
		    equal = 0;
		    break;
		}
		c++;
		d++;
	    }
	    if(equal)
	    {
		return 1;
	    }
	}
	iter++;
    }

    return 0;
}

int substr1(char *text, char *sub)
{
    char *c, *d;
    

}

int main(int argc, const char *argv[])
{
	char *text = (char *)malloc(sizeof(char)*TEXT_SIZE);
	char *sub = (char *)malloc(sizeof(char)*TEXT_SIZE);
	int textLen, subLen, isSubstr;
	size_t maxSize = TEXT_SIZE;


	printf("main text: ");
	textLen = getline(&text, &maxSize, stdin);
	text[textLen-1] = 0;

	printf("substring: ");
	subLen = getline(&sub, &maxSize, stdin);
	sub[subLen-1] = 0;

	isSubstr = substr(text, sub);
	printf("Found? %s\n", isSubstr ? "true" : "false");

	return 0;
}
