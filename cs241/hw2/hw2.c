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
    char *head = text;
    char *search1, *search2;
    char a,b;
    int subLen=0;

    search1=sub;
    while(*search1)
    {
	search1++;
	subLen++;
    }

    while(*(head+subLen-1))
    {
	search1 = head;
	search2 = sub;
	while(1)
	{
	    a = *search1;
	    b = *search2;
	    if(!b)
		return 1;
	    if(a != b)
		break;
	    search1++;
	    search2++;
	}
	head++;
    }
   return 0; 

}

int main(int argc, const char *argv[])
{
	char *text = (char *)malloc(sizeof(char)*TEXT_SIZE);
	char *sub = (char *)malloc(sizeof(char)*TEXT_SIZE);
	int textLen, subLen, isSubstr;
	size_t maxSize = TEXT_SIZE;
	clock_t before, after;
	double hash=0, brute=0;


	printf("main text: ");
	textLen = getline(&text, &maxSize, stdin);
	text[textLen-1] = 0;

	printf("substring: ");
	subLen = getline(&sub, &maxSize, stdin);
	sub[subLen-1] = 0;

	before = clock();
	isSubstr = substr(text, sub);
	after = clock();
	brute = after - before;

	before = clock();
	isSubstr = substr1(text, sub);
	after = clock();
	hash = after - before;

	printf("hash: %lf\nbrute: %lf\n", hash, brute);

	printf("%s is faster\n", (hash < brute) ? "hash" : (hash == brute) ? "neither" : "brute force");

	printf("found? %s\n", isSubstr ? "true" : "false");

	return 0;
}
