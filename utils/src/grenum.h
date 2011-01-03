/* $Id$ */
#include <stdio.h>
#define GRVERSION "24052006"
#define COMP_DATE __DATE__
#define COMP_TIME __TIME__
#define MAX_PREFIX_COUNT 50 /*This specifies the maximum number of different prefixes. e.g. Ux Rx Qx...*/
#define PAGE_JMP 100	/*There will be R101 R102 on page 1, R201 R202 on page 2.*/
#define COUNT_START 0	/*Start the counting from this number+1*/
#define MAX_PREFIX_SIZE 10 /*Define max prefx length*/
#define BUFFSIZE 256
#define FILENAMESIZE 100

/*Flag definitions*/

/* flag bits
 *
 *	bit	function
 *-----------------------
 *	0		pkgskip
 *	1		gap detected
 *	2		not implemented
 *	3		not implemented
 *	4		not implemented
 *	5		not implemented
 *	6		not implemented
 *	7		not implemented
 */

#define PAGEJUMP 0x01
#define GAP_DETECTED 0x02

/*Return status of the main program*/
#define OK 0
#define NO_INPUT_FILE 1
#define FILE_OP_ERROR 2
#define PARSE_ERROR 3
#define OUT_OF_MEMORY 4

/*Return status of get_refdes_from_file()*/
#define END_OF_FILE 2
#define NOT_REFDES_LINE 3

/*Return status of parse_refdes()*/
#define REFDES_WITH_VALUE 0
#define REFDES_WITHOUT_VALUE 1
#define REFDES_ERROR -1

/*Return status of refdes_lookup()*/
#define REFDES_NOT_FOUND -1

/*Return status of seek_value()*/
#define VALUE_FOUND 1
#define VALUE_NOT_FOUND 0

struct refdes_
	{
	char prefix[MAX_PREFIX_SIZE];
	unsigned int value;
	int prefixes;
	};

int main(int argc, char *argv[]);
int get_refdes_from_file(FILE *fp,struct refdes_ *refdes, char *buff);
int seek_value(int prefix, FILE *fp, unsigned int value, struct refdes_ *db);
int parse_refdes(struct refdes_ *refdes, char *ref_str);
int refdes_lookup(struct refdes_ *db, struct refdes_ *ref);
void printhelp(void);
void printver(void);
