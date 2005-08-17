/* $Id$ */

#define GRVERSION "15082005"
#define COMP_DATE __DATE__
#define COMP_TIME __TIME__
#define MAX_PREFIX_COUNT 50 /*This specifies the maximum number of different prefixes. e.g. Ux Rx Qx...*/
#define PAGE_JMP 100	/*There will be R101 R102 on page 1, R201 R202 on page 2.*/
#define COUNT_START 0	/*Start the counting from this number+1*/
#define MAX_PREFIX_SIZE 10 /*Define max prefx length*/

struct refdes_
	{
	char prefix[MAX_PREFIX_SIZE];
	unsigned int value;
	};

int main(int argc, char *argv[]);
int parse_refdes(struct refdes_ *refdes, char *ref_str);
void printhelp(void);
void printver(void);
