/* pincounts.h */

typedef struct st_pincount PINCOUNT;

struct st_pincount {
	char *footprint;
	int pincount;
};

static PINCOUNT p_pincounts[] = {
	{ "footprint=TO92", 3 },
	{ "footprint=TO-220AB", 3 },
	{ NULL, 3 },
	};


int s_get_footprint_size(char *footprint)
{
  PINCOUNT *pcount = p_pincounts;
  char *tmp;
	
  while(pcount->footprint != NULL) {
    if (!strcasecmp(footprint, pcount->footprint))
      return pcount->pincount;
    pcount++;
  }

  tmp = &footprint[strlen(footprint)-1];

  while((*tmp >= '0') && (*tmp <= '9'))
  {
    if (tmp == footprint)
      return -1;
    tmp--;
  }
  tmp++;

  if (*tmp == '\0')
    return -1;
 
  return atoi(tmp);
}

