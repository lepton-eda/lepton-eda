
/* sym check structures (gsymcheck) */
typedef struct st_symcheck SYMCHECK;

/* gsymcheck structure */
struct st_symcheck {
  int graphical_symbol;
  int missing_device_attrib;
  char *device_attribute;
  int device_attribute_incorrect;
  int missing_pinseq_attrib;
  int multiple_pinseq_attrib;
  int missing_pinnumber_attrib;
  int multiple_pinnumber_attrib;
  int missing_numslots_attrib;
  int found_oldpin_attrib;
  int found_oldslot_attrib;
  int unattached_attribs;
};


