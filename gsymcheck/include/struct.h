
/* sym check structures (gsymcheck) */
typedef struct st_symcheck SYMCHECK;

/* gsymcheck structure */
struct st_symcheck {

  int found_oldpin_attrib;
  int found_oldslot_attrib;
  int unattached_attribs;
  int found_net;
  int found_bus;
  int found_connection;
  int numpins;

  GList* info_messages;
  GList* warning_messages;
  GList* error_messages;

  /* device= check */
  int missing_device_attrib;
  int graphical_symbol;
  char *device_attribute;
  int device_attribute_incorrect;

  /* pinseq= check */
  int missing_pinseq_attrib;
  int multiple_pinseq_attrib;
  int duplicate_pinseq_attrib;

  /* multiple pinnumber= check */
  int missing_pinnumber_attrib;
  int multiple_pinnumber_attrib;
  int duplicate_pinnumber_attrib;

  /* slotting checks */ 
  int missing_numslots_attrib;
  int slotting_errors;

  /* total error counter */
  int error_count;
};


