
/* sym check structures (gsymcheck) */
typedef struct st_symcheck SYMCHECK;

/* gsymcheck structure */
struct st_symcheck {

  /* unused for now */
  int unattached_attribs;

  /* info / warning / error messages */
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

  /* old pin#=# and slot#=# checks */
  int found_oldpin_attrib;
  int found_oldslot_attrib;

  /* net, bus, connection checks */
  int found_net;
  int found_bus;
  int found_connection;

  /* obsolete attribute checks */
  /* int found_label; */
  /* int found_uref; */

  /* forbidden attributes */
  /* int found_name; */
  /* int found_type; */

  /* misc attributes */
  int found_footprint;
  int found_refdes;
  
  /* number of pins */
  int numpins;
  /* number of net pins */
  int numnetpins;
  /* number of slots */
  int numslots;  
  /* number of distinct slot pins */
  int numslotpins;
  
  /* total error counter */
  int error_count;

  /* total warning counter */
  int warning_count;

  /* pintype= check */
  int missing_pintype_attrib;
  int multiple_pintype_attrib;
  int duplicate_pintype_attrib;

  
};


