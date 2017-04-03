
/* sym check structures (gsymcheck) */
typedef struct st_symcheck SYMCHECK;

/* gsymcheck structure */
struct st_symcheck {

  /* info / warning / error messages */
  GList* info_messages;
  GList* warning_messages;
  GList* error_messages;

  /* device= check */
  int missing_device_attrib;
  int graphical_symbol;
  char *device_attribute;
  int device_attribute_incorrect;

  /* old pin#=# and slot#=# checks */
  int found_oldpin_attrib;
  int found_oldslot_attrib;

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
};
