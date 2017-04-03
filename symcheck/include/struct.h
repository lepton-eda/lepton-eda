
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
