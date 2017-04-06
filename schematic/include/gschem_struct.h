/*! \brief different kind of snapping mechanisms used in TOPLEVEL */
typedef enum {SNAP_OFF, SNAP_GRID, SNAP_RESNAP, SNAP_STATE_COUNT} SNAP_STATE;

typedef struct st_stretch STRETCH;

struct st_stretch
{
  OBJECT *object;
  int whichone;
};
