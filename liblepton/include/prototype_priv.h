/* m_hatch.c */
void m_hatch_polygon(GArray *points, gint angle, gint pitch, GArray *lines);

/* m_polygon.c */
gboolean m_polygon_interior_point(GArray *points, int x, int y);
double m_polygon_shortest_distance(GArray *points, int x, int y, gboolean closed);

/* o_attrib.c */
GList*
o_read_attribs (LeptonPage *page,
                LeptonObject *object_to_get_attribs,
                TextBuffer *tb,
                unsigned int release_ver,
                unsigned int fileformat_ver,
                GError **err);
LeptonObject *o_attrib_find_attrib_by_name (const GList *list, const char *name, int count);

/* o_selection.c */
void o_selection_select (LeptonObject *object);
void o_selection_unselect (LeptonObject *object);

/* s_conn.c */
LeptonConn*
s_conn_return_new (LeptonObject *other_object,
                   int type,
                   int x,
                   int y,
                   int whichone,
                   int other_whichone);
int
s_conn_uniq (GList *conn_list,
             LeptonConn *input_conn);

int s_conn_remove_other (LeptonObject *other_object, LeptonObject *to_remove);
LeptonObject *s_conn_check_midpoint(LeptonObject *o_current, int x, int y);
void s_conn_print(GList *conn_list);
void
s_conn_add_object (LeptonPage *page,
                   LeptonObject *object);

/* s_encoding.c */
gchar* s_encoding_base64_encode (gchar* src, guint srclen, guint* dstlenp, gboolean strict);
gchar* s_encoding_base64_decode (gchar* src, guint srclen, guint* dstlenp);

/* s_weakref.c */
GList *s_weakref_notify (void *dead_ptr, GList *weak_refs);
GList *s_weakref_add (GList *weak_refs, void (*notify_func)(void *, void *), void *user_data);
GList *s_weakref_remove (GList *weak_refs, void (*notify_func)(void *, void *), void *user_data);
GList *s_weakref_add_ptr (GList *weak_refs, void **weak_pointer_loc);
GList *s_weakref_remove_ptr (GList *weak_refs, void **weak_pointer_loc);
