#if 0
gmanager_icon_new();

iconentry1 = gnome_icon_entry_new (NULL, NULL);
  gtk_widget_ref (iconentry1);
  gtk_object_set_data_full (GTK_OBJECT (MainWindow), "iconentry1", iconentry1,
                            (GtkDestroyNotify) gtk_widget_unref);
  gtk_widget_show (iconentry1);
  gtk_box_pack_start (GTK_BOX (hbox1), iconentry1, FALSE, FALSE, 0);

#endif
