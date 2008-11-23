
svg_images = \
	o_attrib_overview.svg \
	o_object_relations.svg \
	o_text_font_overview.svg \
	o_text_text_overview.svg \
	s_conn_overview.svg \
	s_page_overview.svg \
	s_tile_overview.svg


png_images = $(patsubst %.svg, %.png, $(svg_images))
pdf_images = $(patsubst %.svg, %.pdf, $(svg_images))


all: $(png_images) $(pdf_images)

clean:
	rm -rf $(png_images) $(pdf_images)

%.png: %.svg
	inkscape -e $@ $<  ||  convert $< $@

%.pdf: %.svg
	inkscape -A $@ $<  ||  convert $< $@

