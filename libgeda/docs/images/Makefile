
svg_images = \
	s_conn_overview.svg \
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

