# Need to fetch models rather than including them here because of copyright.
# Dysfunctional, but that's how it is.

ONSEMI=http://www.onsemi.com/pub_link/Collateral/

ONMODELS=MMBT2222ALT1.SP3 MMBT2907ALT1.SP3 NTB25P06.REV0.SP3 NTD5865NL.SP3

models : $(ONMODELS)

$(ONMODELS):
	wget $(ONSEMI)$@
