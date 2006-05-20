# variant, EHC_VARIANT to be configured at top level
EHC_VARIANT								:= X
EHC_VARIANT_PREFIX						:= $(EHC_VARIANT)/
EHC_BLD_VARIANT_PREFIX					:= $(BLD_PREFIX)$(EHC_VARIANT_PREFIX)
EHC_BLD_LIBEHC_VARIANT_PREFIX			:= $(EHC_BLD_VARIANT_PREFIX)lib-ehc/
EHC_BLD_LIBGRINC_VARIANT_PREFIX			:= $(EHC_BLD_VARIANT_PREFIX)lib-grinc/
#EHC_BLD_BIN_VARIANT_PREFIX				:= $(EHC_BLD_VARIANT_PREFIX)bin/
EHC_BIN_PREFIX							:= $(BIN_PREFIX)
EHC_LIB_PREFIX							:= $(LIB_PREFIX)
EHC_BIN_VARIANT_PREFIX					:= $(EHC_BIN_PREFIX)$(EHC_VARIANT_PREFIX)
EHC_VARIANT_RULER_SEL					:= ().().()

# lib/cabal/module config
LIB_EHC_BASE							:= EH
LIB_EHC_QUAL							:= $(subst _,x,$(LIB_EHC_BASE)$(EHC_VARIANT))
LIB_EHC_QUAL_PREFIX						:= $(LIB_EHC_QUAL).
LIB_EHC_HS_PREFIX						:= $(subst .,$(PATH_SEP),$(LIB_EHC_QUAL_PREFIX))
LIB_EHC_PKG_NAME						:= $(subst .,-,$(LIB_EHC_QUAL))
LIB_EHC_INS_FLAG						:= $(INS_FLAG_PREFIX)$(LIB_EHC_PKG_NAME)

# installation
INS_EHC_LIB_PREFIX						:= $(INS_PREFIX)lib/$(LIB_EHC_PKG_NAME)-$(EH_VERSION)/
INS_EHC_LIB_AG_PREFIX					:= $(INS_EHC_LIB_PREFIX)ag/

# further derived info
EHC_BLD_LIB_HS_VARIANT_PREFIX			:= $(EHC_BLD_LIBEHC_VARIANT_PREFIX)$(LIB_EHC_HS_PREFIX)
SRC_EHC_LIB_PREFIX						:= $(SRC_EHC_PREFIX)$(LIB_EHC_BASE)

# tool use
LIB_EHC_SHUFFLE_DEFS					:= --def=EHC:$(LIB_EHC_QUAL_PREFIX) \
											--def=BASE:$(LIB_EHC_QUAL_PREFIX)Base. \
											--def=CORE:$(LIB_EHC_QUAL_PREFIX)Core. \
											--def=TRF:$(LIB_EHC_QUAL_PREFIX)Core.Trf. \
											--def=GAM:$(LIB_EHC_QUAL_PREFIX)Gam. \
											--def=TY:$(LIB_EHC_QUAL_PREFIX)Ty. \
											--def=HS:$(LIB_EHC_QUAL_PREFIX)HS. \
											--def=EH:$(LIB_EHC_QUAL_PREFIX)EH. \
											--def=ERR:$(LIB_EHC_QUAL_PREFIX)Error. \
											--def=GRIN:$(LIB_EHC_QUAL_PREFIX)GrinCode. \
											--def=AST:$(LIB_EHC_QUAL_PREFIX)
