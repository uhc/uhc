###########################################################################################
# cfg in files1*.mk
# rules in files2*.mk (top level only currently)
###########################################################################################

###########################################################################################
# end products, binary, executable, etc
###########################################################################################

TEXT_BLD_PDF				:= $(DOC_PREFIX)$(TEXT_VARIANT).pdf
TEXT_BLD_TWIKI				:= $(DOC_PREFIX)$(TEXT_VARIANT).twiki
TEXT_ALL_PUB_PDFS			:= $(patsubst %,$(DOC_PREFIX)%.pdf,$(TEXT_PUB_VARIANTS))
TEXT_ALL_PDFONLY_PDFS		:= $(patsubst %,$(DOC_PREFIX)%.pdf,$(TEXT_PDFONLY_VARIANTS))
TEXT_ALL_DOCLTX_PDFS		:= $(patsubst %,$(DOC_PREFIX)%.pdf,$(TEXT_DOCLTX_VARIANTS))
TEXT_ALL_DOCLTX_TWIKIS		:= $(patsubst %,$(DOC_PREFIX)%.twiki,$(TEXT_DOCLTX_VARIANTS))
TEXT_ALL_DOCLTX_GIFS		:= $(patsubst $(FIGS_SRC_PREFIX)%.pdf,$(DOC_PREFIX)%.gif,$(FIGS_ASIS_SRC_PDF))

###########################################################################################
# files, source + derived
###########################################################################################

# for main, pdf generation only
TEXT_MAIN_SRC_CLTEX			:= $(TEXT_SRC_PREFIX)$(TEXT_MAIN).cltex
TEXT_MAIN_DRV_LTEX			:= $(TEXT_TMP_VARIANT_PREFIX)$(TEXT_MAIN).ltex
TEXT_MAIN_DRV_TEX			:= $(TEXT_MAIN_DRV_LTEX:.ltex=.tex)
TEXT_MAIN_SRC_CLSTY			:= $(TEXT_SRC_PREFIX)$(TEXT_MAIN)sty.clsty
TEXT_MAIN_DRV_LSTY			:= $(TEXT_TMP_VARIANT_PREFIX)$(TEXT_MAIN)sty.lsty
TEXT_MAIN_DRV_STY			:= $(TEXT_MAIN_DRV_LSTY:.lsty=.sty)

# for doc main, doc LateX, TWiki
TEXT_DOCMAIN_DRV_TTEX		:= $(TEXT_TMP_VARIANT_PREFIX)$(TEXT_DOCMAIN).ttex
TEXT_DOCMAIN_DRV_TEX		:= $(TEXT_DOCMAIN_DRV_TTEX:.ttex=.tex)
TEXT_DOCMAIN_DRV_TWIKI		:= $(TEXT_DOCMAIN_DRV_TTEX:.ttex=.twiki)
TEXT_DOCMAIN_DRV_STY		:= $(TEXT_TMP_VARIANT_PREFIX)$(TEXT_DOCMAIN)sty.sty

# 
TEXT_SUBS_SRC_CLTEX			:= $(patsubst %,$(TEXT_SRC_PREFIX)%.cltex,$(TEXT_SUBS))
TEXT_SUBS_ASIS_SRC			:= $(patsubst %,$(TEXT_SRC_PREFIX)%.tex,$(TEXT_SUBS_ASIS))

EHC_CAG_DRV_LTEX			:= $(patsubst $(SRC_EHC_PREFIX)%.cag,$(TEXT_TMP_VARIANT_PREFIX)%.ltex,$(EHC_AG_ALL_MAIN_SRC_CAG) $(EHC_AG_ALL_DPDS_SRC_CAG))
EHC_CAG_DRV_TEX				:= $(EHC_CAG_DRV_LTEX:.ltex=.tex)

EHC_CHS_DRV_LTEX			:= $(patsubst $(SRC_EHC_PREFIX)%.chs,$(TEXT_TMP_VARIANT_PREFIX)%.ltex,$(EHC_HS_ALL_SRC_CHS))
EHC_CHS_DRV_TEX				:= $(EHC_CHS_DRV_LTEX:.ltex=.tex)

AGPRIMER_CAG_DRV_LTEX		:= $(patsubst $(SRC_AGPRIMER_PREFIX)%.cag,$(TEXT_TMP_VARIANT_PREFIX)%.ltex,$(AGPRIMER_CAG_SRC_CAG))
AGPRIMER_CAG_DRV_TEX		:= $(AGPRIMER_CAG_DRV_LTEX:.ltex=.tex)

AGPRIMER_CHS_DRV_LTEX		:= $(patsubst $(SRC_AGPRIMER_PREFIX)%.chs,$(TEXT_TMP_VARIANT_PREFIX)%.ltex,$(AGPRIMER_CHS_SRC_CHS))
AGPRIMER_CHS_DRV_TEX		:= $(AGPRIMER_CHS_DRV_LTEX:.ltex=.tex)

RULER_12_DRV_LTEX			:= $(patsubst $(SRC_EHC_PREFIX)%.rul,$(TEXT_TMP_VARIANT_PREFIX)%.ltex,$(EHC_RULES_1_SRC_RUL) $(EHC_RULES_2_SRC_RUL))
RULER_12_DRV_TEX			:= $(RULER_12_DRV_LTEX:.ltex=.tex)

RULER_3_DRV_LTEX			:= $(patsubst $(SRC_EHC_PREFIX)%.rul,$(TEXT_TMP_VARIANT_PREFIX)%.ltex,$(EHC_RULES_3_SRC_RL2))
RULER_3_DRV_TEX				:= $(RULER_3_DRV_LTEX:.ltex=.tex)
RULER_3_BASE				:= rules3

RULER_4_DRV_LTEX			:= $(patsubst $(SRC_EHC_RULES_PREFIX)%.rul,$(TEXT_TMP_VARIANT_PREFIX)%.ltex,$(EHC_RULES_4_MAIN_SRC_RUL))
RULER_4_DRV_TEX				:= $(RULER_4_DRV_LTEX:.ltex=.tex)
RULER_4_BASE				:= rules4

RULER2_RULES_DRV_LTEX		:= $(patsubst $(SRC_RULER2_PREFIX)%.rul,$(TEXT_TMP_VARIANT_PREFIX)%.ltex,$(RULER2_RULES_SRC_RL2))
RULER2_RULES_DRV_TEX		:= $(RULER2_RULES_DRV_LTEX:.ltex=.tex)

TEXT_RULES_3_DRV_CAG		:= $(TEXT_TMP_VARIANT_PREFIX)$(EHC_RULER_RULES).cag
TEXT_RULES_3_DRV_LTEX		:= $(TEXT_RULES_3_DRV_CAG:.cag=.ltex)
TEXT_RULES_3_DRV_TEX		:= $(TEXT_RULES_3_DRV_LTEX:.ltex=.tex)

TEXT_RULES_EXPLAIN_3_DRV_CAG:= $(TEXT_TMP_VARIANT_PREFIX)rules3Explain.cag

TEXT_RULES_MISC_SRC_RUL		:= $(addprefix $(TEXT_SRC_PREFIX),MiscRules.rul)
TEXT_RULES_MISC_DRV_LTEX	:= $(patsubst $(TEXT_SRC_PREFIX)%.rul,$(TEXT_TMP_VARIANT_PREFIX)%.ltex,$(TEXT_RULES_MISC_SRC_RUL))
TEXT_RULES_MISC_DRV_TEX		:= $(TEXT_RULES_MISC_DRV_LTEX:.ltex=.tex)

TEXT_HIDE_DRV_TXT			:= $(TEXT_TMP_VARIANT_PREFIX)$(TEXT_MAIN)-hide.hltex
TEXT_HIDE_DRV_LTEX			:= $(TEXT_HIDE_DRV_TXT:.hltex=.ltex)
TEXT_HIDE_DRV_TEX			:= $(TEXT_HIDE_DRV_TXT:.hltex=.tex)

#TEXT_SUBS_DRV_TEX			:= $(EHC_CAG_DRV_TEX) $(EHC_CHS_DRV_TEX) $(AGPRIMER_CAG_DRV_TEX) $(AGPRIMER_CHS_DRV_TEX) $(RULER_12_DRV_TEX) \
#								$(RULER_3_DRV_TEX) $(RULER2_RULES_DRV_TEX) $(TEXT_RULES_3_DRV_TEX) $(TEXT_RULES_MISC_DRV_TEX)
TEXT_SUBS_DRV_TEX			:= $(if $(TEXT_CFG_TEXT_INCLUDES_PREV_RULER_TEX),$(RULER_12_DRV_TEX)) \
								$(RULER2_RULES_DRV_TEX) $(RULER_3_DRV_TEX) $(RULER_4_DRV_TEX) \
								$(TEXT_RULES_3_DRV_TEX) $(TEXT_RULES_MISC_DRV_TEX)
TEXT_SUBS_ASIS_DRV			:= $(patsubst $(TEXT_SRC_PREFIX)%.tex,$(TEXT_TMP_VARIANT_PREFIX)%.tex,$(TEXT_SUBS_ASIS_SRC))

TEXT_INCL_LIST_TEX			:= $(TEXT_TMP_VARIANT_PREFIX)InclList.tex
TEXT_GEN_BY_RULER_TABLE_TEX	:= $(TEXT_TMP_VARIANT_PREFIX)GenByRuler.tex

TEXT_BIB1_SRC				:= $(TEXT_SRC_PREFIX)LitAdm.bib
TEXT_BIB2_SRC				:= $(TEXT_SRC_PREFIX)ArieMiddelkoop.bib
TEXT_BIB3_SRC				:= $(TEXT_SRC_PREFIX)JeroenFokker.bib
TEXT_BIB4_SRC				:= $(TEXT_SRC_PREFIX)JohnVanSchie.bib
TEXT_BIB_DRV				:= $(TEXT_TMP_VARIANT_PREFIX)$(TEXT_MAIN).bib

FIGS_XFIG_DRV_TEX			:= $(patsubst $(FIGS_SRC_PREFIX)%.fig,$(TEXT_TMP_VARIANT_PREFIX)%.tex,$(FIGS_XFIG_SRC_FIG))
FIGS_XFIG_DRV_PDF			:= $(patsubst $(FIGS_SRC_PREFIX)%.fig,$(TEXT_TMP_VARIANT_PREFIX)%.pdf,$(FIGS_XFIG_SRC_FIG_NOPDF))
FIGS_ASIS_DRV				:= $(patsubst $(FIGS_SRC_PREFIX)%,$(TEXT_TMP_VARIANT_PREFIX)%,$(FIGS_ASIS_SRC))
FIGS_EPS_DRV_PDF			:= $(patsubst $(FIGS_SRC_PREFIX)%.eps,$(TEXT_TMP_VARIANT_PREFIX)%.pdf,$(FIGS_EPS_SRC_EPS))
FIGS_DOT_DRV_PDF			:= $(patsubst $(FIGS_SRC_PREFIX)%.dot,$(TEXT_TMP_VARIANT_PREFIX)%.pdf,$(FIGS_DOT_SRC_DOT))
FIGS_PDF_DRV_GIF			:= $(patsubst $(FIGS_SRC_PREFIX)%.pdf,$(TEXT_TMP_VARIANT_PREFIX)%.gif,$(FIGS_ASIS_SRC_PDF))
FIGS_ALL_DRV_GIF			:= $(FIGS_PDF_DRV_GIF)

TEXT_ALL_MK_FILES			:= $(AGPRIMER_MKF) $(EHC_MKF) $(RULER2_MKF) $(TEXT_MKF)

# Ruler examples
TEXT_RULEX					:= RulerExamples
TEXT_RULEX_SRC_CRUL			:= $(TEXT_SRC_PREFIX)$(TEXT_RULEX).crul
TEXT_RULEX_DRV_RUL			:= $(patsubst $(TEXT_SRC_PREFIX)%.crul,$(TEXT_TMP_VARIANT_PREFIX)%.rul,$(TEXT_RULEX_SRC_CRUL))
TEXT_RULEX_DRV_LTEX			:= $(TEXT_RULEX_DRV_RUL:.rul=.ltex)
TEXT_RULEX_DRV_TEX			:= $(TEXT_RULEX_DRV_RUL:.rul=.tex)

TEXT_RULEX_ALL_DRV_TEX		:= $(TEXT_RULEX_DRV_TEX)

# infer2pass paper stuff
TEXT_INF2PS_MARK_CHANGES_CFG	:= --markchanges="HM - *"

TEXT_INF2PS_SRC_CRUL		:= $(INF2PS_RL_RULES_SRC_CRUL)
TEXT_INF2PS_DRV_RUL			:= $(patsubst $(SRC_INF2PS_PREFIX)%.crul,$(TEXT_TMP_VARIANT_PREFIX)%.rul,$(TEXT_INF2PS_SRC_CRUL))
TEXT_INF2PS_DRV_LTEX		:= $(TEXT_INF2PS_DRV_RUL:.rul=.ltex)
TEXT_INF2PS_DRV_TEX			:= $(TEXT_INF2PS_DRV_RUL:.rul=.tex)

TEXT_INF2PS_ALL_DRV_TEX		:= $(TEXT_INF2PS_DRV_TEX)

# subst experiment paper stuff
TEXT_EXPERIMENTS_SUBST_SRC_RUL			:= $(EXPERIMENTS_SUBST_SRC_RUL)
TEXT_EXPERIMENTS_SUBST_DRV_LTEX			:= $(patsubst $(SRC_EXPERIMENTS_SUBST_PREFIX)%.rul,$(TEXT_TMP_VARIANT_PREFIX)%.ltex,$(TEXT_EXPERIMENTS_SUBST_SRC_RUL))
TEXT_EXPERIMENTS_SUBST_DRV_TEX			:= $(TEXT_EXPERIMENTS_SUBST_DRV_LTEX:.ltex=.tex)
TEXT_EXPERIMENTS_SUBST_SRC_RUN_TEX		:= $(RUN_EXPERIMENTS_SUBST_TEX)
TEXT_EXPERIMENTS_SUBST_DRV_RUN_TEX		:= $(patsubst $(RUN_EXPERIMENTS_SUBST_PREFIX)%,$(TEXT_TMP_VARIANT_PREFIX)%,$(TEXT_EXPERIMENTS_SUBST_SRC_RUN_TEX))

TEXT_EXPERIMENTS_SUBST_ALL_DRV_TEX		:= $(TEXT_EXPERIMENTS_SUBST_DRV_TEX) \
											$(if $(TEXT_CFG_TEXT_INCLUDES_EXPERIMENTS_SUBST_TEX),$(TEXT_EXPERIMENTS_SUBST_DRV_RUN_TEX))

# ruler demo
TEXT_RULER2_DEMO_DRV_CAG		:= $(TEXT_TMP_VARIANT_PREFIX)$(RULER2_DEMO_AG_BASE).cag
TEXT_RULER2_DEMO_DRV_LCTEX		:= $(TEXT_TMP_VARIANT_PREFIX)demo.lctex
TEXT_RULER2_DEMO_DRV_CTEX		:= $(TEXT_RULER2_DEMO_DRV_LCTEX:.lctex=.ctex)
TEXT_RULER2_DEMO_DRV_RL2		:= $(TEXT_RULER2_DEMO_DRV_LCTEX:.lctex=.rl2)
TEXT_RULER2_DEMO_DRV_LRTEX		:= $(TEXT_RULER2_DEMO_DRV_LCTEX:.lctex=.lrtex)
TEXT_RULER2_DEMO_DRV_RTEX		:= $(TEXT_RULER2_DEMO_DRV_LCTEX:.lctex=.rtex)
TEXT_RULER2_DEMO_DRV_LATEX		:= $(TEXT_RULER2_DEMO_DRV_LCTEX:.lctex=.latex)
TEXT_RULER2_DEMO_DRV_ATEX		:= $(TEXT_RULER2_DEMO_DRV_LCTEX:.lctex=.atex)

TEXT_RULER2_DEMO_ALL_DRV_TEX	:= $(TEXT_RULER2_DEMO_DRV_CTEX) $(TEXT_RULER2_DEMO_DRV_RTEX) $(TEXT_RULER2_DEMO_DRV_ATEX)

TEXT_RULER2_DEMO_TEX		:= $(patsubst $(RULER2_DEMO_PREFIX)%,$(TEXT_TMP_VARIANT_PREFIX)%,$(RULER2_DEMO_ALL_DRV_TEX))
TEXT_RULER2_DEMO_STUFF		:= $(patsubst $(RULER2_DEMO_PREFIX)%,$(TEXT_TMP_VARIANT_PREFIX)%,$(RULER2_DEMO_DRV_AG) $(RULER2_DEMO_DRV_AG_MAIN) $(RULER2_DEMO_DRV_HS_UTILS))

# Uniqueness examples in ruler
TEXT_RULEUX := RulerUniquenessExamples
TEXT_RULEUX_SRC_CRUL := $(TEXT_SRC_PREFIX)$(TEXT_RULEUX).crul
TEXT_RULEUX_DRV_RUL  := $(patsubst $(TEXT_SRC_PREFIX)%.crul,$(TEXT_TMP_VARIANT_PREFIX)%.rul,$(TEXT_RULEUX_SRC_CRUL))
TEXT_RULEUX_DRV_LTEX := $(TEXT_RULEUX_DRV_RUL:.rul=.ltex)
TEXT_RULEUX_DRV_TEX			:= $(TEXT_RULEUX_DRV_RUL:.rul=.tex)
TEXT_RULEUX_ALL_DRV_TEX		:= $(TEXT_RULEUX_DRV_TEX)

# all src
TEXT_EDIT_SRC				:= $(TEXT_MAIN_SRC_CLTEX) $(TEXT_SUBS_SRC_CLTEX) $(TEXT_MAIN_SRC_CLSTY) $(TEXT_RULES_MISC_SRC_RUL) $(TEXT_INF2PS_SRC_CRUL) $(TEXT_RULEX_SRC_CRUL) $(TEXT_EXPERIMENTS_SUBST_SRC_RUL)
TEXT_ALL_SRC				:= $(TEXT_EDIT_SRC) $(TEXT_SUBS_ASIS_SRC) $(TEXT_BIB1_SRC) $(TEXT_BIB2_SRC) $(TEXT_BIB3_SRC) $(TEXT_BIB4_SRC) $(TEXT_MKF)

# all deriveds (as counting for make dependencies)
TEXT_ALL_PDFONLY_DPD		:= $(TEXT_MAIN_DRV_TEX) $(TEXT_SUBS_DRV_TEX) $(TEXT_MAIN_DRV_STY) $(TEXT_RULER2_DEMO_TEX) $(TEXT_INF2PS_ALL_DRV_TEX) $(TEXT_RULEX_ALL_DRV_TEX) $(TEXT_RULER2_DEMO_ALL_DRV_TEX) \
								$(TEXT_SUBS_ASIS_DRV) $(FIGS_XFIG_DRV_TEX) $(FIGS_XFIG_DRV_PDF) $(FIGS_EPS_DRV_PDF) $(FIGS_DOT_DRV_PDF) $(TEXT_RULER2_DEMO_STUFF) $(FIGS_ASIS_DRV) $(TEXT_HIDE_DRV_TEX)  \
								$(TEXT_GEN_BY_RULER_TABLE_TEX) $(TEXT_INCL_LIST_TEX) $(TEXT_RULEUX_ALL_DRV_TEX) $(TEXT_RULEUX_ALL_DRV_TEX) $(TEXT_EXPERIMENTS_SUBST_ALL_DRV_TEX)

TEXT_ALL_DOCLTX_DPD			:= $(TEXT_DOCMAIN_DRV_TEX) $(TEXT_DOCMAIN_DRV_TWIKI) $(TEXT_DOCMAIN_DRV_STY) $(FIGS_ASIS_DRV)
TEXT_ALL_TWIKI_DPD			:= $(TEXT_DOCMAIN_DRV_TWIKI) $(FIGS_ALL_DRV_GIF)

# all shuffle included material
TEXT_SUBS_SHUFFLE1			:= $(TEXT_SUBS_SRC_CLTEX) $(TEXT_RULES_3_DRV_CAG) $(RULER2_ALL_CHUNK_SRC) $(AGPRIMER_ALL_CHUNK_SRC) $(TEXT_RULES_EXPLAIN_3_DRV_CAG) \
								$(TEXT_INF2PS_SRC_CRUL) $(TEXT_RULEX_SRC_CRUL) $(TEXT_MAIN_SRC_CLSTY)
TEXT_SUBS_SHUFFLE_SRCS		:= $(if $(TEXT_CFG_SHUFFLE_INCLUDES_CHUNK_SRC),$(EHC_ALL_CHUNK_SRC)) \
								$(if $(TEXT_CFG_SHUFFLE_INCLUDES_CHUNK_SRC_EXPERIMENTS_SUBST),$(EXPERIMENTS_SUBST_ALL_CHUNK_SRC))
TEXT_SUBS_SHUFFLE			:= $(TEXT_SUBS_SHUFFLE1) $(TEXT_SUBS_SHUFFLE_SRCS)
TEXT_SUBS_SHUFFLE_ALIAS		:= $(TEXT_SUBS_SHUFFLE1) \
								$(patsubst EHEH%,EH%,\
									$(join $(patsubst %,EH%=,$(basename $(subst /,$(empty),$(patsubst $(SRC_EHC_PREFIX)%,%,$(TEXT_SUBS_SHUFFLE_SRCS))))), \
									       $(TEXT_SUBS_SHUFFLE_SRCS) \
									))

# distribution
TEXT_DIST_DOC_FILES			:= $(TEXT_ALL_PUB_PDFS)
TEXT_DIST_FILES				:= $(TEXT_ALL_SRC)

# www
TEXT_WWW_DOC_PDFS			:= $(TEXT_ALL_DOCLTX_PDFS)
TEXT_WWW_DOC_GIFS			:= $(TEXT_ALL_DOCLTX_GIFS)
TEXT_WWW_DOC_TWIKIS			:= $(TEXT_ALL_DOCLTX_TWIKIS)

###########################################################################################
# variant dispatch rules for targets
###########################################################################################

$(TEXT_ALL_PDFONLY_PDFS) $(TEXT_ALL_DOCLTX_PDFS): $(DOC_PREFIX)%.pdf: $(TEXT_ALL_SRC) $(RULER2_DEMO_ALL_SRC) $(EHC_ALL_SRC) $(RULER2_DEMO_ALL_DRV_TEX) \
										$(RULER2_RULES_SRC_RL2) $(TEXT_ALL_MK_FILES) $(FIGS_ALL_SRC) $(RULER2) $(EXPERIMENTS_SUBST_ALL_SRC)
	$(MAKE) TEXT_VARIANT=$(*F) text-variant-$(*F)

$(TEXT_PDFONLY_VARIANTS) $(TEXT_DOCLTX_VARIANTS) : % : $(DOC_PREFIX)%.pdf
	open $<

text-variant-dflt-once: $(TEXT_ALL_PDFONLY_DPD)
	mkdir -p $(dir $(TEXT_BLD_PDF))
	cd $(TEXT_TMP_VARIANT_PREFIX) ; $(PDFLATEX) $(TEXT_MAIN)
	cp $(TEXT_TMP_VARIANT_PREFIX)$(TEXT_MAIN).pdf $(TEXT_BLD_PDF)

text-variant-dflt-doc: $(TEXT_ALL_DOCLTX_DPD) # $(TEXT_ALL_TWIKI_DPD)
	mkdir -p $(dir $(TEXT_BLD_PDF)) $(dir $(TEXT_BLD_TWIKI))
	cd $(TEXT_TMP_VARIANT_PREFIX) ; $(PDFLATEX) $(TEXT_DOCMAIN) ; $(PDFLATEX) $(TEXT_DOCMAIN)
	cp $(TEXT_TMP_VARIANT_PREFIX)$(TEXT_DOCMAIN).pdf $(TEXT_BLD_PDF)
	cp $(TEXT_TMP_VARIANT_PREFIX)$(TEXT_DOCMAIN).twiki $(TEXT_BLD_TWIKI)

text-variant-dflt-bib: $(TEXT_ALL_PDFONLY_DPD) $(TEXT_BIB_DRV)
	mkdir -p $(dir $(TEXT_BLD_PDF))
	cd $(TEXT_TMP_VARIANT_PREFIX) ; \
	$(PDFLATEX) $(TEXT_MAIN)
	cd $(TEXT_TMP_VARIANT_PREFIX) ; \
	$(BIBTEX) $(TEXT_MAIN) ; \
	$(PDFLATEX) $(TEXT_MAIN) ; \
	$(PDFLATEX) $(TEXT_MAIN)
	cp $(TEXT_TMP_VARIANT_PREFIX)$(TEXT_MAIN).pdf $(TEXT_BLD_PDF)

# variation on -bib where .dot file is generated, to be included later as drawn pdf
text-variant-dflt-bib-dotdpd: $(TEXT_ALL_PDFONLY_DPD) $(TEXT_BIB_DRV)
	mkdir -p $(dir $(TEXT_BLD_PDF))
	cd $(TEXT_TMP_VARIANT_PREFIX) ; \
	echo "graph {}" | dot -Tpdf > $(TEXT_MAIN)-dpd.pdf ; \
	$(PDFLATEX) $(TEXT_MAIN) ; \
	$(BIBTEX) $(TEXT_MAIN) ; \
	$(PDFLATEX) $(TEXT_MAIN) ; \
	dot -Tpdf $(TEXT_MAIN).dot > $(TEXT_MAIN)-dpd.pdf ; \
	$(PDFLATEX) $(TEXT_MAIN)
	cp $(TEXT_TMP_VARIANT_PREFIX)$(TEXT_MAIN).pdf $(TEXT_BLD_PDF)

text-variant-dflt-bib-inx: $(TEXT_ALL_PDFONLY_DPD) $(TEXT_BIB_DRV)
	mkdir -p $(dir $(TEXT_BLD_PDF))
	cd $(TEXT_TMP_VARIANT_PREFIX) ; \
	$(PDFLATEX) $(TEXT_MAIN)
	cd $(TEXT_TMP_VARIANT_PREFIX) ; \
	$(BIBTEX) $(TEXT_MAIN) ; \
	$(PDFLATEX) $(TEXT_MAIN) ; \
	rm -f $(TEXT_MAIN).ind ; \
	$(MAKEINDEX) $(TEXT_MAIN) ; \
	$(PDFLATEX) $(TEXT_MAIN)
	cp $(TEXT_TMP_VARIANT_PREFIX)$(TEXT_MAIN).pdf $(TEXT_BLD_PDF)

###########################################################################################
# rules for individual files
###########################################################################################

$(TEXT_DOCMAIN_DRV_TTEX) : $(TEXT_MAIN_SRC_CLTEX) $(TEXT_SUBS_SHUFFLE) $(SHUFFLE) $(TEXT2TEXT) $(TEXT_MKF)
	mkdir -p $(@D)
	$(SHUFFLE) --gen=$(TEXT_SHUFFLE_VARIANT) --plain --text2text --lhs2tex=no --order="$(TEXT_SHUFFLE_ORDER)" $< $(TEXT_SUBS_SHUFFLE_ALIAS) > $@

$(TEXT_DOCMAIN_DRV_TEX) : %.tex : %.ttex
	$(TEXT2TEXT) --doclatex $< \
	  > $@

$(TEXT_DOCMAIN_DRV_TWIKI) : %.twiki : %.ttex
	$(TEXT2TEXT) --twiki --gen-header-numbering=yes $< \
	  > $@

$(TEXT_ALL_DOCLTX_GIFS): $(DOC_PREFIX)%.gif : $(FIGS_SRC_PREFIX)%.pdf $(TEXT_MKF)
	convert -trim $< $@

$(TEXT_HIDE_DRV_TXT): $(TEXT_MAIN_DRV_LTEX)
	touch $@

$(TEXT_MAIN_DRV_LTEX) : $(TEXT_MAIN_SRC_CLTEX) $(TEXT_SUBS_SHUFFLE) $(SHUFFLE) $(TEXT_MKF)
	mkdir -p $(@D)
	$(SHUFFLE) --gen=$(TEXT_SHUFFLE_VARIANT) --plain --lhs2tex=no --hidedest=appx=$(TEXT_HIDE_DRV_TXT) --order="$(TEXT_SHUFFLE_ORDER)" $< $(TEXT_SUBS_SHUFFLE_ALIAS) > $@

$(TEXT_MAIN_DRV_TEX) : %.tex : %.ltex
	$(SUBST_EHC) $< \
	  | $(SUBST_BAR_IN_TT) \
	  | $(LHS2TEX_CMD) $(LHS2TEX_OPTS_TEXT_CONFIG) $(LHS2TEX_OPTS_VARIANT_CONFIG) $(LHS2TEX_OPTS_POLY) \
	  > $@

$(TEXT_SUBS_DRV_TEX) $(TEXT_HIDE_DRV_TEX) $(TEXT_INF2PS_ALL_DRV_TEX) $(TEXT_RULEX_ALL_DRV_TEX) $(TEXT_RULEUX_ALL_DRV_TEX) $(TEXT_EXPERIMENTS_SUBST_DRV_TEX) : %.tex : %.ltex
	$(LHS2TEX_CMD) $(LHS2TEX_OPTS_TEXT_CONFIG) $(LHS2TEX_OPTS_VARIANT_CONFIG) $(LHS2TEX_OPTS_POLY) $< > $@

$(TEXT_MAIN_DRV_LSTY) : $(TEXT_MAIN_SRC_CLSTY) $(SHUFFLE)
	mkdir -p $(@D)
	$(SHUFFLE) --gen=$(TEXT_SHUFFLE_VARIANT) --plain --lhs2tex=no --order="$(TEXT_SHUFFLE_ORDER)" $< > $@

$(TEXT_DOCMAIN_DRV_STY) : $(TEXT_MAIN_SRC_CLSTY) $(SHUFFLE)
	mkdir -p $(@D)
	$(SHUFFLE) --gen=$(TEXT_SHUFFLE_VARIANT) --plain --lhs2tex=no --order="$(TEXT_SHUFFLE_ORDER)" $< > $@

$(TEXT_MAIN_DRV_STY) : $(TEXT_MAIN_DRV_LSTY) $(TEXT_MKF)
	$(LHS2TEX_CMD) $(LHS2TEX_OPTS_TEXT_CONFIG) $(LHS2TEX_OPTS_VARIANT_CONFIG) $(LHS2TEX_OPTS_POLY) $< > $@

$(EHC_CHS_DRV_LTEX) : $(TEXT_TMP_VARIANT_PREFIX)%.ltex : $(SRC_EHC_PREFIX)%.chs $(SHUFFLE)
	mkdir -p $(@D)
	$(SHUFFLE) --gen=all --latex --lhs2tex=yes  --base=$(*F) --order="$(EHC_SHUFFLE_ORDER)" $< > $@

$(EHC_CAG_DRV_LTEX) : $(TEXT_TMP_VARIANT_PREFIX)%.ltex : $(SRC_EHC_PREFIX)%.cag $(SHUFFLE)
	mkdir -p $(@D)
	$(SHUFFLE) --gen=all --latex --lhs2tex=yes  --base=$(*F) --order="$(EHC_SHUFFLE_ORDER)" $< > $@

$(AGPRIMER_CHS_DRV_LTEX) : $(TEXT_TMP_VARIANT_PREFIX)%.ltex : $(SRC_AGPRIMER_PREFIX)%.chs $(SHUFFLE)
	mkdir -p $(@D)
	$(SHUFFLE) --gen=all --latex --lhs2tex=yes  --base=$(*F) --order="$(AGPRIMER_SHUFFLE_ORDER)" $< > $@

$(AGPRIMER_CAG_DRV_LTEX) : $(TEXT_TMP_VARIANT_PREFIX)%.ltex : $(SRC_AGPRIMER_PREFIX)%.cag $(SHUFFLE)
	mkdir -p $(@D)
	$(SHUFFLE) --gen=all --latex --lhs2tex=yes  --base=$(*F) --order="$(AGPRIMER_SHUFFLE_ORDER)" $< > $@

$(TEXT_RULES_3_DRV_LTEX) : $(TEXT_RULES_3_DRV_CAG) $(SHUFFLE)
	$(SHUFFLE) --gen=all --latex --lhs2tex=yes  --base=$(basename $(@F)) $< > $@

$(RULER_12_DRV_LTEX) : $(TEXT_TMP_VARIANT_PREFIX)%.ltex : $(SRC_EHC_PREFIX)%.rul $(RULER1)
	mkdir -p $(@D)
	$(RULER1) --latex --base=$(*F) $< > $@

$(RULER_3_DRV_LTEX) : $(TEXT_TMP_VARIANT_PREFIX)%.ltex : $(SRC_EHC_PREFIX)%.rul $(RULER2)
	mkdir -p $(@D)
	$(RULER2) $(RULER2_OPTS) $(TEXT_RULER_DEFS_TEX) --lhs2tex $(TEXT_RULER_MARK_CHANGES_CFG) --base=$(RULER_3_BASE) $< > $@

$(RULER_4_DRV_LTEX) : $(TEXT_TMP_VARIANT_PREFIX)%.ltex : $(SRC_EHC_RULES_PREFIX)%.rul $(EHC_RULES_4_DPDS_SRC_RUL) $(RULER2)
	mkdir -p $(@D)
#	$(RULER2) $(RULER2_OPTS) --lhs2tex --base=$(RULER_4_BASE) $< > $@
	$(RULER2) $(RULER2_OPTS) $(TEXT_RULER_DEFS_TEX) --lhs2tex $(TEXT_RULER_MARK_CHANGES_CFG) --base=$(RULER_4_BASE) $< > $@

$(RULER2_RULES_DRV_LTEX) : $(TEXT_TMP_VARIANT_PREFIX)%.ltex : $(SRC_RULER2_PREFIX)%.rul $(RULER2)
	mkdir -p $(@D)
	$(RULER2) $(RULER2_OPTS) $(TEXT_RULER_DEFS_TEX) --lhs2tex --base=$(*F) $< > $@

$(TEXT_RULES_MISC_DRV_LTEX) : $(TEXT_TMP_VARIANT_PREFIX)%.ltex : $(TEXT_SRC_PREFIX)%.rul $(RULER2)
	mkdir -p $(@D)
	$(RULER2) $(RULER2_OPTS) $(TEXT_RULER_DEFS_TEX) --lhs2tex --base=$(*F) $< > $@

$(TEXT_RULES_3_DRV_CAG): $(EHC_RULES_3_SRC_RL2) $(RULER2)
	mkdir -p $(@D)
	$(RULER2) $(RULER2_OPTS) --ag --wrapshuffle --selrule="((1=K),(2=C),(3=HM),(4=EX),(42=I2),(9=P)).(expr.base tyexpr.base patexpr.base decl.base).(*)" --base=$(*F) $< > $@

$(TEXT_RULES_EXPLAIN_3_DRV_CAG): $(EHC_RULES_3_SRC_RL2) $(RULER2)
	mkdir -p $(@D)
	$(RULER2) $(RULER2_OPTS) --explain --wrapshuffle $< > $@

$(TEXT_RULER2_DEMO_TEX) $(TEXT_RULER2_DEMO_STUFF): $(TEXT_TMP_VARIANT_PREFIX)% : $(RULER2_DEMO_PREFIX)%
	mkdir -p $(@D)
	cp $< $@

$(TEXT_BIB_DRV): $(TEXT_BIB1_SRC) $(TEXT_BIB2_SRC) $(TEXT_BIB3_SRC) $(TEXT_BIB4_SRC)
	mkdir -p $(@D)
	cat $(TEXT_BIB1_SRC) > $@
	cat $(TEXT_BIB2_SRC) >> $@
	cat $(TEXT_BIB3_SRC) >> $@
	cat $(TEXT_BIB4_SRC) >> $@

$(TEXT_SUBS_ASIS_DRV): $(TEXT_TMP_VARIANT_PREFIX)% : $(TEXT_SRC_PREFIX)%
	mkdir -p $(@D)
	cp $< $@

$(FIGS_ASIS_DRV): $(TEXT_TMP_VARIANT_PREFIX)% : $(FIGS_SRC_PREFIX)%
	mkdir -p $(@D)
	cp $< $@

$(FIGS_XFIG_DRV_TEX): $(TEXT_TMP_VARIANT_PREFIX)%.tex : $(FIGS_SRC_PREFIX)%.fig $(TEXT_MKF)
	mkdir -p $(@D)
	(echo '%include lhs2TeX.fmt' ; echo '%include afp.fmt' ; echo '%include oneletter.fmt' ; fig2dev -L epic -E 0 $< | sed -e 's/@/@@/g') > $@.ltex
	$(LHS2TEX_CMD) $(LHS2TEX_OPTS_TEXT_CONFIG) $(LHS2TEX_OPTS_VARIANT_CONFIG) $(LHS2TEX_OPTS_POLY) $@.ltex > $@

$(FIGS_XFIG_DRV_PDF): $(TEXT_TMP_VARIANT_PREFIX)%.pdf : $(FIGS_SRC_PREFIX)%.fig
	mkdir -p $(@D)
	fig2dev -L pdf -p dummy $< > $@

$(FIGS_EPS_DRV_PDF): $(TEXT_TMP_VARIANT_PREFIX)%.pdf : $(FIGS_SRC_PREFIX)%.eps
	mkdir -p $(@D)
	ps2pdf $< $@

$(FIGS_DOT_DRV_PDF): $(TEXT_TMP_VARIANT_PREFIX)%.pdf : $(FIGS_SRC_PREFIX)%.dot
	mkdir -p $(@D)
	dot -Tps $< -o $(basename $@).ps
	epstopdf $(basename $@).ps --outfile=$@

$(TEXT_INCL_LIST_TEX): $(TEXT_ALL_MK_FILES)
	@(for f in $(sort $(notdir $(TEXT_SUBS_DRV_TEX) \
	                           $(if $(TEXT_CFG_TEXT_INCLUDES_PREV_RULER_TEX),$(RULER_12_DRV_TEX)) \
	                           $(if $(TEXT_CFG_TEXT_INCLUDES_EXPERIMENTS_SUBST_TEX),$(TEXT_EXPERIMENTS_SUBST_DRV_TEX)) \
	                           $(TEXT_INF2PS_ALL_DRV_TEX) $(RULER_4_DRV_TEX) $(RULER_3_DRV_TEX) $(TEXT_RULEUX_ALL_DRV_TEX) $(TEXT_RULEX_ALL_DRV_TEX) $(TEXT_RULER2_DEMO_ALL_DRV_TEX) $(TEXT_RULER2_DEMO_TEX) \
	            )      ) ; \
	  do \
	    echo "\\input" $$f ; \
	  done \
	) > $@

$(TEXT_GEN_BY_RULER_TABLE_TEX): $(EHC_MKF) $(TEXT_MKF)
	@(echo "\begin{tabular}{llp{.6\linewidth}}" ; \
	  echo "EH version & Ruler view & rules \\\\ \\hline" ; \
	  for f in $(EHC_VARIANTS) ; \
	  do \
	    $(MAKE) echo-gen-by-ruler-$$f ; \
	  done ; \
	  echo "\end{tabular}" \
	) > $@

$(TEXT_HIDE_DRV_LTEX): $(TEXT_HIDE_DRV_TXT)
	(echo '%include lhs2TeX.fmt' ; echo '%include afp.fmt' ; cat $< ) > $@

# ruler demo make rules
$(TEXT_RULER2_DEMO_DRV_LCTEX): $(RULER2_DEMO_SRC_CRL) $(SHUFFLE) $(TEXT_MKF)
	$(SHUFFLE) --gen=all --latex --order="$(RULER2_DEMO_RULER2_ORDER)" --base=$(RULER2_DEMO_RUL_BASE) --lhs2tex=yes $< > $@

$(TEXT_RULER2_DEMO_DRV_CTEX): $(TEXT_RULER2_DEMO_DRV_LCTEX)
	$(LHS2TEX_CMD) $(LHS2TEX_OPTS_POLY) $< > $@

$(TEXT_RULER2_DEMO_DRV_RL2): $(RULER2_DEMO_SRC_CRL) $(SHUFFLE) $(TEXT_MKF)
	$(SHUFFLE) --gen=$(RULER2_DEMO_RULER2_FINAL) --plain --order="$(RULER2_DEMO_RULER2_ORDER)"  --lhs2tex=no $< > $@

$(TEXT_INF2PS_DRV_RUL): $(TEXT_INF2PS_SRC_CRUL) $(SHUFFLE) $(TEXT_MKF)
	$(SHUFFLE) --gen=1 --plain --order="1"  --lhs2tex=no $< > $@

$(TEXT_RULEX_DRV_RUL): $(TEXT_RULEX_SRC_CRUL) $(SHUFFLE) $(TEXT_MKF)
	$(SHUFFLE) --gen=1 --plain --order="1"  --lhs2tex=no $< > $@

$(TEXT_RULER2_DEMO_DRV_LRTEX): $(TEXT_RULER2_DEMO_DRV_RL2) $(RULER2)
	$(RULER2) $(RULER2_OPTS) $(TEXT_RULER_DEFS_TEX) --lhs2tex --selrule="(E - *).(*).(*)" $(RULER2_DEMO_MARK_CHANGES_CFG) --base=rulerDemo $< > $@

$(TEXT_RULER2_DEMO_DRV_RTEX): $(TEXT_RULER2_DEMO_DRV_LRTEX)
	$(LHS2TEX_CMD) $(LHS2TEX_OPTS_POLY) $< > $@

$(TEXT_RULER2_DEMO_DRV_CAG): $(TEXT_RULER2_DEMO_DRV_RL2) $(RULER2)
	$(RULER2) $(RULER2_OPTS) --ag --ATTR --DATA --selrule="(3).(*).(*)" --wrapshuffle  --base=$(RULER2_DEMO_AG_BASE) $< > $@

$(TEXT_RULER2_DEMO_DRV_AG): $(TEXT_RULER2_DEMO_DRV_CAG) $(SHUFFLE)
	$(SHUFFLE) --gen=$(RULER2_DEMO_RULER2_FINAL) --plain --order="$(RULER2_DEMO_RULER2_ORDER)"  --lhs2tex=no $< > $@

$(TEXT_RULER2_DEMO_DRV_LATEX): $(TEXT_RULER2_DEMO_DRV_CAG) $(SHUFFLE)
	$(SHUFFLE) --gen=$(RULER2_DEMO_RULER2_FINAL) --latex --order="$(RULER2_DEMO_RULER2_ORDER)" --base=$(RULER2_DEMO_AG_BASE) --lhs2tex=yes $< > $@

$(TEXT_RULER2_DEMO_DRV_ATEX): $(TEXT_RULER2_DEMO_DRV_LATEX)
	$(LHS2TEX_CMD) $(LHS2TEX_OPTS_POLY) $< > $@

$(TEXT_INF2PS_DRV_LTEX): $(TEXT_INF2PS_DRV_RUL) $(RULER2)
	$(RULER2) $(RULER2_OPTS) $(TEXT_RULER_DEFS_TEX) --lhs2tex --selrule="(HM - *).(*).(*)" $(TEXT_INF2PS_MARK_CHANGES_CFG) --base=infer2pass $< > $@

$(TEXT_EXPERIMENTS_SUBST_DRV_LTEX): $(TEXT_EXPERIMENTS_SUBST_SRC_RUL) $(RULER2)
	$(RULER2) $(RULER2_OPTS) $(TEXT_RULER_DEFS_TEX) --lhs2tex --base=subst $< > $@

$(TEXT_RULEX_DRV_LTEX): $(TEXT_RULEX_DRV_RUL) $(RULER2)
	$(RULER2) $(RULER2_OPTS) $(TEXT_RULER_DEFS_TEX) --lhs2tex --base=$(TEXT_RULEX) $< > $@

$(TEXT_RULEUX_DRV_RUL): $(TEXT_RULEUX_SRC_CRUL) $(SHUFFLE)
	$(SHUFFLE) --gen=20 --plain --order="1<20" --lhs2tex=no $< > $@

$(TEXT_RULEUX_DRV_LTEX): $(TEXT_RULEUX_DRV_RUL) $(RULER2)
	$(RULER2) $(RULER2_OPTS) --markchanges="* - E" --lhs2tex --base=$(TEXT_RULEUX) $< > $@

$(TEXT_EXPERIMENTS_SUBST_DRV_RUN_TEX): $(TEXT_EXPERIMENTS_SUBST_SRC_RUN_TEX)
	mkdir -p $(@D)
	cp $< $@

