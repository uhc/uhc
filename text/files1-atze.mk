# all variants
TEXT_PUB_VARIANTS			+= ruler-doc shuffle-doc ehc-book
TEXT_PRIV_VARIANTS			+= flops06-ruler-paper flops06-ruler \
								pldi06-explimpl \
								hw06-impred \
								truu-explimpl truu-ruler \
								phd-paper phd-draft phd-tst phd \
								ehc-book-tst \
								scratch poster \
								slides-ruler slides-explimpl slides-explimpl-fpnl slides-overview

# subtext
TEXT_SUBS					+= AGMiniPrimer StoryIntro StoryEH1 StoryEH2 StoryAFP Scratch \
								SharedTypeLang SharedFIOpts \
								TopicRuler TopicExplImpl TopicGRIN TopicRec TopicKinds TopicDataTy TopicImpred TopicHM TopicExtRec TopicGADT TopicReflection TopicPartialTySig \
								SlidesIntro Slides SlidesPartTySig SlidesExplImpl SlidesImpred SlidesRuler SlidesShuffle SlidesGRIN \
								CodeFragsExplImpl \
								ToolDocShuffle ToolDocRuler \
								AppxNotation FrontMatter OldText


# chunk view order for text variants, use shuffle hierarchy as crude variant mechanism
# 1	: base (share)
# 2	: ehc book (previously phd)
# 3	: flops06-ruler, truu-ruler
# 4	: pldi06-explimpl, truu-explimpl
# 5	: hw06-impred
# 6	: afp (will be obsolete)
# 7	: scratch
# 8	: slides afp
# 9	: slides explimpl: base (share)
# 10: future
# 11: shuffle doc
# 12: garbage
# 13: poster
# 14: slides ruler
# 15: slides explimpl, general
# 16: slides explimpl, for fpnl dag
# 17: slides overview
# 18: slides: base (share)
# 19: ruler doc

TEXT_SHUFFLE_ORDER			+= 1 < 2, 1 < 3, 1 < 4, 1 < 5, 1 < 6, 1 < 7, 1 < 8, 18 < 9, 1 < 10, 1 < 11, 1 < 13, 18 < 14, 9 < 15, 9 < 16, 18 < 17, 1 < 18, 1 < 19

