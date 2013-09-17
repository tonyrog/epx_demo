
MODULES = \
	epx_equ \
	epx_bang \
	evt_buffer \
	epx_mon \
	eimage \
	eevent \
	mandel \
	edraw \
	eproc \
	epxview \
	epx_pixels \
	epx_QR \
	epx_anim_test \
	epx_sprite \
	epx_router \
	epx_zimage \
	epx_graph


EBIN = ../ebin
ERLC = erlc

override ERLC_FLAGS = -W 


OBJS = $(MODULES:%=$(EBIN)/%.beam)

TARGET_FILES = $(OBJS) 

debug: ERLC_FLAGS += -Ddebug 

all: $(TARGET_FILES)

debug: all

release: all

depend:
	edep -MM -o ../ebin $(ERLC_FLAGS) $(MODULES:%=%.erl) > depend.mk

dialyze:
	dialyzer --src -o dia.out $(ERLC_FLAGS) -c $(MODULES:%=%.erl) 

clean:
	rm -f $(OBJS)

-include depend.mk

$(EBIN)/%.beam:	%.erl
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN) $<
