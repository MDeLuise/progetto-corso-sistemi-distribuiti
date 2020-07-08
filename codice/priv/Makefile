# VARIABLES:
APP_NAME := peer_transactions
SRC_PATH := ../src/
EBIN_PATH := ../ebin/

ERLANG_MODULES := $(EBIN_PATH)peer.beam $(EBIN_PATH)support_server.beam $(EBIN_PATH)bully.beam \
                  $(EBIN_PATH)tester.beam $(EBIN_PATH)$(APP_NAME)_app.beam \
                  $(EBIN_PATH)user_functions.beam $(EBIN_PATH)peer_node.beam \
                  $(EBIN_PATH)support_server_node.beam

JAVA_FILE := PeersCreation
PEER_CONF_FILE := .starting_peers.txt

DEBUG := false
ifeq ($(DEBUG), true)
DEBUG_FLAG := -D debug
else
DEBUG_FLAG :=
endif




# RULES:
.PHONY: run_app input_peers create_app compile run test clean


run_app: input_peers create_app
	chmod +x ./retrieve_strarting_peers.sh
	erl -pa $(EBIN_PATH) -$(APP_NAME) starting_peers \
	'$(shell ./retrieve_strarting_peers.sh $(PEER_CONF_FILE))' \
	-boot ../$(APP_NAME) -eval 'observer:start().'


input_peers: $(JAVA_FILE).class
	java $(JAVA_FILE)


create_app: compile ../$(APP_NAME).boot


run: compile
	erl -pa $(EBIN_PATH) -eval 'observer:start().'


compile: $(ERLANG_MODULES)


test: compile
	erl -pa $(EBIN_PATH) -eval 'tester:test()'


$(EBIN_PATH)%.beam: $(SRC_PATH)%.erl
	erlc -o $(EBIN_PATH) $(DEBUG_FLAG) $<


%.class: %.java
	javac $<


../$(APP_NAME).boot: ../$(APP_NAME).rel
	erl -noshell -pa $(EBIN_PATH) -eval \
	'systools:make_script("peer_transactions",[local, {path,[".."]}, {outdir, ".."}, no_warn_sasl]).' \
	-eval 'init:stop()'


../$(APP_NAME).rel:
	chmod +x ./create_rel_file.sh
	$(shell ./create_rel_file.sh ../$(APP_NAME).rel)


clean:
	rm -f $(EBIN_PATH)*.beam ../$(APP_NAME).boot ../$(APP_NAME).script erl_crash.dump \
	*.class $(PEER_CONF_FILE) $(EBIN_PATH)erl_crash.dump ../$(APP_NAME).rel