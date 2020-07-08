%%% ===============================================================================================
%%% types definition for peer
%%% ===============================================================================================
-include("config.hrl").


-type(trans_id()    :: {integer(), pid()}).            % {num, pid}
-type(trans_state() :: pending | confirmed | aborted).
-type(trans_seq()   :: {integer(), pid()}).            % {agreed_num, pid}


-type(node_pid()     :: {atom(), node()}).
-type(trans_id_node()    :: {integer(), node()}).
-type(trans_seq_node()   :: {integer(), node()}).

-type(guard()       :: fun((#variables{}) -> boolean())).
-type(command()     :: fun((#variables{}) -> #variables{})).


