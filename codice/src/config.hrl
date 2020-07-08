%%% ======================================================================================
%%%  Configuration file containing settings of the network protocol
%%% ======================================================================================

-define(CHECK_NEIGHBOR_EVERY, 10000).  % peers timeout for checking neighbors [ms]
-define(CHECK_NODE_EVERY, 15000).      % support server timeout [ms]
-define(GOSSIP_EVERY, 5000).           % peers timeout for start gossiping [ms]
-define(SEND_EVERY, 3000).

-define(MIN_NEIGHBORS, 2).             % minimum number of neighbors for a node
-define(MONITORS_NUM, ?MIN_NEIGHBORS). % number of monitor processes for a send

-define(MAX_NODES_NUMBER, 5).          % length of node list keept in the support server
-define(RESPONSE_NODE_NUMBER, 3).      % number of nodes sent from the support server
                                       % to who require them



-record(variables, {x    :: integer(),
                    y    :: integer()
}).

-define(DEFAULT_VARS, #variables{x = 0, y = 0}).
-define(ALWAYS, fun(_) -> true end).