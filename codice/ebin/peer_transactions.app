{application, peer_transactions,
[{description, "Peer-to-peer transactions application"},
{vsn, "1.0"},
{modules, [peer, support_server, bully, tester, user_functions]},
{registered, [support_server]},
{applications, [kernel, stdlib]},
{env, [{starting_peers, []}]},
{mod, {peer_transactions_app,[]}}]}.