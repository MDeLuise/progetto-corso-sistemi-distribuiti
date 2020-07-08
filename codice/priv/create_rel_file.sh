#!/usr/bin/env escript

main(FileName) ->
    
    [Kernel | Other] = lists:filter(fun(A) -> element(1, A) == kernel end,
        application:which_applications()),
    [Stdlib | Other] = lists:filter(fun(A) -> element(1, A) == stdlib end,
        application:which_applications()),
    
    Kernel_vsn = element(3, Kernel),
    Stdlib_vsn = element(3, Stdlib),

    Txt = "{release, {\"Peer transactions application\", \"P1\"}, {erts, \"5.6.2\"},\n" ++
          "[{kernel, \"" ++ Kernel_vsn ++ "\"},\n{stdlib, \"" ++ Stdlib_vsn ++ "\"},\n" ++
          "{peer_transactions, \"1.0\"}]}.",
    file:write_file(FileName, Txt).