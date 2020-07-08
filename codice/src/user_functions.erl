-module(user_functions).

-include("../src/config.hrl").

-compile(export_all).




% Cmd functions
id(Vars) -> Vars.
zero(Vars) -> Vars#variables{x = 0, y = 0}.
inc(Vars) -> Vars#variables{x = Vars#variables.x + 1, y = Vars#variables.y + 1}.
dec(Vars) -> Vars#variables{x = Vars#variables.x - 1, y = Vars#variables.y - 1}.
swap(Vars) -> Vars#variables{x = Vars#variables.y, y = Vars#variables.x}.


% Guard functions
all(_) -> true.
positive(Vars) -> (Vars#variables.x >= 0) and (Vars#variables.y >= 0).
negative(Vars) -> (Vars#variables.x < 0) and (Vars#variables.y < 0).