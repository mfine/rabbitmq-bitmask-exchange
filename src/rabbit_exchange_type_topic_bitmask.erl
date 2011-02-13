%% Topic exchange to bitmasked keys.
%%
%% This exchange distributes messages by finding all of the binding keys
%% that "support" the routing key's bits: r_key & b_key == r_key. The
%% binding key is treated as a bitmask for the routing key. After
%% finding all of the matching binding keys, a route is randomly chosen.
%%
%% This is rabbit_exchange_type_topic with a modified route() function.
%%

-module(rabbit_exchange_type_topic_bitmask).
-include_lib("rabbit_common/include/rabbit.hrl").

-behaviour(rabbit_exchange_type).

-export([description/0, route/2]).
-export([validate/1, create/2, recover/2, delete/3,
         add_binding/3, remove_bindings/3, assert_args_equivalence/2]).
-include_lib("rabbit_common/include/rabbit_exchange_type_spec.hrl").

-rabbit_boot_step({?MODULE,
                   [{description, "exchange type topic-bitmask"},
                    {mfa,         {rabbit_registry, register,
                                   [exchange, <<"topic-bitmask">>, ?MODULE]}},
                    {requires,    rabbit_registry},
                    {enables,     kernel_ready}]}).

description() ->
    [{name, <<"topic-bitmask">>},
     {description, <<"Topic exchange with bitmasking of binding and routing keys">>}].

%% Assumes that keys are integers in bit strings: r_key & b_key == r_key.
routing_key_match(<<BindingKey>>, <<RoutingKey>>) ->
    (RoutingKey band BindingKey) == RoutingKey.

%% Return all the matching routes.
route(#exchange{name = Name},
      #delivery{message = #basic_message{routing_key = RoutingKey}}) ->
    rabbit_router:match_bindings(Name,
                                 fun(#binding{key = BindingKey}) ->
                                     routing_key_match(BindingKey, RoutingKey)
                                 end).

validate(_X) -> ok.
create(_Tx, _X) -> ok.
recover(_X, _Bs) -> ok.
delete(_Tx, _X, _Bs) -> ok.
add_binding(_Tx, _X, _B) -> ok.
remove_bindings(_Tx, _X, _Bs) -> ok.
assert_args_equivalence(X, Args) ->
    rabbit_exchange:assert_args_equivalence(X, Args).