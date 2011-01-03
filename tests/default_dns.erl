%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%%% @copyright (C) 2010 Inaka Labs SRL
%%% @doc "Always default" DNS server
%%% @end
%%%-------------------------------------------------------------------
-module(default_dns).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').

-behaviour(gen_dns).

-include_lib("kernel/src/inet_dns.hrl").

-export([start/0, stop/0]).
-export([init/1, handle_query/2, handle_call/3, terminate/2]).

-record(state, {}).
-opaque state() :: #state{}.

-spec start() -> {ok, pid()}.
start() ->
  gen_dns:start({local, ?MODULE}, ?MODULE, [], [{port, 53}]).

-spec stop() -> ok.
stop() ->
  gen_dns:call(?MODULE, stop).

-spec init([]) -> {ok, state()}.
init([]) ->
  io:format("DEFAULT_DNS: Initializing...~n", []),
  {ok, #state{}}.

-spec handle_query(#dns_rec{}, state()) -> {ok, default, state()}.
handle_query(Q, State) ->
  io:format("DEFAULT_DNS: Query received: ~p~n", [Q]),
  {ok, default, State}.

-spec handle_call(stop, reference(), state()) -> {stop, normal, ok, state()}.
handle_call(stop, _From, State) ->
  io:format("DEFAULT_DNS: Stopping.~n", []),
  {stop, normal, ok, State}.

-spec terminate(term(), state()) -> ok.
terminate(Reason, _State) ->
  io:format("DEFAULT_DNS: Terminating: ~p~n", [Reason]),
  ok.