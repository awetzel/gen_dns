%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%%% @copyright (C) 2010 Inaka Labs SRL
%%% @doc "Always home" DNS server (there's no place like 127.0.0.1)
%%% @end
%%%-------------------------------------------------------------------
-module(localhost_dns).
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
  io:format("LOCALHOST_DNS: Initializing...~n", []),
  {ok, #state{}}.

-spec handle_query(#dns_rec{}, state()) -> {ok, #dns_rec{}, state()}.
handle_query(Q = #dns_rec{qdlist = [Question|_]}, State) ->
  io:format("LOCALHOST_DNS: Query received: ~p~n", [Q]),
  {ok, Q#dns_rec{anlist = [#dns_rr{domain = Question#dns_query.domain,
                                   class = Question#dns_query.class,
                                   type = Question#dns_query.type,
                                   ttl = 666,
                                   data = case Question#dns_query.type of
                                            a -> {127, 0, 0, 1};
                                            mx -> {50, "127.0.0.1"};
                                            _ -> "127.0.0.1"
                                          end}]}, State}.

-spec handle_call(stop, reference(), state()) -> {stop, normal, ok, state()}.
handle_call(stop, _From, State) ->
  io:format("LOCALHOST_DNS: Stopping.~n", []),
  {stop, normal, ok, State}.

-spec terminate(term(), state()) -> ok.
terminate(Reason, _State) ->
  io:format("LOCALHOST_DNS: Terminating: ~p~n", [Reason]),
  ok.