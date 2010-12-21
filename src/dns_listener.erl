%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%%% @copyright (C) 2010 Inaka Labs SRL
%%% @doc DNS UDP listener
%%% @end
%%%-------------------------------------------------------------------
-module(dns_listener).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').

-define(REAL_DNS_SERVER, {8,8,8,8}).

-export([start_link/2]).

loop(Caller, Socket) ->
  {ok, {IP, Port, Payload}} = gen_udp:recv(Socket, 65535),
  Caller ! {dns_request, Socket, IP, Port, Payload},
  loop(Caller, Socket).

%% @doc starts the listener process
%% @spec start_link(pos_integer(), [proplists:property()]) -> {ok, pid()}
-spec start_link(1..65535, [proplists:property()]) -> {ok, pid()}.
start_link(Port, Options) ->
  Caller = self(),
  {ok, proc_lib:spawn_link(
     fun() ->
             case gen_udp:open(Port, Options) of
               {ok, Socket} ->
                 loop(Caller, Socket);
               Error ->
                 throw(Error)
             end
     end)}.