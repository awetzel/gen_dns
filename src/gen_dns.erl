%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%%% @copyright (C) 2010 Inaka Labs SRL
%%% @doc Generic DNS server
%%%
%%% The user module should export:
%%%<ul>
%%%   <li>
%%%   <pre>init(Args::term()) -> {ok, State::term()} |
%%%                               {stop, Reason::term()}</pre>
%%%     Opens and/or initializes the server.<br/>
%%%   </li><li>
%%%   <pre>handle_query(Query::#dns_rec{}, State::term()) ->
%%%         {ok, Response::#dns_rec{} | default, State} |
%%%         {error, Reason :: term(), State}</pre>  
%%%     Given a DNS Query, returns the corresponding DNS record.  If it returns 'default',
%%%     the gen_dns server will query its default DNS Server to resolve the query<br/>
%%%   </li><li>
%%%   <pre>handle_call(Msg::term(), From::reference(), State::term()) ->
%%%        {reply, Reply::term(), State::term()}
%%%        {noreply, State::term()}
%%%        {stop, Reason::normal | shutdown | term(), Reply::term(), State::term()} %% terminate(State) is called</pre>
%%%     Called from <code>gen_dns:call/2</code><br/>
%%%   </li><li>
%%%   <pre>terminate(Reason :: normal | shutdown | term(), State) -> _</pre>
%%%     Let the user module clean up.  Always called when server terminates.<br/>
%%%   </li></ul>
%%% @end
%%%-------------------------------------------------------------------
-module(gen_dns).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').

-behaviour(gen_server).

-include_lib("kernel/src/inet_dns.hrl").
-define(DEFAULT_HOST, {8,8,8,8}).
-define(DEFAULT_PORT, 53).

-define(DEFAULT_SOCKET_TIMEOUT, 35000).
-define(UDP_OPTIONS,[binary, {active, false}, {reuseaddr, true}]).

%%% @type ip_address() = {pos_integer(), pos_integer(), pos_integer(), pos_integer()}
%%% @type start_option() = {timeout, non_neg_integer() | infinity | hibernate} |
%%%                        {debug, [trace | log | {logfile, string()} | statistics | debug]} | 
%%%                        {default_server, {ip_address(), pos_integer()}} |
%%%                        {port, pos_integer()}
-type ip_address() :: {1..255, 1..255, 1..255, 1..255}.
-type start_option() :: {timeout, non_neg_integer() | infinity | hibernate} | {debug, [trace | log | {logfile, string()} | statistics | debug]} | {default_server, {ip_address(), 1..65535}} | {port, 1..65535}.
-type start_result() :: {ok, pid()} | {error, {already_started, pid()}} | {error, term()}.

-record(state, {module    :: atom(), % Callback module
                mod_state :: term(), % Callback module state
                def_host  :: ip_address(),
                def_port  :: 1..65535,
                port      :: 1..65535,
                listener  :: reference()
               }).
-opaque state() :: #state{}.

%% BEHAVIOUR
-export([behaviour_info/1]).

%% API
-export([start/3, start/4, start_link/3, start_link/4,
         call/2, call/3, reply/2]).

%% GEN SERVER
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEHAVIOUR FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% @hidden
-spec behaviour_info(callbacks | term()) -> undefined | [{atom(), non_neg_integer()}].
behaviour_info(callbacks) ->
  [{init, 1}, {handle_query, 2}, {handle_call, 3}, {terminate, 2}];
behaviour_info(_Other) ->
  undefined.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% @doc  Starts a generic DNS server.
%%% @spec start(Mod::atom(), Args::term(), Options::[start_option()]) ->
%%%         {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::term()}
-spec start(Mod::atom(), Args::term(), Options::[start_option()]) -> start_result().
start(Mod, Args, Options) ->
  {DefaultHost, DefaultPort, Port, OtherOptions} = parse_options(Options),
  gen_server:start(?MODULE, {Mod, Args, DefaultHost, DefaultPort, Port}, OtherOptions).

%%% @doc  Starts a named generic DNS server.
%%% @spec start(Name::{local | global, atom()}, Mod::atom(), Args::term(), Options::[start_option()]) ->
%%%         {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::term()}
-spec start(Name::{local|global, atom()}, Mod::atom(), Args::term(), Options::[start_option()]) -> start_result().
start(Name, Mod, Args, Options) ->
  {DefaultHost, DefaultPort, Port, OtherOptions} = parse_options(Options),
  gen_server:start(Name, ?MODULE, {Mod, Args, DefaultHost, DefaultPort, Port}, OtherOptions).

%%% @doc  Starts and links a generic DNS server.
%%% @spec start_link(Mod::atom(), Args::term(), Options::[start_option()]) ->
%%%         {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::term()}
-spec start_link(Mod::atom(), Args::term(), Options::[start_option()]) -> start_result().
start_link(Mod, Args, Options) ->
  {DefaultHost, DefaultPort, Port, OtherOptions} = parse_options(Options),
  gen_server:start_link(?MODULE, {Mod, Args, DefaultHost, DefaultPort, Port}, OtherOptions).

%%% @doc  Starts and links a named generic DNS server.
%%% @spec start_link(Name::{local | global, atom()}, Mod::atom(), Args::term(), Options::[start_option()]) ->
%%%         {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::term()}
-spec start_link(Name::{local|global, atom()}, Mod::atom(), Args::term(), Options::[start_option()]) -> start_result().
start_link(Name, Mod, Args, Options) ->
  {DefaultHost, DefaultPort, Port, OtherOptions} = parse_options(Options),
  gen_server:start_link(Name, ?MODULE, {Mod, Args, DefaultHost, DefaultPort, Port}, OtherOptions).

%%% @doc Make a call to a generic server.
%%% If the server is located at another node, that node will be monitored.
%%% If the client is trapping exits and is linked server termination is handled here
%%% @spec call(Name::atom() | pid() | {global, atom()}, Request::term()) -> Response::term()
-spec call(Name::atom() | pid() | {global, atom()}, Request::term()) -> Response::term().
call(Name, Request) ->
  gen_server:call(Name, Request).

%%% @doc Make a call to a generic server.
%%% If the server is located at another node, that node will be monitored.
%%% If the client is trapping exits and is linked server termination is handled here
%%% @spec call(Name::atom() | pid() | {global, atom()}, Request::term(), Timeout::non_neg_integer()|infinity) -> Response::term()
-spec call(Name::atom() | pid() | {global, atom()}, Request::term(), Timeout::non_neg_integer()|infinity) -> Response::term().
call(Name, Request, Timeout) ->
  gen_server:call(Name, Request, Timeout).

%%% @doc  Send a reply to the client
%%% @spec reply(From::reference(), Reply::term()) -> {term(), term()}
-spec reply(reference(), X) -> {_, X}.
reply(From, Reply) ->
  gen_server:reply(From, Reply).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GEN SERVER FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @hidden
-spec init({atom(), term(), ip_address(), 1..65535, 1..65535}) -> {ok, #state{}} | {stop, term()}.
init({Mod, InitArgs, DefHost, DefPort, Port}) ->
  case Mod:init(InitArgs) of
    {ok, ModState} ->
      {ok, Pid} = dns_listener:start_link(Port, ?UDP_OPTIONS),
      {ok, #state{listener  = erlang:monitor(process, Pid),
                  module    = Mod,
                  mod_state = ModState,
                  def_host  = DefHost,
                  def_port  = DefPort,
                  port      = Port}};
    Other ->
      Other
  end.

%% @hidden
-spec handle_call(term(), reference(), state()) -> {reply, term(), state()} | {noreply, term()} | {stop, normal | shutdown | term(), term(), state()}.
handle_call(Request, From, State = #state{module = Mod, mod_state = ModState}) ->
  case Mod:handle_call(Request, From, ModState) of
    {reply, Reply, NewModSt} -> {reply, Reply, State#state{mod_state = NewModSt}};
    {noreply, NewModSt} -> {noreply, State#state{mod_state = NewModSt}};
    {stop, Reason, Reply, NewModSt} -> {stop, Reason, Reply, State#state{mod_state = NewModSt}};
    Other -> Other
  end.

%% @hidden
-spec handle_cast(any(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) -> {noreply, State}.

%% @hidden
-spec handle_info({'DOWN', reference(), process, pid(), _} | {dns_request, port(), ip_address(), 1..65535, binary()} | term(), state()) -> {noreply, state()}.
handle_info({'DOWN', Listener, process, _Pid, _Info},
            State = #state{listener = Listener,
                           port     = Port}) ->
  {ok, Pid} = dns_listener:start_link(Port, ?UDP_OPTIONS),
  {noreply, State#state{listener = erlang:monitor(process, Pid)}};
handle_info({dns_request, Socket, IP, Port, Payload},
            State = #state{module    = Mod,
                           mod_state = ModState,
                           def_host  = DefHost,
                           def_port  = DefPort}) ->
  try
    {ok, DecPayload} = inet_dns:decode(Payload),
    {Response, NewModState} =
      case Mod:handle_query(DecPayload, ModState) of
        {ok, default, NewModSt} ->
          {default_response(DecPayload, DefHost, DefPort), NewModSt};
        {ok, Resp, NewModSt} ->
          {Resp, NewModSt};
        {error, Reason, NewModSt} ->
          error_logger:error_msg("Error trying to query ~p~nQuery was ~p.~n\tState was: ~p~n\tError is: ~p~n",
                                 [Mod, DecPayload, ModState, Reason]),
          {default_response(DecPayload, DefHost, DefPort), NewModSt}
      end,
    
    error_logger:info_msg(
      "~p -> ~p~n",
      [[{T, D} || #dns_query{domain = D, type = T} <- DecPayload#dns_rec.qdlist],
       [{T, D} || #dns_rr{data = D, type = T} <- Response#dns_rec.anlist]]),
    
    case Response of
      no_response -> ok;
      Response ->
        gen_udp:send(Socket, IP, Port, inet_dns:encode(Response))
    end,

    {noreply, State#state{mod_state = NewModState}}
  catch
    exit:Error ->
      error_logger:error_msg("Error in async accept: ~p.~n", [Error]),
      {stop, Error, State}
  end;
handle_info(_Info, State) -> {noreply, State}.

%% @hidden
-spec terminate(any(), #state{}) -> any().
terminate(Reason, #state{module = Mod, mod_state = ModState,
                         listener = Listener}) ->
  erlang:demonitor(Listener, [flush]),
  Mod:terminate(Reason, ModState).

%% @hidden
-spec code_change(any(), any(), any()) -> {ok, any()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PRIVATE FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_options(Options) ->
  {ServerIp, ServerPort, Rest0} =
    case lists:keytake(default_server, 1, Options) of
      {value, {default_server, {SIp, SPort}}, R0} ->
        {SIp, SPort, R0};
      {value, {default_server, SIp}, R0} ->
        {SIp, ?DEFAULT_PORT, R0};
      false ->
        {?DEFAULT_HOST, ?DEFAULT_PORT, Options}
    end,
  case lists:keytake(port, 1, Rest0) of
    {value, {port, Port}, R1} ->
      {ServerIp, ServerPort, Port, R1};
    false ->
      {ServerIp, ServerPort, ?DEFAULT_PORT, Rest0}
  end.

default_response(DecPayload, DefHost, DefPort) ->
  case DecPayload#dns_rec.anlist of
    [] ->
      Header = DecPayload#dns_rec.header,
      NewDecPayload =
          DecPayload#dns_rec{header = Header#dns_header{rcode = 0,
                                                        aa = not_set,
                                                        qr = true}},
      lists:foldl(
        fun(Query, AccDP) ->
                DefaultData = query_default_server(Query, DefHost, DefPort),
                AccHeader = AccDP#dns_rec.header,
                AccDP#dns_rec{header =
                                case (DefaultData#dns_rec.header)#dns_header.rcode of
                                  0 ->
                                    case AccHeader#dns_header.aa of
                                      not_set ->
                                        AccHeader#dns_header{aa =
                                                               (DefaultData#dns_rec.header)#dns_header.aa};
                                      _ ->
                                        AccHeader
                                    end;
                                  OtherCode ->
                                    case AccHeader#dns_header.aa of
                                      not_set ->
                                        AccHeader#dns_header{rcode = OtherCode,
                                                             aa =
                                                               (DefaultData#dns_rec.header)#dns_header.aa};
                                      _ ->
                                        AccHeader#dns_header{rcode = OtherCode}
                                    end
                                end,
                              anlist = DefaultData#dns_rec.anlist ++ AccDP#dns_rec.anlist,
                              nslist = DefaultData#dns_rec.nslist ++ AccDP#dns_rec.nslist,
                              arlist = DefaultData#dns_rec.arlist ++ AccDP#dns_rec.arlist}
        end, NewDecPayload, DecPayload#dns_rec.qdlist);
    _ ->
      no_response
  end.

query_default_server(Query, DefHost, DefPort) ->
  Question = inet_dns:encode(
               #dns_rec{
                        header = #dns_header{id = random:uniform(65535),
                                             opcode = 'query',
                                             rd = 1,
                                             aa = 1},
                        qdlist = [Query]
                       }),
  {ok, Socket} = gen_udp:open(0, [binary, {active, false}]),
  ok = gen_udp:send(Socket, DefHost, DefPort, Question),
  {ok, {DefHost, DefPort, Reply}} = gen_udp:recv(Socket, 65535),
  {ok, Response} = inet_dns:decode(Reply),
  ok = gen_udp:close(Socket),
  Response.