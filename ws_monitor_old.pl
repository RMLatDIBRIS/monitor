%% obsolete version, replaced by iot_ws(s)_monitor.pl

:- use_module(library(http/websocket)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_client)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).

:- use_module(trace_expressions_semantics).

:- http_handler(/,http_upgrade_to_websocket(manage_event, []),[]). %%% default options for both the websocket and the http handler 

%% arguments
%% the server expects a required first argument: the filename containing the specified trace expression
%% second optional argument: a log file, if not provided no logging is performed

%% example:
%% swipl -p node=prolog prolog/server.pl -- http/omitted\ body/204\ response/spec.pl prolog_server_log.txt
%% -p node=prolog
%%          required option to indicate the path to func_match.pl (event domain implementation)
%%  http/omitted\ body/204\ response/spec.pl
%%          the trace expression (required argument)
%%  prolog_server_log.txt
%%          logging enabled to file prolog_server_log.txt (optional argument)

% load specification

:- current_prolog_flag(argv, [Spec|_]), use_module(Spec).

server(Port) :- http_server(http_dispatch,[port('10.251.61.71':Port),workers(1)]). %% one worker to guarantee event sequentiality
%server(Port) :- http_server(http_dispatch,[port('192.168.1.250':Port),workers(1)]). %% one worker to guarantee event sequentiality

log(Log) :-
    nb_getval(log,Stream), Stream\==null->  %% optional logging of server activity
	(Log=(TE,E)->
	     writeln(Stream,"Trace expression:"),writeln(Stream,TE),writeln(Stream,"Event: "),writeln(Stream,E);
	 writeln(Stream,"Error")),
	nl(Stream),
	flush_output(Stream);
    true.
		 
manage_event(WebSocket) :-
    ws_receive(WebSocket, Msg, [format(json),value_string_as(atom)]), %% value_string_as(atom) passed as option to json_read_dict/3
    (Msg.opcode==close ->
	     true;
	 E=Msg.data,
	       nb_getval(state,TE1),
	       log((TE1,E)),
	       (next(TE1,E,TE2) -> nb_setval(state,TE2),Reply=E; Reply=_{error:true}),
	       %% (next(TE1,E,TE2) -> nb_setval(state,TE2),Reply='{"error":false}'; Reply='{"error":true}',log(error)),
	       %% next line: more detailed information computed in case an error occurs
	       %% (next(TE1,Msg.data,TE2) -> nb_setval(state,TE2),Reply='{"error":false}'; term_string(TE1,State),atomics_to_string(['{"error":true, "state":',State,', "event":', Msg.data, '}'], Reply)),
	       atom_json_dict(Json,Reply,[as(string)]),
	       ws_send(WebSocket,string(Json)),
	       manage_event(WebSocket)).

%% old patched version to avoid problems with json dicts, resolved by using value_string_as(atom) option
%% manage_event(WebSocket) :-
%%     ws_receive(WebSocket, Msg), %% uses string as default format, format(json) does not work properly, dict implementation not stable 
%%     (Msg.opcode==close ->
%%      true;
%%      nb_getval(state,TE1),
%%      log((TE1,Msg.data)),
%%      (next(TE1,Msg.data,TE2) -> nb_setval(state,TE2),Reply='{"error":false}'; Reply='{"error":true}'),
%%      %% next line: more detailed information computed in case an error occurs
%%      %% (next(TE1,Msg.data,TE2) -> nb_setval(state,TE2),Reply='{"error":false}'; term_string(TE1,State),atomics_to_string(['{"error":true, "state":',State,', "event":', Msg.data, '}'], Reply)),
%%      ws_send(WebSocket,text(Reply)),
%%      manage_event(WebSocket)).
		
exception(undefined_global_variable, state, retry) :- trace_expression(_, TE), nb_setval(state,TE).
exception(undefined_global_variable, log, retry) :- (current_prolog_flag(argv, [_,LogFile|_])->open(LogFile,append,Stream);Stream=null),nb_setval(log, Stream).

:- server(80).
