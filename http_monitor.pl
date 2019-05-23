:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_client)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).

:- use_module(trace_expressions_semantics).

:- http_handler(/,manage_request,[]).


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

server(Port) :- http_server(http_dispatch,[port(localhost:Port),workers(1)]).

%TODO maybe is better to use the new representation for JSON?
% see http://www.swi-prolog.org/pldoc/man?section=jsonsupport

%TODO better way than using globals with exceptions?

log(TE,E) :- nb_getval(log,Stream), Stream\==null->writeln(Stream,"Trace expression:"),writeln(Stream,TE),writeln(Stream,"Event: "),writeln(Stream,E),writeln(Stream, ''),flush_output(Stream);true.

manage_request(Request) :-
    http_read_json(Request, E),
    nb_getval(state,TE1),
    log(TE1,E),
    (next(TE1,E,TE2)->nb_setval(state,TE2),reply_json(json([error=(@false)]));reply_json(json([error=(@true)]))).

exception(undefined_global_variable,state,retry) :- trace_expression(_, TE), nb_setval(state,TE).
exception(undefined_global_variable,log, retry) :- (current_prolog_flag(argv, [_,LogFile|_])->open(LogFile,append,Stream);Stream=null),nb_setval(log, Stream).

:- server(8081).
