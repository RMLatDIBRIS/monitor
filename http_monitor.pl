:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_client)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).

:- use_module(monitor(trace_expressions_semantics)).

:- http_handler(/,manage_request,[]).


%% arguments
%% the server expects a required first argument: the filename containing the specified trace expression
%% second optional argument: a log file, if not provided no logging is performed

%% example:
%% sudo swipl -O -p monitor=pathToPrologMonitor http_monitor.pl -- RMLspec.pl logFile.txt

% initialization of the state of the worker thread: loads the specification and initializes gobal variable 'state' with it

init :- current_prolog_flag(argv, Argv), Argv = [Spec|Rest], use_module(Spec), trace_expression(_, TE), nb_setval(state,TE),
(Rest = [LogFile|_]->open(LogFile,append,Stream);Stream=null),nb_setval(log_file, Stream).

:- thread_initialization(init).

%% predicates to manage the optional log file

:- if(nb_getval(log_file,null)).
log(_).
:- else.
log(Arg) :-
        nb_getval(log_file,Stream),
	(Arg=(TE,E)->
	     writeln(Stream,"Trace expression:"),writeln(Stream,TE),writeln(Stream,"Event: "),writeln(Stream,E);
	 writeln(Stream,"Error")),
	nl(Stream),
	flush_output(Stream).
:- endif.

server(Port) :- http_server(http_dispatch,[port(Port),workers(1)]).

manage_request(Request) :- 
    http_read_json_dict(Request, E),
    nb_getval(state,TE1),
    log((TE1,E)),
    (next(TE1,E,TE2)->nb_setval(state,TE2),reply_json_dict(_{error:false,data:E});reply_json_dict(_{error:true,data:E})).

:- initialization(server('localhost':80)).
