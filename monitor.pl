:- use_module(library(http/json)).
:- use_module(monitor(trace_expressions_semantics)).
:- initialization(main).

%% arguments
%% - a specification file
%% - a log file containing the trace
%% optional
%% - --silent
%% - --reject
%% remark (Davide) flags should be managed in a better and more efficient way with conditional compilation
%% :- if(:Goal) ... :- elif(:Goal) ... :- else ... :- endif
%% see https://www.swi-prolog.org/pldoc/man?section=conditionalcompilation
%% flag debug should be added to merge monitor.pl with monitor_debug.pl

main :-
    current_prolog_flag(argv, [SpecFile, TraceFile | _]), %!,
    load_spec(SpecFile,TraceExp),
    read_trace(TraceFile, TraceStream),
    (verify(TraceStream, TraceExp, 1),\+ reject ->
	 (lognl('Execution terminated correctly'), Exit=0); (lognl('Trace did not match specification'), Exit=1)),
    close(TraceStream),
    halt(Exit).

main :-
	writeln('expected args: <spec file> <trace file>'),
	halt(1).

file_not_found(Fname) :- write('File not found: '), writeln(Fname), halt(1).

load_spec(SpecFile,TraceExp) :-
    catch(
	(use_module(SpecFile), trace_expression(_, TraceExp)),
	error(existence_error(_,Fname),_),
	file_not_found(Fname)
    ).

read_trace(TraceFile, TraceStream) :-
    catch(
    	open(TraceFile, read, TraceStream),
    	Err,
    	(Err=error(existence_error(_,Fname),_)->
	     file_not_found(Fname); (writeln('Illegal JSON object'), halt(1))
	)
    ).

% true if --silent flag was given
silent :-
	current_prolog_flag(argv, [_, _ | Arguments]),
	member('--silent', Arguments).

% true if --reject flag was given
reject :-
	current_prolog_flag(argv, [_, _ | Arguments]),
	member('--reject', Arguments).

% only print if not in silent mode
log(X) :- silent -> true ; write(X).
lognl(X)  :- silent -> true ; writeln(X).


%% verify(TraceStream, TraceExp, EventId) :-
%% 	at_end_of_stream(TraceStream) ->
%% 		verify_end(TraceExp) ;
%% 		verify_events(TraceStream, TraceExp, EventId).

% check wether end of trace is allowed
verify_end(TraceExp) :- may_halt(TraceExp) ->
	true ;
	(log('Unexpected end of trace\n'), false).

% verify one event and then proceed recursively
%%% important remark:
%%% json_read_dict(TraceStream, Event) throws error(syntax_error(json(unexpected_end_of_file)),_)
%%% if EOF is hit, that is, before at_end_of_stream(TraceStream) succeeds
%%% the workaround json_read_dict(TraceStream, Event,[end_of_file(empty{})]) does not work
%%% because anyway an extraneous event must be returned at the end of the file (the empty event in the
%%% proposed workaround) that may invalidate the specification
%%% the proposed solution is to catch the exception and call verify_end. 
%%% the previous version of verify (commented) is useless, 'verify_events' has been renamed 'verify'

%%% output format
%%% dict_pairs(Event, _, Fields), log(Fields) replaced with log(Event) but not sure which is the more readable output 
verify(TraceStream, TraceExp, EventId) :-
    catch( 
	(
	    json_read_dict(TraceStream, Event),
	    (next(TraceExp, Event, NewTraceExp)
	    -> (log('matched event #'), log(EventId), log(': '), lognl(Event), NewEventId is EventId+1, verify(TraceStream, NewTraceExp, NewEventId))
	    ;  (log('ERROR on event #'), log(EventId), log(': '), lognl(Event), false)
	    )
	),
	error(syntax_error(json(unexpected_end_of_file)),_),
	verify_end(TraceExp)).
	
