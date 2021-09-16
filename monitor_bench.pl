:- use_module(library(http/json)).
:- use_module(trace_expressions_semantics).
:- initialization(main).

%% arguments
%% - a specification file
%% - a log file containing the trace
%% optional
%% - --silent
%% - --reject

main :-
	current_prolog_flag(argv, [SpecFile, TraceFile | _]), !,
	catch(
		(use_module(SpecFile), trace_expression(_, TraceExp)),
		_,
		(write('File not found\n'), halt(1))),
	catch(
		read_trace(TraceFile, TraceStream),
		_,
		(write('Illegal JSON object\n'), halt(1))),
	(verify(TraceStream, TraceExp, 1) -> Accepted=true ; Accepted=false),
	close(TraceStream),
	(reject -> negate(Accepted, Result) ; Result=Accepted),
	(Result=true ->
		(log('Execution terminated correctly\n'), halt(0)) ;
		(log('Trace did not match specification\n'), halt(1))).

main :-
	write('expected args: <spec file> <trace file>\n'),
	halt(1).

negate(false, true).
negate(true, false).

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
lognl  :- silent -> true ; nl.

read_trace(TraceFile, TraceStream) :-
    catch(
    	open(TraceFile, read, TraceStream),
    	_,
    	(write('trace file not found'), nl, halt(1))).

%% verify(TraceStream, TraceExp, EventId) :-
%% 	at_end_of_stream(TraceStream) ->
%% 		verify_end(TraceExp) ;
%% 		verify_events(TraceStream, TraceExp, EventId).

% check wether end of trace is allowed
verify_end(TraceExp) :- may_halt(TraceExp) ->
	true ;
	(log('Unexpected end of trace\n'), false).

%%% the workaround json_read_dict(TraceStream, Event,[end_of_file(empty{})]) does not work
%%% because anyway an extraneous event must be returned at the end of the file (the empty event in the
%%% proposed workaround) that may invalidate the specification
%%% the proposed solution is to catch the exception and call verify_end. 
%%% the previous version of verify (commented) is useless, 'verify_events' has been renamed 'verify'

verify(TraceStream, TraceExp, EventId) :-
    catch( 
	(
	    json_read_dict(TraceStream, Event),
	    (next(TraceExp, Event, NewTraceExp)
	    -> (log('matched event #'), log(EventId), log(': '), log(Event), lognl, log(NewTraceExp), lognl, NewEventId is EventId+1, verify(TraceStream, NewTraceExp, NewEventId))
	    ;  (log('ERROR on event #'), log(EventId), log(': '), log(Event), lognl, false)
	    )
	),
	error(syntax_error(json(unexpected_end_of_file)),_),
	verify_end(TraceExp)).
