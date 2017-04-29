%%%-------------------------------------------------------------------
%%% @author Liam Maloney
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%     The following contains convenience functions for log formatting.
%%% @end
%%% Created : 13. Jan 2017 00:42
%%%-------------------------------------------------------------------

-module(logging_utils).

-author("Liam Maloney").
-student_number("C00179434").

-export([logFormat/3]).

isAlreadyString([]) ->
  true;
isAlreadyString([H|T]) ->
  case H < a of
    false -> false;
    true  -> isAlreadyString(T)
  end;
isAlreadyString(Arg) ->
  false.


nextLogItem(Arg) ->
  case isAlreadyString(Arg) of
    true  -> Arg;
    false -> toString(Arg)
  end.


toString(Term) -> lists:flatten(io_lib:format("~p",[Term])).
logFormat(LogPrefix, {Line, Module}, Log) -> io:format("{~p,~p}: ~p~n", [Line, Module, LogPrefix ++ aggregateLogArgs(Log)]).
aggregateLogArgs(Args) -> lists:foldl(fun(Arg, LogStrAcc) -> LogStrAcc ++ nextLogItem(Arg) end, "", Args).