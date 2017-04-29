#!/usr/bin/env escript

%%%-------------------------------------------------------------------
%%% @author Liam
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%     This file is designed to be the entry point of the benchmarking
%%%     of the simple and advanced versions of the iterative prisoners
%%%     dilemma.
%%% @end
%%% Created : 15. Jan 2017 18:27
%%%-------------------------------------------------------------------

-mode(compile).

main(_) ->

  StrategiesAndPrisoners  = parseInput(fun promptForPrisonerSetup/0),
  NumberOfIterationsToRun = parseInput(fun promptForNumberOfIterations/0),
  EnableDebug             = parseInput(fun promptForDebug/0),

  compileAllFiles(EnableDebug),

  io:format("

    Now beginning simulation.  A total of ~p one-on-one interactions will take place ...

  ", [totalNumberOfGames(StrategiesAndPrisoners, NumberOfIterationsToRun)]),


  timer:sleep(2000),

  io:format("~p", [benchmarking:benchmarking(NumberOfIterationsToRun, StrategiesAndPrisoners)]).


promptForDebug() ->
  io:read("
    Would you like logging enabled?
    (I have used logs in place of comments, but when enabled they will slow execution.)

    (y/n): ").


compileAllFiles(WithDebug) ->

  Files = [advanced_warden, advanced_prisoner, benchmarking, logging_utils, prisoner, warden],

  case WithDebug of
    y   -> lists:foreach(fun(File) -> compile:file(File, {d, debug}) end, Files);
    n   -> lists:foreach(fun(File) -> compile:file(File) end, Files)
  end.


totalNumberOfGames(PrisonerList, NumberOfIterationsToRun) ->
  (lists:foldl(fun(IndexOfPrisoner, Acc) ->
    Acc + IndexOfPrisoner
  end, 0, lists:seq(0, length(PrisonerList) - 1))) * NumberOfIterationsToRun.


parseInput(ParseInputForSection) ->
  case ParseInputForSection() of
    {ok, Input} -> Input;
    _ -> errorParsing()
  end.


errorParsing() ->
  io:format("There was an error parsing your input.  Please try again ... ~n~n"),
  halt().

promptForPrisonerSetup() ->
  io:read(
    "

          CHOOSE YOUR PRISONERS!

    Please enter a list of tuples of atoms in the form:

      [
        {prisoner_name, strategy},
        {prisoner_name, strategy},
        ...
        {prisoner_name, strategy}
      ].

    The strategies which are currently available are:

      titForTat            (will cooperate the first time, and mirror opponents move after that)
      suspiciousTitForTat  (will defect the first time, and mirror opponents move after that)
      grudger              (will cooperate with others until betrayed once)
      random               (approx. 50% variance between cooperate and defect)


    Your prisoners (Note: This parses an Erlang Term, so end input with a '.'!):

    ").


promptForNumberOfIterations() ->

  io:read(
    "

    How many times would you like each prisoner to play with each opponent?
    (Note: This parses an Erlang Term, so end input with a '.'!):  ").
