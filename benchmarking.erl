%%%-------------------------------------------------------------------
%%% @author Liam
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%     A file to run the benchmark suites based on the inputs from
%%%     the console.
%%% @end
%%% Created : 19. Dec 2016 15:51
%%%-------------------------------------------------------------------
-module(benchmarking).

-author("Liam Maloney").
-student_number("C00179434").

-export([benchmarking/2, customProgrammaticBench/0]).
-student_number("C00179434").


customProgrammaticBench() ->

  PrisonersAndStrategies = [
    {liam, titForTat},
    {joe, titForTat},
    {sam, random}
  ],

  NumberOfIterations = 1000,

  io:format("~n~n~p~n~n", [benchmarking(NumberOfIterations, PrisonersAndStrategies)]).


benchmarking(NumIterations, PrisonersAndStrategies) ->
  {{AdvTime, ok}, AdvScores}    = benchmarkAdvancedVersion(NumIterations, PrisonersAndStrategies),
  {{SimTime, ok}, SimScores}    = benchmarkSimpleVersion(NumIterations, PrisonersAndStrategies),
  io:format("~p~n",[{{advanced,  {microseconds, AdvTime}}, {simple,    {microseconds, SimTime}}}]).


etsClear() ->
  ets:delete(prisoners),
  ets:delete(game_outcomes),
  ets:delete(prison_sentences),
  ets:delete(prisoners_memory).

benchmarkAdvancedVersion(NumIterations, PrisonersAndStrategies) ->

  PrisonerTableName         = prisoners,
  HistoryTableName          = game_outcomes,
  SummaryTableName          = prison_sentences,
  PrisonersMemoryTableName  = prisoners_memory,

  InitializedPrisoners = createAdvancedPrisoners(PrisonersAndStrategies),

  ETSSetup = [
    {PrisonerTableName,         [set, named_table, public, {read_concurrency, true}]},
    {HistoryTableName,          [duplicate_bag, named_table, public]},
    {SummaryTableName,          [set, named_table, public, {write_concurrency, true}]},
    {PrisonersMemoryTableName,  [set, named_table, public]}
  ],

  Warden = advanced_warden:start(),
  advanced_warden:createETSTablesUnder(Warden, ETSSetup),
  incarceratePrisoners(Warden, InitializedPrisoners, PrisonerTableName, SummaryTableName),
  ETSTables = {PrisonerTableName, HistoryTableName, SummaryTableName, PrisonersMemoryTableName},
  Time = timer:tc(advanced_warden, playGameNTimes, [Warden, NumIterations, ETSTables]),
  Warden ! {self(), retrieve_prisoner_sentences, SummaryTableName},
  receive {Warden, done_retrieving_prisoner_sentences, Sentences} -> ok end,
  Map = toMap(Sentences, PrisonerTableName),
  etsClear(),
  {Time, Map}.


toMap(AdvSentences, PrisonerTableName) ->
  lists:foldl(
    fun({PrisonerPID, Years}, MapAcc) ->
      [{_, Name, _}] = ets:lookup(PrisonerTableName, PrisonerPID),
      maps:put(Name, Years, MapAcc)
    end,
    maps:new(),
    AdvSentences
  ).


benchmarkSimpleVersion(NumIterations, PrisonersAndStrategies) ->

  InitializedPrisoners = createSimplePrisoners(PrisonersAndStrategies),
  Warden = warden:start({[],maps:new(),[]}),
  lists:foreach(fun(Prisoner) -> warden:add(Warden, Prisoner) end, InitializedPrisoners),
  Time = timer:tc(warden, run, [Warden, NumIterations]),
  Warden ! {self(), stats},
  receive {Warden, Sentences} -> ok end,
  {Time, Sentences}.


createAdvancedPrisoners(PrisonersAndStrategiesToInitialise) ->
  lists:foldl(
    fun({Name, Strategy}, PrisonerAcc) ->
      [ {advanced_prisoner:start(Name, Strategy), Name, Strategy} | PrisonerAcc ]
    end, [], PrisonersAndStrategiesToInitialise).


createSimplePrisoners(PrisonersAndStrategiesToInitialise) ->
  lists:foldl(
    fun({Name, Strategy}, PrisonerAcc) ->
      [ prisoner:create(Name, Strategy, maps:new(), []) | PrisonerAcc ]
    end, [], PrisonersAndStrategiesToInitialise).


incarceratePrisoners(UnderWarden, PrisonersToIncarcerate, IntoTable, SummaryTableName) ->
  lists:foreach(
    fun(Prisoner) ->
      UnderWarden ! {self(), add_new_prisoner, Prisoner, IntoTable, SummaryTableName},
      receive
        {UnderWarden, done_adding_prisoner} -> ok
      end
    end, PrisonersToIncarcerate).


