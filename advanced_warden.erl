%%%-------------------------------------------------------------------
%%% @author Liam Maloney
%%% @copyright (C) 2016
%%% @doc
%%%
%%% The following file contains an altered version of the warden module,
%%% which uses concurrency to achieve speedups in game execution.
%%%
%%% @end
%%% Created : 29. Dec 2016 16:14
%%%-------------------------------------------------------------------

-module(advanced_warden).

-author("Liam Maloney").

-export([start/0, gameSupervisor/3, warden/0, playGameNTimes/3, createETSTablesUnder/2]).

-include_lib("logging_macros.hrl").
-include_lib("eunit/include/eunit.hrl").


start() ->
  NewWardenPID = spawn(?MODULE, warden, []),
  ?LOGPROCESS({?LINE, ?MODULE},["Initialised new advanced_warden process with PID: ", NewWardenPID]),
  NewWardenPID.


createETSTablesUnder(Process, ETSTablesAndOptions) ->
  Process ! {self(), initialise_ets_under_self, ETSTablesAndOptions},
  receive {Process, done_initialising_ETS} -> ok end.


playGameNTimes(WardenPID, N, ETSTables) ->
  WardenPID ! {self(), start_playing, N, ETSTables},
  receive {GameSupervisor, supervisor_observed_all_finished} -> ok end.


initialiseETSTables(TablesToInitialise) ->
  ?LOGETS({?LINE, ?MODULE},["Creating the following tables under warden ", self(), ": ", TablesToInitialise]),
  lists:foreach(fun({TableName, TableOptions}) -> ets:new(TableName, TableOptions) end, TablesToInitialise).


addNewPrisoner(PrisonerTableName, NewPrisonerInformation, NameOfSentencesTable) ->
  ?LOGETS({?LINE, ?MODULE},["Adding prisoner ", NewPrisonerInformation, " to ", PrisonerTableName, " table."]),
  ets:insert(PrisonerTableName, NewPrisonerInformation),
  {PrisonerPID, _, _} = NewPrisonerInformation,
  ets:insert(NameOfSentencesTable, {PrisonerPID, 0}).


retrieveAllRecordsFromETSTable(TableName) ->
  ?LOGETS({?LINE, ?MODULE},["Reading all records from ", TableName, "."]),
  ets:foldl(fun(PrisonerSentence, SentenceAcc) -> [PrisonerSentence | SentenceAcc] end, [], TableName).


spawnGameSupervisorToMonitorGamesUntilFinished(Requester, NumIterations, ETSTables) ->
  {PrisonerTable,_,_,_} = ETSTables,
  PrisonersToWaitOn = ets:info(PrisonerTable, size),
  GameSupervisor = spawn(?MODULE, gameSupervisor, [PrisonersToWaitOn, NumIterations, Requester]),
  GameSupervisor ! {self(), begin_iterations, ETSTables},

  ?LOGPROCESS({?LINE, ?MODULE},
    [ "Warden ", self(), " ",
      "spawned GameSupervisor ", GameSupervisor, " ",
      "to supervise ", NumIterations, " ",
      "iterations"]
  ),

  ?LOGETS({?LINE, ?MODULE},
    [ "Reading ", PrisonerTable, " table, ",
      GameSupervisor, " knows it must wait on ", PrisonersToWaitOn, " Prisoners to complete."]
  ),

  ?LOGSUPERVISOR({?LINE, ?MODULE},[GameSupervisor, " starting ", NumIterations, " iterations for ", Requester, "."]).


warden() ->

  receive

    {Sender, initialise_ets_under_self, ETSTablesAndOptions} ->
      initialiseETSTables(ETSTablesAndOptions),
      Sender ! {self(), done_initialising_ETS};

    {Sender, add_new_prisoner, PrisonerInformation, ToTable, InitializeSentenceCounterInThisTable} ->
      addNewPrisoner(ToTable, PrisonerInformation, InitializeSentenceCounterInThisTable),
      Sender ! {self(), done_adding_prisoner};

    {Sender, retrieve_prisoner_sentences, ContainedInTable} ->
      AllPrisonerSentences = retrieveAllRecordsFromETSTable(ContainedInTable),
      Sender ! {self(), done_retrieving_prisoner_sentences, AllPrisonerSentences};

    {CallbackToWhenFinished, start_playing, NumIterations, ETSTables} ->
      spawnGameSupervisorToMonitorGamesUntilFinished(CallbackToWhenFinished, NumIterations, ETSTables)

  end,
  warden().


kickOffPrisoners(NumGamesForThemToPlay, ETSTables) ->
  {PrisonerTable,_,_,_} = ETSTables,
  ?LOGSUPERVISOR({?LINE, ?MODULE},[self(), " beginning games.  Broadcasting to all in ", PrisonerTable, "."]),
  broadcastToAllPrisoners({self(), play_n_games, NumGamesForThemToPlay, ETSTables}, ETSTables).


broadcastToAllPrisoners(Message, ETSTables) ->
  {PrisonerTable,_,_,_} = ETSTables,
  lists:foreach(fun(ETSRow) -> {PrisonerPID, _, _} = hd(ETSRow), PrisonerPID ! Message end, ets:match(PrisonerTable, '$1')).


gameSupervisor(NumPrisonersWaitingOn, NumIterations, CallbackToWhenFinished) ->
  receive
    {Warden, begin_iterations, ETSTables} ->
      kickOffPrisoners(NumIterations, ETSTables),
      waitForAllPrisonersToFinish(NumPrisonersWaitingOn, CallbackToWhenFinished)
  end.


waitForAllPrisonersToFinish(0, NotifyHasFinished) ->
  ?LOGSUPERVISOR({?LINE, ?MODULE},[self(), " has observed all prisoners finishing.  Notifying: ", NotifyHasFinished]),
  NotifyHasFinished ! {self(), supervisor_observed_all_finished};
waitForAllPrisonersToFinish(NumPrisonersWaitingOn, CallbackToWhenFinished) ->
  receive
    {_, prisoner_finished_games} ->
      ?LOGSUPERVISOR({?LINE, ?MODULE},[self(), " now waiting on ", NumPrisonersWaitingOn, " prisoners."]),
      waitForAllPrisonersToFinish(NumPrisonersWaitingOn - 1, CallbackToWhenFinished)
  end.

