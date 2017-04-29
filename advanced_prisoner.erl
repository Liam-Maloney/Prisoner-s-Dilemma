%%%-------------------------------------------------------------------
%%% @author Liam Maloney
%%% @copyright (C) 2016
%%% @doc
%%%
%%%     The following contains a parallel/concurrent version of the
%%%     prisoner file.
%%%
%%% @end
%%% Created : 29. Dec 2016 16:14
%%%-------------------------------------------------------------------

-module(advanced_prisoner).

-author("Liam Maloney").
-student_number("C00179434").

-export([
  start/2, titForTat/1, suspiciousTitForTat/1,
  watchUntilNIterationsOfOneOnOneMatchFinish/2,
  random/1, grudger/1, gameLobby/5, mockedTestingProcess/1
]).

-include("logging_macros.hrl").
-include_lib("eunit/include/eunit.hrl").


start(PrisonerName, StrategyImplementation) ->
  NewPrisonerPID = spawn(?MODULE, StrategyImplementation, [PrisonerName]),
  ?LOGPROCESS({?LINE,?MODULE}, ["Initialised new advanced_prisoner process with PID: ", NewPrisonerPID]),
  NewPrisonerPID.


gameMessageHandling(Message, GameLogicBelongingToStrategy) ->

  case Message of

    {ReportBackToWhenFinished, play_n_games, NumberOfTimesToPlayEachOpponent, ETSTables} ->
      findOpponentsAndPlay(ReportBackToWhenFinished, NumberOfTimesToPlayEachOpponent, GameLogicBelongingToStrategy, ETSTables);

    {GameLobby, join_game} -> GameLobby ! {self(), providing_game_logic, GameLogicBelongingToStrategy}

  end.


scanAllBelowMeInTableToFindOpponents(ForPrisoner, PrisonersTable) ->
  FirstOpponent = ets:next(PrisonersTable, ForPrisoner),
  setupGamePairings(FirstOpponent, [], PrisonersTable).
setupGamePairings('$end_of_table', ListOfOpponentPIDs, PrisonersTable) ->
  ListOfOpponentPIDs;
setupGamePairings(Opponent, ListOfOpponentPIDsAcc, PrisonersTable) ->
  NextOpponent = ets:next(PrisonersTable, Opponent),
  setupGamePairings(NextOpponent, [Opponent | ListOfOpponentPIDsAcc], PrisonersTable).


findOpponentsAndPlay(ReportBackToWhenFinished, NumberOfTimesToPlayEachOpponent, InstigatorsGameLogic, ETSTables) ->
  {PrisonersTable,_,_,_} = ETSTables,
  MyPairings = scanAllBelowMeInTableToFindOpponents(self(), PrisonersTable),
  ?LOGGAME({?LINE, ?MODULE},
    ["Prisoner ", self(), " will be the instigator against: ", MyPairings,
    ", and will play each one ", NumberOfTimesToPlayEachOpponent, " times."]
  ),
  case MyPairings of
    [] ->
      ReportBackToWhenFinished ! {self(), prisoner_finished_games};
    _ ->
      spawnProcessForEachOneOnOneMatch(MyPairings, NumberOfTimesToPlayEachOpponent, ReportBackToWhenFinished, InstigatorsGameLogic, ETSTables)
  end.


spawnProcessForEachOneOnOneMatch(OpponentsList, NumberOfTimesToPlayEachOpponent, CallBackToWhenAllSequentialGamesFinished, InstigatorsGameLogic, ETSTables) ->

  NotifyWhenOneOnOneFinished = spawn(?MODULE, watchUntilNIterationsOfOneOnOneMatchFinish, [CallBackToWhenAllSequentialGamesFinished, length(OpponentsList)]),

  lists:foreach(
    fun(Opponent) ->
      OneOnOneArena = spawn(?MODULE, gameLobby, [{self(), InstigatorsGameLogic}, Opponent, NumberOfTimesToPlayEachOpponent, NotifyWhenOneOnOneFinished, ETSTables]),
      ?LOGPROCESS({?LINE,?MODULE}, ["Prisoner", self(), ": Spawned process ", OneOnOneArena, " to play against ", Opponent, ", waiting for them to join ... "])
    end,
    OpponentsList).


watchUntilNIterationsOfOneOnOneMatchFinish(Notify, 0) ->
  ?LOGSUPERVISOR({?LINE, ?MODULE},
    ["All pairs of opponents under my watch have finished their iterations.  Letting ", Notify, " know."]
  ),
  Notify ! {self(), prisoner_finished_games};
watchUntilNIterationsOfOneOnOneMatchFinish(Notify, AfterNumGames) ->
  ?LOGPROCESS({?LINE, ?MODULE},     [self(), " spawned to watch ", AfterNumGames, " sequential pairings."]),
  ?LOGSUPERVISOR({?LINE, ?MODULE},  [self(), " now waiting for ", AfterNumGames, " sequential pairings to finished before notifying", Notify]),
  receive
    {_, finished_one_sequential_game} ->
      ?LOGSUPERVISOR({?LINE, ?MODULE}, [self(), " observed one set of games finishing.  Now waiting on ", AfterNumGames - 1, "."]),
      watchUntilNIterationsOfOneOnOneMatchFinish(Notify, AfterNumGames - 1)
  end.


gameLobby(Instigator, Opponent, NumberOfTimesToPlay, CallbackToWhenFinished, ETSTables) ->
  {PlayerPID,_} = Instigator,
  Opponent ! {self(), join_game},
  ?LOGGAME({?LINE, ?MODULE}, [PlayerPID, " has invited ", Opponent, " to play in lobby ", self(), ".  Waiting for them to join ... "]),
  receive
    {Opponent, providing_game_logic, OpponentsLogic} ->
      ?LOGGAME({?LINE, ?MODULE}, [Opponent, " has joined ", PlayerPID, " in lobby ", self(), " to play!"]),
      sequentialGame(Instigator, {Opponent, OpponentsLogic}, NumberOfTimesToPlay, CallbackToWhenFinished, ETSTables)
  end.


determineYearsToAccrue(RoundResults) ->
  case RoundResults of
    {defected,    defected}       -> {2, 2};
    {defected,    cooperated}     -> {0, 3};
    {cooperated,  defected}       -> {3, 0};
    {cooperated,  cooperated}     -> {1, 1}
  end.


sequentialGame({I, _}, _, 0, TellGameHasFinished, _) ->
  ?LOGGAME({?LINE, ?MODULE}, ["Game taking place in lobby ", self(), " has ended!  Letting ", TellGameHasFinished, " know."]),
  TellGameHasFinished ! {I, finished_one_sequential_game};
sequentialGame(PlayerAndLogic, OpponentAndLogic, NumberOfTimesToPlay, NotifyWhenFinished, ETSTables) ->
  {PrisonersTable, GameOutcomesTable, TableOfPrisonerSentences, LookupMemoryInETS} = ETSTables,
  {Player,    {PDecideNextMove, PReactToOpponentsChoice}} = PlayerAndLogic,
  {Opponent,  {ODecideNextMove, OReactToOpponentsChoice}} = OpponentAndLogic,
  PMemoryKey  = {Player, Opponent},
  OMemoryKey  = {Opponent, Player},
  PChoice     = PDecideNextMove(PMemoryKey,     LookupMemoryInETS),
  OChoice     = ODecideNextMove(OMemoryKey,     LookupMemoryInETS),
  PReactToOpponentsChoice(PMemoryKey, OChoice,  LookupMemoryInETS),
  OReactToOpponentsChoice(OMemoryKey, PChoice,  LookupMemoryInETS),
  {PYears, OYears} = determineYearsToAccrue({PChoice, OChoice}),
  IndexOfSentenceField = 2,

  %% Note, if attempting again would have buffered the ETS interactions until a set time,
  %% as constant updates are causing a bottle neck.

  ets:update_counter(TableOfPrisonerSentences, Player,    {IndexOfSentenceField, PYears}),
  ets:update_counter(TableOfPrisonerSentences, Opponent,  {IndexOfSentenceField, OYears}),
  ets:insert(GameOutcomesTable,
    {
      {NumberOfTimesToPlay, rounds_remaining},
      {{initiator, Player}, {opponent, Opponent}},
      {initiators_choice, PChoice},
      {opponents_choice,  OChoice}
    }
  ),

  ?LOGGAMESUMMARY({?LINE, ?MODULE},
    [
      "End of round ", NumberOfTimesToPlay, " in lobby ", self(), ".",
      "Game summary is as follows:  ",
      ets:lookup(PrisonersTable, Player),  " played against ", ets:lookup(PrisonersTable, Opponent),
      ".  The outcome was that ", ets:lookup(PrisonersTable, Player), " got ", PYears, " Years, and ",
      ets:lookup(PrisonersTable, Opponent), " got ", OYears, ".  The summary inserted in to ETS is: ",
      {
        {rounds_remaining, NumberOfTimesToPlay},
        {{initiator, Player}, {opponent, Opponent}},
        {initiators_choice, PChoice},
        {opponents_choice,  OChoice}
      }
    ]
  ),

  sequentialGame(PlayerAndLogic, OpponentAndLogic, NumberOfTimesToPlay - 1, NotifyWhenFinished, ETSTables).


titForTat(Name) ->

  HowToReactToOpponentsChoice = fun(MemoryKey, RememberItAndForgetOthers, MemoryTable) ->
    ets:insert(MemoryTable, {MemoryKey, RememberItAndForgetOthers})
  end,

  HowToDecideNextMove = fun(MemoryKey, MemoryTable) ->
    Memory = ets:lookup(MemoryTable, MemoryKey),
    case Memory of
      [{_, WhatTheyDidLast}]  -> WhatTheyDidLast;
      []                      -> cooperated
    end
  end,

  LogicNeededToPlay = {HowToDecideNextMove, HowToReactToOpponentsChoice},

  receive
    Message -> gameMessageHandling(Message, LogicNeededToPlay)
  end,

  titForTat(Name).


suspiciousTitForTat(Name) ->

  HowToReactToOpponentsChoice = fun(MemoryKey, RememberItAndForgetOthers, MemoryTable) ->
    ets:insert(MemoryTable, {MemoryKey, RememberItAndForgetOthers})
  end,

  HowToDecideNextMove = fun(MemoryKey, MemoryTable) ->
    Memory = ets:lookup(MemoryTable, MemoryKey),
    case Memory of
      [{_, WhatTheyDidLast}]  -> WhatTheyDidLast;
      []                      -> defected
    end
  end,

  LogicNeededToPlay = {HowToDecideNextMove, HowToReactToOpponentsChoice},

  receive
    Message -> gameMessageHandling(Message, LogicNeededToPlay)
  end,

  suspiciousTitForTat(Name).


random(Name) ->

  HowToReactToOpponentsChoice = fun(_,_,_) -> doesnt_matter_as_random end,

  HowToDecideNextMove = fun(_,_) ->
    random:seed(now()),
    case random:uniform() of
      D when D < 0.5 -> defected;
      _              -> cooperated
    end
  end,

  LogicNeededToPlay = {HowToDecideNextMove, HowToReactToOpponentsChoice},

  receive
    Message -> gameMessageHandling(Message, LogicNeededToPlay)
  end,

  random(Name).


grudger(Name) ->

  HowToReactToOpponentsChoice = fun(MemoryKey, FlipToDefectIfBetrayed, MemoryTable) ->
    case FlipToDefectIfBetrayed of
      defected  -> ets:insert(MemoryTable, {MemoryKey, defected});
      _         -> still_good
    end
  end,

  HowToDecideNextMove = fun(MemoryKey, MemoryTable) ->
    case ets:lookup(MemoryTable, MemoryKey) of
      []  -> cooperated;
      _   -> defected
    end
  end,

  LogicNeededToPlay = {HowToDecideNextMove, HowToReactToOpponentsChoice},

  receive
    Message -> gameMessageHandling(Message, LogicNeededToPlay)
  end,

  grudger(Name).


%% Testing Code %%

whenTitForTatPlays_thenItShouldMirrorTheOpponentsLastMove_test() ->
  MockedInstigatorActions =         [cooperated, cooperated, defected,    defected,   cooperated,   cooperated],
  ActorUnderTestExpectedResponses = [cooperated, cooperated, cooperated,  defected,   defected,     cooperated],
  ActorUnderTestExpectedResponses = runTest(titForTat, MockedInstigatorActions).


whenSuspiciousTitForTatPlays_thenItShouldMirrorTheOpponentsLastMove_AndInitiallyDefect_test() ->
  MockedInstigatorActions =         [cooperated,  cooperated, defected,    defected,   cooperated,   cooperated],
  ActorUnderTestExpectedResponses = [defected,    cooperated, cooperated,  defected,   defected,     cooperated],
  ActorUnderTestExpectedResponses = runTest(suspiciousTitForTat, MockedInstigatorActions).


whenGrudgerPlays_thenItShouldCooperateUntilDefectedAgainst_andThenDefectFromThatPointOn_test() ->
  MockedInstigatorActions =         [cooperated, defected, defected,    defected,   cooperated,   cooperated],
  ActorUnderTestExpectedResponses = [cooperated, cooperated, defected,  defected,   defected,     defected],
  ActorUnderTestExpectedResponses = runTest(grudger, MockedInstigatorActions).


whenRandomPlays_thenItShouldAlternateChoiceRoughlyFiftyPercentOfTheTime_test() ->
  MockedInstigatorActions =         [cooperated || _ <- lists:seq(1, 1000)],
  {UnderTest, {Mock, MockGameLogic}, ETSTables} = testSetup(random, MockedInstigatorActions),
  sequentialGame({Mock, MockGameLogic}, UnderTest, length(MockedInstigatorActions), self(), ETSTables),
  Results = ets:foldl(fun({_,_,_,{_,Actual}}, ResultAcc) -> [Actual | ResultAcc] end, [], game_outcomes),
  testCleanup(),
  Variance = (lists:foldl(fun(Outcome, CountOfDefectAcc) ->
    case Outcome of
      cooperated -> CountOfDefectAcc + 1;
      _ -> CountOfDefectAcc
    end
  end, 0, Results) / 1000) * 100,
  ?assert(Variance >= 40 andalso Variance =< 60).


testSetup(UnderTestStrategy, MockedChoices) ->

  ets:new(actuals, [named_table, duplicate_bag]),
  ets:new(mocked_interactions, [named_table, ordered_set]),

  ETSTables = {
    ets:new(prisoners,        [set, named_table, public]),
    ets:new(game_outcomes,    [ordered_set, named_table, public]),
    ets:new(prison_sentences, [set, named_table, public]),
    ets:new(prisoners_memory, [set, named_table, public])
  },

  lists:foldl(
    fun(Choice, Counter) ->
      ets:insert(mocked_interactions, {Counter, Choice}),
      Counter + 1
    end,
    0,
    MockedChoices
  ),

  UnderTest = start(under_test, UnderTestStrategy),
  UnderTest ! {self(), join_game},
  receive {UnderTest, providing_game_logic, UnderTestGameLogic} -> ok end,

  Mock = start(mock, mockedTestingProcess),
  Mock ! {self(), join_game},
  receive {Mock, get_mocked_logic, MockedGameLogic} -> ok end,

  ets:insert(prison_sentences, {UnderTest, 0}),
  ets:insert(prison_sentences, {Mock, 0}),

  {{UnderTest, UnderTestGameLogic}, {Mock, MockedGameLogic}, ETSTables}.


testCleanup() ->
  ets:delete(actuals),
  ets:delete(prisoners),
  ets:delete(game_outcomes),
  ets:delete(prison_sentences),
  ets:delete(prisoners_memory),
  ets:delete(mocked_interactions).


mockedTestingProcess(NotUsed) ->

  HowToReactToOpponentsChoice = fun(_, Actual ,_) ->
    ets:insert(actuals, {Actual})
  end,

  HowToDecideNextMove = fun(_,_) ->
    [{_,NextMockedChoice}|_] = ets:lookup(mocked_interactions, ets:first(mocked_interactions)),
    ets:delete(mocked_interactions, ets:first(mocked_interactions)),
    NextMockedChoice
  end,

  MockedLogic = {HowToDecideNextMove, HowToReactToOpponentsChoice},

  receive
    {TestSetup, join_game} -> TestSetup ! {self(), get_mocked_logic, MockedLogic}
  end,

  mockedTestingProcess(none).


runTest(StrategyUnderTest, MockedInteractions) ->
  {UnderTest, {Mock, MockGameLogic}, ETSTables} = testSetup(StrategyUnderTest, MockedInteractions),
  sequentialGame({Mock, MockGameLogic}, UnderTest, length(MockedInteractions), self(), ETSTables),
  Results = ets:foldl(fun({_,_,_,{_,Actual}}, ResultAcc) -> [Actual | ResultAcc] end, [], game_outcomes),
  testCleanup(),
  Results.