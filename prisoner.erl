%%%-------------------------------------------------------------------
%%% @author Liam Maloney <C00179434@itcarlow.ie>
%%% @copyright (C) 2016, Liam Maloney
%%% @doc
%%%     A simple implementation of a Prisoner
%%% @end
%%% Created : 17. Nov 2016 22:03
%%%-------------------------------------------------------------------

-module(prisoner).
-author("Liam Maloney").

-ifdef(debug).
-define(LOG(X), io:format("{~p,~p}: ~p~n", [?MODULE, ?LINE, X])).
-else.
-define(LOG(X), true).
-endif.

-include_lib("eunit/include/eunit.hrl").

-export([create/4, titForTat/3, random/3, grudger/3, suspiciousTitForTat/3, getName/1, getState/1, getStrategy/1]).

create(Name, Strategy, State, History) ->
  PrisonerPID = spawn(?MODULE, Strategy, [Name, State, History]),
  PrisonerPID.

getName(PID) ->
  PID ! {self(), name},
  receive
    {PID, name, Name} -> Name
  end.

getState(PID) ->
  PID ! {self(), results},
  receive
    {PID, results, State} -> State
  end.

getStrategy(PID) ->
  PID ! {self(), strategy},
  receive
    {PID, strategy, Strategy} -> Strategy
  end.


%% As each prisoner handles the same messages, we can have the message handling logic here and inject the
%% Strategy implementation via HOF's

handleGenericMessage(Message, ForStrategy, Name, State, History, StrategyImplementation) -> case Message of
    {Sender, name} ->
      ?LOG(lists:concat(["Message: {Sender, name} received on ", Name, " (", ForStrategy , ").  Sending ", Name])),
      Sender ! {self(), name, Name};

    {Sender, results} ->
      ?LOG(lists:concat(["Message: {Sender, results} received in ", ForStrategy, ".  Sending History: ", History])),
      Sender ! {self(), results, History};

    {Sender, strategy} ->
      ?LOG(lists:concat(["Message: {Sender, strategy} received in ", ForStrategy,".  Sending ", ForStrategy])),
      Sender ! {self(), strategy, titForTat};

    {Sender, choose, PartnerName} ->
      ?LOG(lists:concat(["Message: {Sender, choose, PartnerName} received in ", ForStrategy, ".  Running interaction between ", Name, " and ", PartnerName])),
      StrategyImplementation(Name, PartnerName, State, Sender)
  end.


titForTat(Name, State, History) ->
  receive

    Message -> handleGenericMessage(Message, titForTat, Name, State, History,
      fun(Name, PartnerName, State, Sender) ->
        MyChoice = maps:get(PartnerName, State, cooperated),
        Sender ! {self(), choice, MyChoice},
        receive
          {Sender, result, TheirChoice} -> titForTat(Name, maps:put(PartnerName, TheirChoice, State), [{PartnerName, MyChoice, TheirChoice}|History])
        end
      end

    )
  end,
  titForTat(Name, State, History).


suspiciousTitForTat(Name, State, History) ->
  receive
    Message -> handleGenericMessage(Message, suspiciousTitForTat, Name, State, History,
      fun(Name, PartnerName, State, Sender) ->
        MyChoice = maps:get(PartnerName, State, defected),
        Sender ! {self(), choice, MyChoice},
        receive
          {Sender, result, TheirChoice} ->
            suspiciousTitForTat(Name, maps:put(PartnerName, TheirChoice, State), [{PartnerName, MyChoice, TheirChoice}|History])
        end
      end
    )
  end,
  suspiciousTitForTat(Name, State, History).


random(Name, NotUsed, History) ->
  receive
    Message -> handleGenericMessage(Message, random, Name, NotUsed, History,
      fun(Name, PartnerName, NotUsed, Sender) ->
        random:seed(now()),

        MyChoice = case random:uniform() of
                     D when D =< 0.5 -> defected;
                     _ -> cooperated
                   end,


        Sender ! {self(), choice, MyChoice},

        receive

          {Sender, result, TheirChoice} -> random(Name, NotUsed, [{PartnerName, MyChoice, TheirChoice}|History])

        end
      end
    )
  end,
  random(Name, NotUsed, History).


grudger(Name, State, History) ->
  receive
    Message -> handleGenericMessage(Message, grudger, Name, State, History,
      fun(Name, PartnerName, State, Sender) ->
        MyChoice = maps:get(PartnerName, State, cooperated),
        Sender ! {self(), choice, MyChoice},
        receive
          {Sender, result, TheirChoice} ->
            case TheirChoice of
              defected  -> grudger(Name, maps:put(PartnerName, defected, State), [{PartnerName, MyChoice, TheirChoice}|History]);
              _         -> grudger(Name, State, [{PartnerName, MyChoice, TheirChoice}|History])
            end
        end
      end
    )
  end,
  grudger(Name, State, History).


%% Testing Code %%

whenGrudgerIsPreviouslyBetrayedByOpponent_thenGrudgerShouldDefectFromThenOn_test() ->

  GrudgerUnderTest = create(grudger_under_test, grudger, maps:new(), []),

  GrudgerUnderTest ! {self(), choose, mocked_defector},

  receive {GrudgerUnderTest, choice, _} -> GrudgerUnderTest ! {self(), result, cooperated} end,

  GrudgerUnderTest ! {self(), choose, mocked_cooperator},
  receive {GrudgerUnderTest, choice, _} -> GrudgerUnderTest ! {self(), result, cooperated} end,

  GrudgerUnderTest ! {self(), choose, mocked_defector},
  receive {GrudgerUnderTest, choice, _} -> GrudgerUnderTest ! {self(), result, defected} end,

  GrudgerUnderTest ! {self(), choose, mocked_cooperator},
  receive {GrudgerUnderTest, choice, _} -> GrudgerUnderTest ! {self(), result, cooperated} end,

  GrudgerUnderTest ! {self(), choose, mocked_defector},
  receive {GrudgerUnderTest, choice, _} -> GrudgerUnderTest ! {self(), result, cooperated} end,

  GrudgerUnderTest ! {self(), choose, mocked_defector},
  receive {GrudgerUnderTest, choice, _} -> GrudgerUnderTest ! {self(), result, cooperated} end,

  GrudgerUnderTest ! {self(), results},
  receive {GrudgerUnderTest, results, State} -> ok end,

  [
    {mocked_defector, defected, cooperated},
    {mocked_defector, defected, cooperated},
    {mocked_cooperator, cooperated, cooperated},
    {mocked_defector, cooperated, defected},
    {mocked_cooperator, cooperated, cooperated},
    {mocked_defector, cooperated, cooperated}
  ] = State.


whenTitForTatPlays_thenItShouldMirrorTheOpponentsLastMove_test() ->

  TitForTatUnderTest = create(tit_for_tat_under_test, titForTat, maps:new(), []),

  TitForTatUnderTest ! {self(), choose, player1},
  receive {TitForTatUnderTest, choice, _} -> TitForTatUnderTest ! {self(), result, cooperated} end,

  TitForTatUnderTest ! {self(), choose, player2},
  receive {TitForTatUnderTest, choice, _} -> TitForTatUnderTest ! {self(), result, cooperated} end,

  TitForTatUnderTest ! {self(), choose, player1},
  receive {TitForTatUnderTest, choice, _} -> TitForTatUnderTest ! {self(), result, defected} end,

  TitForTatUnderTest ! {self(), choose, player2},
  receive {TitForTatUnderTest, choice, _} -> TitForTatUnderTest ! {self(), result, cooperated} end,

  TitForTatUnderTest ! {self(), choose, player1},
  receive {TitForTatUnderTest, choice, _} -> TitForTatUnderTest ! {self(), result, cooperated} end,

  TitForTatUnderTest ! {self(), choose, player2},
  receive {TitForTatUnderTest, choice, _} -> TitForTatUnderTest ! {self(), result, defected} end,

  TitForTatUnderTest ! {self(), choose, player2},
  receive {TitForTatUnderTest, choice, _} -> TitForTatUnderTest ! {self(), result, cooperated} end,

  TitForTatUnderTest ! {self(), results},
  receive {TitForTatUnderTest, results, State} -> ok end,

  [
    {player2, defected, cooperated},
    {player2, cooperated, defected},
    {player1, defected,   cooperated},
    {player2, cooperated, cooperated},
    {player1, cooperated, defected},
    {player2, cooperated, cooperated},
    {player1, cooperated, cooperated}
  ] = State.


whenSuspiciousTitForTatPlays_thenItShouldMirrorTheOpponentsLastMove_andTheFirstMoveShouldBeDefect_test() ->

  SuspiciousTitForTatUnderTest = create(suspicious_tit_for_tat_under_test, suspiciousTitForTat, maps:new(), []),

  SuspiciousTitForTatUnderTest ! {self(), choose, player1},
  receive {SuspiciousTitForTatUnderTest, choice, _} -> SuspiciousTitForTatUnderTest ! {self(), result, cooperated} end,

  SuspiciousTitForTatUnderTest ! {self(), choose, player2},
  receive {SuspiciousTitForTatUnderTest, choice, _} -> SuspiciousTitForTatUnderTest ! {self(), result, cooperated} end,

  SuspiciousTitForTatUnderTest ! {self(), choose, player1},
  receive {SuspiciousTitForTatUnderTest, choice, _} -> SuspiciousTitForTatUnderTest ! {self(), result, defected} end,

  SuspiciousTitForTatUnderTest ! {self(), choose, player2},
  receive {SuspiciousTitForTatUnderTest, choice, _} -> SuspiciousTitForTatUnderTest ! {self(), result, defected} end,

  SuspiciousTitForTatUnderTest ! {self(), choose, player1},
  receive {SuspiciousTitForTatUnderTest, choice, _} -> SuspiciousTitForTatUnderTest ! {self(), result, cooperated} end,

  SuspiciousTitForTatUnderTest ! {self(), choose, player2},
  receive {SuspiciousTitForTatUnderTest, choice, _} -> SuspiciousTitForTatUnderTest ! {self(), result, cooperated} end,

  SuspiciousTitForTatUnderTest ! {self(), results},
  receive {SuspiciousTitForTatUnderTest, results, State} -> ok end,

  [
    {player2, defected, cooperated},
    {player1, defected, cooperated},
    {player2, cooperated, defected},
    {player1, cooperated, defected},
    {player2, defected, cooperated},
    {player1, defected, cooperated}
  ] = State.


whenRandomPlays_thenItShouldRoughlyAlternateDecisionFiftyPercentOfTime_test() ->

  RandomUnderTest = create(random_under_test, random, maps:new(), []),

  Loop = fun Loop(Iteration) -> case Iteration of
                             1000 ->
                               ok;
                             _ ->
                               RandomUnderTest ! {self(), choose, mock_player},
                               receive {RandomUnderTest, choice, _} -> RandomUnderTest ! {self(), result, cooperated} end,
                               Loop(Iteration + 1)
                           end
         end,
  Loop(0),
  RandomUnderTest ! {self(), results},

  receive
    {RandomUnderTest, results, State} ->
      Variance = (lists:foldl(fun(Outcome, CountOfDefect) ->
        case Outcome of
          {_, defected, _} -> CountOfDefect + 1;
          _ -> CountOfDefect
        end end, 0, State) / 1000) * 100
  end,
  ?assert(Variance >= 40 andalso Variance =< 60).
