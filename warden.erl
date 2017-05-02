%%%-------------------------------------------------------------------
%%% @author Joseph Kehoe <joseph@joseph-XPS-8500>, Liam Maloney <liam@liammaloney.ie>
%%% @copyright (C) 2016, Joseph Kehoe
%%% @doc
%%%
%%% @end
%%% Created :  7 Nov 2016 by Joseph Kehoe <joseph@joseph-XPS-8500>
%%%-------------------------------------------------------------------

-module(warden).
-export([start/1,add/2,stats/1,run/2,supervisor/1]).

-include_lib("eunit/include/eunit.hrl").

start(State)->
  PID=spawn(?MODULE, supervisor, [State]),
  PID.

add(SupervisorPID,PID)->
  SupervisorPID!{self(),add,PID},
  receive
    {SupervisorPID,done,Total} ->
      Total
  end.

stats(SupervisorPID)->
  SupervisorPID!{self(),stats},
  receive
    {SupervisorPID,State} ->
      State
  end.

run(SupervisorPID,Iterations)->
  SupervisorPID!{self(),run,Iterations},
  receive
    {SupervisorPID,done} -> ok
  end.

supervisor({PrisonerList,Summary,History})->
  receive

    {Sender,add,PID} ->
      Sender!{self(),done,length(PrisonerList)+1},
      supervisor({[PID|PrisonerList],Summary,History});

    {Sender,stats} ->
      Sender!{self(),Summary},
      supervisor({PrisonerList,Summary,History});

    {Sender,run,Count} ->
      {NewSummary,NewHistory}=iterate(PrisonerList,Summary,History,Count),
      Sender!{self(),done},
      supervisor({PrisonerList,NewSummary,NewHistory})

  end.


iterate(_,Summary,History,0) ->
  {Summary,History};
iterate(Prisoners,Summary,History,N) ->
  {NewSummary,NewHistory}=doOneRun(Prisoners,Summary,History),
  iterate(Prisoners,NewSummary,NewHistory,N-1).


doOneRun([],Summary,History)->
  {Summary,History};
doOneRun([First|Rest],Summary,History) ->
  {NewSummary,NewHistory}=doOnce(First,Rest,Summary,History),
  doOneRun(Rest,NewSummary,NewHistory).

doOnce(_,[],Summary,History)->
  {Summary,History};
doOnce(Agent,[OtherAgent|Rest],Summary,History) ->
  Agent ! {self(),name},
  receive
    {Agent,name,MyName}->
      ok
  end,
  OtherAgent!{self(),name},
  receive
    {OtherAgent,name,OtherName}->
      ok
  end,
  OtherAgent!{self(),choose,MyName},
  receive
    {OtherAgent,choice,OtherChoice}->
      ok
  end,
  Agent!{self(),choose,OtherName},
  receive
    {Agent,choice,MyChoice}->
      ok
  end,
  OtherAgent!{self(), result, MyChoice},
  Agent!{self(), result, OtherChoice},
  CurrentGameOutcome = {MyName,MyChoice,OtherName,OtherChoice},
  doOnce(Agent,Rest,updateSummary(CurrentGameOutcome, Summary),[CurrentGameOutcome|History]).


updateSummary(CurrentGameOutcome, ExistingSummary) ->

  case CurrentGameOutcome of

    {I, defected, They, defected} ->

      MyTime = maps:get(I, ExistingSummary, 0),
      TheirTime = maps:get(They, ExistingSummary, 0),
      SummeryWithMyTimeIncreased = maps:put(I, MyTime + 2, ExistingSummary),
      maps:put(They, TheirTime + 2, SummeryWithMyTimeIncreased);

    {I, defected, They, cooperated} ->

      MyTime = maps:get(I, ExistingSummary, 0),
      SummaryWithMyTimeTheSame = maps:put(I, MyTime, ExistingSummary),
      TheirTime = maps:get(They, SummaryWithMyTimeTheSame, 0),
      maps:put(They, TheirTime + 3, SummaryWithMyTimeTheSame);

    {I, cooperated, They, defected} ->

      MyTime = maps:get(I, ExistingSummary, 0),
      SummaryWithMyTimeIncreased = maps:put(I, MyTime + 3, ExistingSummary),
      TheirTime = maps:get(They, SummaryWithMyTimeIncreased, 0),
      maps:put(They, TheirTime, SummaryWithMyTimeIncreased);

    {I, cooperated, They, cooperated} ->

      MyTime = maps:get(I, ExistingSummary, 0),
      TheirTime = maps:get(They, ExistingSummary, 0),
      SummeryWithMyTimeIncreased = maps:put(I, MyTime + 1, ExistingSummary),
      maps:put(They, TheirTime + 1, SummeryWithMyTimeIncreased)

  end.


 %% TESTING CODE %%


expectedBuilder(Player1IncreasedBy, Player2IncreasedBy) ->
  ExpectedBuilder = maps:put(player1, Player1IncreasedBy, maps:new()),
  maps:put(player2, Player2IncreasedBy, ExpectedBuilder).

whenTheyDefect_andIDefect_thenTheStateShouldBeUpdatedWithBothOurTimesIncreasedByTwo_test() ->

  Expected = expectedBuilder(2, 2),
  Actual = updateSummary({player1 , defected, player2, defected}, maps:new()),
  Expected = Actual.

whenTheyCooperate_andIDefect_thenTheStateShouldBeUpdatedWithTheirTimeIncreasedByThree_test() ->

  Expected = expectedBuilder(0, 3),
  Actual = updateSummary({player1 , defected, player2, cooperated}, maps:new()),
  Expected = Actual.

whenTheyDefect_andICooperate_thenTheStateShouldBeUpdatedWithMyTimeIncreasedByThree_test() ->

  Expected = expectedBuilder(3, 0),
  Actual = updateSummary({player1 , cooperated, player2, defected}, maps:new()),
  Expected = Actual.

whenWeBothCooperate_thenTheStateShouldBeUpdatedWithBothOurTimesIncreasedByOne_test() ->

  Expected = expectedBuilder(1, 1),
  Actual = updateSummary({player1 , cooperated, player2, cooperated}, maps:new()),
  Expected = Actual.