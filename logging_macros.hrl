%%%-------------------------------------------------------------------
%%% @author Liam Maloney
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%     The following contains macros to make logging cleaner.
%%% @end
%%% Created : 12. Jan 2017 23:44
%%%-------------------------------------------------------------------

-author("Liam Maloney").
-student_number("C00179434").

-ifdef(debug).
-define(LOG(Location, Log),             logging_utils:logFormat("LOG: ",              Location, Log)).
-define(LOGETS(Location, Log),          logging_utils:logFormat("ETS: ",              Location, Log)).
-define(LOGGAME(Location, Log),         logging_utils:logFormat("GAME: ",             Location, Log)).
-define(LOGPROCESS(Location, Log),      logging_utils:logFormat("PROCESSES: ",        Location, Log)).
-define(LOGSUPERVISOR(Location, Log),   logging_utils:logFormat("GAME-SUPERVISOR: ",  Location, Log)).
-define(LOGGAMESUMMARY(Location, Log),  logging_utils:logFormat("GAME-SUPERVISOR: ",  Location, Log)).
-else.
-define(LOG(Location, Log),             no_log).
-define(LOGETS(Location, Log),          no_log).
-define(LOGGAME(Location, Log),         no_log).
-define(LOGPROCESS(Location, Log),      no_log).
-define(LOGSUPERVISOR(Location, Log),   no_log).
-define(LOGGAMESUMMARY(Location, Log),  no_log).
-endif.
