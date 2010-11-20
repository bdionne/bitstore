-define(DEBUG, false).
-define(INFO, true).
-define(ERROR,true).


-define(LOG(Type, Format, Args),
        case Type of
            true -> io:format(Format, Args);
            false -> ok
        end).

%% how often to poll for db changes
-define(FTI_DBS, "false").
-define(POLL_INTERVAL,"10000").
