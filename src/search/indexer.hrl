-define(DEBUG, true).
-define(INFO, false).
-define(ERROR,true).

-define(LOG(Type, Format, Args),
        case Type of
            true -> io:format(Format, Args);
            false -> ok
        end.
