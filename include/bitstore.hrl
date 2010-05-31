-define(DEBUG, true).
-define(INFO, true).
-define(ERROR,true).

-define(LOG(Type, Format, Args),
        case Type of
            true -> io:format(Format, Args);
            false -> ok
        end.
