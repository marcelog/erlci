-ifndef(erlci_trigger_hrl).
-define(erlci_trigger_hrl, true).

-type erlci_trigger_name() :: string().
-type erlci_trigger_type() :: string().
-type erlci_trigger_config() :: erlci_config().

-type erlci_trigger() :: map().

-type erlci_trigger_result() :: {ok, pid()}.

-endif.
