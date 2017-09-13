-ifndef(erlci_config_hrl).
-define(erlci_config_hrl, true).

-include_lib("yamerl/include/yamerl_nodes.hrl").

-type erlci_config() :: proplists:proplist().
-type erlci_config_key() :: string().
-type erlci_config_value() :: any().

-endif.
