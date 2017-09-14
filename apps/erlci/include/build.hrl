-ifndef(erlci_build_hrl).
-define(erlci_build_hrl, true).

-type erlci_build_result() :: success | failed | in_progress.
-type erlci_build() :: map().
-type erlci_build_number() :: pos_integer().

-endif.
