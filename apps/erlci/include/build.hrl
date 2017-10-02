-ifndef(erlci_build_hrl).
-define(erlci_build_hrl, true).

-type erlci_build_status() :: created | in_progress | success | failed.
-type erlci_build() :: map().
-type erlci_build_number() :: pos_integer().

-type erlci_build_description() :: map().

-endif.
