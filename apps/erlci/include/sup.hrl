-ifndef(erlci_sup_hrl).
-define(erlci_sup_hrl, true).

-define(WORKER(Module), {
  Module,
  {Module, start_link, []},
  permanent,
  brutal_kill,
  worker,
  []
}).

-define(SUP(Module), {
  Module,
  {Module, start_link, []},
  permanent,
  brutal_kill,
  supervisor,
  []
}).
-endif.
