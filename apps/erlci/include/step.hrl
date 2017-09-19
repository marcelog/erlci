-ifndef(erlci_step_hrl).
-define(erlci_step_hrl, true).

-type erlci_step_name() :: string().
-type erlci_step_type() :: string().
-type erlci_step_config() :: erlci_config().
-type erlci_step_state() :: term().

-type erlci_step() :: map().

-type erlci_step_result() :: {ok, pid()} | {
  erlci_build_status(),
  erlci_job(),
  erlci_build()
}.

-endif.
