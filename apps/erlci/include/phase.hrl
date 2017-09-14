-ifndef(erlci_phase_hrl).
-define(erlci_phase_hrl, true).

-define(PHASES, [
  prepare_config,
  config,
  prepare_fetch_source,
  fetch_source,
  prepare_source,
  prepare_build,
  build,
  post_build,
  create_artifacts,
  publish_artifacts,
  finish
]).

-type erlci_phase_name() :: atom().

-endif.
