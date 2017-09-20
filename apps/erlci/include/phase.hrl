-ifndef(erlci_phase_hrl).
-define(erlci_phase_hrl, true).

-define(PHASES, [
  prepare_config,
  config,
  prepare_fetch_source,
  fetch_source,
  prepare_source,
  prepare_dependencies,
  fetch_dependencies,
  build_dependencies,
  prepare_build,
  build,
  static_analysis,
  prepare_tests,
  run_tests,
  post_build,
  prepare_doc_generation,
  generate_doc,
  create_artifacts,
  publish_artifacts,
  finish
]).

-type erlci_phase_name() :: atom().

-endif.
