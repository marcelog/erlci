-ifndef(erlci_job_hrl).
-define(erlci_job_hrl, true).

-type erlci_job_name() :: string().
-type erlci_job_description() :: string().
-type erlci_job_config() :: string().
-type erlci_job_home() :: erlci_directory().

-type erlci_job() :: map().
-endif.
