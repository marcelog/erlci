-ifndef(erlci_file_hrl).
-define(erlci_file_hrl, true).

-type erlci_filename() :: string().
-type erlci_directory() :: string().
-type erlci_path() :: erlci_filename() | erlci_directory().

-endif.
