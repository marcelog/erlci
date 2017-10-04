erlci
=====

A CI server written in Erlang.

**NOTE**: I'm NSWIDY (Not Sure What I'm Doing, Yet). But please, feel free to
Not-Sure-What-I'm-Doing-Yet with me!

# Configuration
Edit [config/sys.config](https://github.com/marcelog/erlci/blob/master/config/sys.config.example) and
change `config` so it points to your own config file.

## Main config file
See [examples/config.yml](https://github.com/marcelog/erlci/blob/master/examples/config.yml).

## Job config file
See [examples/jobs/my_job/config.yml](https://github.com/marcelog/erlci/blob/master/examples/jobs/my_job/config.yml).

## Variables
Config files can include variables (actually, they are parsed as [mustache](https://mustache.github.io)
template files). For example, your main config file could be something like:
```yaml
# ... other config options...
variables:
  shell: /bin/bash
  bash: {{shell}}
```

So the variable `bash` will be equal to the value of the variable `shell`. Then,
in your job config file:
```yaml
# ... other config options...
variables:
  my_shell: {{bash}}
phases:
  # ... other phases...
  create_artifacts:
    main:
      type: cmd
      config:
        shell: {{my_shell}}
        executable: /usr/bin/tar
        args:
          - zcf
          - artifact.tar.bz2
          - "*"
```

As result, global variables declared in your main config file are also
available in the configuration file of your jobs.

# Build phases, steps, and plugins
The build goes through different `phases` in order, you can see all of them
[here](https://github.com/marcelog/erlci/blob/master/apps/erlci/include/phase.hrl).

It's **not necessary** to have all of them setup in your job's config file, and
the build will skip the ones missing. You can configure the different build
phases like this:
```yaml
# ... other config options...
phases:
  # ... other phases...
  fetch_source:
    main:
      type: git
      config:
        executable: {{git_location}}
        repository: https://github.com/marcelog/erlci
  fetch_dependencies:
    main:
      type: rebar
      config:
        source_directory: {{src_dir}}
        executable: {{rebar_location}}
  # ... other phases...
```

In this case, two phases are shown (`fetch_source` and `fetch_dependencies`),
each one with just 1 **step** called `main` in both cases.

The name of the step is anything you'd like, it's just a description useful
for you when configuring the build. Steps are run in order of appearance. Each
step will run a plugin, defined with the `type` key. The `config` key will
depend on a per plugin basis. Current available plugins:
 * [rebar](https://github.com/marcelog/erlci/blob/master/apps/erlci/src/plugins/erlci_plugin_rebar.erl): Uses rebar to run different tasks on the source, can be used in many different phases.
 * [git](https://github.com/marcelog/erlci/blob/master/apps/erlci/src/plugins/erlci_plugin_git.erl): Uses [git](https://git-scm.com/) to fetch sources.
 * [cmd](https://github.com/marcelog/erlci/blob/master/apps/erlci/src/plugins/erlci_plugin_cmd.erl): Runs a shell command.

# Triggers
Triggers can be setup in your job config file like this:
```yaml
name: my_job
# ... other config options ...
triggers:
  git_poll:
    expression: "*/1 * * * *"
    executable: {{git_location}}
    repository: {{repo}}
    source_directory: {{src_dir}}
  cron:
    expression: "*/1 23 * * *"
phases:
  # ... your phases and other config options here ...
```

Triggers are useful to automatically start a build of your job in many different
situations. The current available triggers are:

* [cron](https://github.com/marcelog/erlci/blob/master/apps/erlci/src/triggers/erlci_trigger_cron.erl): Takes a
[Vixie cron-like expression](https://en.wikipedia.org/wiki/Cron) in order to trigger a build.
* [git_poll](https://github.com/marcelog/erlci/blob/master/apps/erlci/src/triggers/erlci_trigger_git_poll.erl): Given a
Vixie cron-like expression, polls a GIT scm and starts a new build if there is a new revision.

# Running it

```bash
make run
```

# Start a build
```
BuildDescription = erlci_build:describe_build(
  "user", "marcelog", "manual trigger", "a nice build description"
).
erlci_build_monitor:start_build("my_job", BuildDescription).
```

The build will generate a `log.txt` file inside the workspace of the build (e.g:
/tmp/erlci/workspace/my_job/BUILD_NUMBER/log.txt).

# Plugins
You can write plugins to extend the build process with new features, for a few
examples see: [src/plugins](https://github.com/marcelog/erlci/tree/master/apps/erlci/src/plugins).

## License
The source code is released under Apache 2 License.

Check [LICENSE](https://github.com/marcelog/erlci/blob/master/LICENSE) file for more information.
