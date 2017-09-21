erlci
=====

A CI server written in Erlang.

**NOTE**: I'm NSWIDY (Not Sure What I'm Doing, Yet). But please, feel free to
Not-Sure-What-I'm-Doing-Yet with me!

# Configuration
Edit [config/sys.config](https://github.com/marcelog/erlci/blob/master/config/sys.config) and
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

# Running it

```bash
make run
```

# Start a build
```
erlci_build_monitor:start_build("my_job").
```

The build will generate a `log.txt` file inside the workspace of the build (e.g:
/tmp/erlci/workspace/my_job/BUILD_NUMBER/log.txt).

# Plugins
You can write plugins to extend the build process with new features, for a few
examples see: [src/plugins](https://github.com/marcelog/erlci/tree/master/apps/erlci/src/plugins).

## License
The source code is released under Apache 2 License.

Check [LICENSE](https://github.com/marcelog/erlci/blob/master/LICENSE) file for more information.
