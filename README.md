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

# Running it

```bash
make run
```

# Plugins
You can write plugins to extend the build process with new features, for a few
examples see: [src/plugins](https://github.com/marcelog/erlci/tree/master/apps/erlci/src/plugins).

## License
The source code is released under Apache 2 License.

Check [LICENSE](https://github.com/marcelog/erlci/blob/master/LICENSE) file for more information.
