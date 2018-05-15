This directory contains scripts related to continuous integration. They can
also be used to setup a development environment manually.

```
./get_and_install_music.sh master
```

will install the most recent version of music in the current directory.


```
./get_and_install_nest.sh master
```

will install the most recent version of nest in the current directory.
At the moment it has a hardcoded dependency on the music installation in
the previous step.

To use it source the file
```
. nest-master/bin/nest_vars.sh
```

