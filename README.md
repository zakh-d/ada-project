# ADA project

### Building

create folder for bin files

```
mkdir .objs
```

and build using gprbuild tool

```
gprbuild -P default.gpr -cargs -gnatef
```

or use vs code extention for Ada 

[Using Ada with VS Code](https://github.com/AdaCore/ada_language_server/wiki/Getting-Started)

build task is placed in [.vscode/tasks.json](.vscode/tasks.json)

### Running

Build project and then run

```
.objs/simulation
```
