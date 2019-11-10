Image Manager
=============

> Simple console app for managing images.

## Release
Compiled files will be in /dist/ directory.

```sh
fake build target Release
```

## Commands

### List
Will show a list of available commands.
```sh
dotnet run list
```
```
Available commands:
    help     Displays help for a command
    list     Lists commands
    prepare  Prepares images for sorting
```

### Prepare for sorting
Prepares images from source dir (_recursively_) into the target dir, it may exclude images from a specific dir (_recursively_).

Target directory is automatically added to excluded directories - it means, files which are already in the target directory, will never be overwritten (unless `--force` option is set).

#### Examples
```sh
# without excluding
dotnet run prepare /source/dir/ /target/dir/

# with excluding
dotnet run prepare /source/dir/ /target/dir/ /excluded/dir/
```
