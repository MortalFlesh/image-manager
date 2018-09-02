Image Manager
=============

Simple console app for managing images.

## Commands

### Prepare for sorting
Prepares images from source dir (_recursively_) into the target dir, it might exclude images from a specific dir (_recursively_).

#### Examples
```sh
# without excluding
dotnet run prepare /source/dir/ /target/dir/

# with excluding
dotnet run prepare /source/dir/ /target/dir/ /excluded/dir/
```
