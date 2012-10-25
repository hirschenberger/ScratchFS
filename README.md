# ScratchFS - Size limited filesystem

ScratchFS is a FUSE-based filesystem which provides a size-limited directory tree. When the filesystem 
is mounted, the user provides a maximum size the target directory and all it's subdirectories may get.
All IO operations in the mountdirectory are tracked and the oldest files in the watchdirectory get 
deleted automatically.

## Usage

    ScratchFs {-s|--size} SIZE WATCHDIR MOUNTDIR
     -s SIZE  --size=SIZE  The maximum size in kilobytes (allowed extensions: MB, GB, TB)
     -h       --help       Show help message


## **CAUTION**

**Don't use ScratchFS with important files or mount important directories, you may loose your data.**