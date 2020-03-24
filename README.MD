
## `ada-wfc`

An implementation of the popular [Wave Function Collapse](https://github.com/mxgmn/WaveFunctionCollapse) algorithm in the Ada programming language.

Heavily inspired by and based off of the excellent article
[here](https://gridbugs.org/wave-function-collapse/), describing a similarly-optmized [rust implementation](github.com/stevebob/wfc).

The basic interface and code may still improve, but they do implement the algorithm and can be visualized with the simple provided demo program, which can take input as a text file or image.

### requirements

`ada-wfc` itself can be built as an Ada library with `gprbuild`, with no external dependencies.

The demo program requires that `libIL`/`libILU` be available
for the image processing features, which can be provided by `libdevil` (or some similar name) library on most linux systems; consult your package manager. It is also built with `gprbuild`.

Both the demo program and the library can be passed flags to build them in debug or release mode (e.g. `gprbuild -Xmode={debug,release}`).

### demo cli flags

The demo program can be given flags to adjust the WFC tile size, input source file, output file name template, and whether to include rotated/reflected tiles in the computed tileset.

For example, given the input file `sample1.txt` included here:
```
.......#
#.......
.###....
....##..
......##
###.....
...##...
.....##.
```

We can get sample output such as:

```
# tile size NxM, output to stdout (-s), 50x10
> ./main -n3 -m3 -s 50x10

#..........#.........###......#..........#......##
.##.........###.........#......##.........##......
...####........##........##......####.......##....
.......##........##........##........#........#...
.........##........##........###......##.......##.
#..........##........#..........#.......#........#
.##..........##.......##.........##......##.......
...###.........##.......##.........##......##.....
......###........##.......##.........##......#....
.........##........##.......##.........##.....##..
```

```
# include rotations/reflections too, default output dest
# to generate multiple outputs from the same sample, list multiple sizes.
> ./main -n3 -m3 --rot --ref 50x10 70x10 90x10

(50x10_0_sample1.txt) =>
....############...............................###
.....................####......#####....#######...
.##..............####....######.....####..........
#..##############...............................##
......................##........##..........####..
...................###..###..###..##########......
#####..##..##..####........##....................#
.....##..##..##...............................###.
....................######....################....
####............####......####....................

(70x10_1_sample1.txt) =>
.....#######.........###..##..........#.....####...........#..........
....#...................##............#..............###..#...##..###.
####.................................#..............#...##...#..##...#
...................................##...#####.....##........#.........
..........................##......#....#.....#####.........#..........
......##.....####.....####..#....#....#....................#..........
...###..#..##....#####.......#...#....#...............###...#.........
###......##..................#....#....#...........###...#...#........
..............##.............#.....##...##.......##.......#...########
............##..#####.......#........#....##....#..........#..........

(90x10_2_sample1.txt) =>
.......#...#.......#...#........#..#.....#......#.....#...#....#.....#..#...#...#.....#...
.......#...#......#...#...##....#...#...#...##...#....#....#....#....#..#....#...#..##....
......#.....#....#...#...#..#..#....#..#...#..#...#..#.....#....#...#...#....#....##....##
#.....#......#..#...#...#....##....#...#..#....#...##......#....#...#....#....#........#..
.###...#.....#..#...#...#........##...#...#.....#......##...#....#...#....##...##.....#...
....#...##..#....#...#...#......#....#.....#.....#....#..#...#....#..#......#....#...#....
.....#....##......#...#...#....#....#......#......#..#....#...#...#...#......#....#..#....
.....#............#...#...#....#...#.......#......#..#....#...#....#...#.....#....#...#...
......#......##..#...#...#.....#...#......#......#....#..#....#.....#...#....#...#.....#..
......#.....#..##...#...#.......#..#.....#......#.....#..#....#.....#...#...#...#......#..
```

The outputs are all self-tiling.
The option for non-tiling outputs and more constrained adjacency inference
are prioritized features. In the meantime, some amount of extra control can be gained
when the library's generic interface is used, which supports certain callbacks
to be injected and information about the collapse queried during the algorithm.