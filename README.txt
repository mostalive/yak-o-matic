This is a work in progress, not yet fit for use or participation (should
change in a week or two)

## Usage

```
Usage: yak-o-matic [OPTIONS]
A utility for shaving a yak efficiently
 - Output a list of JSon structures representing the evolution
   of node count in clusters from a graphviz file
 - Alternatively, can use an org-mode file as input, using TODO
   keywords as clusters

  -g DIR   --git-repo=DIR   git repository containing yak file
  -f FILE  --yak-file=FILE  path of yak file relative to git repository
  -v       --verbose        verbose parsing of yak file
```

## Planned

> yak-cfd --start=GUID --end=GUID 

Iterates over a number of git commits, optionally limited by a commit
range, collecting the number of vertices in a subgraph

output in json for easier display in javascript libs ?

like this:
[{ CID : 1352434654b { "technical debt" : 50, "in progress" : 2,
"global" : 30}] (and so on for each commit)
