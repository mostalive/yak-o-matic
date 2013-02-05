This is a work in progress, not yet fit for use or participation (should
change in a week or two)

yak-cfd --start GUID --end GUID 
iterates over a number of git commits, optionally limited by a commit
range, collecting the number of vertices in a subgraph

output in json for easier display in javascript libs ?

like this:
[{ CID : 1352434654b { "technical debt" : 50, "in progress" : 2,
"global" : 30}] (and so on for each commit)
