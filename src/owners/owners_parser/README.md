https://chromium.googlesource.com/chromium/src/+/master/docs/code_reviews.md

```
lines      := (\s* line? \s* comment? \s* "\n")*

line       := directive
           | "per-file" \s+ glob \s* "=" \s* directive

directive  := "set noparent"
           |  "file:" owner_file
           |  email_address
           |  "*"

glob       := [a-zA-Z0-9_-*?]+

comment    := "#" [^"\n"]*

owner_file := "OWNERS"
           |  [^"\n"]* "_OWNERS"
```
