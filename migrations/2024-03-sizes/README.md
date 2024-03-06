# Sizes conversion

Change "bit sizes" to "byte sizes":

```bash
cd specs

# REP value in regular repetitive
find . | grep "\.ast" | xargs sed -i 's/repetitive 8/repetitive 1/g'

# Fspec size in expansion
find . | grep "\.ast" | xargs sed -i 's/compound 8/compound 1/g'
```
