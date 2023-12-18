# Number data type fix

`Number` data type change from 'real' to precise numeric expression.

```haskell
# old structure
data Number
    = NumberZ Integer
    | NumberQ Rational
    | NumberR Rational

# new structure
data Number
    = NumInt Integer
    | NumDiv Number Number
    | NumPow Integer Integer
```

This directory contains a migration script, to convert a spec file (one by one).

```bash
nix-shell
ghcid "--command=ghci -Wall -i../../tools/lib Main.hs"
runhaskell $EXTENSIONS -Wall -i../../tools/lib Main.hs -h

# convert all specs
for i in $(find ../../specs -type f | grep "\.ast$")
do
    echo $i
    runhaskell $EXTENSIONS -Wall -i../../tools/lib Main.hs $i --in-place
    if [ $? -ne 0 ]; then echo $i; break; fi
done
```
