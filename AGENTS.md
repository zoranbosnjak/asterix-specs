# AI coding agent guiding rules

## Asterix specification type

In this project, the asterix specification structure is defined *as type* and
the concrete specifications are *values of that type*. The main toplevel type
is `data Asterix`. There are many more smaller subtypes.

A source definition of main and subtypes (what is actually used throughout the
project) are provided in `aspecs/lib/Asterix/Specs/Types.hs` in `haskell`
format.

There is also a simplified version of the original `Types.hs` file, located at
`website/types_simple.hs`. This version shall be in sync with the original, but
with some nonessential details removed.

### Validation and prettify actions

The strict typing already excludes a lot of invalid specifications (prevented
by the haskell type system - invalid values are unrepresentable). But
unfortunately not all problems can be handled this way. The remaining checks
are performed by *validation rules* as specified in
`aspecs/lib/Asterix/Specs/Validation.hs`.

Pretttify action is a process of reading the file and writting it back as a
same format, with a purpose of removing unnecessary white spaces.

After a change on any specification file, do:

```bash
cd specs/
nix-shell   # for 'aspecs' command to become available

# validate a file, it shall return no error/warning output and 0 exit code
aspecs validate --input-ast catNNN/FILE.ast

# prettify a file to a compact form
aspecs prettify catNNN/FILE.ast --ast
```

## Specs files

The specs files are stored in a `specs/` subfolder. Conventions are difined
separately in [CONVENTIONS.md](specs/CONVENTIONS.md) file.

