# Asterix data specs migrations

This directory contains migration scripts to convert specification
files between incompatible versions. Such script is required for
example when some part of the spec is removed or altered by some
known formula.

Each migration is a miniature stand alone project, which is typically
used only once on a particular commit that needs to be migrated.
