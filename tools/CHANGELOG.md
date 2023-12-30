# Revision history

## 1.3.0

* Backward incompatible change:
  Repetitive item is extended with `RepetitiveType` attribute.

* Extended item with one subitem is not allowed.
  In this case the `repetitive fx` variation is likely the correct type.
  See for example: `I002/050`.

* JSON format, change "regular", "no-trailing-fx" to
  "Regular", "No-trailing-fx" respectively.

## 1.7.0

* `tools/lib/Asterix/Indent.hs` module cleanup.

