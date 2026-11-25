# Asterix specification conventions

This directory contains asterix specifications in structured format.
The files shall closely correspond to the original PDF files.

The specifications are organized by so called *categories*, which is a simple
integer identifier from `0` to `255`.
Each category can contain:
- pure category definition (denoted as *cat*)
- reserved expansion definition (denoted as *ref*)

There are also multiple *editions* for each *cat* and *ref*. Edition contain
*major* and *minor* number. The category and edition are encoded in a file path
and also inside each file. The two values must match.

For example: *category 62, expansion, edition 1.3* is stored at
`/specs/cat062/ref-1.3.ast`.

The `.ast` format is one of the encodings of asterix structure type (the
in-memory representation of asterix specification). This is the format used to
store and edit the actual specifications. Another possible format is `.json`
which might be more appropriate to process with different programming
environments, such as *python*. Both formats are *isomorphic* - it is possible
to convert from one format to another and back, without data loss, using
the `aspecs` command line tool.

## Original PDF files from Eurocontrol

Original PDF files are provided by
[Eurocontrol](https://www.eurocontrol.int/asterix). There is a search option on
the web page to help locating the specifications. File names are sometimes
inconsistent and hard to find.

For convenience, the same files are also stored in a [dedicated
repository](https://github.com/zoranbosnjak/asterix-eurocontrol-pdf-files),
where the file names and paths of the PDFs correspond to the specification
files in this project.

## Converting information

The original PDF files are semi-structured. Converting information to a
structured form (to this project) is not always trivial. This chapter provides
some rules which shall be followed whenever possible.

### Respecting the source
- Original PDF files from Eurocontrol are the information source and shall be
  respected, whenever possible.
- If the source can not be respected (for example: duplicated item name within
  the parent item, which is not allowed in this project), resolve the problem
  as necessary and create a note to the `remarks.md` file, located near the
  corresponding `.ast` file.

### Essence of the specifications and the structure
- Overall structure (UAP), top level item..., shall be represented correctly.
- Each item shall follow correct *Variation*, however sometimes it is hard to
  select the correct one. For example "Extended vs Repetitive Fx".
- Each *leaf* item (Element, Spare) shall define a correct bit size.
- This project distinguishes between *Group* and *Element* variations, which
  are both *Fixed* sized structures. Original PDF files do not make this
  destinction explicit and deal with both types simply as *Fixed* item.
  When there is just one subitem (without deeper structure) it shall normally
  be converted to *Element*. If there are more subitems, it is *Group*.
  Problem is described in more detail here:
  <https://github.com/zoranbosnjak/asterix-remarks/blob/main/2026-10-17-fixed-item.md>
- There is a subtle difference between *raw* and *unsigned integer* content.
  An *unsigned integer* is used for items which are a *natural number*, for
  example *number of seconds since midnight*. It's a countable value, possible
  to order, add..., like an integer.
  On the other hand the *raw* content is a content where integer operations do
  not make sense. It is used for "just an identifier" or "something else".
  SAC/SIC values are such examples.
- All specifications shall pass validation rules.

### Item names, titles and string values
- Item name is mandatory. The names are normally specified in the PDFs. If this
  is not the case, try to derive (sub)item name from it's title by capitalizing
  the first letters of each word. For example:
  "Calculated Track Barometric Altitude" -> "CTBA".
- Titles are optional elements and shall be preserved when possible. No
  character capitalizing is necessary. It is a single line string.

### Constraints
This include relations like "<, <= >, >=".
- The specs shall mirror the explicitely provided range from the PDF files. It
  does not need to consider the actual bit sizes (that is: copy the problem
  from the PDF file if there is one regarding explicitely given range). In
  other words: respect the primary source.
- It is not necessary to define implicit ranges (as follows from number of bits
  in use) if the ranges are not already provided in the PDFs. For example, if
  the value has 8 bits, the implicit range is between 0 and 255. Range
  (constraints) is optional.

### Free text
- Free text (including notes) shall be copy-pasted as close as possible to the
  original. Capital letters shall be preserved from the original.
- Non-ascii characters may be replaced by ascii equivalent, for example the
  quoting sign '"'.

