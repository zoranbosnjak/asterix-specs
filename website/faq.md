---
title: Faq
---

# FAQ - Frequently Asked Questions

## Why custom format? Why not `json` or `xml` or ...?

In short: a custom `ast` format is used only to *edit* and *store* files.
`json` format is also available and is a prefered format for processing
asterix descriptions.

Users (author included) normally prefer standard formats like `json` or `xml`.
However, for the purpose of editing and storing asterix specifications,
a custom format is more convenient.

It is a form of a [domain specific language](https://en.wikipedia.org/wiki/Domain-specific_language)
with a lot of benefits. Comparing to `json`, the custom format is:

- 5-6 times shorter in terms of characters or lines required;
- easier to read and write;
- better fits to capture the essence of asterix definition.

Compare for example the *same definitions* snippet

... in `.ast`

```
SAC "System Area Code"
    element 8
        raw
```

... and in `.json` format.

```json
{
    "definition": null,
    "description": null,
    "name": "SAC",
    "remark": null,
    "spare": false,
    "title": "System Area Code",
    "variation": {
        "content": {
            "type": "Raw"
        },
        "size": 8,
        "type": "Element"
    }
}
```
Admittedly, the `null` values could be excluded, but the `json` definitions
still contain a lot of overhead.

## I do not like a converter in a processing chain.

This is reasonable, but in order to keep the definitions
clean and correct, some form of a tool (editor, converter)
can not be avoided.

Specifications need to be *validated*, which requires
*reading* and *parsing* functions.

Also, when the definitions are written by hand (using a general
purpose editor), there should be a mechanism to *write* a *normalized*
version of a file (align indents, sort key/value pairs...).

In other words, *reading* and *writing* of the specs files **is required**,
regardless of the format.

It is important to note that [tools](/tools.html) handle
the conversion in a very safe way.

During automatic conversion process, it is verified that all supported formats
generate the same file signature, which is a very strong assurance that all formats
contain the same definitions.

## Why LSB is missing in definitions?

It is not missing. It is defined using *scaling factor* and number of *fractional bits*.
The LSB value is calculated using the following formula:

```math
LSB = scaling_factor / 2^fractional_bits
```

It is implemented in this way to avoid rounding error in specifications.

For example, the extreme case is item `I062/105/LAT`.
It is defined by using:

- scaling factor: `180`
- fractional bits: `25`

Both factors are precise, and the resulting LSB is calculated as `180/2^25`.
If specified as decimal number, like `0.00000536442`,
some unexpected rounding error might occur.

## Some unsorted remarks about asterix

It is not strictly related to this project, but there are some
facts about asterix which are worth mentioning. This might save
you some time when implementing asterix encoder/decoder.

* Asterix definitions are *recursive* in nature. To avoid unpleasant
  surprises, do not asume fixed nesting levels on subitems when
  implementing the asterix processor. For example: compound item might contain
  a repetitive item, which might in turn contain any other group of
  items... There is no fixed rule about which item type may or may not contain
  other item types.

* Some (old) categories define multiple *UAPs*. The (incomplete)
  list include:
    - cat001 (I001/020/TYP determines UAP)
    - cat007 (I007/410 determines UAP)
    - cat150
    - cat252
    - cat253

* In case of a category with multiple UAPs, it is not always clear from
  the received data, which UAP to use. In those cases, the UAP needs
  to be agreed between the source and the destination (example: cat253).

* Each *repetitive* item has the first part "the repetition indicator - REP",
  which is normally 1 octet long, but the REP indicator can also use more
  octets. This project is always explicit about REP indicator size.
  See `variation/rep` field in *json*.

* Newer editions of the same category are not always backward compatible.
  They mostly are, but in general it is **NOT safe** to always use the
  latest edition to cope with the problem. It is more safe to be
  always explicit about which edition is in use on a particular data flow.

* There is no 1-to-1 corespondence between *category* edition and
  *expansion* edition. For example: cat021, edition 2.5 can be combined with
  expansion edition 1.4 or any other edition. For a safe operation,
  both category **AND** expansion edition must be agreed between the source
  and the destination.

* Compound item might contain some "always zero" bits in the first part,
  or the so called "empty slots". Example: I011/380 (the third slot).

* The mechanism in expansion field (example cat021, expansion edition 1.4)
  is similar to 'compound' with the difference that "fspec" size is
  a priori known (no FX bits in fspec).
  In this project, this difference is encoded as `Compound`, when
  FX mechanism is in use or `Compound 8`, when fspec is for example
  8-bits long. In *json* it is encoded in `variation/fspec` field.

* Some items definitions are not "context free", for example
  item `I062/380/IAS`. In this case, the interpretation of some bits depends
  on the value of some other bit(s). In this project, such case is
  handled with a special `case` syntax.
  In *json*, there is `variation/rule/type = "Dependent"` indication.

* I004/120, subitem 2 has an even more complicated content definition,
  with nested depentent rules.

* Some encoding rules are complex. This project does not include any
  encoding rules, such as: "This item is optional/mandatory...".

