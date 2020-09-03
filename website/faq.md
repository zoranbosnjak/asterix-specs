---
title: Faq
---

# FAQ - Frequently Asked Questions

## Why custom format? Why not `json` or `xml` or ...?

In short: a custom `ast` format is only to *edit* and *store* the files.
`json` format is available too and is a prefered format for processing
asterix descriptions.

Users (author included) normally prefer standard formats like `json` or `xml`.
However, for the purpose of editing and storing asterix specifications,
a custom format is much more convenient.

It's a form of a [domain specific language](https://en.wikipedia.org/wiki/Domain-specific_language)
with a lot of benefits. Comparing to `json`, the custom format is:

- 5-6 times shorter in terms of characters or lines required;
- easier to read and write;
- better fit to capture the essence of asterix definition;

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
            "type": "Unspecified"
        },
        "size": 8,
        "type": "Element"
    }
}
```
Admittedly, the `null` values could be excluded, but the definitions
still contain a lot of overhead.

## I don't like a converter in a processing chain.

This is reasonable, but in order to keep the definitions
clean and correct, some form of a tool (editor, converter)
can not be avoided.

The speciffications need to be *validated*, which requires
*reading* and *parsing* function.

Also, when the definitions are written by hand (using a general
purpose editor), there should be a mechanism to *write* a *normalized*
version of a file (align indents, sort key/value pairs...).

In other words, *reading* and *writing* of the specs files **is required**,
regardless of the format.

It is important to note that a [converter](/converter.html) handles
the conversion in very safe way.

During automatic conversion process, it is verified that all supported formats
generate the same file signature, which is very strong assurance that all formats
contains the same definitions.


## Why LSB is missing in definitions?

It's not missing. It's defined using *scaling factor* and number of *fractional bits*.
The LSB value is calculated using this formula:

```math
LSB = scaling_factor / 2^fractional_bits
```

It is implemented this way to avoid rounding error in the specifications.

For example, the extreme case is item `I062/105/LAT`.
It's defined using:

- scaling factor: `180`
- fractional bits: `25`

Both factors are precise, and the resulting LSB is calculated as `180/2^25`.
If specified as decimal number, like `0.00000536442`,
some unexpected rounding error might be generated.

