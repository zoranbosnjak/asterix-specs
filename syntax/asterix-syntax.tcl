set title "asterix-def"
set spec {
    stack
        {line /category 3-DIGIT-NUMBER {''} {TITLE} {''}}
        {line /edition INT.INT}
        {line /date YYYY-MM-DD}
        {line /preamble TEXT}
        {line /items {loop {line (newline) (indent) item-def (unindent)} {}}}
        uap-def
}

