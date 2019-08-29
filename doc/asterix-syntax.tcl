set spec {
    stack
        {line /category 3-DIGIT-NUMBER {''} {TITLE} {''}}
        {line /edition INT.INT}
        {line /date YYYY-MM}
        {line /preamble TEXT}
        {line /items {loop {line (newline) (indent) top-item-def (unindent)} {}}}
        uap-def
}

