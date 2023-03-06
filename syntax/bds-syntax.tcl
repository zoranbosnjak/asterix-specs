set title "bds-def"
set spec {
    stack
        {line /bds}
        {line /reference DOC-REF}
        {line /date DATE-REF}
        { loop
            { stack
                {(emptyline)}
                {line /addr {''} {TITLE} {''}}
                {line (indent) subitem-def (unindent)}
            }}
}

