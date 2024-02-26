set title "case-def"
set spec {
    stack
        { line
            /case
            { (item-name,...) }
            { loop { line (indent) { or (INT,...) /default } : some-def (unindent)} {} }
        }
}
