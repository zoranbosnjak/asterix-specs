set title "uap-def"
set spec {
    stack
        { or
            { line /uap { loop { line (indent) { or name {-} } (unindent)} {} } }
            { line
                /uaps
                { loop { line (indent) /uap-name { loop { line (indent) { or name {-} } (unindent)} {} } (unindent)} {} }
            }
        }
}

