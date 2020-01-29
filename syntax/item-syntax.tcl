set title "item-def"
set spec {
    stack
        {line NAME {''} TITLE {''}}
        {or
            /unspecified
            /mandatory
            /optional
            { line
                /case
                { loop /name "/" }
                { loop { line (indent) INT : { or /mandatory /optional /absent} (unindent)} {} }
            }
        }
        {line /definition TEXT }
        {line subitem-def}
        {opt /remark TEXT }
}

