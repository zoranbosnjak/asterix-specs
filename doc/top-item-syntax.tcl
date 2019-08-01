set spec {
    stack
        {line NAME {''} TITLE {''}}
        {or /mandatory /optional}
        {line /definition (indent) {loop LINE-OF-TEXT {}} (unindent)}
        {line item-def}
        {opt /remark (indent) {loop LINE-OF-TEXT {}} (unindent)}
}

