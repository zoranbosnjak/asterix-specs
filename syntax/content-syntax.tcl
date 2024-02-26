set title "content-def"
set spec {
    stack
        { or
            { line /raw}
            { line /table { loop { line (indent) INT : \"STRING\" (unindent)} {} } }
            { line /string {or /ascii /icao /octal } }
            { line
                { or /signed /unsigned }
                /integer
                { loop {} CONSTRAIN }
            }
            { stack
                { line
                    { or /signed /unsigned }
                    { line /quantity LSB \"UNIT\" }
                    { loop {} CONSTRAIN }
                }
            }
            { line
                /bds
                { or
                    {}
                    {/addr}
                    {?}
                }
            }
        }
}
