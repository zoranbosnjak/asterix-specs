set spec {
    stack
        { or
            {line /items
                {loop {line (indent) item-def (unindent) {}}}
            }
            {line /extended BITS BITS
                {loop {line (indent) item-def (unindent) {}}}
            }
            {line /compound
                {loop {line (indent) item-def (unindent) {}}}
            }
            {line /fixed BITS
                { or
                    {line /raw}
                    {line /values {loop {line (indent) INT : STRING (unindent)} {}}}
                    {stack
                        {line {or /signed /unsigned}
                            /scale INT
                            /fract INT
                        }
                        {line {opt /unit STRING}}
                        {line
                            {or
                                {}
                                {line {or /ge /gt} NUM}
                            }
                            {or
                                {}
                                {line {or /le /lt} NUM}
                            }
                        }
                    }
                }
            }
        }
}

