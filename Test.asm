; great comment
        SET A, 0x1
        ADD A, 0x2
        SET B, 0x2
        SUB A, B            ; even better comment
        SET [0x1000], 0x6
        MOD [0x1000], 0x5
:addone ADD A, [0x1000]
        IFE A, 0x2
           JSR addone
