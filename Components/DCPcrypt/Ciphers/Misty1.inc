const
  S7TABLE: array[0..$7F] of byte= (
    $89, $19, $84, $4B, $63, $F9, $E3, $B7, $59, $1D, $CC, $2C, $23, $C7, $B6, $72, 
    $DA, $26, $86, $D5, $C3, $15, $1E, $49, $1E, $42, $A8, $6E, $8E, $12, $D7, $14, 
    $C9, $FA, $55, $FC, $8C, $41, $16, $E8, $12, $39, $67, $3D, $8B, $89, $CE, $D2, 
    $4B, $04, $15, $B5, $B0, $1A, $28, $DA, $FC, $05, $9A, $2A, $9B, $17, $EF, $56, 
    $93, $E3, $FE, $58, $1D, $20, $20, $95, $9B, $73, $DF, $04, $75, $9E, $79, $CF, 
    $87, $A5, $51, $54, $2B, $E7, $29, $71, $12, $56, $CF, $85, $6E, $2E, $80, $A9, 
    $25, $E7, $3C, $DF, $0B, $45, $BF, $57, $5A, $AA, $EF, $09, $EF, $30, $E5, $38, 
    $FE, $16, $D7, $A2, $3B, $84, $9F, $72, $B5, $CF, $A3, $34, $E4, $9A, $3E, $68);

  S9TABLE: array[0..$1FF] of Dword= (
    $828, $FC0, $9C7, $B87, $F09, $9A6, $08D, $6CE, $17B, $EC3, $7A8, $346, $692, $CE4, $406, $52F, 
    $9F1, $9EA, $98B, $C7D, $5BD, $894, $1FC, $257, $D28, $027, $259, $ED7, $04A, $2C3, $96C, $CDE, 
    $615, $213, $64D, $59B, $D17, $626, $997, $19C, $7D7, $E2A, $812, $129, $D7D, $6D0, $994, $198, 
    $B96, $606, $2D2, $970, $9A7, $FEA, $CE7, $907, $356, $C8E, $153, $16B, $1E7, $A6C, $520, $745, 
    $1CE, $D36, $210, $B90, $511, $CD3, $06A, $7C5, $743, $C35, $84C, $F52, $BD2, $3DC, $528, $C14, 
    $364, $FB9, $A32, $F93, $90B, $438, $A8D, $6C8, $C15, $226, $BE3, $21C, $F22, $548, $E34, $77A, 
    $957, $D47, $798, $816, $A62, $4D2, $6B6, $7C2, $615, $E8D, $2B1, $784, $04E, $3BC, $344, $92C, 
    $5FD, $43F, $FD4, $5B8, $3F7, $BC0, $83C, $42F, $05E, $3BA, $E02, $2CD, $F5B, $E28, $8C8, $826, 
    $FF9, $8A1, $4AF, $61D, $1BC, $B85, $D6D, $C4A, $7E0, $D1E, $CD9, $BBB, $2EC, $463, $7A9, $1A7, 
    $D48, $B13, $723, $2ED, $BB7, $307, $288, $615, $663, $654, $EEA, $D9B, $330, $E23, $03F, $831, 
    $E37, $FDF, $A5F, $0D4, $108, $491, $4C1, $C54, $D7C, $216, $79E, $639, $4ED, $206, $D58, $28D, 
    $86B, $5F2, $6C7, $E80, $2DC, $CCF, $540, $D91, $19B, $142, $114, $4E7, $47C, $C8A, $739, $AC2, 
    $1D9, $DDD, $CF1, $3A9, $F76, $9D7, $C22, $B19, $4FE, $9DD, $F1A, $97C, $0CC, $716, $695, $D23, 
    $BCB, $0D6, $8AA, $005, $D2F, $21E, $B77, $7FC, $2BC, $E0D, $239, $351, $65E, $EF1, $798, $741, 
    $DE1, $7B6, $3F0, $A4E, $573, $B7D, $244, $80F, $9BB, $320, $AA7, $044, $A47, $C49, $CDF, $6F3, 
    $10E, $1FB, $9F6, $F46, $FC1, $235, $F7E, $EEA, $7BA, $883, $B52, $BB6, $533, $92D, $27E, $955, 
    $A97, $CC5, $A23, $0B2, $CAD, $FE5, $516, $EED, $449, $ECA, $CDC, $D8F, $E5C, $891, $0F9, $9C4, 
    $718, $9DB, $B14, $458, $0DF, $298, $AE8, $837, $BCC, $EAE, $09B, $939, $698, $D4F, $04C, $0E5, 
    $580, $6A7, $D96, $304, $011, $3B5, $1CB, $7AC, $A7D, $009, $396, $C9F, $34A, $123, $AE3, $39E, 
    $211, $334, $FAD, $786, $814, $F0A, $684, $5F7, $767, $0DA, $18B, $F36, $96B, $BAC, $0A3, $21A, 
    $162, $936, $F90, $DB2, $BCC, $9CD, $4D3, $683, $F6C, $C45, $7EB, $0F3, $88B, $970, $9E0, $6C7, 
    $B5C, $301, $EAB, $162, $E30, $991, $A65, $683, $041, $192, $8DC, $94F, $5CB, $2D8, $A65, $559, 
    $640, $28E, $49D, $D6E, $D4C, $B52, $9E1, $BEA, $36E, $B2B, $E37, $649, $6E0, $32E, $471, $9C7, 
    $1A1, $87C, $239, $ABA, $D40, $A6E, $DDD, $472, $94E, $5A2, $C89, $B63, $D13, $FA4, $BB4, $74C, 
    $163, $B0B, $485, $F27, $340, $7A8, $BE6, $497, $415, $6A8, $613, $0A2, $044, $E4F, $857, $767, 
    $8C2, $D21, $7BE, $C9E, $092, $026, $77C, $69B, $2C6, $415, $DCA, $191, $9E2, $D26, $9F2, $8DA, 
    $54E, $548, $A51, $00C, $D92, $472, $514, $983, $C38, $AE4, $A58, $C3C, $FF5, $907, $A94, $EAC, 
    $9E8, $4AD, $FE4, $05F, $4B1, $F7B, $D14, $018, $A1B, $637, $919, $038, $016, $4B2, $1BF, $F29, 
    $9C7, $E21, $44D, $D8B, $D72, $D93, $EDB, $FE4, $3EE, $64E, $120, $D98, $A71, $FC9, $769, $2E0, 
    $377, $D1A, $E95, $F8A, $76D, $372, $3B8, $03D, $A05, $793, $630, $3F8, $CCA, $DD8, $6FD, $3A3, 
    $BD4, $CB1, $3FF, $557, $54E, $531, $1EE, $934, $08B, $990, $BC3, $175, $D76, $D4A, $258, $B88, 
    $914, $EA3, $AFC, $3F1, $7D3, $D4D, $FB6, $2A4, $A7A, $5F5, $807, $1B1, $65F, $E6F, $4CE, $3ED);
