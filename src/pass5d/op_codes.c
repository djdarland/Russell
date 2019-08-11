/* RIC (Russell Intermediate Code) Op codes */

# define NONE ((char *) 0)
char * op_code_table[] = {
/* 0 */     NONE,   "BR",   "BRT",  "BRF",  "CLL",
/* 5 */     "LBL",  "EXT",  "LBA",  NONE,   "BFN",
/* 10 */    "TFB",  "TFE",  "PRO",  "ADT",  "ERR",
/* 15 */    "BSF",  "LBR",  "DDT",  "FDT",  NONE,
/* 20 */    "DCL",  "UDC",  "ALH",  "GAR",  "ALS",
/* 25 */    "LDI",  "STI",  "CLI",  "LDN",  "RTN",
/* 30 */    "LDL",  "MOV",  "TAR",  "PSH",  NONE,
/* 35 */    "ADP",  "CLC",  "ALA",  "HINT", "ARG",
/* 40 */    "ADI",  "SBI",  "MLI",  "DVI",  "NGI",
/* 45 */    "IDT",  "EQI",  "LTI",  "GTI",  "NEI",
/* 50 */    "LEI",  "GEI",  "SHI",  "ABI",  NONE,
/* 55 */    NONE,   NONE,   NONE,   NONE,   NONE,
/* 60 */    "TRU",  "FLS",  "AND",  "OR",   "NOT",
/* 65 */    NONE,   NONE,   NONE,   NONE,   NONE,
/* 70 */    "LDS",  "LDC",  NONE,   NONE,   NONE,
/* 75 */    NONE,   NONE,   NONE,   NONE,   NONE,
/* 80 */    "ADF",  "SBF",  "MLF",  "DVF",  "NGF",
/* 85 */    "EXF",  "EQF",  "LTF",  "GTF",  "NEF",
/* 90 */    "LEF",  "GEF",  "SHF",  NONE,   NONE,
/* 95 */    NONE,   NONE,   NONE,   NONE,   NONE,
/* 100 */   NONE,   NONE,   NONE,   NONE,   NONE,
};

