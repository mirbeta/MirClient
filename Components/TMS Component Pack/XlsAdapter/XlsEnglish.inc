  ErrFileIsNotXLS='The file "%s" is invalid';
  ErrCantReadFile='Can''t read file "%s"';
  ErrExcelInvalid= 'Error reading Excel records. File invalid';
  ErrCantWriteOutput='Can''t create output file "%s"';
  ErrCantWrite='Error writing output file';

  ErrFileNotFound='Can''t find file "%s"';
  ErrStreamNotFound='Can''t open Stream "%s"';

  ErrExtRefsNotSupported='External references to another workbook are not supported by FlexCel';

  ErrWrongExcelRecord='Record with ID 0x%x is invalid';

  ErrBadToken='Error reading formula, Token Id: 0x%2x is not supported';
  ErrBadFormula='Formula at Row %d, column %d is not supported. Token Id: 0x%2x';
  ErrBadName='Error in Worksheet name: "%s". Token Id: 0x%2x';
  ErrBadCF='Bad Conditional format. Token Id: 0x%2x';
  ErrBadChartFormula='Error in Chart formula. Token Id: 0x%2x';

  Err3DRef='Found a reference to another worksheet on a formula that does not support it';

  ErrReadingRecord='Error reading record';

  ErrInvalidVersion='Template file is saved with an invalid Excel version';

  ErrTooManyEntries= 'Too many entries for this kind of record (%d out of a maximum of %d)';
  ErrRecordNotSupported='Template contains an unsopported record: %4x';

  ErrSectionNotLoaded='A section of the workbook is not loaded';

  ErrInvalidContinue='Trying to add more than one CONTINUE record to the same record';
  ErrStringTooLarge='String too large';
  ErrInvalidDrawing='The file has an usupported drawing record';
  ErrInvalidCF='Invalid conditional format record';

  ErrDuplicatedSheetName='Sheet name already exists: %s';

  ErrDupRow='Duplicated ROW record' ;
  ErrRowMissing='Missing ROW Record for row %d';

  ErrBadCopyRows='Ivalid arguments to call InsertAndCopyRows';
  ErrInvalidSheetNo='Sheet number "%d" invalid. Must be between %d and %d';

  ErrBadRowCount='Number of ROW record is not the same that the cell count';

  ErrEscherNotLoaded='Escher record is not fully loaded';
  ErrLoadingEscher='Error loading Escher record';
  ErrBStroreDuplicated='Error loading Escher record. Duplicated BStore';
  ErrDgDuplicated='Error loading Escher record. Duplicated Dg';
  ErrDggDuplicated='Error loading Escher record. Duplicated Dgg';
  ErrsolverDuplicated='Error loading Escher record. Duplicated Solver container';

  ErrChangingEscher='Error processing Escher record';

  ErrInternal='FlexCel: Logical Error';

  ErrCantCopyPictFmla='Can''t copy OLE objects embedded on a sheet';

  ErrInvalidErrStr='Invalid cell error code: "%s"';
  ErrInvalidCellValue='Invalid cell value: "%s"';

  ErrCantWriteToFile='Can''t write to file "%s". Check it is not in use by another user';

  ErrShrFmlaNotFound='Can''t find Shared Formula';

  //Strings
  ErrInvalidStrLenLength='StrLenLength should be 1 or 2';
  ErrInvalidStringRecord='Invalid String record';

  ErrNotImplemented='This file has a not implemented kind of record: %s';

  BaseSheetName='Sheet';

  ErrNoDataSet='No dataset assigned';
  ErrReportNotFound='Report not found: %s';
  ErrTooManyPageBreaks='Too many manual Page breaks for this sheet';

  ErrInvalidRow='Invalid Row (%d)';
  ErrInvalidCol='Invalid Column (%d)';

  ErrXlsIndexOutBounds='The index "%d" of the property "%s" is not in the range %d-%d';

  ErrFileIsPasswordProtected='File is password protected';

  ErrNotAString='String expected';
  ErrUnexpectedEof='Unexpected end of formula: "%s"';
  ErrUnexpectedChar='Unexpected char "%s" at pos %d on formula: "%s"';
  ErrUnterminatedString='Unterminated string on formula: "%s"';
  ErrMissingParen='Missing closing parentheses on formula: "%s"';
  ErrFormulaTooLong='Formula too long: "%s"';
  ErrUnexpectedId='Unexpected Identifier: "%s" on formula "%s"';
  ErrFunctionNotFound='Function not found: "%s" on formula "%s"';
  ErrInvalidNumberOfParams='Invalid number of parameters to function "%s": Expected %d and found %d';
  ErrFormulaInvalid='Formula can''t be evaluated: "%s"';
  ErrFormulaStart='Formula must start with "=": "%s"';

  ErrHiddenSheetSelected='To save this file, you must select a visible sheet';
  ErrNoSheetVisible='There are not visible sheets on the file';

  ErrCantDeleteSheetWithMacros='Can''t delete a sheet on a file containing macros';

  ErrInvalidSheet='Invalid sheet: %s';
