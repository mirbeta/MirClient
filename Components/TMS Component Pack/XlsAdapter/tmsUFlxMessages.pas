/// <summary>
/// Contains a list of types, constants, variables and generic utility methods used in the whole suite.<para></para>
/// You will normally need to use this file when using FlexCel.
/// </summary>                                                                                                      
unit tmsUFlxMessages;
{$INCLUDE ..\FLXCOMPILER.INC}
{$INCLUDE ..\FLXCONFIG.INC}

interface
uses {$IFNDEF FLX_CROSSPLAT}Windows,{$ENDIF}
     {$IFDEF FLX_NEEDSVARIANTS} variants, varutils, {$ENDIF}
     {$IFDEF FLX_EXTRAWINDOWS}ActiveX,{$ENDIF} //Delphi 5

     {$INCLUDE UsePngLib.inc}
     Classes, SysUtils;

const
  /// <summary>
  /// \Internal use. Specifies the default locale.
  /// </summary>                                  
  FLX_VAR_LOCALE_USER_DEFAULT = $400;

resourcestring
  /// <summary>
  /// \Internal use. Specifies the separator character for tags in TFlexCelReport.
  /// </summary>
  FieldStr='##';

  /// <summary>
  /// \Internal use. Specifies the characters that a name has to have to be a data range in TFlexCelReport.
  /// </summary>
  DataSetStr='__';

  /// <summary>
  /// \Internal use. Specifies the separator character for report variables in TFlexCelReport.
  /// </summary>
  VarStr='#.';

  /// <summary>
  /// \Internal use. Specifies an special character in TFlexCelReport.
  /// </summary>
  StrOpen='<';

  /// <summary>
  /// \Internal use. Specifies an special character in TFlexCelReport.
  /// </summary>
  StrClose='>';


  /// <summary>
  /// \Internal use. Specifies an special character in TFlexCelReport.
  /// </summary>
  ExtrasDelim='...';

  /// <summary>
  /// \Internal use. Specifies an special character in TFlexCelReport.
  /// </summary>
  MarkedRowStr='...delete row...';  //Remember to change ExtrasDelim if changing this

  /// <summary>
  /// \Internal use. Specifies an special character in TFlexCelReport.
  /// </summary>
  HPageBreakStr='...page break...'; //Remember to change ExtrasDelim if changing this

  /// <summary>
  /// \Internal use. Specifies an special character in TFlexCelReport.
  /// </summary>
  FullDataSetStr='*';

  /// <summary>
  /// \Internal use. Specifies an special character in TFlexCelReport.
  /// </summary>
  MainTxt='MAIN'; //This is not strictly necessary... just for checking the template

  /// <summary>
  /// \Internal use. Specifies an special character in TFlexCelReport.
  /// </summary>
  RecordCountPrefix='RC_';

  /// <summary>
  /// \Internal use. Specifies an special character in TFlexCelReport.
  /// </summary>
  DefaultDateTimeFormat='mm/dd/yyyy hh:mm';

  {$I FlexCelVersion.inc}

{$IFDEF SPANISH}
  {$INCLUDE FlxSpanish.inc}
{$ELSE}
{$IFDEF FRENCH}
  {$INCLUDE FlxFrench.inc}
{$ELSE}
{$IFDEF ITALIAN}
  {$INCLUDE FlxItalian.inc}
{$ELSE}
{$IFDEF ROMANIAN}
  {$INCLUDE FlxRomanian.inc}
{$ELSE}
{$IFDEF PORTUGUESEBR}
  {$INCLUDE FlxPortugueseBR.inc}
{$ELSE}
{$IFDEF CHINESE}
  {$INCLUDE FlxChinese.inc}
{$ELSE}
{$IFDEF RUSSIAN}
  {$INCLUDE FlxRussian.inc}
{$ELSE}
{$IFDEF GERMAN}
  {$INCLUDE FlxGerman.inc}
{$ELSE}
{$IFDEF POLISH}
  {$INCLUDE FlxPolish.inc}
{$ELSE}
{$IFDEF FINNISH}
  {$INCLUDE FlxFinnish.inc}
{$ELSE}
  {$INCLUDE FlxEnglish.inc}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}

  /// <summary>
  /// \Internal use. Specifies an special character in TFlexCelReport.
  /// </summary>
  xls_Emf='EMF';

  /// <summary>
  /// \Internal use. Specifies an special character in TFlexCelReport.
  /// </summary>
  xls_Wmf='WMF';

  /// <summary>
  /// \Internal use. Specifies an special character in TFlexCelReport.
  /// </summary>
  xls_Jpeg='JPEG';

  /// <summary>
  /// \Internal use. Specifies an special character in TFlexCelReport.
  /// </summary>
  xls_Png='PNG';

type
{$IFDEF DELPHI2008UP}
    UTF16String = UnicodeString;
    UTF16Char = Char;
    PAddress = PByte;
{$ELSE}
    /// <summary>
    /// An UTF16 wide string. This type maps to WideString in Delphi less than 2009 or to UnicodeString otherwise.
    /// </summary>
    UTF16String = WideString;

    /// <summary>
    /// An UTF16 wide char. This type maps to WideChar in Delphi less than 2009 or to Char otherwise.
    /// </summary>
    UTF16Char = WideChar;

    /// <summary>
    /// Used for pointer arithmetic. Point to PAnsiChar if Delphi is less than 2009, or to PByte otherwise.
    /// </summary>                                                                                         
    PAddress = PAnsiChar;
    UInt32 = LongWord;
    Int32 = LongInt;
    UInt16 = word;
    Int16 = SmallInt;
{$ENDIF}

{$IFDEF USEPNGLIB}
  
    {$IFNDEF DELPHI2008UP}
     TPngImage = TPNGObject;
    
  {$ENDIF}
{$ENDIF}

	/// <summary>
	/// Image Anchor information.
	/// </summary>
  TClientAnchor= packed record

		/// <summary>
		/// How the image behaves when copying/inserting cells. It might have 3 values: <para></para>
    ///  0 - Move and Resize the image. <para></para>
    ///  2 - Move but don't Resize the image. <para></para>
    ///  3 - Don't Move and don't Resize the image. <para></para>
		/// </summary>
    Flag: word;

		/// <summary>
		/// First column of object
		/// </summary>
    Col1: word;

		/// <summary>
		/// Delta x of image, on 1/1024 of a cell.  0 means totally at the left, 512 on half of the cell, 1024 means at the left of next cell.
		/// </summary>
    Dx1: word;

		/// <summary>
		/// First Row of object.
		/// </summary>
    Row1: word;

		/// <summary>
		/// Delta y of image on 1/255 of a cell. 0 means totally at the top, 128 on half of the cell, 255 means at the top of next cell.
		/// </summary>
    Dy1: word;

		/// <summary>
		/// Last column of object.
		/// </summary>
    Col2: word;

		/// <summary>
		/// Delta x of image, on 1/1024 of a cell.  0 means totally at the left, 512 on half of the cell, 1024 means at the left of next cell.
		/// </summary>
    Dx2: word;

		/// <summary>
		/// Last row of object.
		/// </summary>
    Row2: word;

		/// <summary>
		/// Delta y of image on 1/255 of a cell. 0 means totally at the top, 128 on half of the cell, 255 means at the top of next cell.
		/// </summary>
    Dy2: word;
  end;

  /// <summary> Pointer to a TClientAnchor </summary>
  PClientAnchor = ^TClientAnchor;

  WidestringArray=array of UTF16String;
  WideCharArray=array of UTF16Char;
  BooleanArray = Array of Boolean;
  ByteArray = Array of Byte;


  /// <summary>
  /// Printer specific settings. It is a byte array with a Win32 DEVMODE structure. 
  /// </summary>                                                                    
  TPrinterDriverSettings = record
    /// <summary>
    /// Determines the O.S. this structure was saved in. 0 means windows. 
    /// </summary>                                                        
    OperatingEnviroment: word;

    /// <summary>
    /// When OperatingEnviroment=0 (windows) you can cast this Data to a DevMode struct.
    /// </summary>
    Data: array of byte;
  end;

  /// <summary>
  /// An integer expressing an Excel standard paper size.<para></para>
  /// Use TExcelPaperSize_XXX constants for the allowed values.
  /// </summary>                                                      
  TExcelPaperSize = integer;

  const
       /// <summary>Difference in days between the 1900 and 1904 date systems supported by Excel.</summary>
       Date1904Diff = 4 * 365 + 2;

        /// <summary>Not defined.</summary>
        TExcelPaperSize_Undefined=0;
        ///<summary>Letter - 81/2"" x 11""</summary>
        TExcelPaperSize_Letter=1;
        ///<summary>Letter small - 81/2"" x 11""</summary>
        TExcelPaperSize_Lettersmall=2;
        ///<summary>Tabloid - 11"" x 17""</summary>
        TExcelPaperSize_Tabloid=3;
        ///<summary>Ledger - 17"" x 11""</summary>
        TExcelPaperSize_Ledger=4;
        ///<summary>Legal - 81/2"" x 14""</summary>
        TExcelPaperSize_Legal=5;
        ///<summary>Statement - 51/2"" x 81/2""</summary>
        TExcelPaperSize_Statement=6;
        ///<summary>Executive - 71/4"" x 101/2""</summary>
        TExcelPaperSize_Executive=7;
        ///<summary>A3 - 297mm x 420mm</summary>
        TExcelPaperSize_A3=8;
        ///<summary>A4 - 210mm x 297mm</summary>
        TExcelPaperSize_A4=9;
        ///<summary>A4 small - 210mm x 297mm</summary>
        TExcelPaperSize_A4small=10;
        ///<summary>A5 - 148mm x 210mm</summary>
        TExcelPaperSize_A5=11;
        ///<summary>B4 (JIS) - 257mm x 364mm</summary>
        TExcelPaperSize_B4_JIS=12;
        ///<summary>B5 (JIS) - 182mm x 257mm</summary>
        TExcelPaperSize_B5_JIS=13;
        ///<summary>Folio - 81/2"" x 13""</summary>
        TExcelPaperSize_Folio=14;
        ///<summary>Quarto - 215mm x 275mm</summary>
        TExcelPaperSize_Quarto=15;
        ///<summary>10x14 - 10"" x 14""</summary>
        TExcelPaperSize_s10x14=16;
        ///<summary>11x17 - 11"" x 17""</summary>
        TExcelPaperSize_s11x17=17;
        ///<summary>Note - 81/2"" x 11""</summary>
        TExcelPaperSize_Note=18;
        ///<summary>Envelope #9 - 37/8"" x 87/8""</summary>
        TExcelPaperSize_Envelope9=19;
        ///<summary>Envelope #10 - 41/8"" x 91/2""</summary>
        TExcelPaperSize_Envelope10=20;
        ///<summary>Envelope #11 - 41/2"" x 103/8""</summary>
        TExcelPaperSize_Envelope11=21;
        ///<summary>Envelope #12 - 43/4"" x 11""</summary>
        TExcelPaperSize_Envelope12=22;
        ///<summary>Envelope #14 - 5"" x 111/2""</summary>
        TExcelPaperSize_Envelope14=23;
        ///<summary>C - 17"" x 22""</summary>
        TExcelPaperSize_C=24;
        ///<summary>D - 22"" x 34""</summary>
        TExcelPaperSize_D=25;
        ///<summary>E - 34"" x 44""</summary>
        TExcelPaperSize_E=26;
        ///<summary>Envelope DL - 110mm x 220mm</summary>
        TExcelPaperSize_EnvelopeDL=27;
        ///<summary>Envelope C5 - 162mm x 229mm</summary>
        TExcelPaperSize_EnvelopeC5=28;
        ///<summary>Envelope C3 - 324mm x 458mm</summary>
        TExcelPaperSize_EnvelopeC3=29;
        ///<summary>Envelope C4 - 229mm x 324mm</summary>
        TExcelPaperSize_EnvelopeC4=30;
        ///<summary>Envelope C6 - 114mm x 162mm</summary>
        TExcelPaperSize_EnvelopeC6=31;
        ///<summary>Envelope C6/C5 - 114mm x 229mm</summary>
        TExcelPaperSize_EnvelopeC6_C5=32;
        ///<summary>B4 (ISO) - 250mm x 353mm</summary>
        TExcelPaperSize_B4_ISO=33;
        ///<summary>B5 (ISO) - 176mm x 250mm</summary>
        TExcelPaperSize_B5_ISO=34;
        ///<summary>B6 (ISO) - 125mm x 176mm</summary>
        TExcelPaperSize_B6_ISO=35;
        ///<summary>Envelope Italy - 110mm x 230mm</summary>
        TExcelPaperSize_EnvelopeItaly=36;
        ///<summary>Envelope Monarch - 37/8"" x 71/2""</summary>
        TExcelPaperSize_EnvelopeMonarch=37;
        ///<summary>63/4 Envelope - 35/8"" x 61/2""</summary>
        TExcelPaperSize_s63_4Envelope=38;
        ///<summary>US Standard Fanfold - 147/8"" x 11""</summary>
        TExcelPaperSize_USStandardFanfold=39;
        ///<summary>German Std. Fanfold - 81/2"" x 12""</summary>
        TExcelPaperSize_GermanStdFanfold=40;
        ///<summary>German Legal Fanfold - 81/2"" x 13""</summary>
        TExcelPaperSize_GermanLegalFanfold=41;
        ///<summary>B4 (ISO) - 250mm x 353mm</summary>
        TExcelPaperSize_B4_ISO_2=42;
        ///<summary>Japanese Postcard - 100mm x 148mm</summary>
        TExcelPaperSize_JapanesePostcard=43;
        ///<summary>9x11 - 9"" x 11""</summary>
        TExcelPaperSize_s9x11=44;
        ///<summary>10x11 - 10"" x 11""</summary>
        TExcelPaperSize_s10x11=45;
        ///<summary>15x11 - 15"" x 11""</summary>
        TExcelPaperSize_s15x11=46;
        ///<summary>Envelope Invite - 220mm x 220mm</summary>
        TExcelPaperSize_EnvelopeInvite=47;
        ///<summary>Undefined - </summary>
        ///<summary>Letter Extra - 91/2"" x 12""</summary>
        TExcelPaperSize_LetterExtra=50;
        ///<summary>Legal Extra - 91/2"" x 15""</summary>
        TExcelPaperSize_LegalExtra=51;
        ///<summary>Tabloid Extra - 1111/16"" x 18""</summary>
        TExcelPaperSize_TabloidExtra=52;
        ///<summary>A4 Extra - 235mm x 322mm</summary>
        TExcelPaperSize_A4Extra=53;
        ///<summary>Letter Transverse - 81/2"" x 11""</summary>
        TExcelPaperSize_LetterTransverse=54;
        ///<summary>A4 Transverse - 210mm x 297mm</summary>
        TExcelPaperSize_A4Transverse=55;
        ///<summary>Letter Extra Transv. - 91/2"" x 12""</summary>
        TExcelPaperSize_LetterExtraTransv=56;
        ///<summary>Super A/A4 - 227mm x 356mm</summary>
        TExcelPaperSize_SuperA_A4=57;
        ///<summary>Super B/A3 - 305mm x 487mm</summary>
        TExcelPaperSize_SuperB_A3=58;
        ///<summary>Letter Plus - 812"" x 1211/16""</summary>
        TExcelPaperSize_LetterPlus=59;
        ///<summary>A4 Plus - 210mm x 330mm</summary>
        TExcelPaperSize_A4Plus=60;
        ///<summary>A5 Transverse - 148mm x 210mm</summary>
        TExcelPaperSize_A5Transverse=61;
        ///<summary>B5 (JIS) Transverse - 182mm x 257mm</summary>
        TExcelPaperSize_B5_JIS_Transverse=62;
        ///<summary>A3 Extra - 322mm x 445mm</summary>
        TExcelPaperSize_A3Extra=63;
        ///<summary>A5 Extra - 174mm x 235mm</summary>
        TExcelPaperSize_A5Extra=64;
        ///<summary>B5 (ISO) Extra - 201mm x 276mm</summary>
        TExcelPaperSize_B5_ISO_Extra=65;
        ///<summary>A2 - 420mm x 594mm</summary>
        TExcelPaperSize_A2=66;
        ///<summary>A3 Transverse - 297mm x 420mm</summary>
        TExcelPaperSize_A3Transverse=67;
        ///<summary>A3 Extra Transverse - 322mm x 445mm</summary>
        TExcelPaperSize_A3ExtraTransverse=68;
        ///<summary>Dbl. Japanese Postcard - 200mm x 148mm</summary>
        TExcelPaperSize_DblJapanesePostcard=69;
        ///<summary>A6 - 105mm x 148mm</summary>
        TExcelPaperSize_A6=70;
        ///<summary>Letter Rotated - 11"" x 81/2""</summary>
        TExcelPaperSize_LetterRotated=75;
        ///<summary>A3 Rotated - 420mm x 297mm</summary>
        TExcelPaperSize_A3Rotated=76;
        ///<summary>A4 Rotated - 297mm x 210mm</summary>
        TExcelPaperSize_A4Rotated=77;
        ///<summary>A5 Rotated - 210mm x 148mm</summary>
        TExcelPaperSize_A5Rotated=78;
        ///<summary>B4 (JIS) Rotated - 364mm x 257mm</summary>
        TExcelPaperSize_B4_JIS_Rotated=79;
        ///<summary>B5 (JIS) Rotated - 257mm x 182mm</summary>
        TExcelPaperSize_B5_JIS_Rotated=80;
        ///<summary>Japanese Postcard Rot. - 148mm x 100mm</summary>
        TExcelPaperSize_JapanesePostcardRot=81;
        ///<summary>Dbl. Jap. Postcard Rot. - 148mm x 200mm</summary>
        TExcelPaperSize_DblJapPostcardRot=82;
        ///<summary>A6 Rotated - 148mm x 105mm</summary>
        TExcelPaperSize_A6Rotated=83;
        ///<summary>B6 (JIS) - 128mm x 182mm</summary>
        TExcelPaperSize_B6_JIS=88;
        ///<summary>B6 (JIS) Rotated - 182mm x 128mm</summary>
        TExcelPaperSize_B6_JIS_Rotated=89;
        ///<summary>12x11 - 12"" x 11""</summary>
        TExcelPaperSize_s12x11=90;

const
  /// <summary>
	/// Convert the DEFAULT column width to pixels. This is different from ColMult, that goes in a column by column basis.
	/// </summary>
  DefColWidthAdapt: integer=Round(256*8/7);  //font used here is 8 pixels wide, not 7

  //Printer Options
  ///Print over, then down
  fpo_LeftToRight = $01;

  ///0= landscape, 1=portrait
  fpo_Orientation = $02;

  /// if 1, then PaperSize, Scale, Res, VRes, Copies, and Landscape data have not been obtained from the printer, so they are not valid.
  /// MAKE SURE YOU MAKE THIS BIT = 0 *BEFORE* CHANGING ANY OTHER OPTION. THEY WILL NOT CHANGE IF THIS IS NOT SET.
  fpo_NoPls       = $04;

  ///1= Black and white
  fpo_NoColor     = $08;

  ///1= Draft quality
  fpo_Draft       = $10;

  ///1= Print Notes
  fpo_Notes       = $20;

  ///1=orientation not set
  fpo_NoOrient    = $40;

  ///1=use custom starting page number.
  fpo_UsePage     = $80;

  /// <summary>
  /// Internal range name.
  /// On Excel, internal range names like "Print_Range" are stored as a 1 character string.
  /// </summary>
  InternalNameRange_Consolidate_Area  = AnsiChar($00);

  /// <summary>
  /// Internal range name.
  /// On Excel, internal range names like "Print_Range" are stored as a 1 character string.
  /// </summary>
  InternalNameRange_Auto_Open         = AnsiChar($01);

  /// <summary>
  /// Internal range name.
  /// On Excel, internal range names like "Print_Range" are stored as a 1 character string.
  /// </summary>
  InternalNameRange_Auto_Close        = AnsiChar($02);

  /// <summary>
  /// Internal range name.
  /// On Excel, internal range names like "Print_Range" are stored as a 1 character string.
  /// </summary>
  InternalNameRange_Extract           = AnsiChar($03);

  /// <summary>
  /// Internal range name.
  /// On Excel, internal range names like "Print_Range" are stored as a 1 character string.
  /// </summary>
  InternalNameRange_Database          = AnsiChar($04);

  /// <summary>
  /// Internal range name.
  /// On Excel, internal range names like "Print_Range" are stored as a 1 character string.
  /// </summary>
  InternalNameRange_Criteria          = AnsiChar($05);

  /// <summary>
  /// Internal range name.
  /// On Excel, internal range names like "Print_Range" are stored as a 1 character string.
  /// </summary>
  InternalNameRange_Print_Area        = AnsiChar($06);

  /// <summary>
  /// Internal range name.
  /// On Excel, internal range names like "Print_Range" are stored as a 1 character string.
  /// </summary>
  InternalNameRange_Print_Titles      = AnsiChar($07);

  /// <summary>
  /// Internal range name.
  /// On Excel, internal range names like "Print_Range" are stored as a 1 character string.
  /// </summary>

  /// <summary>
  /// Internal range name.
  /// On Excel, internal range names like "Print_Range" are stored as a 1 character string.
  /// </summary>
  InternalNameRange_Recorder          = AnsiChar($08);

  /// <summary>
  /// Internal range name.
  /// On Excel, internal range names like "Print_Range" are stored as a 1 character string.
  /// </summary>

  /// <summary>
  /// Internal range name.
  /// On Excel, internal range names like "Print_Range" are stored as a 1 character string.
  /// </summary>
  InternalNameRange_Data_Form         = AnsiChar($09);

  /// <summary>
  /// Internal range name.
  /// On Excel, internal range names like "Print_Range" are stored as a 1 character string.
  /// </summary>
  InternalNameRange_Auto_Activate     = AnsiChar($0A);

  /// <summary>
  /// Internal range name.
  /// On Excel, internal range names like "Print_Range" are stored as a 1 character string.
  /// </summary>
  InternalNameRange_Auto_Deactivate   = AnsiChar($0B);

  /// <summary>
  /// Internal range name.
  /// On Excel, internal range names like "Print_Range" are stored as a 1 character string.
  /// </summary>
  InternalNameRange_Sheet_Title       = AnsiChar($0C);

  /// <summary>
  /// Internal range name.
  /// On Excel, internal range names like "Print_Range" are stored as a 1 character string.
  /// </summary>
  InternalNameRange_Filter_DataBase = AnsiChar($0D);



var
  /// <summary>
  /// Factor to convert from <see cref="Excel Internal Units" /> to pixels or viceversa. Look at <see cref="Excel Internal Units" />
  /// for more information.
  /// </summary>
  ColMult:extended=256/7; //36.6;

  /// <summary>
  /// Factor to convert from <see cref="Excel Internal Units" /> to pixels or viceversa. Look at <see cref="Excel Internal Units" />
  /// for more information.
  /// </summary>                                                                                                                    
  RowMult:extended=15;


type
  /// <summary>
  /// The range of color indexes allowed by Excel 2003 or older.
  /// </summary>                                                
  TColorPaletteRange=1..56;

	/// <summary>
	/// An Excel Cell range, 1-based.
	/// </summary>
  TXlsCellRange=record
		/// <summary>
		/// First column on range.
		/// </summary>
    Left: integer;

		/// <summary>
		/// First row on range.
		/// </summary>
    Top: integer;

		/// <summary>
		/// Last column on range.
		/// </summary>
    Right: integer;

		/// <summary>
		/// Last row on range.
		/// </summary>
    Bottom: integer;
  end;

  {$IFDEF FLX_NO_TSIZE}     //delphi 5
  TSize = tagSIZE;
  {$ENDIF}


  /// <summary>
  /// An Excel named range.
  /// </summary>
  TXlsNamedRange=record
    /// <summary>
    /// The name of the range.
    /// </summary>
    Name: string;

    /// <summary>
    /// This is a formula defining the range. It can be used to define complex ranges.
    /// For example you can use "=Sheet1!$A1:$B65536,Sheet1!$A1:$IV2".
    /// </summary>
    /// <remarks>
    /// Do not use ranges like "A:B" this is not supported by FlexCel. Always use the full name (A1:B65536).
    /// </remarks>
    RangeFormula: string;

    /// <summary>
    /// Options of the range as an integer.
    /// Bit   Mask   Description
    ///   0  0001h   = 1 if the name is hidden
    ///   1  0002h   = 1 if the name is a function
    ///   2  0004h   = 1 if the name is a Visual Basic procedure
    ///   3  0008h   = 1 if the name is a function or command name on a macro sheet
    ///   4  0010h   = 1 if the name contains a complex function
    ///   5  0020h   = 1 if the name is a built-in name. (NOTE: When setting a built in named range, this bit is automatically set)
    /// </summary>
    OptionFlags: integer;

    /// <summary>
    /// The sheet index for the name (1 based). A named range can have the same name than other
    /// as long as they are on different sheets. The default value(0) means a global named range, not tied to
    /// any specific sheet.
    /// </summary>
    NameSheetIndex: integer;
  end;

  /// <summary>
  /// Initializes a TXlsNamedRange record with the default values.
  /// </summary>
  /// <remarks>
  /// Use this method always after creating a new TXlsNamedRange record if you are not getting the value
  /// from other function.<para></para>
  /// \Note that initializing the record yourself by setting all the members might fail in the future if
  /// new members are added to the record.
  /// </remarks>
  /// <param name="NamedRange">Record you want to initialize.</param>                                   
  procedure InitializeNamedRange(out NamedRange: TXlsNamedRange);

  type
	/// <summary>
	/// Sheet margin for printing, in inches.
	/// </summary>
  TXlsMargins=packed record  //C++ builder gets this struct wrong if we use a normal record.
		/// <summary>
		/// Left Margin in inches.
		/// </summary>
    Left: extended;

		/// <summary>
		/// Top Margin in inches.
		/// </summary>
    Top: extended;

		/// <summary>
		/// Right Margin in inches.
		/// </summary>
    Right: extended;

		/// <summary>
		/// Bottom Margin in inches.
		/// </summary>
    Bottom: extended;

		/// <summary>
		/// Header Margin in inches. Space for the header at top of page, it is taken from Top margin.
		/// </summary>
    Header: extended;

		/// <summary>
		/// Footer Margin in inches. Space for the footer at bottom of page, it is taken from Bottom margin.
		/// </summary>
    Footer: extended;
  end;

	/// <summary>
	/// Sheet visibility.
	/// </summary>
  TXlsSheetVisible= (
		/// <summary>Sheet is visible to the user.</summary>
    sv_Visible,

		/// <summary>Sheet is hidden, can be shown by the user with excel.</summary>
    sv_Hidden,

		/// <summary>Sheet is hidden, only way to show it is with a macro. (user can't see it with excel)</summary>
    sv_VeryHidden);

	/// <summary>
	/// One RTF run for the text in a cell. FirstChar is the first (base 1) character to apply the format, and FontIndex is the font index for the text
	/// </summary>
  TRTFRun= record
		/// <summary>
		/// First character on the string where we will apply the font. (1 based)
		/// </summary>
    FirstChar: word;

		/// <summary>
		/// Font index for this string part.
		/// </summary>
    FontIndex: word;
  end;

	/// <summary>
	/// An array of TRTFRun structures, where each struct identifies a font style for a portion of text.<para></para>
	/// For example, if you have: <c>Value=&quot;Hello&quot; RTFRuns={FirstChar:1 FontIndex=1, FirstChar=3,
	/// FontIndex=2}</c><para></para>
	/// &quot;H&quot; (char 0) will be formatted with the specific cell format. &quot;el&quot; (chars 1 and
	/// 2) will have font number 1 &quot;lo&quot; (chars 3 and 4) will have font number 2
	/// </summary>                                                                                                   
  TRTFRunList= array of TRTFRun;

	/// <summary>
	/// A string cell value with its rich text information.
	/// </summary>
  TRichString= record

		/// <summary>
		/// Cell text.
		/// </summary>
    Value: UTF16String;

		/// <summary>
		/// Rich text info.
		/// </summary>
    RTFRuns: TRTFRunList;
  end;

  /// <summary>
  /// Possible types of cell hyperlinks.
  /// </summary>
  THyperLinkType= (

    /// <summary>
    /// Web, file or mail URL. (like http://, file://, mailto://, ftp://)
    /// </summary>
    hl_URL,

    /// <summary>
    /// A file on the local disk. Not an url or unc file.
    /// </summary>
    hl_LocalFile,

    /// <summary>
    /// Universal Naming convention. For example: \\server\path\file.ext
    /// </summary>
    hl_UNC,

    /// <summary>
    /// An hyperlink inside the current file.
    /// </summary>
    hl_CurrentWorkbook);

  /// <summary>
  /// An encapsulation of an Excel hyperlink.
  /// </summary>
  THyperLink= record

    /// <summary>
    /// The type of hyperlink: to a local file, to an url, to a cell or to a networked file.
    /// </summary>
    LinkType: THyperLinkType;

    /// <summary>
    /// Text of the HyperLink. This is empty when linking to a cell.
    /// </summary>
    Description: UTF16String;

    /// <summary>
    /// Description of the HyperLink.
    /// </summary>
    TargetFrame: UTF16String;

    /// <summary>
    /// When entering an URL on excel, you can enter additional text following the url with a "#" character (for example www.your_url.com#myurl") The text Mark is the text after the "#" char. When entering an address to a cell, the address goes here too.
    /// </summary>
    TextMark: UTF16String;

    /// <summary>
    /// This parameter is not documented. You can leave it empty.
    /// </summary>
    Text: UTF16String;

    /// <summary>
    /// Hint when the mouse hovers over the hyperlink.
    /// </summary>
    Hint: UTF16String;
  end;


type
  /// <summary>
  /// Event associated with <see cref="TCustomFlexCelReport.OnGetFilename" />.
  /// </summary>                                                              
  TOnGetFileNameEvent  = procedure (Sender: TObject; const  FileFormat: integer; var Filename: TFileName) of object;
  /// <summary>
  /// Event associated with <see cref="TCustomFlexCelReport.OnGetOutStream" />.
  /// </summary>                                                               
  TOnGetOutStreamEvent = procedure (Sender: TObject; const  FileFormat: integer; var OutStream: TStream) of object;


	/// <summary>
	/// Possible image types on an excel sheet.
	/// </summary>
  TXlsImgTypes = (
    /// <summary>
    /// Enhanced Windows Metafile. This is a Vectorial image format.
    /// </summary>
    xli_Emf,

		/// <summary>
		/// Windows Metafile. This is a Vectorial image format.
		/// </summary>
    xli_Wmf,

		/// <summary>
		/// JPEG Image. This is a losely compressed bitmap, best suited for photos.
		/// </summary>
    xli_Jpeg,

		/// <summary>
		/// Portable Network Graphics. This is a lossless compressed bitmap, best suited for text.
		/// </summary>
    xli_Png,

		/// <summary>
		/// Windows Bitmap. As this is not compressed, don't use it except for really small images.
		/// </summary>
    xli_Bmp,

		/// <summary>
		/// Unsupported image format.
		/// </summary>
    xli_Unknown);

  VariantArray=Array [0..maxint div sizeof(Variant)-1]of variant;
  ArrayOfVariant=Array of Variant;


  /// <summary>
	/// Encapsulates the value in a cell.
	/// </summary>
  TXlsCellValue= record

    /// <summary> Value of the cell </summary>
    Value: variant;

    /// <summary> Index to the Format for the cell. </summary>
    XF: integer;

    /// <summary> True if the cell contains a formula. If this is the case, you need to use CellFormula to read the value. </summary>
    IsFormula: boolean;
  end;

  {$IFDEF NOFORMATSETTINGS}
  /// <summary>
  /// This record is a placeholder for older Delphi versions that don't have FormatSettings defined for
  /// handling different locales.
  /// </summary>                                                                                       
  TFormatSettings = record
  /// <summary>
  /// Represents a format setting for Delphi 6.
  /// </summary>
    CurrencyString: string;

  /// <summary>
  /// Represents a format setting for Delphi 6.
  /// </summary>
    CurrencyFormat: Byte;

  /// <summary>
  /// Represents a format setting for Delphi 6.
  /// </summary>
    CurrencyDecimals: Byte;

  /// <summary>
  /// Represents a format setting for Delphi 6.
  /// </summary>
    DateSeparator: Char;

  /// <summary>
  /// Represents a format setting for Delphi 6.
  /// </summary>
    TimeSeparator: Char;

  /// <summary>
  /// Represents a format setting for Delphi 6.
  /// </summary>
    ListSeparator: Char;

  /// <summary>
  /// Represents a format setting for Delphi 6.
  /// </summary>
    ShortDateFormat: string;
    
  /// <summary>
  /// Represents a format setting for Delphi 6.
  /// </summary>
	LongDateFormat: string;

  /// <summary>
  /// Represents a format setting for Delphi 6.
  /// </summary>
    TimeAMString: string;

  /// <summary>
  /// Represents a format setting for Delphi 6.
  /// </summary>
    TimePMString: string;

  /// <summary>
  /// Represents a format setting for Delphi 6.
  /// </summary>
    ShortTimeFormat: string;

  /// <summary>
  /// Represents a format setting for Delphi 6.
  /// </summary>
    LongTimeFormat: string;

  /// <summary>
  /// Represents a format setting for Delphi 6.
  /// </summary>
    ThousandSeparator: Char;

  /// <summary>
  /// Represents a format setting for Delphi 6.
  /// </summary>
    DecimalSeparator: Char;

  /// <summary>
  /// Represents a format setting for Delphi 6.
  /// </summary>
    TwoDigitYearCenturyWindow: Word;

  /// <summary>
  /// Represents a format setting for Delphi 6.
  /// </summary>
    NegCurrFormat: Byte;
  end;
  {$ENDIF}

  PFormatSettings = ^TFormatSettings;

  ///<summary> Defines how an image is anchored to the sheet.</summary>
  TFlxAnchorType=(

    ///<summary>Move and resize the image when inserting rows or columns.</summary>
    at_MoveAndResize,

    ///<summary>Move but don't resize the image when inserting rows or columns.</summary>
    at_MoveAndDontResize,

    /// <summary>
    /// Don't move and don't resize the image when inserting rows or columns.
    /// </summary>                                                           
    at_DontMoveAndDontResize);

	/// <summary>
	/// Image information for a normal image.
	/// </summary>
  TImageProperties=record
		/// <summary>
		/// First column of object
		/// </summary>
    Col1: integer;

		/// <summary>
		/// Delta x of image, on 1/1024 of a cell.  0 means totally at the left, 512 on half of the cell, 1024 means at the left of next cell.
		/// </summary>
    dx1: integer;

		/// <summary>
		/// First Row of object.
		/// </summary>
    Row1: integer;

		/// <summary>
		/// Delta y of image on 1/255 of a cell. 0 means totally at the top, 128 on half of the cell, 255 means at the top of next cell.
		/// </summary>
    dy1: integer;

		/// <summary>
		/// Last column of object.
		/// </summary>
    Col2: integer;

		/// <summary>
		/// Delta x of image, on 1/1024 of a cell.  0 means totally at the left, 512 on half of the cell, 1024 means at the left of next cell.
		/// </summary>
    dx2: integer;

		/// <summary>
		/// Last row of object.
		/// </summary>
    Row2: integer;

		/// <summary>
		/// Delta y of image on 1/255 of a cell. 0 means totally at the top, 128 on half of the cell, 255 means at the top of next cell.
		/// </summary>
    dy2: integer;

		/// <summary>
		/// FileName of the image. It sets/gets the original filename of the image before it was inserted.
		/// (For example: c:\image.jpg) It is not necessary to set this field, and when the image is not inserted
		/// from a file but pasted, Excel does not set it either.
		/// </summary>
    FileName: UTF16String;
  end;


  /// <summary>
  /// This method will search for a file in the disk.
  /// </summary>
  /// <remarks>
  /// You can use this method to quickly find files in your application folder.<para></para>
  /// <para></para>
  /// The order in which this method will search for the file is:<para></para>
  /// 1) If the path is an absolute path, it will return it.<para></para>
  /// 2) If the path is relative, it will first search for it in the folder where the application is.<para></para>
  /// 3) If it couldn't find it, it will search for the file in the folder where the package is, if FlexCel
  /// is in a package. This might be useful if you are running for example in IIS where the the path for
  /// the Application is the path for IIS and not for your dll.
  /// </remarks>
  /// <param name="AFileName">FileName we want to find.</param>
  /// <returns>
  /// Fully qualified filename and path to the file.
  /// </returns>
  function SearchPathStr(const AFileName: String): String; overload;

  /// <summary>
  /// This method will search for a file in the disk.
  /// </summary>
  /// <remarks>
  /// You can use this method to quickly find files in your application folder.<para></para>
  /// <para></para>
  /// The order in which this method will search for the file is:<para></para>
  /// 1) If AFileName is an absolute path, it will return it.<para></para>
  /// 2) If AFilePath is not empty, this method will look for AFIleName in AFilePath.<para></para>
  /// 3) If AFilePath is empty and AFileName is relative, it will first search for it in the folder where the application is.<para></para>
  /// 4) If it couldn't find it in 3), it will search for the file in the folder where the package is, if FlexCel
  /// is in a package. This might be useful if you are running for example in IIS where the the path for
  /// the Application is the path for IIS and not for your dll.
  /// </remarks>
  /// <param name="AFilePath">Path to append before AFileName. If this string is empty, FlexCel will
  /// search in the application path.</param>
  /// <param name="AFileName">FileName we want to find.</param>
  /// <returns>
  /// Fully qualified filename and path to the file.
  /// </returns>
  function SearchPathStr(const AFilePath, AFileName: String): String; overload;

  {$IFDEF  VER130}
  function IncludeTrailingPathDelimiter(const S: string): string;
  function VarIsClear(const v: variant): boolean;
  function TryStrToInt(const s: string; var i: integer): boolean;
  function TryStrToFloat(const s: string; var i: extended): boolean;
  {$ENDIF}

  /// <summary>
  /// A simple helper function that will convert a variant to a widestring in Delphi &lt; 2009, and
  ///  to an UnicodeString for Delphi &gt;= 2009
  /// </summary>
  /// <remarks>
  /// This method is used internally by FlexCel.
  /// </remarks>
  /// <param name="v">Variant we want to convert.</param>
  /// <returns>
  /// The converted string.
  /// </returns>
  function VariantToString(const v: variant): UTF16String;

  {$IFDEF NOFORMATSETTINGS}
  procedure GetLocaleFormatSettings(LCID: Integer; var FormatSettings: TFormatSettings);
  {$ENDIF}

  procedure EnsureAMPM(var FormatSettings: PFormatSettings);

  /// <summary>
  /// Tries to convert a string to a float in an invariant culture. This means &quot;.&quot; is <b>always</b>
  /// decimal separator.
  /// </summary>
  /// <remarks>
  /// This method is used internally by FlexCel.
  /// </remarks>
  /// <param name="s">String we want to convert, in English locale.</param>
  /// <param name="i">\Returns the converted value.</param>
  /// <returns>
  /// True if the string was successfully converted.
  /// </returns>
  function TryStrToFloatInvariant(const s: string; out i: extended): boolean;

  {$IFDEF FLX_NEEDSPOSEX}
  function PosEx(const SubStr, S: UTF16String; Offset: Cardinal): Integer;
  {$ENDIF} //Delphi 6


  function WideUpperCase98(const s: UTF16String):UTF16String;

  function StringReplaceSkipQuotes(const S, OldPattern, NewPattern: UTF16String): UTF16String;
  /// <summary>
  /// Tries to convert a string to a TDateTime.
  /// </summary>
  /// <remarks>
  /// This routine uses the locale in the machine where the application is running to try to guess the
  /// correct date.<para></para>
  /// This means for example that if your locale is &quot;dd/mm/yyyy&quot; the date 5/8/2004 will be
  /// converted to Agust 5, 2004, while if you have &quot;mm/dd/yyyy&quot; the date will be May 8, 2004.
  /// </remarks>
  /// <param name="S">String with the date.</param>
  /// <param name="Value">S converted to a TDateTime.</param>
  /// <param name="dFormat">\Returns the date format used to convert.</param>
  /// <param name="HasDate">\Returns true if the string has a date format.</param>
  /// <param name="HasTime">\Returns true if the string had a time format.</param>
  /// <param name="DateFormat">Optional parameter. If you specify a string here like &quot;dd/mm/yyyy&quot;
  ///                          this will be returned in dformat if the string contains a date.</param>
  /// <param name="TimeFormat">Optional parameter. If you specify a string here like &quot;hh\:mm\:ss&quot;
  ///                          this will be returned in dformat if the string contains time.</param>
  /// <param name="FormatSettings">Optional parameter. If you specify a format here it will be used. If not the default format settings in the machine will be.</param>
  /// <returns>
  /// True if the string could be converted, false otherwise.<para></para>
  ///
  /// </returns>
  function FlxTryStrToDateTime(const S: UTF16String; out Value: TDateTime; out dFormat: UTF16String; out HasDate, HasTime: boolean; const DateFormat: UTF16String=''; const TimeFormat: UTF16String=''; const FormatSettings: PFormatSettings = nil): Boolean;

  /// <property name="flag" value="deprecated" />
  /// 
  /// <summary>
  /// This method has been deprecated. Use <see cref="TryFormatDateTime1904@string@TDateTime@boolean" text="TryFormatDateTime1904" />
  /// instead.
  /// </summary>
  /// <remarks>
  /// Excel has two different date systems: one starts at 1900 and the other at 1904. While 1900 is the
  /// most common (it is the default in Windows), 1904 is used in Macs and also might be set in Excel for
  /// Windows too in the options dialog.<para></para>
  /// <para></para>
  /// This method assumes always 1900 dates, so it is not safe to use and you should use &quot;1904&quot;
  /// overloads instead.
  /// </remarks>
  /// <param name="Fmt">Excel format to apply to the date.</param>
  /// <param name="value">DateTime we want to format.</param>
  /// <returns>
  /// A string with the formatted datetime. 
  /// </returns>                                                                                                                     
  function TryFormatDateTime(const Fmt: string; value: TDateTime): string; deprecated {$IFDEF FLX_HAS_DEPRECATED_COMMENTS}'Use TryFormatDateTime1904 instead'{$ENDIF};

  /// <summary>
  /// Converts a datetime value to a formatted string, using Excel format strings.
  /// </summary>
  /// <remarks>
  /// This method correctly handles 1904 dates, so it should be used instead of TryFormatDateTime.
  /// </remarks>
  /// <param name="Fmt">Format string in Excel notation. (something like &quot;dd/mmm/yyyyy hh\:ss&quot;)</param>
  /// <param name="value">DateTime we want to format.</param>
  /// <param name="Dates1904">A boolean indicating if the workbook uses 1904 or 1900 dates. <b>Note that
  ///                         the result will be different depending on this parameter.</b> You will
  ///                         normally get the value for this parameter from
  ///                         TFlexCelImport.Options1904Dates</param>
  /// <returns>
  /// The datetime formatted as a string.<para></para>
  /// 
  /// </returns>                                                                                                 
  function TryFormatDateTime1904(const Fmt: string; value: TDateTime; const Dates1904: boolean): string; overload;

  /// <summary>
  /// Converts a datetime value to a formatted string, using Excel format strings.
  /// </summary>
  /// <remarks>
  /// This method correctly handles 1904 dates, so it should be used instead of TryFormatDateTime.
  /// </remarks>
  /// <param name="Fmt">Format string in Excel notation. (something like &quot;dd/mmm/yyyyy
  ///                   hh\:ss&quot;)</param>
  /// <param name="value">DateTime we want to format.</param>
  /// <param name="Dates1904">A boolean indicating if the workbook uses 1904 or 1900 dates. <b>Note
  ///                         that the result will be different depending on this parameter.</b> You
  ///                         will normally get the value for this parameter from
  ///                         TFlexCelImport.Options1904Dates</param>
  /// <param name="LocalSettings">Locale settings used for the conversion. If for example the locale is
  ///                             Spanish, the resulting string might be &quot;5 de Abril&quot; instead of
  ///                             &quot;April 5&quot;</param>
  /// <returns>
  /// The datetime formatted as a string. 
  /// </returns>                                                                                          
  function TryFormatDateTime1904(const Fmt: string; value: TDateTime; const Dates1904: boolean; const LocalSettings: TFormatSettings): string; overload;

  /// <summary>
  /// Increments a Cell range by an offset.
  /// </summary>
  /// <remarks>
  /// This is a simple method that will take a cell range, increment its columns and rows by a given
  /// value, and return the new range.
  /// </remarks>
  /// <param name="CellRange">Original cell range</param>
  /// <param name="DeltaRow">Rows to add to the range (both top and bottom rows)</param>
  /// <param name="DeltaCol">Columns to add to the range (both left and right columns)</param>
  /// <returns>
  /// A new range with the rows and columns incremented or decremented.
  /// </returns>                                                                                     
  function OffsetRange(const CellRange: TXlsCellRange; const DeltaRow, DeltaCol: integer): TXlsCellRange;

  //Returns "A" for column 1, "B"  for 2 and so on
  /// <summary>
  /// \Returns a column identifier for a column index.
  /// </summary>
  /// <remarks>
  /// This method will return &quot;A&quot; for column 1, &quot;B&quot; for column 2, and &quot;IV&quot;
  /// for column 256.
  /// </remarks>
  /// <param name="C">Index to the column (1 based)</param>
  function EncodeColumn(const C: integer): string;

  /// <summary>
  /// \Internal use. Returns a TFormatSettings object with the default settings.
  /// </summary>
  /// <remarks>
  /// This method will return a cached LocalSettings if supported by the Delphi version.
  /// </remarks>
  function GetDefaultLocaleFormatSettings: PFormatSettings;

  /// <summary>
  /// \Internal use. Returns a TFormatSettings object with invariant settings.
  /// </summary>
  /// <remarks>
  /// This method will return a cached LocalSettings if supported by the Delphi version.
  /// </remarks>
  function InvariantFormatSettings: PFormatSettings;

implementation

function EncodeColumn(const C: integer): string;
var
  Delta: integer;
begin
  Delta:=Ord('Z')-Ord('A')+1;
  if C<=Delta then Result:=chr(Ord('A')+C-1) else
    Result:=EncodeColumn(((C-1) div Delta))+ chr(Ord('A')+(C-1) mod Delta);
end;

function IsAbsolute(const AFileName: string): boolean;
begin
  if ExtractFileDrive(AFileName) <> '' then Result := true //this takes care of UNC drives too.
  else if (Length(AFileName) > 0) and (AFileName[1] = PathDelim) then Result := true
  else Result := false;
end;

function SearchPathStr(const AFileName: String): String;
begin
  Result := SearchPathStr('', AFileName)
end;

function SearchPathStr(const AFilePath, AFileName: String): String; overload;
var
  SearchPath: string;
  SearchFile: string;
begin
  if IsAbsolute(AFileName) then
  begin;
    if not FileExists(AFileName) then raise Exception.CreateFmt(ErrCantFindFile,[AFileName]);
    Result := AFileName;
    exit;
  end;

  if Trim(AFilePath) <> '' then
  begin
    SearchFile := IncludeTrailingPathDelimiter(AFilePath) + AFileName;
    if FileExists(SearchFile) then begin; Result := SearchFile; exit; end;
    raise Exception.CreateFmt(ErrCantFindFile,[SearchFile]);
  end;

  SearchPath := ExtractFilePath(ParamStr(0));
  SearchFile := IncludeTrailingPathDelimiter(SearchPath) + AFileName;
  if FileExists(SearchFile) then begin; Result := SearchFile; exit; end;

  {$IFNDEF FLX_CROSSPLAT}
  SearchPath := ExtractFilePath(GetModuleName(HINSTANCE));
  SearchFile := IncludeTrailingPathDelimiter(SearchPath) + AFileName;
  if FileExists(SearchFile) then begin; Result := SearchFile; exit; end;
  {$ENDIF}

  raise Exception.CreateFmt(ErrCantFindFile,[AFileName]);

end; // SearchRecStr

{$IFDEF  VER130}
function IncludeTrailingPathDelimiter(const S: string): string;
begin
  Result:=IncludeTrailingBackslash(s);
end;

function VarIsClear(const v: variant): boolean;
begin
  Result:=VarIsNull(v);
end;

function TryStrToInt(const s: string; var i: integer): boolean;
var
  errcode: integer;
begin
  val(s, i, errcode);
  Result:= errCode = 0;
end;

function TryStrToFloat(const s: string; var i: extended): boolean;
var
  errcode: integer;
begin
  val(s, i, errcode);
  Result:= errCode = 0;
end;
{$ENDIF}

{$IFDEF NOFORMATSETTINGS}
procedure GetLocaleFormatSettings(LCID: Integer; var FormatSettings: TFormatSettings);
begin
  //Built in formatsettings are not in Delphi 5/6
    FormatSettings.CurrencyString:= CurrencyString;
    FormatSettings.CurrencyFormat:= CurrencyFormat;
    FormatSettings.CurrencyDecimals:= CurrencyDecimals;
    FormatSettings.DateSeparator:= DateSeparator;
    FormatSettings.TimeSeparator:= TimeSeparator;
    FormatSettings.ListSeparator:= ListSeparator;
    FormatSettings.ShortDateFormat:= ShortDateFormat;
    FormatSettings.LongDateFormat:= LongDateFormat;
    FormatSettings.TimeAMString:= TimeAMString;
    FormatSettings.TimePMString:= TimePMString;
    FormatSettings.ShortTimeFormat:= ShortTimeFormat;
    FormatSettings.LongTimeFormat:= LongTimeFormat;
    FormatSettings.ThousandSeparator:= ThousandSeparator;
    FormatSettings.DecimalSeparator:= DecimalSeparator;
    FormatSettings.TwoDigitYearCenturyWindow:= TwoDigitYearCenturyWindow;
    FormatSettings.NegCurrFormat:= NegCurrFormat;
end;

{$ENDIF}
procedure EnsureAMPM(var FormatSettings: PFormatSettings);
begin
       //Windows uses empty AM/PM designators as empty. Excel uses AM/PM. This happens for example on German locale.
      if (FormatSettings.TimeAMString = '') then
      begin
        FormatSettings.TimeAMString := 'AM';
      end;
      if (FormatSettings.TimePMString = '') then
      begin
        FormatSettings.TimePMString := 'PM';
      end;
end;

var
  CachedRegionalCulture: TFormatSettings;  //Cached because it is slow.
  CachedInvariantCulture: TFormatSettings;

function GetDefaultLocaleFormatSettings: PFormatSettings;
begin
{$IFDEF DELPHIXEUP}
    if (CachedRegionalCulture.DecimalSeparator = #0) then
      CachedRegionalCulture := TFormatSettings.Create();
{$ELSE}
    if (CachedRegionalCulture.DecimalSeparator = #0) then GetLocaleFormatSettings(GetThreadLocale, CachedRegionalCulture);
{$ENDIF}

  Result:= @CachedRegionalCulture;
end;

function InvariantFormatSettings: PFormatSettings;
begin
{$IFDEF DELPHIXEUP}
  if CachedInvariantCulture.DecimalSeparator = #0 then
      CachedInvariantCulture := TFormatSettings.Create('en-US');
{$ELSE}
  if CachedInvariantCulture.DecimalSeparator = #0 then
      GetLocaleFormatSettings($0409, CachedInvariantCulture);
{$ENDIF}

  Result := @CachedInvariantCulture;
end;

function TryStrToFloatInvariant(const s: string; out i: extended): boolean;
var
  errcode: integer;
begin
  i := 0;
  val(s, i, errcode);
  Result:= errCode = 0;
end;


{$IFDEF WIDEUPPEROK}
  function WideUpperCase98(const s: UTF16String):UTF16String;
  begin
  
  Result:=WideUpperCase(s);
  end;
{$ELSE}
  function WideUpperCase98(const s: UTF16String):UTF16String;
  var
    Len: Integer;
  begin
    Len := Length(S);
    SetString(Result, PWideChar(S), Len);
    if Len > 0 then CharUpperBuffW(Pointer(Result), Len);
    if GetLastError> 0 then result := UpperCase(s);
  end;
{$ENDIF}

//Defined as there is not posex on d5
function PosEx(const SubStr, S: UTF16String; Offset: Cardinal): Integer;
var
  i,k: integer;
  Equal: boolean;
begin
  i:= Offset;
  Result:=-1;

  while i<=Length(s)-Length(SubStr)+1 do
  begin
    if s[i]=Substr[1] then
    begin
      Equal:=true;
      for k:=2 to Length(Substr) do if s[i+k-1]<>Substr[k] then
      begin
        Equal:=false;
        break;
      end;
      if Equal then
      begin
        Result:=i;
        exit;
      end;
    end;
    inc(i);
  end;
end;

function StartsWith(const SubStr, S: UTF16String; Offset: integer): boolean;
var
  i: integer;
begin
  Result := false;

  if Offset - 1 + Length(SubStr) > Length(s)  then exit;

  for i := 1 to Length(SubStr) do
  begin
    if S[i + Offset - 1] <> SubStr[i] then exit;
  end;
  Result:= true;
end;

function StringReplaceSkipQuotes(const S, OldPattern, NewPattern: UTF16String): UTF16String;
var
  SearchStr, Patt: UTF16String;
  i,k,z: Integer;
  InQuote: boolean;
begin
  SearchStr := WideUpperCase98(S);
  Patt := WideUpperCase98(OldPattern);

  SetLength(Result, Length(SearchStr)*2);
  InQuote:=false;

  i:=1;k:=1;
  while i<= Length(SearchStr) do
  begin
    if SearchStr[i]='"' then InQuote:= not InQuote;
    if not InQuote and (StartsWith(Patt,SearchStr,i)) then
    begin
       if k+Length(NewPattern)-1>Length(Result) then SetLength(Result, k+Length(NewPattern)+100);
     for z:=1 to Length(NewPattern) do Result[z+k-1]:=NewPattern[z];
      inc(k, Length(NewPattern));
      inc(i, Length(Patt));
    end else
    begin
      if k>Length(Result) then SetLength(Result, k+100);
      Result[k]:=s[i];
      inc(i);
      inc(k);
    end;
  end;

  SetLength(Result, k-1);
end;


function DateIsOk(s: string; const v: TDateTime): boolean;
  //We have an issue with a string like '1.2.3'
  //If we are using german date separator (".") it will be converted to
  //Feb 1, 2003, which is ok. But, if we use another format, windows will think it
  //is a time, and will convert it to 1:02:03 am. That's why we added this 'patch' function.
var
  p: integer;
  i, err, k: integer;
begin
  Result:= true;
  if (Trunc(v)<>0) then exit;
  s:=s+'.';
  for i:=1 to 3 do
  begin
    p:= pos('.',s);
    if p<=0 then
    begin
      if i=3 then Result:=false;
      exit;
    end;
    val(copy(s,1,p-1), k, err);
    if (err<>0) or (k<0) then exit;
    s:=copy(s,p+1,Length(s));
  end;
  if trim(s)<'' then exit;
  Result:=false;
end;

function FlxTryStrToDateTime(const s:UTF16String; out Value: TDateTime; out dFormat: UTF16String; out HasDate, HasTime: boolean; const DateFormat: UTF16String=''; const TimeFormat: UTF16String=''; const FormatSettings: PFormatSettings = nil): Boolean;
var
{$IFNDEF FLX_CROSSPLAT}
  LResult: HResult;
{$ENDIF}
  aDateFormat, aTimeFormat: UTF16String;
  {$IFDEF FLX_NOVARDATEFROMSTRING} //Delphi 5
    v1: olevariant;
  {$ENDIF}
  FmSet: PFormatSettings;
begin
  if FormatSettings = nil then FmSet := GetDefaultLocaleFormatSettings else FmSet := FormatSettings;

  if DateFormat='' then aDateFormat:= FmSet.ShortDateFormat else aDateFormat:=DateFormat;
  if TimeFormat='' then aTimeFormat:= FmSet.ShortTimeFormat else aTimeFormat:=TimeFormat;
  aTimeFormat:=StringReplaceSkipQuotes(aTimeFormat,'AMPM','AM/PM'); //Format AMPM is not recognized by Excel. This is harcoded on sysutils

  {$IFDEF FLX_CROSSPLAT}
    Result := TryStrToDate(s, value, FmSet^) and DateIsOk(s, Value);
  {$ELSE}
      //--------------------READ THIS!--------------------------------------------------------------------------------------//
      // If you get an error here with Delphi 6, make sure to install ALL latest Delphi 6 update packs, including RTL3 update
      //--------------------------------------------------------------------------------------------------------------------//
      // available from www.borland.com
     {$IFDEF DELPHIXE3UP}
      LResult := VarDateFromStr(PCHAR(S), FLX_VAR_LOCALE_USER_DEFAULT, 0, Value);
     {$ELSE}
      LResult := VarDateFromStr(S, FLX_VAR_LOCALE_USER_DEFAULT, 0, Value);
     {$ENDIF}
      Result:=(LResult = 0) and DateIsOk(s,Value);  //VAR_OK doesnt work on D5;
  {$ENDIF}

  //We have a problem with the german date separator "." and a.m. or p.m.
  //so we cant just test for a "." inside a formula to know it includes a date.
  HasDate:=(pos('.', s)>0) or (pos('/',s)>0) or (pos('-',s)>0)   //hate to hard-code this values, but I see not other viable way
          or (pos(FmSet.DateSeparator, s)>0);
  HasDate:= HasDate and (Trunc(Value)>0);
  HasTime:=(pos(':',s)>0) or (pos(FmSet.TimeSeparator, s)>0);    //Again... hard-coding :-( At least is isolated here

  if not HasDate and not HasTime then Result:=false;  //Things like "1A" are converted to times, even when it doesn't make sense.
  dFormat:='';
  if HasDate then dFormat:=dFormat+aDateFormat;
  if HasTime then
  begin
    if dFormat<>'' then dFormat:=dFormat+' ';
    dFormat:=dFormat+aTimeFormat;
  end;

end;

function TryFormatDateTime(const Fmt: string; value: TDateTime): string;
begin
  try
    Result :=FormatDateTime(Fmt, value);
  except
    Result :='##';
  end;
end;

function TryFormatDateTime1904(const Fmt: string; value: TDateTime; const Dates1904: boolean; const LocalSettings: TFormatSettings): string;
begin
  try
    if (Dates1904) then value:= value + Date1904Diff;
   {$IFDEF  NOFORMATSETTINGS}
    Result :=FormatDateTime(Fmt, value);
   {$ELSE}
    Result :=FormatDateTime(Fmt, value, LocalSettings);
   {$ENDIF}

  except
    Result :='##';
  end;
end;

function TryFormatDateTime1904(const Fmt: string; value: TDateTime; const Dates1904: boolean): string;
begin
  try
    if (Dates1904) then value:= value + Date1904Diff;
    Result :=FormatDateTime(Fmt, value);
  except
    Result :='##';
  end;
end;


function OffsetRange(const CellRange: TXlsCellRange; const DeltaRow, DeltaCol: integer): TXlsCellRange;
begin
  Result:=CellRange;
  inc(Result.Top, DeltaRow);
  inc(Result.Left, DeltaCol);
  inc(Result.Bottom, DeltaRow);
  inc(Result.Right, DeltaCol);
end;

procedure InitializeNamedRange(out NamedRange: TXlsNamedRange);
begin
  NamedRange.Name:='';
  NamedRange.RangeFormula:='';
  NamedRange.OptionFlags:=0;
  NamedRange.NameSheetIndex:=0;
end;


function VariantToString(const v: variant): UTF16String;
begin
{$IFDEF DELPHI2008UP}
  Result := VarToStr(v);
{$ELSE}
  Result := VarToWideStr(v);
{$ENDIF}
end;
end.







