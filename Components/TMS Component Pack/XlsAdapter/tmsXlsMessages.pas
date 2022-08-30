unit tmsXlsMessages;
{$INCLUDE ..\FLXCOMPILER.INC}
{$INCLUDE ..\FLXCONFIG.INC}

interface
uses SysUtils,
     {$IFDEF FLX_NEEDSVARIANTS} variants,{$ENDIF}
     tmsUFlxMessages;

resourcestring
{$IFDEF SPANISH}
  {$INCLUDE XlsSpanish.inc}
{$ELSE}
{$IFDEF FRENCH}
  {$INCLUDE XlsFrench.inc}
{$ELSE}
{$IFDEF ITALIAN}
  {$INCLUDE XlsItalian.inc}
{$ELSE}
{$IFDEF ROMANIAN}
  {$INCLUDE XlsRomanian.inc}
{$ELSE}
{$IFDEF PORTUGUESEBR}
  {$INCLUDE XlsPortugueseBR.inc}
{$ELSE}
{$IFDEF CHINESE}
  {$INCLUDE XlsChinese.inc}
{$ELSE}
{$IFDEF RUSSIAN}
  {$INCLUDE XlsRussian.inc}
{$ELSE}
{$IFDEF GERMAN}
  {$INCLUDE XlsGerman.inc}
{$ELSE}
{$IFDEF POLISH}
  {$INCLUDE XlsPolish.inc}
{$ELSE}
{$IFDEF FINNISH}
  {$INCLUDE XlsFinnish.inc}
{$ELSE}
  {$INCLUDE XlsEnglish.inc}
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

  WorkbookStrS='Workbook';

  ErrInvalidStream = 'Invalid OLE Stream operation on stream: "%s"';
  ErrEofReached = 'End of stream reached with %d bytes left to read';
  ErrInvalidPropertySector = 'FlexCel can''t read the properties of this file.';


  //Error Codes
    xlerr_Null  = '#NULL!';
    xlerr_Div0  = '#DIV/0!';
    xlerr_Value = '#VALUE!';
    xlerr_Ref   = '#REF!';
    xlerr_Name  = '#NAME?';
    xlerr_Num   = '#NUM!';
    xlerr_NA    = '#N/A';

    //MADE: Traducir mensajes

const

    xlerrcode_Null  = $00;
    xlerrcode_Div0  = $07;
    xlerrcode_Value = $0F;
    xlerrcode_Ref   = $17;
    xlerrcode_Name  = $1D;
    xlerrcode_Num   = $24;
    xlerrcode_NA    = $2A;


  MaxRecordDataSize=8223;
  MaxExternSheetDataSize= 8220;  // 1370 records of 6 bytes each, and 2 bytes for the count
  MaxHPageBreaks=1025;
  MaxVPageBreaks=1025;
type
  ArrayOfByte = array[0..maxint div 2] of byte;
  PArrayOfByte=^ArrayOfByte;

  TRecordHeader = packed record
    Id: word;
    Size: word;
  end;

  PRecordHeader= ^TRecordHeader;

  EExcelException=class (Exception)
  end;

  TGetSheet=function(const aSheetRef: word): integer of object;

  TSheetInfo= record
    InsSheet, FormulaSheet: integer;
    GetSheet, SetSheet: TGetSheet;
    Names: TObject;
  end;

const
  Max_Columns       = 255;
  Max_Rows          = 65535; //0 based
  MaxSheets         = 65000;  //some negative values are reserved


  xlr_BofVersion    = $0600;

  xlb_Globals       = $0005;
  xlb_Worksheet     = $0010;
  xlb_Chart         = $0020;



  xlr_INTEGER                    = $0002;
  xlr_FORMULA                    = $0006;
  xlr_EOF                        = $000A;

  xlr_CALCCOUNT                  = $000C;
  xlr_CALCMODE                   = $000D;
  xlr_PRECISION                  = $000E;
  xlr_REFMODE                    = $000F;
  xlr_DELTA                      = $0010;
  xlr_ITERATION                  = $0011;
  xlr_PROTECT                    = $0012;
  xlr_PASSWORD                   = $0013;
  xlr_HEADER                     = $0014;
  xlr_FOOTER                     = $0015;
  xlr_EXTERNCOUNT                = $0016;
  xlr_EXTERNSHEET                = $0017;
  xlr_NAME                       = $0018;

  xlr_WINDOWPROTECT              = $0019;
  xlr_VERTICALPAGEBREAKS         = $001A;
  xlr_HORIZONTALPAGEBREAKS       = $001B;
  xlr_NOTE                       = $001C;
  xlr_SELECTION                  = $001D;

  xlr_FORMATCOUNT                = $001F;
  xlr_COLUMNDEFAULT              = $0020;

  xlr_1904                       = $0022;

  xlr_COLWIDTH                   = $0024;

  xlr_LEFTMARGIN                 = $0026;
  xlr_RIGHTMARGIN                = $0027;
  xlr_TOPMARGIN                  = $0028;
  xlr_BOTTOMMARGIN               = $0029;
  xlr_PRINTHEADERS               = $002A;
  xlr_PRINTGRIDLINES             = $002B;
  xlr_FILEPASS                   = $002F;

  xlr_PRINTSIZE                  = $0033;

  xlr_CONTINUE                   = $003C;
  xlr_WINDOW1                    = $003D;

  xlr_BACKUP                     = $0040;
  xlr_PANE                       = $0041;
  xlr_CODEPAGE                   = $0042;

  xlr_IXFE                       = $0044;
  xlr_PLS                        = $004D;
  xlr_DCON                       = $0050;
  xlr_DCONREF                    = $0051;
  xlr_DCONNAME                   = $0053;
  xlr_DEFCOLWIDTH                = $0055;
  xlr_BUILTINFMTCNT              = $0056;
  xlr_XCT                        = $0059;
  xlr_CRN                        = $005A;
  xlr_FILESHARING                = $005B;
  xlr_WRITEACCESS                = $005C;
  xlr_OBJ                        = $005D;
  xlr_UNCALCED                   = $005E;
  xlr_SAFERECALC                 = $005F;
  xlr_TEMPLATE                   = $0060;
  xlr_OBJPROTECT                 = $0063;
  xlr_COLINFO                    = $007D;

  xlr_IMDATA                     = $007F;
  xlr_GUTS                       = $0080;
  xlr_WSBOOL                     = $0081;
  xlr_GRIDSET                    = $0082;
  xlr_HCENTER                    = $0083;
  xlr_VCENTER                    = $0084;
  xlr_BOUNDSHEET                 = $0085;
  xlr_WRITEPROT                  = $0086;
  xlr_ADDIN                      = $0087;
  xlr_EDG                        = $0088;
  xlr_PUB                        = $0089;
  xlr_COUNTRY                    = $008C;
  xlr_HIDEOBJ                    = $008D;
  xlr_BUNDLESOFFSET              = $008E;
  xlr_BUNDLEHEADER               = $008F;
  xlr_SORT                       = $0090;
  xlr_SUB                        = $0091;
  xlr_PALETTE                    = $0092;

  xlr_LHRECORD                   = $0094;
  xlr_LHNGRAPH                   = $0095;
  xlr_SOUND                      = $0096;
  xlr_LPR                        = $0098;
  xlr_STANDARDWIDTH              = $0099;
  xlr_FNGROUPNAME                = $009A;
  xlr_FILTERMODE                 = $009B;
  xlr_FNGROUPCOUNT               = $009C;
  xlr_AUTOFILTERINFO             = $009D;
  xlr_AUTOFILTER                 = $009E;
  xlr_SCL                        = $00A0;
  xlr_SETUP                      = $00A1;
  xlr_COORDLIST                  = $00A9;
  xlr_GCW                        = $00AB;
  xlr_SCENMAN                    = $00AE;
  xlr_SCENARIO                   = $00AF;
  xlr_SXVIEW                     = $00B0;
  xlr_SXVD                       = $00B1;
  xlr_SXVI                       = $00B2;
  xlr_SXIVD                      = $00B4;
  xlr_SXLI                       = $00B5;
  xlr_SXPI                       = $00B6;
  xlr_DOCROUTE                   = $00B8;
  xlr_RECIPNAME                  = $00B9;

  xlr_MULRK                      = $00BD;
  xlr_MULBLANK                   = $00BE;
  xlr_MMS                        = $00C1;
  xlr_ADDMENU                    = $00C2;
  xlr_DELMENU                    = $00C3;
  xlr_SXDI                       = $00C5;
  xlr_SXDB                       = $00C6;
  xlr_SXFIELD                    = $00C7;
  xlr_SXINDEXLIST                = $00C8;
  xlr_SXDOUBLE                   = $00C9;
  xlr_SXSTRING                   = $00CD;
  xlr_SXDATETIME                 = $00CE;
  xlr_SXTBL                      = $00D0;
  xlr_SXTBRGITEM                 = $00D1;
  xlr_SXTBPG                     = $00D2;
  xlr_OBPROJ                     = $00D3;
  xlr_SXIDSTM                    = $00D5;
  xlr_RSTRING                    = $00D6;
  xlr_DBCELL                     = $00D7;
  xlr_BOOKBOOL                   = $00DA;
  xlr_SXEXTPARAMQRY              = $00DC;
  xlr_SCENPROTECT                = $00DD;
  xlr_OLESIZE                    = $00DE;
  xlr_UDDESC                     = $00DF;

  xlr_INTERFACEHDR               = $00E1;
  xlr_INTERFACEEND               = $00E2;
  xlr_SXVS                       = $00E3;
  xlr_CELLMERGING                = $00E5;
  xlr_BITMAP                     = $00E9;
  xlr_MSODRAWINGGROUP            = $00EB;
  xlr_MSODRAWING                 = $00EC;
  xlr_MSODRAWINGSELECTION        = $00ED;
  xlr_PHONETIC                   = $00EF;
  xlr_SXRULE                     = $00F0;
  xlr_SXEX                       = $00F1;
  xlr_SXFILT                     = $00F2;
  xlr_SXNAME                     = $00F6;
  xlr_SXSELECT                   = $00F7;
  xlr_SXPAIR                     = $00F8;
  xlr_SXFMLA                     = $00F9;
  xlr_SXFORMAT                   = $00FB;
  xlr_SST                        = $00FC;
  xlr_LABELSST                   = $00FD;
  xlr_EXTSST                     = $00FF;
  xlr_SXVDEX                     = $0100;
  xlr_SXFORMULA                  = $0103;
  xlr_SXDBEX                     = $0122;
  xlr_CHTRINSERT                 = $0137;
  xlr_CHTRINFO                   = $0138;
  xlr_CHTRCELLCONTENT            = $013B;
  xlr_TABID                      = $013D;
  xlr_CHTRMOVERANGE              = $0140;
  xlr_CHTRINSERTTAB              = $014D;
  xlr_USESELFS                   = $0160;
  xlr_XL5MODIFY                  = $0162;
  xlr_CHTRHEADER                 = $0196;
  xlr_USERBVIEW                  = $01A9;
  xlr_USERSVIEWBEGIN             = $01AA;
  xlr_USERSVIEWEND               = $01AB;
  xlr_QSI                        = $01AD;
  xlr_SUPBOOK                    = $01AE;
  xlr_PROT4REV                   = $01AF;
  xlr_DSF                        = $0161;
  xlr_CONDFMT                    = $01B0;
  xlr_CF                         = $01B1;
  xlr_DVAL                       = $01B2;
  xlr_DCONBIN                    = $01B5;
  xlr_TXO                        = $01B6;
  xlr_REFRESHALL                 = $01B7;
  xlr_HLINK                      = $01B8;
  xlr_CODENAME                   = $01BA;
  xlr_SXFDBTYPE                  = $01BB;
  xlr_PROT4REVPASS               = $01BC;
  xlr_DV                         = $01BE;
  xlr_XL9FILE                    = $01C0;
  xlr_RECALCID                   = $01C1;
  xlr_DIMENSIONS                 = $0200;
  xlr_BLANK                      = $0201;
  xlr_NUMBER                     = $0203;
  xlr_LABEL                      = $0204;
  xlr_BOOLERR                    = $0205;

  xlr_STRING                     = $0207;
  xlr_ROW                        = $0208;

  xlr_INDEX                      = $020B;
  xlr_ARRAY                      = $0221;
  xlr_EXTERNNAME                 = $0023;
  xlr_EXTERNNAME2                = $0223;
  xlr_DEFAULTROWHEIGHT           = $0225;
  xlr_FONT                       = $0031;
  xlr_TABLE                      = $0236;
  xlr_WINDOW2                    = $023E;

  xlr_RK                         = $027E;
  xlr_STYLE                      = $0293;

  xlr_FORMAT                     = $041E;
  xlr_XF                         = $00E0;
  xlr_SHRFMLA                    = $04BC;
  xlr_SCREENTIP                  = $0800;
  xlr_WEBQRYSETTINGS             = $0803;
  xlr_WEBQRYTABLES               = $0804;
  xlr_BOF                        = $0809;
  xlr_UNITS                      = $1001;
  xlr_ChartChart                 = $1002;
  xlr_ChartSeries                = $1003;
  xlr_ChartDataformat            = $1006;
  xlr_ChartLineformat            = $1007;
  xlr_ChartMarkerformat          = $1009;
  xlr_ChartAreaformat            = $100A;
  xlr_ChartPieformat             = $100B;
  xlr_ChartAttachedlabel         = $100C;
  xlr_ChartSeriestext            = $100D;
  xlr_ChartChartformat           = $1014;
  xlr_ChartLegend                = $1015;
  xlr_ChartSerieslist            = $1016;
  xlr_ChartBar                   = $1017;
  xlr_ChartLine                  = $1018;
  xlr_ChartPie                   = $1019;
  xlr_ChartArea                  = $101A;
  xlr_ChartScatter               = $101B;
  xlr_ChartChartline             = $101C;
  xlr_ChartAxis                  = $101D;
  xlr_ChartTick                  = $101E;
  xlr_ChartValuerange            = $101F;
  xlr_ChartCatserrange           = $1020;
  xlr_ChartAxislineformat        = $1021;
  xlr_ChartFormatlink            = $1022;
  xlr_ChartDefaulttext           = $1024;
  xlr_ChartText                  = $1025;
  xlr_ChartFontx                 = $1026;
  xlr_ChartObjectLink            = $1027;
  xlr_ChartFrame                 = $1032;
  xlr_BEGIN                      = $1033;
  xlr_END                        = $1034;
  xlr_ChartPlotarea              = $1035;
  xlr_Chart3D                    = $103A;
  xlr_ChartPicf                  = $103C;
  xlr_ChartDropbar               = $103D;
  xlr_ChartRadar                 = $103E;
  xlr_ChartSurface               = $103F;
  xlr_ChartRadararea             = $1040;
  xlr_ChartAxisparent            = $1041;
  xlr_ChartLegendxn              = $1043;
  xlr_ChartShtprops              = $1044;
  xlr_ChartSertocrt              = $1045;
  xlr_ChartAxesused              = $1046;
  xlr_ChartSbaseref              = $1048;
  xlr_ChartSerparent             = $104A;
  xlr_ChartSerauxtrend           = $104B;
  xlr_ChartIfmt                  = $104E;
  xlr_ChartPos                   = $104F;
  xlr_ChartAlruns                = $1050;
  xlr_ChartAI                    = $1051;
  xlr_ChartSerauxerrbar          = $105B;
  xlr_ChartClrClient             = $105C;
  xlr_ChartSerfmt                = $105D;
  xlr_Chart3DDataFormat          = $105F;
  xlr_ChartFbi                   = $1060;
  xlr_ChartBoppop                = $1061;
  xlr_ChartAxcext                = $1062;
  xlr_ChartDat                   = $1063;
  xlr_ChartPlotgrowth            = $1064;
  xlr_ChartSiindex               = $1065;
  xlr_ChartGelframe              = $1066;
  xlr_ChartBoppcustom            = $1067;

//------------------------------------Tokens-------------------------//
    //Globals
    tk_Arrayformula     = $1;
    tk_Table     = $2;
    tk_BinaryOps = [$3..$11];
    tk_UnaryOps  = [$12..$15];

    //Constants
    tk_MissArg   = $16;
    tk_Str       = $17;
    tk_Attr      = $19;
    tk_Err       = $1C;
    tk_Bool      = $1D;
    tk_Int       = $1E;
    tk_Num       = $1F;

    tk_MemFunc   = $29; 

    //Func
    tk_Func      = [$21, $41, $61];
    tk_FuncVar   = [$22, $42, $62];

    //Operand
    tk_Array     = [$20, $40, $60];
    tk_Name      = [$23, $43, $63];
    tk_Ref       = [$24, $44, $64];
    tk_Area      = [$25, $45, $65];
    tk_RefErr    = [$2A, $4A, $6A];
    tk_AreaErr   = [$2B, $4B, $6B];
    tk_RefN      = [$2C, $4C, $6C];  //Reference relative to the current row. Can be < 0
    tk_AreaN     = [$2D, $4D, $6D];  //Area relative to the current row
    tk_NameX     = [$39, $59, $79];
    tk_Ref3D     = [$3A, $5A, $7A];
    tk_Area3D    = [$3B, $5B, $7B];
    tk_Ref3DErr  = [$3C, $5C, $7C];
    tk_Area3DErr = [$3D, $5D, $7D];

    tk_RefToRefErr         = $2A - $24;
    tk_AreaToAreaErr       = $2B - $25;
    tk_Ref3DToRef3DErr     = $3C - $3A;
    tk_Area3DToArea3DErr   = $3D - $3B;

    tk_Operand=
        tk_Array+
        tk_Name+
        tk_Ref+
        tk_Area+
        tk_RefErr+
        tk_AreaErr+
        tk_RefN+
        tk_AreaN+
        tk_NameX+
        tk_Ref3D+
        tk_Area3D+
        tk_Ref3DErr+
        tk_Area3DErr;

//-------------------------------------Object Types--------------------------//
		ftEnd	       = $0000;
		ftMacro	     = $0004;
		ftButton     = $0005;
		ftGmo	       = $0006;
		ftCf	       = $0007;
		ftPioGrbit	 = $0008;
    ftPictFmla	 = $0009;
		ftCbls	     = $000A;
		ftRbo	       = $000B;
		ftSbs	       = $000C;
		ftNts	       = $000D;
		ftSbsFmla	   = $000E;
		ftGboData	   = $000F;
    ftEdoData	   = $0010;
		ftRboData	   = $0011;
		ftCblsData	 = $0012;
		ftLbsData	   = $0013;
		ftCblsFmla	 = $0014;
		ftCmo	       = $0015;

//-------------------------------------Cmo Object Types-----------------------//

		xlcmo_Group   	      = $00;
		xlcmo_Line  		      = $01;
		xlcmo_Rectangle  	    = $02;
		xlcmo_Oval  		      = $03;
		xlcmo_Arc  		        = $04;
		xlcmo_Chart  	        = $05;
		xlcmo_TextBox  	      = $06;
		xlcmo_Button  	      = $07;
		xlcmo_Picture  	      = $08;
		xlcmo_Polygon  	      = $09;
		xlcmo_CheckBox  	    = $0B;
		xlcmo_Option  	      = $0C;
		xlcmo_Edit  		      = $0D;
		xlcmo_Label         	= $0E;
		xlcmo_Dialog  	      = $0F;
		xlcmo_Spinner  	      = $10;
		xlcmo_Scroll  	      = $11;
		xlcmo_List  		      = $12;
		xlcmo_Group1         	= $13;
		xlcmo_Combo         	= $14;
		xlcmo_Comment  	      = $19;
		xlcmo_MSDrawingx    	= $1E;

// Escher records
    MsofbtDggContainer             = $F000;
    MsofbtDgg                      = $F006;
    MsofbtCLSID                    = $F016;
    MsofbtOPT                      = $F00B;
    MsofbtColorMRU                 = $F11A;
    MsofbtSplitMenuColors          = $F11E;
    MsofbtBstoreContainer          = $F001;
    MsofbtBSE                      = $F007;
    MsofbtDgContainer              = $F002;
    MsofbtDg                       = $F008;
    MsofbtRegroupItem              = $F118;
    MsofbtColorScheme              = $F120;
    MsofbtSpgrContainer            = $F003;
    MsofbtSpContainer              = $F004;
    MsofbtSpgr                     = $F009;
    MsofbtSp                       = $F00A;
    MsofbtTextbox                  = $F00C;
    MsofbtClientTextbox            = $F00D;
    MsofbtAnchor                   = $F00E;
    MsofbtChildAnchor              = $F00F;
    MsofbtClientAnchor             = $F010;
    MsofbtClientData               = $F011;
    MsofbtOleObject                = $F11F;
    MsofbtDeletedPspl              = $F11D;
    MsofbtSolverContainer          = $F005;
    MsofbtConnectorRule            = $F012;
    MsofbtAlignRule                = $F013;
    MsofbtArcRule                  = $F014;
    MsofbtClientRule               = $F015;
    MsofbtCalloutRule              = $F017;
    MsofbtSelection                = $F119;

    //Image types
     msobiUNKNOWN = 0;
     msobiWMF  = $216;      // Metafile header then compressed WMF
     msobiEMF  = $3D4;      // Metafile header then compressed EMF
     msobiPICT = $542;      // Metafile header then compressed PICT
     msobiPNG  = $6E0;      // One byte tag then PNG data
     msobiJFIF = $46A;      // One byte tag then JFIF data
     msobiJPEG = msobiJFIF;
     msobiDIB  = $7A8;      // One byte tag then DIB data
     msobiClient=$800;      // Clients should set this bit

     msoblipERROR   = 0;        // An error occured during loading
     msoblipUNKNOWN = 1;        // An unknown blip type
     msoblipEMF     = 2;        // Windows Enhanced Metafile
     msoblipWMF     = 3;        // Windows Metafile
     msoblipPICT    = 4;        // Macintosh PICT
     msoblipJPEG    = 5;        // JFIF
     msoblipPNG     = 6;        // PNG
     msoblipDIB     = 7;        // Windows DIB


     XlsImgConv: array[TXlsImgTypes] of byte = (msoblipEMF, msoblipWMF, msoblipJPEG, msoblipPNG, msoblipDIB, msoblipUNKNOWN);
     XlsBlipHeaderConv: array[TXlsImgTypes] of Word=($F01A, $F01B, $F01D, $F01E, $F01F, $F01A-1);
     XlsBlipSignConv: array[TXlsImgTypes] of Word=(msobiEMF, msobiWMF, msobiJPEG, msobiPNG, msobiDIB, msobiUNKNOWN);

  procedure IncMax(var X: word ; N, Max: Longint );
  procedure IncMaxMin(var X: word ; N, Max,  Min: Longint );

  procedure IncByte( const Pdata: PArrayOfByte; const tPos: integer; const Offset: integer; const Max: integer);
  procedure IncWord( const Pdata: PArrayOfByte; const tPos: integer; const Offset: integer; const Max: integer); 
  function GetWord(const Pdata: PArrayOfByte; const tPos: integer): word;
  procedure SetWord(const Pdata: PArrayOfByte; const tPos: integer; const number: Word); 
  procedure IncLongWord( const Pdata: PArrayOfByte; const tPos: integer; const Offset: int64);
  function GetLongWord(const Pdata: PArrayOfByte; const tPos: integer): LongWord;
  procedure SetLongWord(const Pdata: PArrayOfByte; const tPos: integer; const number: LongWord); 

  //These functions do not take Continue records. use them with care, only where we are sure we don't have continues
  function GetStrLen(const Length16Bit: boolean ;const Pdata: PArrayOfByte; const tPos: integer; const UseExtStrLen: boolean; const ExtStrLen: LongWord): int64;
  procedure GetSimpleString(const Length16Bit: boolean ;const Pdata: PArrayOfByte; const tPos: integer;const UseExtStrLen: boolean; const ExtStrLen: LongWord; var St: UTF16String; var StSize: integer);

  function IsWide(const W: UTF16String): boolean;
  function WideStringToStringNoCodePage(const W: UTF16String): AnsiString;
  function StringToWideStringNoCodePage(const s: AnsiString): UTF16String;
  procedure CompressBestUnicode(const w: UTF16String; const PData: PArrayOfByte; const PDataPos: integer);

type
  pWord=^Word;
  pLongWord=^LongWord;


implementation

procedure IncWord( const Pdata: PArrayOfByte; const tPos: integer; const Offset: integer; const Max: integer);
var
  w: int64;
begin
  w:=Pdata^[tPos] or (PData^[tPos+1] shl 8);
  inc(w, Offset);
  if (w<0) or (w>Max) then Raise Exception.CreateFmt(ErrTooManyEntries,[w, Max]);

  Pdata^[tPos]:= byte(w);
  Pdata^[tPos+1]:= hi(word(w));
end;

procedure IncByte( const Pdata: PArrayOfByte; const tPos: integer; const Offset: integer; const Max: integer);
var
  w: int64;
begin
  w:=Pdata^[tPos];
  inc(w, Offset);
  if (w<0) or (w>Max) then Raise Exception.CreateFmt(ErrTooManyEntries,[w, Max]);
  Pdata^[tPos]:= byte(w);
end;


procedure IncMax(var X: word ; N, Max: Longint );
begin
  if (N+X>Max) or (N+X<0) then Raise Exception.CreateFmt(ErrTooManyEntries,[N+X, Max]);
  Inc(X,N);
end;

procedure IncLongWord( const Pdata: PArrayOfByte; const tPos: integer; const Offset: int64);
var
  Pc: ^LongWord;
begin
  Pc:= @PData[tPos];
  Inc(Pc^,Offset);
end;

procedure IncMaxMin(var X: word ; N, Max, Min: Longint );
begin
  if (N+X>Max) then X:=Max else if N+X<Min then X:=Min else Inc(X,N);
end;


function GetWord(const Pdata: PArrayOfByte; const tPos: integer): word;
begin
  Result:=pWord(PAddress(Pdata)+tPos)^;
end;

function GetLongWord(const Pdata: PArrayOfByte; const tPos: integer): LongWord;
begin
  result:=PLongWord(PAddress(Pdata)+tPos)^;
end;

procedure SetLongWord(const Pdata: PArrayOfByte; const tPos: integer; const number: LongWord);
begin
  System.Move(Number, Pdata^[tPos], sizeof(LongWord))
end;

procedure SetWord(const Pdata: PArrayOfByte; const tPos: integer; const number: Word);
begin
  System.Move(Number, Pdata^[tPos], sizeof(Word))
end;

function GetStrLen(const Length16Bit: boolean ;const Pdata: PArrayOfByte; const tPos: integer;const UseExtStrLen: boolean; const ExtStrLen: LongWord): int64;
var
  l, rt: LongWord;
  bsize: byte;
  sz: LongWord;
  myPos: integer;
  oField: byte;
begin
  myPos:=tPos;
  if UseExtStrLen then l:= ExtStrLen
  else
  begin
    if Length16Bit then begin;l:=GetWord( Pdata, myPos);inc(myPos,2);end
    else begin;l:=Pdata^[myPos];inc(myPos);end;
  end;

  oField:= Pdata^[myPos];
  inc(myPos);

  bsize:= oField and $1;

  rt:=0;
  if (oField and $8)= $8 then //RTF Info
  begin
    rt:=GetWord( Pdata, myPos);
    inc(myPos, 2);
  end;

  sz:=0;
  if (oField and $4)= $4 then //Far East Info
  begin
    sz:= GetLongWord( Pdata, myPos);
    inc(myPos, 4);
  end;

  Result:=int64(myPos-tPos) + l shl bsize+ rt shl 2 + sz;

end;

procedure GetSimpleString(const Length16Bit: boolean ;const Pdata: PArrayOfByte; const tPos: integer;const UseExtStrLen: boolean; const ExtStrLen: LongWord; var St: UTF16String; var StSize: integer);
var
  l, rt: LongWord;
  bsize: byte;
  sz: LongWord;
  myPos: integer;
  oField: byte;
  ShortSt: AnsiString;
begin
  myPos:=tPos;
  if UseExtStrLen then l:= ExtStrLen
  else
  begin
    if Length16Bit then begin;l:=GetWord( Pdata, myPos);inc(myPos,2);end
    else begin;l:=Pdata^[myPos];inc(myPos);end;
  end;

  oField:= Pdata^[myPos];
  inc(myPos);

  bsize:= oField and $1;

  rt:=0;
  if (oField and $8)= $8 then //RTF Info
  begin
    rt:=GetWord( Pdata, myPos);
    inc(myPos, 2);
  end;

  sz:=0;
  if (oField and $4)= $4 then //Far East Info
  begin
    sz:= GetLongWord( Pdata, myPos);
    inc(myPos, 4);
  end;

  StSize:=int64(myPos-tPos) + l shl bsize+ rt shl 2 + sz;
  if bsize=0 then
  begin
    SetLength(ShortSt, l);
    Move(pData^[myPos], ShortSt[1], l);
    St:=StringToWideStringNoCodePage(ShortSt);
  end else
  begin
    SetLength(St, l);
    Move(pData^[myPos], St[1], l shl bsize);
  end;

end;

function IsWide(const W: UTF16String): boolean;
var
  i:integer;
begin
  for i:=1 to length(w) do if ord(w[i])>$FF then begin;Result:=true;exit;end;
  Result:=false;
end;

function WideStringToStringNoCodePage(const W: UTF16String): AnsiString;
var
  i:integer;
begin
  SetLength(Result, Length(W));
  for i:=1 to length(w) do Result[i]:=AnsiChar(Ord(w[i]) and $FF);
end;

procedure CompressBestUnicode(const w: UTF16String; const PData: PArrayOfByte; const PDataPos: integer);
var
  i:integer;
begin
  for i:=1 to length(w) do
    if Ord(w[i])<=$FF then PData[PDataPos+i-1]:=Ord(w[i]) else PData[PDataPos+i-1]:=Ord('?');
end;

function StringToWideStringNoCodePage(const s: AnsiString): UTF16String;
var
  i:integer;
begin
  SetLength(Result, Length(s));
  for i:=1 to length(s) do Result[i]:=UTF16Char(Ord(s[i]));
end;


end.
