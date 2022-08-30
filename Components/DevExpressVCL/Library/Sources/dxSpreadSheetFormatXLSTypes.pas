{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpreadSheet                                       }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL    }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxSpreadSheetFormatXLSTypes;

{$I cxVer.Inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Classes, Generics.Defaults, Generics.Collections, Graphics, dxCore, dxXMLDoc, dxGDIPlusClasses, dxSpreadSheetCore,
  dxSpreadSheetFormatXLSXReader, dxSpreadSheetFormatXLSXTags, dxSpreadSheetClasses, cxVariants,
  cxGraphics, dxZIPUtils, dxSpreadSheetFormatUtils, dxSpreadSheetHyperlinks, dxCoreClasses, dxSpreadSheetGraphics,
  dxSpreadSheetCoreStyles;

type
  TdxXLSMethod = procedure of object;

  TdxXFExtPropertyData = class;
  TdxSpreadSheetXLSReaderThemeHelper = class;
  TdxBIFFHyperlinkHelper = class;

  { TdxXLSFont }

  TdxXLSFont = class
  public
    Style: TFontStyles;
    Script: Word;
    Size: Integer;
    Charset: TFontCharset;
    Color: TColor;
    Name: string;
    Handle: TdxSpreadSheetFontHandle;

    procedure AssignTo(AFont: TdxSpreadSheetFontHandle);
  end;

  { TdxXLSCellStyle }

  TdxXLSCellStyle = class
  public
    Font: Word;
    Format: Word;
    Flags: Word;
    Alignments: Word;
    Indents: Word;
    Borders: TcxBorders;
    BordersStyle: Word;
    LeftRightBorders: Word;
    TopBottomBordersAndFill: Integer;
    FillColors: Word;
    //
    Handle: TdxSpreadSheetCellStyleHandle;
    ExtendedData: TdxXFExtPropertyData;

    constructor Create;
    destructor Destroy; override;
  end;

  TdxXLSWriterSSTBacketInfo = packed record
    StreamPosition: Integer;
    OffsetInSST: Word;
    Reserved: Word;
  end;

  TdxXFExtColorProp = packed record
    xclrType: Word;
    nTintShade: SmallInt;
    xclrValue: TColor;
    Reserved: Double;
  end;

  TdxXFExtPropertyID = (xfextRGBForeColor, xfextRGBBackColor, reserved1, reserved2, xfextForeColor, xfextBackColor,
    xfextGradientTint, xfextBorderColorTop, xfextBorderColorBottom, xfextBorderColorLeft, xfextBorderColorRight,
    xfextBorderColorDiag, reserved3, xfextTextColor, xfextFontScheme, xfextIndent);

  TdxXFExtProperties = set of TdxXFExtPropertyID;

  TdxXFExtPropertyData = class
  public
    AssignedThemeValues: TdxXFExtProperties;
    AssignedValues: TdxXFExtProperties;
    DataValue: array[TdxXFExtPropertyID] of Integer;
    TintShade: array[TdxXFExtPropertyID] of SmallInt;

    procedure CheckColors(ATheme: TdxSpreadSheetXLSReaderThemeHelper);
    procedure CorrectStyle(ABrush: TdxSpreadSheetBrushHandle; ABorders: TdxSpreadSheetBordersHandle;
      AStyle: TdxSpreadSheetCellStyleHandle);
    procedure SetThemeColorValue(APropID: TdxXFExtPropertyID; const AColorIndex: Integer; ATintShade: SmallInt);
    procedure SetValue(APropID: TdxXFExtPropertyID; const AValue: Integer);
  end;

  TdxMsoCrc32Compute = class
  private
    crcCache: array[0..255] of Cardinal;
    procedure MsoCrc32Compute;
    procedure AddByte(AData: Byte);
  public
    crcValue: Cardinal;
    constructor Create;
    procedure Add(AData: PByte; ACount: Integer);
  end;

  TdxXLSRecordBand = class
  public
    Offset: Integer;
    Size: Integer;
  end;

  TdxSpreadSheetXLSReaderThemeHelper = class(TdxSpreadSheetXLSXReaderThemeParser)
  strict private
    FColorMap: TDictionary<TdxXMLString, TColor>;
    FThemedBrushes: TObjectList<TdxGPBrush>;
    FThemedColors: TList;
    FThemedPens: TObjectList<TdxGPPen>;
  protected
    function ExtractDocument(AStream: TStream; const ADocName: AnsiString): TdxXMLDocument;
    function GetColorMap: TDictionary<TdxXMLString, TColor>; override;
    function GetThemedBrushes: TObjectList<TdxGPBrush>; override;
    function GetThemedColors: TList; override;
    function GetThemedPens: TObjectList<TdxGPPen>; override;

    procedure InitializeStandardTheme;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure Clear;
    procedure Execute; override;
    procedure LoadFromStream(AStream: TStream);
    function GetColor(AColorIndex: Integer; ATintShade: SmallInt; var AColor: Integer): Boolean;
  end;

  TdxXLSNameDefinition = class
  private
    FData: Pointer;
    FName: TdxSpreadSheetDefinedName;
    FSize: Integer;
  public
    constructor Create(AName: TdxSpreadSheetDefinedName; ASize: Integer; AData: Pointer);
    destructor Destroy; override;
    procedure Restore(AReader: TObject);
  end;

  // Hyperlinks

  TdxSpreadSheetHyperlinkType = (hltFile, hltURL, hltEMail, hltReference);

  TdxSpreadSheetURICreationFlag = (createAllowRelative, createAllowImplicitWildcardScheme, createAllowImplicitFileScheme,
    createNoFrag, createNoCanonicalize, createCanonicalize, createFileUseDosPath, createDecodeExtraInfo,
    createNoDecodeExtraInfo, createCrackUnknownSchemes, createNoCrackUnknownSchemes, createPreProcessHtmlUri,
    createNoPreProcessHtmlUri, createIESettings, createNoIESettings, createNoEncodeForbiddenCharacters);

  TdxSpreadSheetURICreationFlags = set of TdxSpreadSheetURICreationFlag;

  TdxHyperlinkMonikerObject = class
  strict private
    FVersion: Integer;
  protected
    function GetValue: string; virtual;
    procedure Initialize(AHyperlink: TdxSpreadSheetHyperlink); virtual;
    procedure ReadFromStream(AHelper: TdxBIFFHyperlinkHelper; AReader: TcxReader); virtual;
    procedure WriteToStream(AHelper: TdxBIFFHyperlinkHelper; AWriter: TcxWriter); virtual;
  public
    class procedure Register; virtual;

    property Version: Integer read FVersion write FVersion;
  end;

  TdxHyperlinkMonikerObjectClass = class of TdxHyperlinkMonikerObject;

  TdxURLMoniker = class(TdxHyperlinkMonikerObject)
  strict private
    FGUID: TGUID;
    FURL: string;
    FURIFlags: TdxSpreadSheetURICreationFlags;
  protected
    function GetValue: string; override;
    procedure Initialize(AHyperlink: TdxSpreadSheetHyperlink); override;
    procedure ReadFromStream(AHelper: TdxBIFFHyperlinkHelper; AReader: TcxReader); override;
    procedure WriteToStream(AHelper: TdxBIFFHyperlinkHelper; AWriter: TcxWriter); override;
  public
    class procedure Register; override;

    property GUID: TGUID read FGUID write FGUID;
    property URIFlags: TdxSpreadSheetURICreationFlags read FURIFlags write FURIFlags;
    property URL: string read FURL write FURL;
  end;

  TdxFileMoniker = class(TdxURLMoniker)
  strict private
    FEndServer: Word;
    FParentCount: Word;
  protected
    function GetValue: string; override;
    procedure Initialize(AHyperlink: TdxSpreadSheetHyperlink); override;
    procedure ReadFromStream(AHelper: TdxBIFFHyperlinkHelper; AReader: TcxReader); override;
    procedure WriteToStream(AHelper: TdxBIFFHyperlinkHelper; AWriter: TcxWriter); override;
  public
    class procedure Register; override;

    property EndServer: Word read FEndServer write FEndServer;
    property ParentCount: Word read FParentCount write FParentCount;
  end;

  TdxItemMoniker = class(TdxHyperlinkMonikerObject)
  strict private
    FDelimiter: string;
    FPath: string;
    function GetMonikerPart(AHelper: TdxBIFFHyperlinkHelper; AReader: TcxReader): string;
    procedure WriteMonikerPart(AHelper: TdxBIFFHyperlinkHelper; AWriter: TcxWriter; const AValue: string);
  protected
    procedure ReadFromStream(AHelper: TdxBIFFHyperlinkHelper; AReader: TcxReader); override;
    procedure WriteToStream(AHelper: TdxBIFFHyperlinkHelper; AWriter: TcxWriter); override;
  public
    class procedure Register; override;

    property Delimiter: string read FDelimiter write FDelimiter;
    property Path: string read FPath write FPath;
  end;

  TdxBIFFHyperlinkHelper = class
  strict private
    FFrameText: string;
    FHyperlink: TdxSpreadSheetHyperLink;
    FID: TGUID;
    FLink: string;
    FLinkType: TdxSpreadSheetHyperlinkType;
    FMoniker: TdxHyperlinkMonikerObject;
    FOptions: Integer;
    FSecondID: TGUID;
    FStream: TStream;
    FTime: TDateTime;
    FValue: string;
    FVersion: Integer;
    function GetIsAbsolute: Boolean;
    function GetFlag(AMask: Integer): Boolean;
    procedure SetFrameText(const AValue: string);
    procedure SetFlag(AMask: Integer; AValue: Boolean);
    procedure SetIsAbsolute(AValue: Boolean);
    procedure SetTime(AValue: TDateTime);
  protected
    procedure DoLoadFromStream(AReader: TcxReader); virtual;
    procedure DoSaveToStream(AWriter: TcxWriter); virtual;

    function IsZeroID(const AValue: TGUID): Boolean;
    function ReadHTMLString(AReader: TcxReader): string; overload;
    function ReadHTMLString(AReader: TcxReader; ASize: Integer; APosition: Integer = -1): string; overload;
    function ReadGUID: TGUID;
    function ReadNullTerminatedAnsiString(AReader: TcxReader): AnsiString;
    function ReadNullTerminatedString(AReader: TcxReader): string;

    procedure WriteGUID(const AValue: TGUID);
    procedure WriteHTMLString(const AValue: string);
    procedure WriteNullTerminatedString(const AValue: string);

    property Flags[AMask: Integer]: Boolean read GetFlag write SetFlag;
    property ID: TGUID read FID;
    property Options: Integer read FOptions;
    property Stream: TStream read FStream;
  public
    constructor Create(AHyperlink: TdxSpreadSheetHyperLink); virtual;
    destructor Destroy; override;

    procedure SaveToStream(AStream: TStream);
    procedure LoadFromStream(AStream: TStream);

    property FrameText: string read FFrameText write SetFrameText;
    property Link: string read FLink;
    property LinkType: TdxSpreadSheetHyperlinkType read FLinkType write FLinkType;
    property IsAbsolute: Boolean read GetIsAbsolute write SetIsAbsolute;
    property Version: Integer read FVersion write FVersion;
    property SecondID: TGUID read FSecondID write FSecondID;
    property Hyperlink: TdxSpreadSheetHyperLink read FHyperlink;
    // url moniker
    property Moniker: TdxHyperlinkMonikerObject read FMoniker;
    property Time: TDateTime read FTime write SetTime;
    property Value: string read FValue;
  end;

const

  { Biff records constants }
  brc1904                    =   $0022;              // 1904 Date System
  brcADDIN                   =   $0087;              // Workbook Is an Add-in Macro
  brcADDMENU                 =   $00C2;              // Menu Addition
  brcARRAY                   =   $0221;              // Array-Entered Formula
  brcAUTOFILTER              =   $009E;              // AutoFilter Data
  brcAUTOFILTERINFO          =   $009D;              // Drop-Down Arrow Count
  brcBACKUP                  =   $0040;              // Save Backup Version of the File
  brcBLANK                   =   $0201;              // Cell Value, Blank Cell
  brcBOF                     =   $0809;              // Beginning of File
  brcBOOKBOOL                =   $00DA;              // Workbook Option Flag
  brcBOOLERR                 =   $0205;              // Cell Value, Boolean or Error
  brcBOTTOMMARGIN            =   $0029;              // Bottom Margin Measurement
  brcBOUNDSHEET              =   $0085;              // Sheet Information
  brcCALCCOUNT               =   $000C;              // Iteration Count
  brcCALCMODE                =   $000D;              // Calculation Mode
  brcCF                      =   $01B1;              // Conditional Formatting Conditions
  brcCFEX                    =   $087B;              // Conditional Formatting Extension
  brcCODENAME                =   $0042;              // VBE Object Name
  brcCODEPAGE                =   $0142;              // Default Code Page
  brcCOLINFO                 =   $007D;              // Column Formatting Information
  brcCONDFMT                 =   $01B0;              // Conditional Formatting Range Information
  brcCONTINUE                =   $003C;              // Continues Long Records
  brcCONTINUEFRT             =   $0812;              // Continued FRT
  brcCOORDLIST               =   $00A9;              // Polygon Object Vertex Coordinates
  brcCOUNTRY                 =   $008C;              // Default Country and WIN.INI Country
  brcCRN                     =   $005A;              // Nonresident Operands
  brcDBCELL                  =   $00D7;              // Stream Offsets
  brcDBQUERYEXT              =   $0803;              // Database Query Extensions
  brcDCON                    =   $0050;              // Data Consolidation Information
  brcDCONBIN                 =   $01B5;              // Data Consolidation Information
  brcDCONNAME                =   $0052;              // Data Consolidation Named References
  brcDCONREF                 =   $0051;              // Data Consolidation References
  brcDEFAULTROWHEIGHT        =   $0225;              // Default Row Height
  brcDEFCOLWIDTH             =   $0055;              // Default Width for Columns
  brcDELMENU                 =   $00C3;              // Menu Deletion
  brcDELTA                   =   $0010;              // Iteration Increment
  brcDIMENSIONS              =   $0200;              // Cell Table Size
  brcDOCROUTE                =   $00B8;              // Routing Slip Information
  brcDSF                     =   $0161;              // Double Stream File
  brcDV                      =   $01BE;              // Data Validation Criteria
  brcDVAL                    =   $01B2;              // Data Validation Information
  brcEDG                     =   $0088;              // Edition Globals
  brcEOF                     =   $000A;              // End of File
  brcEXCEL9FILE              =   $01C0;              // Excel 9 File
  brcEXTERNCOUNT             =   $0016;              // Number of External References
  brcEXTERNNAME              =   $0023;              // Externally Referenced Name
  brcEXTERNSHEET             =   $0017;              // External Reference
  brcEXTSST                  =   $00FF;              // Extended Shared String Table
  brcEXTSTRING               =   $0804;              // FRT String
  brcFILEPASS                =   $002F;              // File Is Password-Protected
  brcFILESHARING             =   $005B;              // File-Sharing Information
  brcFILESHARING2            =   $01A5;              // File-Sharing Information for Shared Lists
  brcFILTERMODE              =   $009B;              // Sheet Contains Filtered List
  brcFNGROUPCOUNT            =   $009C;              // Built-in Function Group Count
  brcFNGROUPNAME             =   $009A;              // Function Group Name
  brcFEATHEADR               =   $0867;              // Shared Feature Header
  brcFONT                    =   $0031;              // Font Description
  brcFOOTER                  =   $0015;              // Print Footer on Each Page
  brcFORMAT                  =   $041E;              // Number Format
  brcFORMULA                 =   $0006;              // Cell Formula
  brcGCW                     =   $00Ab;              // Global Column-Width Flags
  brcGRIDSET                 =   $0082;              // State Change of GridLines Option
  brcGUTS                    =   $0080;              // Size of Row and Column Gutters
  brcHCENTER                 =   $0083;              // Center Between Horizontal Margins
  brcHEADER                  =   $0014;              // Print Header on Each Page
  brcHIDEOBJ                 =   $008D;              // Object Display Options
  brcHLINK                   =   $01B8;              // Hyperlink
  brcHLINKTOOLTIP            =   $0800;              // Hyperlink Tooltip
  brcHORIZONTALPAGEBREAKS    =   $001B;              // Explicit Row Page Breaks
  brcIMDATA                  =   $007F;              // Image Data
  brcINDEX                   =   $020B;              // Index Record
  brcINTERFACEEND            =   $00E2;              // End of User Interface Records
  brcINTERFACEHDR            =   $00E1;              // Beginning of User Interface Records
  brcITERATION               =   $0011;              // Iteration Mode
  brcLABEL                   =   $0204;              // Cell Value, String Constant
  brcLABELSST                =   $00FD;              // Cell Value, String Constant/SST
  brcLEFTMARGIN              =   $0026;              // Left Margin Measurement
  brcLHNGRAPH                =   $0095;              // Named Graph Information
  brcLHRECORD                =   $0094;              // .WK? File Conversion Information
  brcLPR                     =   $0098;              // Sheet Was Printed Using LINE.PRINT(
  brcMERGECELLS              =   $00E5;              // Merged Cells
  brcMMS                     =   $00C1;              // ADDMENU/DELMENU Record Group Count
  brcMSODRAWINGGROUP         =   $00EB;              // Microsoft Office Drawing Group
  brcMSODRAWING              =   $00EC;              // Microsoft Office Drawing
  brcMSODRAWINGSELECTION     =   $00ED;              // Microsoft Office Drawing Selection
  brcMULBLANK                =   $00BE;              // Multiple Blank Cells
  brcMULRK                   =   $00BD;              // Multiple RK Cells
  brcNAME                    =   $0018;              // Defined Name
  brcNOTE                    =   $001C;              // Comment Associated with a Cell
  brcNUMBER                  =   $0203;              // Cell Value, Floating-Point Number
  brcOBJ                     =   $005D;              // Describes a Graphic Object
  brcOBJPROTECT              =   $0063;              // Objects Are Protected
  brcOBPROJ                  =   $00D3;              // Visual Basic Project
  brcOLEDBCONN               =   $080A;              // OLE Database Connection
  brcOLESIZE                 =   $00DE;              // Size of OLE Object
  brcPALETTE                 =   $0092;              // Color Palette Definition
  brcPANE                    =   $0041;              // Number of Panes and Their Position
  brcPARAMQRY                =   $00DC;              // Query Parameters
  brcPASSWORD                =   $0013;              // Protection Password
  brcPLS                     =   $004D;              // Environment-Specific Print Record
  brcPRECISION               =   $000E;              // Precision
  brcPRINTGridLines          =   $002B;              // Print GridLines Flag
  brcPRINTHEADERS            =   $002A;              // Print Row/Column Labels
  brcPROTECT                 =   $0012;              // Protection Flag
  brcPROT4REV                =   $01AF;              // Shared Workbook Protection Flag
  brcQSI                     =   $01AD;              // External Data Range
  brcQSIF                    =   $0807;              // Query Table Field Formatting
  brcQSIR                    =   $0806;              // Query Table Formatting
  brcQSISXTAG                =   $0802;              // PivotTable and Query Table Extensions
  brcRECALCID                =   $01C1;              // Recalc Information
  brcRECIPNAME               =   $00B9;              // Recipient Name
  brcREFMODE                 =   $000F;              // Reference Mode
  brcREFRESHALL              =   $01B7;              // Refresh Flag
  brcRIGHTMARGIN             =   $0027;              // Right Margin Measurement
  brcRK                      =   $027E;              // Cell Value, RK Number
  brcROW                     =   $0208;              // Describes a Row
  brcRSTRING                 =   $00D6;              // Cell with Character Formatting
  brcSAVERECALC              =   $005F;              // Recalculate Before Save
  brcSCENARIO                =   $00AF;              // Scenario Data
  brcSCENMAN                 =   $00AE;              // Scenario Output Data
  brcSCENPROTECT             =   $00DD;              // Scenario Protection
  brcSCL                     =   $00A0;              // Window Zoom Magnification
  brcSELECTION               =   $001D;              // Current Selection
  brcSETUP                   =   $00A1;              // Page Setup
  brcSHRFMLA                 =   $04BC;              // Shared Formula
  brcSORT                    =   $0090;              // Sorting Options
  brcSOUND                   =   $0096;              // Sound Note
  brcSST                     =   $00FC;              // Shared String Table
  brcSTANDARDWIDTH           =   $0099;              // Standard Column Width
  brcSTRING                  =   $0207;              // String Value of a Formula
  brcSTYLE                   =   $0293;              // Style Information
  brcSUB                     =   $0091;              // Subscriber
  brcSUPBOOK                 =   $01AE;              // Supporting Workbook
  brcSXDB                    =   $00C6;              // PivotTable Cache Data
  brcSXDBEX                  =   $0122;              // PivotTable Cache Data
  brcSXDI                    =   $00C5;              // Data Item
  brcSXEX                    =   $00F1;              // PivotTable View Extended Information
//  brcSXEXT                   =   $00DC;              // External Source Information
  brcSXFDBTYPE               =   $01BB;              // SQL DataType Identifier
  brcSXFILT                  =   $00F2;              // PivotTable Rule Filter
  brcSXFORMAT                =   $00FB;              // PivotTable Format Record
  brcSXFORMULA               =   $0103;              // PivotTable Formula Record
  brcSXFMLA                  =   $00F9;              // PivotTable Parsed Expression
  brcSXIDSTM                 =   $00D5;              // Stream ID
  brcSXIVD                   =   $00B4;              // Row/Column Field Ids
  brcSXLI                    =   $00B5;              // Line Item Array
  brcSXNAME                  =   $00F6;              // PivotTable Name
  brcSXPAIR                  =   $00F8;              // PivotTable Name Pair
  brcSXPI                    =   $00B6;              // Page Item
  brcSXPIEX                  =   $080E;              // OLAP Page Item Extensions
  brcSXRULE                  =   $00F0;              // PivotTable Rule Data
  brcSXSTRING                =   $00CD;              // String
  brcSXSELECT                =   $00F7;              // PivotTable Selection Information
  brcSXTBL                   =   $00D0;              // Multiple Consolidation Source Info
  brcSXTBPG                  =   $00D2;              // Page Item Indexes
  brcSXTBRGIITM              =   $00D1;              // Page Item Name Count
  brcSXTH                    =   $080D;              // PivotTable OLAP Hierarchy
  brcSXVD                    =   $00B1;              // View Fields
  brcSXVDEX                  =   $0100;              // Extended PivotTable View Fields
  brcSXVDTEX                 =   $080F;              // View Dimension OLAP Extensions
  brcSXVI                    =   $00B2;              // View Item
  brcSXVIEW                  =   $00B0;              // View Definition
  brcSXVIEWEX                =   $080C;              // Pivot Table OLAP Extensions
  brcSXVIEWEX9               =   $0810;              // Pivot Table Extensions
  brcSXVS                    =   $00E3;              // View Source
  brcTABID                   =   $013D;              // Sheet Tab Index Array
  brcTABIDCONF               =   $00EA;              // Sheet Tab ID of Conflict History
  brcTABLE                   =   $0236;              // Data Table
  brcTEMPLATE                =   $0060;              // Workbook Is a Template
  brcTOPMARGIN               =   $0028;              // Top Margin Measurement
  brcTXO                     =   $01B6;              // Text Object
  brcTheme                   =   $0896;              // Information about the current theme
  brcUDDESC                  =   $00DF;              // Description String for Chart AutoFormat
  brcUNCALCED                =   $005E;              // Recalculation Status
  brcUSERBVIEW               =   $01A9;              // Workbook Custom View Settings
  brcUSERSVIEWBEGIN          =   $01AA;              // Custom View Settings
  brcUSERSVIEWEND            =   $01Ab;              // End of Custom View Records
  brcUSESELFS                =   $0160;              // Natural Language Formulas Flag
  brcVCENTER                 =   $0084;              // Center Between Vertical Margins
  brcVERTICALPAGEBREAKS      =   $001A;              // Explicit Column Page Breaks
  brcWINDOW1                 =   $003D;              // Window Information
  brcWINDOW2                 =   $023E;              // Sheet Window Information
  brcWINDOWPROTECT           =   $0019;              // Windows Are Protected
  brcWOPT                    =   $080B;              // Web Options
  brcWRITEACCESS             =   $005C;              // Write Access User Name
  brcWRITEPROT               =   $0086;              // Workbook Is Write-Protected
  brcWSBOOL                  =   $0081;              // Additional Workspace Information
  brcXCT                     =   $0059;              // CRN Record Count
  brcXF                      =   $00E0;              // Extended Format
  brcXFCRC                   =   $087C;              // XF Extensions Checksum
  brcXFEXT                   =   $087D;              // XF Extension
  brcXL5MODIFY               =   $0162;              // Flag for DSF

  // microsoft excel formula tokens for parsed expression
  ptgExp       = $01;
  ptgTbl       = $02;
  ptgAdd       = $03;
  ptgSub       = $04;
  ptgMul       = $05;
  ptgDiv       = $06;
  ptgPower     = $07;
  ptgConcat    = $08;
  ptgLT        = $09;
  ptgLE        = $0A;
  ptgEQ        = $0B;
  ptgGE        = $0C;
  ptgGT        = $0D;
  ptgNE        = $0E;
  ptgIsect     = $0F;
  ptgUnion     = $10;
  ptgRange     = $11;
  ptgUplus     = $12;
  ptgUminus    = $13;
  ptgPercent   = $14;
  ptgParen     = $15;
  ptgMissArg   = $16;
  ptgStr       = $17;
  ptgExtend    = $18;
  ptgAttr      = $19;
  ptgSheet     = $1A;
  ptgEndSheet  = $1B;
  ptgErr       = $1C;
  ptgBool      = $1D;
  ptgInt       = $1E;
  ptgNum       = $1F;
  ptgArray     = $20;
  ptgFunc      = $21;
  ptgFuncVar   = $22;
  ptgName      = $23;
  ptgRef       = $24;
  ptgArea      = $25;
  ptgMemArea   = $26;
  ptgMemErr    = $27;
  ptgMemNoMem  = $28;
  ptgMemFunc   = $29;
  ptgRefErr    = $2A;
  ptgAreaErr   = $2B;
  ptgRefN      = $2C;
  ptgAreaN     = $2D;
  ptgMemAreaN  = $2E;
  ptgMemNoMemN = $2F;

  ptgNameX     = $39;
  ptgRef3d     = $3A;
  ptgArea3d    = $3B;
  ptgRefErr3d  = $3C;
  ptgAreaErr3d = $3D;

  ptgArrayV    = $40;
  ptgFuncV     = $41;
  ptgFuncVarV  = $42;
  ptgNameV     = $43;
  ptgRefV      = $44;
  ptgAreaV     = $45;
  ptgMemAreaV  = $46;
  ptgMemErrV   = $47;
  ptgMemNoMemV = $48;
  ptgMemFuncV  = $49;
  ptgRefErrV   = $4A;
  ptgAreaErrV  = $4B;
  ptgRefNV     = $4C;
  ptgAreaNV    = $4D;
  ptgMemAreaNV = $4E;
  ptgMemNoMemNV= $4F;

  ptgFuncCEV   = $58;
  ptgNameXV    = $59;
  ptgRef3dV    = $5A;
  ptgArea3dV   = $5B;
  ptgRefErr3dV = $5C;
  ptgAreaErr3dV= $5D;

  ptgArrayA    = $60;
  ptgFuncA     = $61;
  ptgFuncVarA  = $62;
  ptgNameA     = $63;
  ptgRefA      = $64;
  ptgAreaA     = $65;
  ptgMemAreaA  = $66;
  ptgMemErrA   = $67;
  ptgMemNoMemA = $68;
  ptgMemFuncA  = $69;
  ptgRefErrA   = $6A;
  ptgAreaErrA  = $6B;
  ptgRefNA     = $6C;
  ptgAreaNA    = $6D;
  ptgMemAreaNA = $6E;
  ptgMemNoMemNA= $6F;

  ptgFuncCEA   = $78;
  ptgNameXA    = $79;
  ptgRef3dA    = $7A;
  ptgArea3dA   = $7B;
  ptgRefErr3dA = $7C;
  ptgAreaErr3dA= $7D;

const
  HyperlinkVersion          = 2;
  hlstmfHasMoniker          = $0001;  // 0 = No link extant     1 = File link or URL
  hlstmfIsAbsolute          = $0002;  // 0 = Relative file path 1 = Absolute path or URL
  hlstmfSiteGaveDisplayName = $0004;
  hlstmfHasLocationStr      = $0008;  // 0 = No text mark 1 = Text mark
  hlstmfHasDisplayName      = $0010;  //
  hlstmfHasGUID             = $0020;
  hlstmfHasCreationTime     = $0040;
  hlstmfHasFrameName        = $0080;  // 0 = No target frame  1 = Target frame
  hlstmfMonikerSavedAsStr   = $0100;  // 0 = File link or URL 1 = UNC path (incl. server name)
  hlstmfAbsFromGetDataRel   = $0200;

  STDLink: TGUID = '{79EAC9D0-BAF9-11CE-8C82-00AA004BA90B}';
  URLMoniker:  TGUID = '{79EAC9E0-BAF9-11CE-8C82-00AA004BA90B}';
  FileMoniker: TGUID = '{00000303-0000-0000-C000-000000000046}';
  ItemMoniker: TGUID = '{00000304-0000-0000-C000-000000000046}';
  AntiMoniker: TGUID = '{00000305-0000-0000-C000-000000000046}';
  CompositeMoniker: TGUID = '{00000309-0000-0000-C000-000000000046}';

  DefaultURLFlags: TdxSpreadSheetURICreationFlags = [createAllowRelative, createAllowImplicitFileScheme, createCanonicalize,
                                                    createDecodeExtraInfo, createNoDecodeExtraInfo, createCrackUnknownSchemes,
                                                    createPreProcessHtmlUri, createIESettings, createNoEncodeForbiddenCharacters];

{  DefaultFileFlags: TdxSpreadSheetURICreationFlag = [createAllowRelative, createAllowImplicitFileScheme, createCanonicalize,
                                                    createDecodeExtraInfo, createNoDecodeExtraInfo, createCrackUnknownSchemes,
                                                    createPreProcessHtmlUri, createIESettings, createNoEncodeForbiddenCharacters]}

const
  iprotObject           = $00000001;
  iprotScenario         = $00000002;
  iprotFormatCells      = $00000004;
  iprotFormatColumns    = $00000008;
  iprotFormatRows       = $00000010;
  iprotInsertColumns    = $00000020;
  iprotInsertRows       = $00000040;
  iprotInsertHyperlinks = $00000080;
  iprotDeleteColumns    = $00000100;
  iprotDeleteRows       = $00000200;
  iprotSelLockedCells   = $00000400;
  iprotSort             = $00000800;
  iprotAutoFilter       = $00001000;
  iprotPivotTables      = $00002000;
  iprotSelUnlockedCells = $00004000;

const
  CFRULE_OPTION_ALIGN = $08000000;
  CFRULE_OPTION_BORDER = $10000000;
  CFRULE_OPTION_BORDER_BLTR = $00008000;
  CFRULE_OPTION_BORDER_BOTTOM = $00002000;
  CFRULE_OPTION_BORDER_LEFT = $00000400;
  CFRULE_OPTION_BORDER_RIGHT = $00000800;
  CFRULE_OPTION_BORDER_TLBR = $00004000;
  CFRULE_OPTION_BORDER_TOP = $00001000;
  CFRULE_OPTION_FONT = $04000000;
  CFRULE_OPTION_NUMBERFORMATTING = $2000000;
  CFRULE_OPTION_PATTERN = $20000000;
  CFRULE_OPTION_PATTERN_BGCOLOR = $00040000;
  CFRULE_OPTION_PATTERN_FGCOLOR = $00020000;
  CFRULE_OPTION_PATTERN_STYLE = $00010000;
  CFRULE_OPTION_PROTECTION = $40000000;

  CFRULE_FONT_FLAG_STYLE    = $00000002;
  CFRULE_FONT_FLAG_OUTLINE  = $00000008;
  CFRULE_FONT_FLAG_SHADOW   = $00000010;
  CFRULE_FONT_FLAG_CONDENSE = $00000020;
  CFRULE_FONT_FLAG_CANCELLATION = $00000080;

  ftCmo = $15;

  BorderLineStyleMap: array[TdxSpreadSheetCellBorderStyle] of Byte = (
    $0, $7, $4, $B, $9, $3, $1, $C, $D, $A, $8, $2, $5, $6, $0
  );
  FillStyleMap: array[TdxSpreadSheetCellFillStyle] of Byte = (
    $01, $03, $02, $04, $11, $12, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $10, $0F
  );

  FillStyles: array[0..17] of Byte = ($01, $03, $02, $04, $11, $12,
    $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $10, $0F);
  DefaultStylesRecordCount = 21;

  dxXLSMaxRecordSize = 8192 - 4;
  dxXLSCountStringsInSSTBucked = 8;

  dxMaxColumn = MAXBYTE;
  dxMaxRow    = MAXWORD;

implementation

uses
  dxSpreadSheetFormatXLSFormulas, dxSpreadSheetFormatXLS, SysUtils, dxSpreadSheetFormulas, dxSpreadSheetFormatXLSReader,
  dxSpreadSheetCoreFormulasParser;

var
  EmptyGUID: TGUID;
  MonikersList: TDictionary<TGUID, TdxHyperlinkMonikerObjectClass>;

{ TdxXLSCellStyle }

constructor TdxXLSCellStyle.Create;
begin
  Borders := cxBordersAll;
  Alignments := 2 shl 4;
  Flags := $1;
end;

destructor TdxXLSCellStyle.Destroy;
begin
  if Handle <> nil then
    Handle.Release;
  FreeAndNil(ExtendedData);
  inherited Destroy;
end;

{ TdxXLSFont }

procedure TdxXLSFont.AssignTo(AFont: TdxSpreadSheetFontHandle);
begin
  AFont.Charset := Charset;
  AFont.Color := Color;
  AFont.Name := Name;
  AFont.Size := Size;
  AFont.Style := Style;
  Byte(AFont.Script) := Script;
end;

{ TdxXFExtPropertyData }

procedure TdxXFExtPropertyData.CheckColors(ATheme: TdxSpreadSheetXLSReaderThemeHelper);
var
  APropID: TdxXFExtPropertyID;
begin
  for APropID := Low(TdxXFExtPropertyID) to High(TdxXFExtPropertyID) do
    if APropID in AssignedThemeValues then
      if not ATheme.GetColor(DataValue[APropID], TintShade[APropID], DataValue[APropID]) then
      begin
        Exclude(AssignedValues, APropID);
        Exclude(AssignedThemeValues, APropID);
      end;
end;

procedure TdxXFExtPropertyData.CorrectStyle(ABrush: TdxSpreadSheetBrushHandle;
  ABorders: TdxSpreadSheetBordersHandle; AStyle: TdxSpreadSheetCellStyleHandle);
begin
  if xfextForeColor in AssignedValues then
    ABrush.ForegroundColor := DataValue[xfextForeColor];
  if xfextBackColor in AssignedValues then
    ABrush.BackgroundColor := DataValue[xfextBackColor];

  if xfextBorderColorTop in AssignedValues then
    ABorders.BorderColor[bTop] := DataValue[xfextBorderColorTop];

  if xfextBorderColorLeft in AssignedValues then
    ABorders.BorderColor[bLeft] := DataValue[xfextBorderColorLeft];

  if xfextBorderColorRight in AssignedValues then
    ABorders.BorderColor[bRight] := DataValue[xfextBorderColorRight];

  if xfextBorderColorBottom in AssignedValues then
    ABorders.BorderColor[bBottom] := DataValue[xfextBorderColorBottom];

  if xfextIndent in AssignedValues then
    AStyle.AlignHorzIndent := DataValue[xfextIndent];
end;

procedure TdxXFExtPropertyData.SetThemeColorValue(
  APropID: TdxXFExtPropertyID; const AColorIndex: Integer; ATintShade: SmallInt);
begin
  SetValue(APropID, AColorIndex);
  Include(AssignedThemeValues, APropID);
  TintShade[APropID] := ATintShade;
end;

procedure TdxXFExtPropertyData.SetValue(APropID: TdxXFExtPropertyID; const AValue: Integer);
begin
  DataValue[APropID] := AValue;
  Include(AssignedValues, APropID);
end;

{ TdxMsoCrc32Compute }

constructor TdxMsoCrc32Compute.Create;
begin
  MsoCrc32Compute;
end;

procedure TdxMsoCrc32Compute.Add(AData: PByte; ACount: Integer);
var
  I: Integer;
begin
  for I := 0 to ACount - 1 do
  begin
    AddByte(AData^);
    Inc(AData);
  end;
end;

procedure TdxMsoCrc32Compute.AddByte(AData: Byte);
var
  AIndex: Cardinal;
begin
  AIndex := crcValue shr 24;
  AIndex := AIndex xor AData;
  crcValue := crcValue shl 8;
  crcValue := crcValue xor crcCache[AIndex];
end;

procedure TdxMsoCrc32Compute.MsoCrc32Compute;
var
  I, J: Integer;
  AValue: Cardinal;
begin
  for I := 0 to 255 do
  begin
    AValue := I shl 24;
    for J := 0 to 7 do
      if (AValue and $80000000) = $80000000 then
      begin
        AValue := AValue shl 1;
        AValue := AValue xor $AF;
      end
      else
        AValue := AValue shl 1;
    AValue := AValue and $FFFF;
    crcCache[i] := AValue;
  end;
end;

{ TdxSpreadSheetXLSReaderThemeHelper }

constructor TdxSpreadSheetXLSReaderThemeHelper.Create;
begin
  FColorMap := TDictionary<TdxXMLString, TColor>.Create;
  FThemedBrushes := TObjectList<TdxGPBrush>.Create;
  FThemedColors := TList.Create;
  FThemedPens := TObjectList<TdxGPPen>.Create;
  InitializeStandardTheme;
end;

destructor TdxSpreadSheetXLSReaderThemeHelper.Destroy;
begin
  FColorMap.Free;
  FThemedBrushes.Free;
  FThemedColors.Free;
  FThemedPens.Free;
  inherited Destroy;
end;

procedure TdxSpreadSheetXLSReaderThemeHelper.Clear;
begin
  FColorMap.Clear;
  FThemedBrushes.Clear;
  FThemedColors.Clear;
  FThemedPens.Clear;
end;

procedure TdxSpreadSheetXLSReaderThemeHelper.Execute;
var
  AChildNode: TdxXMLNode;
begin
  if Node.FindChild([sdxXLSXNodeThemesElements, sdxXLSXNodeThemesColorScheme], AChildNode) then
    ReadColorSchema(AChildNode);
end;

function TdxSpreadSheetXLSReaderThemeHelper.GetColor(AColorIndex: Integer; ATintShade: SmallInt;
  var AColor: Integer): Boolean;
begin
  Result := (AColorIndex >= 0) and (AColorIndex < ThemedColors.Count);
  if not Result then Exit;
  if AColorIndex = 0 then
    AColor := clWindow
  else
    if AColorIndex = 1 then
      AColor := clWindowText
    else
      AColor := Integer(ThemedColors[AColorIndex]);
  if ATintShade <> 0 then
    AColor := TdxSpreadSheetColorHelper.ApplyTint(ColorToRgb(AColor), ATintShade / MAXSHORT)
end;

procedure TdxSpreadSheetXLSReaderThemeHelper.LoadFromStream(AStream: TStream);
begin
  Clear;
  if AStream.Size > AStream.Position then
  begin
    FDocumentFileName := 'XLS\Theme';
    FDocument := ExtractDocument(AStream, 'theme/theme/theme1.xml');
    if (FDocument <> nil) and (FDocument.Root <> nil) then
    begin
      FNode := FDocument.Root.First;
      Execute;
    end;
  end;
  if ThemedColors.Count = 0 then
    InitializeStandardTheme;
end;

function TdxSpreadSheetXLSReaderThemeHelper.ExtractDocument(
  AStream: TStream; const ADocName: AnsiString): TdxXMLDocument;
var
  ADataStream, AXMLStream: TMemoryStream;
  AZIPStream: TdxZIPStreamReader;
begin
  Result := nil;
  ADataStream := TMemoryStream.Create;
  try
    ADataStream.CopyFrom(AStream, AStream.Size - AStream.Position);
    ADataStream.Position := 0;
    AZIPStream := TdxZIPStreamReader.Create(ADataStream);
    try
      AXMLStream := TMemoryStream.Create;
      try
        if AZIPStream.Extract(ADocName, AXMLStream) then
        begin
          AXMLStream.Position := 0;
          Result := TdxXMLDocument.Create(nil);
          Result.LoadFromStream(AXMLStream);
        end;
      finally
        AXMLStream.Free;
      end;
    finally
      AZIPStream.Free;
    end;
  finally
    ADataStream.Free;
  end;
end;

function TdxSpreadSheetXLSReaderThemeHelper.GetColorMap: TDictionary<TdxXMLString, TColor>;
begin
  Result := FColorMap;
end;

function TdxSpreadSheetXLSReaderThemeHelper.GetThemedBrushes: TObjectList<TdxGPBrush>;
begin
  Result := FThemedBrushes;
end;

function TdxSpreadSheetXLSReaderThemeHelper.GetThemedColors: TList;
begin
  Result := FThemedColors;
end;

function TdxSpreadSheetXLSReaderThemeHelper.GetThemedPens: TObjectList<TdxGPPen>;
begin
  Result := FThemedPens;
end;

procedure TdxSpreadSheetXLSReaderThemeHelper.InitializeStandardTheme;
var
  I: Integer;
const
  StandardColors: array[0..11] of TColor =
    (0, 0, $E1ECEE, $7D491F, $BD814F, $4D50C0, $59BB9B, $A26480, $C6AC4B, $4696F7, $FF0000, $800080);
begin
  Clear;
  StandardColors[0] := clWindow;
  StandardColors[1] := clWindowText;
  for I := 0 to Length(StandardColors) - 1 do
    FThemedColors.Add(Pointer(StandardColors[I]));
end;

{ TdxXLSNameDefinition }

constructor TdxXLSNameDefinition.Create(AName: TdxSpreadSheetDefinedName; ASize: Integer; AData: Pointer);
begin
  FName := AName;
  FSize := ASize;
  GetMem(FData,  ASize);
  CopyMemory(FData, AData, ASize);
end;

destructor TdxXLSNameDefinition.Destroy;
begin
  FreeMem(FData);
  inherited;
end;

procedure TdxXLSNameDefinition.Restore(AReader: TObject);
var
  AFormulasReader: TdxXLSFormulaReader;
begin
  AFormulasReader := TdxXLSFormulaReader.Create(TdxSpreadSheetXLSReader(AReader));
  try
    AFormulasReader.ReadName(FName, FSize, FData);
  finally
    AFormulasReader.Free;
  end;
end;

{ TdxBIFFHyperlinkHelper }

constructor TdxBIFFHyperlinkHelper.Create(AHyperlink: TdxSpreadSheetHyperLink);
begin
  FHyperlink := AHyperlink;
end;

destructor TdxBIFFHyperlinkHelper.Destroy;
begin
  FreeAndNil(FMoniker);
  inherited Destroy;
end;

procedure TdxBIFFHyperlinkHelper.LoadFromStream(AStream: TStream);
var
  AReader: TcxReader;
begin
  FStream := AStream;
  AReader := TcxReader.Create(AStream);
  try
    DoLoadFromStream(AReader);
  finally
    FStream := nil;
    AReader.Free;
  end;
end;

procedure TdxBIFFHyperlinkHelper.SaveToStream(AStream: TStream);
var
  AWriter: TcxWriter;
begin
  FStream := AStream;
  AWriter := TcxWriter.Create(AStream);
  try
    DoSaveToStream(AWriter);
  finally
    FStream := nil;
    AWriter.Free;
  end;
end;

procedure TdxBIFFHyperlinkHelper.DoLoadFromStream(AReader: TcxReader);
var
  AMonikerClass: TdxHyperlinkMonikerObjectClass;
begin
  FID := ReadGUID;
  FVersion := AReader.ReadInteger;
  FOptions := AReader.ReadInteger;

  if Flags[hlstmfHasDisplayName] then
    Hyperlink.DisplayText := ReadHTMLString(AReader);

  if Flags[hlstmfHasFrameName] then
    FFrameText := ReadHTMLString(AReader);

  if Flags[hlstmfHasMoniker] then
  begin
    if Flags[hlstmfMonikerSavedAsStr] then
      FFrameText := ReadHTMLString(AReader)
    else
    begin
      if MonikersList.TryGetValue(ReadGUID, AMonikerClass) then
      begin
        FMoniker := AMonikerClass.Create;
        FMoniker.ReadFromStream(Self, AReader);
        FValue := FMoniker.GetValue;
      end;
    end;
  end;
  if Flags[hlstmfHasLocationStr] then
    FValue := dxSpreadSheetFormulaIncludeEqualSymbol(ReadHTMLString(AReader));
  if Flags[hlstmfHasGUID] then
    FSecondID := ReadGUID;
  if Flags[hlstmfHasCreationTime] then
    FTime := AReader.ReadDateTime;
end;

procedure TdxBIFFHyperlinkHelper.DoSaveToStream(AWriter: TcxWriter);
var
  AClassID: TGUID;
  AMonikerClass: TdxHyperlinkMonikerObjectClass;
begin
  WriteGUID(STDLink);
  AWriter.WriteInteger(HyperlinkVersion);
  //
  Flags[hlstmfHasDisplayName] := havDisplayText in Hyperlink.AssignedValues;
  Flags[hlstmfSiteGaveDisplayName] := Flags[hlstmfHasDisplayName];
  Flags[hlstmfHasLocationStr] := Hyperlink.ValueType = hvtReference;
  Flags[hlstmfHasMoniker] := Hyperlink.ValueType <> hvtReference;
  AWriter.WriteInteger(Options);
  if havDisplayText in Hyperlink.AssignedValues then
    WriteHTMLString(Hyperlink.DisplayText);
  //
  AClassID := EmptyGUID;
  AMonikerClass := nil;
  if Hyperlink.ValueType = hvtReference then
    WriteHTMLString(dxSpreadSheetFormulaExcludeEqualSymbol(Hyperlink.Value))
  else
    if (Hyperlink.ValueType = hvtEMail) or (Pos('http:', Hyperlink.Value) = 1) then
    begin
      AClassID := URLMoniker;
      AMonikerClass := TdxURLMoniker;
    end
    else
      if Hyperlink.ValueType <> hvtReference then
      begin
        AClassID := FileMoniker;
        AMonikerClass := TdxFileMoniker;
      end;
  if AMonikerClass <> nil then
  begin
    FMoniker := AMonikerClass.Create;
    WriteGUID(AClassID);
    FMoniker.Initialize(Hyperlink);
    FMoniker.WriteToStream(Self, AWriter);
  end
end;

function TdxBIFFHyperlinkHelper.IsZeroID(const AValue: TGUID): Boolean;
begin
  Result := IsEqualGUID(AValue, EmptyGUID);
end;

function TdxBIFFHyperlinkHelper.ReadHTMLString(AReader: TcxReader): string;
begin
  Result := ReadHTMLString(AReader, AReader.ReadInteger);
end;

function TdxBIFFHyperlinkHelper.ReadHTMLString(AReader: TcxReader; ASize: Integer; APosition: Integer = -1): string;
begin
  SetLength(Result, ASize);
  if APosition <> -1 then
    Stream.Position := APosition;
  Stream.ReadBuffer(Result[1], ASize * 2);
  if ASize > 0 then // remove #0 char from last position
    SetLength(Result, ASize - 1);
end;

function TdxBIFFHyperlinkHelper.ReadGUID: TGUID;
begin
  Stream.ReadBuffer(Result, SizeOf(Result));
end;

function TdxBIFFHyperlinkHelper.ReadNullTerminatedAnsiString(AReader: TcxReader): AnsiString;
var
  AChar: AnsiChar;
begin
  Result := '';
  repeat
    AChar := AnsiChar(AReader.ReadByte);
    if AChar <> #0 then
      Result := Result + AChar;
  until AChar = #0;
end;

function TdxBIFFHyperlinkHelper.ReadNullTerminatedString(AReader: TcxReader): string;
var
  AChar: Char;
begin
  Result := '';
  repeat
    AChar := AReader.ReadChar;
    if AChar <> #0 then
      Result := Result + AChar;
  until AChar = #0;
end;

procedure TdxBIFFHyperlinkHelper.WriteNullTerminatedString(const AValue: string);
begin
  if Length(AValue) > 0 then
    Stream.WriteBuffer(AValue[1], (Length(AValue) + 1) * 2);
end;

procedure TdxBIFFHyperlinkHelper.WriteGUID(const AValue: TGUID);
begin
  Stream.WriteBuffer(AValue, SizeOf(TGUID));
end;

procedure TdxBIFFHyperlinkHelper.WriteHTMLString(const AValue: string);
begin
  WriteIntegerProc(Stream, Length(AValue) + 1);
  Stream.WriteBuffer(AValue[1], (Length(AValue) + 1) * 2);
end;

function TdxBIFFHyperlinkHelper.GetIsAbsolute: Boolean;
begin
  Result := Flags[hlstmfIsAbsolute]
end;

function TdxBIFFHyperlinkHelper.GetFlag(AMask: Integer): Boolean;
begin
  Result := FOptions and AMask = AMask;
end;

procedure TdxBIFFHyperlinkHelper.SetFrameText(const AValue: string);
begin
  FFrameText := AValue;
  Flags[hlstmfHasFrameName] := FFrameText <> '';
end;

procedure TdxBIFFHyperlinkHelper.SetFlag(AMask: Integer; AValue: Boolean);
begin
  if AValue then
    FOptions := FOptions or AMask
  else
    FOptions := FOptions and not AMask;
end;

procedure TdxBIFFHyperlinkHelper.SetIsAbsolute(AValue: Boolean);
begin
  Flags[hlstmfIsAbsolute] := AValue;
end;

procedure TdxBIFFHyperlinkHelper.SetTime(AValue: TDateTime);
begin
  FTime := AValue;
  Flags[hlstmfHasCreationTime] := FTime <> 0;
end;

{ TdxHyperlinkMonikerObject }

class procedure TdxHyperlinkMonikerObject.Register;
begin
  MonikersList.Add(EmptyGUID, Self);
end;

function TdxHyperlinkMonikerObject.GetValue: string;
begin
  Result := '';
end;

procedure TdxHyperlinkMonikerObject.Initialize(AHyperlink: TdxSpreadSheetHyperlink);
begin
end;

procedure TdxHyperlinkMonikerObject.ReadFromStream(AHelper: TdxBIFFHyperlinkHelper; AReader: TcxReader);
begin
end;

procedure TdxHyperlinkMonikerObject.WriteToStream(AHelper: TdxBIFFHyperlinkHelper; AWriter: TcxWriter);
begin
end;

{ TdxURLMoniker }

class procedure TdxURLMoniker.Register;
begin
  MonikersList.Add(URLMoniker, Self);
end;

function TdxURLMoniker.GetValue: string;
begin
  Result := URL;
end;

procedure TdxURLMoniker.Initialize(AHyperlink: TdxSpreadSheetHyperlink);
const
  SerialGUID: TGUID = '{F4815879-1D3B-487F-AF2C-825DC4852763}';
begin
  URL := AHyperlink.Value;
  GUID := SerialGUID;
  FURIFlags := DefaultURLFlags;
end;

procedure TdxURLMoniker.ReadFromStream(AHelper: TdxBIFFHyperlinkHelper; AReader: TcxReader);
var
  ASize: Integer;
begin
  ASize  := AReader.ReadInteger;
  FURL := AHelper.ReadNullTerminatedString(AReader);
  if ASize - (Length(FURL) + 1) * 2 = 24 then
  begin
    GUID := AHelper.ReadGUID;
    Version := AReader.ReadInteger;
    FURIFlags := TdxSpreadSheetURICreationFlags(Word(AReader.ReadInteger));
  end;
end;

procedure TdxURLMoniker.WriteToStream(AHelper: TdxBIFFHyperlinkHelper; AWriter: TcxWriter);
begin
  AWriter.WriteInteger((Length(URL) + 1) * 2 + 24);
  AHelper.WriteNullTerminatedString(URL);
  AHelper.WriteGUID(GUID);
  AWriter.WriteInteger(0);
  AWriter.WriteInteger(Word(FURIFlags));
(*
SerialGUID (16 bytes): An optional GUID as specified by [MS-DTYP] for this implementation of
the URL moniker serialization. This field MUST equal {0xF4815879, 0x1D3B, 0x487F, 0xAF,
0x2C, 0x82, 0x5D, 0xC4, 0x85, 0x27, 0x63} if present.
*)
end;

{ TdxFileMoniker }

class procedure TdxFileMoniker.Register;
begin
  MonikersList.Add(FileMoniker, Self);
end;

function TdxFileMoniker.GetValue: string;
var
  I: Integer;
begin
  Result := inherited GetValue;
  for I := 0 to ParentCount - 1 do
    Result := '..\' + Result;
end;

procedure TdxFileMoniker.Initialize(AHyperlink: TdxSpreadSheetHyperlink);
begin
  inherited Initialize(AHyperlink);
  GUID := FileMoniker;
end;

procedure TdxFileMoniker.ReadFromStream(AHelper: TdxBIFFHyperlinkHelper; AReader: TcxReader);
var
  ALen: Integer;
  APath: AnsiString;
begin
  FParentCount := AReader.ReadWord;
  ALen := AReader.ReadInteger;
  SetLength(APath, ALen - 1);
  AHelper.Stream.ReadBuffer(APath[1], ALen);
  URL := dxAnsiStringToString(APath);
  FEndServer := AReader.ReadWord;
  Version  := AReader.ReadWord;
  GUID := AHelper.ReadGUID;       // reserved 16, MUST be zero and MUST be ignored.
  AReader.ReadInteger;            // reserved 4,  MUST be zero and MUST be ignored.
  if AReader.ReadInteger > 0 then // unicode part
  begin
    ALen := AReader.ReadInteger;
    AReader.ReadWord;
    SetLength(APath, ALen);
    AHelper.Stream.ReadBuffer(APath[1], ALen);
  end;
end;

procedure TdxFileMoniker.WriteToStream(AHelper: TdxBIFFHyperlinkHelper; AWriter: TcxWriter);
var
  S: AnsiString;
begin
  AWriter.WriteWord(0);
  S := dxStringToAnsiString(URL);
  AWriter.WriteInteger(Length(S) + 1);
  AWriter.Stream.WriteBuffer(S[1], Length(S) + 1);
  AWriter.WriteWord($FFFF);
  AWriter.WriteWord($DEAD);
  AHelper.WriteGuid(EmptyGUID);
  AWriter.WriteInteger(0);
  AWriter.WriteInteger(0);
end;

{ TdxItemMoniker }

class procedure TdxItemMoniker.Register;
begin
  MonikersList.Add(ItemMoniker, Self);
end;

procedure TdxItemMoniker.ReadFromStream(AHelper: TdxBIFFHyperlinkHelper; AReader: TcxReader);
begin
  FDelimiter := GetMonikerPart(AHelper, AReader);
  FPath := GetMonikerPart(AHelper, AReader);
end;

procedure TdxItemMoniker.WriteToStream(AHelper: TdxBIFFHyperlinkHelper; AWriter: TcxWriter);
begin
  WriteMonikerPart(AHelper, AWriter, FDelimiter);
  WriteMonikerPart(AHelper, AWriter, FPath);
end;

function TdxItemMoniker.GetMonikerPart(AHelper: TdxBIFFHyperlinkHelper; AReader: TcxReader): string;
var
  L: Integer;
  AStr: AnsiString;
begin
  L := AReader.ReadInteger;
  AStr := AHelper.ReadNullTerminatedAnsiString(AReader);
  if L > Length(AStr) + 1 then
  begin
    SetLength(Result, (L - Length(AStr) + 1) div 2);
    AHelper.Stream.ReadBuffer(Result[1], Length(Result));
  end
  else
    Result := dxAnsiStringToString(AStr);
end;

procedure TdxItemMoniker.WriteMonikerPart(AHelper: TdxBIFFHyperlinkHelper; AWriter: TcxWriter; const AValue: string);
var
  AStr: AnsiString;
begin
  AStr := dxStringToAnsiString(AValue);
  AWriter.WriteInteger(Length(AStr) + 1 + Length(AValue));
  AHelper.Stream.WriteBuffer(AStr[1], Length(AStr) + 1);
  if Length(AValue) > 0 then
    AHelper.Stream.WriteBuffer(AValue[1], Length(AStr));
end;

procedure dxInitializeXLSTypes;
begin
  MonikersList := TDictionary<TGUID, TdxHyperlinkMonikerObjectClass>.Create;
  TdxURLMoniker.Register;
  TdxFileMoniker.Register;
  TdxItemMoniker.Register;
end;

procedure dxFinalizeXLSTypes;
begin
  FreeAndNil(MonikersList);
end;

initialization
  dxInitializeXLSTypes;

finalization
  dxFinalizeXLSTypes;

end.
