{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
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

unit dxPSTrueTypeFont;

{$I cxVer.inc}

interface

uses
  Windows, Classes, SysUtils, cxClasses, Math, dxCore, Graphics;

type
  TdxPSTTFFile = class;
  TdxPSTTFGlyphSection = class;
  TdxPSTTFTableDirectory = class;

  EdxPSTTFFileException = class(EdxException);

  TdxPSTTFSectionTag = Integer;

  TdxPSTTFTableEntryData = packed record
    CheckSum: Cardinal;
    Length: Cardinal;
    Offset: Cardinal;
    Tag: TdxPSTTFSectionTag;
  end;

  TdxPSTTFMapEntryInfo = packed record
    Delta: Word;
    EntryEnd: Word;
    EntryStarts: Word;
    RangeOffsets: Word;
  end;

  TdxPSTTFMapEntryInfoList = array of TdxPSTTFMapEntryInfo;

  { TdxPSTTFCharCacheListItem }

  TdxPSTTFCharCacheListItem = class(TObject)
  private
    FCharCode: Word;
    FGlyphIndex: Word;
  public
    constructor Create(ACharCode, AGlyphIndex: Word);
    //
    property CharCode: Word read FCharCode;
    property GlyphIndex: Word read FGlyphIndex;
  end;

  { TdxPSTTFCharCacheList }

  TdxPSTTFCharCacheList = class(TObject)
  private
    FList: TcxObjectList;
    function GetCacheItem(AIndex: Integer): TdxPSTTFCharCacheListItem;
    function GetCount: Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Add(ACharCode, AGlyphIndex: Word);
    function IndexOf(ACharCode: Word): Integer;
    procedure SortByGlyphIndex;
    //
    property CacheItem[Index: Integer]: TdxPSTTFCharCacheListItem read GetCacheItem;
    property Count: Integer read GetCount;
  end;

  { TdxPSTTFStream }

  TdxPSTTFStream = class(TObject)
  private
    FStream: TStream;
    function GetPosition: Int64;
    function GetSize: Int64;
    procedure SetPosition(AValue: Int64);
  protected
    procedure ReorderBytes(ABuff: PByteArray; ABuffSize: Integer);
    procedure ReorderLong(var L: Cardinal); inline;
    procedure ReorderWord(var W: Word); inline;
  public
    constructor Create(AStream: TStream); virtual;
    function CalculateCheckSum(AStartsPosition, ALength: Cardinal): Cardinal;
    function ReadDate: Int64;
    function ReadLong: Cardinal;
    function ReadWord: Word;
    procedure ReadRaw(var Buffer; Count: LongInt); virtual;
    procedure Seek(AOffset: Integer; AOrigin: TSeekOrigin = soCurrent);
    procedure WriteDate(AValue: Int64);
    procedure WriteLong(AValue: Cardinal);
    procedure WritePadding; virtual;
    procedure WriteRaw(var Buffer; Count: LongInt); virtual;
    procedure WriteWord(AValue: Word);
    //
    property Position: Int64 read GetPosition write SetPosition;
    property Size: Int64 read GetSize;
    property Stream: TStream read FStream;
  end;

  { TdxPSTTFCustomObject }

  TdxPSTTFCustomObject = class(TObject)
  private
    FOwner: TdxPSTTFFile;
  protected
    procedure LoadFromStream(Stream: TdxPSTTFStream); virtual; abstract;
    procedure SaveToStream(Stream: TdxPSTTFStream); virtual; abstract;
  public
    constructor Create(AOwner: TdxPSTTFFile); virtual;
    //
    property Owner: TdxPSTTFFile read FOwner;
  end;

  { TdxPSTTFCustomSection }

  TdxPSTTFCustomSection = class(TdxPSTTFCustomObject)
  private
    FExists: Boolean;
    FVersion: Cardinal;
    FTableEntryData: TdxPSTTFTableEntryData;
    function GetSectionOffset: Integer;
  protected
    function GetSize: Integer; virtual;
    procedure LoadFromStream(Stream: TdxPSTTFStream); override;
    procedure ReadData(Stream: TdxPSTTFStream); virtual;
    procedure ReadDataFromStream(AStream: TStream);
    procedure Reset; virtual;
    procedure SaveToStream(Stream: TdxPSTTFStream); override;
    procedure WriteData(Stream: TdxPSTTFStream); virtual;
    //
    property Exists: Boolean read FExists write FExists;
    property SectionOffset: Integer read GetSectionOffset;
    property Size: Integer read GetSize;
    property TableEntryData: TdxPSTTFTableEntryData read FTableEntryData;
  public
    class function Tag: TdxPSTTFSectionTag; virtual;
  end;

  { TdxPSTTFHeadSection }

  TdxPSTTFHeadSection = class(TdxPSTTFCustomSection)
  private
    FCheckSum: Cardinal;
    FCreated: Int64;
    FFlags: Cardinal;
    FFontDirectionHint: SmallInt;
    FFontRevision: Cardinal;
    FGlyphDataFormat: SmallInt;
    FIndexToLocFormat: SmallInt;
    FLowestRecPPEM: Word;
    FMacStyle: Word;
    FMaginNumber: Cardinal;
    FMaxSize, FMinSize: TSize;
    FModified: Int64;
    FUnitsPerEm: Cardinal;
  protected
    function GetSize: Integer; override;
    procedure ReadData(Stream: TdxPSTTFStream); override;
    procedure WriteCheckSum(Stream: TdxPSTTFStream);
    procedure WriteData(Stream: TdxPSTTFStream); override;
  public
    class function Tag: TdxPSTTFSectionTag; override;
    //
    property IndexToLocFormat: SmallInt read FIndexToLocFormat;
    property MaxSize: TSize read FMaxSize;
    property MinSize: TSize read FMinSize;
  end;

  { TdxPSTTFHHEASection }

  TdxPSTTFHHEASection = class(TdxPSTTFCustomSection)
  private
    FAdvanceWidthMax: Word;
    FAscender: SmallInt;
    FCaretSlopeRise: SmallInt;
    FCaretSlopeRun: SmallInt;
    FDescender: SmallInt;
    FLineGap: SmallInt;
    FMaxExtentX: SmallInt;
    FMetricDataFormat: SmallInt;
    FMinLeftSideBearing: SmallInt;
    FMinRightSideBearing: SmallInt;
    FNumberOfHMetrics: Word;
    FReserved: array[0..4] of Word;
  protected
    function GetSize: Integer; override;
    procedure ReadData(Stream: TdxPSTTFStream); override;
    procedure WriteData(Stream: TdxPSTTFStream); override;
  public
    class function Tag: TdxPSTTFSectionTag; override;
    //
    property AdvanceWidthMax: Word read FAdvanceWidthMax;
    property Ascender: SmallInt read FAscender;
    property CaretSlopeRise: SmallInt read FCaretSlopeRise;
    property CaretSlopeRun: SmallInt read FCaretSlopeRun;
    property Descender: SmallInt read FDescender;
    property LineGap: SmallInt read FLineGap;
    property MaxExtentX: SmallInt read FMaxExtentX;
    property MetricDataFormat: SmallInt read FMetricDataFormat;
    property MinLeftSideBearing: SmallInt read FMinLeftSideBearing;
    property MinRightSideBearing: SmallInt read FMinRightSideBearing;
    property NumberOfHMetrics: Word read FNumberOfHMetrics;
  end;

  { TdxPSTTFMaxPSection }

  TdxPSTTFMaxPSection = class(TdxPSTTFCustomSection)
  private
    FMaxComponentDepth: Word;
    FMaxComponentElements: Word;
    FMaxCompositeContours: Word;
    FMaxCompositePoints: Word;
    FMaxContours: Word;
    FMaxFunctionDefs: Word;
    FMaxInstructionDefs: Word;
    FMaxPoints: Word;
    FMaxSizeOfInstructions: Word;
    FMaxStackElements: Word;
    FMaxStorage: Word;
    FMaxTwilightPoints: Word;
    FMaxZones: Word;
    FNumGlyphs: Word;
  protected
    function GetSize: Integer; override;
    procedure ReadData(Stream: TdxPSTTFStream); override;
    procedure WriteData(Stream: TdxPSTTFStream); override;
  public
    class function Tag: TdxPSTTFSectionTag; override;
    //
    property NumGlyphs: Word read FNumGlyphs;
  end;

  { TdxPSTTFCMapSection }

  TdxPSTTFCMapSection = class(TdxPSTTFCustomSection)
  private
    FEntryInfoList: TdxPSTTFMapEntryInfoList;
    FID: Word;
    FMap: array[Word] of Word;
    FVersion: Word;

    function GetMap(Index: Word): Word;
  protected
    function GetSize: Integer; override;
    function CalculateEntriesInfo(ACharList: TdxPSTTFCharCacheList): TdxPSTTFMapEntryInfoList;
    function CalculateSegmentCount(ACharList: TdxPSTTFCharCacheList): Integer;
    procedure ReadData(Stream: TdxPSTTFStream); override;
    procedure ReadUnicodeTable(Stream: TdxPSTTFStream; AUnicodeTableOffset: Integer);
    procedure ReadUnicodeTableVersion4(Stream: TdxPSTTFStream);
    procedure WriteData(Stream: TdxPSTTFStream); override;
    procedure WriteGlyphIndexes(Stream: TdxPSTTFStream; const AEntriesInfo: TdxPSTTFMapEntryInfoList);
    procedure WriteUnicodeTable(Stream: TdxPSTTFStream; const AEntriesInfo: TdxPSTTFMapEntryInfoList);
  public
    destructor Destroy; override;
    class function Tag: TdxPSTTFSectionTag; override;
    //
    property EntryInfoList: TdxPSTTFMapEntryInfoList read FEntryInfoList;
    property ID: Word read FID;
    property Map[Index: Word]: Word read GetMap;
  end;

  { TdxPSTTFOS2Section }

  TdxPSTTFOS2Section = class(TdxPSTTFCustomSection)
  private
    FFamilyClass: Byte;
    FFamilySubClass: Byte;
    FFirstCharIndex: Word;
    FFontType: SmallInt;
    FLastCharIndex: Word;
    FPanose: TPanose;
    FVendor: array[0..3] of AnsiChar;
  protected
    procedure ReadData(Stream: TdxPSTTFStream); override;
  public
    class function Tag: TdxPSTTFSectionTag; override;
    //
    property FamilyClass: Byte read FFamilyClass;
    property FamilySubClass: Byte read FFamilySubClass;
    property FontType: SmallInt read FFontType;
    property Panose: TPanose read FPanose;
  end;

  { TdxPSTTFHMtxSection }

  TdxPSTTFHMtxEntry = packed record
    AdvanceWidth: Word;
    LeftSideBearing: SmallInt;
  end;

  TdxPSTTFHMtxSection = class(TdxPSTTFCustomSection)
  private
    FEntries: array of TdxPSTTFHMtxEntry;
    function GetEntry(Index: Integer): TdxPSTTFHMtxEntry;
    function GetEntryCount: Integer;
    function GetHHEASection: TdxPSTTFHHEASection;
  protected
    function GetSize: Integer; override;
    procedure ReadData(Stream: TdxPSTTFStream); override;
    procedure WriteData(Stream: TdxPSTTFStream); override;
  public
    destructor Destroy; override;
    class function Tag: TdxPSTTFSectionTag; override;
    //
    property Entry[Index: Integer]: TdxPSTTFHMtxEntry read GetEntry;
    property EntryCount: Integer read GetEntryCount;
    property HHEASection: TdxPSTTFHHEASection read GetHHEASection;
  end;

  { TdxPSTTFLocaSection }

  TdxPSTTFLocaSection = class(TdxPSTTFCustomSection)
  private
    FOffsets: array of Integer;
    function GetGlyphSection: TdxPSTTFGlyphSection;
    function GetIndexToLocFormat: Integer;
    function GetOffset(Index: Integer): Integer;
  protected
    procedure CalculateOffsets(ACharList: TdxPSTTFCharCacheList);
    function GetSize: Integer; override;
    procedure ReadData(Stream: TdxPSTTFStream); override;
    procedure WriteData(Stream: TdxPSTTFStream); override;
  public
    destructor Destroy; override;
    class function Tag: TdxPSTTFSectionTag; override;
    procedure Clear;
    //
    property GlyphSection: TdxPSTTFGlyphSection read GetGlyphSection;
    property IndexToLocFormat: Integer read GetIndexToLocFormat;
    property Offset[Index: Integer]: Integer read GetOffset;
  end;

  { TdxPSTTFGlyphInfo }

  TdxPSTTFGlyphInfo = class(TObject)
  private
    FComponentGlyphs: TList;
    FData: Pointer;
    FDataSize: Integer;
    FIndex: Integer;
    function GetComponentCount: Integer;
    function GetComponentGlyph(Index: Integer): Word;
  public
    constructor Create(AIndex, ASize: Integer);
    destructor Destroy; override;
    procedure ReadData(Stream: TdxPSTTFStream);
    procedure WriteData(Stream: TdxPSTTFStream);
    //
    property ComponentGlyph[Index: Integer]: Word read GetComponentGlyph;
    property ComponentCount: Integer read GetComponentCount;
    property Data: Pointer read FData;
    property DataSize: Integer read FDataSize;
    property Index: Integer read FIndex;
  end;

  { TdxPSTTFGlyphSection }

  TdxPSTTFGlyphSection = class(TdxPSTTFCustomSection)
  private
    FGlyphList: TcxObjectList;
    function GetGlyphInfo(Index: Integer): TdxPSTTFGlyphInfo;
    function GetGlyphInfoCount: Integer;
    function GetLocaSection: TdxPSTTFLocaSection;
  protected
    function GetGlyphInfoByGlyphIndex(AGlyphIndex: Integer; out AGlyphInfo: TdxPSTTFGlyphInfo): Boolean;
    function GetSize: Integer; override;
    procedure ReadData(Stream: TdxPSTTFStream); override;
    procedure RemoveUnusedGlyphs(ACharList: TdxPSTTFCharCacheList);
    procedure WriteData(Stream: TdxPSTTFStream); override;
  public
    constructor Create(AOwner: TdxPSTTFFile); override;
    destructor Destroy; override;
    class function Tag: TdxPSTTFSectionTag; override;
    //
    property GlyphInfo[Index: Integer]: TdxPSTTFGlyphInfo read GetGlyphInfo;
    property GlyphInfoCount: Integer read GetGlyphInfoCount;
    property LocaSection: TdxPSTTFLocaSection read GetLocaSection;
  end;

  { TdxPSTTFTableDirectory }

  TdxPSTTFTableDirectory = class(TdxPSTTFCustomObject)
  private
    FEntrySelector: Word;
    FExistsSectionsCount: Integer;
    FHeaderOffset: Integer;
    FRangeShift: Word;
    FSearchRange: Word;
    FSections: TcxObjectList;
    FVersion: Cardinal;
    function GetSection(AIndex: Integer): TdxPSTTFCustomSection;
    function GetSectionCount: Integer;
  protected
    function CalculateExistsSectionCount: Integer;
    function FindSection(const ATag: TdxPSTTFSectionTag): TdxPSTTFCustomSection;
    function ReadSectionEntry(Stream: TdxPSTTFStream; ASection: TdxPSTTFCustomSection;
      AEntriesCount, AHeaderOffset: Integer): Boolean;
    procedure CalculateHeader;
    procedure CalculateTableEntryDatas(AHeaderSize: Integer);
    procedure LoadFromStream(Stream: TdxPSTTFStream); override;
    procedure SaveToStream(Stream: TdxPSTTFStream); override;
  public
    constructor Create(AOwner: TdxPSTTFFile); override;
    destructor Destroy; override;
    procedure Register(ASection: TdxPSTTFCustomSection);
    //
    property ExistsSectionsCount: Integer read FExistsSectionsCount;
    property Section[Index: Integer]: TdxPSTTFCustomSection read GetSection;
    property SectionCount: Integer read GetSectionCount;
  end;

  { TdxPSTTFBinarySection }

  TdxPSTTFBinarySection = class(TdxPSTTFCustomSection)
  private
    FData: Pointer;
    FDataSize: Integer;
  protected
    function GetSize: Integer; override;
    procedure FreeBuffer;
    procedure ReadData(Stream: TdxPSTTFStream); override;
    procedure WriteData(Stream: TdxPSTTFStream); override;
  public
    destructor Destroy; override;
  end;

  { TdxPSTTFPrepSection }

  TdxPSTTFPrepSection = class(TdxPSTTFBinarySection)
  public
    class function Tag: TdxPSTTFSectionTag; override;
  end;

  { TdxPSTTFCvtSection }

  TdxPSTTFCvtSection = class(TdxPSTTFBinarySection)
  public
    class function Tag: TdxPSTTFSectionTag; override;
  end;

  { TdxPSTTFFpgmSection }

  TdxPSTTFFpgmSection = class(TdxPSTTFBinarySection)
  public
    class function Tag: TdxPSTTFSectionTag; override;
  end;

  { TdxPSTTFFile }

  TdxPSTTFFile = class(TObject)
  private
    FCMapSection: TdxPSTTFCMapSection;
    FCvtSection: TdxPSTTFBinarySection;
    FFpgmSection: TdxPSTTFBinarySection;
    FGlyphSection: TdxPSTTFGlyphSection;
    FHeadSection: TdxPSTTFHeadSection;
    FHHEASection: TdxPSTTFHHEASection;
    FHMtxSection: TdxPSTTFHMtxSection;
    FLocaSection: TdxPSTTFLocaSection;
    FMaxPSection: TdxPSTTFMaxPSection;
    FOS2Section: TdxPSTTFOS2Section;
    FPrepSection: TdxPSTTFBinarySection;
    FTableDirectory: TdxPSTTFTableDirectory;

    function GetCharCodeByGlyphIndex(AGlyphIndex: Word): Word;
    function GetCharWidthByCode(ACharCode: Word): Integer;
    function GetCharWidthByGlyphIndex(AGlyphIndex: Word): Integer;
    function GetGlyphIndex(ACharCode: Word): Word;
    function GetScaleFactor: Single;
  public
    constructor Create; overload; virtual;
    constructor Create(AFont: TFont); overload; virtual;
    destructor Destroy; override;
    procedure Rebuild(ACharList: TdxPSTTFCharCacheList);
    //
    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromFont(AFont: TFont);
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToFile(const AFileName: string);
    procedure SaveToStream(AStream: TStream);
    //
    property CharCodeByGlyphIndex[AGlyphIndex: Word]: Word read GetCharCodeByGlyphIndex;
    property CharWidthByCode[ACharCode: Word]: Integer read GetCharWidthByCode;
    property CharWidthByGlyphIndex[AIndex: Word]: Integer read GetCharWidthByGlyphIndex;
    property GlyphIndex[ACharCode: Word]: Word read GetGlyphIndex;
    property ScaleFactor: Single read GetScaleFactor;
    //
    property CMapSection: TdxPSTTFCMapSection read FCMapSection;
    property GlyphSection: TdxPSTTFGlyphSection read FGlyphSection;
    property HeadSection: TdxPSTTFHeadSection read FHeadSection;
    property HHEASection: TdxPSTTFHHEASection read FHHEASection;
    property HMtxSection: TdxPSTTFHMtxSection read FHMtxSection;
    property LocaSection: TdxPSTTFLocaSection read FLocaSection;
    property MaxPSection: TdxPSTTFMaxPSection read FMaxPSection;
    property OS2Section: TdxPSTTFOS2Section read FOS2Section;
    property TableDirectory: TdxPSTTFTableDirectory read FTableDirectory;
  end;

function dxPSGetFontCharsMap(AFont: TFont; out AMap: TdxPSTTFCMapSection): Boolean;
function dxPSGetFontData(AStream: TMemoryStream; AFont: TFont; ATable: Integer = 0): Boolean;
implementation

const
  GLYPH_ARG_1_AND_2_ARE_WORDS    = 0001;
  GLYPH_ARGS_ARE_XY_VALUES       = 0002;
  GLYPH_ROUND_XY_TO_GRID         = 0004;
  GLYPH_WE_HAVE_A_SCALE          = 0008;
  GLYPH_MORE_COMPONENTS          = 0032;
  GLYPH_WE_HAVE_AN_X_AND_Y_SCALE = 0064;
  GLYPH_WE_HAVE_A_TWO_BY_TWO     = 0128;
  GLYPH_WE_HAVE_INSTRUCTIONS     = 0256;
  GLYPH_USE_MY_METRICS           = 0512;
  GLYPH_OVERLAP_COMPOUND         = 1024;

  sdxCMapInvalidTableFormat = 'Invalid table format in the CMap font section';
  sdxCMapUnicodeTableMissing = 'The unicode CMap doesn''t exist in the font file';
  sdxReadFontDataError = 'Error when reading font file';

// Note: for CBuilder
function GetSectionTag(const ID: AnsiString): TdxPSTTFSectionTag;
var
  I: Integer;
begin
  Result := 0;
  for I := Length(ID) downto 1 do
    Result := (Result shl 8) or Byte(ID[I]);
end;

function CalculatePaddingSize(APosition: Integer): Integer;
begin
  Result := 4 - APosition mod 4;
end;

function CompareCacheListItemByGlyphIndex(Item1, Item2: TdxPSTTFCharCacheListItem): Integer;
begin
  Result := Item1.GlyphIndex - Item2.GlyphIndex;
end;

function dxPSGetFontCharsMap(AFont: TFont; out AMap: TdxPSTTFCMapSection): Boolean;
var
  AMemStream: TMemoryStream;
  APrevCharset: Integer;
begin
  AMemStream := TMemoryStream.Create;
  try
    APrevCharset := AFont.Charset;
    try
      AFont.Charset := DEFAULT_CHARSET;
      Result := dxPSGetFontData(AMemStream, AFont, TdxPSTTFCMapSection.Tag);
      if Result then
      begin
        AMemStream.Position := 0;
        AMap := TdxPSTTFCMapSection.Create(nil);
        AMap.ReadDataFromStream(AMemStream);
      end;
    finally
      AFont.Charset := APrevCharset;
    end;
  finally
    AMemStream.Free;
  end;
end;

function dxPSGetFontData(AStream: TMemoryStream; AFont: TFont; ATable: Integer = 0): Boolean;
var
  AFontDataLength: Cardinal;
  AOldFont: HFONT;
  DC: HDC;
begin
  DC := GetDC(0);
  AOldFont := SelectObject(DC, AFont.Handle);
  AFontDataLength := GetFontData(DC, ATable, 0, nil, 0);
  Result := AFontDataLength <> GDI_ERROR;
  if Result and Assigned(AStream) then
  begin
    AStream.Size := AFontDataLength;
    GetFontData(DC, ATable, 0, AStream.Memory, AStream.Size);
  end;
  SelectObject(DC, AOldFont);
  ReleaseDC(0, DC);
end;

{ TdxPSTTFStream }

constructor TdxPSTTFStream.Create(AStream: TStream);
begin
  FStream := AStream;
end;

function TdxPSTTFStream.CalculateCheckSum(AStartsPosition, ALength: Cardinal): Cardinal;
var
  AOldPosition: Integer;
  ARemainder: Integer;
begin
  AOldPosition := Position;
  try
    Result := 0;
    ARemainder := ALength mod 4;
    if ARemainder > 0 then
      Inc(ALength, 4 - ARemainder);
    ALength := ALength div 4;
    Position := AStartsPosition;
    while ALength > 0 do
    begin
      Inc(Result, ReadLong);
      Dec(ALength);
    end;
  finally
    Position := AOldPosition;
  end;
end;

function TdxPSTTFStream.ReadDate: Int64;
begin
  ReadRaw(Result, SizeOf(Result));
  ReorderBytes(@Result, SizeOf(Result));
end;

function TdxPSTTFStream.ReadLong: Cardinal;
begin
  ReadRaw(Result, SizeOf(Result));
  ReorderLong(Result);
end;

procedure TdxPSTTFStream.ReadRaw(var Buffer; Count: LongInt);
begin
  Stream.ReadBuffer(Buffer, Count);
end;

function TdxPSTTFStream.ReadWord: Word;
begin
  ReadRaw(Result, SizeOf(Result));
  ReorderWord(Result);
end;

procedure TdxPSTTFStream.ReorderBytes(ABuff: PByteArray; ABuffSize: Integer);
var
  ATempByte: Byte;
  I: Integer;
begin
  for I := 0 to (ABuffSize div 2) - 1 do
  begin
    ATempByte := ABuff^[I];
    ABuff^[I] := ABuff^[ABuffSize - I - 1];
    ABuff^[ABuffSize - I - 1] := ATempByte;
  end;
end;

procedure TdxPSTTFStream.ReorderLong(var L: Cardinal);
var
  D: array[0..3] of Byte absolute L;
begin
  L := D[3] or (D[2] shl 8) or (D[1] shl 16) or (D[0] shl 24);
end;

procedure TdxPSTTFStream.ReorderWord(var W: Word);
begin
  W := (Byte(W) shl 8) or (W shr 8);
end;

procedure TdxPSTTFStream.Seek(AOffset: Integer; AOrigin: TSeekOrigin = soCurrent);
begin
  Stream.Seek(AOffset, AOrigin);
end;

procedure TdxPSTTFStream.WriteDate(AValue: Int64);
begin
  ReorderBytes(@AValue, SizeOf(AValue));
  WriteRaw(AValue, SizeOf(AValue));
end;

procedure TdxPSTTFStream.WriteLong(AValue: Cardinal);
begin
  ReorderLong(AValue);
  WriteRaw(AValue, SizeOf(AValue));
end;

procedure TdxPSTTFStream.WritePadding;
const
  PaddingValue: Byte = 0;
var
  I, APaddingSize: Integer;
begin
  APaddingSize := CalculatePaddingSize(Position);
  for I := 0 to APaddingSize - 1 do
    Stream.Write(PaddingValue, SizeOf(PaddingValue));
end;

procedure TdxPSTTFStream.WriteRaw(var Buffer; Count: LongInt);
begin
  Stream.WriteBuffer(Buffer, Count);
end;

procedure TdxPSTTFStream.WriteWord(AValue: Word);
begin
  ReorderWord(AValue);
  WriteRaw(AValue, SizeOf(AValue));
end;

function TdxPSTTFStream.GetPosition: Int64;
begin
  Result := Stream.Position;
end;

function TdxPSTTFStream.GetSize: Int64;
begin
  Result := Stream.Size;
end;

procedure TdxPSTTFStream.SetPosition(AValue: Int64);
begin
  Stream.Position := AValue;
end;

{ TdxPSTTFFile }

constructor TdxPSTTFFile.Create;
begin
  FTableDirectory := TdxPSTTFTableDirectory.Create(Self);
  FCMapSection := TdxPSTTFCMapSection.Create(Self);
  FHeadSection := TdxPSTTFHeadSection.Create(Self);
  FHHEASection := TdxPSTTFHHEASection.Create(Self);
  FMaxPSection := TdxPSTTFMaxPSection.Create(Self);
  FHMtxSection := TdxPSTTFHMtxSection.Create(Self);
  FLocaSection := TdxPSTTFLocaSection.Create(Self);
  FGlyphSection := TdxPSTTFGlyphSection.Create(Self);
  FPrepSection := TdxPSTTFPrepSection.Create(Self);
  FCvtSection := TdxPSTTFCvtSection.Create(Self);
  FFpgmSection := TdxPSTTFFpgmSection.Create(Self);
  FOS2Section := TdxPSTTFOS2Section.Create(Self);

  // note: don't change order
  TableDirectory.Register(FHeadSection);
  TableDirectory.Register(FCMapSection);
  TableDirectory.Register(FHHEASection);
  TableDirectory.Register(FMaxPSection);
  TableDirectory.Register(FHMtxSection);
  TableDirectory.Register(FLocaSection);
  TableDirectory.Register(FGlyphSection);
  TableDirectory.Register(FPrepSection);
  TableDirectory.Register(FCvtSection);
  TableDirectory.Register(FFpgmSection);
  TableDirectory.Register(FOS2Section);
end;

constructor TdxPSTTFFile.Create(AFont: TFont);
begin
  Create;
  LoadFromFont(AFont);
end;

destructor TdxPSTTFFile.Destroy;
begin
  FreeAndNil(FTableDirectory);
  inherited Destroy;
end;

procedure TdxPSTTFFile.LoadFromFile(const AFileName: string);
var
  AFileStream: TFileStream;
begin
  AFileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    LoadFromStream(AFileStream);
  finally
    AFileStream.Free;
  end;
end;

procedure TdxPSTTFFile.LoadFromFont(AFont: TFont);
var
  AMemStream: TMemoryStream;
  ASection: TdxPSTTFCustomSection;
  I, ASavedCharset: Integer;
begin
  ASavedCharset := AFont.Charset;
  AMemStream := TMemoryStream.Create;
  try
    AFont.Charset := DEFAULT_CHARSET; // note: for get all font Glyphs!
    for I := 0 to TableDirectory.SectionCount - 1 do
    begin
      AMemStream.Size := 0;
      ASection := TableDirectory.Section[I];
      ASection.Exists := dxPSGetFontData(AMemStream, AFont, ASection.Tag);
      if ASection.Exists then
        ASection.ReadDataFromStream(AMemStream);
    end;
  finally
    AFont.Charset := ASavedCharset;
    AMemStream.Free;
  end;
end;

procedure TdxPSTTFFile.LoadFromStream(AStream: TStream);
var
  AFontStream: TdxPSTTFStream;
begin
  AFontStream := TdxPSTTFStream.Create(AStream);
  try
    TableDirectory.LoadFromStream(AFontStream);
  finally
    AFontStream.Free;
  end;
end;

procedure TdxPSTTFFile.SaveToFile(const AFileName: string);
var
  AStream: TFileStream;
begin
  AStream := TFileStream.Create(AFileName, fmCreate or fmShareDenyNone);
  try
    SaveToStream(AStream);
  finally
    AStream.Free;
  end;
end;

procedure TdxPSTTFFile.SaveToStream(AStream: TStream);
var
  AFontStream: TdxPSTTFStream;
begin
  AFontStream := TdxPSTTFStream.Create(AStream);
  try
    TableDirectory.SaveToStream(AFontStream);
  finally
    AFontStream.Free;
  end;
end;

procedure TdxPSTTFFile.Rebuild(ACharList: TdxPSTTFCharCacheList);
begin
  OS2Section.Exists := False;
  CMapSection.Exists := False;
  GlyphSection.RemoveUnusedGlyphs(ACharList);
  LocaSection.CalculateOffsets(ACharList);
end;

function TdxPSTTFFile.GetCharCodeByGlyphIndex(AGlyphIndex: Word): Word;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to MaxWord do
    if GlyphIndex[I] = AGlyphIndex then
    begin
      Result := I;
      Break;
    end;
end;

function TdxPSTTFFile.GetCharWidthByCode(ACharCode: Word): Integer;
begin
  Result := CharWidthByGlyphIndex[GlyphIndex[ACharCode]];
end;

function TdxPSTTFFile.GetCharWidthByGlyphIndex(AGlyphIndex: Word): Integer;
begin
  Result := Round(ScaleFactor * HMtxSection.Entry[AGlyphIndex].AdvanceWidth);
end;

function TdxPSTTFFile.GetGlyphIndex(ACharCode: Word): Word;
begin
  Result := CMapSection.Map[ACharCode];
end;

function TdxPSTTFFile.GetScaleFactor: Single;
begin
  Result := 1000 / HeadSection.FUnitsPerEm;
end;

{ TdxPSTTFCustomObject }

constructor TdxPSTTFCustomObject.Create(AOwner: TdxPSTTFFile);
begin
  FOwner := AOwner;
end;

{ TdxPSTTFTableDirectory }

constructor TdxPSTTFTableDirectory.Create(AOwner: TdxPSTTFFile);
begin
  inherited Create(AOwner);
  FSections := TcxObjectList.Create;
end;

destructor TdxPSTTFTableDirectory.Destroy;
begin
  FreeAndNil(FSections);
  inherited Destroy;
end;

procedure TdxPSTTFTableDirectory.CalculateHeader;
var
  ATemp: Integer;
begin
  FVersion := 65536;
  FExistsSectionsCount := CalculateExistsSectionCount;
  FEntrySelector := Floor(Log2(ExistsSectionsCount));
  ATemp := Trunc(Power(2, FEntrySelector));
  FSearchRange := ATemp * 16;
  FRangeShift := ExistsSectionsCount * 16 - FSearchRange;
end;

procedure TdxPSTTFTableDirectory.CalculateTableEntryDatas(AHeaderSize: Integer);
var
  ASection: TdxPSTTFCustomSection;
  ATableData: TdxPSTTFTableEntryData;
  I, AOffset: Integer;
begin
  AOffset := AHeaderSize + SizeOf(TdxPSTTFTableEntryData) * ExistsSectionsCount;
  for I := 0 to SectionCount - 1 do
  begin
    ASection := Section[I];
    if ASection.Exists then
    begin
      Inc(AOffset, CalculatePaddingSize(AOffset));
      ATableData := ASection.FTableEntryData;
      ATableData.CheckSum := 0;
      ATableData.Tag := ASection.Tag;
      ATableData.Length := ASection.Size;
      ATableData.Offset := AOffset;
      ASection.FTableEntryData := ATableData;
      Inc(AOffset, ATableData.Length);
    end;
  end;
end;

function TdxPSTTFTableDirectory.FindSection(const ATag: TdxPSTTFSectionTag): TdxPSTTFCustomSection;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to SectionCount - 1 do
    if Section[I].Tag = ATag then
    begin
      Result := Section[I];
      Break;
    end;
end;

function TdxPSTTFTableDirectory.GetSection(AIndex: Integer): TdxPSTTFCustomSection;
begin
  Result := TdxPSTTFCustomSection(FSections.Items[AIndex]);
end;

function TdxPSTTFTableDirectory.GetSectionCount: Integer;
begin
  Result := FSections.Count;
end;

function TdxPSTTFTableDirectory.CalculateExistsSectionCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to SectionCount - 1 do
  begin
    if Section[I].Exists then
      Inc(Result);
  end;
end;

function TdxPSTTFTableDirectory.ReadSectionEntry(Stream: TdxPSTTFStream;
  ASection: TdxPSTTFCustomSection; AEntriesCount, AHeaderOffset: Integer): Boolean;
var
  I: Integer;
  ATableData: TdxPSTTFTableEntryData;
begin
  ASection.Reset;
  Result := False;
  Stream.Position := AHeaderOffset;
  for I := 0 to AEntriesCount - 1 do
  begin
    Stream.ReadRaw(ATableData.Tag, SizeOf(ATableData.Tag));
    ATableData.CheckSum := Stream.ReadLong;
    ATableData.Offset := Stream.ReadLong;
    ATableData.Length := Stream.ReadLong;
    Result := ATableData.Tag = ASection.Tag;
    if Result then
    begin
      ASection.FTableEntryData := ATableData;
      ASection.LoadFromStream(Stream);
      Break;
    end;
  end;
end;

procedure TdxPSTTFTableDirectory.LoadFromStream(Stream: TdxPSTTFStream);
var
  I, ACount: Integer;
begin
  FVersion := Stream.ReadLong;
  ACount := Stream.ReadWord;
  FSearchRange := Stream.ReadWord;
  FEntrySelector := Stream.ReadWord;
  FRangeShift := Stream.ReadWord;
  FHeaderOffset := Stream.Position;
  for I := 0 to SectionCount - 1 do
    ReadSectionEntry(Stream, Section[I], ACount, FHeaderOffset);
  FExistsSectionsCount := CalculateExistsSectionCount;
end;

procedure TdxPSTTFTableDirectory.SaveToStream(Stream: TdxPSTTFStream);

  procedure WriteTableEntries(AStream: TdxPSTTFStream);
  var
    ATableData: TdxPSTTFTableEntryData;
    I: Integer;
  begin
    for I := 0 to SectionCount - 1 do
      if Section[I].Exists then
      begin
        ATableData := Section[I].TableEntryData;
        AStream.WriteRaw(ATableData.Tag, SizeOf(ATableData.Tag));
        AStream.WriteLong(ATableData.CheckSum);
        AStream.WriteLong(ATableData.Offset);
        AStream.WriteLong(ATableData.Length);
      end;
  end;

var
  I: Integer;
begin
  CalculateHeader;
  Stream.WriteLong(FVersion);
  Stream.WriteWord(ExistsSectionsCount);
  Stream.WriteWord(FSearchRange);
  Stream.WriteWord(FEntrySelector);
  Stream.WriteWord(FRangeShift);
  CalculateTableEntryDatas(Stream.Position);
  WriteTableEntries(Stream);
  for I := 0 to SectionCount - 1 do
  begin
    if Section[I].Exists then
      Section[I].SaveToStream(Stream);
  end;
end;

procedure TdxPSTTFTableDirectory.Register(ASection: TdxPSTTFCustomSection);
begin
  FSections.Add(ASection);
end;

{ TdxPSTTFCustomSection }

function TdxPSTTFCustomSection.GetSectionOffset: Integer;
begin
  Result := TableEntryData.Offset;
end;

function TdxPSTTFCustomSection.GetSize: Integer;
begin
  Result := SizeOf(FVersion);
end;

procedure TdxPSTTFCustomSection.LoadFromStream(Stream: TdxPSTTFStream);
begin
  if SectionOffset >= 0 then
  begin
    Exists := True;
    Stream.Position := SectionOffset;
    ReadData(Stream);
  end;
end;

procedure TdxPSTTFCustomSection.ReadData(Stream: TdxPSTTFStream);
begin
  FVersion := Stream.ReadLong;
end;

procedure TdxPSTTFCustomSection.ReadDataFromStream(AStream: TStream);
var
  ATTFStream: TdxPSTTFStream;
begin
  ATTFStream := TdxPSTTFStream.Create(AStream);
  try
    ReadData(ATTFStream);
  finally
    ATTFStream.Free;
  end;
end;

procedure TdxPSTTFCustomSection.Reset;
begin
  Exists := False;
end;

procedure TdxPSTTFCustomSection.SaveToStream(Stream: TdxPSTTFStream);
begin
  Stream.WritePadding;
  WriteData(Stream);
end;

class function TdxPSTTFCustomSection.Tag: TdxPSTTFSectionTag;
begin
  Result := 0;
end;

procedure TdxPSTTFCustomSection.WriteData(Stream: TdxPSTTFStream);
begin
  Stream.WriteLong(FVersion);
end;

{ TdxPSTTFHeadSection }

function TdxPSTTFHeadSection.GetSize: Integer;
begin
  Result := inherited GetSize + 50;
end;

class function TdxPSTTFHeadSection.Tag: TdxPSTTFSectionTag;
begin
  Result := GetSectionTag('head');
end;

procedure TdxPSTTFHeadSection.ReadData(Stream: TdxPSTTFStream);
begin
  inherited ReadData(Stream);
  FFontRevision := Stream.ReadLong;
  FCheckSum := Stream.ReadLong;
  FMaginNumber := Stream.ReadLong;
  FFlags := Stream.ReadWord;
  FUnitsPerEm := Stream.ReadWord;
  FCreated := Stream.ReadDate;
  FModified := Stream.ReadDate;
  FMinSize.cx := SmallInt(Stream.ReadWord);
  FMinSize.cy := SmallInt(Stream.ReadWord);
  FMaxSize.cx := SmallInt(Stream.ReadWord);
  FMaxSize.cy := SmallInt(Stream.ReadWord);
  FMacStyle := Stream.ReadWord;
  FLowestRecPPEM := Stream.ReadWord;
  FFontDirectionHint := SmallInt(Stream.ReadWord);
  FIndexToLocFormat := SmallInt(Stream.ReadWord);
  FGlyphDataFormat := SmallInt(Stream.ReadWord);
end;

procedure TdxPSTTFHeadSection.WriteCheckSum(Stream: TdxPSTTFStream);
begin
  if FCheckSum = 0 then
    FCheckSum := $B1B0AFBA - Stream.CalculateCheckSum(0, Stream.Size);
  Stream.Position := SectionOffset + SizeOf(LongInt) * 2;
  Stream.WriteLong(FCheckSum);
end;

procedure TdxPSTTFHeadSection.WriteData(Stream: TdxPSTTFStream);
begin
  FCheckSum := 0;
  inherited WriteData(Stream);
  Stream.WriteLong(FFontRevision);
  Stream.WriteLong(FCheckSum);
  Stream.WriteLong(FMaginNumber);
  Stream.WriteWord(FFlags);
  Stream.WriteWord(FUnitsPerEm);
  Stream.WriteDate(FCreated);
  Stream.WriteDate(FModified);
  Stream.WriteWord(FMinSize.cx);
  Stream.WriteWord(FMinSize.cy);
  Stream.WriteWord(FMaxSize.cx);
  Stream.WriteWord(FMaxSize.cy);
  Stream.WriteWord(FMacStyle);
  Stream.WriteWord(FLowestRecPPEM);
  Stream.WriteWord(FFontDirectionHint);
  Stream.WriteWord(FIndexToLocFormat);
  Stream.WriteWord(FGlyphDataFormat);
end;

{ TdxPSTTFBinarySection }

destructor TdxPSTTFBinarySection.Destroy;
begin
  FreeBuffer;
  inherited Destroy;
end;

procedure TdxPSTTFBinarySection.FreeBuffer;
begin
  if Assigned(FData) then
  begin
    FreeMem(FData, FDataSize);
    FDataSize := 0;
    FData := nil;
  end;
end;

function TdxPSTTFBinarySection.GetSize: Integer;
begin
  Result := FDataSize;
end;

procedure TdxPSTTFBinarySection.ReadData(Stream: TdxPSTTFStream);
begin
  FreeBuffer;
  FDataSize := TableEntryData.Length;
  FData := AllocMem(FDataSize);
  Stream.ReadRaw(FData^, FDataSize);
end;

procedure TdxPSTTFBinarySection.WriteData(Stream: TdxPSTTFStream);
begin
  Stream.WriteRaw(FData^, FDataSize);
end;

{ TdxPSTTFPrepSection }

class function TdxPSTTFPrepSection.Tag: TdxPSTTFSectionTag;
begin
  Result := GetSectionTag('prep');
end;

{ TdxPSTTFCvtSection }

class function TdxPSTTFCvtSection.Tag: TdxPSTTFSectionTag;
begin
  Result := GetSectionTag('cvt ');
end;

{ TdxPSTTFFpgmSection }

class function TdxPSTTFFpgmSection.Tag: TdxPSTTFSectionTag;
begin
  Result := GetSectionTag('fpgm');
end;

{ TdxPSTTFHHEASection }

function TdxPSTTFHHEASection.GetSize: Integer;
begin
  Result := inherited GetSize + 32;
end;

class function TdxPSTTFHHEASection.Tag: TdxPSTTFSectionTag;
begin
  Result := GetSectionTag('hhea');
end;

procedure TdxPSTTFHHEASection.ReadData(Stream: TdxPSTTFStream);
begin
  inherited ReadData(Stream);
  FAscender := Stream.ReadWord;
  FDescender := Stream.ReadWord;
  FLineGap := Stream.ReadWord;
  FAdvanceWidthMax := Stream.ReadWord;
  FMinLeftSideBearing := Stream.ReadWord;
  FMinRightSideBearing := Stream.ReadWord;
  FMaxExtentX := Stream.ReadWord;
  FCaretSlopeRise := Stream.ReadWord;
  FCaretSlopeRun := Stream.ReadWord;
  Stream.ReadRaw(FReserved, SizeOf(FReserved));
  FMetricDataFormat := Stream.ReadWord;
  FNumberOfHMetrics := Stream.ReadWord;
end;

procedure TdxPSTTFHHEASection.WriteData(Stream: TdxPSTTFStream);
begin
  inherited WriteData(Stream);
  Stream.WriteWord(FAscender);
  Stream.WriteWord(FDescender);
  Stream.WriteWord(FLineGap);
  Stream.WriteWord(FAdvanceWidthMax);
  Stream.WriteWord(FMinLeftSideBearing);
  Stream.WriteWord(FMinRightSideBearing);
  Stream.WriteWord(FMaxExtentX);
  Stream.WriteWord(FCaretSlopeRise);
  Stream.WriteWord(FCaretSlopeRun);
  Stream.WriteRaw(FReserved, SizeOf(FReserved));
  Stream.WriteWord(FMetricDataFormat);
  Stream.WriteWord(FNumberOfHMetrics);
end;

{ TdxPSTTFMaxPSection }

function TdxPSTTFMaxPSection.GetSize: Integer;
begin
  Result := inherited GetSize + 28;
end;

class function TdxPSTTFMaxPSection.Tag: TdxPSTTFSectionTag;
begin
  Result := GetSectionTag('maxp');
end;

procedure TdxPSTTFMaxPSection.ReadData(Stream: TdxPSTTFStream);
begin
  inherited ReadData(Stream);
  FNumGlyphs := Stream.ReadWord;
  FMaxPoints := Stream.ReadWord;
  FMaxContours := Stream.ReadWord;
  FMaxCompositePoints := Stream.ReadWord;
  FMaxCompositeContours := Stream.ReadWord;
  FMaxZones := Stream.ReadWord;
  FMaxTwilightPoints := Stream.ReadWord;
  FMaxStorage := Stream.ReadWord;
  FMaxFunctionDefs := Stream.ReadWord;
  FMaxInstructionDefs := Stream.ReadWord;
  FMaxStackElements := Stream.ReadWord;
  FMaxSizeOfInstructions := Stream.ReadWord;
  FMaxComponentElements := Stream.ReadWord;
  FMaxComponentDepth := Stream.ReadWord;
end;

procedure TdxPSTTFMaxPSection.WriteData(Stream: TdxPSTTFStream);
begin
  inherited WriteData(Stream);
  Stream.WriteWord(FNumGlyphs);
  Stream.WriteWord(FMaxPoints);
  Stream.WriteWord(FMaxContours);
  Stream.WriteWord(FMaxCompositePoints);
  Stream.WriteWord(FMaxCompositeContours);
  Stream.WriteWord(FMaxZones);
  Stream.WriteWord(FMaxTwilightPoints);
  Stream.WriteWord(FMaxStorage);
  Stream.WriteWord(FMaxFunctionDefs);
  Stream.WriteWord(FMaxInstructionDefs);
  Stream.WriteWord(FMaxStackElements);
  Stream.WriteWord(FMaxSizeOfInstructions);
  Stream.WriteWord(FMaxComponentElements);
  Stream.WriteWord(FMaxComponentDepth);
end;

{ TdxPSTTFHMtxSection }

destructor TdxPSTTFHMtxSection.Destroy;
begin
  SetLength(FEntries, 0);
  inherited Destroy;
end;

function TdxPSTTFHMtxSection.GetEntry(Index: Integer): TdxPSTTFHMtxEntry;
begin
  Result := FEntries[Min(Index, High(FEntries))];
end;

function TdxPSTTFHMtxSection.GetEntryCount: Integer;
begin
  Result := Length(FEntries);
end;

function TdxPSTTFHMtxSection.GetHHEASection: TdxPSTTFHHEASection;
begin
  Result := Owner.HHEASection;
end;

function TdxPSTTFHMtxSection.GetSize: Integer;
begin
  Result := EntryCount * SizeOf(TdxPSTTFHMtxEntry);
end;

class function TdxPSTTFHMtxSection.Tag: TdxPSTTFSectionTag;
begin
  Result := GetSectionTag('hmtx');
end;

procedure TdxPSTTFHMtxSection.ReadData(Stream: TdxPSTTFStream);
var
  I: Integer;
begin
  SetLength(FEntries, HHEASection.FNumberOfHMetrics);
  for I := 0 to HHEASection.FNumberOfHMetrics - 1 do
  begin
    FEntries[I].AdvanceWidth := Stream.ReadWord;
    FEntries[I].LeftSideBearing := Stream.ReadWord;
  end;
end;

procedure TdxPSTTFHMtxSection.WriteData(Stream: TdxPSTTFStream);
var
  I: Integer;
begin
  for I := 0 to Length(FEntries) - 1 do
  begin
    Stream.WriteWord(FEntries[I].AdvanceWidth);
    Stream.WriteWord(FEntries[I].LeftSideBearing);
  end;
end;

{ TdxPSTTFCMapSectionEntry }

destructor TdxPSTTFCMapSection.Destroy;
begin
  FEntryInfoList := nil;
  inherited Destroy;
end;

function TdxPSTTFCMapSection.CalculateSegmentCount(ACharList: TdxPSTTFCharCacheList): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ACharList.Count - 1 do
  begin
    if ACharList.CacheItem[I + 1].CharCode <> ACharList.CacheItem[I].CharCode + 1 then
      Inc(Result);
  end;
  Inc(Result, 2);
end;

function TdxPSTTFCMapSection.CalculateEntriesInfo(
  ACharList: TdxPSTTFCharCacheList): TdxPSTTFMapEntryInfoList;

  procedure CalculateEntriesBounds(InfoList: TdxPSTTFMapEntryInfoList);
  var
    I, J: Integer;
  begin
    if ACharList.Count > 0 then
    begin
      J := 0;
      Result[0].EntryStarts := ACharList.CacheItem[0].CharCode;
      for I := 0 to ACharList.Count - 1 do
      begin
        if ACharList.CacheItem[I + 1].CharCode <> ACharList.CacheItem[I].CharCode + 1 then
        begin
          Result[J].EntryEnd := ACharList.CacheItem[I].CharCode;
          Result[J + 1].EntryStarts := ACharList.CacheItem[I + 1].CharCode;
          Inc(J);
        end;
      end;
      Result[J].EntryEnd := ACharList.CacheItem[ACharList.Count - 1].CharCode;
      Result[J + 1].EntryStarts := MAXWORD;
      Result[J + 1].EntryEnd := MAXWORD;
    end;
  end;

  function IsContiguousRange(const AEntryInfo: TdxPSTTFMapEntryInfo): Boolean;
  var
    I: Integer;
  begin
    Result := True;
    for I := AEntryInfo.EntryStarts to AEntryInfo.EntryEnd do
    begin
      Result := Map[I + 1] = Map[I] + 1;
      if not Result then
        Break;
      end;
  end;

  procedure CalculateOffsets(InfoList: TdxPSTTFMapEntryInfoList);
  var
    AEntryInfo: TdxPSTTFMapEntryInfo;
    AListLength: Integer;
    AStartGlyphIdOffset: Integer;
    I, ADelta: Integer;
  begin
    AStartGlyphIdOffset := 0;
    AListLength := Length(InfoList);
    for I := 0 to AListLength - 1 do
    begin
      AEntryInfo := InfoList[I];
      if IsContiguousRange(AEntryInfo) then
      begin
        ADelta := Map[AEntryInfo.EntryStarts] - AEntryInfo.EntryStarts;
        if ADelta < 0 then
          Inc(ADelta, $10000);
        AEntryInfo.RangeOffsets := 0;
        AEntryInfo.Delta := ADelta;
      end
      else
      begin
        AEntryInfo.RangeOffsets := AStartGlyphIdOffset + (AListLength - I) * 2;
        Inc(AStartGlyphIdOffset, (AEntryInfo.EntryEnd - AEntryInfo.EntryStarts + 1) * 2);
        AEntryInfo.Delta := 0;
      end;
      InfoList[I] := AEntryInfo;
    end;
  end;

begin
  SetLength(Result, CalculateSegmentCount(ACharList));
  CalculateEntriesBounds(Result);
  CalculateOffsets(Result);
end;

function TdxPSTTFCMapSection.GetMap(Index: Word): Word;
begin
  if ID = 0 then
    Index := Index or $F000;
  Result := FMap[Index];
end;

function TdxPSTTFCMapSection.GetSize: Integer;

  function CalculateMapSize: Integer;
  var
    AEntryInfo: TdxPSTTFMapEntryInfo;
    I, ACode: Integer;
  begin
    Result := 0;
    for I := 0 to Length(FEntryInfoList) - 1 do
      if FEntryInfoList[I].RangeOffsets <> 0 then
      begin
        AEntryInfo := FEntryInfoList[I];
        for ACode := AEntryInfo.EntryStarts to AEntryInfo.EntryEnd do
          Inc(Result, 2);
      end;
  end;

begin
  Result := 28 + CalculateMapSize + Length(FEntryInfoList) * SizeOf(TdxPSTTFMapEntryInfo);
end;

class function TdxPSTTFCMapSection.Tag: TdxPSTTFSectionTag;
begin
  Result := GetSectionTag('cmap');
end;

procedure TdxPSTTFCMapSection.ReadData(Stream: TdxPSTTFStream);

  function FindUnicodeTable(Stream: TdxPSTTFStream): Integer;
  var
    I, ATableNumber: Integer;
    pID: Word;
  begin
    Result := -1;
    ATableNumber := Stream.ReadWord;
    for I := 0 to ATableNumber - 1  do
    begin
      pID := Stream.ReadWord;
      FID := Stream.ReadWord;
      if (pID = 3) and ((ID = 1) or (ID = 0) or (ID = 3)) then
      begin
        Result := Stream.ReadLong;
        Break;
      end;
      Stream.Seek(SizeOf(LongInt));
    end;
  end;

var
  AUnicodeTableOffset: Integer;
begin
  FVersion := Stream.ReadWord;
  AUnicodeTableOffset := FindUnicodeTable(Stream);
  if AUnicodeTableOffset = -1 then
    raise EdxPSTTFFileException.Create(sdxCMapUnicodeTableMissing);
  ReadUnicodeTable(Stream, AUnicodeTableOffset);
end;

procedure TdxPSTTFCMapSection.ReadUnicodeTable(
  Stream: TdxPSTTFStream; AUnicodeTableOffset: Integer);
begin
  Stream.Position := SectionOffset + AUnicodeTableOffset;
  if Stream.ReadWord = 4 then
    ReadUnicodeTableVersion4(Stream)
  else
    raise EdxPSTTFFileException.Create(sdxCMapInvalidTableFormat);
end;

procedure TdxPSTTFCMapSection.ReadUnicodeTableVersion4(Stream: TdxPSTTFStream);

  procedure ReadEntriesInfo(var AEntriesInfo: TdxPSTTFMapEntryInfoList);
  var
    I, AEntriesCount: Integer;
  begin
    Stream.Seek(SizeOf(LongInt));
    AEntriesCount := Stream.ReadWord div 2;
    Stream.Seek(3 * SizeOf(Word));
    SetLength(AEntriesInfo, AEntriesCount);
    for I := 0 to AEntriesCount - 1 do
      AEntriesInfo[I].EntryEnd := Stream.ReadWord;
    Stream.Seek(SizeOf(Word));
    for I := 0 to AEntriesCount - 1 do
      AEntriesInfo[I].EntryStarts := Stream.ReadWord;
    for I := 0 to AEntriesCount - 1 do
      AEntriesInfo[I].Delta := Stream.ReadWord;
    for I := 0 to AEntriesCount - 1 do
      AEntriesInfo[I].RangeOffsets := Stream.ReadWord;
  end;

var
  AEntryInfo: TdxPSTTFMapEntryInfo;
  ASavedPosition: Integer;
  I, ACode, AGlyphIndex: Integer;
begin
  ZeroMemory(@FMap[0], SizeOf(FMap));
  ReadEntriesInfo(FEntryInfoList);
  ASavedPosition := Stream.Position;
  for I := 0 to Length(FEntryInfoList) - 1 do
  begin
    AEntryInfo := FEntryInfoList[I];
    for ACode := AEntryInfo.EntryStarts to AEntryInfo.EntryEnd do
    begin
      AGlyphIndex := ACode;
      if (AEntryInfo.RangeOffsets <> 0) and (ACode <> MaxWord) then
      begin
        Stream.Position := ASavedPosition + AEntryInfo.RangeOffsets +
          (ACode - AEntryInfo.EntryStarts + I - Length(FEntryInfoList)) * 2;
        AGlyphIndex := Stream.ReadWord;
      end;
      if AGlyphIndex <> 0 then
        FMap[ACode] := (AGlyphIndex + AEntryInfo.Delta) and $FFFF;
    end;
  end;
end;

procedure TdxPSTTFCMapSection.WriteData(Stream: TdxPSTTFStream);
var
  ASavedPosition: Integer;
begin
  ASavedPosition := Stream.Position;
  Stream.WriteWord(FVersion);
  Stream.WriteWord(1); // Number of tables
  Stream.WriteWord(3); // PlatformID
  Stream.WriteWord(1); // EncodingID
  Stream.WriteLong(Stream.Position - ASavedPosition + SizeOf(Cardinal));
  WriteUnicodeTable(Stream, FEntryInfoList);
end;

procedure TdxPSTTFCMapSection.WriteGlyphIndexes(
  Stream: TdxPSTTFStream; const AEntriesInfo: TdxPSTTFMapEntryInfoList);
var
  AEntryInfo: TdxPSTTFMapEntryInfo;
  I, ACode: Integer;
begin
  for I := 0 to Length(AEntriesInfo) - 1 do
  begin
    AEntryInfo := AEntriesInfo[I];
    if AEntryInfo.RangeOffsets <> 0 then
    begin
      for ACode := AEntryInfo.EntryStarts to AEntryInfo.EntryEnd do
        Stream.WriteWord(Map[ACode]);
    end;
  end;
end;

procedure TdxPSTTFCMapSection.WriteUnicodeTable(
  Stream: TdxPSTTFStream; const AEntriesInfo: TdxPSTTFMapEntryInfoList);
var
  AEntrySelector, ASearchRange: Word;
  ASegmentCount, ASavedPosition: Integer;
  I, ALength: Integer;
begin
  ASavedPosition := Stream.Position;
  ASegmentCount := Length(AEntriesInfo);
  Stream.WriteWord(4); // Format
  Stream.WriteLong(0); // Length
  Stream.WriteWord(ASegmentCount * 2);
  AEntrySelector := Floor(Log2(ASegmentCount));
  ASearchRange := Trunc(Power(2, AEntrySelector) * 2);
  Stream.WriteWord(ASearchRange);
  Stream.WriteWord(AEntrySelector);
  Stream.WriteWord(ASegmentCount * 2 - ASearchRange);
  for I := 0 to ASegmentCount - 1 do
    Stream.WriteWord(AEntriesInfo[I].EntryEnd);
  Stream.WriteWord(0); // Reserved Padding
  for I := 0 to ASegmentCount - 1 do
    Stream.WriteWord(AEntriesInfo[I].EntryStarts);
  for I := 0 to ASegmentCount - 1 do
    Stream.WriteWord(AEntriesInfo[I].Delta);
  for I := 0 to ASegmentCount - 1 do
    Stream.WriteWord(AEntriesInfo[I].RangeOffsets);
  WriteGlyphIndexes(Stream, AEntriesInfo);
  ALength := Stream.Position - ASavedPosition;
  Stream.Position := ASavedPosition + SizeOf(Word);
  Stream.WriteWord(ALength);
  Stream.Position := ASavedPosition + ALength;
end;

{ TdxPSTTFLocaSection }

destructor TdxPSTTFLocaSection.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TdxPSTTFLocaSection.CalculateOffsets(ACharList: TdxPSTTFCharCacheList);

  procedure PopulateOffsets(AStartIndex, AEndIndex, AValue: Integer);
  var
    I: Integer;
  begin
    for I := AStartIndex to AEndIndex - 1 do
      FOffsets[I] := AValue;
  end;

var
  AGlyphInfo: TdxPSTTFGlyphInfo;
  ALastOffset, ALastIndex: Integer;
  I: Integer;
begin
  ALastIndex := 0;
  ALastOffset := 0;
  ACharList.SortByGlyphIndex;
  for I := 0 to GlyphSection.GlyphInfoCount - 1 do
  begin
    AGlyphInfo := GlyphSection.GlyphInfo[I];
    PopulateOffsets(ALastIndex, AGlyphInfo.Index + 1, ALastOffset);
    Inc(ALastOffset, AGlyphInfo.DataSize);
    ALastIndex := AGlyphInfo.Index + 1;
  end;
  PopulateOffsets(ALastIndex, Length(FOffsets), ALastOffset);
end;

procedure TdxPSTTFLocaSection.Clear;
begin
  SetLength(FOffsets, 0);
end;

function TdxPSTTFLocaSection.GetGlyphSection: TdxPSTTFGlyphSection;
begin
  Result := Owner.GlyphSection;
end;

function TdxPSTTFLocaSection.GetIndexToLocFormat: Integer;
begin
  Result := Owner.HeadSection.IndexToLocFormat;
end;

function TdxPSTTFLocaSection.GetOffset(Index: Integer): Integer;
begin
  Result := FOffsets[Index];
end;

function TdxPSTTFLocaSection.GetSize: Integer;
begin
  if IndexToLocFormat = 1 then
    Result := Length(FOffsets) * 4
  else
    Result := Length(FOffsets) * 2;
end;

class function TdxPSTTFLocaSection.Tag: TdxPSTTFSectionTag;
begin
  Result := GetSectionTag('loca');
end;

procedure TdxPSTTFLocaSection.ReadData(Stream: TdxPSTTFStream);
var
  I: Integer;
begin
  Clear;
  SetLength(FOffsets, Owner.MaxPSection.NumGlyphs + 1);
  for I := 0 to Length(FOffsets) - 1 do
  begin
    if IndexToLocFormat = 1 then
      FOffsets[I] := Stream.ReadLong
    else
      FOffsets[I] := Stream.ReadWord shl 1;
  end;
end;

procedure TdxPSTTFLocaSection.WriteData(Stream: TdxPSTTFStream);
var
  I: Integer;
begin
  for I := 0 to Length(FOffsets) - 1 do
  begin
    if IndexToLocFormat = 1 then
      Stream.WriteLong(FOffsets[I])
    else
      Stream.WriteWord(FOffsets[I] shr 1);
  end;
end;

{ TdxPSTTFGlyphInfo }

constructor TdxPSTTFGlyphInfo.Create(AIndex, ASize: Integer);
begin
  inherited Create;
  FIndex := AIndex;
  FDataSize := ASize;
  FData := AllocMem(ASize);
end;

destructor TdxPSTTFGlyphInfo.Destroy;
begin
  FreeAndNil(FComponentGlyphs);
  if Assigned(Data) then
  begin
    FreeMem(FData, FDataSize);
    FDataSize := 0;
    FData := nil;
  end;
  inherited Destroy;
end;

function TdxPSTTFGlyphInfo.GetComponentCount: Integer;
begin
  if Assigned(FComponentGlyphs) then
    Result := FComponentGlyphs.Count
  else
    Result := 0;
end;

function TdxPSTTFGlyphInfo.GetComponentGlyph(Index: Integer): Word;
begin
  Result := Word(FComponentGlyphs.Items[Index]);
end;

procedure TdxPSTTFGlyphInfo.ReadData(Stream: TdxPSTTFStream);
const
  OffsetMap: array[Boolean] of Integer = (2, 4);
  GLYPH_HEADER_SIZE = 10; //Note: Flags + SizeBox = (1 + 4) Words
var
  AFlags, AGlyphIndex: Word;
  AOffset: Integer;
begin
  FreeAndNil(FComponentGlyphs);
  Stream.ReadRaw(Data^, DataSize);
  if PSmallInt(Data)^ = -1 then
  begin
    FComponentGlyphs := TList.Create;
    Stream.Seek(GLYPH_HEADER_SIZE - DataSize);
    while True do
    begin
      AFlags := Stream.ReadWord;
      AGlyphIndex := Stream.ReadWord;
      FComponentGlyphs.Add(Pointer(AGlyphIndex));
      if AFlags and GLYPH_MORE_COMPONENTS = 0 then
        Break;

      if AFlags and GLYPH_WE_HAVE_A_SCALE = 0 then
        AOffset := 0
      else
        if AFlags and GLYPH_WE_HAVE_A_TWO_BY_TWO <> 0 then
          AOffset := 32
        else
          if AFlags and GLYPH_WE_HAVE_AN_X_AND_Y_SCALE <> 0 then
            AOffset := 16
          else
            AOffset := 4;

      Stream.Seek(AOffset + OffsetMap[AFlags and GLYPH_ARG_1_AND_2_ARE_WORDS <> 0]);
    end;
  end;
end;

procedure TdxPSTTFGlyphInfo.WriteData(Stream: TdxPSTTFStream);
begin
  if Assigned(Data) then
    Stream.WriteRaw(Data^, DataSize);
end;

{ TdxPSTTFGlyphSection }

constructor TdxPSTTFGlyphSection.Create(AOwner: TdxPSTTFFile);
begin
  inherited Create(AOwner);
  FGlyphList := TcxObjectList.Create;
end;

destructor TdxPSTTFGlyphSection.Destroy;
begin
  FreeAndNil(FGlyphList);
  inherited Destroy;
end;

function TdxPSTTFGlyphSection.GetGlyphInfoByGlyphIndex(
  AGlyphIndex: Integer; out AGlyphInfo: TdxPSTTFGlyphInfo): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to GlyphInfoCount - 1 do
  begin
    Result := GlyphInfo[I].Index = AGlyphIndex;
    if Result then
    begin
      AGlyphInfo := GlyphInfo[I];
      Break;
    end;
  end;
end;

function TdxPSTTFGlyphSection.GetLocaSection: TdxPSTTFLocaSection;
begin
  Result := Owner.LocaSection;
end;

function TdxPSTTFGlyphSection.GetGlyphInfo(Index: Integer): TdxPSTTFGlyphInfo;
begin
  Result := TdxPSTTFGlyphInfo(FGlyphList.Items[Index]);
end;

function TdxPSTTFGlyphSection.GetGlyphInfoCount: Integer;
begin
  Result := FGlyphList.Count;
end;

function TdxPSTTFGlyphSection.GetSize: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to GlyphInfoCount - 1 do
    Inc(Result, GlyphInfo[I].DataSize);
end;

class function TdxPSTTFGlyphSection.Tag: TdxPSTTFSectionTag;
begin
  Result := GetSectionTag('glyf');
end;

procedure TdxPSTTFGlyphSection.ReadData(Stream: TdxPSTTFStream);
var
  AGlyphInfo: TdxPSTTFGlyphInfo;
  AGlyphOffset, AGlyphDataSize: Integer;
  I: Integer;
begin
  FGlyphList.Clear;
  FGlyphList.Capacity := Owner.MaxPSection.NumGlyphs;
  for I := 0 to Owner.MaxPSection.NumGlyphs - 1 do
  begin
    AGlyphOffset := LocaSection.Offset[I];
    AGlyphDataSize := LocaSection.Offset[I + 1] - AGlyphOffset;
    if AGlyphDataSize > 0 then
    begin
      AGlyphInfo := TdxPSTTFGlyphInfo.Create(I, AGlyphDataSize);
      Stream.Position := SectionOffset + AGlyphOffset;
      AGlyphInfo.ReadData(Stream);
      FGlyphList.Add(AGlyphInfo);
    end;
  end;
end;

procedure TdxPSTTFGlyphSection.RemoveUnusedGlyphs(ACharList: TdxPSTTFCharCacheList);

  procedure PopulateUsedList(AGlyphIndex: Integer; AList: TList);
  var
    AGlyphInfo: TdxPSTTFGlyphInfo;
    I: Integer;
  begin
    if GetGlyphInfoByGlyphIndex(AGlyphIndex, AGlyphInfo) then
    begin
      AList.Add(Pointer(AGlyphInfo.Index));
      for I := 0 to AGlyphInfo.ComponentCount - 1 do
        PopulateUsedList(AGlyphInfo.ComponentGlyph[I], AList);
    end;
  end;

var
  AUsedGlyphs: TList;
  I: Integer;
begin
  AUsedGlyphs := TList.Create;
  try
    AUsedGlyphs.Capacity := GlyphInfoCount;
    AUsedGlyphs.Add(nil); //note: null symbol used always
    for I := 0 to ACharList.Count - 1 do
      PopulateUsedList(ACharList.CacheItem[I].GlyphIndex, AUsedGlyphs);
    for I := GlyphInfoCount - 1 downto 0 do
    begin
      if AUsedGlyphs.IndexOf(Pointer(GlyphInfo[I].Index)) < 0 then
        FGlyphList.FreeAndDelete(I);
    end;
  finally
    AUsedGlyphs.Free;
  end;
end;

procedure TdxPSTTFGlyphSection.WriteData(Stream: TdxPSTTFStream);
var
  I: Integer;
begin
  for I := 0 to GlyphInfoCount - 1 do
    GlyphInfo[I].WriteData(Stream);
end;

{ TdxPSTTFCharCacheListItem }

constructor TdxPSTTFCharCacheListItem.Create(ACharCode, AGlyphIndex: Word);
begin
  FCharCode := ACharCode;
  FGlyphIndex := AGlyphIndex;
end;

{ TdxPSTTFCharCacheList }

constructor TdxPSTTFCharCacheList.Create;
begin
  inherited Create;
  FList := TcxObjectList.Create;
  FList.Capacity := MaxByte;
end;

destructor TdxPSTTFCharCacheList.Destroy;
begin
  FList.Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TdxPSTTFCharCacheList.Add(ACharCode, AGlyphIndex: Word);
begin
  if IndexOf(ACharCode) < 0 then
    FList.Add(TdxPSTTFCharCacheListItem.Create(ACharCode, AGlyphIndex));
end;

function TdxPSTTFCharCacheList.GetCacheItem(AIndex: Integer): TdxPSTTFCharCacheListItem;
begin
  Result := TdxPSTTFCharCacheListItem(FList.Items[AIndex]);
end;

function TdxPSTTFCharCacheList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TdxPSTTFCharCacheList.IndexOf(ACharCode: Word): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Count - 1 downto 0 do
    if CacheItem[I].CharCode = ACharCode then
    begin
      Result := I;
      Break;
    end;
end;

procedure TdxPSTTFCharCacheList.SortByGlyphIndex;
begin
  FList.Sort(@CompareCacheListItemByGlyphIndex);
end;

{ TdxPSTTFOS2Section }

class function TdxPSTTFOS2Section.Tag: TdxPSTTFSectionTag;
begin
  Result := GetSectionTag('OS/2');
end;

procedure TdxPSTTFOS2Section.ReadData(Stream: TdxPSTTFStream);
begin
  ZeroMemory(@FPanose, SizeOf(FPanose));
  Stream.Seek(4 * SizeOf(Word));
  FFontType := SmallInt(Stream.ReadWord);
  Stream.Seek(10 * SizeOf(Word));
  Stream.ReadRaw(FFamilyClass, SizeOf(FFamilyClass));
  Stream.ReadRaw(FFamilySubClass, SizeOf(FFamilySubClass));
  Stream.ReadRaw(FPanose, SizeOf(FPanose));
  Stream.Seek(4 * SizeOf(DWORD));
  Stream.ReadRaw(FVendor[0], SizeOf(FVendor));
  Stream.Seek(SizeOf(Word));
  FFirstCharIndex := Stream.ReadWord;
  FLastCharIndex := Stream.ReadWord;
end;

end.

