{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPDFViewer                                         }
{                                                                    }
{           Copyright (c) 2015-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPDFVIEWER AND ALL              }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit dxPDFFont;

{$I cxVer.inc}

interface

uses
  Types, SysUtils, Generics.Defaults, Generics.Collections, Classes, cxGeometry, cxClasses, dxPDFBase, dxPDFTypes,
  dxPDFCore, dxPDFCharacterMapping, dxPDFFontEncoding, dxPDFText, dxFontFile;

type
  { TdxPDFCompositeFont }

  TdxPDFCompositeFont = class(TdxPDFCustomFont)
  strict private
    FCIDBaseFont: string;
    FCIDToGIDMap: array of Word;
    FDefaultWidth: Integer;
    FEmbeddedCIDToGIDMapping: TDictionary<SmallInt, SmallInt>;
    FIsIdentityEncoding: Boolean;

    procedure ReadGlyphMap(ADictionary: TdxPDFReaderDictionary);
    procedure ReadWidthArray(AElementList: TdxPDFBaseList; var AStartIndex: Integer);
  private
    FCompactFontFileData: TBytes;
    FFontFileData: TBytes;
    FOpenTypeFontFileData: TBytes;
  protected
    function GetFontDictionary(ADictionary: TdxPDFReaderDictionary): TdxPDFReaderDictionary; override;
    function GetFontDescriptorDictionary(ADictionary: TdxPDFReaderDictionary): TdxPDFReaderDictionary; override;
    function CreateGlyphWidths: TdxPDFDoubleList; override;
    function GetCompactFontFileData: TBytes; override;
    function GetCharset: TDictionary<SmallInt, SmallInt>; override;
    function GetUseGlyphIndexes: Boolean; override;
    procedure DestroySubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;
    procedure ReadEncoding(ASourceObject: TdxPDFBase); override;
    procedure ReadFontDescriptor(ADictionary: TdxPDFReaderDictionary); override;
    procedure ReadWidths(ADictionary: TdxPDFReaderDictionary); override;

    procedure ReadFontFileData(ADictionary: TdxPDFReaderDictionary); virtual;
    procedure SetFontFileData(const AValue: TBytes); virtual;

    procedure UpdateCharset;

    property CompactFontFileData: TBytes read GetCompactFontFileData;
  public
    constructor Create(const ABaseFont: string; AFontDescriptor: TdxPDFFontDescriptor); override;
    class function GetDictionary(ADictionary: TdxPDFReaderDictionary): TdxPDFReaderDictionary; static;
    class function GetSubTypeName: string; override;
    procedure GetGlyphPositions(const AStringData: TdxPDFStringData; AFontSizeFactor, ACharacterSpacing, AWordSpacing,
      AScalingFactor: Double; var AResult: TDoubleDynArray);  override;
    procedure UpdateGlyphs(var AGlyphs: TSmallIntDynArray); override;

    property DefaultWidth: Integer read FDefaultWidth;
    property FontFileData: TBytes read FFontFileData write SetFontFileData;
    property Widths;
  end;

  { TdxPDFCIDType0Font }

  TdxPDFCIDType0Font = class(TdxPDFCompositeFont)
  strict private
    FActualCompactFontFileData: TBytes;
    FType1FontFileData: TdxPDFType1FontFileData;
  protected
    function GetActualCompactFontFileData: TBytes; override;
    function GetCompactFontFileData: TBytes; override;
    function GetUseGlyphIndexes: Boolean; override;
    procedure DestroySubClasses; override;
    procedure ReadFontFileData(ADictionary: TdxPDFReaderDictionary); override;
    procedure SetActualCompactFontFileData(const AValue: TBytes); override;
  public
    class function GetSubTypeName: string; override;
  end;

  { TdxPDFCIDType2Font }

  TdxPDFCIDType2Font = class(TdxPDFCompositeFont)
  strict private
    FPatchedFontFileData: TBytes;
    procedure PatchFontFileData(const AFontFileData: TBytes);
  protected
    function GetActualCompactFontFileData: TBytes; override;
    function GetTrueTypeFontFileData: TBytes; override;
    function GetUseGlyphIndexes: Boolean; override;
    procedure ReadFontFileData(ADictionary: TdxPDFReaderDictionary); override;
    procedure SetFontFileData(const AData: TBytes); override;

    function GetBaseFontName: string;
    function GetFontName: string;
    function GetFontFileData: TBytes;
    function GetDescriptor: TdxPDFFontDescriptor;
    function GetCompactFontFileData: TBytes; override;

    property PatchedFontFileData: TBytes read FPatchedFontFileData write FPatchedFontFileData;
  public
    class function GetSubTypeName: string; override;
  end;

  { TdxPDFDeferredCIDType2Font }

  TdxPDFDeferredCIDType2Font = class(TdxPDFCIDType2Font);

  { TdxPDFSimpleFont }

  TdxPDFSimpleFont = class(TdxPDFCustomFont)
  strict private
    FActualWidths: TdxPDFDoubleList;
    FFirstChar: Integer;
    FLastChar: Integer;
    FWidths: TDoubleDynArray;

    FUseGlyphIndexes: Boolean;

    FGlyphMapping: TdxType1FontGlyphMapping;
    FSIDToGIDMapping: TDictionary<SmallInt, SmallInt>;

    function CreateEncoding(ADictionary: TdxPDFReaderDictionary): TdxPDFSimpleFontEncoding;
    function GetGlyphMapping: TdxType1FontGlyphMapping;
    procedure UpdateType1FontProgramProperties;
    procedure UpdateWidthsList;
  protected
    function CreateGlyphWidths: TdxPDFDoubleList; override;
    function GetCharset: TDictionary<SmallInt, SmallInt>; override;
    function GetUseGlyphIndexes: Boolean; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure ReadEncoding(AObject: TdxPDFBase); override;
    procedure ReadProperties(ADictionary: TdxPDFReaderDictionary); override;
    procedure ReadWidths(ADictionary: TdxPDFReaderDictionary); override;

    function GetDefaultWidthsDictionary: TdxPDFWordDictionary;
    function IsCourierFont: Boolean; virtual;

    property GlyphMapping: TdxType1FontGlyphMapping read GetGlyphMapping;
    property LastChar: Integer read FLastChar;
  public
    constructor Create(const ABaseFont: string; AFontDescriptor: TdxPDFFontDescriptor; AEncoding: TdxPDFSimpleFontEncoding;
      AFirstChar: Integer; const AWidths: TDoubleDynArray); reintroduce; overload;
    procedure GetGlyphPositions(const AStringData: TdxPDFStringData;
      AFontSizeFactor, ACharacterSpacing, AWordSpacing, AScalingFactor: Double; var AResult: TDoubleDynArray); override;
    procedure UpdateGlyphs(var AGlyphs: TSmallIntDynArray); override;

    property ActualWidths: TdxPDFDoubleList read FActualWidths;
    property FirstChar: Integer read FFirstChar;
  end;

  { TdxPDFTrueTypeFont }

  TdxPDFTrueTypeFont = class(TdxPDFSimpleFont)
  strict private
    FEmbeddedFontEncoding: TSmallIntDynArray;
    FFontFileData: TBytes;
    FGlyphMapping: TdxPDFWordDictionary;
    FGlyphRanges: TList<TdxFontFileCMapGlyphRange>;
    FOpenTypeFontFileData: TBytes;
    FPatchedFontFileData: TBytes;
    FType1FontFileData: TdxPDFType1FontFileData;

    class procedure DoUpdateGlyphCodes(AEncoding: TdxPDFSimpleFontEncoding; AGlyphCodes: TdxPDFWordDictionary;
      var AGlyphs: TSmallIntDynArray);
    procedure ReadFontFileData(ADictionary: TdxPDFReaderDictionary);
  protected
    function IsCourierFont: Boolean; override;
    function GetTrueTypeFontFileData: TBytes; override;
    function GetType1FontFileData: TdxPDFType1FontFileData; override;
    function GetUseGlyphIndexes: Boolean; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;
  public
    class function GetSubTypeName: string; override;
    class procedure UpdateGlyphs(var AGlyphs: TSmallIntDynArray; AEncoding: TdxPDFSimpleFontEncoding); reintroduce; overload;

    procedure UpdateGlyphs(var AGlyphs: TSmallIntDynArray); overload; override;
  end;

  { TdxPDFDeferredTrueTypeFont }

  TdxPDFDeferredTrueTypeFont = class(TdxPDFTrueTypeFont)
  public
    constructor Create(const ABaseFont: string; AFontDescriptor: TdxPDFFontDescriptor; AEncoding: TdxPDFSimpleFontEncoding);
  end;

  { TdxPDFType3Font }

  TdxPDFType3Font = class(TdxPDFSimpleFont)
  strict private
    FCharProcs: TObjectDictionary<string, TdxPDFReferencedObjects>;
    FFontBBox: TdxRectF;
    FMatrix: TdxPDFTransformationMatrix;
    FWidthFactor: Double;
    FHeightFactor: Double;
    FResources: TdxPDFResources;

    procedure ReadCharProcs(ADictionary: TdxPDFReaderDictionary);
    procedure ReadFontMatrix(AArray: TdxPDFArray);
  protected
    function GetHeightFactor: Double; override;
    function GetWidthFactor: Double; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;
    procedure ReadProperties(ADictionary: TdxPDFReaderDictionary); override;

    property Resources: TdxPDFResources read FResources write FResources;
  public
    class function GetSubTypeName: string; override;

    property FontBBox: TdxRectF read FFontBBox;
    property CharProcs: TObjectDictionary<string, TdxPDFReferencedObjects> read FCharProcs;
    property Matrix: TdxPDFTransformationMatrix read FMatrix;
  end;

  { TdxPDFFontFactory }

  TdxPDFFontFactory = class(TdxPDFFactory<TdxPDFCustomFontClass>);

function dxPDFFontFactory: TdxPDFFontFactory;
function dxPDFUnicodeConverter: TdxPDFUnicodeConverter;

implementation

uses
  Windows, Math, StrUtils, dxCore, dxCoreClasses, dxPDFParser, dxPDFCommand, dxPDFType1Font, dxPDFFontUtils, dxPDFUtils;

type
  TdxPDFDictionaryAccess = class(TdxPDFDictionary);
  TdxPDFDocumentRepositoryAccess = class(TdxPDFDocumentRepository);
  TdxPDFObjectAccess = class(TdxPDFObject);
  TdxPDFNumericObjectAccess = class(TdxPDFNumericObject);
  TdxFontFileUnicodeConverterAccess = class(TdxFontFileUnicodeConverter);
  TdxPDFStringAccess = class(TdxPDFString);

  { TdxPDFType3FontContentStreamParser }

  TdxPDFType3FontContentStreamParser = class(TdxPDFCommandStreamParser)
  public
    class function Parse(const AData: TBytes; ARepository: TdxPDFCustomRepository;
      AResources: TdxPDFResources): TdxPDFReferencedObjects;
  end;

  { TdxPDFCompactFontFormatTopDictIndexWriter }

  TdxPDFCompactFontFormatTopDictIndexWriter = class
  strict private
    FGlobalSubrs: TdxCompactFontFormatBinaryIndex;
    FMajorVersion: Byte;
    FMinorVersion: Byte;
    FName: string;
    FOffsetOperators: TInterfaceList;
    FOperators: TObjectList<TdxCompactFontFormatDictIndexOperator>;
    FStringIndex: TdxCompactFontFormatStringIndex;
    procedure CalculateOffsets;
    procedure DoWrite(AStream: TdxFontFileStream);
    procedure Read(AFontProgram: TdxType1FontCompactFontProgram);
    procedure ReadCIDFontProgramOperators(AFontProgram: TdxType1FontCompactCIDFontProgram);
    procedure ReadCharsetOperator(ACharset: TdxType1FontCustomCharset);
    procedure ReadCharStringsOperator(ACharStrings: TList<TBytes>);
    procedure ReadEncodingOperator(AEncoding: TdxType1FontEncoding);
    procedure ReadFontInfoOperators(AFontInfo: TdxType1FontInfo);
    procedure ReadPrivateOperator(AData: TdxType1FontPrivateData);
  public
    constructor Create(AFontProgram: TdxType1FontCompactFontProgram);
    destructor Destroy; override;

    class function Write(AFontProgram: TdxType1FontCompactFontProgram): TBytes; static;
  end;

  { TdxPDFSimpleFontDefaultWidths }

  TdxPDFSimpleFontDefaultWidths = class
  strict private type
    TGetDefaultWidthsFunc = function: TdxPDFWordDictionary of object;
  strict private class var
    FStandardFontDefaultWidthsMap: TDictionary<string, TGetDefaultWidthsFunc>;

    FCourierBoldWidths: TdxPDFWordDictionary;
    FCourierBoldObliqueWidths: TdxPDFWordDictionary;
    FCourierObliqueWidths: TdxPDFWordDictionary;
    FCourierWidths: TdxPDFWordDictionary;
    FHelveticaBoldWidths: TdxPDFWordDictionary;
    FHelveticaBoldObliqueWidths: TdxPDFWordDictionary;
    FHelveticaObliqueWidths: TdxPDFWordDictionary;
    FHelveticaWidths: TdxPDFWordDictionary;
    FTimesBoldWidths: TdxPDFWordDictionary;
    FTimesBoldItalicWidths: TdxPDFWordDictionary;
    FTimesItalicWidths: TdxPDFWordDictionary;
    FTimesRomanWidths: TdxPDFWordDictionary;
    FSymbolWidths: TdxPDFWordDictionary;
    FZapfDingbatsWidths: TdxPDFWordDictionary;
    function GetCourierBoldWidths: TdxPDFWordDictionary;
    function GetCourierBoldObliqueWidths: TdxPDFWordDictionary;
    function GetCourierObliqueWidths: TdxPDFWordDictionary;
    function GetCourierWidths: TdxPDFWordDictionary;
    function GetHelveticaBoldWidths: TdxPDFWordDictionary;
    function GetHelveticaBoldObliqueWidths: TdxPDFWordDictionary;
    function GetHelveticaObliqueWidths: TdxPDFWordDictionary;
    function GetHelveticaWidths: TdxPDFWordDictionary;
    function GetTimesBoldWidths: TdxPDFWordDictionary;
    function GetTimesBoldItalicWidths: TdxPDFWordDictionary;
    function GetTimesItalicWidths: TdxPDFWordDictionary;
    function GetTimesRomanWidths: TdxPDFWordDictionary;
    function GetStandardFontDefaultWidthsMap: TDictionary<string, TGetDefaultWidthsFunc>;
    function GetSymbolWidths: TdxPDFWordDictionary;
    function GetZapfDingbatsWidths: TdxPDFWordDictionary;

    property StandardFontDefaultWidthsMap: TDictionary<string, TGetDefaultWidthsFunc> read GetStandardFontDefaultWidthsMap;
  public
    constructor Create;
    destructor Destroy; override;
    function GetDefaultWidthsDictionary(const AFontName: string): TdxPDFWordDictionary;
  end;

  { TdxFontFileCMapGlyphRangeComparer }

  TdxFontFileCMapGlyphRangeComparer = class(TInterfacedObject, IComparer<TdxFontFileCMapGlyphRange>)
  strict private
    function Compare(const Left, Right: TdxFontFileCMapGlyphRange): Integer;
  end;
var
  dxgPDFRegisteredFontClasses: TdxPDFFontFactory;
  dxgPDFUnicodeConverter: TdxPDFUnicodeConverter;
  dxgPDFSimpleFontDefaultWidths: TdxPDFSimpleFontDefaultWidths;

function dxPDFFontFactory: TdxPDFFontFactory;
begin
  if dxgPDFRegisteredFontClasses = nil then
    dxgPDFRegisteredFontClasses := TdxPDFFontFactory.Create;
  Result := dxgPDFRegisteredFontClasses;
end;

function dxPDFUnicodeConverter: TdxPDFUnicodeConverter;
begin
  if dxgPDFUnicodeConverter = nil then
    dxgPDFUnicodeConverter := TdxPDFUnicodeConverter.Create;
  Result := dxgPDFUnicodeConverter;
end;

function dxPDFSimpleFontDefaultWidths: TdxPDFSimpleFontDefaultWidths;
begin
  if dxgPDFSimpleFontDefaultWidths = nil then
    dxgPDFSimpleFontDefaultWidths := TdxPDFSimpleFontDefaultWidths.Create;
  Result := dxgPDFSimpleFontDefaultWidths;
end;

{ TdxFontFileCMapGlyphRangeComparer }

function TdxFontFileCMapGlyphRangeComparer.Compare(const Left, Right: TdxFontFileCMapGlyphRange): Integer;
begin
  Result := Left.StartValue - Right.StartValue;
end;

{ TdxPDFCompactFontFormatTopDictIndexWriter }

constructor TdxPDFCompactFontFormatTopDictIndexWriter.Create(AFontProgram: TdxType1FontCompactFontProgram);
begin
  inherited Create;
  FOffsetOperators := TInterfaceList.Create;
  FOperators := TObjectList<TdxCompactFontFormatDictIndexOperator>.Create;
end;

destructor TdxPDFCompactFontFormatTopDictIndexWriter.Destroy;
begin
  FreeAndNil(FOffsetOperators);
  FreeAndNil(FOperators);
  FreeAndNil(FGlobalSubrs);
  inherited Destroy;
end;

class function TdxPDFCompactFontFormatTopDictIndexWriter.Write(AFontProgram: TdxType1FontCompactFontProgram): TBytes;
var
  AStream: TdxFontFileStream;
  AWriter: TdxPDFCompactFontFormatTopDictIndexWriter;
begin
  AStream := TdxFontFileStream.Create;
  AWriter := TdxPDFCompactFontFormatTopDictIndexWriter.Create(AFontProgram);
  try
    AWriter.Read(AFontProgram);
    AWriter.DoWrite(AStream);
    Result := AStream.Data;
  finally
    AWriter.Free;
    AStream.Free;
  end;
end;

procedure TdxPDFCompactFontFormatTopDictIndexWriter.CalculateOffsets;
var
  I, AOffset, APrevSize: Integer;
  AOffsetOperator: IdxCompactFontFormatDictIndexOffsetOperator;
  ANeedRecalculate: Boolean;
begin
  ANeedRecalculate := True;
  while ANeedRecalculate do
  begin
    ANeedRecalculate := False;
    AOffset := Length(FName) + FStringIndex.DataLength + FGlobalSubrs.DataLength + 26;
    for I := 0 to FOperators.Count - 1 do
      Inc(AOffset, FOperators[I].GetSize(FStringIndex));
    for I := 0 to FOffsetOperators.Count - 1 do
    begin
      AOffsetOperator := FOffsetOperators[I] as IdxCompactFontFormatDictIndexOffsetOperator;
      APrevSize := AOffsetOperator.GetSize(FStringIndex);
      AOffsetOperator.Offset := AOffset;
      if AOffsetOperator.GetSize(FStringIndex) <> APrevSize then
      begin
        ANeedRecalculate := True;
        Break;
      end;
      Inc(AOffset, AOffsetOperator.Length);
    end;
  end;
end;

procedure TdxPDFCompactFontFormatTopDictIndexWriter.DoWrite(AStream: TdxFontFileStream);
var
  I: Integer;
  ANameIndex: TdxCompactFontFormatNameIndex;
  AOperator: TdxCompactFontFormatDictIndexOperator;
  ATopDictIndexStream: TdxFontFileStream;
  ATopDictIndex: TdxCompactFontFormatTopDictIndex;
  AOffsetOperator: IdxCompactFontFormatDictIndexOffsetOperator;
begin
  AStream.WriteByte(FMajorVersion);
  AStream.WriteByte(FMinorVersion);
  AStream.WriteByte(4);
  AStream.WriteByte(1);
  ANameIndex := TdxCompactFontFormatNameIndex.Create;
  try
    ANameIndex.AddString(FName);
    ANameIndex.Write(AStream);
  finally
    ANameIndex.Free;
  end;
  ATopDictIndexStream := TdxFontFileStream.Create;
  try
    for AOperator in FOperators do
      AOperator.Write(ATopDictIndexStream, FStringIndex);
    ATopDictIndex := TdxCompactFontFormatTopDictIndex.CreateEx(ATopDictIndexStream.Data);
    try
      ATopDictIndex.Write(AStream);
    finally
      ATopDictIndex.Free;
    end;
  finally
    ATopDictIndexStream.Free;
  end;
  FStringIndex.Write(AStream);
  FGlobalSubrs.Write(AStream);
  for I := 0 to FOffsetOperators.Count - 1 do
  begin
    AOffsetOperator := FOffsetOperators[I] as IdxCompactFontFormatDictIndexOffsetOperator;
    AOffsetOperator.WriteData(AStream);
  end;
end;

procedure TdxPDFCompactFontFormatTopDictIndexWriter.Read(AFontProgram: TdxType1FontCompactFontProgram);
begin
  FMajorVersion := AFontProgram.MajorVersion;
  FMinorVersion := AFontProgram.MinorVersion;
  FName := AFontProgram.FontName;
  FGlobalSubrs := TdxCompactFontFormatBinaryIndex.CreateEx(AFontProgram.GlobalSubrs);
  FStringIndex := AFontProgram.StringIndex;
  if AFontProgram is TdxType1FontCompactCIDFontProgram then
    ReadCIDFontProgramOperators(TdxType1FontCompactCIDFontProgram(AFontProgram));
  ReadFontInfoOperators(AFontProgram.FontInfo);
  if AFontProgram.FontType <> TdxType1FontCompactFontProgram.DefaultFontType then
    FOperators.Add(TdxCompactFontFormatDictIndexCharstringTypeOperator.Create(AFontProgram.FontType));
  if AFontProgram.UniqueID <> TdxType1CustomFontProgram.DefaultUniqueID then
    FOperators.Add(TdxCompactFontFormatDictIndexUniqueIDOperator.Create(AFontProgram.UniqueID));
  if AFontProgram.FontBBox <> TdxType1FontCompactFontProgram.DefaultFontBBox then
    FOperators.Add(TdxCompactFontFormatDictIndexFontBBoxOperator.Create(AFontProgram.FontBBox));
  if AFontProgram.StrokeWidth <> TdxType1CustomFontProgram.DefaultStrokeWidth then
    FOperators.Add(TdxCompactFontFormatDictIndexStrokeWidthOperator.Create(AFontProgram.StrokeWidth));
  if Length(AFontProgram.XUID) > 0 then
    FOperators.Add(TdxCompactFontFormatDictIndexXUIDOperator.Create(AFontProgram.XUID));
  ReadEncodingOperator(AFontProgram.Encoding);
  ReadCharsetOperator(AFontProgram.Charset);
  ReadPrivateOperator(AFontProgram.PrivateData);
  ReadCharStringsOperator(AFontProgram.CharStrings);
  if AFontProgram.PostScript <> '' then
    FOperators.Add(TdxCompactFontFormatDictIndexPostScriptOperator.Create(AFontProgram.PostScript));
  if AFontProgram.FontInfo.BaseFontName <> '' then
    FOperators.Add(TdxCompactFontFormatDictIndexBaseFontNameOperator.Create(AFontProgram.FontInfo.BaseFontName));
  if Length(AFontProgram.BaseFontBlend) > 0 then
    FOperators.Add(TdxCompactFontFormatDictIndexBaseFontBlendOperator.Create(AFontProgram.BaseFontBlend));
  if AFontProgram.CIDFontName <> '' then
    FOperators.Add(TdxCompactFontFormatDictIndexFontNameOperator.Create(AFontProgram.CIDFontName));
  CalculateOffsets;
end;

procedure TdxPDFCompactFontFormatTopDictIndexWriter.ReadCIDFontProgramOperators(AFontProgram: TdxType1FontCompactCIDFontProgram);
var
  I: Integer;
  AFDArrayOperator: TdxCompactFontFormatDictIndexFDArrayOperator;
  AFDSelectOperator: TdxCompactFontFormatDictIndexFDSelectOperator;
  AOffsetOperators: TInterfaceList;
begin
  FOperators.Add(TdxCompactFontFormatDictIndexROSOperator.Create(AFontProgram.Registry, AFontProgram.Ordering,
    AFontProgram.Supplement));
  if AFontProgram.CIDFontVersion <> TdxType1FontCompactCIDFontProgram.DefaultCIDFontVersion then
    FOperators.Add(TdxCompactFontFormatDictIndexCIDFontVersionOperator.Create(AFontProgram.CIDFontVersion));

  if AFontProgram.CIDCount <> TdxType1FontCompactCIDFontProgram.DefaultCIDCount then
    FOperators.Add(TdxCompactFontFormatDictIndexCIDCountOperator.Create(AFontProgram.CIDCount));

  if TdxPDFUtils.IsIntegerValid(AFontProgram.UIDBase) then
    FOperators.Add(TdxCompactFontFormatDictIndexUIDBaseOperator.Create(AFontProgram.UIDBase));

  if AFontProgram.GlyphGroupSelector <> nil then
  begin
    AFDSelectOperator := TdxCompactFontFormatDictIndexFDSelectOperator.Create(AFontProgram.GlyphGroupSelector);
    FOperators.Add(AFDSelectOperator);
    FOffsetOperators.Add(AFDSelectOperator);
  end;

  if AFontProgram.GlyphGroupData <> nil then
  begin
    AFDArrayOperator := TdxCompactFontFormatDictIndexFDArrayOperator.Create(AFontProgram.GlyphGroupData, FStringIndex);
    FOperators.Add(AFDArrayOperator);
    FOffsetOperators.Add(AFDArrayOperator);
    AOffsetOperators := AFDArrayOperator.OffsetOperators;
    try
      for I := 0 to AOffsetOperators.Count - 1 do
        FOffsetOperators.Add(AOffsetOperators[I] as IdxCompactFontFormatDictIndexOffsetOperator);
    finally
      AOffsetOperators.Free;
    end;
  end;
end;

procedure TdxPDFCompactFontFormatTopDictIndexWriter.ReadCharsetOperator(ACharset: TdxType1FontCustomCharset);
var
  AOperator: TdxCompactFontFormatDictIndexCharsetOperator;
begin
  if (ACharset <> nil) and not ACharset.IsDefault then
  begin
    AOperator := TdxCompactFontFormatDictIndexCharsetOperator.Create(ACharset);
    FOperators.Add(AOperator);
    FOffsetOperators.Add(AOperator);
  end;
end;

procedure TdxPDFCompactFontFormatTopDictIndexWriter.ReadCharStringsOperator(ACharStrings: TList<TBytes>);
var
  AOperator: TdxCompactFontFormatDictIndexCharStringsOperator;
begin
  if ACharStrings <> nil then
  begin
    AOperator := TdxCompactFontFormatDictIndexCharStringsOperator.Create(ACharStrings);
    FOperators.Add(AOperator);
    FOffsetOperators.Add(AOperator);
  end;
end;

procedure TdxPDFCompactFontFormatTopDictIndexWriter.ReadEncodingOperator(AEncoding: TdxType1FontEncoding);
var
  AOperator: TdxCompactFontFormatDictIndexEncodingOperator;
begin
  if (AEncoding <> nil) and not AEncoding.IsDefault then
  begin
    AOperator := TdxCompactFontFormatDictIndexEncodingOperator.Create(AEncoding);
    FOperators.Add(AOperator);
    if AEncoding.Offset = 0 then
      FOffsetOperators.Add(AOperator);
  end;
end;

procedure TdxPDFCompactFontFormatTopDictIndexWriter.ReadFontInfoOperators(AFontInfo: TdxType1FontInfo);
begin
  if AFontInfo.Version <> '' then
    FOperators.Add(TdxCompactFontFormatDictIndexVersionOperator.Create(AFontInfo.Version));
  if AFontInfo.Notice <> '' then
    FOperators.Add(TdxCompactFontFormatDictIndexNoticeOperator.Create(AFontInfo.Notice));
  if AFontInfo.Copyright <> '' then
    FOperators.Add(TdxCompactFontFormatDictIndexCopyrightOperator.Create(AFontInfo.Copyright));
  if AFontInfo.FullName <> '' then
    FOperators.Add(TdxCompactFontFormatDictIndexFullNameOperator.Create(AFontInfo.FullName));
  if AFontInfo.FamilyName <> '' then
    FOperators.Add(TdxCompactFontFormatDictIndexFamilyNameOperator.Create(AFontInfo.FamilyName));
  if AFontInfo.Weight <> '' then
    FOperators.Add(TdxCompactFontFormatDictIndexWeightOperator.Create(AFontInfo.Weight));
  if AFontInfo.IsFixedPitch then
    FOperators.Add(TdxCompactFontFormatDictIndexIsFixedPitchOperator.Create(True));
  if AFontInfo.ItalicAngle <> TdxType1FontInfo.DefaultItalicAngle then
    FOperators.Add(TdxCompactFontFormatDictIndexItalicAngleOperator.Create(AFontInfo.ItalicAngle));
  if AFontInfo.UnderlinePosition <> TdxType1FontInfo.DefaultUnderlinePosition then
    FOperators.Add(TdxCompactFontFormatDictIndexUnderlinePositionOperator.Create(AFontInfo.UnderlinePosition));
  if AFontInfo.UnderlineThickness <> TdxType1FontInfo.DefaultUnderlineThickness then
    FOperators.Add(TdxCompactFontFormatDictIndexUnderlineThicknessOperator.Create(AFontInfo.UnderlineThickness));
end;

procedure TdxPDFCompactFontFormatTopDictIndexWriter.ReadPrivateOperator(AData: TdxType1FontPrivateData);
var
  APrivateOperator: TdxCompactFontFormatDictIndexPrivateOperator;
begin
  if AData <> nil then
  begin
    APrivateOperator := TdxCompactFontFormatDictIndexPrivateOperator.Create(AData);
    FOperators.Add(APrivateOperator);
    FOffsetOperators.Add(APrivateOperator);
  end;
end;

{ TdxPDFType3FontContentStreamParser }

class function TdxPDFType3FontContentStreamParser.Parse(const AData: TBytes; ARepository: TdxPDFCustomRepository;
  AResources: TdxPDFResources): TdxPDFReferencedObjects;
var
  AParser: TdxPDFType3FontContentStreamParser;
begin
  Result := TdxPDFReferencedObjects.Create;
  AParser := TdxPDFType3FontContentStreamParser.Create(ARepository);
  try
    AParser.Read(AData, Result, AResources);
  finally
    AParser.Free;
  end;
end;

{ TdxPDFSimpleFontDefaultWidths }

constructor TdxPDFSimpleFontDefaultWidths.Create;
begin
  FStandardFontDefaultWidthsMap := TDictionary<string, TGetDefaultWidthsFunc>.Create;
  FCourierBoldWidths := TdxPDFWordDictionary.Create;
  FCourierBoldObliqueWidths := TdxPDFWordDictionary.Create;
  FCourierObliqueWidths := TdxPDFWordDictionary.Create;
  FCourierWidths := TdxPDFWordDictionary.Create;
  FHelveticaBoldWidths := TdxPDFWordDictionary.Create;
  FHelveticaBoldObliqueWidths := TdxPDFWordDictionary.Create;
  FHelveticaObliqueWidths := TdxPDFWordDictionary.Create;
  FHelveticaWidths := TdxPDFWordDictionary.Create;
  FTimesBoldWidths := TdxPDFWordDictionary.Create;
  FTimesBoldItalicWidths := TdxPDFWordDictionary.Create;
  FTimesItalicWidths := TdxPDFWordDictionary.Create;
  FTimesRomanWidths := TdxPDFWordDictionary.Create;
  FSymbolWidths := TdxPDFWordDictionary.Create;
  FZapfDingbatsWidths := TdxPDFWordDictionary.Create;
end;

destructor TdxPDFSimpleFontDefaultWidths.Destroy;
begin
  FreeAndNil(FZapfDingbatsWidths);
  FreeAndNil(FSymbolWidths);
  FreeAndNil(FTimesRomanWidths);
  FreeAndNil(FTimesItalicWidths);
  FreeAndNil(FTimesBoldItalicWidths);
  FreeAndNil(FTimesBoldWidths);
  FreeAndNil(FHelveticaWidths);
  FreeAndNil(FHelveticaObliqueWidths);
  FreeAndNil(FHelveticaBoldObliqueWidths);
  FreeAndNil(FHelveticaBoldWidths);
  FreeAndNil(FCourierWidths);
  FreeAndNil(FCourierObliqueWidths);
  FreeAndNil(FCourierBoldObliqueWidths);
  FreeAndNil(FCourierBoldWidths);
  FreeAndNil(FStandardFontDefaultWidthsMap);
end;

function TdxPDFSimpleFontDefaultWidths.GetDefaultWidthsDictionary(const AFontName: string): TdxPDFWordDictionary;
var
  AFunc: TGetDefaultWidthsFunc;
begin
  if StandardFontDefaultWidthsMap.TryGetValue(AFontName, AFunc) then
    Result := AFunc
  else
    Result := nil;
end;

function TdxPDFSimpleFontDefaultWidths.GetCourierBoldWidths: TdxPDFWordDictionary;
begin
  if FCourierBoldWidths.Count = 0 then
  begin
    FCourierBoldWidths.Add('a', 600);
    FCourierBoldWidths.Add('A', 600);
    FCourierBoldWidths.Add('aacute', 600);
    FCourierBoldWidths.Add('Aacute', 600);
    FCourierBoldWidths.Add('abreve', 600);
    FCourierBoldWidths.Add('Abreve', 600);
    FCourierBoldWidths.Add('acircumflex', 600);
    FCourierBoldWidths.Add('Acircumflex', 600);
    FCourierBoldWidths.Add('acute', 600);
    FCourierBoldWidths.Add('adieresis', 600);
    FCourierBoldWidths.Add('Adieresis', 600);
    FCourierBoldWidths.Add('ae', 600);
    FCourierBoldWidths.Add('AE', 600);
    FCourierBoldWidths.Add('agrave', 600);
    FCourierBoldWidths.Add('Agrave', 600);
    FCourierBoldWidths.Add('amacron', 600);
    FCourierBoldWidths.Add('Amacron', 600);
    FCourierBoldWidths.Add('ampersand', 600);
    FCourierBoldWidths.Add('aogonek', 600);
    FCourierBoldWidths.Add('Aogonek', 600);
    FCourierBoldWidths.Add('aring', 600);
    FCourierBoldWidths.Add('Aring', 600);
    FCourierBoldWidths.Add('asciicircum', 600);
    FCourierBoldWidths.Add('asciitilde', 600);
    FCourierBoldWidths.Add('asterisk', 600);
    FCourierBoldWidths.Add('at', 600);
    FCourierBoldWidths.Add('atilde', 600);
    FCourierBoldWidths.Add('Atilde', 600);
    FCourierBoldWidths.Add('b', 600);
    FCourierBoldWidths.Add('B', 600);
    FCourierBoldWidths.Add('backslash', 600);
    FCourierBoldWidths.Add('bar', 600);
    FCourierBoldWidths.Add('braceleft', 600);
    FCourierBoldWidths.Add('braceright', 600);
    FCourierBoldWidths.Add('bracketleft', 600);
    FCourierBoldWidths.Add('bracketright', 600);
    FCourierBoldWidths.Add('breve', 600);
    FCourierBoldWidths.Add('brokenbar', 600);
    FCourierBoldWidths.Add('bullet', 600);
    FCourierBoldWidths.Add('c', 600);
    FCourierBoldWidths.Add('C', 600);
    FCourierBoldWidths.Add('cacute', 600);
    FCourierBoldWidths.Add('Cacute', 600);
    FCourierBoldWidths.Add('caron', 600);
    FCourierBoldWidths.Add('ccaron', 600);
    FCourierBoldWidths.Add('Ccaron', 600);
    FCourierBoldWidths.Add('ccedilla', 600);
    FCourierBoldWidths.Add('Ccedilla', 600);
    FCourierBoldWidths.Add('cedilla', 600);
    FCourierBoldWidths.Add('cent', 600);
    FCourierBoldWidths.Add('circumflex', 600);
    FCourierBoldWidths.Add('colon', 600);
    FCourierBoldWidths.Add('comma', 600);
    FCourierBoldWidths.Add('commaaccent', 600);
    FCourierBoldWidths.Add('copyright', 600);
    FCourierBoldWidths.Add('currency', 600);
    FCourierBoldWidths.Add('d', 600);
    FCourierBoldWidths.Add('D', 600);
    FCourierBoldWidths.Add('dagger', 600);
    FCourierBoldWidths.Add('daggerdbl', 600);
    FCourierBoldWidths.Add('dcaron', 600);
    FCourierBoldWidths.Add('Dcaron', 600);
    FCourierBoldWidths.Add('dcroat', 600);
    FCourierBoldWidths.Add('Dcroat', 600);
    FCourierBoldWidths.Add('degree', 600);
    FCourierBoldWidths.Add('Delta', 600);
    FCourierBoldWidths.Add('dieresis', 600);
    FCourierBoldWidths.Add('divide', 600);
    FCourierBoldWidths.Add('dollar', 600);
    FCourierBoldWidths.Add('dotaccent', 600);
    FCourierBoldWidths.Add('dotlessi', 600);
    FCourierBoldWidths.Add('e', 600);
    FCourierBoldWidths.Add('E', 600);
    FCourierBoldWidths.Add('eacute', 600);
    FCourierBoldWidths.Add('Eacute', 600);
    FCourierBoldWidths.Add('ecaron', 600);
    FCourierBoldWidths.Add('Ecaron', 600);
    FCourierBoldWidths.Add('ecircumflex', 600);
    FCourierBoldWidths.Add('Ecircumflex', 600);
    FCourierBoldWidths.Add('edieresis', 600);
    FCourierBoldWidths.Add('Edieresis', 600);
    FCourierBoldWidths.Add('edotaccent', 600);
    FCourierBoldWidths.Add('Edotaccent', 600);
    FCourierBoldWidths.Add('egrave', 600);
    FCourierBoldWidths.Add('Egrave', 600);
    FCourierBoldWidths.Add('eight', 600);
    FCourierBoldWidths.Add('ellipsis', 600);
    FCourierBoldWidths.Add('emacron', 600);
    FCourierBoldWidths.Add('Emacron', 600);
    FCourierBoldWidths.Add('emdash', 600);
    FCourierBoldWidths.Add('endash', 600);
    FCourierBoldWidths.Add('eogonek', 600);
    FCourierBoldWidths.Add('Eogonek', 600);
    FCourierBoldWidths.Add('equal', 600);
    FCourierBoldWidths.Add('eth', 600);
    FCourierBoldWidths.Add('Eth', 600);
    FCourierBoldWidths.Add('Euro', 600);
    FCourierBoldWidths.Add('exclam', 600);
    FCourierBoldWidths.Add('exclamdown', 600);
    FCourierBoldWidths.Add('f', 600);
    FCourierBoldWidths.Add('F', 600);
    FCourierBoldWidths.Add('fi', 600);
    FCourierBoldWidths.Add('five', 600);
    FCourierBoldWidths.Add('fl', 600);
    FCourierBoldWidths.Add('florin', 600);
    FCourierBoldWidths.Add('four', 600);
    FCourierBoldWidths.Add('fraction', 600);
    FCourierBoldWidths.Add('g', 600);
    FCourierBoldWidths.Add('G', 600);
    FCourierBoldWidths.Add('gbreve', 600);
    FCourierBoldWidths.Add('Gbreve', 600);
    FCourierBoldWidths.Add('gcommaaccent', 600);
    FCourierBoldWidths.Add('Gcommaaccent', 600);
    FCourierBoldWidths.Add('germandbls', 600);
    FCourierBoldWidths.Add('grave', 600);
    FCourierBoldWidths.Add('greater', 600);
    FCourierBoldWidths.Add('greaterequal', 600);
    FCourierBoldWidths.Add('guillemotleft', 600);
    FCourierBoldWidths.Add('guillemotright', 600);
    FCourierBoldWidths.Add('guilsinglleft', 600);
    FCourierBoldWidths.Add('guilsinglright', 600);
    FCourierBoldWidths.Add('h', 600);
    FCourierBoldWidths.Add('H', 600);
    FCourierBoldWidths.Add('hungarumlaut', 600);
    FCourierBoldWidths.Add('hyphen', 600);
    FCourierBoldWidths.Add('i', 600);
    FCourierBoldWidths.Add('I', 600);
    FCourierBoldWidths.Add('iacute', 600);
    FCourierBoldWidths.Add('Iacute', 600);
    FCourierBoldWidths.Add('icircumflex', 600);
    FCourierBoldWidths.Add('Icircumflex', 600);
    FCourierBoldWidths.Add('idieresis', 600);
    FCourierBoldWidths.Add('Idieresis', 600);
    FCourierBoldWidths.Add('Idotaccent', 600);
    FCourierBoldWidths.Add('igrave', 600);
    FCourierBoldWidths.Add('Igrave', 600);
    FCourierBoldWidths.Add('imacron', 600);
    FCourierBoldWidths.Add('Imacron', 600);
    FCourierBoldWidths.Add('iogonek', 600);
    FCourierBoldWidths.Add('Iogonek', 600);
    FCourierBoldWidths.Add('j', 600);
    FCourierBoldWidths.Add('J', 600);
    FCourierBoldWidths.Add('k', 600);
    FCourierBoldWidths.Add('K', 600);
    FCourierBoldWidths.Add('kcommaaccent', 600);
    FCourierBoldWidths.Add('Kcommaaccent', 600);
    FCourierBoldWidths.Add('l', 600);
    FCourierBoldWidths.Add('L', 600);
    FCourierBoldWidths.Add('lacute', 600);
    FCourierBoldWidths.Add('Lacute', 600);
    FCourierBoldWidths.Add('lcaron', 600);
    FCourierBoldWidths.Add('Lcaron', 600);
    FCourierBoldWidths.Add('lcommaaccent', 600);
    FCourierBoldWidths.Add('Lcommaaccent', 600);
    FCourierBoldWidths.Add('less', 600);
    FCourierBoldWidths.Add('lessequal', 600);
    FCourierBoldWidths.Add('logicalnot', 600);
    FCourierBoldWidths.Add('lozenge', 600);
    FCourierBoldWidths.Add('lslash', 600);
    FCourierBoldWidths.Add('Lslash', 600);
    FCourierBoldWidths.Add('m', 600);
    FCourierBoldWidths.Add('M', 600);
    FCourierBoldWidths.Add('macron', 600);
    FCourierBoldWidths.Add('minus', 600);
    FCourierBoldWidths.Add('mu', 600);
    FCourierBoldWidths.Add('multiply', 600);
    FCourierBoldWidths.Add('n', 600);
    FCourierBoldWidths.Add('N', 600);
    FCourierBoldWidths.Add('nacute', 600);
    FCourierBoldWidths.Add('Nacute', 600);
    FCourierBoldWidths.Add('ncaron', 600);
    FCourierBoldWidths.Add('Ncaron', 600);
    FCourierBoldWidths.Add('ncommaaccent', 600);
    FCourierBoldWidths.Add('Ncommaaccent', 600);
    FCourierBoldWidths.Add('nine', 600);
    FCourierBoldWidths.Add('notequal', 600);
    FCourierBoldWidths.Add('ntilde', 600);
    FCourierBoldWidths.Add('Ntilde', 600);
    FCourierBoldWidths.Add('numbersign', 600);
    FCourierBoldWidths.Add('o', 600);
    FCourierBoldWidths.Add('O', 600);
    FCourierBoldWidths.Add('oacute', 600);
    FCourierBoldWidths.Add('Oacute', 600);
    FCourierBoldWidths.Add('ocircumflex', 600);
    FCourierBoldWidths.Add('Ocircumflex', 600);
    FCourierBoldWidths.Add('odieresis', 600);
    FCourierBoldWidths.Add('Odieresis', 600);
    FCourierBoldWidths.Add('oe', 600);
    FCourierBoldWidths.Add('OE', 600);
    FCourierBoldWidths.Add('ogonek', 600);
    FCourierBoldWidths.Add('ograve', 600);
    FCourierBoldWidths.Add('Ograve', 600);
    FCourierBoldWidths.Add('ohungarumlaut', 600);
    FCourierBoldWidths.Add('Ohungarumlaut', 600);
    FCourierBoldWidths.Add('omacron', 600);
    FCourierBoldWidths.Add('Omacron', 600);
    FCourierBoldWidths.Add('one', 600);
    FCourierBoldWidths.Add('onehalf', 600);
    FCourierBoldWidths.Add('onequarter', 600);
    FCourierBoldWidths.Add('onesuperior', 600);
    FCourierBoldWidths.Add('ordfeminine', 600);
    FCourierBoldWidths.Add('ordmasculine', 600);
    FCourierBoldWidths.Add('oslash', 600);
    FCourierBoldWidths.Add('Oslash', 600);
    FCourierBoldWidths.Add('otilde', 600);
    FCourierBoldWidths.Add('Otilde', 600);
    FCourierBoldWidths.Add('p', 600);
    FCourierBoldWidths.Add('P', 600);
    FCourierBoldWidths.Add('paragraph', 600);
    FCourierBoldWidths.Add('parenleft', 600);
    FCourierBoldWidths.Add('parenright', 600);
    FCourierBoldWidths.Add('partialdiff', 600);
    FCourierBoldWidths.Add('percent', 600);
    FCourierBoldWidths.Add('period', 600);
    FCourierBoldWidths.Add('periodcentered', 600);
    FCourierBoldWidths.Add('perthousand', 600);
    FCourierBoldWidths.Add('plus', 600);
    FCourierBoldWidths.Add('plusminus', 600);
    FCourierBoldWidths.Add('q', 600);
    FCourierBoldWidths.Add('Q', 600);
    FCourierBoldWidths.Add('question', 600);
    FCourierBoldWidths.Add('questiondown', 600);
    FCourierBoldWidths.Add('quotedbl', 600);
    FCourierBoldWidths.Add('quotedblbase', 600);
    FCourierBoldWidths.Add('quotedblleft', 600);
    FCourierBoldWidths.Add('quotedblright', 600);
    FCourierBoldWidths.Add('quoteleft', 600);
    FCourierBoldWidths.Add('quoteright', 600);
    FCourierBoldWidths.Add('quotesinglbase', 600);
    FCourierBoldWidths.Add('quotesingle', 600);
    FCourierBoldWidths.Add('r', 600);
    FCourierBoldWidths.Add('R', 600);
    FCourierBoldWidths.Add('racute', 600);
    FCourierBoldWidths.Add('Racute', 600);
    FCourierBoldWidths.Add('radical', 600);
    FCourierBoldWidths.Add('rcaron', 600);
    FCourierBoldWidths.Add('Rcaron', 600);
    FCourierBoldWidths.Add('rcommaaccent', 600);
    FCourierBoldWidths.Add('Rcommaaccent', 600);
    FCourierBoldWidths.Add('registered', 600);
    FCourierBoldWidths.Add('ring', 600);
    FCourierBoldWidths.Add('s', 600);
    FCourierBoldWidths.Add('S', 600);
    FCourierBoldWidths.Add('sacute', 600);
    FCourierBoldWidths.Add('Sacute', 600);
    FCourierBoldWidths.Add('scaron', 600);
    FCourierBoldWidths.Add('Scaron', 600);
    FCourierBoldWidths.Add('scedilla', 600);
    FCourierBoldWidths.Add('Scedilla', 600);
    FCourierBoldWidths.Add('scommaaccent', 600);
    FCourierBoldWidths.Add('Scommaaccent', 600);
    FCourierBoldWidths.Add('section', 600);
    FCourierBoldWidths.Add('semicolon', 600);
    FCourierBoldWidths.Add('seven', 600);
    FCourierBoldWidths.Add('six', 600);
    FCourierBoldWidths.Add('slash', 600);
    FCourierBoldWidths.Add('space', 600);
    FCourierBoldWidths.Add('sterling', 600);
    FCourierBoldWidths.Add('summation', 600);
    FCourierBoldWidths.Add('t', 600);
    FCourierBoldWidths.Add('T', 600);
    FCourierBoldWidths.Add('tcaron', 600);
    FCourierBoldWidths.Add('Tcaron', 600);
    FCourierBoldWidths.Add('tcommaaccent', 600);
    FCourierBoldWidths.Add('Tcommaaccent', 600);
    FCourierBoldWidths.Add('thorn', 600);
    FCourierBoldWidths.Add('Thorn', 600);
    FCourierBoldWidths.Add('three', 600);
    FCourierBoldWidths.Add('threequarters', 600);
    FCourierBoldWidths.Add('threesuperior', 600);
    FCourierBoldWidths.Add('tilde', 600);
    FCourierBoldWidths.Add('trademark', 600);
    FCourierBoldWidths.Add('two', 600);
    FCourierBoldWidths.Add('twosuperior', 600);
    FCourierBoldWidths.Add('u', 600);
    FCourierBoldWidths.Add('U', 600);
    FCourierBoldWidths.Add('uacute', 600);
    FCourierBoldWidths.Add('Uacute', 600);
    FCourierBoldWidths.Add('ucircumflex', 600);
    FCourierBoldWidths.Add('Ucircumflex', 600);
    FCourierBoldWidths.Add('udieresis', 600);
    FCourierBoldWidths.Add('Udieresis', 600);
    FCourierBoldWidths.Add('ugrave', 600);
    FCourierBoldWidths.Add('Ugrave', 600);
    FCourierBoldWidths.Add('uhungarumlaut', 600);
    FCourierBoldWidths.Add('Uhungarumlaut', 600);
    FCourierBoldWidths.Add('umacron', 600);
    FCourierBoldWidths.Add('Umacron', 600);
    FCourierBoldWidths.Add('underscore', 600);
    FCourierBoldWidths.Add('uogonek', 600);
    FCourierBoldWidths.Add('Uogonek', 600);
    FCourierBoldWidths.Add('uring', 600);
    FCourierBoldWidths.Add('Uring', 600);
    FCourierBoldWidths.Add('v', 600);
    FCourierBoldWidths.Add('V', 600);
    FCourierBoldWidths.Add('w', 600);
    FCourierBoldWidths.Add('W', 600);
    FCourierBoldWidths.Add('x', 600);
    FCourierBoldWidths.Add('X', 600);
    FCourierBoldWidths.Add('y', 600);
    FCourierBoldWidths.Add('Y', 600);
    FCourierBoldWidths.Add('yacute', 600);
    FCourierBoldWidths.Add('Yacute', 600);
    FCourierBoldWidths.Add('ydieresis', 600);
    FCourierBoldWidths.Add('Ydieresis', 600);
    FCourierBoldWidths.Add('yen', 600);
    FCourierBoldWidths.Add('z', 600);
    FCourierBoldWidths.Add('Z', 600);
    FCourierBoldWidths.Add('zacute', 600);
    FCourierBoldWidths.Add('Zacute', 600);
    FCourierBoldWidths.Add('zcaron', 600);
    FCourierBoldWidths.Add('Zcaron', 600);
    FCourierBoldWidths.Add('zdotaccent', 600);
    FCourierBoldWidths.Add('Zdotaccent', 600);
    FCourierBoldWidths.Add('zero', 600);
    FCourierBoldWidths.TrimExcess;
  end;
  Result := FCourierBoldWidths;
end;

function TdxPDFSimpleFontDefaultWidths.GetCourierBoldObliqueWidths: TdxPDFWordDictionary;
begin
  if FCourierBoldObliqueWidths.Count = 0 then
  begin
    FCourierBoldObliqueWidths.Add('a', 600);
    FCourierBoldObliqueWidths.Add('A', 600);
    FCourierBoldObliqueWidths.Add('aacute', 600);
    FCourierBoldObliqueWidths.Add('Aacute', 600);
    FCourierBoldObliqueWidths.Add('abreve', 600);
    FCourierBoldObliqueWidths.Add('Abreve', 600);
    FCourierBoldObliqueWidths.Add('acircumflex', 600);
    FCourierBoldObliqueWidths.Add('Acircumflex', 600);
    FCourierBoldObliqueWidths.Add('acute', 600);
    FCourierBoldObliqueWidths.Add('adieresis', 600);
    FCourierBoldObliqueWidths.Add('Adieresis', 600);
    FCourierBoldObliqueWidths.Add('ae', 600);
    FCourierBoldObliqueWidths.Add('AE', 600);
    FCourierBoldObliqueWidths.Add('agrave', 600);
    FCourierBoldObliqueWidths.Add('Agrave', 600);
    FCourierBoldObliqueWidths.Add('amacron', 600);
    FCourierBoldObliqueWidths.Add('Amacron', 600);
    FCourierBoldObliqueWidths.Add('ampersand', 600);
    FCourierBoldObliqueWidths.Add('aogonek', 600);
    FCourierBoldObliqueWidths.Add('Aogonek', 600);
    FCourierBoldObliqueWidths.Add('aring', 600);
    FCourierBoldObliqueWidths.Add('Aring', 600);
    FCourierBoldObliqueWidths.Add('asciicircum', 600);
    FCourierBoldObliqueWidths.Add('asciitilde', 600);
    FCourierBoldObliqueWidths.Add('asterisk', 600);
    FCourierBoldObliqueWidths.Add('at', 600);
    FCourierBoldObliqueWidths.Add('atilde', 600);
    FCourierBoldObliqueWidths.Add('Atilde', 600);
    FCourierBoldObliqueWidths.Add('b', 600);
    FCourierBoldObliqueWidths.Add('B', 600);
    FCourierBoldObliqueWidths.Add('backslash', 600);
    FCourierBoldObliqueWidths.Add('bar', 600);
    FCourierBoldObliqueWidths.Add('braceleft', 600);
    FCourierBoldObliqueWidths.Add('braceright', 600);
    FCourierBoldObliqueWidths.Add('bracketleft', 600);
    FCourierBoldObliqueWidths.Add('bracketright', 600);
    FCourierBoldObliqueWidths.Add('breve', 600);
    FCourierBoldObliqueWidths.Add('brokenbar', 600);
    FCourierBoldObliqueWidths.Add('bullet', 600);
    FCourierBoldObliqueWidths.Add('c', 600);
    FCourierBoldObliqueWidths.Add('C', 600);
    FCourierBoldObliqueWidths.Add('cacute', 600);
    FCourierBoldObliqueWidths.Add('Cacute', 600);
    FCourierBoldObliqueWidths.Add('caron', 600);
    FCourierBoldObliqueWidths.Add('ccaron', 600);
    FCourierBoldObliqueWidths.Add('Ccaron', 600);
    FCourierBoldObliqueWidths.Add('ccedilla', 600);
    FCourierBoldObliqueWidths.Add('Ccedilla', 600);
    FCourierBoldObliqueWidths.Add('cedilla', 600);
    FCourierBoldObliqueWidths.Add('cent', 600);
    FCourierBoldObliqueWidths.Add('circumflex', 600);
    FCourierBoldObliqueWidths.Add('colon', 600);
    FCourierBoldObliqueWidths.Add('comma', 600);
    FCourierBoldObliqueWidths.Add('commaaccent', 600);
    FCourierBoldObliqueWidths.Add('copyright', 600);
    FCourierBoldObliqueWidths.Add('currency', 600);
    FCourierBoldObliqueWidths.Add('d', 600);
    FCourierBoldObliqueWidths.Add('D', 600);
    FCourierBoldObliqueWidths.Add('dagger', 600);
    FCourierBoldObliqueWidths.Add('daggerdbl', 600);
    FCourierBoldObliqueWidths.Add('dcaron', 600);
    FCourierBoldObliqueWidths.Add('Dcaron', 600);
    FCourierBoldObliqueWidths.Add('dcroat', 600);
    FCourierBoldObliqueWidths.Add('Dcroat', 600);
    FCourierBoldObliqueWidths.Add('degree', 600);
    FCourierBoldObliqueWidths.Add('Delta', 600);
    FCourierBoldObliqueWidths.Add('dieresis', 600);
    FCourierBoldObliqueWidths.Add('divide', 600);
    FCourierBoldObliqueWidths.Add('dollar', 600);
    FCourierBoldObliqueWidths.Add('dotaccent', 600);
    FCourierBoldObliqueWidths.Add('dotlessi', 600);
    FCourierBoldObliqueWidths.Add('e', 600);
    FCourierBoldObliqueWidths.Add('E', 600);
    FCourierBoldObliqueWidths.Add('eacute', 600);
    FCourierBoldObliqueWidths.Add('Eacute', 600);
    FCourierBoldObliqueWidths.Add('ecaron', 600);
    FCourierBoldObliqueWidths.Add('Ecaron', 600);
    FCourierBoldObliqueWidths.Add('ecircumflex', 600);
    FCourierBoldObliqueWidths.Add('Ecircumflex', 600);
    FCourierBoldObliqueWidths.Add('edieresis', 600);
    FCourierBoldObliqueWidths.Add('Edieresis', 600);
    FCourierBoldObliqueWidths.Add('edotaccent', 600);
    FCourierBoldObliqueWidths.Add('Edotaccent', 600);
    FCourierBoldObliqueWidths.Add('egrave', 600);
    FCourierBoldObliqueWidths.Add('Egrave', 600);
    FCourierBoldObliqueWidths.Add('eight', 600);
    FCourierBoldObliqueWidths.Add('ellipsis', 600);
    FCourierBoldObliqueWidths.Add('emacron', 600);
    FCourierBoldObliqueWidths.Add('Emacron', 600);
    FCourierBoldObliqueWidths.Add('emdash', 600);
    FCourierBoldObliqueWidths.Add('endash', 600);
    FCourierBoldObliqueWidths.Add('eogonek', 600);
    FCourierBoldObliqueWidths.Add('Eogonek', 600);
    FCourierBoldObliqueWidths.Add('equal', 600);
    FCourierBoldObliqueWidths.Add('eth', 600);
    FCourierBoldObliqueWidths.Add('Eth', 600);
    FCourierBoldObliqueWidths.Add('Euro', 600);
    FCourierBoldObliqueWidths.Add('exclam', 600);
    FCourierBoldObliqueWidths.Add('exclamdown', 600);
    FCourierBoldObliqueWidths.Add('f', 600);
    FCourierBoldObliqueWidths.Add('F', 600);
    FCourierBoldObliqueWidths.Add('fi', 600);
    FCourierBoldObliqueWidths.Add('five', 600);
    FCourierBoldObliqueWidths.Add('fl', 600);
    FCourierBoldObliqueWidths.Add('florin', 600);
    FCourierBoldObliqueWidths.Add('four', 600);
    FCourierBoldObliqueWidths.Add('fraction', 600);
    FCourierBoldObliqueWidths.Add('g', 600);
    FCourierBoldObliqueWidths.Add('G', 600);
    FCourierBoldObliqueWidths.Add('gbreve', 600);
    FCourierBoldObliqueWidths.Add('Gbreve', 600);
    FCourierBoldObliqueWidths.Add('gcommaaccent', 600);
    FCourierBoldObliqueWidths.Add('Gcommaaccent', 600);
    FCourierBoldObliqueWidths.Add('germandbls', 600);
    FCourierBoldObliqueWidths.Add('grave', 600);
    FCourierBoldObliqueWidths.Add('greater', 600);
    FCourierBoldObliqueWidths.Add('greaterequal', 600);
    FCourierBoldObliqueWidths.Add('guillemotleft', 600);
    FCourierBoldObliqueWidths.Add('guillemotright', 600);
    FCourierBoldObliqueWidths.Add('guilsinglleft', 600);
    FCourierBoldObliqueWidths.Add('guilsinglright', 600);
    FCourierBoldObliqueWidths.Add('h', 600);
    FCourierBoldObliqueWidths.Add('H', 600);
    FCourierBoldObliqueWidths.Add('hungarumlaut', 600);
    FCourierBoldObliqueWidths.Add('hyphen', 600);
    FCourierBoldObliqueWidths.Add('i', 600);
    FCourierBoldObliqueWidths.Add('I', 600);
    FCourierBoldObliqueWidths.Add('iacute', 600);
    FCourierBoldObliqueWidths.Add('Iacute', 600);
    FCourierBoldObliqueWidths.Add('icircumflex', 600);
    FCourierBoldObliqueWidths.Add('Icircumflex', 600);
    FCourierBoldObliqueWidths.Add('idieresis', 600);
    FCourierBoldObliqueWidths.Add('Idieresis', 600);
    FCourierBoldObliqueWidths.Add('Idotaccent', 600);
    FCourierBoldObliqueWidths.Add('igrave', 600);
    FCourierBoldObliqueWidths.Add('Igrave', 600);
    FCourierBoldObliqueWidths.Add('imacron', 600);
    FCourierBoldObliqueWidths.Add('Imacron', 600);
    FCourierBoldObliqueWidths.Add('iogonek', 600);
    FCourierBoldObliqueWidths.Add('Iogonek', 600);
    FCourierBoldObliqueWidths.Add('j', 600);
    FCourierBoldObliqueWidths.Add('J', 600);
    FCourierBoldObliqueWidths.Add('k', 600);
    FCourierBoldObliqueWidths.Add('K', 600);
    FCourierBoldObliqueWidths.Add('kcommaaccent', 600);
    FCourierBoldObliqueWidths.Add('Kcommaaccent', 600);
    FCourierBoldObliqueWidths.Add('l', 600);
    FCourierBoldObliqueWidths.Add('L', 600);
    FCourierBoldObliqueWidths.Add('lacute', 600);
    FCourierBoldObliqueWidths.Add('Lacute', 600);
    FCourierBoldObliqueWidths.Add('lcaron', 600);
    FCourierBoldObliqueWidths.Add('Lcaron', 600);
    FCourierBoldObliqueWidths.Add('lcommaaccent', 600);
    FCourierBoldObliqueWidths.Add('Lcommaaccent', 600);
    FCourierBoldObliqueWidths.Add('less', 600);
    FCourierBoldObliqueWidths.Add('lessequal', 600);
    FCourierBoldObliqueWidths.Add('logicalnot', 600);
    FCourierBoldObliqueWidths.Add('lozenge', 600);
    FCourierBoldObliqueWidths.Add('lslash', 600);
    FCourierBoldObliqueWidths.Add('Lslash', 600);
    FCourierBoldObliqueWidths.Add('m', 600);
    FCourierBoldObliqueWidths.Add('M', 600);
    FCourierBoldObliqueWidths.Add('macron', 600);
    FCourierBoldObliqueWidths.Add('minus', 600);
    FCourierBoldObliqueWidths.Add('mu', 600);
    FCourierBoldObliqueWidths.Add('multiply', 600);
    FCourierBoldObliqueWidths.Add('n', 600);
    FCourierBoldObliqueWidths.Add('N', 600);
    FCourierBoldObliqueWidths.Add('nacute', 600);
    FCourierBoldObliqueWidths.Add('Nacute', 600);
    FCourierBoldObliqueWidths.Add('ncaron', 600);
    FCourierBoldObliqueWidths.Add('Ncaron', 600);
    FCourierBoldObliqueWidths.Add('ncommaaccent', 600);
    FCourierBoldObliqueWidths.Add('Ncommaaccent', 600);
    FCourierBoldObliqueWidths.Add('nine', 600);
    FCourierBoldObliqueWidths.Add('notequal', 600);
    FCourierBoldObliqueWidths.Add('ntilde', 600);
    FCourierBoldObliqueWidths.Add('Ntilde', 600);
    FCourierBoldObliqueWidths.Add('numbersign', 600);
    FCourierBoldObliqueWidths.Add('o', 600);
    FCourierBoldObliqueWidths.Add('O', 600);
    FCourierBoldObliqueWidths.Add('oacute', 600);
    FCourierBoldObliqueWidths.Add('Oacute', 600);
    FCourierBoldObliqueWidths.Add('ocircumflex', 600);
    FCourierBoldObliqueWidths.Add('Ocircumflex', 600);
    FCourierBoldObliqueWidths.Add('odieresis', 600);
    FCourierBoldObliqueWidths.Add('Odieresis', 600);
    FCourierBoldObliqueWidths.Add('oe', 600);
    FCourierBoldObliqueWidths.Add('OE', 600);
    FCourierBoldObliqueWidths.Add('ogonek', 600);
    FCourierBoldObliqueWidths.Add('ograve', 600);
    FCourierBoldObliqueWidths.Add('Ograve', 600);
    FCourierBoldObliqueWidths.Add('ohungarumlaut', 600);
    FCourierBoldObliqueWidths.Add('Ohungarumlaut', 600);
    FCourierBoldObliqueWidths.Add('omacron', 600);
    FCourierBoldObliqueWidths.Add('Omacron', 600);
    FCourierBoldObliqueWidths.Add('one', 600);
    FCourierBoldObliqueWidths.Add('onehalf', 600);
    FCourierBoldObliqueWidths.Add('onequarter', 600);
    FCourierBoldObliqueWidths.Add('onesuperior', 600);
    FCourierBoldObliqueWidths.Add('ordfeminine', 600);
    FCourierBoldObliqueWidths.Add('ordmasculine', 600);
    FCourierBoldObliqueWidths.Add('oslash', 600);
    FCourierBoldObliqueWidths.Add('Oslash', 600);
    FCourierBoldObliqueWidths.Add('otilde', 600);
    FCourierBoldObliqueWidths.Add('Otilde', 600);
    FCourierBoldObliqueWidths.Add('p', 600);
    FCourierBoldObliqueWidths.Add('P', 600);
    FCourierBoldObliqueWidths.Add('paragraph', 600);
    FCourierBoldObliqueWidths.Add('parenleft', 600);
    FCourierBoldObliqueWidths.Add('parenright', 600);
    FCourierBoldObliqueWidths.Add('partialdiff', 600);
    FCourierBoldObliqueWidths.Add('percent', 600);
    FCourierBoldObliqueWidths.Add('period', 600);
    FCourierBoldObliqueWidths.Add('periodcentered', 600);
    FCourierBoldObliqueWidths.Add('perthousand', 600);
    FCourierBoldObliqueWidths.Add('plus', 600);
    FCourierBoldObliqueWidths.Add('plusminus', 600);
    FCourierBoldObliqueWidths.Add('q', 600);
    FCourierBoldObliqueWidths.Add('Q', 600);
    FCourierBoldObliqueWidths.Add('question', 600);
    FCourierBoldObliqueWidths.Add('questiondown', 600);
    FCourierBoldObliqueWidths.Add('quotedbl', 600);
    FCourierBoldObliqueWidths.Add('quotedblbase', 600);
    FCourierBoldObliqueWidths.Add('quotedblleft', 600);
    FCourierBoldObliqueWidths.Add('quotedblright', 600);
    FCourierBoldObliqueWidths.Add('quoteleft', 600);
    FCourierBoldObliqueWidths.Add('quoteright', 600);
    FCourierBoldObliqueWidths.Add('quotesinglbase', 600);
    FCourierBoldObliqueWidths.Add('quotesingle', 600);
    FCourierBoldObliqueWidths.Add('r', 600);
    FCourierBoldObliqueWidths.Add('R', 600);
    FCourierBoldObliqueWidths.Add('racute', 600);
    FCourierBoldObliqueWidths.Add('Racute', 600);
    FCourierBoldObliqueWidths.Add('radical', 600);
    FCourierBoldObliqueWidths.Add('rcaron', 600);
    FCourierBoldObliqueWidths.Add('Rcaron', 600);
    FCourierBoldObliqueWidths.Add('rcommaaccent', 600);
    FCourierBoldObliqueWidths.Add('Rcommaaccent', 600);
    FCourierBoldObliqueWidths.Add('registered', 600);
    FCourierBoldObliqueWidths.Add('ring', 600);
    FCourierBoldObliqueWidths.Add('s', 600);
    FCourierBoldObliqueWidths.Add('S', 600);
    FCourierBoldObliqueWidths.Add('sacute', 600);
    FCourierBoldObliqueWidths.Add('Sacute', 600);
    FCourierBoldObliqueWidths.Add('scaron', 600);
    FCourierBoldObliqueWidths.Add('Scaron', 600);
    FCourierBoldObliqueWidths.Add('scedilla', 600);
    FCourierBoldObliqueWidths.Add('Scedilla', 600);
    FCourierBoldObliqueWidths.Add('scommaaccent', 600);
    FCourierBoldObliqueWidths.Add('Scommaaccent', 600);
    FCourierBoldObliqueWidths.Add('section', 600);
    FCourierBoldObliqueWidths.Add('semicolon', 600);
    FCourierBoldObliqueWidths.Add('seven', 600);
    FCourierBoldObliqueWidths.Add('six', 600);
    FCourierBoldObliqueWidths.Add('slash', 600);
    FCourierBoldObliqueWidths.Add('space', 600);
    FCourierBoldObliqueWidths.Add('sterling', 600);
    FCourierBoldObliqueWidths.Add('summation', 600);
    FCourierBoldObliqueWidths.Add('t', 600);
    FCourierBoldObliqueWidths.Add('T', 600);
    FCourierBoldObliqueWidths.Add('tcaron', 600);
    FCourierBoldObliqueWidths.Add('Tcaron', 600);
    FCourierBoldObliqueWidths.Add('tcommaaccent', 600);
    FCourierBoldObliqueWidths.Add('Tcommaaccent', 600);
    FCourierBoldObliqueWidths.Add('thorn', 600);
    FCourierBoldObliqueWidths.Add('Thorn', 600);
    FCourierBoldObliqueWidths.Add('three', 600);
    FCourierBoldObliqueWidths.Add('threequarters', 600);
    FCourierBoldObliqueWidths.Add('threesuperior', 600);
    FCourierBoldObliqueWidths.Add('tilde', 600);
    FCourierBoldObliqueWidths.Add('trademark', 600);
    FCourierBoldObliqueWidths.Add('two', 600);
    FCourierBoldObliqueWidths.Add('twosuperior', 600);
    FCourierBoldObliqueWidths.Add('u', 600);
    FCourierBoldObliqueWidths.Add('U', 600);
    FCourierBoldObliqueWidths.Add('uacute', 600);
    FCourierBoldObliqueWidths.Add('Uacute', 600);
    FCourierBoldObliqueWidths.Add('ucircumflex', 600);
    FCourierBoldObliqueWidths.Add('Ucircumflex', 600);
    FCourierBoldObliqueWidths.Add('udieresis', 600);
    FCourierBoldObliqueWidths.Add('Udieresis', 600);
    FCourierBoldObliqueWidths.Add('ugrave', 600);
    FCourierBoldObliqueWidths.Add('Ugrave', 600);
    FCourierBoldObliqueWidths.Add('uhungarumlaut', 600);
    FCourierBoldObliqueWidths.Add('Uhungarumlaut', 600);
    FCourierBoldObliqueWidths.Add('umacron', 600);
    FCourierBoldObliqueWidths.Add('Umacron', 600);
    FCourierBoldObliqueWidths.Add('underscore', 600);
    FCourierBoldObliqueWidths.Add('uogonek', 600);
    FCourierBoldObliqueWidths.Add('Uogonek', 600);
    FCourierBoldObliqueWidths.Add('uring', 600);
    FCourierBoldObliqueWidths.Add('Uring', 600);
    FCourierBoldObliqueWidths.Add('v', 600);
    FCourierBoldObliqueWidths.Add('V', 600);
    FCourierBoldObliqueWidths.Add('w', 600);
    FCourierBoldObliqueWidths.Add('W', 600);
    FCourierBoldObliqueWidths.Add('x', 600);
    FCourierBoldObliqueWidths.Add('X', 600);
    FCourierBoldObliqueWidths.Add('y', 600);
    FCourierBoldObliqueWidths.Add('Y', 600);
    FCourierBoldObliqueWidths.Add('yacute', 600);
    FCourierBoldObliqueWidths.Add('Yacute', 600);
    FCourierBoldObliqueWidths.Add('ydieresis', 600);
    FCourierBoldObliqueWidths.Add('Ydieresis', 600);
    FCourierBoldObliqueWidths.Add('yen', 600);
    FCourierBoldObliqueWidths.Add('z', 600);
    FCourierBoldObliqueWidths.Add('Z', 600);
    FCourierBoldObliqueWidths.Add('zacute', 600);
    FCourierBoldObliqueWidths.Add('Zacute', 600);
    FCourierBoldObliqueWidths.Add('zcaron', 600);
    FCourierBoldObliqueWidths.Add('Zcaron', 600);
    FCourierBoldObliqueWidths.Add('zdotaccent', 600);
    FCourierBoldObliqueWidths.Add('Zdotaccent', 600);
    FCourierBoldObliqueWidths.Add('zero', 600);
    FCourierBoldObliqueWidths.TrimExcess;
  end;
  Result := FCourierBoldObliqueWidths;
end;

function TdxPDFSimpleFontDefaultWidths.GetCourierObliqueWidths: TdxPDFWordDictionary;
begin
  if FCourierObliqueWidths.Count = 0 then
  begin
    FCourierObliqueWidths.Add('a', 600);
    FCourierObliqueWidths.Add('A', 600);
    FCourierObliqueWidths.Add('aacute', 600);
    FCourierObliqueWidths.Add('Aacute', 600);
    FCourierObliqueWidths.Add('abreve', 600);
    FCourierObliqueWidths.Add('Abreve', 600);
    FCourierObliqueWidths.Add('acircumflex', 600);
    FCourierObliqueWidths.Add('Acircumflex', 600);
    FCourierObliqueWidths.Add('acute', 600);
    FCourierObliqueWidths.Add('adieresis', 600);
    FCourierObliqueWidths.Add('Adieresis', 600);
    FCourierObliqueWidths.Add('ae', 600);
    FCourierObliqueWidths.Add('AE', 600);
    FCourierObliqueWidths.Add('agrave', 600);
    FCourierObliqueWidths.Add('Agrave', 600);
    FCourierObliqueWidths.Add('amacron', 600);
    FCourierObliqueWidths.Add('Amacron', 600);
    FCourierObliqueWidths.Add('ampersand', 600);
    FCourierObliqueWidths.Add('aogonek', 600);
    FCourierObliqueWidths.Add('Aogonek', 600);
    FCourierObliqueWidths.Add('aring', 600);
    FCourierObliqueWidths.Add('Aring', 600);
    FCourierObliqueWidths.Add('asciicircum', 600);
    FCourierObliqueWidths.Add('asciitilde', 600);
    FCourierObliqueWidths.Add('asterisk', 600);
    FCourierObliqueWidths.Add('at', 600);
    FCourierObliqueWidths.Add('atilde', 600);
    FCourierObliqueWidths.Add('Atilde', 600);
    FCourierObliqueWidths.Add('b', 600);
    FCourierObliqueWidths.Add('B', 600);
    FCourierObliqueWidths.Add('backslash', 600);
    FCourierObliqueWidths.Add('bar', 600);
    FCourierObliqueWidths.Add('braceleft', 600);
    FCourierObliqueWidths.Add('braceright', 600);
    FCourierObliqueWidths.Add('bracketleft', 600);
    FCourierObliqueWidths.Add('bracketright', 600);
    FCourierObliqueWidths.Add('breve', 600);
    FCourierObliqueWidths.Add('brokenbar', 600);
    FCourierObliqueWidths.Add('bullet', 600);
    FCourierObliqueWidths.Add('c', 600);
    FCourierObliqueWidths.Add('C', 600);
    FCourierObliqueWidths.Add('cacute', 600);
    FCourierObliqueWidths.Add('Cacute', 600);
    FCourierObliqueWidths.Add('caron', 600);
    FCourierObliqueWidths.Add('ccaron', 600);
    FCourierObliqueWidths.Add('Ccaron', 600);
    FCourierObliqueWidths.Add('ccedilla', 600);
    FCourierObliqueWidths.Add('Ccedilla', 600);
    FCourierObliqueWidths.Add('cedilla', 600);
    FCourierObliqueWidths.Add('cent', 600);
    FCourierObliqueWidths.Add('circumflex', 600);
    FCourierObliqueWidths.Add('colon', 600);
    FCourierObliqueWidths.Add('comma', 600);
    FCourierObliqueWidths.Add('commaaccent', 600);
    FCourierObliqueWidths.Add('copyright', 600);
    FCourierObliqueWidths.Add('currency', 600);
    FCourierObliqueWidths.Add('d', 600);
    FCourierObliqueWidths.Add('D', 600);
    FCourierObliqueWidths.Add('dagger', 600);
    FCourierObliqueWidths.Add('daggerdbl', 600);
    FCourierObliqueWidths.Add('dcaron', 600);
    FCourierObliqueWidths.Add('Dcaron', 600);
    FCourierObliqueWidths.Add('dcroat', 600);
    FCourierObliqueWidths.Add('Dcroat', 600);
    FCourierObliqueWidths.Add('degree', 600);
    FCourierObliqueWidths.Add('Delta', 600);
    FCourierObliqueWidths.Add('dieresis', 600);
    FCourierObliqueWidths.Add('divide', 600);
    FCourierObliqueWidths.Add('dollar', 600);
    FCourierObliqueWidths.Add('dotaccent', 600);
    FCourierObliqueWidths.Add('dotlessi', 600);
    FCourierObliqueWidths.Add('e', 600);
    FCourierObliqueWidths.Add('E', 600);
    FCourierObliqueWidths.Add('eacute', 600);
    FCourierObliqueWidths.Add('Eacute', 600);
    FCourierObliqueWidths.Add('ecaron', 600);
    FCourierObliqueWidths.Add('Ecaron', 600);
    FCourierObliqueWidths.Add('ecircumflex', 600);
    FCourierObliqueWidths.Add('Ecircumflex', 600);
    FCourierObliqueWidths.Add('edieresis', 600);
    FCourierObliqueWidths.Add('Edieresis', 600);
    FCourierObliqueWidths.Add('edotaccent', 600);
    FCourierObliqueWidths.Add('Edotaccent', 600);
    FCourierObliqueWidths.Add('egrave', 600);
    FCourierObliqueWidths.Add('Egrave', 600);
    FCourierObliqueWidths.Add('eight', 600);
    FCourierObliqueWidths.Add('ellipsis', 600);
    FCourierObliqueWidths.Add('emacron', 600);
    FCourierObliqueWidths.Add('Emacron', 600);
    FCourierObliqueWidths.Add('emdash', 600);
    FCourierObliqueWidths.Add('endash', 600);
    FCourierObliqueWidths.Add('eogonek', 600);
    FCourierObliqueWidths.Add('Eogonek', 600);
    FCourierObliqueWidths.Add('equal', 600);
    FCourierObliqueWidths.Add('eth', 600);
    FCourierObliqueWidths.Add('Eth', 600);
    FCourierObliqueWidths.Add('Euro', 600);
    FCourierObliqueWidths.Add('exclam', 600);
    FCourierObliqueWidths.Add('exclamdown', 600);
    FCourierObliqueWidths.Add('f', 600);
    FCourierObliqueWidths.Add('F', 600);
    FCourierObliqueWidths.Add('fi', 600);
    FCourierObliqueWidths.Add('five', 600);
    FCourierObliqueWidths.Add('fl', 600);
    FCourierObliqueWidths.Add('florin', 600);
    FCourierObliqueWidths.Add('four', 600);
    FCourierObliqueWidths.Add('fraction', 600);
    FCourierObliqueWidths.Add('g', 600);
    FCourierObliqueWidths.Add('G', 600);
    FCourierObliqueWidths.Add('gbreve', 600);
    FCourierObliqueWidths.Add('Gbreve', 600);
    FCourierObliqueWidths.Add('gcommaaccent', 600);
    FCourierObliqueWidths.Add('Gcommaaccent', 600);
    FCourierObliqueWidths.Add('germandbls', 600);
    FCourierObliqueWidths.Add('grave', 600);
    FCourierObliqueWidths.Add('greater', 600);
    FCourierObliqueWidths.Add('greaterequal', 600);
    FCourierObliqueWidths.Add('guillemotleft', 600);
    FCourierObliqueWidths.Add('guillemotright', 600);
    FCourierObliqueWidths.Add('guilsinglleft', 600);
    FCourierObliqueWidths.Add('guilsinglright', 600);
    FCourierObliqueWidths.Add('h', 600);
    FCourierObliqueWidths.Add('H', 600);
    FCourierObliqueWidths.Add('hungarumlaut', 600);
    FCourierObliqueWidths.Add('hyphen', 600);
    FCourierObliqueWidths.Add('i', 600);
    FCourierObliqueWidths.Add('I', 600);
    FCourierObliqueWidths.Add('iacute', 600);
    FCourierObliqueWidths.Add('Iacute', 600);
    FCourierObliqueWidths.Add('icircumflex', 600);
    FCourierObliqueWidths.Add('Icircumflex', 600);
    FCourierObliqueWidths.Add('idieresis', 600);
    FCourierObliqueWidths.Add('Idieresis', 600);
    FCourierObliqueWidths.Add('Idotaccent', 600);
    FCourierObliqueWidths.Add('igrave', 600);
    FCourierObliqueWidths.Add('Igrave', 600);
    FCourierObliqueWidths.Add('imacron', 600);
    FCourierObliqueWidths.Add('Imacron', 600);
    FCourierObliqueWidths.Add('iogonek', 600);
    FCourierObliqueWidths.Add('Iogonek', 600);
    FCourierObliqueWidths.Add('j', 600);
    FCourierObliqueWidths.Add('J', 600);
    FCourierObliqueWidths.Add('k', 600);
    FCourierObliqueWidths.Add('K', 600);
    FCourierObliqueWidths.Add('kcommaaccent', 600);
    FCourierObliqueWidths.Add('Kcommaaccent', 600);
    FCourierObliqueWidths.Add('l', 600);
    FCourierObliqueWidths.Add('L', 600);
    FCourierObliqueWidths.Add('lacute', 600);
    FCourierObliqueWidths.Add('Lacute', 600);
    FCourierObliqueWidths.Add('lcaron', 600);
    FCourierObliqueWidths.Add('Lcaron', 600);
    FCourierObliqueWidths.Add('lcommaaccent', 600);
    FCourierObliqueWidths.Add('Lcommaaccent', 600);
    FCourierObliqueWidths.Add('less', 600);
    FCourierObliqueWidths.Add('lessequal', 600);
    FCourierObliqueWidths.Add('logicalnot', 600);
    FCourierObliqueWidths.Add('lozenge', 600);
    FCourierObliqueWidths.Add('lslash', 600);
    FCourierObliqueWidths.Add('Lslash', 600);
    FCourierObliqueWidths.Add('m', 600);
    FCourierObliqueWidths.Add('M', 600);
    FCourierObliqueWidths.Add('macron', 600);
    FCourierObliqueWidths.Add('minus', 600);
    FCourierObliqueWidths.Add('mu', 600);
    FCourierObliqueWidths.Add('multiply', 600);
    FCourierObliqueWidths.Add('n', 600);
    FCourierObliqueWidths.Add('N', 600);
    FCourierObliqueWidths.Add('nacute', 600);
    FCourierObliqueWidths.Add('Nacute', 600);
    FCourierObliqueWidths.Add('ncaron', 600);
    FCourierObliqueWidths.Add('Ncaron', 600);
    FCourierObliqueWidths.Add('ncommaaccent', 600);
    FCourierObliqueWidths.Add('Ncommaaccent', 600);
    FCourierObliqueWidths.Add('nine', 600);
    FCourierObliqueWidths.Add('notequal', 600);
    FCourierObliqueWidths.Add('ntilde', 600);
    FCourierObliqueWidths.Add('Ntilde', 600);
    FCourierObliqueWidths.Add('numbersign', 600);
    FCourierObliqueWidths.Add('o', 600);
    FCourierObliqueWidths.Add('O', 600);
    FCourierObliqueWidths.Add('oacute', 600);
    FCourierObliqueWidths.Add('Oacute', 600);
    FCourierObliqueWidths.Add('ocircumflex', 600);
    FCourierObliqueWidths.Add('Ocircumflex', 600);
    FCourierObliqueWidths.Add('odieresis', 600);
    FCourierObliqueWidths.Add('Odieresis', 600);
    FCourierObliqueWidths.Add('oe', 600);
    FCourierObliqueWidths.Add('OE', 600);
    FCourierObliqueWidths.Add('ogonek', 600);
    FCourierObliqueWidths.Add('ograve', 600);
    FCourierObliqueWidths.Add('Ograve', 600);
    FCourierObliqueWidths.Add('ohungarumlaut', 600);
    FCourierObliqueWidths.Add('Ohungarumlaut', 600);
    FCourierObliqueWidths.Add('omacron', 600);
    FCourierObliqueWidths.Add('Omacron', 600);
    FCourierObliqueWidths.Add('one', 600);
    FCourierObliqueWidths.Add('onehalf', 600);
    FCourierObliqueWidths.Add('onequarter', 600);
    FCourierObliqueWidths.Add('onesuperior', 600);
    FCourierObliqueWidths.Add('ordfeminine', 600);
    FCourierObliqueWidths.Add('ordmasculine', 600);
    FCourierObliqueWidths.Add('oslash', 600);
    FCourierObliqueWidths.Add('Oslash', 600);
    FCourierObliqueWidths.Add('otilde', 600);
    FCourierObliqueWidths.Add('Otilde', 600);
    FCourierObliqueWidths.Add('p', 600);
    FCourierObliqueWidths.Add('P', 600);
    FCourierObliqueWidths.Add('paragraph', 600);
    FCourierObliqueWidths.Add('parenleft', 600);
    FCourierObliqueWidths.Add('parenright', 600);
    FCourierObliqueWidths.Add('partialdiff', 600);
    FCourierObliqueWidths.Add('percent', 600);
    FCourierObliqueWidths.Add('period', 600);
    FCourierObliqueWidths.Add('periodcentered', 600);
    FCourierObliqueWidths.Add('perthousand', 600);
    FCourierObliqueWidths.Add('plus', 600);
    FCourierObliqueWidths.Add('plusminus', 600);
    FCourierObliqueWidths.Add('q', 600);
    FCourierObliqueWidths.Add('Q', 600);
    FCourierObliqueWidths.Add('question', 600);
    FCourierObliqueWidths.Add('questiondown', 600);
    FCourierObliqueWidths.Add('quotedbl', 600);
    FCourierObliqueWidths.Add('quotedblbase', 600);
    FCourierObliqueWidths.Add('quotedblleft', 600);
    FCourierObliqueWidths.Add('quotedblright', 600);
    FCourierObliqueWidths.Add('quoteleft', 600);
    FCourierObliqueWidths.Add('quoteright', 600);
    FCourierObliqueWidths.Add('quotesinglbase', 600);
    FCourierObliqueWidths.Add('quotesingle', 600);
    FCourierObliqueWidths.Add('r', 600);
    FCourierObliqueWidths.Add('R', 600);
    FCourierObliqueWidths.Add('racute', 600);
    FCourierObliqueWidths.Add('Racute', 600);
    FCourierObliqueWidths.Add('radical', 600);
    FCourierObliqueWidths.Add('rcaron', 600);
    FCourierObliqueWidths.Add('Rcaron', 600);
    FCourierObliqueWidths.Add('rcommaaccent', 600);
    FCourierObliqueWidths.Add('Rcommaaccent', 600);
    FCourierObliqueWidths.Add('registered', 600);
    FCourierObliqueWidths.Add('ring', 600);
    FCourierObliqueWidths.Add('s', 600);
    FCourierObliqueWidths.Add('S', 600);
    FCourierObliqueWidths.Add('sacute', 600);
    FCourierObliqueWidths.Add('Sacute', 600);
    FCourierObliqueWidths.Add('scaron', 600);
    FCourierObliqueWidths.Add('Scaron', 600);
    FCourierObliqueWidths.Add('scedilla', 600);
    FCourierObliqueWidths.Add('Scedilla', 600);
    FCourierObliqueWidths.Add('scommaaccent', 600);
    FCourierObliqueWidths.Add('Scommaaccent', 600);
    FCourierObliqueWidths.Add('section', 600);
    FCourierObliqueWidths.Add('semicolon', 600);
    FCourierObliqueWidths.Add('seven', 600);
    FCourierObliqueWidths.Add('six', 600);
    FCourierObliqueWidths.Add('slash', 600);
    FCourierObliqueWidths.Add('space', 600);
    FCourierObliqueWidths.Add('sterling', 600);
    FCourierObliqueWidths.Add('summation', 600);
    FCourierObliqueWidths.Add('t', 600);
    FCourierObliqueWidths.Add('T', 600);
    FCourierObliqueWidths.Add('tcaron', 600);
    FCourierObliqueWidths.Add('Tcaron', 600);
    FCourierObliqueWidths.Add('tcommaaccent', 600);
    FCourierObliqueWidths.Add('Tcommaaccent', 600);
    FCourierObliqueWidths.Add('thorn', 600);
    FCourierObliqueWidths.Add('Thorn', 600);
    FCourierObliqueWidths.Add('three', 600);
    FCourierObliqueWidths.Add('threequarters', 600);
    FCourierObliqueWidths.Add('threesuperior', 600);
    FCourierObliqueWidths.Add('tilde', 600);
    FCourierObliqueWidths.Add('trademark', 600);
    FCourierObliqueWidths.Add('two', 600);
    FCourierObliqueWidths.Add('twosuperior', 600);
    FCourierObliqueWidths.Add('u', 600);
    FCourierObliqueWidths.Add('U', 600);
    FCourierObliqueWidths.Add('uacute', 600);
    FCourierObliqueWidths.Add('Uacute', 600);
    FCourierObliqueWidths.Add('ucircumflex', 600);
    FCourierObliqueWidths.Add('Ucircumflex', 600);
    FCourierObliqueWidths.Add('udieresis', 600);
    FCourierObliqueWidths.Add('Udieresis', 600);
    FCourierObliqueWidths.Add('ugrave', 600);
    FCourierObliqueWidths.Add('Ugrave', 600);
    FCourierObliqueWidths.Add('uhungarumlaut', 600);
    FCourierObliqueWidths.Add('Uhungarumlaut', 600);
    FCourierObliqueWidths.Add('umacron', 600);
    FCourierObliqueWidths.Add('Umacron', 600);
    FCourierObliqueWidths.Add('underscore', 600);
    FCourierObliqueWidths.Add('uogonek', 600);
    FCourierObliqueWidths.Add('Uogonek', 600);
    FCourierObliqueWidths.Add('uring', 600);
    FCourierObliqueWidths.Add('Uring', 600);
    FCourierObliqueWidths.Add('v', 600);
    FCourierObliqueWidths.Add('V', 600);
    FCourierObliqueWidths.Add('w', 600);
    FCourierObliqueWidths.Add('W', 600);
    FCourierObliqueWidths.Add('x', 600);
    FCourierObliqueWidths.Add('X', 600);
    FCourierObliqueWidths.Add('y', 600);
    FCourierObliqueWidths.Add('Y', 600);
    FCourierObliqueWidths.Add('yacute', 600);
    FCourierObliqueWidths.Add('Yacute', 600);
    FCourierObliqueWidths.Add('ydieresis', 600);
    FCourierObliqueWidths.Add('Ydieresis', 600);
    FCourierObliqueWidths.Add('yen', 600);
    FCourierObliqueWidths.Add('z', 600);
    FCourierObliqueWidths.Add('Z', 600);
    FCourierObliqueWidths.Add('zacute', 600);
    FCourierObliqueWidths.Add('Zacute', 600);
    FCourierObliqueWidths.Add('zcaron', 600);
    FCourierObliqueWidths.Add('Zcaron', 600);
    FCourierObliqueWidths.Add('zdotaccent', 600);
    FCourierObliqueWidths.Add('Zdotaccent', 600);
    FCourierObliqueWidths.Add('zero', 600);
    FCourierObliqueWidths.TrimExcess;
  end;
  Result := FCourierObliqueWidths;
end;

function TdxPDFSimpleFontDefaultWidths.GetCourierWidths: TdxPDFWordDictionary;
begin
  if FCourierWidths.Count = 0 then
  begin
    FCourierWidths.Add('a', 600);
    FCourierWidths.Add('A', 600);
    FCourierWidths.Add('aacute', 600);
    FCourierWidths.Add('Aacute', 600);
    FCourierWidths.Add('abreve', 600);
    FCourierWidths.Add('Abreve', 600);
    FCourierWidths.Add('acircumflex', 600);
    FCourierWidths.Add('Acircumflex', 600);
    FCourierWidths.Add('acute', 600);
    FCourierWidths.Add('adieresis', 600);
    FCourierWidths.Add('Adieresis', 600);
    FCourierWidths.Add('ae', 600);
    FCourierWidths.Add('AE', 600);
    FCourierWidths.Add('agrave', 600);
    FCourierWidths.Add('Agrave', 600);
    FCourierWidths.Add('amacron', 600);
    FCourierWidths.Add('Amacron', 600);
    FCourierWidths.Add('ampersand', 600);
    FCourierWidths.Add('aogonek', 600);
    FCourierWidths.Add('Aogonek', 600);
    FCourierWidths.Add('aring', 600);
    FCourierWidths.Add('Aring', 600);
    FCourierWidths.Add('asciicircum', 600);
    FCourierWidths.Add('asciitilde', 600);
    FCourierWidths.Add('asterisk', 600);
    FCourierWidths.Add('at', 600);
    FCourierWidths.Add('atilde', 600);
    FCourierWidths.Add('Atilde', 600);
    FCourierWidths.Add('b', 600);
    FCourierWidths.Add('B', 600);
    FCourierWidths.Add('backslash', 600);
    FCourierWidths.Add('bar', 600);
    FCourierWidths.Add('braceleft', 600);
    FCourierWidths.Add('braceright', 600);
    FCourierWidths.Add('bracketleft', 600);
    FCourierWidths.Add('bracketright', 600);
    FCourierWidths.Add('breve', 600);
    FCourierWidths.Add('brokenbar', 600);
    FCourierWidths.Add('bullet', 600);
    FCourierWidths.Add('c', 600);
    FCourierWidths.Add('C', 600);
    FCourierWidths.Add('cacute', 600);
    FCourierWidths.Add('Cacute', 600);
    FCourierWidths.Add('caron', 600);
    FCourierWidths.Add('ccaron', 600);
    FCourierWidths.Add('Ccaron', 600);
    FCourierWidths.Add('ccedilla', 600);
    FCourierWidths.Add('Ccedilla', 600);
    FCourierWidths.Add('cedilla', 600);
    FCourierWidths.Add('cent', 600);
    FCourierWidths.Add('circumflex', 600);
    FCourierWidths.Add('colon', 600);
    FCourierWidths.Add('comma', 600);
    FCourierWidths.Add('commaaccent', 600);
    FCourierWidths.Add('copyright', 600);
    FCourierWidths.Add('currency', 600);
    FCourierWidths.Add('d', 600);
    FCourierWidths.Add('D', 600);
    FCourierWidths.Add('dagger', 600);
    FCourierWidths.Add('daggerdbl', 600);
    FCourierWidths.Add('dcaron', 600);
    FCourierWidths.Add('Dcaron', 600);
    FCourierWidths.Add('dcroat', 600);
    FCourierWidths.Add('Dcroat', 600);
    FCourierWidths.Add('degree', 600);
    FCourierWidths.Add('Delta', 600);
    FCourierWidths.Add('dieresis', 600);
    FCourierWidths.Add('divide', 600);
    FCourierWidths.Add('dollar', 600);
    FCourierWidths.Add('dotaccent', 600);
    FCourierWidths.Add('dotlessi', 600);
    FCourierWidths.Add('e', 600);
    FCourierWidths.Add('E', 600);
    FCourierWidths.Add('eacute', 600);
    FCourierWidths.Add('Eacute', 600);
    FCourierWidths.Add('ecaron', 600);
    FCourierWidths.Add('Ecaron', 600);
    FCourierWidths.Add('ecircumflex', 600);
    FCourierWidths.Add('Ecircumflex', 600);
    FCourierWidths.Add('edieresis', 600);
    FCourierWidths.Add('Edieresis', 600);
    FCourierWidths.Add('edotaccent', 600);
    FCourierWidths.Add('Edotaccent', 600);
    FCourierWidths.Add('egrave', 600);
    FCourierWidths.Add('Egrave', 600);
    FCourierWidths.Add('eight', 600);
    FCourierWidths.Add('ellipsis', 600);
    FCourierWidths.Add('emacron', 600);
    FCourierWidths.Add('Emacron', 600);
    FCourierWidths.Add('emdash', 600);
    FCourierWidths.Add('endash', 600);
    FCourierWidths.Add('eogonek', 600);
    FCourierWidths.Add('Eogonek', 600);
    FCourierWidths.Add('equal', 600);
    FCourierWidths.Add('eth', 600);
    FCourierWidths.Add('Eth', 600);
    FCourierWidths.Add('Euro', 600);
    FCourierWidths.Add('exclam', 600);
    FCourierWidths.Add('exclamdown', 600);
    FCourierWidths.Add('f', 600);
    FCourierWidths.Add('F', 600);
    FCourierWidths.Add('fi', 600);
    FCourierWidths.Add('five', 600);
    FCourierWidths.Add('fl', 600);
    FCourierWidths.Add('florin', 600);
    FCourierWidths.Add('four', 600);
    FCourierWidths.Add('fraction', 600);
    FCourierWidths.Add('g', 600);
    FCourierWidths.Add('G', 600);
    FCourierWidths.Add('gbreve', 600);
    FCourierWidths.Add('Gbreve', 600);
    FCourierWidths.Add('gcommaaccent', 600);
    FCourierWidths.Add('Gcommaaccent', 600);
    FCourierWidths.Add('germandbls', 600);
    FCourierWidths.Add('grave', 600);
    FCourierWidths.Add('greater', 600);
    FCourierWidths.Add('greaterequal', 600);
    FCourierWidths.Add('guillemotleft', 600);
    FCourierWidths.Add('guillemotright', 600);
    FCourierWidths.Add('guilsinglleft', 600);
    FCourierWidths.Add('guilsinglright', 600);
    FCourierWidths.Add('h', 600);
    FCourierWidths.Add('H', 600);
    FCourierWidths.Add('hungarumlaut', 600);
    FCourierWidths.Add('hyphen', 600);
    FCourierWidths.Add('i', 600);
    FCourierWidths.Add('I', 600);
    FCourierWidths.Add('iacute', 600);
    FCourierWidths.Add('Iacute', 600);
    FCourierWidths.Add('icircumflex', 600);
    FCourierWidths.Add('Icircumflex', 600);
    FCourierWidths.Add('idieresis', 600);
    FCourierWidths.Add('Idieresis', 600);
    FCourierWidths.Add('Idotaccent', 600);
    FCourierWidths.Add('igrave', 600);
    FCourierWidths.Add('Igrave', 600);
    FCourierWidths.Add('imacron', 600);
    FCourierWidths.Add('Imacron', 600);
    FCourierWidths.Add('iogonek', 600);
    FCourierWidths.Add('Iogonek', 600);
    FCourierWidths.Add('j', 600);
    FCourierWidths.Add('J', 600);
    FCourierWidths.Add('k', 600);
    FCourierWidths.Add('K', 600);
    FCourierWidths.Add('kcommaaccent', 600);
    FCourierWidths.Add('Kcommaaccent', 600);
    FCourierWidths.Add('l', 600);
    FCourierWidths.Add('L', 600);
    FCourierWidths.Add('lacute', 600);
    FCourierWidths.Add('Lacute', 600);
    FCourierWidths.Add('lcaron', 600);
    FCourierWidths.Add('Lcaron', 600);
    FCourierWidths.Add('lcommaaccent', 600);
    FCourierWidths.Add('Lcommaaccent', 600);
    FCourierWidths.Add('less', 600);
    FCourierWidths.Add('lessequal', 600);
    FCourierWidths.Add('logicalnot', 600);
    FCourierWidths.Add('lozenge', 600);
    FCourierWidths.Add('lslash', 600);
    FCourierWidths.Add('Lslash', 600);
    FCourierWidths.Add('m', 600);
    FCourierWidths.Add('M', 600);
    FCourierWidths.Add('macron', 600);
    FCourierWidths.Add('minus', 600);
    FCourierWidths.Add('mu', 600);
    FCourierWidths.Add('multiply', 600);
    FCourierWidths.Add('n', 600);
    FCourierWidths.Add('N', 600);
    FCourierWidths.Add('nacute', 600);
    FCourierWidths.Add('Nacute', 600);
    FCourierWidths.Add('ncaron', 600);
    FCourierWidths.Add('Ncaron', 600);
    FCourierWidths.Add('ncommaaccent', 600);
    FCourierWidths.Add('Ncommaaccent', 600);
    FCourierWidths.Add('nine', 600);
    FCourierWidths.Add('notequal', 600);
    FCourierWidths.Add('ntilde', 600);
    FCourierWidths.Add('Ntilde', 600);
    FCourierWidths.Add('numbersign', 600);
    FCourierWidths.Add('o', 600);
    FCourierWidths.Add('O', 600);
    FCourierWidths.Add('oacute', 600);
    FCourierWidths.Add('Oacute', 600);
    FCourierWidths.Add('ocircumflex', 600);
    FCourierWidths.Add('Ocircumflex', 600);
    FCourierWidths.Add('odieresis', 600);
    FCourierWidths.Add('Odieresis', 600);
    FCourierWidths.Add('oe', 600);
    FCourierWidths.Add('OE', 600);
    FCourierWidths.Add('ogonek', 600);
    FCourierWidths.Add('ograve', 600);
    FCourierWidths.Add('Ograve', 600);
    FCourierWidths.Add('ohungarumlaut', 600);
    FCourierWidths.Add('Ohungarumlaut', 600);
    FCourierWidths.Add('omacron', 600);
    FCourierWidths.Add('Omacron', 600);
    FCourierWidths.Add('one', 600);
    FCourierWidths.Add('onehalf', 600);
    FCourierWidths.Add('onequarter', 600);
    FCourierWidths.Add('onesuperior', 600);
    FCourierWidths.Add('ordfeminine', 600);
    FCourierWidths.Add('ordmasculine', 600);
    FCourierWidths.Add('oslash', 600);
    FCourierWidths.Add('Oslash', 600);
    FCourierWidths.Add('otilde', 600);
    FCourierWidths.Add('Otilde', 600);
    FCourierWidths.Add('p', 600);
    FCourierWidths.Add('P', 600);
    FCourierWidths.Add('paragraph', 600);
    FCourierWidths.Add('parenleft', 600);
    FCourierWidths.Add('parenright', 600);
    FCourierWidths.Add('partialdiff', 600);
    FCourierWidths.Add('percent', 600);
    FCourierWidths.Add('period', 600);
    FCourierWidths.Add('periodcentered', 600);
    FCourierWidths.Add('perthousand', 600);
    FCourierWidths.Add('plus', 600);
    FCourierWidths.Add('plusminus', 600);
    FCourierWidths.Add('q', 600);
    FCourierWidths.Add('Q', 600);
    FCourierWidths.Add('question', 600);
    FCourierWidths.Add('questiondown', 600);
    FCourierWidths.Add('quotedbl', 600);
    FCourierWidths.Add('quotedblbase', 600);
    FCourierWidths.Add('quotedblleft', 600);
    FCourierWidths.Add('quotedblright', 600);
    FCourierWidths.Add('quoteleft', 600);
    FCourierWidths.Add('quoteright', 600);
    FCourierWidths.Add('quotesinglbase', 600);
    FCourierWidths.Add('quotesingle', 600);
    FCourierWidths.Add('r', 600);
    FCourierWidths.Add('R', 600);
    FCourierWidths.Add('racute', 600);
    FCourierWidths.Add('Racute', 600);
    FCourierWidths.Add('radical', 600);
    FCourierWidths.Add('rcaron', 600);
    FCourierWidths.Add('Rcaron', 600);
    FCourierWidths.Add('rcommaaccent', 600);
    FCourierWidths.Add('Rcommaaccent', 600);
    FCourierWidths.Add('registered', 600);
    FCourierWidths.Add('ring', 600);
    FCourierWidths.Add('s', 600);
    FCourierWidths.Add('S', 600);
    FCourierWidths.Add('sacute', 600);
    FCourierWidths.Add('Sacute', 600);
    FCourierWidths.Add('scaron', 600);
    FCourierWidths.Add('Scaron', 600);
    FCourierWidths.Add('scedilla', 600);
    FCourierWidths.Add('Scedilla', 600);
    FCourierWidths.Add('scommaaccent', 600);
    FCourierWidths.Add('Scommaaccent', 600);
    FCourierWidths.Add('section', 600);
    FCourierWidths.Add('semicolon', 600);
    FCourierWidths.Add('seven', 600);
    FCourierWidths.Add('six', 600);
    FCourierWidths.Add('slash', 600);
    FCourierWidths.Add('space', 600);
    FCourierWidths.Add('sterling', 600);
    FCourierWidths.Add('summation', 600);
    FCourierWidths.Add('t', 600);
    FCourierWidths.Add('T', 600);
    FCourierWidths.Add('tcaron', 600);
    FCourierWidths.Add('Tcaron', 600);
    FCourierWidths.Add('tcommaaccent', 600);
    FCourierWidths.Add('Tcommaaccent', 600);
    FCourierWidths.Add('thorn', 600);
    FCourierWidths.Add('Thorn', 600);
    FCourierWidths.Add('three', 600);
    FCourierWidths.Add('threequarters', 600);
    FCourierWidths.Add('threesuperior', 600);
    FCourierWidths.Add('tilde', 600);
    FCourierWidths.Add('trademark', 600);
    FCourierWidths.Add('two', 600);
    FCourierWidths.Add('twosuperior', 600);
    FCourierWidths.Add('u', 600);
    FCourierWidths.Add('U', 600);
    FCourierWidths.Add('uacute', 600);
    FCourierWidths.Add('Uacute', 600);
    FCourierWidths.Add('ucircumflex', 600);
    FCourierWidths.Add('Ucircumflex', 600);
    FCourierWidths.Add('udieresis', 600);
    FCourierWidths.Add('Udieresis', 600);
    FCourierWidths.Add('ugrave', 600);
    FCourierWidths.Add('Ugrave', 600);
    FCourierWidths.Add('uhungarumlaut', 600);
    FCourierWidths.Add('Uhungarumlaut', 600);
    FCourierWidths.Add('umacron', 600);
    FCourierWidths.Add('Umacron', 600);
    FCourierWidths.Add('underscore', 600);
    FCourierWidths.Add('uogonek', 600);
    FCourierWidths.Add('Uogonek', 600);
    FCourierWidths.Add('uring', 600);
    FCourierWidths.Add('Uring', 600);
    FCourierWidths.Add('v', 600);
    FCourierWidths.Add('V', 600);
    FCourierWidths.Add('w', 600);
    FCourierWidths.Add('W', 600);
    FCourierWidths.Add('x', 600);
    FCourierWidths.Add('X', 600);
    FCourierWidths.Add('y', 600);
    FCourierWidths.Add('Y', 600);
    FCourierWidths.Add('yacute', 600);
    FCourierWidths.Add('Yacute', 600);
    FCourierWidths.Add('ydieresis', 600);
    FCourierWidths.Add('Ydieresis', 600);
    FCourierWidths.Add('yen', 600);
    FCourierWidths.Add('z', 600);
    FCourierWidths.Add('Z', 600);
    FCourierWidths.Add('zacute', 600);
    FCourierWidths.Add('Zacute', 600);
    FCourierWidths.Add('zcaron', 600);
    FCourierWidths.Add('Zcaron', 600);
    FCourierWidths.Add('zdotaccent', 600);
    FCourierWidths.Add('Zdotaccent', 600);
    FCourierWidths.Add('zero', 600);
    FCourierWidths.TrimExcess;
  end;
  Result := FCourierWidths;
end;

function TdxPDFSimpleFont.GetGlyphMapping: TdxType1FontGlyphMapping;
begin
  UpdateType1FontProgramProperties;
  Result := FGlyphMapping;
end;

function TdxPDFSimpleFontDefaultWidths.GetHelveticaBoldWidths: TdxPDFWordDictionary;
begin
  if FHelveticaBoldWidths.Count = 0 then
  begin
    FHelveticaBoldWidths.Add('a', 556);
    FHelveticaBoldWidths.Add('A', 722);
    FHelveticaBoldWidths.Add('aacute', 556);
    FHelveticaBoldWidths.Add('Aacute', 722);
    FHelveticaBoldWidths.Add('abreve', 556);
    FHelveticaBoldWidths.Add('Abreve', 722);
    FHelveticaBoldWidths.Add('acircumflex', 556);
    FHelveticaBoldWidths.Add('Acircumflex', 722);
    FHelveticaBoldWidths.Add('acute', 333);
    FHelveticaBoldWidths.Add('adieresis', 556);
    FHelveticaBoldWidths.Add('Adieresis', 722);
    FHelveticaBoldWidths.Add('ae', 889);
    FHelveticaBoldWidths.Add('AE', 1000);
    FHelveticaBoldWidths.Add('agrave', 556);
    FHelveticaBoldWidths.Add('Agrave', 722);
    FHelveticaBoldWidths.Add('amacron', 556);
    FHelveticaBoldWidths.Add('Amacron', 722);
    FHelveticaBoldWidths.Add('ampersand', 722);
    FHelveticaBoldWidths.Add('aogonek', 556);
    FHelveticaBoldWidths.Add('Aogonek', 722);
    FHelveticaBoldWidths.Add('aring', 556);
    FHelveticaBoldWidths.Add('Aring', 722);
    FHelveticaBoldWidths.Add('asciicircum', 584);
    FHelveticaBoldWidths.Add('asciitilde', 584);
    FHelveticaBoldWidths.Add('asterisk', 389);
    FHelveticaBoldWidths.Add('at', 975);
    FHelveticaBoldWidths.Add('atilde', 556);
    FHelveticaBoldWidths.Add('Atilde', 722);
    FHelveticaBoldWidths.Add('b', 611);
    FHelveticaBoldWidths.Add('B', 722);
    FHelveticaBoldWidths.Add('backslash', 278);
    FHelveticaBoldWidths.Add('bar', 280);
    FHelveticaBoldWidths.Add('braceleft', 389);
    FHelveticaBoldWidths.Add('braceright', 389);
    FHelveticaBoldWidths.Add('bracketleft', 333);
    FHelveticaBoldWidths.Add('bracketright', 333);
    FHelveticaBoldWidths.Add('breve', 333);
    FHelveticaBoldWidths.Add('brokenbar', 280);
    FHelveticaBoldWidths.Add('bullet', 350);
    FHelveticaBoldWidths.Add('c', 556);
    FHelveticaBoldWidths.Add('C', 722);
    FHelveticaBoldWidths.Add('cacute', 556);
    FHelveticaBoldWidths.Add('Cacute', 722);
    FHelveticaBoldWidths.Add('caron', 333);
    FHelveticaBoldWidths.Add('ccaron', 556);
    FHelveticaBoldWidths.Add('Ccaron', 722);
    FHelveticaBoldWidths.Add('ccedilla', 556);
    FHelveticaBoldWidths.Add('Ccedilla', 722);
    FHelveticaBoldWidths.Add('cedilla', 333);
    FHelveticaBoldWidths.Add('cent', 556);
    FHelveticaBoldWidths.Add('circumflex', 333);
    FHelveticaBoldWidths.Add('colon', 333);
    FHelveticaBoldWidths.Add('comma', 278);
    FHelveticaBoldWidths.Add('commaaccent', 250);
    FHelveticaBoldWidths.Add('copyright', 737);
    FHelveticaBoldWidths.Add('currency', 556);
    FHelveticaBoldWidths.Add('d', 611);
    FHelveticaBoldWidths.Add('D', 722);
    FHelveticaBoldWidths.Add('dagger', 556);
    FHelveticaBoldWidths.Add('daggerdbl', 556);
    FHelveticaBoldWidths.Add('dbldaggerumlaut', 556);
    FHelveticaBoldWidths.Add('dcaron', 743);
    FHelveticaBoldWidths.Add('Dcaron', 722);
    FHelveticaBoldWidths.Add('dcroat', 611);
    FHelveticaBoldWidths.Add('Dcroat', 722);
    FHelveticaBoldWidths.Add('degree', 400);
    FHelveticaBoldWidths.Add('Delta', 612);
    FHelveticaBoldWidths.Add('dieresis', 333);
    FHelveticaBoldWidths.Add('divide', 584);
    FHelveticaBoldWidths.Add('dollar', 556);
    FHelveticaBoldWidths.Add('dotaccent', 333);
    FHelveticaBoldWidths.Add('dotlessi', 278);
    FHelveticaBoldWidths.Add('e', 556);
    FHelveticaBoldWidths.Add('E', 667);
    FHelveticaBoldWidths.Add('eacute', 556);
    FHelveticaBoldWidths.Add('Eacute', 667);
    FHelveticaBoldWidths.Add('ecaron', 556);
    FHelveticaBoldWidths.Add('Ecaron', 667);
    FHelveticaBoldWidths.Add('ecircumflex', 556);
    FHelveticaBoldWidths.Add('Ecircumflex', 667);
    FHelveticaBoldWidths.Add('edieresis', 556);
    FHelveticaBoldWidths.Add('Edieresis', 667);
    FHelveticaBoldWidths.Add('edotaccent', 556);
    FHelveticaBoldWidths.Add('Edotaccent', 667);
    FHelveticaBoldWidths.Add('egrave', 556);
    FHelveticaBoldWidths.Add('Egrave', 667);
    FHelveticaBoldWidths.Add('eight', 556);
    FHelveticaBoldWidths.Add('ellipsis', 1000);
    FHelveticaBoldWidths.Add('emacron', 556);
    FHelveticaBoldWidths.Add('Emacron', 667);
    FHelveticaBoldWidths.Add('emdash', 1000);
    FHelveticaBoldWidths.Add('endash', 556);
    FHelveticaBoldWidths.Add('eogonek', 556);
    FHelveticaBoldWidths.Add('Eogonek', 667);
    FHelveticaBoldWidths.Add('equal', 584);
    FHelveticaBoldWidths.Add('eth', 611);
    FHelveticaBoldWidths.Add('Eth', 722);
    FHelveticaBoldWidths.Add('Euro', 556);
    FHelveticaBoldWidths.Add('exclam', 333);
    FHelveticaBoldWidths.Add('exclamdown', 333);
    FHelveticaBoldWidths.Add('f', 333);
    FHelveticaBoldWidths.Add('F', 611);
    FHelveticaBoldWidths.Add('fi', 611);
    FHelveticaBoldWidths.Add('five', 556);
    FHelveticaBoldWidths.Add('fl', 611);
    FHelveticaBoldWidths.Add('florin', 556);
    FHelveticaBoldWidths.Add('four', 556);
    FHelveticaBoldWidths.Add('fraction', 167);
    FHelveticaBoldWidths.Add('g', 611);
    FHelveticaBoldWidths.Add('G', 778);
    FHelveticaBoldWidths.Add('gbreve', 611);
    FHelveticaBoldWidths.Add('Gbreve', 778);
    FHelveticaBoldWidths.Add('gcommaaccent', 611);
    FHelveticaBoldWidths.Add('Gcommaaccent', 778);
    FHelveticaBoldWidths.Add('germandbls', 611);
    FHelveticaBoldWidths.Add('grave', 333);
    FHelveticaBoldWidths.Add('greater', 584);
    FHelveticaBoldWidths.Add('greaterequal', 549);
    FHelveticaBoldWidths.Add('guillemotleft', 556);
    FHelveticaBoldWidths.Add('guillemotright', 556);
    FHelveticaBoldWidths.Add('guilsinglleft', 333);
    FHelveticaBoldWidths.Add('guilsinglright', 333);
    FHelveticaBoldWidths.Add('h', 611);
    FHelveticaBoldWidths.Add('H', 722);
    FHelveticaBoldWidths.Add('hungarumlaut', 333);
    FHelveticaBoldWidths.Add('hyphen', 333);
    FHelveticaBoldWidths.Add('i', 278);
    FHelveticaBoldWidths.Add('I', 278);
    FHelveticaBoldWidths.Add('iacute', 278);
    FHelveticaBoldWidths.Add('Iacute', 278);
    FHelveticaBoldWidths.Add('icircumflex', 278);
    FHelveticaBoldWidths.Add('Icircumflex', 278);
    FHelveticaBoldWidths.Add('idieresis', 278);
    FHelveticaBoldWidths.Add('Idieresis', 278);
    FHelveticaBoldWidths.Add('Idotaccent', 278);
    FHelveticaBoldWidths.Add('igrave', 278);
    FHelveticaBoldWidths.Add('Igrave', 278);
    FHelveticaBoldWidths.Add('imacron', 278);
    FHelveticaBoldWidths.Add('Imacron', 278);
    FHelveticaBoldWidths.Add('iogonek', 278);
    FHelveticaBoldWidths.Add('Iogonek', 278);
    FHelveticaBoldWidths.Add('j', 278);
    FHelveticaBoldWidths.Add('J', 556);
    FHelveticaBoldWidths.Add('k', 556);
    FHelveticaBoldWidths.Add('K', 722);
    FHelveticaBoldWidths.Add('kcommaaccent', 556);
    FHelveticaBoldWidths.Add('Kcommaaccent', 722);
    FHelveticaBoldWidths.Add('l', 278);
    FHelveticaBoldWidths.Add('L', 611);
    FHelveticaBoldWidths.Add('lacute', 278);
    FHelveticaBoldWidths.Add('Lacute', 611);
    FHelveticaBoldWidths.Add('lcaron', 400);
    FHelveticaBoldWidths.Add('Lcaron', 611);
    FHelveticaBoldWidths.Add('lcommaaccent', 278);
    FHelveticaBoldWidths.Add('Lcommaaccent', 611);
    FHelveticaBoldWidths.Add('less', 584);
    FHelveticaBoldWidths.Add('lessequal', 549);
    FHelveticaBoldWidths.Add('logicalnot', 584);
    FHelveticaBoldWidths.Add('lozenge', 494);
    FHelveticaBoldWidths.Add('lslash', 278);
    FHelveticaBoldWidths.Add('Lslash', 611);
    FHelveticaBoldWidths.Add('m', 889);
    FHelveticaBoldWidths.Add('M', 833);
    FHelveticaBoldWidths.Add('macron', 333);
    FHelveticaBoldWidths.Add('minus', 584);
    FHelveticaBoldWidths.Add('mu', 611);
    FHelveticaBoldWidths.Add('multiply', 584);
    FHelveticaBoldWidths.Add('n', 611);
    FHelveticaBoldWidths.Add('N', 722);
    FHelveticaBoldWidths.Add('nacute', 611);
    FHelveticaBoldWidths.Add('Nacute', 722);
    FHelveticaBoldWidths.Add('ncaron', 611);
    FHelveticaBoldWidths.Add('Ncaron', 722);
    FHelveticaBoldWidths.Add('ncommaaccent', 611);
    FHelveticaBoldWidths.Add('Ncommaaccent', 722);
    FHelveticaBoldWidths.Add('nine', 556);
    FHelveticaBoldWidths.Add('notequal', 549);
    FHelveticaBoldWidths.Add('ntilde', 611);
    FHelveticaBoldWidths.Add('Ntilde', 722);
    FHelveticaBoldWidths.Add('numbersign', 556);
    FHelveticaBoldWidths.Add('o', 611);
    FHelveticaBoldWidths.Add('O', 778);
    FHelveticaBoldWidths.Add('oacute', 611);
    FHelveticaBoldWidths.Add('Oacute', 778);
    FHelveticaBoldWidths.Add('ocircumflex', 611);
    FHelveticaBoldWidths.Add('Ocircumflex', 778);
    FHelveticaBoldWidths.Add('odieresis', 611);
    FHelveticaBoldWidths.Add('Odieresis', 778);
    FHelveticaBoldWidths.Add('oe', 944);
    FHelveticaBoldWidths.Add('OE', 1000);
    FHelveticaBoldWidths.Add('ogonek', 333);
    FHelveticaBoldWidths.Add('ograve', 611);
    FHelveticaBoldWidths.Add('Ograve', 778);
    FHelveticaBoldWidths.Add('ohungarumlaut', 611);
    FHelveticaBoldWidths.Add('Ohungarumlaut', 778);
    FHelveticaBoldWidths.Add('omacron', 611);
    FHelveticaBoldWidths.Add('Omacron', 778);
    FHelveticaBoldWidths.Add('one', 556);
    FHelveticaBoldWidths.Add('onehalf', 834);
    FHelveticaBoldWidths.Add('onequarter', 834);
    FHelveticaBoldWidths.Add('onesuperior', 333);
    FHelveticaBoldWidths.Add('ordfeminine', 370);
    FHelveticaBoldWidths.Add('ordmasculine', 365);
    FHelveticaBoldWidths.Add('oslash', 611);
    FHelveticaBoldWidths.Add('Oslash', 778);
    FHelveticaBoldWidths.Add('otilde', 611);
    FHelveticaBoldWidths.Add('Otilde', 778);
    FHelveticaBoldWidths.Add('p', 611);
    FHelveticaBoldWidths.Add('P', 667);
    FHelveticaBoldWidths.Add('paragraph', 556);
    FHelveticaBoldWidths.Add('parenleft', 333);
    FHelveticaBoldWidths.Add('parenright', 333);
    FHelveticaBoldWidths.Add('partialdiff', 494);
    FHelveticaBoldWidths.Add('percent', 889);
    FHelveticaBoldWidths.Add('period', 278);
    FHelveticaBoldWidths.Add('periodcentered', 278);
    FHelveticaBoldWidths.Add('perthousand', 1000);
    FHelveticaBoldWidths.Add('plus', 584);
    FHelveticaBoldWidths.Add('plusminus', 584);
    FHelveticaBoldWidths.Add('q', 611);
    FHelveticaBoldWidths.Add('Q', 778);
    FHelveticaBoldWidths.Add('question', 611);
    FHelveticaBoldWidths.Add('questiondown', 611);
    FHelveticaBoldWidths.Add('quotedbl', 474);
    FHelveticaBoldWidths.Add('quotedblbase', 500);
    FHelveticaBoldWidths.Add('quotedblleft', 500);
    FHelveticaBoldWidths.Add('quotedblright', 500);
    FHelveticaBoldWidths.Add('quoteleft', 278);
    FHelveticaBoldWidths.Add('quoteright', 278);
    FHelveticaBoldWidths.Add('quotesinglbase', 278);
    FHelveticaBoldWidths.Add('quotesingle', 238);
    FHelveticaBoldWidths.Add('r', 389);
    FHelveticaBoldWidths.Add('R', 722);
    FHelveticaBoldWidths.Add('racute', 389);
    FHelveticaBoldWidths.Add('Racute', 722);
    FHelveticaBoldWidths.Add('radical', 549);
    FHelveticaBoldWidths.Add('rcaron', 389);
    FHelveticaBoldWidths.Add('Rcaron', 722);
    FHelveticaBoldWidths.Add('rcommaaccent', 389);
    FHelveticaBoldWidths.Add('Rcommaaccent', 722);
    FHelveticaBoldWidths.Add('registered', 737);
    FHelveticaBoldWidths.Add('ring', 333);
    FHelveticaBoldWidths.Add('s', 556);
    FHelveticaBoldWidths.Add('S', 667);
    FHelveticaBoldWidths.Add('sacute', 556);
    FHelveticaBoldWidths.Add('Sacute', 667);
    FHelveticaBoldWidths.Add('scaron', 556);
    FHelveticaBoldWidths.Add('Scaron', 667);
    FHelveticaBoldWidths.Add('scedilla', 556);
    FHelveticaBoldWidths.Add('Scedilla', 667);
    FHelveticaBoldWidths.Add('scommaaccent', 556);
    FHelveticaBoldWidths.Add('Scommaaccent', 667);
    FHelveticaBoldWidths.Add('section', 556);
    FHelveticaBoldWidths.Add('semicolon', 333);
    FHelveticaBoldWidths.Add('seven', 556);
    FHelveticaBoldWidths.Add('six', 556);
    FHelveticaBoldWidths.Add('slash', 278);
    FHelveticaBoldWidths.Add('space', 278);
    FHelveticaBoldWidths.Add('sterling', 556);
    FHelveticaBoldWidths.Add('summation', 600);
    FHelveticaBoldWidths.Add('t', 333);
    FHelveticaBoldWidths.Add('T', 611);
    FHelveticaBoldWidths.Add('tcaron', 389);
    FHelveticaBoldWidths.Add('Tcaron', 611);
    FHelveticaBoldWidths.Add('tcommaaccent', 333);
    FHelveticaBoldWidths.Add('Tcommaaccent', 611);
    FHelveticaBoldWidths.Add('thorn', 611);
    FHelveticaBoldWidths.Add('Thorn', 667);
    FHelveticaBoldWidths.Add('three', 556);
    FHelveticaBoldWidths.Add('threequarters', 834);
    FHelveticaBoldWidths.Add('threesuperior', 333);
    FHelveticaBoldWidths.Add('tilde', 333);
    FHelveticaBoldWidths.Add('trademark', 1000);
    FHelveticaBoldWidths.Add('two', 556);
    FHelveticaBoldWidths.Add('twosuperior', 333);
    FHelveticaBoldWidths.Add('u', 611);
    FHelveticaBoldWidths.Add('U', 722);
    FHelveticaBoldWidths.Add('uacute', 611);
    FHelveticaBoldWidths.Add('Uacute', 722);
    FHelveticaBoldWidths.Add('ucircumflex', 611);
    FHelveticaBoldWidths.Add('Ucircumflex', 722);
    FHelveticaBoldWidths.Add('udieresis', 611);
    FHelveticaBoldWidths.Add('Udieresis', 722);
    FHelveticaBoldWidths.Add('ugrave', 611);
    FHelveticaBoldWidths.Add('Ugrave', 722);
    FHelveticaBoldWidths.Add('uhungarumlaut', 611);
    FHelveticaBoldWidths.Add('Uhungarumlaut', 722);
    FHelveticaBoldWidths.Add('umacron', 611);
    FHelveticaBoldWidths.Add('Umacron', 722);
    FHelveticaBoldWidths.Add('underscore', 556);
    FHelveticaBoldWidths.Add('uogonek', 611);
    FHelveticaBoldWidths.Add('Uogonek', 722);
    FHelveticaBoldWidths.Add('uring', 611);
    FHelveticaBoldWidths.Add('Uring', 722);
    FHelveticaBoldWidths.Add('v', 556);
    FHelveticaBoldWidths.Add('V', 667);
    FHelveticaBoldWidths.Add('w', 778);
    FHelveticaBoldWidths.Add('W', 944);
    FHelveticaBoldWidths.Add('x', 556);
    FHelveticaBoldWidths.Add('X', 667);
    FHelveticaBoldWidths.Add('y', 556);
    FHelveticaBoldWidths.Add('Y', 667);
    FHelveticaBoldWidths.Add('yacute', 556);
    FHelveticaBoldWidths.Add('Yacute', 667);
    FHelveticaBoldWidths.Add('ydieresis', 556);
    FHelveticaBoldWidths.Add('Ydieresis', 667);
    FHelveticaBoldWidths.Add('yen', 556);
    FHelveticaBoldWidths.Add('z', 500);
    FHelveticaBoldWidths.Add('Z', 611);
    FHelveticaBoldWidths.Add('zacute', 500);
    FHelveticaBoldWidths.Add('Zacute', 611);
    FHelveticaBoldWidths.Add('zcaron', 500);
    FHelveticaBoldWidths.Add('Zcaron', 611);
    FHelveticaBoldWidths.Add('zdotaccent', 500);
    FHelveticaBoldWidths.Add('Zdotaccent', 611);
    FHelveticaBoldWidths.Add('zero', 556);
    FHelveticaBoldWidths.TrimExcess;
  end;
  Result := FHelveticaBoldWidths;
end;

function TdxPDFSimpleFontDefaultWidths.GetHelveticaBoldObliqueWidths: TdxPDFWordDictionary;
begin
  if FHelveticaBoldObliqueWidths.Count = 0 then
  begin
    FHelveticaBoldObliqueWidths.Add('a', 556);
    FHelveticaBoldObliqueWidths.Add('A', 722);
    FHelveticaBoldObliqueWidths.Add('aacute', 556);
    FHelveticaBoldObliqueWidths.Add('Aacute', 722);
    FHelveticaBoldObliqueWidths.Add('abreve', 556);
    FHelveticaBoldObliqueWidths.Add('Abreve', 722);
    FHelveticaBoldObliqueWidths.Add('acircumflex', 556);
    FHelveticaBoldObliqueWidths.Add('Acircumflex', 722);
    FHelveticaBoldObliqueWidths.Add('acute', 333);
    FHelveticaBoldObliqueWidths.Add('adieresis', 556);
    FHelveticaBoldObliqueWidths.Add('Adieresis', 722);
    FHelveticaBoldObliqueWidths.Add('ae', 889);
    FHelveticaBoldObliqueWidths.Add('AE', 1000);
    FHelveticaBoldObliqueWidths.Add('agrave', 556);
    FHelveticaBoldObliqueWidths.Add('Agrave', 722);
    FHelveticaBoldObliqueWidths.Add('amacron', 556);
    FHelveticaBoldObliqueWidths.Add('Amacron', 722);
    FHelveticaBoldObliqueWidths.Add('ampersand', 722);
    FHelveticaBoldObliqueWidths.Add('aogonek', 556);
    FHelveticaBoldObliqueWidths.Add('Aogonek', 722);
    FHelveticaBoldObliqueWidths.Add('aring', 556);
    FHelveticaBoldObliqueWidths.Add('Aring', 722);
    FHelveticaBoldObliqueWidths.Add('asciicircum', 584);
    FHelveticaBoldObliqueWidths.Add('asciitilde', 584);
    FHelveticaBoldObliqueWidths.Add('asterisk', 389);
    FHelveticaBoldObliqueWidths.Add('at', 975);
    FHelveticaBoldObliqueWidths.Add('atilde', 556);
    FHelveticaBoldObliqueWidths.Add('Atilde', 722);
    FHelveticaBoldObliqueWidths.Add('b', 611);
    FHelveticaBoldObliqueWidths.Add('B', 722);
    FHelveticaBoldObliqueWidths.Add('backslash', 278);
    FHelveticaBoldObliqueWidths.Add('bar', 280);
    FHelveticaBoldObliqueWidths.Add('braceleft', 389);
    FHelveticaBoldObliqueWidths.Add('braceright', 389);
    FHelveticaBoldObliqueWidths.Add('bracketleft', 333);
    FHelveticaBoldObliqueWidths.Add('bracketright', 333);
    FHelveticaBoldObliqueWidths.Add('breve', 333);
    FHelveticaBoldObliqueWidths.Add('brokenbar', 280);
    FHelveticaBoldObliqueWidths.Add('bullet', 350);
    FHelveticaBoldObliqueWidths.Add('c', 556);
    FHelveticaBoldObliqueWidths.Add('C', 722);
    FHelveticaBoldObliqueWidths.Add('cacute', 556);
    FHelveticaBoldObliqueWidths.Add('Cacute', 722);
    FHelveticaBoldObliqueWidths.Add('caron', 333);
    FHelveticaBoldObliqueWidths.Add('ccaron', 556);
    FHelveticaBoldObliqueWidths.Add('Ccaron', 722);
    FHelveticaBoldObliqueWidths.Add('ccedilla', 556);
    FHelveticaBoldObliqueWidths.Add('Ccedilla', 722);
    FHelveticaBoldObliqueWidths.Add('cedilla', 333);
    FHelveticaBoldObliqueWidths.Add('cent', 556);
    FHelveticaBoldObliqueWidths.Add('circumflex', 333);
    FHelveticaBoldObliqueWidths.Add('colon', 333);
    FHelveticaBoldObliqueWidths.Add('comma', 278);
    FHelveticaBoldObliqueWidths.Add('commaaccent', 250);
    FHelveticaBoldObliqueWidths.Add('copyright', 737);
    FHelveticaBoldObliqueWidths.Add('currency', 556);
    FHelveticaBoldObliqueWidths.Add('d', 611);
    FHelveticaBoldObliqueWidths.Add('D', 722);
    FHelveticaBoldObliqueWidths.Add('dagger', 556);
    FHelveticaBoldObliqueWidths.Add('daggerdbl', 556);
    FHelveticaBoldObliqueWidths.Add('dcaron', 743);
    FHelveticaBoldObliqueWidths.Add('Dcaron', 722);
    FHelveticaBoldObliqueWidths.Add('dcroat', 611);
    FHelveticaBoldObliqueWidths.Add('Dcroat', 722);
    FHelveticaBoldObliqueWidths.Add('degree', 400);
    FHelveticaBoldObliqueWidths.Add('Delta', 612);
    FHelveticaBoldObliqueWidths.Add('dieresis', 333);
    FHelveticaBoldObliqueWidths.Add('divide', 584);
    FHelveticaBoldObliqueWidths.Add('dollar', 556);
    FHelveticaBoldObliqueWidths.Add('dotaccent', 333);
    FHelveticaBoldObliqueWidths.Add('dotlessi', 278);
    FHelveticaBoldObliqueWidths.Add('e', 556);
    FHelveticaBoldObliqueWidths.Add('E', 667);
    FHelveticaBoldObliqueWidths.Add('eacute', 556);
    FHelveticaBoldObliqueWidths.Add('Eacute', 667);
    FHelveticaBoldObliqueWidths.Add('ecaron', 556);
    FHelveticaBoldObliqueWidths.Add('Ecaron', 667);
    FHelveticaBoldObliqueWidths.Add('ecircumflex', 556);
    FHelveticaBoldObliqueWidths.Add('Ecircumflex', 667);
    FHelveticaBoldObliqueWidths.Add('edieresis', 556);
    FHelveticaBoldObliqueWidths.Add('Edieresis', 667);
    FHelveticaBoldObliqueWidths.Add('edotaccent', 556);
    FHelveticaBoldObliqueWidths.Add('Edotaccent', 667);
    FHelveticaBoldObliqueWidths.Add('egrave', 556);
    FHelveticaBoldObliqueWidths.Add('Egrave', 667);
    FHelveticaBoldObliqueWidths.Add('eight', 556);
    FHelveticaBoldObliqueWidths.Add('ellipsis', 1000);
    FHelveticaBoldObliqueWidths.Add('emacron', 556);
    FHelveticaBoldObliqueWidths.Add('Emacron', 667);
    FHelveticaBoldObliqueWidths.Add('emdash', 1000);
    FHelveticaBoldObliqueWidths.Add('endash', 556);
    FHelveticaBoldObliqueWidths.Add('eogonek', 556);
    FHelveticaBoldObliqueWidths.Add('Eogonek', 667);
    FHelveticaBoldObliqueWidths.Add('equal', 584);
    FHelveticaBoldObliqueWidths.Add('eth', 611);
    FHelveticaBoldObliqueWidths.Add('Eth', 722);
    FHelveticaBoldObliqueWidths.Add('Euro', 556);
    FHelveticaBoldObliqueWidths.Add('exclam', 333);
    FHelveticaBoldObliqueWidths.Add('exclamdown', 333);
    FHelveticaBoldObliqueWidths.Add('f', 333);
    FHelveticaBoldObliqueWidths.Add('F', 611);
    FHelveticaBoldObliqueWidths.Add('fi', 611);
    FHelveticaBoldObliqueWidths.Add('five', 556);
    FHelveticaBoldObliqueWidths.Add('fl', 611);
    FHelveticaBoldObliqueWidths.Add('florin', 556);
    FHelveticaBoldObliqueWidths.Add('four', 556);
    FHelveticaBoldObliqueWidths.Add('fraction', 167);
    FHelveticaBoldObliqueWidths.Add('g', 611);
    FHelveticaBoldObliqueWidths.Add('G', 778);
    FHelveticaBoldObliqueWidths.Add('gbreve', 611);
    FHelveticaBoldObliqueWidths.Add('Gbreve', 778);
    FHelveticaBoldObliqueWidths.Add('gcommaaccent', 611);
    FHelveticaBoldObliqueWidths.Add('Gcommaaccent', 778);
    FHelveticaBoldObliqueWidths.Add('germandbls', 611);
    FHelveticaBoldObliqueWidths.Add('grave', 333);
    FHelveticaBoldObliqueWidths.Add('greater', 584);
    FHelveticaBoldObliqueWidths.Add('greaterequal', 549);
    FHelveticaBoldObliqueWidths.Add('guillemotleft', 556);
    FHelveticaBoldObliqueWidths.Add('guillemotright', 556);
    FHelveticaBoldObliqueWidths.Add('guilsinglleft', 333);
    FHelveticaBoldObliqueWidths.Add('guilsinglright', 333);
    FHelveticaBoldObliqueWidths.Add('h', 611);
    FHelveticaBoldObliqueWidths.Add('H', 722);
    FHelveticaBoldObliqueWidths.Add('hungarumlaut', 333);
    FHelveticaBoldObliqueWidths.Add('hyphen', 333);
    FHelveticaBoldObliqueWidths.Add('i', 278);
    FHelveticaBoldObliqueWidths.Add('I', 278);
    FHelveticaBoldObliqueWidths.Add('iacute', 278);
    FHelveticaBoldObliqueWidths.Add('Iacute', 278);
    FHelveticaBoldObliqueWidths.Add('icircumflex', 278);
    FHelveticaBoldObliqueWidths.Add('Icircumflex', 278);
    FHelveticaBoldObliqueWidths.Add('idieresis', 278);
    FHelveticaBoldObliqueWidths.Add('Idieresis', 278);
    FHelveticaBoldObliqueWidths.Add('Idotaccent', 278);
    FHelveticaBoldObliqueWidths.Add('igrave', 278);
    FHelveticaBoldObliqueWidths.Add('Igrave', 278);
    FHelveticaBoldObliqueWidths.Add('imacron', 278);
    FHelveticaBoldObliqueWidths.Add('Imacron', 278);
    FHelveticaBoldObliqueWidths.Add('iogonek', 278);
    FHelveticaBoldObliqueWidths.Add('Iogonek', 278);
    FHelveticaBoldObliqueWidths.Add('j', 278);
    FHelveticaBoldObliqueWidths.Add('J', 556);
    FHelveticaBoldObliqueWidths.Add('k', 556);
    FHelveticaBoldObliqueWidths.Add('K', 722);
    FHelveticaBoldObliqueWidths.Add('kcommaaccent', 556);
    FHelveticaBoldObliqueWidths.Add('Kcommaaccent', 722);
    FHelveticaBoldObliqueWidths.Add('l', 278);
    FHelveticaBoldObliqueWidths.Add('L', 611);
    FHelveticaBoldObliqueWidths.Add('lacute', 278);
    FHelveticaBoldObliqueWidths.Add('Lacute', 611);
    FHelveticaBoldObliqueWidths.Add('lcaron', 400);
    FHelveticaBoldObliqueWidths.Add('Lcaron', 611);
    FHelveticaBoldObliqueWidths.Add('lcommaaccent', 278);
    FHelveticaBoldObliqueWidths.Add('Lcommaaccent', 611);
    FHelveticaBoldObliqueWidths.Add('less', 584);
    FHelveticaBoldObliqueWidths.Add('lessequal', 549);
    FHelveticaBoldObliqueWidths.Add('logicalnot', 584);
    FHelveticaBoldObliqueWidths.Add('lozenge', 494);
    FHelveticaBoldObliqueWidths.Add('lslash', 278);
    FHelveticaBoldObliqueWidths.Add('Lslash', 611);
    FHelveticaBoldObliqueWidths.Add('m', 889);
    FHelveticaBoldObliqueWidths.Add('M', 833);
    FHelveticaBoldObliqueWidths.Add('macron', 333);
    FHelveticaBoldObliqueWidths.Add('minus', 584);
    FHelveticaBoldObliqueWidths.Add('mu', 611);
    FHelveticaBoldObliqueWidths.Add('multiply', 584);
    FHelveticaBoldObliqueWidths.Add('n', 611);
    FHelveticaBoldObliqueWidths.Add('N', 722);
    FHelveticaBoldObliqueWidths.Add('nacute', 611);
    FHelveticaBoldObliqueWidths.Add('Nacute', 722);
    FHelveticaBoldObliqueWidths.Add('ncaron', 611);
    FHelveticaBoldObliqueWidths.Add('Ncaron', 722);
    FHelveticaBoldObliqueWidths.Add('ncommaaccent', 611);
    FHelveticaBoldObliqueWidths.Add('Ncommaaccent', 722);
    FHelveticaBoldObliqueWidths.Add('nine', 556);
    FHelveticaBoldObliqueWidths.Add('notequal', 549);
    FHelveticaBoldObliqueWidths.Add('ntilde', 611);
    FHelveticaBoldObliqueWidths.Add('Ntilde', 722);
    FHelveticaBoldObliqueWidths.Add('numbersign', 556);
    FHelveticaBoldObliqueWidths.Add('o', 611);
    FHelveticaBoldObliqueWidths.Add('O', 778);
    FHelveticaBoldObliqueWidths.Add('oacute', 611);
    FHelveticaBoldObliqueWidths.Add('Oacute', 778);
    FHelveticaBoldObliqueWidths.Add('ocircumflex', 611);
    FHelveticaBoldObliqueWidths.Add('Ocircumflex', 778);
    FHelveticaBoldObliqueWidths.Add('odieresis', 611);
    FHelveticaBoldObliqueWidths.Add('Odieresis', 778);
    FHelveticaBoldObliqueWidths.Add('oe', 944);
    FHelveticaBoldObliqueWidths.Add('OE', 1000);
    FHelveticaBoldObliqueWidths.Add('ogonek', 333);
    FHelveticaBoldObliqueWidths.Add('ograve', 611);
    FHelveticaBoldObliqueWidths.Add('Ograve', 778);
    FHelveticaBoldObliqueWidths.Add('ohungarumlaut', 611);
    FHelveticaBoldObliqueWidths.Add('Ohungarumlaut', 778);
    FHelveticaBoldObliqueWidths.Add('omacron', 611);
    FHelveticaBoldObliqueWidths.Add('Omacron', 778);
    FHelveticaBoldObliqueWidths.Add('one', 556);
    FHelveticaBoldObliqueWidths.Add('onehalf', 834);
    FHelveticaBoldObliqueWidths.Add('onequarter', 834);
    FHelveticaBoldObliqueWidths.Add('onesuperior', 333);
    FHelveticaBoldObliqueWidths.Add('ordfeminine', 370);
    FHelveticaBoldObliqueWidths.Add('ordmasculine', 365);
    FHelveticaBoldObliqueWidths.Add('oslash', 611);
    FHelveticaBoldObliqueWidths.Add('Oslash', 778);
    FHelveticaBoldObliqueWidths.Add('otilde', 611);
    FHelveticaBoldObliqueWidths.Add('Otilde', 778);
    FHelveticaBoldObliqueWidths.Add('p', 611);
    FHelveticaBoldObliqueWidths.Add('P', 667);
    FHelveticaBoldObliqueWidths.Add('paragraph', 556);
    FHelveticaBoldObliqueWidths.Add('parenleft', 333);
    FHelveticaBoldObliqueWidths.Add('parenright', 333);
    FHelveticaBoldObliqueWidths.Add('partialdiff', 494);
    FHelveticaBoldObliqueWidths.Add('percent', 889);
    FHelveticaBoldObliqueWidths.Add('period', 278);
    FHelveticaBoldObliqueWidths.Add('periodcentered', 278);
    FHelveticaBoldObliqueWidths.Add('perthousand', 1000);
    FHelveticaBoldObliqueWidths.Add('plus', 584);
    FHelveticaBoldObliqueWidths.Add('plusminus', 584);
    FHelveticaBoldObliqueWidths.Add('q', 611);
    FHelveticaBoldObliqueWidths.Add('Q', 778);
    FHelveticaBoldObliqueWidths.Add('question', 611);
    FHelveticaBoldObliqueWidths.Add('questiondown', 611);
    FHelveticaBoldObliqueWidths.Add('quotedbl', 474);
    FHelveticaBoldObliqueWidths.Add('quotedblbase', 500);
    FHelveticaBoldObliqueWidths.Add('quotedblleft', 500);
    FHelveticaBoldObliqueWidths.Add('quotedblright', 500);
    FHelveticaBoldObliqueWidths.Add('quoteleft', 278);
    FHelveticaBoldObliqueWidths.Add('quoteright', 278);
    FHelveticaBoldObliqueWidths.Add('quotesinglbase', 278);
    FHelveticaBoldObliqueWidths.Add('quotesingle', 238);
    FHelveticaBoldObliqueWidths.Add('r', 389);
    FHelveticaBoldObliqueWidths.Add('R', 722);
    FHelveticaBoldObliqueWidths.Add('racute', 389);
    FHelveticaBoldObliqueWidths.Add('Racute', 722);
    FHelveticaBoldObliqueWidths.Add('radical', 549);
    FHelveticaBoldObliqueWidths.Add('rcaron', 389);
    FHelveticaBoldObliqueWidths.Add('Rcaron', 722);
    FHelveticaBoldObliqueWidths.Add('rcommaaccent', 389);
    FHelveticaBoldObliqueWidths.Add('Rcommaaccent', 722);
    FHelveticaBoldObliqueWidths.Add('registered', 737);
    FHelveticaBoldObliqueWidths.Add('ring', 333);
    FHelveticaBoldObliqueWidths.Add('s', 556);
    FHelveticaBoldObliqueWidths.Add('S', 667);
    FHelveticaBoldObliqueWidths.Add('sacute', 556);
    FHelveticaBoldObliqueWidths.Add('Sacute', 667);
    FHelveticaBoldObliqueWidths.Add('scaron', 556);
    FHelveticaBoldObliqueWidths.Add('Scaron', 667);
    FHelveticaBoldObliqueWidths.Add('scedilla', 556);
    FHelveticaBoldObliqueWidths.Add('Scedilla', 667);
    FHelveticaBoldObliqueWidths.Add('scommaaccent', 556);
    FHelveticaBoldObliqueWidths.Add('Scommaaccent', 667);
    FHelveticaBoldObliqueWidths.Add('section', 556);
    FHelveticaBoldObliqueWidths.Add('semicolon', 333);
    FHelveticaBoldObliqueWidths.Add('seven', 556);
    FHelveticaBoldObliqueWidths.Add('six', 556);
    FHelveticaBoldObliqueWidths.Add('slash', 278);
    FHelveticaBoldObliqueWidths.Add('space', 278);
    FHelveticaBoldObliqueWidths.Add('sterling', 556);
    FHelveticaBoldObliqueWidths.Add('summation', 600);
    FHelveticaBoldObliqueWidths.Add('t', 333);
    FHelveticaBoldObliqueWidths.Add('T', 611);
    FHelveticaBoldObliqueWidths.Add('tcaron', 389);
    FHelveticaBoldObliqueWidths.Add('Tcaron', 611);
    FHelveticaBoldObliqueWidths.Add('tcommaaccent', 333);
    FHelveticaBoldObliqueWidths.Add('Tcommaaccent', 611);
    FHelveticaBoldObliqueWidths.Add('thorn', 611);
    FHelveticaBoldObliqueWidths.Add('Thorn', 667);
    FHelveticaBoldObliqueWidths.Add('three', 556);
    FHelveticaBoldObliqueWidths.Add('threequarters', 834);
    FHelveticaBoldObliqueWidths.Add('threesuperior', 333);
    FHelveticaBoldObliqueWidths.Add('tilde', 333);
    FHelveticaBoldObliqueWidths.Add('trademark', 1000);
    FHelveticaBoldObliqueWidths.Add('two', 556);
    FHelveticaBoldObliqueWidths.Add('twosuperior', 333);
    FHelveticaBoldObliqueWidths.Add('u', 611);
    FHelveticaBoldObliqueWidths.Add('U', 722);
    FHelveticaBoldObliqueWidths.Add('uacute', 611);
    FHelveticaBoldObliqueWidths.Add('Uacute', 722);
    FHelveticaBoldObliqueWidths.Add('ucircumflex', 611);
    FHelveticaBoldObliqueWidths.Add('Ucircumflex', 722);
    FHelveticaBoldObliqueWidths.Add('udieresis', 611);
    FHelveticaBoldObliqueWidths.Add('Udieresis', 722);
    FHelveticaBoldObliqueWidths.Add('ugrave', 611);
    FHelveticaBoldObliqueWidths.Add('Ugrave', 722);
    FHelveticaBoldObliqueWidths.Add('uhungarumlaut', 611);
    FHelveticaBoldObliqueWidths.Add('Uhungarumlaut', 722);
    FHelveticaBoldObliqueWidths.Add('umacron', 611);
    FHelveticaBoldObliqueWidths.Add('Umacron', 722);
    FHelveticaBoldObliqueWidths.Add('underscore', 556);
    FHelveticaBoldObliqueWidths.Add('uogonek', 611);
    FHelveticaBoldObliqueWidths.Add('Uogonek', 722);
    FHelveticaBoldObliqueWidths.Add('uring', 611);
    FHelveticaBoldObliqueWidths.Add('Uring', 722);
    FHelveticaBoldObliqueWidths.Add('v', 556);
    FHelveticaBoldObliqueWidths.Add('V', 667);
    FHelveticaBoldObliqueWidths.Add('w', 778);
    FHelveticaBoldObliqueWidths.Add('W', 944);
    FHelveticaBoldObliqueWidths.Add('x', 556);
    FHelveticaBoldObliqueWidths.Add('X', 667);
    FHelveticaBoldObliqueWidths.Add('y', 556);
    FHelveticaBoldObliqueWidths.Add('Y', 667);
    FHelveticaBoldObliqueWidths.Add('yacute', 556);
    FHelveticaBoldObliqueWidths.Add('Yacute', 667);
    FHelveticaBoldObliqueWidths.Add('ydieresis', 556);
    FHelveticaBoldObliqueWidths.Add('Ydieresis', 667);
    FHelveticaBoldObliqueWidths.Add('yen', 556);
    FHelveticaBoldObliqueWidths.Add('z', 500);
    FHelveticaBoldObliqueWidths.Add('Z', 611);
    FHelveticaBoldObliqueWidths.Add('zacute', 500);
    FHelveticaBoldObliqueWidths.Add('Zacute', 611);
    FHelveticaBoldObliqueWidths.Add('zcaron', 500);
    FHelveticaBoldObliqueWidths.Add('Zcaron', 611);
    FHelveticaBoldObliqueWidths.Add('zdotaccent', 500);
    FHelveticaBoldObliqueWidths.Add('Zdotaccent', 611);
    FHelveticaBoldObliqueWidths.Add('zero', 556);
    FHelveticaBoldObliqueWidths.TrimExcess;
  end;
  Result := FHelveticaBoldObliqueWidths;
end;

function TdxPDFSimpleFontDefaultWidths.GetHelveticaObliqueWidths: TdxPDFWordDictionary;
begin
  if FHelveticaObliqueWidths.Count = 0 then
  begin
    FHelveticaObliqueWidths.Add('a', 556);
    FHelveticaObliqueWidths.Add('A', 667);
    FHelveticaObliqueWidths.Add('aacute', 556);
    FHelveticaObliqueWidths.Add('Aacute', 667);
    FHelveticaObliqueWidths.Add('abreve', 556);
    FHelveticaObliqueWidths.Add('Abreve', 667);
    FHelveticaObliqueWidths.Add('acircumflex', 556);
    FHelveticaObliqueWidths.Add('Acircumflex', 667);
    FHelveticaObliqueWidths.Add('acute', 333);
    FHelveticaObliqueWidths.Add('adieresis', 556);
    FHelveticaObliqueWidths.Add('Adieresis', 667);
    FHelveticaObliqueWidths.Add('ae', 889);
    FHelveticaObliqueWidths.Add('AE', 1000);
    FHelveticaObliqueWidths.Add('agrave', 556);
    FHelveticaObliqueWidths.Add('Agrave', 667);
    FHelveticaObliqueWidths.Add('amacron', 556);
    FHelveticaObliqueWidths.Add('Amacron', 667);
    FHelveticaObliqueWidths.Add('ampersand', 667);
    FHelveticaObliqueWidths.Add('aogonek', 556);
    FHelveticaObliqueWidths.Add('Aogonek', 667);
    FHelveticaObliqueWidths.Add('aring', 556);
    FHelveticaObliqueWidths.Add('Aring', 667);
    FHelveticaObliqueWidths.Add('asciicircum', 469);
    FHelveticaObliqueWidths.Add('asciitilde', 584);
    FHelveticaObliqueWidths.Add('asterisk', 389);
    FHelveticaObliqueWidths.Add('at', 1015);
    FHelveticaObliqueWidths.Add('atilde', 556);
    FHelveticaObliqueWidths.Add('Atilde', 667);
    FHelveticaObliqueWidths.Add('b', 556);
    FHelveticaObliqueWidths.Add('B', 667);
    FHelveticaObliqueWidths.Add('backslash', 278);
    FHelveticaObliqueWidths.Add('bar', 260);
    FHelveticaObliqueWidths.Add('braceleft', 334);
    FHelveticaObliqueWidths.Add('braceright', 334);
    FHelveticaObliqueWidths.Add('bracketleft', 278);
    FHelveticaObliqueWidths.Add('bracketright', 278);
    FHelveticaObliqueWidths.Add('breve', 333);
    FHelveticaObliqueWidths.Add('brokenbar', 260);
    FHelveticaObliqueWidths.Add('bullet', 350);
    FHelveticaObliqueWidths.Add('c', 500);
    FHelveticaObliqueWidths.Add('C', 722);
    FHelveticaObliqueWidths.Add('cacute', 500);
    FHelveticaObliqueWidths.Add('Cacute', 722);
    FHelveticaObliqueWidths.Add('caron', 333);
    FHelveticaObliqueWidths.Add('ccaron', 500);
    FHelveticaObliqueWidths.Add('Ccaron', 722);
    FHelveticaObliqueWidths.Add('ccedilla', 500);
    FHelveticaObliqueWidths.Add('Ccedilla', 722);
    FHelveticaObliqueWidths.Add('cedilla', 333);
    FHelveticaObliqueWidths.Add('cent', 556);
    FHelveticaObliqueWidths.Add('circumflex', 333);
    FHelveticaObliqueWidths.Add('colon', 278);
    FHelveticaObliqueWidths.Add('comma', 278);
    FHelveticaObliqueWidths.Add('commaaccent', 250);
    FHelveticaObliqueWidths.Add('copyright', 737);
    FHelveticaObliqueWidths.Add('currency', 556);
    FHelveticaObliqueWidths.Add('d', 556);
    FHelveticaObliqueWidths.Add('D', 722);
    FHelveticaObliqueWidths.Add('dagger', 556);
    FHelveticaObliqueWidths.Add('daggerdbl', 556);
    FHelveticaObliqueWidths.Add('dcaron', 643);
    FHelveticaObliqueWidths.Add('Dcaron', 722);
    FHelveticaObliqueWidths.Add('dcroat', 556);
    FHelveticaObliqueWidths.Add('Dcroat', 722);
    FHelveticaObliqueWidths.Add('degree', 400);
    FHelveticaObliqueWidths.Add('Delta', 612);
    FHelveticaObliqueWidths.Add('dieresis', 333);
    FHelveticaObliqueWidths.Add('divide', 584);
    FHelveticaObliqueWidths.Add('dollar', 556);
    FHelveticaObliqueWidths.Add('dotaccent', 333);
    FHelveticaObliqueWidths.Add('dotlessi', 278);
    FHelveticaObliqueWidths.Add('e', 556);
    FHelveticaObliqueWidths.Add('E', 667);
    FHelveticaObliqueWidths.Add('eacute', 556);
    FHelveticaObliqueWidths.Add('Eacute', 667);
    FHelveticaObliqueWidths.Add('ecaron', 556);
    FHelveticaObliqueWidths.Add('Ecaron', 667);
    FHelveticaObliqueWidths.Add('ecircumflex', 556);
    FHelveticaObliqueWidths.Add('Ecircumflex', 667);
    FHelveticaObliqueWidths.Add('edieresis', 556);
    FHelveticaObliqueWidths.Add('Edieresis', 667);
    FHelveticaObliqueWidths.Add('edotaccent', 556);
    FHelveticaObliqueWidths.Add('Edotaccent', 667);
    FHelveticaObliqueWidths.Add('egrave', 556);
    FHelveticaObliqueWidths.Add('Egrave', 667);
    FHelveticaObliqueWidths.Add('eight', 556);
    FHelveticaObliqueWidths.Add('ellipsis', 1000);
    FHelveticaObliqueWidths.Add('emacron', 556);
    FHelveticaObliqueWidths.Add('Emacron', 667);
    FHelveticaObliqueWidths.Add('emdash', 1000);
    FHelveticaObliqueWidths.Add('endash', 556);
    FHelveticaObliqueWidths.Add('eogonek', 556);
    FHelveticaObliqueWidths.Add('Eogonek', 667);
    FHelveticaObliqueWidths.Add('equal', 584);
    FHelveticaObliqueWidths.Add('eth', 556);
    FHelveticaObliqueWidths.Add('Eth', 722);
    FHelveticaObliqueWidths.Add('Euro', 556);
    FHelveticaObliqueWidths.Add('exclam', 278);
    FHelveticaObliqueWidths.Add('exclamdown', 333);
    FHelveticaObliqueWidths.Add('f', 278);
    FHelveticaObliqueWidths.Add('F', 611);
    FHelveticaObliqueWidths.Add('fi', 500);
    FHelveticaObliqueWidths.Add('five', 556);
    FHelveticaObliqueWidths.Add('fl', 500);
    FHelveticaObliqueWidths.Add('florin', 556);
    FHelveticaObliqueWidths.Add('four', 556);
    FHelveticaObliqueWidths.Add('fraction', 167);
    FHelveticaObliqueWidths.Add('g', 556);
    FHelveticaObliqueWidths.Add('G', 778);
    FHelveticaObliqueWidths.Add('gbreve', 556);
    FHelveticaObliqueWidths.Add('Gbreve', 778);
    FHelveticaObliqueWidths.Add('gcommaaccent', 556);
    FHelveticaObliqueWidths.Add('Gcommaaccent', 778);
    FHelveticaObliqueWidths.Add('germandbls', 611);
    FHelveticaObliqueWidths.Add('grave', 333);
    FHelveticaObliqueWidths.Add('greater', 584);
    FHelveticaObliqueWidths.Add('greaterequal', 549);
    FHelveticaObliqueWidths.Add('guillemotleft', 556);
    FHelveticaObliqueWidths.Add('guillemotright', 556);
    FHelveticaObliqueWidths.Add('guilsinglleft', 333);
    FHelveticaObliqueWidths.Add('guilsinglright', 333);
    FHelveticaObliqueWidths.Add('h', 556);
    FHelveticaObliqueWidths.Add('H', 722);
    FHelveticaObliqueWidths.Add('hungarumlaut', 333);
    FHelveticaObliqueWidths.Add('hyphen', 333);
    FHelveticaObliqueWidths.Add('i', 222);
    FHelveticaObliqueWidths.Add('I', 278);
    FHelveticaObliqueWidths.Add('iacute', 278);
    FHelveticaObliqueWidths.Add('Iacute', 278);
    FHelveticaObliqueWidths.Add('icircumflex', 278);
    FHelveticaObliqueWidths.Add('Icircumflex', 278);
    FHelveticaObliqueWidths.Add('idieresis', 278);
    FHelveticaObliqueWidths.Add('Idieresis', 278);
    FHelveticaObliqueWidths.Add('Idotaccent', 278);
    FHelveticaObliqueWidths.Add('igrave', 278);
    FHelveticaObliqueWidths.Add('Igrave', 278);
    FHelveticaObliqueWidths.Add('imacron', 278);
    FHelveticaObliqueWidths.Add('Imacron', 278);
    FHelveticaObliqueWidths.Add('iogonek', 222);
    FHelveticaObliqueWidths.Add('Iogonek', 278);
    FHelveticaObliqueWidths.Add('j', 222);
    FHelveticaObliqueWidths.Add('J', 500);
    FHelveticaObliqueWidths.Add('k', 500);
    FHelveticaObliqueWidths.Add('K', 667);
    FHelveticaObliqueWidths.Add('kcommaaccent', 500);
    FHelveticaObliqueWidths.Add('Kcommaaccent', 667);
    FHelveticaObliqueWidths.Add('l', 222);
    FHelveticaObliqueWidths.Add('L', 556);
    FHelveticaObliqueWidths.Add('lacute', 222);
    FHelveticaObliqueWidths.Add('Lacute', 556);
    FHelveticaObliqueWidths.Add('lcaron', 299);
    FHelveticaObliqueWidths.Add('Lcaron', 556);
    FHelveticaObliqueWidths.Add('lcommaaccent', 222);
    FHelveticaObliqueWidths.Add('Lcommaaccent', 556);
    FHelveticaObliqueWidths.Add('less', 584);
    FHelveticaObliqueWidths.Add('lessequal', 549);
    FHelveticaObliqueWidths.Add('logicalnot', 584);
    FHelveticaObliqueWidths.Add('lozenge', 471);
    FHelveticaObliqueWidths.Add('lslash', 222);
    FHelveticaObliqueWidths.Add('Lslash', 556);
    FHelveticaObliqueWidths.Add('m', 833);
    FHelveticaObliqueWidths.Add('M', 833);
    FHelveticaObliqueWidths.Add('macron', 333);
    FHelveticaObliqueWidths.Add('minus', 584);
    FHelveticaObliqueWidths.Add('mu', 556);
    FHelveticaObliqueWidths.Add('multiply', 584);
    FHelveticaObliqueWidths.Add('n', 556);
    FHelveticaObliqueWidths.Add('N', 722);
    FHelveticaObliqueWidths.Add('nacute', 556);
    FHelveticaObliqueWidths.Add('Nacute', 722);
    FHelveticaObliqueWidths.Add('ncaron', 556);
    FHelveticaObliqueWidths.Add('Ncaron', 722);
    FHelveticaObliqueWidths.Add('ncommaaccent', 556);
    FHelveticaObliqueWidths.Add('Ncommaaccent', 722);
    FHelveticaObliqueWidths.Add('nine', 556);
    FHelveticaObliqueWidths.Add('notequal', 549);
    FHelveticaObliqueWidths.Add('ntilde', 556);
    FHelveticaObliqueWidths.Add('Ntilde', 722);
    FHelveticaObliqueWidths.Add('numbersign', 556);
    FHelveticaObliqueWidths.Add('o', 556);
    FHelveticaObliqueWidths.Add('O', 778);
    FHelveticaObliqueWidths.Add('oacute', 556);
    FHelveticaObliqueWidths.Add('Oacute', 778);
    FHelveticaObliqueWidths.Add('ocircumflex', 556);
    FHelveticaObliqueWidths.Add('Ocircumflex', 778);
    FHelveticaObliqueWidths.Add('odieresis', 556);
    FHelveticaObliqueWidths.Add('Odieresis', 778);
    FHelveticaObliqueWidths.Add('oe', 944);
    FHelveticaObliqueWidths.Add('OE', 1000);
    FHelveticaObliqueWidths.Add('ogonek', 333);
    FHelveticaObliqueWidths.Add('ograve', 556);
    FHelveticaObliqueWidths.Add('Ograve', 778);
    FHelveticaObliqueWidths.Add('ohungarumlaut', 556);
    FHelveticaObliqueWidths.Add('Ohungarumlaut', 778);
    FHelveticaObliqueWidths.Add('omacron', 556);
    FHelveticaObliqueWidths.Add('Omacron', 778);
    FHelveticaObliqueWidths.Add('one', 556);
    FHelveticaObliqueWidths.Add('onehalf', 834);
    FHelveticaObliqueWidths.Add('onequarter', 834);
    FHelveticaObliqueWidths.Add('onesuperior', 333);
    FHelveticaObliqueWidths.Add('ordfeminine', 370);
    FHelveticaObliqueWidths.Add('ordmasculine', 365);
    FHelveticaObliqueWidths.Add('oslash', 611);
    FHelveticaObliqueWidths.Add('Oslash', 778);
    FHelveticaObliqueWidths.Add('otilde', 556);
    FHelveticaObliqueWidths.Add('Otilde', 778);
    FHelveticaObliqueWidths.Add('p', 556);
    FHelveticaObliqueWidths.Add('P', 667);
    FHelveticaObliqueWidths.Add('paragraph', 537);
    FHelveticaObliqueWidths.Add('parenleft', 333);
    FHelveticaObliqueWidths.Add('parenright', 333);
    FHelveticaObliqueWidths.Add('partialdiff', 476);
    FHelveticaObliqueWidths.Add('percent', 889);
    FHelveticaObliqueWidths.Add('period', 278);
    FHelveticaObliqueWidths.Add('periodcentered', 278);
    FHelveticaObliqueWidths.Add('perthousand', 1000);
    FHelveticaObliqueWidths.Add('plus', 584);
    FHelveticaObliqueWidths.Add('plusminus', 584);
    FHelveticaObliqueWidths.Add('q', 556);
    FHelveticaObliqueWidths.Add('Q', 778);
    FHelveticaObliqueWidths.Add('question', 556);
    FHelveticaObliqueWidths.Add('questiondown', 611);
    FHelveticaObliqueWidths.Add('quotedbl', 355);
    FHelveticaObliqueWidths.Add('quotedblbase', 333);
    FHelveticaObliqueWidths.Add('quotedblleft', 333);
    FHelveticaObliqueWidths.Add('quotedblright', 333);
    FHelveticaObliqueWidths.Add('quoteleft', 222);
    FHelveticaObliqueWidths.Add('quoteright', 222);
    FHelveticaObliqueWidths.Add('quotesinglbase', 222);
    FHelveticaObliqueWidths.Add('quotesingle', 191);
    FHelveticaObliqueWidths.Add('r', 333);
    FHelveticaObliqueWidths.Add('R', 722);
    FHelveticaObliqueWidths.Add('racute', 333);
    FHelveticaObliqueWidths.Add('Racute', 722);
    FHelveticaObliqueWidths.Add('radical', 453);
    FHelveticaObliqueWidths.Add('rcaron', 333);
    FHelveticaObliqueWidths.Add('Rcaron', 722);
    FHelveticaObliqueWidths.Add('rcommaaccent', 333);
    FHelveticaObliqueWidths.Add('Rcommaaccent', 722);
    FHelveticaObliqueWidths.Add('registered', 737);
    FHelveticaObliqueWidths.Add('ring', 333);
    FHelveticaObliqueWidths.Add('s', 500);
    FHelveticaObliqueWidths.Add('S', 667);
    FHelveticaObliqueWidths.Add('sacute', 500);
    FHelveticaObliqueWidths.Add('Sacute', 667);
    FHelveticaObliqueWidths.Add('scaron', 500);
    FHelveticaObliqueWidths.Add('Scaron', 667);
    FHelveticaObliqueWidths.Add('scedilla', 500);
    FHelveticaObliqueWidths.Add('Scedilla', 667);
    FHelveticaObliqueWidths.Add('scommaaccent', 500);
    FHelveticaObliqueWidths.Add('Scommaaccent', 667);
    FHelveticaObliqueWidths.Add('section', 556);
    FHelveticaObliqueWidths.Add('semicolon', 278);
    FHelveticaObliqueWidths.Add('seven', 556);
    FHelveticaObliqueWidths.Add('six', 556);
    FHelveticaObliqueWidths.Add('slash', 278);
    FHelveticaObliqueWidths.Add('space', 278);
    FHelveticaObliqueWidths.Add('sterling', 556);
    FHelveticaObliqueWidths.Add('summation', 600);
    FHelveticaObliqueWidths.Add('t', 278);
    FHelveticaObliqueWidths.Add('T', 611);
    FHelveticaObliqueWidths.Add('tcaron', 317);
    FHelveticaObliqueWidths.Add('Tcaron', 611);
    FHelveticaObliqueWidths.Add('tcommaaccent', 278);
    FHelveticaObliqueWidths.Add('Tcommaaccent', 611);
    FHelveticaObliqueWidths.Add('thorn', 556);
    FHelveticaObliqueWidths.Add('Thorn', 667);
    FHelveticaObliqueWidths.Add('three', 556);
    FHelveticaObliqueWidths.Add('threequarters', 834);
    FHelveticaObliqueWidths.Add('threesuperior', 333);
    FHelveticaObliqueWidths.Add('tilde', 333);
    FHelveticaObliqueWidths.Add('trademark', 1000);
    FHelveticaObliqueWidths.Add('two', 556);
    FHelveticaObliqueWidths.Add('twosuperior', 333);
    FHelveticaObliqueWidths.Add('u', 556);
    FHelveticaObliqueWidths.Add('U', 722);
    FHelveticaObliqueWidths.Add('uacute', 556);
    FHelveticaObliqueWidths.Add('Uacute', 722);
    FHelveticaObliqueWidths.Add('ucircumflex', 556);
    FHelveticaObliqueWidths.Add('Ucircumflex', 722);
    FHelveticaObliqueWidths.Add('udieresis', 556);
    FHelveticaObliqueWidths.Add('Udieresis', 722);
    FHelveticaObliqueWidths.Add('ugrave', 556);
    FHelveticaObliqueWidths.Add('Ugrave', 722);
    FHelveticaObliqueWidths.Add('uhungarumlaut', 556);
    FHelveticaObliqueWidths.Add('Uhungarumlaut', 722);
    FHelveticaObliqueWidths.Add('umacron', 556);
    FHelveticaObliqueWidths.Add('Umacron', 722);
    FHelveticaObliqueWidths.Add('underscore', 556);
    FHelveticaObliqueWidths.Add('uogonek', 556);
    FHelveticaObliqueWidths.Add('Uogonek', 722);
    FHelveticaObliqueWidths.Add('uring', 556);
    FHelveticaObliqueWidths.Add('Uring', 722);
    FHelveticaObliqueWidths.Add('v', 500);
    FHelveticaObliqueWidths.Add('V', 667);
    FHelveticaObliqueWidths.Add('w', 722);
    FHelveticaObliqueWidths.Add('W', 944);
    FHelveticaObliqueWidths.Add('x', 500);
    FHelveticaObliqueWidths.Add('X', 667);
    FHelveticaObliqueWidths.Add('y', 500);
    FHelveticaObliqueWidths.Add('Y', 667);
    FHelveticaObliqueWidths.Add('yacute', 500);
    FHelveticaObliqueWidths.Add('Yacute', 667);
    FHelveticaObliqueWidths.Add('ydieresis', 500);
    FHelveticaObliqueWidths.Add('Ydieresis', 667);
    FHelveticaObliqueWidths.Add('yen', 556);
    FHelveticaObliqueWidths.Add('z', 500);
    FHelveticaObliqueWidths.Add('Z', 611);
    FHelveticaObliqueWidths.Add('zacute', 500);
    FHelveticaObliqueWidths.Add('Zacute', 611);
    FHelveticaObliqueWidths.Add('zcaron', 500);
    FHelveticaObliqueWidths.Add('Zcaron', 611);
    FHelveticaObliqueWidths.Add('zdotaccent', 500);
    FHelveticaObliqueWidths.Add('Zdotaccent', 611);
    FHelveticaObliqueWidths.Add('zero', 556);
    FHelveticaObliqueWidths.TrimExcess;
  end;
  Result := FHelveticaObliqueWidths;
end;

function TdxPDFSimpleFontDefaultWidths.GetHelveticaWidths: TdxPDFWordDictionary;
begin
  if FHelveticaWidths.Count = 0 then
  begin
    FHelveticaWidths.Add('a', 556);
    FHelveticaWidths.Add('A', 667);
    FHelveticaWidths.Add('aacute', 556);
    FHelveticaWidths.Add('Aacute', 667);
    FHelveticaWidths.Add('abreve', 556);
    FHelveticaWidths.Add('Abreve', 667);
    FHelveticaWidths.Add('acircumflex', 556);
    FHelveticaWidths.Add('Acircumflex', 667);
    FHelveticaWidths.Add('acute', 333);
    FHelveticaWidths.Add('adieresis', 556);
    FHelveticaWidths.Add('Adieresis', 667);
    FHelveticaWidths.Add('ae', 889);
    FHelveticaWidths.Add('AE', 1000);
    FHelveticaWidths.Add('agrave', 556);
    FHelveticaWidths.Add('Agrave', 667);
    FHelveticaWidths.Add('amacron', 556);
    FHelveticaWidths.Add('Amacron', 667);
    FHelveticaWidths.Add('ampersand', 667);
    FHelveticaWidths.Add('aogonek', 556);
    FHelveticaWidths.Add('Aogonek', 667);
    FHelveticaWidths.Add('aring', 556);
    FHelveticaWidths.Add('Aring', 667);
    FHelveticaWidths.Add('asciicircum', 469);
    FHelveticaWidths.Add('asciitilde', 584);
    FHelveticaWidths.Add('asterisk', 389);
    FHelveticaWidths.Add('at', 1015);
    FHelveticaWidths.Add('atilde', 556);
    FHelveticaWidths.Add('Atilde', 667);
    FHelveticaWidths.Add('b', 556);
    FHelveticaWidths.Add('B', 667);
    FHelveticaWidths.Add('backslash', 278);
    FHelveticaWidths.Add('bar', 260);
    FHelveticaWidths.Add('braceleft', 334);
    FHelveticaWidths.Add('braceright', 334);
    FHelveticaWidths.Add('bracketleft', 278);
    FHelveticaWidths.Add('bracketright', 278);
    FHelveticaWidths.Add('breve', 333);
    FHelveticaWidths.Add('brokenbar', 260);
    FHelveticaWidths.Add('bullet', 350);
    FHelveticaWidths.Add('c', 500);
    FHelveticaWidths.Add('C', 722);
    FHelveticaWidths.Add('cacute', 500);
    FHelveticaWidths.Add('Cacute', 722);
    FHelveticaWidths.Add('caron', 333);
    FHelveticaWidths.Add('ccaron', 500);
    FHelveticaWidths.Add('Ccaron', 722);
    FHelveticaWidths.Add('ccedilla', 500);
    FHelveticaWidths.Add('Ccedilla', 722);
    FHelveticaWidths.Add('cedilla', 333);
    FHelveticaWidths.Add('cent', 556);
    FHelveticaWidths.Add('circumflex', 333);
    FHelveticaWidths.Add('colon', 278);
    FHelveticaWidths.Add('comma', 278);
    FHelveticaWidths.Add('commaaccent', 250);
    FHelveticaWidths.Add('copyright', 737);
    FHelveticaWidths.Add('currency', 556);
    FHelveticaWidths.Add('d', 556);
    FHelveticaWidths.Add('D', 722);
    FHelveticaWidths.Add('dagger', 556);
    FHelveticaWidths.Add('daggerdbl', 556);
    FHelveticaWidths.Add('dcaron', 643);
    FHelveticaWidths.Add('Dcaron', 722);
    FHelveticaWidths.Add('dcroat', 556);
    FHelveticaWidths.Add('Dcroat', 722);
    FHelveticaWidths.Add('degree', 400);
    FHelveticaWidths.Add('Delta', 612);
    FHelveticaWidths.Add('dieresis', 333);
    FHelveticaWidths.Add('divide', 584);
    FHelveticaWidths.Add('dollar', 556);
    FHelveticaWidths.Add('dotaccent', 333);
    FHelveticaWidths.Add('dotlessi', 278);
    FHelveticaWidths.Add('e', 556);
    FHelveticaWidths.Add('E', 667);
    FHelveticaWidths.Add('eacute', 556);
    FHelveticaWidths.Add('Eacute', 667);
    FHelveticaWidths.Add('ecaron', 556);
    FHelveticaWidths.Add('Ecaron', 667);
    FHelveticaWidths.Add('ecircumflex', 556);
    FHelveticaWidths.Add('Ecircumflex', 667);
    FHelveticaWidths.Add('edieresis', 556);
    FHelveticaWidths.Add('Edieresis', 667);
    FHelveticaWidths.Add('edotaccent', 556);
    FHelveticaWidths.Add('Edotaccent', 667);
    FHelveticaWidths.Add('egrave', 556);
    FHelveticaWidths.Add('Egrave', 667);
    FHelveticaWidths.Add('eight', 556);
    FHelveticaWidths.Add('ellipsis', 1000);
    FHelveticaWidths.Add('emacron', 556);
    FHelveticaWidths.Add('Emacron', 667);
    FHelveticaWidths.Add('emdash', 1000);
    FHelveticaWidths.Add('endash', 556);
    FHelveticaWidths.Add('eogonek', 556);
    FHelveticaWidths.Add('Eogonek', 667);
    FHelveticaWidths.Add('equal', 584);
    FHelveticaWidths.Add('eth', 556);
    FHelveticaWidths.Add('Eth', 722);
    FHelveticaWidths.Add('Euro', 556);
    FHelveticaWidths.Add('exclam', 278);
    FHelveticaWidths.Add('exclamdown', 333);
    FHelveticaWidths.Add('f', 278);
    FHelveticaWidths.Add('F', 611);
    FHelveticaWidths.Add('fi', 500);
    FHelveticaWidths.Add('five', 556);
    FHelveticaWidths.Add('fl', 500);
    FHelveticaWidths.Add('florin', 556);
    FHelveticaWidths.Add('four', 556);
    FHelveticaWidths.Add('fraction', 167);
    FHelveticaWidths.Add('g', 556);
    FHelveticaWidths.Add('G', 778);
    FHelveticaWidths.Add('gbreve', 556);
    FHelveticaWidths.Add('Gbreve', 778);
    FHelveticaWidths.Add('gcommaaccent', 556);
    FHelveticaWidths.Add('Gcommaaccent', 778);
    FHelveticaWidths.Add('germandbls', 611);
    FHelveticaWidths.Add('grave', 333);
    FHelveticaWidths.Add('greater', 584);
    FHelveticaWidths.Add('greaterequal', 549);
    FHelveticaWidths.Add('guillemotleft', 556);
    FHelveticaWidths.Add('guillemotright', 556);
    FHelveticaWidths.Add('guilsinglleft', 333);
    FHelveticaWidths.Add('guilsinglright', 333);
    FHelveticaWidths.Add('h', 556);
    FHelveticaWidths.Add('H', 722);
    FHelveticaWidths.Add('hungarumlaut', 333);
    FHelveticaWidths.Add('hyphen', 333);
    FHelveticaWidths.Add('i', 222);
    FHelveticaWidths.Add('I', 278);
    FHelveticaWidths.Add('iacute', 278);
    FHelveticaWidths.Add('Iacute', 278);
    FHelveticaWidths.Add('icircumflex', 278);
    FHelveticaWidths.Add('Icircumflex', 278);
    FHelveticaWidths.Add('idieresis', 278);
    FHelveticaWidths.Add('Idieresis', 278);
    FHelveticaWidths.Add('Idotaccent', 278);
    FHelveticaWidths.Add('igrave', 278);
    FHelveticaWidths.Add('Igrave', 278);
    FHelveticaWidths.Add('imacron', 278);
    FHelveticaWidths.Add('Imacron', 278);
    FHelveticaWidths.Add('iogonek', 222);
    FHelveticaWidths.Add('Iogonek', 278);
    FHelveticaWidths.Add('j', 222);
    FHelveticaWidths.Add('J', 500);
    FHelveticaWidths.Add('k', 500);
    FHelveticaWidths.Add('K', 667);
    FHelveticaWidths.Add('kcommaaccent', 500);
    FHelveticaWidths.Add('Kcommaaccent', 667);
    FHelveticaWidths.Add('l', 222);
    FHelveticaWidths.Add('L', 556);
    FHelveticaWidths.Add('lacute', 222);
    FHelveticaWidths.Add('Lacute', 556);
    FHelveticaWidths.Add('lcaron', 299);
    FHelveticaWidths.Add('Lcaron', 556);
    FHelveticaWidths.Add('lcommaaccent', 222);
    FHelveticaWidths.Add('Lcommaaccent', 556);
    FHelveticaWidths.Add('less', 584);
    FHelveticaWidths.Add('lessequal', 549);
    FHelveticaWidths.Add('logicalnot', 584);
    FHelveticaWidths.Add('lozenge', 471);
    FHelveticaWidths.Add('lslash', 222);
    FHelveticaWidths.Add('Lslash', 556);
    FHelveticaWidths.Add('m', 833);
    FHelveticaWidths.Add('M', 833);
    FHelveticaWidths.Add('macron', 333);
    FHelveticaWidths.Add('minus', 584);
    FHelveticaWidths.Add('mu', 556);
    FHelveticaWidths.Add('multiply', 584);
    FHelveticaWidths.Add('n', 556);
    FHelveticaWidths.Add('N', 722);
    FHelveticaWidths.Add('nacute', 556);
    FHelveticaWidths.Add('Nacute', 722);
    FHelveticaWidths.Add('ncaron', 556);
    FHelveticaWidths.Add('Ncaron', 722);
    FHelveticaWidths.Add('ncommaaccent', 556);
    FHelveticaWidths.Add('Ncommaaccent', 722);
    FHelveticaWidths.Add('nine', 556);
    FHelveticaWidths.Add('notequal', 549);
    FHelveticaWidths.Add('ntilde', 556);
    FHelveticaWidths.Add('Ntilde', 722);
    FHelveticaWidths.Add('numbersign', 556);
    FHelveticaWidths.Add('o', 556);
    FHelveticaWidths.Add('O', 778);
    FHelveticaWidths.Add('oacute', 556);
    FHelveticaWidths.Add('Oacute', 778);
    FHelveticaWidths.Add('ocircumflex', 556);
    FHelveticaWidths.Add('Ocircumflex', 778);
    FHelveticaWidths.Add('odieresis', 556);
    FHelveticaWidths.Add('Odieresis', 778);
    FHelveticaWidths.Add('oe', 944);
    FHelveticaWidths.Add('OE', 1000);
    FHelveticaWidths.Add('ogonek', 333);
    FHelveticaWidths.Add('ograve', 556);
    FHelveticaWidths.Add('Ograve', 778);
    FHelveticaWidths.Add('ohungarumlaut', 556);
    FHelveticaWidths.Add('Ohungarumlaut', 778);
    FHelveticaWidths.Add('omacron', 556);
    FHelveticaWidths.Add('Omacron', 778);
    FHelveticaWidths.Add('one', 556);
    FHelveticaWidths.Add('onehalf', 834);
    FHelveticaWidths.Add('onequarter', 834);
    FHelveticaWidths.Add('onesuperior', 333);
    FHelveticaWidths.Add('ordfeminine', 370);
    FHelveticaWidths.Add('ordmasculine', 365);
    FHelveticaWidths.Add('oslash', 611);
    FHelveticaWidths.Add('Oslash', 778);
    FHelveticaWidths.Add('otilde', 556);
    FHelveticaWidths.Add('Otilde', 778);
    FHelveticaWidths.Add('p', 556);
    FHelveticaWidths.Add('P', 667);
    FHelveticaWidths.Add('paragraph', 537);
    FHelveticaWidths.Add('parenleft', 333);
    FHelveticaWidths.Add('parenright', 333);
    FHelveticaWidths.Add('partialdiff', 476);
    FHelveticaWidths.Add('percent', 889);
    FHelveticaWidths.Add('period', 278);
    FHelveticaWidths.Add('periodcentered', 278);
    FHelveticaWidths.Add('perthousand', 1000);
    FHelveticaWidths.Add('plus', 584);
    FHelveticaWidths.Add('plusminus', 584);
    FHelveticaWidths.Add('q', 556);
    FHelveticaWidths.Add('Q', 778);
    FHelveticaWidths.Add('question', 556);
    FHelveticaWidths.Add('questiondown', 611);
    FHelveticaWidths.Add('quotedbl', 355);
    FHelveticaWidths.Add('quotedblbase', 333);
    FHelveticaWidths.Add('quotedblleft', 333);
    FHelveticaWidths.Add('quotedblright', 333);
    FHelveticaWidths.Add('quoteleft', 222);
    FHelveticaWidths.Add('quoteright', 222);
    FHelveticaWidths.Add('quotesinglbase', 222);
    FHelveticaWidths.Add('quotesingle', 191);
    FHelveticaWidths.Add('r', 333);
    FHelveticaWidths.Add('R', 722);
    FHelveticaWidths.Add('racute', 333);
    FHelveticaWidths.Add('Racute', 722);
    FHelveticaWidths.Add('radical', 453);
    FHelveticaWidths.Add('rcaron', 333);
    FHelveticaWidths.Add('Rcaron', 722);
    FHelveticaWidths.Add('rcommaaccent', 333);
    FHelveticaWidths.Add('Rcommaaccent', 722);
    FHelveticaWidths.Add('registered', 737);
    FHelveticaWidths.Add('ring', 333);
    FHelveticaWidths.Add('s', 500);
    FHelveticaWidths.Add('S', 667);
    FHelveticaWidths.Add('sacute', 500);
    FHelveticaWidths.Add('Sacute', 667);
    FHelveticaWidths.Add('scaron', 500);
    FHelveticaWidths.Add('Scaron', 667);
    FHelveticaWidths.Add('scedilla', 500);
    FHelveticaWidths.Add('Scedilla', 667);
    FHelveticaWidths.Add('scommaaccent', 500);
    FHelveticaWidths.Add('Scommaaccent', 667);
    FHelveticaWidths.Add('section', 556);
    FHelveticaWidths.Add('semicolon', 278);
    FHelveticaWidths.Add('seven', 556);
    FHelveticaWidths.Add('six', 556);
    FHelveticaWidths.Add('slash', 278);
    FHelveticaWidths.Add('space', 278);
    FHelveticaWidths.Add('sterling', 556);
    FHelveticaWidths.Add('summation', 600);
    FHelveticaWidths.Add('t', 278);
    FHelveticaWidths.Add('T', 611);
    FHelveticaWidths.Add('tcaron', 317);
    FHelveticaWidths.Add('Tcaron', 611);
    FHelveticaWidths.Add('tcommaaccent', 278);
    FHelveticaWidths.Add('Tcommaaccent', 611);
    FHelveticaWidths.Add('thorn', 556);
    FHelveticaWidths.Add('Thorn', 667);
    FHelveticaWidths.Add('three', 556);
    FHelveticaWidths.Add('threequarters', 834);
    FHelveticaWidths.Add('threesuperior', 333);
    FHelveticaWidths.Add('tilde', 333);
    FHelveticaWidths.Add('trademark', 1000);
    FHelveticaWidths.Add('two', 556);
    FHelveticaWidths.Add('twosuperior', 333);
    FHelveticaWidths.Add('u', 556);
    FHelveticaWidths.Add('U', 722);
    FHelveticaWidths.Add('uacute', 556);
    FHelveticaWidths.Add('Uacute', 722);
    FHelveticaWidths.Add('ucircumflex', 556);
    FHelveticaWidths.Add('Ucircumflex', 722);
    FHelveticaWidths.Add('udieresis', 556);
    FHelveticaWidths.Add('Udieresis', 722);
    FHelveticaWidths.Add('ugrave', 556);
    FHelveticaWidths.Add('Ugrave', 722);
    FHelveticaWidths.Add('uhungarumlaut', 556);
    FHelveticaWidths.Add('Uhungarumlaut', 722);
    FHelveticaWidths.Add('umacron', 556);
    FHelveticaWidths.Add('Umacron', 722);
    FHelveticaWidths.Add('underscore', 556);
    FHelveticaWidths.Add('uogonek', 556);
    FHelveticaWidths.Add('Uogonek', 722);
    FHelveticaWidths.Add('uring', 556);
    FHelveticaWidths.Add('Uring', 722);
    FHelveticaWidths.Add('v', 500);
    FHelveticaWidths.Add('V', 667);
    FHelveticaWidths.Add('w', 722);
    FHelveticaWidths.Add('W', 944);
    FHelveticaWidths.Add('x', 500);
    FHelveticaWidths.Add('X', 667);
    FHelveticaWidths.Add('y', 500);
    FHelveticaWidths.Add('Y', 667);
    FHelveticaWidths.Add('yacute', 500);
    FHelveticaWidths.Add('Yacute', 667);
    FHelveticaWidths.Add('ydieresis', 500);
    FHelveticaWidths.Add('Ydieresis', 667);
    FHelveticaWidths.Add('yen', 556);
    FHelveticaWidths.Add('z', 500);
    FHelveticaWidths.Add('Z', 611);
    FHelveticaWidths.Add('zacute', 500);
    FHelveticaWidths.Add('Zacute', 611);
    FHelveticaWidths.Add('zcaron', 500);
    FHelveticaWidths.Add('Zcaron', 611);
    FHelveticaWidths.Add('zdotaccent', 500);
    FHelveticaWidths.Add('Zdotaccent', 611);
    FHelveticaWidths.Add('zero', 556);

    FHelveticaWidths.TrimExcess;
  end;
  Result := FHelveticaWidths;
end;

function TdxPDFSimpleFontDefaultWidths.GetTimesBoldWidths: TdxPDFWordDictionary;
begin
  if FTimesBoldWidths.Count = 0 then
  begin
    FTimesBoldWidths.Add(TdxGlyphNames._notdef, 250);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerSpace, 250);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerExclam, 333);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerQuotedbl, 555);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerNumbersign, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerDollar, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerPercent, 1000);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerAmpersand, 833);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerQuoteright, 333);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerParenleft, 333);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerParenright, 333);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerAsterisk, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerPlus, 570);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerComma, 250);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerHyphen, 333);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerPeriod, 250);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerSlash, 278);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerZero, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerOne, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerTwo, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerThree, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerFour, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerFive, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerSix, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerSeven, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerEight, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerNine, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerColon, 333);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerSemicolon, 333);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerLess, 570);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerEqual, 570);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerGreater, 570);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerQuestion, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerAt, 930);
    FTimesBoldWidths.Add(TdxGlyphNames.A, 722);
    FTimesBoldWidths.Add(TdxGlyphNames.B, 667);
    FTimesBoldWidths.Add(TdxGlyphNames.C, 722);
    FTimesBoldWidths.Add(TdxGlyphNames.D, 722);
    FTimesBoldWidths.Add(TdxGlyphNames.E, 667);
    FTimesBoldWidths.Add(TdxGlyphNames.F, 611);
    FTimesBoldWidths.Add(TdxGlyphNames.G, 778);
    FTimesBoldWidths.Add(TdxGlyphNames.H, 778);
    FTimesBoldWidths.Add(TdxGlyphNames.I, 389);
    FTimesBoldWidths.Add(TdxGlyphNames.J, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.K, 778);
    FTimesBoldWidths.Add(TdxGlyphNames.L, 667);
    FTimesBoldWidths.Add(TdxGlyphNames.M, 944);
    FTimesBoldWidths.Add(TdxGlyphNames.N, 722);
    FTimesBoldWidths.Add(TdxGlyphNames.O, 778);
    FTimesBoldWidths.Add(TdxGlyphNames.P, 611);
    FTimesBoldWidths.Add(TdxGlyphNames.Q, 778);
    FTimesBoldWidths.Add(TdxGlyphNames.R, 722);
    FTimesBoldWidths.Add(TdxGlyphNames.S, 556);
    FTimesBoldWidths.Add(TdxGlyphNames.T, 667);
    FTimesBoldWidths.Add(TdxGlyphNames.U, 722);
    FTimesBoldWidths.Add(TdxGlyphNames.V, 722);
    FTimesBoldWidths.Add(TdxGlyphNames.W, 1000);
    FTimesBoldWidths.Add(TdxGlyphNames.X, 722);
    FTimesBoldWidths.Add(TdxGlyphNames.Y, 722);
    FTimesBoldWidths.Add(TdxGlyphNames.Z, 667);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerBracketleft, 333);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerBackslash, 278);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerBracketright, 333);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerAsciicircum, 581);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerUnderscore, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerQuoteleft, 333);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerA, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerB, 556);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerC, 444);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerD, 556);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerE, 444);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerF, 333);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerG, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerH, 556);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerI, 278);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerJ, 333);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerK, 556);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerL, 278);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerM, 833);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerN, 556);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerO, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerP, 556);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerQ, 556);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerR, 444);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerS, 389);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerT, 333);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerU, 556);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerV, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerW, 722);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerX, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerY, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerZ, 444);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerBraceleft, 394);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerBar, 220);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerBraceright, 394);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerAsciitilde, 520);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerExclamdown, 333);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerCent, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerSterling, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerFraction, 167);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerYen, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerFlorin, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerSection, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerCurrency, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerQuotesingle, 278);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerQuotedblleft, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerGuillemotleft, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerGuilsinglleft, 333);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerGuilsinglright, 333);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerFi, 556);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerFl, 556);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerEndash, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerDagger, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerDaggerdbl, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerPeriodcentered, 250);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerParagraph, 540);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerBullet, 350);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerQuotesinglbase, 333);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerQuotedblbase, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerQuotedblright, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerGuillemotright, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerEllipsis, 1000);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerPerthousand, 1000);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerQuestiondown, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerGrave, 333);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerAcute, 333);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerCircumflex, 333);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerTilde, 333);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerMacron, 333);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerBreve, 333);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerDotaccent, 333);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerDieresis, 333);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerRing, 333);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerCedilla, 333);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerHungarumlaut, 333);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerOgonek, 333);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerCaron, 333);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerEmdash, 1000);
    FTimesBoldWidths.Add(TdxGlyphNames.AE, 1000);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerOrdfeminine, 300);
    FTimesBoldWidths.Add(TdxGlyphNames.Lslash, 667);
    FTimesBoldWidths.Add(TdxGlyphNames.Oslash, 778);
    FTimesBoldWidths.Add(TdxGlyphNames.OE, 1000);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerOrdmasculine, 330);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerAe, 722);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerDotlessi, 278);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerLslash, 278);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerOslash, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerOe, 722);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerGermandbls, 556);
    FTimesBoldWidths.Add(TdxGlyphNames.Idieresis, 389);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerEacute, 444);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerAbreve, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerUhungarumlaut, 556);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerEcaron, 444);
    FTimesBoldWidths.Add(TdxGlyphNames.Ydieresis, 722);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerDivide, 570);
    FTimesBoldWidths.Add(TdxGlyphNames.Yacute, 722);
    FTimesBoldWidths.Add(TdxGlyphNames.Acircumflex, 722);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerAacute, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.Ucircumflex, 722);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerYacute, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerScommaaccent, 389);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerEcircumflex, 444);
    FTimesBoldWidths.Add(TdxGlyphNames.Uring, 722);
    FTimesBoldWidths.Add(TdxGlyphNames.Udieresis, 722);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerAogonek, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.Uacute, 722);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerUogonek, 556);
    FTimesBoldWidths.Add(TdxGlyphNames.Edieresis, 667);
    FTimesBoldWidths.Add(TdxGlyphNames.Dcroat, 722);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerCommaaccent, 250);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerCopyright, 747);
    FTimesBoldWidths.Add(TdxGlyphNames.Emacron, 667);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerCcaron, 444);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerAring, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.Ncommaaccent, 722);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerLacute, 278);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerAgrave, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.Tcommaaccent, 667);
    FTimesBoldWidths.Add(TdxGlyphNames.Cacute, 722);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerAtilde, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.Edotaccent, 667);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerScaron, 389);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerScedilla, 389);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerIacute, 278);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerLozenge, 494);
    FTimesBoldWidths.Add(TdxGlyphNames.Rcaron, 722);
    FTimesBoldWidths.Add(TdxGlyphNames.Gcommaaccent, 778);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerUcircumflex, 556);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerAcircumflex, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.Amacron, 722);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerRcaron, 444);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerCcedilla, 444);
    FTimesBoldWidths.Add(TdxGlyphNames.Zdotaccent, 667);
    FTimesBoldWidths.Add(TdxGlyphNames.Thorn, 611);
    FTimesBoldWidths.Add(TdxGlyphNames.Omacron, 778);
    FTimesBoldWidths.Add(TdxGlyphNames.Racute, 722);
    FTimesBoldWidths.Add(TdxGlyphNames.Sacute, 556);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerDcaron, 672);
    FTimesBoldWidths.Add(TdxGlyphNames.Umacron, 722);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerUring, 556);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerThreesuperior, 300);
    FTimesBoldWidths.Add(TdxGlyphNames.Ograve, 778);
    FTimesBoldWidths.Add(TdxGlyphNames.Agrave, 722);
    FTimesBoldWidths.Add(TdxGlyphNames.Abreve, 722);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerMultiply, 570);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerUacute, 556);
    FTimesBoldWidths.Add(TdxGlyphNames.Tcaron, 667);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerPartialdiff, 494);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerYdieresis, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.Nacute, 722);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerIcircumflex, 278);
    FTimesBoldWidths.Add(TdxGlyphNames.Ecircumflex, 667);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerAdieresis, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerEdieresis, 444);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerCacute, 444);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerNacute, 556);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerUmacron, 556);
    FTimesBoldWidths.Add(TdxGlyphNames.Ncaron, 722);
    FTimesBoldWidths.Add(TdxGlyphNames.Iacute, 389);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerPlusminus, 570);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerBrokenbar, 220);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerRegistered, 747);
    FTimesBoldWidths.Add(TdxGlyphNames.Gbreve, 778);
    FTimesBoldWidths.Add(TdxGlyphNames.Idotaccent, 389);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerSummation, 600);
    FTimesBoldWidths.Add(TdxGlyphNames.Egrave, 667);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerRacute, 444);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerOmacron, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.Zacute, 667);
    FTimesBoldWidths.Add(TdxGlyphNames.Zcaron, 667);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerGreaterequal, 549);
    FTimesBoldWidths.Add(TdxGlyphNames.Eth, 722);
    FTimesBoldWidths.Add(TdxGlyphNames.Ccedilla, 722);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerLcommaaccent, 278);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerTcaron, 416);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerEogonek, 444);
    FTimesBoldWidths.Add(TdxGlyphNames.Uogonek, 722);
    FTimesBoldWidths.Add(TdxGlyphNames.Aacute, 722);
    FTimesBoldWidths.Add(TdxGlyphNames.Adieresis, 722);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerEgrave, 444);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerZacute, 444);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerIogonek, 278);
    FTimesBoldWidths.Add(TdxGlyphNames.Oacute, 778);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerOacute, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerAmacron, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerSacute, 389);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerIdieresis, 278);
    FTimesBoldWidths.Add(TdxGlyphNames.Ocircumflex, 778);
    FTimesBoldWidths.Add(TdxGlyphNames.Ugrave, 722);
    FTimesBoldWidths.Add(TdxGlyphNames.Delta, 612);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerThorn, 556);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerTwosuperior, 300);
    FTimesBoldWidths.Add(TdxGlyphNames.Odieresis, 778);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerMu, 556);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerIgrave, 278);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerOhungarumlaut, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.Eogonek, 667);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerDcroat, 556);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerThreequarters, 750);
    FTimesBoldWidths.Add(TdxGlyphNames.Scedilla, 556);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerLcaron, 394);
    FTimesBoldWidths.Add(TdxGlyphNames.Kcommaaccent, 778);
    FTimesBoldWidths.Add(TdxGlyphNames.Lacute, 667);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerTrademark, 1000);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerEdotaccent, 444);
    FTimesBoldWidths.Add(TdxGlyphNames.Igrave, 389);
    FTimesBoldWidths.Add(TdxGlyphNames.Imacron, 389);
    FTimesBoldWidths.Add(TdxGlyphNames.Lcaron, 667);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerOnehalf, 750);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerLessequal, 549);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerOcircumflex, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerNtilde, 556);
    FTimesBoldWidths.Add(TdxGlyphNames.Uhungarumlaut, 722);
    FTimesBoldWidths.Add(TdxGlyphNames.Eacute, 667);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerEmacron, 444);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerGbreve, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerOnequarter, 750);
    FTimesBoldWidths.Add(TdxGlyphNames.Scaron, 556);
    FTimesBoldWidths.Add(TdxGlyphNames.Scommaaccent, 556);
    FTimesBoldWidths.Add(TdxGlyphNames.Ohungarumlaut, 778);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerDegree, 400);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerOgrave, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.Ccaron, 722);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerUgrave, 556);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerRadical, 549);
    FTimesBoldWidths.Add(TdxGlyphNames.Dcaron, 722);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerRcommaaccent, 444);
    FTimesBoldWidths.Add(TdxGlyphNames.Ntilde, 722);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerOtilde, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.Rcommaaccent, 722);
    FTimesBoldWidths.Add(TdxGlyphNames.Lcommaaccent, 667);
    FTimesBoldWidths.Add(TdxGlyphNames.Atilde, 722);
    FTimesBoldWidths.Add(TdxGlyphNames.Aogonek, 722);
    FTimesBoldWidths.Add(TdxGlyphNames.Aring, 722);
    FTimesBoldWidths.Add(TdxGlyphNames.Otilde, 778);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerZdotaccent, 444);
    FTimesBoldWidths.Add(TdxGlyphNames.Ecaron, 667);
    FTimesBoldWidths.Add(TdxGlyphNames.Iogonek, 389);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerKcommaaccent, 556);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerMinus, 570);
    FTimesBoldWidths.Add(TdxGlyphNames.Icircumflex, 389);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerNcaron, 556);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerTcommaaccent, 333);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerLogicalnot, 570);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerOdieresis, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerUdieresis, 556);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerNotequal, 549);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerGcommaaccent, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerEth, 500);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerZcaron, 444);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerNcommaaccent, 556);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerOnesuperior, 300);
    FTimesBoldWidths.Add(TdxGlyphNames.LowerImacron, 278);
    FTimesBoldWidths.Add(TdxGlyphNames.Euro, 500);

    FTimesBoldWidths.TrimExcess;
  end;
  Result := FTimesBoldWidths;
end;

function TdxPDFSimpleFontDefaultWidths.GetTimesBoldItalicWidths: TdxPDFWordDictionary;
begin
  if FTimesBoldItalicWidths.Count = 0 then
  begin
    FTimesBoldItalicWidths.Add('a', 500);
    FTimesBoldItalicWidths.Add('A', 667);
    FTimesBoldItalicWidths.Add('aacute', 500);
    FTimesBoldItalicWidths.Add('Aacute', 667);
    FTimesBoldItalicWidths.Add('abreve', 500);
    FTimesBoldItalicWidths.Add('Abreve', 667);
    FTimesBoldItalicWidths.Add('acircumflex', 500);
    FTimesBoldItalicWidths.Add('Acircumflex', 667);
    FTimesBoldItalicWidths.Add('acute', 333);
    FTimesBoldItalicWidths.Add('adieresis', 500);
    FTimesBoldItalicWidths.Add('Adieresis', 667);
    FTimesBoldItalicWidths.Add('ae', 722);
    FTimesBoldItalicWidths.Add('AE', 944);
    FTimesBoldItalicWidths.Add('agrave', 500);
    FTimesBoldItalicWidths.Add('Agrave', 667);
    FTimesBoldItalicWidths.Add('amacron', 500);
    FTimesBoldItalicWidths.Add('Amacron', 667);
    FTimesBoldItalicWidths.Add('ampersand', 778);
    FTimesBoldItalicWidths.Add('aogonek', 500);
    FTimesBoldItalicWidths.Add('Aogonek', 667);
    FTimesBoldItalicWidths.Add('aring', 500);
    FTimesBoldItalicWidths.Add('Aring', 667);
    FTimesBoldItalicWidths.Add('asciicircum', 570);
    FTimesBoldItalicWidths.Add('asciitilde', 570);
    FTimesBoldItalicWidths.Add('asterisk', 500);
    FTimesBoldItalicWidths.Add('at', 832);
    FTimesBoldItalicWidths.Add('atilde', 500);
    FTimesBoldItalicWidths.Add('Atilde', 667);
    FTimesBoldItalicWidths.Add('b', 500);
    FTimesBoldItalicWidths.Add('B', 667);
    FTimesBoldItalicWidths.Add('backslash', 278);
    FTimesBoldItalicWidths.Add('bar', 220);
    FTimesBoldItalicWidths.Add('braceleft', 348);
    FTimesBoldItalicWidths.Add('braceright', 348);
    FTimesBoldItalicWidths.Add('bracketleft', 333);
    FTimesBoldItalicWidths.Add('bracketright', 333);
    FTimesBoldItalicWidths.Add('breve', 333);
    FTimesBoldItalicWidths.Add('brokenbar', 220);
    FTimesBoldItalicWidths.Add('bullet', 350);
    FTimesBoldItalicWidths.Add('c', 444);
    FTimesBoldItalicWidths.Add('C', 667);
    FTimesBoldItalicWidths.Add('cacute', 444);
    FTimesBoldItalicWidths.Add('Cacute', 667);
    FTimesBoldItalicWidths.Add('caron', 333);
    FTimesBoldItalicWidths.Add('ccaron', 444);
    FTimesBoldItalicWidths.Add('Ccaron', 667);
    FTimesBoldItalicWidths.Add('ccedilla', 444);
    FTimesBoldItalicWidths.Add('Ccedilla', 667);
    FTimesBoldItalicWidths.Add('cedilla', 333);
    FTimesBoldItalicWidths.Add('cent', 500);
    FTimesBoldItalicWidths.Add('circumflex', 333);
    FTimesBoldItalicWidths.Add('colon', 333);
    FTimesBoldItalicWidths.Add('comma', 250);
    FTimesBoldItalicWidths.Add('commaaccent', 250);
    FTimesBoldItalicWidths.Add('copyright', 747);
    FTimesBoldItalicWidths.Add('currency', 500);
    FTimesBoldItalicWidths.Add('d', 500);
    FTimesBoldItalicWidths.Add('D', 722);
    FTimesBoldItalicWidths.Add('dagger', 500);
    FTimesBoldItalicWidths.Add('daggerdbl', 500);
    FTimesBoldItalicWidths.Add('dcaron', 608);
    FTimesBoldItalicWidths.Add('Dcaron', 722);
    FTimesBoldItalicWidths.Add('dcroat', 500);
    FTimesBoldItalicWidths.Add('Dcroat', 722);
    FTimesBoldItalicWidths.Add('degree', 400);
    FTimesBoldItalicWidths.Add('Delta', 612);
    FTimesBoldItalicWidths.Add('dieresis', 333);
    FTimesBoldItalicWidths.Add('divide', 570);
    FTimesBoldItalicWidths.Add('dollar', 500);
    FTimesBoldItalicWidths.Add('dotaccent', 667);
    FTimesBoldItalicWidths.Add('dotlessi', 278);
    FTimesBoldItalicWidths.Add('e', 444);
    FTimesBoldItalicWidths.Add('E', 667);
    FTimesBoldItalicWidths.Add('eacute', 444);
    FTimesBoldItalicWidths.Add('Eacute', 667);
    FTimesBoldItalicWidths.Add('ecaron', 444);
    FTimesBoldItalicWidths.Add('Ecaron', 667);
    FTimesBoldItalicWidths.Add('ecircumflex', 444);
    FTimesBoldItalicWidths.Add('Ecircumflex', 667);
    FTimesBoldItalicWidths.Add('edieresis', 444);
    FTimesBoldItalicWidths.Add('Edieresis', 667);
    FTimesBoldItalicWidths.Add('edotaccent', 444);
    FTimesBoldItalicWidths.Add('egrave', 444);
    FTimesBoldItalicWidths.Add('Egrave', 667);
    FTimesBoldItalicWidths.Add('eight', 500);
    FTimesBoldItalicWidths.Add('ellipsis', 1000);
    FTimesBoldItalicWidths.Add('emacron', 444);
    FTimesBoldItalicWidths.Add('Emacron', 667);
    FTimesBoldItalicWidths.Add('emdash', 1000);
    FTimesBoldItalicWidths.Add('endash', 500);
    FTimesBoldItalicWidths.Add('eogonek', 444);
    FTimesBoldItalicWidths.Add('Eogonek', 667);
    FTimesBoldItalicWidths.Add('equal', 570);
    FTimesBoldItalicWidths.Add('eth', 500);
    FTimesBoldItalicWidths.Add('Eth', 722);
    FTimesBoldItalicWidths.Add('Euro', 500);
    FTimesBoldItalicWidths.Add('exclam', 389);
    FTimesBoldItalicWidths.Add('exclamdown', 389);
    FTimesBoldItalicWidths.Add('f', 333);
    FTimesBoldItalicWidths.Add('F', 667);
    FTimesBoldItalicWidths.Add('fi', 556);
    FTimesBoldItalicWidths.Add('five', 500);
    FTimesBoldItalicWidths.Add('fl', 556);
    FTimesBoldItalicWidths.Add('florin', 500);
    FTimesBoldItalicWidths.Add('four', 500);
    FTimesBoldItalicWidths.Add('fraction', 167);
    FTimesBoldItalicWidths.Add('g', 500);
    FTimesBoldItalicWidths.Add('G', 722);
    FTimesBoldItalicWidths.Add('gbreve', 500);
    FTimesBoldItalicWidths.Add('Gbreve', 722);
    FTimesBoldItalicWidths.Add('gcommaaccent', 500);
    FTimesBoldItalicWidths.Add('Gcommaaccent', 722);
    FTimesBoldItalicWidths.Add('germandbls', 500);
    FTimesBoldItalicWidths.Add('grave', 333);
    FTimesBoldItalicWidths.Add('greater', 570);
    FTimesBoldItalicWidths.Add('greaterequal', 549);
    FTimesBoldItalicWidths.Add('guillemotleft', 500);
    FTimesBoldItalicWidths.Add('guillemotright', 500);
    FTimesBoldItalicWidths.Add('guilsinglleft', 333);
    FTimesBoldItalicWidths.Add('guilsinglright', 333);
    FTimesBoldItalicWidths.Add('h', 556);
    FTimesBoldItalicWidths.Add('H', 778);
    FTimesBoldItalicWidths.Add('hungarumlaut', 333);
    FTimesBoldItalicWidths.Add('hyphen', 333);
    FTimesBoldItalicWidths.Add('i', 278);
    FTimesBoldItalicWidths.Add('I', 389);
    FTimesBoldItalicWidths.Add('iacute', 278);
    FTimesBoldItalicWidths.Add('Iacute', 389);
    FTimesBoldItalicWidths.Add('icircumflex', 278);
    FTimesBoldItalicWidths.Add('Icircumflex', 389);
    FTimesBoldItalicWidths.Add('idieresis', 278);
    FTimesBoldItalicWidths.Add('Idieresis', 389);
    FTimesBoldItalicWidths.Add('Idotaccent', 389);
    FTimesBoldItalicWidths.Add('igrave', 278);
    FTimesBoldItalicWidths.Add('Igrave', 389);
    FTimesBoldItalicWidths.Add('imacron', 278);
    FTimesBoldItalicWidths.Add('Imacron', 389);
    FTimesBoldItalicWidths.Add('iogonek', 278);
    FTimesBoldItalicWidths.Add('Iogonek', 389);
    FTimesBoldItalicWidths.Add('j', 278);
    FTimesBoldItalicWidths.Add('J', 500);
    FTimesBoldItalicWidths.Add('k', 500);
    FTimesBoldItalicWidths.Add('K', 667);
    FTimesBoldItalicWidths.Add('kcommaaccent', 500);
    FTimesBoldItalicWidths.Add('Kcommaaccent', 667);
    FTimesBoldItalicWidths.Add('l', 278);
    FTimesBoldItalicWidths.Add('L', 611);
    FTimesBoldItalicWidths.Add('lacute', 278);
    FTimesBoldItalicWidths.Add('Lacute', 611);
    FTimesBoldItalicWidths.Add('lcaron', 382);
    FTimesBoldItalicWidths.Add('Lcaron', 611);
    FTimesBoldItalicWidths.Add('lcommaaccent', 278);
    FTimesBoldItalicWidths.Add('Lcommaaccent', 611);
    FTimesBoldItalicWidths.Add('less', 570);
    FTimesBoldItalicWidths.Add('lessequal', 549);
    FTimesBoldItalicWidths.Add('logicalnot', 606);
    FTimesBoldItalicWidths.Add('lozenge', 494);
    FTimesBoldItalicWidths.Add('lslash', 278);
    FTimesBoldItalicWidths.Add('Lslash', 611);
    FTimesBoldItalicWidths.Add('m', 778);
    FTimesBoldItalicWidths.Add('M', 889);
    FTimesBoldItalicWidths.Add('macron', 333);
    FTimesBoldItalicWidths.Add('minus', 606);
    FTimesBoldItalicWidths.Add('mu', 576);
    FTimesBoldItalicWidths.Add('multiply', 570);
    FTimesBoldItalicWidths.Add('n', 556);
    FTimesBoldItalicWidths.Add('N', 722);
    FTimesBoldItalicWidths.Add('nacute', 556);
    FTimesBoldItalicWidths.Add('Nacute', 722);
    FTimesBoldItalicWidths.Add('ncaron', 556);
    FTimesBoldItalicWidths.Add('Ncaron', 722);
    FTimesBoldItalicWidths.Add('ncommaaccent', 556);
    FTimesBoldItalicWidths.Add('Ncommaaccent', 722);
    FTimesBoldItalicWidths.Add('nine', 500);
    FTimesBoldItalicWidths.Add('notequal', 549);
    FTimesBoldItalicWidths.Add('ntilde', 556);
    FTimesBoldItalicWidths.Add('Ntilde', 722);
    FTimesBoldItalicWidths.Add('numbersign', 500);
    FTimesBoldItalicWidths.Add('o', 500);
    FTimesBoldItalicWidths.Add('O', 722);
    FTimesBoldItalicWidths.Add('oacute', 500);
    FTimesBoldItalicWidths.Add('Oacute', 722);
    FTimesBoldItalicWidths.Add('ocircumflex', 500);
    FTimesBoldItalicWidths.Add('Ocircumflex', 722);
    FTimesBoldItalicWidths.Add('odieresis', 500);
    FTimesBoldItalicWidths.Add('Odieresis', 722);
    FTimesBoldItalicWidths.Add('oe', 722);
    FTimesBoldItalicWidths.Add('OE', 944);
    FTimesBoldItalicWidths.Add('ogonek', 333);
    FTimesBoldItalicWidths.Add('ograve', 500);
    FTimesBoldItalicWidths.Add('Ograve', 722);
    FTimesBoldItalicWidths.Add('ohungarumlaut', 500);
    FTimesBoldItalicWidths.Add('Ohungarumlaut', 722);
    FTimesBoldItalicWidths.Add('omacron', 500);
    FTimesBoldItalicWidths.Add('Omacron', 722);
    FTimesBoldItalicWidths.Add('one', 500);
    FTimesBoldItalicWidths.Add('onehalf', 750);
    FTimesBoldItalicWidths.Add('onequarter', 750);
    FTimesBoldItalicWidths.Add('onesuperior', 300);
    FTimesBoldItalicWidths.Add('ordfeminine', 266);
    FTimesBoldItalicWidths.Add('ordmasculine', 300);
    FTimesBoldItalicWidths.Add('oslash', 500);
    FTimesBoldItalicWidths.Add('Oslash', 722);
    FTimesBoldItalicWidths.Add('otilde', 500);
    FTimesBoldItalicWidths.Add('Otilde', 722);
    FTimesBoldItalicWidths.Add('p', 500);
    FTimesBoldItalicWidths.Add('P', 611);
    FTimesBoldItalicWidths.Add('paragraph', 500);
    FTimesBoldItalicWidths.Add('parenleft', 333);
    FTimesBoldItalicWidths.Add('parenright', 333);
    FTimesBoldItalicWidths.Add('partialdiff', 494);
    FTimesBoldItalicWidths.Add('percent', 833);
    FTimesBoldItalicWidths.Add('period', 250);
    FTimesBoldItalicWidths.Add('periodcentered', 250);
    FTimesBoldItalicWidths.Add('perthousand', 1000);
    FTimesBoldItalicWidths.Add('plus', 570);
    FTimesBoldItalicWidths.Add('plusminus', 570);
    FTimesBoldItalicWidths.Add('q', 500);
    FTimesBoldItalicWidths.Add('Q', 722);
    FTimesBoldItalicWidths.Add('question', 500);
    FTimesBoldItalicWidths.Add('questiondown', 500);
    FTimesBoldItalicWidths.Add('quotedbl', 555);
    FTimesBoldItalicWidths.Add('quotedblbase', 500);
    FTimesBoldItalicWidths.Add('quotedblleft', 500);
    FTimesBoldItalicWidths.Add('quotedblright', 500);
    FTimesBoldItalicWidths.Add('quoteleft', 333);
    FTimesBoldItalicWidths.Add('quoteright', 333);
    FTimesBoldItalicWidths.Add('quotesinglbase', 333);
    FTimesBoldItalicWidths.Add('quotesingle', 278);
    FTimesBoldItalicWidths.Add('r', 389);
    FTimesBoldItalicWidths.Add('R', 667);
    FTimesBoldItalicWidths.Add('racute', 389);
    FTimesBoldItalicWidths.Add('Racute', 667);
    FTimesBoldItalicWidths.Add('radical', 549);
    FTimesBoldItalicWidths.Add('rcaron', 389);
    FTimesBoldItalicWidths.Add('Rcaron', 667);
    FTimesBoldItalicWidths.Add('rcommaaccent', 389);
    FTimesBoldItalicWidths.Add('Rcommaaccent', 667);
    FTimesBoldItalicWidths.Add('registered', 747);
    FTimesBoldItalicWidths.Add('ring', 333);
    FTimesBoldItalicWidths.Add('s', 389);
    FTimesBoldItalicWidths.Add('S', 556);
    FTimesBoldItalicWidths.Add('sacute', 389);
    FTimesBoldItalicWidths.Add('Sacute', 556);
    FTimesBoldItalicWidths.Add('scaron', 389);
    FTimesBoldItalicWidths.Add('Scaron', 556);
    FTimesBoldItalicWidths.Add('scedilla', 389);
    FTimesBoldItalicWidths.Add('Scedilla', 556);
    FTimesBoldItalicWidths.Add('scommaaccent', 389);
    FTimesBoldItalicWidths.Add('Scommaaccent', 556);
    FTimesBoldItalicWidths.Add('section', 500);
    FTimesBoldItalicWidths.Add('semicolon', 333);
    FTimesBoldItalicWidths.Add('seven', 500);
    FTimesBoldItalicWidths.Add('six', 500);
    FTimesBoldItalicWidths.Add('slash', 278);
    FTimesBoldItalicWidths.Add('space', 250);
    FTimesBoldItalicWidths.Add('sterling', 500);
    FTimesBoldItalicWidths.Add('summation', 600);
    FTimesBoldItalicWidths.Add('t', 278);
    FTimesBoldItalicWidths.Add('T', 611);
    FTimesBoldItalicWidths.Add('tcaron', 366);
    FTimesBoldItalicWidths.Add('Tcaron', 611);
    FTimesBoldItalicWidths.Add('tcommaaccent', 278);
    FTimesBoldItalicWidths.Add('Tcommaaccent', 611);
    FTimesBoldItalicWidths.Add('thorn', 500);
    FTimesBoldItalicWidths.Add('Thorn', 611);
    FTimesBoldItalicWidths.Add('three', 500);
    FTimesBoldItalicWidths.Add('threequarters', 750);
    FTimesBoldItalicWidths.Add('threesuperior', 300);
    FTimesBoldItalicWidths.Add('tilde', 333);
    FTimesBoldItalicWidths.Add('trademark', 1000);
    FTimesBoldItalicWidths.Add('two', 500);
    FTimesBoldItalicWidths.Add('twosuperior', 300);
    FTimesBoldItalicWidths.Add('u', 556);
    FTimesBoldItalicWidths.Add('U', 722);
    FTimesBoldItalicWidths.Add('uacute', 556);
    FTimesBoldItalicWidths.Add('Uacute', 722);
    FTimesBoldItalicWidths.Add('ucircumflex', 556);
    FTimesBoldItalicWidths.Add('Ucircumflex', 722);
    FTimesBoldItalicWidths.Add('udieresis', 556);
    FTimesBoldItalicWidths.Add('Udieresis', 722);
    FTimesBoldItalicWidths.Add('ugrave', 556);
    FTimesBoldItalicWidths.Add('Ugrave', 722);
    FTimesBoldItalicWidths.Add('uhungarumlaut', 556);
    FTimesBoldItalicWidths.Add('Uhungarumlaut', 722);
    FTimesBoldItalicWidths.Add('umacron', 556);
    FTimesBoldItalicWidths.Add('Umacron', 722);
    FTimesBoldItalicWidths.Add('underscore', 500);
    FTimesBoldItalicWidths.Add('uogonek', 556);
    FTimesBoldItalicWidths.Add('Uogonek', 722);
    FTimesBoldItalicWidths.Add('uring', 556);
    FTimesBoldItalicWidths.Add('Uring', 722);
    FTimesBoldItalicWidths.Add('v', 444);
    FTimesBoldItalicWidths.Add('V', 667);
    FTimesBoldItalicWidths.Add('w', 667);
    FTimesBoldItalicWidths.Add('W', 889);
    FTimesBoldItalicWidths.Add('x', 500);
    FTimesBoldItalicWidths.Add('X', 667);
    FTimesBoldItalicWidths.Add('y', 444);
    FTimesBoldItalicWidths.Add('Y', 611);
    FTimesBoldItalicWidths.Add('yacute', 444);
    FTimesBoldItalicWidths.Add('Yacute', 611);
    FTimesBoldItalicWidths.Add('ydieresis', 444);
    FTimesBoldItalicWidths.Add('Ydieresis', 611);
    FTimesBoldItalicWidths.Add('yen', 500);
    FTimesBoldItalicWidths.Add('z', 389);
    FTimesBoldItalicWidths.Add('Z', 611);
    FTimesBoldItalicWidths.Add('zacute', 389);
    FTimesBoldItalicWidths.Add('Zacute', 611);
    FTimesBoldItalicWidths.Add('zcaron', 389);
    FTimesBoldItalicWidths.Add('Zcaron', 611);
    FTimesBoldItalicWidths.Add('zdotaccent', 389);
    FTimesBoldItalicWidths.Add('Zdotaccent', 611);
    FTimesBoldItalicWidths.Add('zero', 500);
    FTimesBoldItalicWidths.TrimExcess;
  end;
  Result := FTimesBoldItalicWidths;
end;

function TdxPDFSimpleFontDefaultWidths.GetTimesItalicWidths: TdxPDFWordDictionary;
begin
  if FTimesItalicWidths.Count = 0 then
  begin
    FTimesItalicWidths.Add('a', 500);
    FTimesItalicWidths.Add('A', 611);
    FTimesItalicWidths.Add('aacute', 500);
    FTimesItalicWidths.Add('Aacute', 611);
    FTimesItalicWidths.Add('abreve', 500);
    FTimesItalicWidths.Add('Abreve', 611);
    FTimesItalicWidths.Add('acircumflex', 500);
    FTimesItalicWidths.Add('Acircumflex', 611);
    FTimesItalicWidths.Add('acute', 333);
    FTimesItalicWidths.Add('adieresis', 500);
    FTimesItalicWidths.Add('Adieresis', 611);
    FTimesItalicWidths.Add('ae', 667);
    FTimesItalicWidths.Add('AE', 889);
    FTimesItalicWidths.Add('agrave', 500);
    FTimesItalicWidths.Add('Agrave', 611);
    FTimesItalicWidths.Add('amacron', 500);
    FTimesItalicWidths.Add('Amacron', 611);
    FTimesItalicWidths.Add('ampersand', 778);
    FTimesItalicWidths.Add('aogonek', 500);
    FTimesItalicWidths.Add('Aogonek', 611);
    FTimesItalicWidths.Add('aring', 500);
    FTimesItalicWidths.Add('Aring', 611);
    FTimesItalicWidths.Add('asciicircum', 422);
    FTimesItalicWidths.Add('asciitilde', 541);
    FTimesItalicWidths.Add('asterisk', 500);
    FTimesItalicWidths.Add('at', 920);
    FTimesItalicWidths.Add('atilde', 500);
    FTimesItalicWidths.Add('Atilde', 611);
    FTimesItalicWidths.Add('b', 500);
    FTimesItalicWidths.Add('B', 611);
    FTimesItalicWidths.Add('backslash', 278);
    FTimesItalicWidths.Add('bar', 275);
    FTimesItalicWidths.Add('braceleft', 400);
    FTimesItalicWidths.Add('braceright', 400);
    FTimesItalicWidths.Add('bracketleft', 389);
    FTimesItalicWidths.Add('bracketright', 389);
    FTimesItalicWidths.Add('breve', 333);
    FTimesItalicWidths.Add('brokenbar', 275);
    FTimesItalicWidths.Add('bullet', 350);
    FTimesItalicWidths.Add('c', 444);
    FTimesItalicWidths.Add('C', 667);
    FTimesItalicWidths.Add('cacute', 444);
    FTimesItalicWidths.Add('Cacute', 667);
    FTimesItalicWidths.Add('caron', 333);
    FTimesItalicWidths.Add('ccaron', 444);
    FTimesItalicWidths.Add('Ccaron', 667);
    FTimesItalicWidths.Add('ccedilla', 444);
    FTimesItalicWidths.Add('Ccedilla', 667);
    FTimesItalicWidths.Add('cedilla', 333);
    FTimesItalicWidths.Add('cent', 500);
    FTimesItalicWidths.Add('circumflex', 333);
    FTimesItalicWidths.Add('colon', 333);
    FTimesItalicWidths.Add('comma', 250);
    FTimesItalicWidths.Add('commaaccent', 250);
    FTimesItalicWidths.Add('copyright', 760);
    FTimesItalicWidths.Add('currency', 500);
    FTimesItalicWidths.Add('d', 500);
    FTimesItalicWidths.Add('D', 722);
    FTimesItalicWidths.Add('dagger', 500);
    FTimesItalicWidths.Add('daggerdbl', 500);
    FTimesItalicWidths.Add('dcaron', 544);
    FTimesItalicWidths.Add('Dcaron', 722);
    FTimesItalicWidths.Add('dcroat', 500);
    FTimesItalicWidths.Add('Dcroat', 722);
    FTimesItalicWidths.Add('degree', 400);
    FTimesItalicWidths.Add('Delta', 612);
    FTimesItalicWidths.Add('dieresis', 333);
    FTimesItalicWidths.Add('divide', 675);
    FTimesItalicWidths.Add('dollar', 500);
    FTimesItalicWidths.Add('dotaccent', 333);
    FTimesItalicWidths.Add('dotlessi', 278);
    FTimesItalicWidths.Add('e', 444);
    FTimesItalicWidths.Add('E', 611);
    FTimesItalicWidths.Add('eacute', 444);
    FTimesItalicWidths.Add('Eacute', 611);
    FTimesItalicWidths.Add('ecaron', 444);
    FTimesItalicWidths.Add('Ecaron', 611);
    FTimesItalicWidths.Add('ecircumflex', 444);
    FTimesItalicWidths.Add('Ecircumflex', 611);
    FTimesItalicWidths.Add('edieresis', 444);
    FTimesItalicWidths.Add('Edieresis', 611);
    FTimesItalicWidths.Add('edotaccent', 444);
    FTimesItalicWidths.Add('Edotaccent', 611);
    FTimesItalicWidths.Add('egrave', 444);
    FTimesItalicWidths.Add('Egrave', 611);
    FTimesItalicWidths.Add('eight', 500);
    FTimesItalicWidths.Add('ellipsis', 889);
    FTimesItalicWidths.Add('emacron', 444);
    FTimesItalicWidths.Add('Emacron', 611);
    FTimesItalicWidths.Add('emdash', 889);
    FTimesItalicWidths.Add('endash', 500);
    FTimesItalicWidths.Add('eogonek', 444);
    FTimesItalicWidths.Add('Eogonek', 611);
    FTimesItalicWidths.Add('equal', 675);
    FTimesItalicWidths.Add('eth', 500);
    FTimesItalicWidths.Add('Eth', 722);
    FTimesItalicWidths.Add('Euro', 500);
    FTimesItalicWidths.Add('exclam', 333);
    FTimesItalicWidths.Add('exclamdown', 389);
    FTimesItalicWidths.Add('f', 278);
    FTimesItalicWidths.Add('F', 611);
    FTimesItalicWidths.Add('fi', 500);
    FTimesItalicWidths.Add('five', 500);
    FTimesItalicWidths.Add('fl', 500);
    FTimesItalicWidths.Add('florin', 500);
    FTimesItalicWidths.Add('four', 500);
    FTimesItalicWidths.Add('fraction', 167);
    FTimesItalicWidths.Add('g', 500);
    FTimesItalicWidths.Add('G', 722);
    FTimesItalicWidths.Add('gbreve', 500);
    FTimesItalicWidths.Add('Gbreve', 722);
    FTimesItalicWidths.Add('gcommaaccent', 500);
    FTimesItalicWidths.Add('Gcommaaccent', 722);
    FTimesItalicWidths.Add('germandbls', 500);
    FTimesItalicWidths.Add('grave', 333);
    FTimesItalicWidths.Add('greater', 675);
    FTimesItalicWidths.Add('greaterequal', 549);
    FTimesItalicWidths.Add('guillemotleft', 500);
    FTimesItalicWidths.Add('guillemotright', 500);
    FTimesItalicWidths.Add('guilsinglleft', 333);
    FTimesItalicWidths.Add('guilsinglright', 333);
    FTimesItalicWidths.Add('h', 500);
    FTimesItalicWidths.Add('H', 722);
    FTimesItalicWidths.Add('hungarumlaut', 333);
    FTimesItalicWidths.Add('hyphen', 333);
    FTimesItalicWidths.Add('i', 278);
    FTimesItalicWidths.Add('I', 333);
    FTimesItalicWidths.Add('iacute', 278);
    FTimesItalicWidths.Add('Iacute', 333);
    FTimesItalicWidths.Add('icircumflex', 278);
    FTimesItalicWidths.Add('Icircumflex', 333);
    FTimesItalicWidths.Add('idieresis', 278);
    FTimesItalicWidths.Add('Idieresis', 333);
    FTimesItalicWidths.Add('Idotaccent', 333);
    FTimesItalicWidths.Add('igrave', 278);
    FTimesItalicWidths.Add('Igrave', 333);
    FTimesItalicWidths.Add('imacron', 278);
    FTimesItalicWidths.Add('Imacron', 333);
    FTimesItalicWidths.Add('iogonek', 278);
    FTimesItalicWidths.Add('Iogonek', 333);
    FTimesItalicWidths.Add('j', 278);
    FTimesItalicWidths.Add('J', 444);
    FTimesItalicWidths.Add('k', 444);
    FTimesItalicWidths.Add('K', 667);
    FTimesItalicWidths.Add('kcommaaccent', 444);
    FTimesItalicWidths.Add('Kcommaaccent', 667);
    FTimesItalicWidths.Add('l', 278);
    FTimesItalicWidths.Add('L', 556);
    FTimesItalicWidths.Add('lacute', 278);
    FTimesItalicWidths.Add('Lacute', 556);
    FTimesItalicWidths.Add('lcaron', 300);
    FTimesItalicWidths.Add('Lcaron', 611);
    FTimesItalicWidths.Add('lcommaaccent', 278);
    FTimesItalicWidths.Add('Lcommaaccent', 556);
    FTimesItalicWidths.Add('less', 675);
    FTimesItalicWidths.Add('lessequal', 549);
    FTimesItalicWidths.Add('logicalnot', 675);
    FTimesItalicWidths.Add('lozenge', 471);
    FTimesItalicWidths.Add('lslash', 278);
    FTimesItalicWidths.Add('Lslash', 556);
    FTimesItalicWidths.Add('m', 722);
    FTimesItalicWidths.Add('M', 833);
    FTimesItalicWidths.Add('macron', 333);
    FTimesItalicWidths.Add('minus', 675);
    FTimesItalicWidths.Add('mu', 500);
    FTimesItalicWidths.Add('multiply', 675);
    FTimesItalicWidths.Add('n', 500);
    FTimesItalicWidths.Add('N', 667);
    FTimesItalicWidths.Add('nacute', 500);
    FTimesItalicWidths.Add('Nacute', 667);
    FTimesItalicWidths.Add('ncaron', 500);
    FTimesItalicWidths.Add('Ncaron', 667);
    FTimesItalicWidths.Add('ncommaaccent', 500);
    FTimesItalicWidths.Add('Ncommaaccent', 667);
    FTimesItalicWidths.Add('nine', 500);
    FTimesItalicWidths.Add('notequal', 549);
    FTimesItalicWidths.Add('ntilde', 500);
    FTimesItalicWidths.Add('Ntilde', 667);
    FTimesItalicWidths.Add('numbersign', 500);
    FTimesItalicWidths.Add('o', 500);
    FTimesItalicWidths.Add('O', 722);
    FTimesItalicWidths.Add('oacute', 500);
    FTimesItalicWidths.Add('Oacute', 722);
    FTimesItalicWidths.Add('ocircumflex', 500);
    FTimesItalicWidths.Add('Ocircumflex', 722);
    FTimesItalicWidths.Add('odieresis', 500);
    FTimesItalicWidths.Add('Odieresis', 722);
    FTimesItalicWidths.Add('oe', 667);
    FTimesItalicWidths.Add('OE', 944);
    FTimesItalicWidths.Add('ogonek', 333);
    FTimesItalicWidths.Add('ograve', 500);
    FTimesItalicWidths.Add('Ograve', 722);
    FTimesItalicWidths.Add('ohungarumlaut', 500);
    FTimesItalicWidths.Add('Ohungarumlaut', 722);
    FTimesItalicWidths.Add('omacron', 500);
    FTimesItalicWidths.Add('Omacron', 722);
    FTimesItalicWidths.Add('one', 500);
    FTimesItalicWidths.Add('onehalf', 750);
    FTimesItalicWidths.Add('onequarter', 750);
    FTimesItalicWidths.Add('onesuperior', 300);
    FTimesItalicWidths.Add('ordfeminine', 276);
    FTimesItalicWidths.Add('ordmasculine', 310);
    FTimesItalicWidths.Add('oslash', 500);
    FTimesItalicWidths.Add('Oslash', 722);
    FTimesItalicWidths.Add('otilde', 500);
    FTimesItalicWidths.Add('Otilde', 722);
    FTimesItalicWidths.Add('p', 500);
    FTimesItalicWidths.Add('P', 611);
    FTimesItalicWidths.Add('paragraph', 523);
    FTimesItalicWidths.Add('parenleft', 333);
    FTimesItalicWidths.Add('parenright', 333);
    FTimesItalicWidths.Add('partialdiff', 476);
    FTimesItalicWidths.Add('percent', 833);
    FTimesItalicWidths.Add('period', 250);
    FTimesItalicWidths.Add('periodcentered', 250);
    FTimesItalicWidths.Add('perthousand', 1000);
    FTimesItalicWidths.Add('plus', 675);
    FTimesItalicWidths.Add('plusminus', 675);
    FTimesItalicWidths.Add('q', 500);
    FTimesItalicWidths.Add('Q', 722);
    FTimesItalicWidths.Add('question', 500);
    FTimesItalicWidths.Add('questiondown', 500);
    FTimesItalicWidths.Add('quotedbl', 420);
    FTimesItalicWidths.Add('quotedblbase', 556);
    FTimesItalicWidths.Add('quotedblleft', 556);
    FTimesItalicWidths.Add('quotedblright', 556);
    FTimesItalicWidths.Add('quoteleft', 333);
    FTimesItalicWidths.Add('quoteright', 333);
    FTimesItalicWidths.Add('quotesinglbase', 333);
    FTimesItalicWidths.Add('quotesingle', 214);
    FTimesItalicWidths.Add('r', 389);
    FTimesItalicWidths.Add('R', 611);
    FTimesItalicWidths.Add('racute', 389);
    FTimesItalicWidths.Add('Racute', 611);
    FTimesItalicWidths.Add('radical', 453);
    FTimesItalicWidths.Add('rcaron', 389);
    FTimesItalicWidths.Add('Rcaron', 611);
    FTimesItalicWidths.Add('rcommaaccent', 389);
    FTimesItalicWidths.Add('Rcommaaccent', 611);
    FTimesItalicWidths.Add('registered', 760);
    FTimesItalicWidths.Add('ring', 333);
    FTimesItalicWidths.Add('s', 389);
    FTimesItalicWidths.Add('S', 500);
    FTimesItalicWidths.Add('sacute', 389);
    FTimesItalicWidths.Add('Sacute', 500);
    FTimesItalicWidths.Add('scaron', 389);
    FTimesItalicWidths.Add('Scaron', 500);
    FTimesItalicWidths.Add('scedilla', 389);
    FTimesItalicWidths.Add('Scedilla', 500);
    FTimesItalicWidths.Add('scommaaccent', 389);
    FTimesItalicWidths.Add('Scommaaccent', 500);
    FTimesItalicWidths.Add('section', 500);
    FTimesItalicWidths.Add('semicolon', 333);
    FTimesItalicWidths.Add('seven', 500);
    FTimesItalicWidths.Add('six', 500);
    FTimesItalicWidths.Add('slash', 278);
    FTimesItalicWidths.Add('space', 250);
    FTimesItalicWidths.Add('sterling', 500);
    FTimesItalicWidths.Add('summation', 600);
    FTimesItalicWidths.Add('t', 278);
    FTimesItalicWidths.Add('T', 556);
    FTimesItalicWidths.Add('tcaron', 300);
    FTimesItalicWidths.Add('Tcaron', 556);
    FTimesItalicWidths.Add('tcommaaccent', 278);
    FTimesItalicWidths.Add('Tcommaaccent', 556);
    FTimesItalicWidths.Add('thorn', 500);
    FTimesItalicWidths.Add('Thorn', 611);
    FTimesItalicWidths.Add('three', 500);
    FTimesItalicWidths.Add('threequarters', 750);
    FTimesItalicWidths.Add('threesuperior', 300);
    FTimesItalicWidths.Add('tilde', 333);
    FTimesItalicWidths.Add('trademark', 980);
    FTimesItalicWidths.Add('two', 500);
    FTimesItalicWidths.Add('twosuperior', 300);
    FTimesItalicWidths.Add('u', 500);
    FTimesItalicWidths.Add('U', 722);
    FTimesItalicWidths.Add('uacute', 500);
    FTimesItalicWidths.Add('Uacute', 722);
    FTimesItalicWidths.Add('ucircumflex', 500);
    FTimesItalicWidths.Add('Ucircumflex', 722);
    FTimesItalicWidths.Add('udieresis', 500);
    FTimesItalicWidths.Add('Udieresis', 722);
    FTimesItalicWidths.Add('ugrave', 500);
    FTimesItalicWidths.Add('Ugrave', 722);
    FTimesItalicWidths.Add('uhungarumlaut', 500);
    FTimesItalicWidths.Add('Uhungarumlaut', 722);
    FTimesItalicWidths.Add('umacron', 500);
    FTimesItalicWidths.Add('Umacron', 722);
    FTimesItalicWidths.Add('underscore', 500);
    FTimesItalicWidths.Add('uogonek', 500);
    FTimesItalicWidths.Add('Uogonek', 722);
    FTimesItalicWidths.Add('uring', 500);
    FTimesItalicWidths.Add('Uring', 722);
    FTimesItalicWidths.Add('v', 444);
    FTimesItalicWidths.Add('V', 611);
    FTimesItalicWidths.Add('w', 667);
    FTimesItalicWidths.Add('W', 833);
    FTimesItalicWidths.Add('x', 444);
    FTimesItalicWidths.Add('X', 611);
    FTimesItalicWidths.Add('y', 444);
    FTimesItalicWidths.Add('Y', 556);
    FTimesItalicWidths.Add('yacute', 444);
    FTimesItalicWidths.Add('Yacute', 556);
    FTimesItalicWidths.Add('ydieresis', 444);
    FTimesItalicWidths.Add('Ydieresis', 556);
    FTimesItalicWidths.Add('yen', 500);
    FTimesItalicWidths.Add('z', 389);
    FTimesItalicWidths.Add('Z', 556);
    FTimesItalicWidths.Add('zacute', 389);
    FTimesItalicWidths.Add('Zacute', 556);
    FTimesItalicWidths.Add('zcaron', 389);
    FTimesItalicWidths.Add('Zcaron', 556);
    FTimesItalicWidths.Add('zdotaccent', 389);
    FTimesItalicWidths.Add('Zdotaccent', 556);
    FTimesItalicWidths.Add('zero', 500);
    FTimesItalicWidths.TrimExcess;
  end;
  Result := FTimesItalicWidths;
end;

function TdxPDFSimpleFontDefaultWidths.GetStandardFontDefaultWidthsMap: TDictionary<string, TGetDefaultWidthsFunc>;
begin
  if FStandardFontDefaultWidthsMap.Count = 0 then
  begin
    FStandardFontDefaultWidthsMap.Add('Arial', GetHelveticaWidths);
    FStandardFontDefaultWidthsMap.Add('Arial,Bold', GetHelveticaBoldWidths);
    FStandardFontDefaultWidthsMap.Add('Arial,BoldItalic', GetHelveticaBoldObliqueWidths);
    FStandardFontDefaultWidthsMap.Add('Arial,Italic', GetHelveticaObliqueWidths);
    FStandardFontDefaultWidthsMap.Add('Arial-Bold', GetHelveticaBoldWidths);
    FStandardFontDefaultWidthsMap.Add('Arial-BoldItalic', GetHelveticaBoldObliqueWidths);
    FStandardFontDefaultWidthsMap.Add('Arial-BoldItalicMT', GetHelveticaBoldObliqueWidths);
    FStandardFontDefaultWidthsMap.Add('Arial-BoldMT', GetHelveticaBoldWidths);
    FStandardFontDefaultWidthsMap.Add('Arial-Italic', GetHelveticaObliqueWidths);
    FStandardFontDefaultWidthsMap.Add('Arial-ItalicMT', GetHelveticaObliqueWidths);
    FStandardFontDefaultWidthsMap.Add('Helvetica-Compressed', GetHelveticaWidths);
    FStandardFontDefaultWidthsMap.Add('ArialMT', GetHelveticaWidths);
    FStandardFontDefaultWidthsMap.Add('CourierNew', GetCourierWidths);
    FStandardFontDefaultWidthsMap.Add('CourierNew,Bold', GetCourierBoldWidths);
    FStandardFontDefaultWidthsMap.Add('CourierNew,BoldItalic', GetCourierBoldObliqueWidths);
    FStandardFontDefaultWidthsMap.Add('CourierNew,Italic', GetCourierObliqueWidths);
    FStandardFontDefaultWidthsMap.Add('CourierNew-Bold', GetCourierBoldWidths);
    FStandardFontDefaultWidthsMap.Add('CourierNew-BoldItalic', GetCourierBoldObliqueWidths);
    FStandardFontDefaultWidthsMap.Add('CourierNew-Italic', GetCourierObliqueWidths);
    FStandardFontDefaultWidthsMap.Add('CourierNewPS-BoldItalicMT', GetCourierBoldObliqueWidths);
    FStandardFontDefaultWidthsMap.Add('CourierNewPS-BoldMT', GetCourierBoldWidths);
    FStandardFontDefaultWidthsMap.Add('CourierNewPS-ItalicMT', GetCourierObliqueWidths);
    FStandardFontDefaultWidthsMap.Add('CourierNewPSMT', GetCourierWidths);
    FStandardFontDefaultWidthsMap.Add('Helvetica', GetHelveticaWidths);
    FStandardFontDefaultWidthsMap.Add('Helvetica,Bold', GetHelveticaBoldWidths);
    FStandardFontDefaultWidthsMap.Add('Helvetica,BoldItalic', GetHelveticaBoldObliqueWidths);
    FStandardFontDefaultWidthsMap.Add('Helvetica,Italic', GetHelveticaObliqueWidths);
    FStandardFontDefaultWidthsMap.Add('Helvetica-BoldItalic', GetHelveticaBoldObliqueWidths);
    FStandardFontDefaultWidthsMap.Add('Helvetica-Italic', GetHelveticaObliqueWidths);
    FStandardFontDefaultWidthsMap.Add('Symbol,Bold', GetSymbolWidths);
    FStandardFontDefaultWidthsMap.Add('Symbol,BoldItalic', GetSymbolWidths);
    FStandardFontDefaultWidthsMap.Add('Symbol,Italic', GetSymbolWidths);
    FStandardFontDefaultWidthsMap.Add('TimesNewRoman', GetTimesRomanWidths);
    FStandardFontDefaultWidthsMap.Add('TimesNewRoman,Bold', GetTimesBoldWidths);
    FStandardFontDefaultWidthsMap.Add('TimesNewRoman,BoldItalic', GetTimesBoldItalicWidths);
    FStandardFontDefaultWidthsMap.Add('TimesNewRoman,Italic', GetTimesItalicWidths);
    FStandardFontDefaultWidthsMap.Add('TimesNewRoman-Bold', GetTimesBoldWidths);
    FStandardFontDefaultWidthsMap.Add('TimesNewRoman-BoldItalic', GetTimesBoldItalicWidths);
    FStandardFontDefaultWidthsMap.Add('TimesNewRoman-Italic', GetTimesItalicWidths);
    FStandardFontDefaultWidthsMap.Add('TimesNewRomanPS', GetTimesRomanWidths);
    FStandardFontDefaultWidthsMap.Add('TimesNewRomanPS-Bold', GetTimesBoldWidths);
    FStandardFontDefaultWidthsMap.Add('TimesNewRomanPS-BoldItalic', GetTimesBoldItalicWidths);
    FStandardFontDefaultWidthsMap.Add('TimesNewRomanPS-BoldItalicMT', GetTimesBoldItalicWidths);
    FStandardFontDefaultWidthsMap.Add('TimesNewRomanPS-BoldMT', GetTimesBoldWidths);
    FStandardFontDefaultWidthsMap.Add('TimesNewRomanPS-Italic', GetTimesItalicWidths);
    FStandardFontDefaultWidthsMap.Add('TimesNewRomanPS-ItalicMT', GetTimesItalicWidths);
    FStandardFontDefaultWidthsMap.Add('TimesNewRomanPSMT', GetTimesRomanWidths);
    FStandardFontDefaultWidthsMap.Add('TimesNewRomanPSMT,Bold', GetTimesBoldWidths);
    FStandardFontDefaultWidthsMap.Add('TimesNewRomanPSMT,BoldItalic', GetTimesBoldItalicWidths);
    FStandardFontDefaultWidthsMap.Add('TimesNewRomanPSMT,Italic', GetTimesItalicWidths);
    FStandardFontDefaultWidthsMap.Add(TdxPDFKeywords.CourierObliqueFontName, GetCourierObliqueWidths);
    FStandardFontDefaultWidthsMap.Add(TdxPDFKeywords.CourierBoldObliqueFontName, GetCourierBoldObliqueWidths);
    FStandardFontDefaultWidthsMap.Add(TdxPDFKeywords.HelveticaBoldFontName, GetHelveticaBoldWidths);
    FStandardFontDefaultWidthsMap.Add(TdxPDFKeywords.HelveticaBoldObliqueFontName, GetHelveticaBoldWidths);
    FStandardFontDefaultWidthsMap.Add(TdxPDFKeywords.HelveticaObliqueFontName, GetHelveticaWidths);
    FStandardFontDefaultWidthsMap.Add(TdxPDFKeywords.SymbolFontName, GetSymbolWidths);
    FStandardFontDefaultWidthsMap.Add(TdxPDFKeywords.TimesRomanFontName, GetTimesRomanWidths);
    FStandardFontDefaultWidthsMap.Add(TdxPDFKeywords.TimesBoldFontName, GetTimesBoldWidths);
    FStandardFontDefaultWidthsMap.Add(TdxPDFKeywords.TimesBoldItalicFontName, GetTimesBoldItalicWidths);
    FStandardFontDefaultWidthsMap.Add(TdxPDFKeywords.TimesItalicFontName, GetTimesItalicWidths);
    FStandardFontDefaultWidthsMap.Add(TdxPDFKeywords.ZapfDingbatsFontName, GetZapfDingbatsWidths);
  end;
  Result := FStandardFontDefaultWidthsMap;
end;

function TdxPDFSimpleFontDefaultWidths.GetTimesRomanWidths: TdxPDFWordDictionary;
begin
  if FTimesRomanWidths.Count = 0 then
  begin
    FTimesRomanWidths.Add(TdxGlyphNames._notdef, 250);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerSpace, 250);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerExclam, 333);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerQuotedbl, 408);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerNumbersign, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerDollar, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerPercent, 833);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerAmpersand, 778);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerQuoteright, 333);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerParenleft, 333);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerParenright, 333);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerAsterisk, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerPlus, 564);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerComma, 250);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerHyphen, 333);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerPeriod, 250);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerSlash, 278);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerZero, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerOne, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerTwo, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerThree, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerFour, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerFive, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerSix, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerSeven, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerEight, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerNine, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerColon, 278);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerSemicolon, 278);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerLess, 564);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerEqual, 564);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerGreater, 564);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerQuestion, 444);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerAt, 921);
    FTimesRomanWidths.Add(TdxGlyphNames.A, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.B, 667);
    FTimesRomanWidths.Add(TdxGlyphNames.C, 667);
    FTimesRomanWidths.Add(TdxGlyphNames.D, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.E, 611);
    FTimesRomanWidths.Add(TdxGlyphNames.F, 556);
    FTimesRomanWidths.Add(TdxGlyphNames.G, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.H, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.I, 333);
    FTimesRomanWidths.Add(TdxGlyphNames.J, 389);
    FTimesRomanWidths.Add(TdxGlyphNames.K, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.L, 611);
    FTimesRomanWidths.Add(TdxGlyphNames.M, 889);
    FTimesRomanWidths.Add(TdxGlyphNames.N, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.O, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.P, 556);
    FTimesRomanWidths.Add(TdxGlyphNames.Q, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.R, 667);
    FTimesRomanWidths.Add(TdxGlyphNames.S, 556);
    FTimesRomanWidths.Add(TdxGlyphNames.T, 611);
    FTimesRomanWidths.Add(TdxGlyphNames.U, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.V, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.W, 944);
    FTimesRomanWidths.Add(TdxGlyphNames.X, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.Y, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.Z, 611);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerBracketleft, 333);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerBackslash, 278);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerBracketright, 333);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerAsciicircum, 469);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerUnderscore, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerQuoteleft, 333);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerA, 444);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerB, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerC, 444);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerD, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerE, 444);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerF, 333);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerG, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerH, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerI, 278);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerJ, 278);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerK, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerL, 278);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerM, 778);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerN, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerO, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerP, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerQ, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerR, 333);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerS, 389);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerT, 278);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerU, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerV, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerW, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerX, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerY, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerZ, 444);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerBraceleft, 480);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerBar, 200);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerBraceright, 480);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerAsciitilde, 541);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerExclamdown, 333);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerCent, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerSterling, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerFraction, 167);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerYen, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerFlorin, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerSection, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerCurrency, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerQuotesingle, 180);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerQuotedblleft, 444);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerGuillemotleft, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerGuilsinglleft, 333);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerGuilsinglright, 333);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerFi, 556);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerFl, 556);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerEndash, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerDagger, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerDaggerdbl, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerPeriodcentered, 250);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerParagraph, 453);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerBullet, 350);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerQuotesinglbase, 333);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerQuotedblbase, 444);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerQuotedblright, 444);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerGuillemotright, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerEllipsis, 1000);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerPerthousand, 1000);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerQuestiondown, 444);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerGrave, 333);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerAcute, 333);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerCircumflex, 333);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerTilde, 333);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerMacron, 333);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerBreve, 333);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerDotaccent, 333);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerDieresis, 333);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerRing, 333);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerCedilla, 333);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerHungarumlaut, 333);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerOgonek, 333);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerCaron, 333);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerEmdash, 1000);
    FTimesRomanWidths.Add(TdxGlyphNames.AE, 889);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerOrdfeminine, 276);
    FTimesRomanWidths.Add(TdxGlyphNames.Lslash, 611);
    FTimesRomanWidths.Add(TdxGlyphNames.Oslash, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.OE, 889);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerOrdmasculine, 310);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerAe, 667);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerDotlessi, 278);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerLslash, 278);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerOslash, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerOe, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerGermandbls, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.Idieresis, 333);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerEacute, 444);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerAbreve, 444);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerUhungarumlaut, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerEcaron, 444);
    FTimesRomanWidths.Add(TdxGlyphNames.Ydieresis, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerDivide, 564);
    FTimesRomanWidths.Add(TdxGlyphNames.Yacute, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.Acircumflex, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerAacute, 444);
    FTimesRomanWidths.Add(TdxGlyphNames.Ucircumflex, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerYacute, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerScommaaccent, 389);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerEcircumflex, 444);
    FTimesRomanWidths.Add(TdxGlyphNames.Uring, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.Udieresis, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerAogonek, 444);
    FTimesRomanWidths.Add(TdxGlyphNames.Uacute, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerUogonek, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.Edieresis, 611);
    FTimesRomanWidths.Add(TdxGlyphNames.Dcroat, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerCommaaccent, 250);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerCopyright, 760);
    FTimesRomanWidths.Add(TdxGlyphNames.Emacron, 611);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerCcaron, 444);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerAring, 444);
    FTimesRomanWidths.Add(TdxGlyphNames.Ncommaaccent, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerLacute, 278);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerAgrave, 444);
    FTimesRomanWidths.Add(TdxGlyphNames.Tcommaaccent, 611);
    FTimesRomanWidths.Add(TdxGlyphNames.Cacute, 667);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerAtilde, 444);
    FTimesRomanWidths.Add(TdxGlyphNames.Edotaccent, 611);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerScaron, 389);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerScedilla, 389);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerIacute, 278);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerLozenge, 471);
    FTimesRomanWidths.Add(TdxGlyphNames.Rcaron, 667);
    FTimesRomanWidths.Add(TdxGlyphNames.Gcommaaccent, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerUcircumflex, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerAcircumflex, 444);
    FTimesRomanWidths.Add(TdxGlyphNames.Amacron, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerRcaron, 333);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerCcedilla, 444);
    FTimesRomanWidths.Add(TdxGlyphNames.Zdotaccent, 611);
    FTimesRomanWidths.Add(TdxGlyphNames.Thorn, 556);
    FTimesRomanWidths.Add(TdxGlyphNames.Omacron, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.Racute, 667);
    FTimesRomanWidths.Add(TdxGlyphNames.Sacute, 556);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerDcaron, 588);
    FTimesRomanWidths.Add(TdxGlyphNames.Umacron, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerUring, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerThreesuperior, 300);
    FTimesRomanWidths.Add(TdxGlyphNames.Ograve, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.Agrave, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.Abreve, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerMultiply, 564);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerUacute, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.Tcaron, 611);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerPartialdiff, 476);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerYdieresis, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.Nacute, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerIcircumflex, 278);
    FTimesRomanWidths.Add(TdxGlyphNames.Ecircumflex, 611);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerAdieresis, 444);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerEdieresis, 444);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerCacute, 444);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerNacute, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerUmacron, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.Ncaron, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.Iacute, 333);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerPlusminus, 564);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerBrokenbar, 200);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerRegistered, 760);
    FTimesRomanWidths.Add(TdxGlyphNames.Gbreve, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.Idotaccent, 333);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerSummation, 600);
    FTimesRomanWidths.Add(TdxGlyphNames.Egrave, 611);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerRacute, 333);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerOmacron, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.Zacute, 611);
    FTimesRomanWidths.Add(TdxGlyphNames.Zcaron, 611);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerGreaterequal, 549);
    FTimesRomanWidths.Add(TdxGlyphNames.Eth, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.Ccedilla, 667);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerLcommaaccent, 278);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerTcaron, 326);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerEogonek, 444);
    FTimesRomanWidths.Add(TdxGlyphNames.Uogonek, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.Aacute, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.Adieresis, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerEgrave, 444);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerZacute, 444);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerIogonek, 278);
    FTimesRomanWidths.Add(TdxGlyphNames.Oacute, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerOacute, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerAmacron, 444);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerSacute, 389);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerIdieresis, 278);
    FTimesRomanWidths.Add(TdxGlyphNames.Ocircumflex, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.Ugrave, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.Delta, 612);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerThorn, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerTwosuperior, 300);
    FTimesRomanWidths.Add(TdxGlyphNames.Odieresis, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerMu, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerIgrave, 278);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerOhungarumlaut, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.Eogonek, 611);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerDcroat, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerThreequarters, 750);
    FTimesRomanWidths.Add(TdxGlyphNames.Scedilla, 556);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerLcaron, 344);
    FTimesRomanWidths.Add(TdxGlyphNames.Kcommaaccent, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.Lacute, 611);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerTrademark, 980);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerEdotaccent, 444);
    FTimesRomanWidths.Add(TdxGlyphNames.Igrave, 333);
    FTimesRomanWidths.Add(TdxGlyphNames.Imacron, 333);
    FTimesRomanWidths.Add(TdxGlyphNames.Lcaron, 611);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerOnehalf, 750);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerLessequal, 549);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerOcircumflex, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerNtilde, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.Uhungarumlaut, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.Eacute, 611);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerEmacron, 444);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerGbreve, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerOnequarter, 750);
    FTimesRomanWidths.Add(TdxGlyphNames.Scaron, 556);
    FTimesRomanWidths.Add(TdxGlyphNames.Scommaaccent, 556);
    FTimesRomanWidths.Add(TdxGlyphNames.Ohungarumlaut, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerDegree, 400);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerOgrave, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.Ccaron, 667);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerUgrave, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerRadical, 453);
    FTimesRomanWidths.Add(TdxGlyphNames.Dcaron, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerRcommaaccent, 333);
    FTimesRomanWidths.Add(TdxGlyphNames.Ntilde, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerOtilde, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.Rcommaaccent, 667);
    FTimesRomanWidths.Add(TdxGlyphNames.Lcommaaccent, 611);
    FTimesRomanWidths.Add(TdxGlyphNames.Atilde, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.Aogonek, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.Aring, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.Otilde, 722);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerZdotaccent, 444);
    FTimesRomanWidths.Add(TdxGlyphNames.Ecaron, 611);
    FTimesRomanWidths.Add(TdxGlyphNames.Iogonek, 333);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerKcommaaccent, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerMinus, 564);
    FTimesRomanWidths.Add(TdxGlyphNames.Icircumflex, 333);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerNcaron, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerTcommaaccent, 278);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerLogicalnot, 564);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerOdieresis, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerUdieresis, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerNotequal, 549);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerGcommaaccent, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerEth, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerZcaron, 444);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerNcommaaccent, 500);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerOnesuperior, 300);
    FTimesRomanWidths.Add(TdxGlyphNames.LowerImacron, 278);
    FTimesRomanWidths.Add(TdxGlyphNames.Euro, 500);
    FHelveticaBoldWidths.TrimExcess;
  end;
  Result := FTimesRomanWidths;
end;

function TdxPDFSimpleFontDefaultWidths.GetSymbolWidths: TdxPDFWordDictionary;
begin
  if FSymbolWidths.Count = 0 then
  begin
    FSymbolWidths.Add(TdxGlyphNames.LowerSpace, 250);
    FSymbolWidths.Add(TdxGlyphNames.LowerExclam, 333);
    FSymbolWidths.Add(TdxGlyphNames.LowerUniversal, 713);
    FSymbolWidths.Add(TdxGlyphNames.LowerNumbersign, 500);
    FSymbolWidths.Add(TdxGlyphNames.LowerExistential, 549);
    FSymbolWidths.Add(TdxGlyphNames.LowerPercent, 833);
    FSymbolWidths.Add(TdxGlyphNames.LowerAmpersand, 778);
    FSymbolWidths.Add(TdxGlyphNames.LowerSuchthat, 439);
    FSymbolWidths.Add(TdxGlyphNames.LowerParenleft, 333);
    FSymbolWidths.Add(TdxGlyphNames.LowerParenright, 333);
    FSymbolWidths.Add(TdxGlyphNames.LowerAsteriskmath, 500);
    FSymbolWidths.Add(TdxGlyphNames.LowerPlus, 549);
    FSymbolWidths.Add(TdxGlyphNames.LowerComma, 250);
    FSymbolWidths.Add(TdxGlyphNames.LowerMinus, 549);
    FSymbolWidths.Add(TdxGlyphNames.LowerPeriod, 250);
    FSymbolWidths.Add(TdxGlyphNames.LowerSlash, 278);
    FSymbolWidths.Add(TdxGlyphNames.LowerZero, 500);
    FSymbolWidths.Add(TdxGlyphNames.LowerOne, 500);
    FSymbolWidths.Add(TdxGlyphNames.LowerTwo, 500);
    FSymbolWidths.Add(TdxGlyphNames.LowerThree, 500);
    FSymbolWidths.Add(TdxGlyphNames.LowerFour, 500);
    FSymbolWidths.Add(TdxGlyphNames.LowerFive, 500);
    FSymbolWidths.Add(TdxGlyphNames.LowerSix, 500);
    FSymbolWidths.Add(TdxGlyphNames.LowerSeven, 500);
    FSymbolWidths.Add(TdxGlyphNames.LowerEight, 500);
    FSymbolWidths.Add(TdxGlyphNames.LowerNine, 500);
    FSymbolWidths.Add(TdxGlyphNames.LowerColon, 278);
    FSymbolWidths.Add(TdxGlyphNames.LowerSemicolon, 278);
    FSymbolWidths.Add(TdxGlyphNames.LowerLess, 549);
    FSymbolWidths.Add(TdxGlyphNames.LowerEqual, 549);
    FSymbolWidths.Add(TdxGlyphNames.LowerGreater, 549);
    FSymbolWidths.Add(TdxGlyphNames.LowerQuestion, 444);
    FSymbolWidths.Add(TdxGlyphNames.LowerCongruent, 549);
    FSymbolWidths.Add(TdxGlyphNames.Alpha, 722);
    FSymbolWidths.Add(TdxGlyphNames.Beta, 667);
    FSymbolWidths.Add(TdxGlyphNames.Chi, 722);
    FSymbolWidths.Add(TdxGlyphNames.Delta, 612);
    FSymbolWidths.Add(TdxGlyphNames.Epsilon, 611);
    FSymbolWidths.Add(TdxGlyphNames.Phi, 763);
    FSymbolWidths.Add(TdxGlyphNames.Gamma, 603);
    FSymbolWidths.Add(TdxGlyphNames.Eta, 722);
    FSymbolWidths.Add(TdxGlyphNames.Iota, 333);
    FSymbolWidths.Add(TdxGlyphNames.LowerTheta1, 631);
    FSymbolWidths.Add(TdxGlyphNames.Kappa, 722);
    FSymbolWidths.Add(TdxGlyphNames.Lambda, 686);
    FSymbolWidths.Add(TdxGlyphNames.Mu, 889);
    FSymbolWidths.Add(TdxGlyphNames.Nu, 722);
    FSymbolWidths.Add(TdxGlyphNames.Omicron, 722);
    FSymbolWidths.Add(TdxGlyphNames.Pi, 768);
    FSymbolWidths.Add(TdxGlyphNames.Theta, 741);
    FSymbolWidths.Add(TdxGlyphNames.Rho, 556);
    FSymbolWidths.Add(TdxGlyphNames.Sigma, 592);
    FSymbolWidths.Add(TdxGlyphNames.Tau, 611);
    FSymbolWidths.Add(TdxGlyphNames.Upsilon, 690);
    FSymbolWidths.Add(TdxGlyphNames.LowerSigma1, 439);
    FSymbolWidths.Add(TdxGlyphNames.Omega, 768);
    FSymbolWidths.Add(TdxGlyphNames.Xi, 645);
    FSymbolWidths.Add(TdxGlyphNames.Psi, 795);
    FSymbolWidths.Add(TdxGlyphNames.Zeta, 611);
    FSymbolWidths.Add(TdxGlyphNames.LowerBracketleft, 333);
    FSymbolWidths.Add(TdxGlyphNames.LowerTherefore, 863);
    FSymbolWidths.Add(TdxGlyphNames.LowerBracketright, 333);
    FSymbolWidths.Add(TdxGlyphNames.LowerPerpendicular, 658);
    FSymbolWidths.Add(TdxGlyphNames.LowerUnderscore, 500);
    FSymbolWidths.Add(TdxGlyphNames.LowerRadicalex, 500);
    FSymbolWidths.Add(TdxGlyphNames.LowerAlpha, 631);
    FSymbolWidths.Add(TdxGlyphNames.LowerBeta, 549);
    FSymbolWidths.Add(TdxGlyphNames.LowerChi, 549);
    FSymbolWidths.Add(TdxGlyphNames.LowerDelta, 494);
    FSymbolWidths.Add(TdxGlyphNames.LowerEpsilon, 439);
    FSymbolWidths.Add(TdxGlyphNames.LowerPhi, 521);
    FSymbolWidths.Add(TdxGlyphNames.LowerGamma, 411);
    FSymbolWidths.Add(TdxGlyphNames.LowerEta, 603);
    FSymbolWidths.Add(TdxGlyphNames.LowerIota, 329);
    FSymbolWidths.Add(TdxGlyphNames.LowerPhi1, 603);
    FSymbolWidths.Add(TdxGlyphNames.LowerKappa, 549);
    FSymbolWidths.Add(TdxGlyphNames.LowerLambda, 549);
    FSymbolWidths.Add(TdxGlyphNames.LowerMu, 576);
    FSymbolWidths.Add(TdxGlyphNames.LowerNu, 521);
    FSymbolWidths.Add(TdxGlyphNames.LowerOmicron, 549);
    FSymbolWidths.Add(TdxGlyphNames.LowerPi, 549);
    FSymbolWidths.Add(TdxGlyphNames.LowerTheta, 521);
    FSymbolWidths.Add(TdxGlyphNames.LowerRho, 549);
    FSymbolWidths.Add(TdxGlyphNames.LowerSigma, 603);
    FSymbolWidths.Add(TdxGlyphNames.LowerTau, 439);
    FSymbolWidths.Add(TdxGlyphNames.LowerUpsilon, 576);
    FSymbolWidths.Add(TdxGlyphNames.LowerOmega1, 713);
    FSymbolWidths.Add(TdxGlyphNames.LowerOmega, 686);
    FSymbolWidths.Add(TdxGlyphNames.LowerXi, 493);
    FSymbolWidths.Add(TdxGlyphNames.LowerPsi, 686);
    FSymbolWidths.Add(TdxGlyphNames.LowerZeta, 494);
    FSymbolWidths.Add(TdxGlyphNames.LowerBraceleft, 480);
    FSymbolWidths.Add(TdxGlyphNames.LowerBar, 200);
    FSymbolWidths.Add(TdxGlyphNames.LowerBraceright, 480);
    FSymbolWidths.Add(TdxGlyphNames.LowerSimilar, 549);
    FSymbolWidths.Add(TdxGlyphNames.Euro, 750);
    FSymbolWidths.Add(TdxGlyphNames.Upsilon1, 620);
    FSymbolWidths.Add(TdxGlyphNames.LowerMinute, 247);
    FSymbolWidths.Add(TdxGlyphNames.LowerLessequal, 549);
    FSymbolWidths.Add(TdxGlyphNames.LowerFraction, 167);
    FSymbolWidths.Add(TdxGlyphNames.LowerInfinity, 713);
    FSymbolWidths.Add(TdxGlyphNames.LowerFlorin, 500);
    FSymbolWidths.Add(TdxGlyphNames.LowerClub, 753);
    FSymbolWidths.Add(TdxGlyphNames.LowerDiamond, 753);
    FSymbolWidths.Add(TdxGlyphNames.LowerHeart, 753);
    FSymbolWidths.Add(TdxGlyphNames.LowerSpade, 753);
    FSymbolWidths.Add(TdxGlyphNames.LowerArrowboth, 1042);
    FSymbolWidths.Add(TdxGlyphNames.LowerArrowleft, 987);
    FSymbolWidths.Add(TdxGlyphNames.LowerArrowup, 603);
    FSymbolWidths.Add(TdxGlyphNames.LowerArrowright, 987);
    FSymbolWidths.Add(TdxGlyphNames.LowerArrowdown, 603);
    FSymbolWidths.Add(TdxGlyphNames.LowerDegree, 400);
    FSymbolWidths.Add(TdxGlyphNames.LowerPlusminus, 549);
    FSymbolWidths.Add(TdxGlyphNames.LowerSecond, 411);
    FSymbolWidths.Add(TdxGlyphNames.LowerGreaterequal, 549);
    FSymbolWidths.Add(TdxGlyphNames.LowerMultiply, 549);
    FSymbolWidths.Add(TdxGlyphNames.LowerProportional, 713);
    FSymbolWidths.Add(TdxGlyphNames.LowerPartialdiff, 494);
    FSymbolWidths.Add(TdxGlyphNames.LowerBullet, 460);
    FSymbolWidths.Add(TdxGlyphNames.LowerDivide, 549);
    FSymbolWidths.Add(TdxGlyphNames.LowerNotequal, 549);
    FSymbolWidths.Add(TdxGlyphNames.LowerEquivalence, 549);
    FSymbolWidths.Add(TdxGlyphNames.LowerApproxequal, 549);
    FSymbolWidths.Add(TdxGlyphNames.LowerEllipsis, 1000);
    FSymbolWidths.Add(TdxGlyphNames.LowerArrowvertex, 603);
    FSymbolWidths.Add(TdxGlyphNames.LowerArrowhorizex, 1000);
    FSymbolWidths.Add(TdxGlyphNames.LowerCarriagereturn, 658);
    FSymbolWidths.Add(TdxGlyphNames.LowerAleph, 823);
    FSymbolWidths.Add(TdxGlyphNames.Ifraktur, 686);
    FSymbolWidths.Add(TdxGlyphNames.Rfraktur, 795);
    FSymbolWidths.Add(TdxGlyphNames.LowerWeierstrass, 987);
    FSymbolWidths.Add(TdxGlyphNames.LowerCirclemultiply, 768);
    FSymbolWidths.Add(TdxGlyphNames.LowerCircleplus, 768);
    FSymbolWidths.Add(TdxGlyphNames.LowerEmptyset, 823);
    FSymbolWidths.Add(TdxGlyphNames.LowerIntersection, 768);
    FSymbolWidths.Add(TdxGlyphNames.LowerUnion, 768);
    FSymbolWidths.Add(TdxGlyphNames.LowerPropersuperset, 713);
    FSymbolWidths.Add(TdxGlyphNames.LowerReflexsuperset, 713);
    FSymbolWidths.Add(TdxGlyphNames.LowerNotsubset, 713);
    FSymbolWidths.Add(TdxGlyphNames.LowerPropersubset, 713);
    FSymbolWidths.Add(TdxGlyphNames.LowerReflexsubset, 713);
    FSymbolWidths.Add(TdxGlyphNames.LowerElement, 713);
    FSymbolWidths.Add(TdxGlyphNames.LowerNotelement, 713);
    FSymbolWidths.Add(TdxGlyphNames.LowerAngle, 768);
    FSymbolWidths.Add(TdxGlyphNames.LowerGradient, 713);
    FSymbolWidths.Add(TdxGlyphNames.LowerRegisterserif, 790);
    FSymbolWidths.Add(TdxGlyphNames.LowerCopyrightserif, 790);
    FSymbolWidths.Add(TdxGlyphNames.LowerTrademarkserif, 890);
    FSymbolWidths.Add(TdxGlyphNames.LowerProduct, 823);
    FSymbolWidths.Add(TdxGlyphNames.LowerRadical, 549);
    FSymbolWidths.Add(TdxGlyphNames.LowerDotmath, 250);
    FSymbolWidths.Add(TdxGlyphNames.LowerLogicalnot, 713);
    FSymbolWidths.Add(TdxGlyphNames.LowerLogicaland, 603);
    FSymbolWidths.Add(TdxGlyphNames.LowerLogicalor, 603);
    FSymbolWidths.Add(TdxGlyphNames.LowerArrowdblboth, 1042);
    FSymbolWidths.Add(TdxGlyphNames.LowerArrowdblleft, 987);
    FSymbolWidths.Add(TdxGlyphNames.LowerArrowdblup, 603);
    FSymbolWidths.Add(TdxGlyphNames.LowerArrowdblright, 987);
    FSymbolWidths.Add(TdxGlyphNames.LowerArrowdbldown, 603);
    FSymbolWidths.Add(TdxGlyphNames.LowerLozenge, 494);
    FSymbolWidths.Add(TdxGlyphNames.LowerAngleleft, 329);
    FSymbolWidths.Add(TdxGlyphNames.LowerRegistersans, 790);
    FSymbolWidths.Add(TdxGlyphNames.LowerCopyrightsans, 790);
    FSymbolWidths.Add(TdxGlyphNames.LowerTrademarksans, 786);
    FSymbolWidths.Add(TdxGlyphNames.LowerSummation, 713);
    FSymbolWidths.Add(TdxGlyphNames.LowerParenlefttp, 384);
    FSymbolWidths.Add(TdxGlyphNames.LowerParenleftex, 384);
    FSymbolWidths.Add(TdxGlyphNames.LowerParenleftbt, 384);
    FSymbolWidths.Add(TdxGlyphNames.LowerBracketlefttp, 384);
    FSymbolWidths.Add(TdxGlyphNames.LowerBracketleftex, 384);
    FSymbolWidths.Add(TdxGlyphNames.LowerBracketleftbt, 384);
    FSymbolWidths.Add(TdxGlyphNames.LowerBracelefttp, 494);
    FSymbolWidths.Add(TdxGlyphNames.LowerBraceleftmid, 494);
    FSymbolWidths.Add(TdxGlyphNames.LowerBraceleftbt, 494);
    FSymbolWidths.Add(TdxGlyphNames.LowerBraceex, 494);
    FSymbolWidths.Add(TdxGlyphNames.LowerAngleright, 329);
    FSymbolWidths.Add(TdxGlyphNames.LowerIntegral, 274);
    FSymbolWidths.Add(TdxGlyphNames.LowerIntegraltp, 686);
    FSymbolWidths.Add(TdxGlyphNames.LowerIntegralex, 686);
    FSymbolWidths.Add(TdxGlyphNames.LowerIntegralbt, 686);
    FSymbolWidths.Add(TdxGlyphNames.LowerParenrighttp, 384);
    FSymbolWidths.Add(TdxGlyphNames.LowerParenrightex, 384);
    FSymbolWidths.Add(TdxGlyphNames.LowerParenrightbt, 384);
    FSymbolWidths.Add(TdxGlyphNames.LowerBracketrighttp, 384);
    FSymbolWidths.Add(TdxGlyphNames.LowerBracketrightex, 384);
    FSymbolWidths.Add(TdxGlyphNames.LowerBracketrightbt, 384);
    FSymbolWidths.Add(TdxGlyphNames.LowerBracerighttp, 494);
    FSymbolWidths.Add(TdxGlyphNames.LowerBracerightmid, 494);
    FSymbolWidths.Add(TdxGlyphNames.LowerBracerightbt, 494);
    FSymbolWidths.Add(TdxGlyphNames.LowerApple, 790);
    FSymbolWidths.TrimExcess;
  end;
  Result := FSymbolWidths;
end;

function TdxPDFSimpleFontDefaultWidths.GetZapfDingbatsWidths: TdxPDFWordDictionary;
begin
  if FZapfDingbatsWidths.Count = 0 then
  begin
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerSpace, 278);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA1, 974);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA2, 961);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA202, 974);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA3, 980);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA4, 719);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA5, 789);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA119, 790);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA118, 791);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA117, 690);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA11, 960);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA12, 939);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA13, 549);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA14, 855);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA15, 911);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA16, 933);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA105, 911);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA17, 945);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA18, 974);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA19, 755);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA20, 846);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA21, 762);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA22, 761);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA23, 571);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA24, 677);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA25, 763);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA26, 760);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA27, 759);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA28, 754);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA6, 494);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA7, 552);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA8, 537);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA9, 577);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA10, 692);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA29, 786);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA30, 788);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA31, 788);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA32, 790);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA33, 793);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA34, 794);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA35, 816);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA36, 823);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA37, 789);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA38, 841);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA39, 823);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA40, 833);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA41, 816);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA42, 831);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA43, 923);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA44, 744);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA45, 723);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA46, 749);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA47, 790);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA48, 792);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA49, 695);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA50, 776);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA51, 768);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA52, 792);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA53, 759);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA54, 707);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA55, 708);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA56, 682);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA57, 701);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA58, 826);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA59, 815);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA60, 789);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA61, 789);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA62, 707);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA63, 687);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA64, 696);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA65, 689);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA66, 786);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA67, 787);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA68, 713);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA69, 791);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA70, 785);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA71, 791);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA72, 873);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA73, 761);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA74, 762);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA203, 762);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA75, 759);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA204, 759);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA76, 892);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA77, 892);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA78, 788);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA79, 784);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA81, 438);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA82, 138);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA83, 277);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA84, 415);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA97, 392);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA98, 392);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA99, 668);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA100, 668);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA89, 390);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA90, 390);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA93, 317);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA94, 317);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA91, 276);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA92, 276);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA205, 509);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA85, 509);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA206, 410);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA86, 410);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA87, 234);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA88, 234);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA95, 334);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA96, 334);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA101, 732);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA102, 544);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA103, 544);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA104, 910);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA106, 667);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA107, 760);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA108, 760);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA112, 776);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA111, 595);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA110, 694);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA109, 626);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA120, 788);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA121, 788);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA122, 788);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA123, 788);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA124, 788);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA125, 788);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA126, 788);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA127, 788);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA128, 788);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA129, 788);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA130, 788);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA131, 788);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA132, 788);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA133, 788);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA134, 788);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA135, 788);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA136, 788);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA137, 788);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA138, 788);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA139, 788);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA140, 788);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA141, 788);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA142, 788);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA143, 788);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA144, 788);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA145, 788);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA146, 788);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA147, 788);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA148, 788);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA149, 788);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA150, 788);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA151, 788);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA152, 788);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA153, 788);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA154, 788);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA155, 788);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA156, 788);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA157, 788);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA158, 788);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA159, 788);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA160, 894);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA161, 838);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA163, 1016);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA164, 458);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA196, 748);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA165, 924);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA192, 748);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA166, 918);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA167, 927);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA168, 928);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA169, 928);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA170, 834);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA171, 873);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA172, 828);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA173, 924);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA162, 924);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA174, 917);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA175, 930);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA176, 931);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA177, 463);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA178, 883);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA179, 836);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA193, 836);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA180, 867);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA199, 867);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA181, 696);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA200, 696);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA182, 874);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA201, 874);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA183, 760);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA184, 946);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA197, 771);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA185, 865);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA194, 771);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA198, 888);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA186, 967);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA195, 888);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA187, 831);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA188, 873);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA189, 927);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA190, 970);
    FZapfDingbatsWidths.Add(TdxGlyphNames.LowerA191, 918);
    FZapfDingbatsWidths.TrimExcess;
  end;
  Result := FZapfDingbatsWidths;
end;

{ TdxPDFCompositeFont }

constructor TdxPDFCompositeFont.Create(const ABaseFont: string; AFontDescriptor: TdxPDFFontDescriptor);
begin
  inherited Create(ABaseFont, AFontDescriptor);
  Encoding := TdxPDFHorizontalIdentityEncoding.Create(Self);
  FCIDBaseFont := ABaseFont;
  FDefaultWidth := 1000;
end;

class function TdxPDFCompositeFont.GetSubTypeName: string;
begin
  Result := TdxPDFKeywords.FontType0;
end;

class function TdxPDFCompositeFont.GetDictionary(ADictionary: TdxPDFReaderDictionary): TdxPDFReaderDictionary;
var
  AFont: TdxPDFCompositeFont;
begin
  AFont := TdxPDFCompositeFont.Create(nil);
  try
    Result := AFont.GetFontDictionary(ADictionary);
  finally
    AFont.Free;
  end;
end;

procedure TdxPDFCompositeFont.GetGlyphPositions(const AStringData: TdxPDFStringData; AFontSizeFactor,
  ACharacterSpacing, AWordSpacing, AScalingFactor: Double; var AResult: TDoubleDynArray);
var
  I, J: Integer;
  AActualCharCode: Word;
  ACharCode: TBytes;
  APosition, ACurrentWidth, ACurrentWordSpacing: Double;
  AIndex, ACharCodeLength: Integer;
begin
  J := 1;
  APosition := 0;
  for I := 0 to Length(AStringData.Glyphs) - 1 do
  begin
    ACharCode := AStringData.CharacterCodes[I];
    ACharCodeLength := Length(ACharCode);
    if ACharCodeLength > 0 then
    begin
      AActualCharCode := ACharCode[0];
      for AIndex := 0 to ACharCodeLength - 1 do
        AActualCharCode := ACharCode[AIndex] + (AActualCharCode shl 8);
    end
    else
      AActualCharCode := 0;
    ACurrentWordSpacing := IfThen(AActualCharCode = 32, AWordSpacing, 0);
    if not Widths.TryGetValue(AStringData.Glyphs[I], ACurrentWidth) then
      ACurrentWidth := DefaultWidth;
    APosition := APosition + ((ACurrentWidth - AStringData.Offsets[J]) * AFontSizeFactor * WidthFactor +
      ACharacterSpacing + ACurrentWordSpacing * WidthFactor) * AScalingFactor;
    TdxPDFUtils.AddValue(APosition, AResult);
    Inc(J);
  end;
end;

procedure TdxPDFCompositeFont.UpdateGlyphs(var AGlyphs: TSmallIntDynArray);
var
  ACID: Word;
  AGID: SmallInt;
  I, AMapLength, ALength: Integer;
begin
  inherited UpdateGlyphs(AGlyphs);
  if Length(FCIDToGIDMap) > 0 then
  begin
    AMapLength := Length(FCIDToGIDMap);
    ALength := Length(AGlyphs);
    for I := 0 to ALength - 1 do
    begin
      ACID := Word(AGlyphs[I]);
      if ACID >= AMapLength then
        AGlyphs[I] := Word(0)
      else
        AGlyphs[I] := FCIDToGIDMap[ACID];
    end;
  end;
  if FIsIdentityEncoding then
    for I := 0 to Length(AGlyphs) - 1 do
    begin
      if not FEmbeddedCIDToGIDMapping.TryGetValue(AGlyphs[I], AGID) then
        AGID := 0;
      AGlyphs[I] := AGID;
    end;
end;

function TdxPDFCompositeFont.GetFontDescriptorDictionary(ADictionary: TdxPDFReaderDictionary): TdxPDFReaderDictionary;
var
  ACIDFontDictionary: TdxPDFDictionary;
begin
  Result := nil;
  ACIDFontDictionary := GetFontDictionary(ADictionary);
  if ACIDFontDictionary <> nil then
    Result := ACIDFontDictionary.GetDictionary(TdxPDFKeywords.FontDescriptor) as TdxPDFReaderDictionary;
end;

function TdxPDFCompositeFont.CreateGlyphWidths: TdxPDFDoubleList;
begin
  Result := TdxPDFDoubleList.Create(Widths.Values);
end;

procedure TdxPDFCompositeFont.Read(ADictionary: TdxPDFReaderDictionary);
begin
  inherited Read(ADictionary);
  ReadGlyphMap(GetFontDictionary(ADictionary));
  ReadFontFileData(GetFontDescriptorDictionary(ADictionary));
end;

procedure TdxPDFCompositeFont.ReadEncoding(ASourceObject: TdxPDFBase);
begin
  inherited ReadEncoding(ASourceObject);
  Encoding := TdxPDFCompositeFontEncoding.Parse(Repository, ASourceObject) as TdxPDFCustomEncoding;
end;

procedure TdxPDFCompositeFont.ReadFontDescriptor(ADictionary: TdxPDFReaderDictionary);
begin
  inherited ReadFontDescriptor(ADictionary);
  if FontDescriptor.FontName = ''  then
    FontDescriptor.FontName := BaseFont;
  if FontDescriptor.FontWeight = 0 then
    FontDescriptor.FontWeight := 400;
  if FontDescriptor.Ascent < 0 then
    FontDescriptor.Ascent := -FontDescriptor.Ascent;
  if FontDescriptor.Descent > 0 then
    FontDescriptor.Descent := -FontDescriptor.Descent;
end;

procedure TdxPDFCompositeFont.ReadWidths(ADictionary: TdxPDFReaderDictionary);
var
  I: Integer;
  AStartIndex, AEndIndex: Integer;
  AArrayObject: TdxPDFReferencedObject;
  AValue: TdxPDFBase;
  AWidth: Double;
  AWidthArray: TdxPDFArray;
begin
  FDefaultWidth := ADictionary.GetInteger('DW', 1000);
  AWidthArray := ADictionary.GetArray(TdxPDFKeywords.ShortWidths, TdxPDFKeywords.Widths);
  if (AWidthArray <> nil) and (AWidthArray.Count > 0 )then
  begin
    AStartIndex := -1;
    AEndIndex := -1;
    for AValue in AWidthArray.ElementList do
    begin
      if AStartIndex < 0 then
        AStartIndex := (AValue as TdxPDFInteger).Value
      else
        if AEndIndex < 0 then
          case AValue.ObjectType of
            otInteger:
              AEndIndex := TdxPDFInteger(AValue).Value;
            otIndirectReference:
              begin
                AArrayObject := Repository.GetObject(TdxPDFIndirectObject(AValue).Number);
                if AArrayObject is TdxPDFArray then
                  ReadWidthArray(TdxPDFArray(AArrayObject).ElementList, AStartIndex);
              end;
            otArray:
              ReadWidthArray(TdxPDFArray(AValue).ElementList, AStartIndex);
          end
        else
        begin
          AWidth := TdxPDFDouble(AValue).Value;
          for I := AStartIndex to AEndIndex do
            AddWidth(I, AWidth);
          AStartIndex := -1;
          AEndIndex := -1;
        end;
    end;
  end;
end;

procedure TdxPDFCompositeFont.ReadFontFileData(ADictionary: TdxPDFReaderDictionary);
begin
// do nothing
end;

procedure TdxPDFCompositeFont.SetFontFileData(const AValue: TBytes);
begin
  GenerateRegistrationName;
  FFontFileData := AValue;
end;

procedure TdxPDFCompositeFont.ReadGlyphMap(ADictionary: TdxPDFReaderDictionary);
var
  AValue: TdxPDFBase;
  AStream: TdxPDFStream;
  AData: TBytes;
  I, J, AMappingSize, ALength: Integer;
begin
  AValue := ADictionary.GetObject(TdxPDFKeywords.CIDToGIDMap);
  if AValue <> nil then
    case AValue.ObjectType of
      otName:
        SetLength(FCIDToGIDMap, 0);
      otIndirectReference, otStream:
        begin
          if AValue.ObjectType = otStream then
            AStream := AValue as TdxPDFStream
          else
            AStream := Repository.GetStream(TdxPDFReference(AValue).Number);
          if AStream <> nil then
          begin
            AData := AStream.UncompressedData;
            ALength := Length(AData);
            if ALength mod 2 <= 0 then
            begin
              AMappingSize := ALength div 2;
              SetLength(FCIDToGIDMap, AMappingSize);
              J := 0;
              for I := 0 to AMappingSize - 1 do
              begin
                FCIDToGIDMap[I] := Word((AData[J] shl 8) + AData[J + 1]);
                Inc(J, 2);
              end;
            end;
          end;
        end;
    end;
end;

procedure TdxPDFCompositeFont.ReadWidthArray(AElementList: TdxPDFBaseList; var AStartIndex: Integer);
var
  AValue: TdxPDFBase;
begin
  for AValue in AElementList do
  begin
    if AValue.ObjectType in [otInteger, otDouble] then
      AddWidth(AStartIndex, TdxPDFNumericObjectAccess(AValue).InternalValue);
    Inc(AStartIndex);
  end;
  AStartIndex := -1;
end;

function TdxPDFCompositeFont.GetFontDictionary(ADictionary: TdxPDFReaderDictionary): TdxPDFReaderDictionary;
var
  ADescendantFonts: TdxPDFArray;
begin
  ADescendantFonts := ADictionary.GetArray(TdxPDFKeywords.DescendantFonts);
  if (ADescendantFonts = nil) or (ADescendantFonts.Count <> 1) then
    TdxPDFUtils.RaiseTestException('TdxPDFCompositeFont.Parse');

  if ADescendantFonts[0].ObjectType <> otDictionary then
  begin
    if ADescendantFonts[0].ObjectType <> otIndirectReference then
      TdxPDFUtils.RaiseTestException('TdxPDFCompositeFont.Parse');
    Result := ADictionary.Repository.GetDictionary(TdxPDFReference(ADescendantFonts[0]).Number);
    if Result = nil then
      TdxPDFUtils.RaiseTestException('TdxPDFCompositeFont.Parse');
  end
  else
    Result := ADescendantFonts[0] as TdxPDFReaderDictionary;
end;

function TdxPDFCompositeFont.GetCompactFontFileData: TBytes;
begin
  Result := FCompactFontFileData;
end;

function TdxPDFCompositeFont.GetCharset: TDictionary<SmallInt, SmallInt>;
begin
  Result := FEmbeddedCIDToGIDMapping;
end;

function TdxPDFCompositeFont.GetUseGlyphIndexes: Boolean;
begin
  Result := not FIsIdentityEncoding;
end;

procedure TdxPDFCompositeFont.DestroySubclasses;
begin
  FreeAndNil(FEmbeddedCIDToGIDMapping);
  inherited DestroySubclasses;
end;

procedure TdxPDFCompositeFont.UpdateCharset;
var
  ACompactFontFileData: TBytes;
  AFontProgram: TdxType1FontCompactFontProgram;
  ASIDToGIDMapping: TDictionary<SmallInt, SmallInt>;
  AKey, AValue: SmallInt;
begin
  ACompactFontFileData := ActualCompactFontFileData;
  if ACompactFontFileData <> nil then
  begin
    AFontProgram := TdxType1FontCompactFontProgram.Parse(ACompactFontFileData);
    if AFontProgram <> nil then
      try
        if AFontProgram.Validate then
          ActualCompactFontFileData := TdxPDFCompactFontFormatTopDictIndexWriter.Write(AFontProgram);
        FEmbeddedCIDToGIDMapping := TDictionary<SmallInt, SmallInt>.Create;
        ASIDToGIDMapping := AFontProgram.Charset.SidToGIDMapping;
        for AKey in ASIDToGIDMapping.Keys do
          if ASidToGIDMapping.TryGetValue(AKey, AValue) then
            FEmbeddedCIDToGIDMapping.Add(AKey, AValue);
        FIsIdentityEncoding := AFontProgram is TdxType1FontCompactCIDFontProgram;
      finally
        AFontProgram.Free;
      end;
  end;
end;

{ TdxPDFCIDType0Font }

class function TdxPDFCIDType0Font.GetSubTypeName: string;
begin
  Result := TdxPDFKeywords.CIDFontType0;
end;

function TdxPDFCIDType0Font.GetActualCompactFontFileData: TBytes;
begin
  Result := FActualCompactFontFileData;
end;

function TdxPDFCIDType0Font.GetCompactFontFileData: TBytes;
begin
  Result := ActualCompactFontFileData;
end;

function TdxPDFCIDType0Font.GetUseGlyphIndexes: Boolean;
begin
  Result := True;
end;

procedure TdxPDFCIDType0Font.DestroySubClasses;
begin
  FreeAndNil(FType1FontFileData);
  inherited DestroySubClasses;
end;

procedure TdxPDFCIDType0Font.ReadFontFileData(ADictionary: TdxPDFReaderDictionary);
var
  AStream: TdxPDFStream;
begin
  inherited ReadFontFileData(ADictionary);
  if ADictionary <> nil then
  begin
    AStream := GetStream(TdxPDFKeywords.FontFile3, ADictionary);
    if AStream = nil then
    begin
      FType1FontFileData := TdxPDFType1FontFileData.Parse(Self, ADictionary);
      if FType1FontFileData = nil then
      begin
        if not (Encoding is TdxPDFPredefinedCompositeFontEncoding) then
          TdxPDFUtils.RaiseTestException('TdxPDFCIDType0Font.ReadFontFileData');
      end
      else
        GenerateRegistrationName;
    end
    else
    begin
      GenerateRegistrationName;
      ReadOpenTypeFontFileData(ADictionary, True);
      if FOpenTypeFontFileData = nil then
      begin
        if AStream.Dictionary.GetString(TdxPDFKeywords.Subtype) <> TdxPDFKeywords.CIDFontType0C then
          TdxPDFUtils.RaiseTestException('TdxPDFCIDType0Font.ReadFontFileData');
        FCompactFontFileData := TdxFontFileHelper.GetValidCFFData(AStream.UncompressedData);
      end
      else
        FCompactFontFileData := TdxFontFileHelper.GetValidCFFData(FOpenTypeFontFileData);
      FActualCompactFontFileData := FCompactFontFileData;
      UpdateCharset;
    end;
  end;
end;

procedure TdxPDFCIDType0Font.SetActualCompactFontFileData(const AValue: TBytes);
begin
  FActualCompactFontFileData := AValue;
end;

{ TdxPDFCIDType2Font }

class function TdxPDFCIDType2Font.GetSubTypeName: string;
begin
  Result := TdxPDFKeywords.CIDFontType2;
end;

function TdxPDFCIDType2Font.GetActualCompactFontFileData: TBytes;
begin
  Result := FCompactFontFileData;
end;

function TdxPDFCIDType2Font.GetTrueTypeFontFileData: TBytes;
begin
  Result := FPatchedFontFileData;
end;

function TdxPDFCIDType2Font.GetUseGlyphIndexes: Boolean;
begin
  Result := (Length(FCompactFontFileData) = 0) or (FCompactFontFileData = nil) or inherited GetUseGlyphIndexes;
end;

procedure TdxPDFCIDType2Font.ReadFontFileData(ADictionary: TdxPDFReaderDictionary);
var
  AStream: TdxPDFStream;
begin
  inherited ReadFontFileData(ADictionary);
  if ADictionary <> nil then
  begin
    AStream := ADictionary.GetStream(TdxPDFKeywords.FontFile2);
    if AStream = nil then
    begin
      FOpenTypeFontFileData := ReadOpenTypeFontFileData(ADictionary, False);
      PatchFontFileData(FOpenTypeFontFileData);
    end
    else
    begin
      GenerateRegistrationName;
      if AStream.Dictionary.GetString(TdxPDFKeywords.Subtype) = TdxPDFKeywords.FontFileSubtype then
      begin
        FCompactFontFileData := AStream.UncompressedData;
        UpdateCharset;
      end
      else
        FontFileData := AStream.UncompressedData;
    end;
  end;
end;

function TdxPDFCIDType2Font.GetBaseFontName: string;
begin
  Result := BaseFont;
end;

function TdxPDFCIDType2Font.GetFontName: string;
begin
  Result := Name;
end;

function TdxPDFCIDType2Font.GetFontFileData: TBytes;
begin
  Result := FPatchedFontFileData;
end;

function TdxPDFCIDType2Font.GetDescriptor: TdxPDFFontDescriptor;
begin
  Result := FontDescriptor;
end;

function TdxPDFCIDType2Font.GetCompactFontFileData: TBytes;
begin
  Result := FCompactFontFileData;
end;

procedure TdxPDFCIDType2Font.SetFontFileData(const AData: TBytes);
begin
  FFontFileData := AData;
  PatchFontFileData(FFontFileData);
end;

procedure TdxPDFCIDType2Font.PatchFontFileData(const AFontFileData: TBytes);
var
  AFontFileInfo: TdxFontFileInfo;
begin
  if (AFontFileData <> nil) or (Length(AFontFileData) > 0) then
  begin
    FCompactFontFileData := TdxFontFileHelper.GetCFFData(AFontFileData);
    if Length(FCompactFontFileData) = 0 then
    begin
      GenerateRegistrationName;
      AFontFileInfo := TdxFontFileHelper.GetFontFileInfo(Self, AFontFileData);
      FPatchedFontFileData := AFontFileInfo.Data;
      FreeAndNil(AFontFileInfo.GlyphMapping);
      FreeAndNil(AFontFileInfo.GlyphRanges);
    end
    else
      UpdateCharset;
  end;
end;

{ TdxPDFSimpleFont }

constructor TdxPDFSimpleFont.Create(const ABaseFont: string; AFontDescriptor: TdxPDFFontDescriptor;
  AEncoding: TdxPDFSimpleFontEncoding; AFirstChar: Integer; const AWidths: TDoubleDynArray);
begin
  inherited Create(ABaseFont, AFontDescriptor);
  Encoding := AEncoding;
  FFirstChar := AFirstChar;
  FLastChar := 255;
  FWidths := AWidths;
  UpdateWidthsList;
end;

procedure TdxPDFSimpleFont.GetGlyphPositions(const AStringData: TdxPDFStringData; AFontSizeFactor,
  ACharacterSpacing, AWordSpacing, AScalingFactor: Double; var AResult: TDoubleDynArray);
var
  AMissingWidth, ATx, AW0: Double;
  I, J: Integer;
  AChr: Word;
  ATempWordSpacing: Double;
begin
  if FontDescriptor = nil then
    AMissingWidth := 0
  else
    AMissingWidth := FontDescriptor.MissingWidth;

  SetLength(AResult, Length(AStringData.Glyphs));
  ATx := 0;
  J := 1;
  for I := 0 to Length(AStringData.Glyphs) - 1 do
  begin
    AChr := AStringData.Glyphs[I];

    AW0 := 0;
    if not Widths.TryGetValue(AChr, AW0) then
      AW0 := AMissingWidth;

    ATempWordSpacing := IfThen(AStringData.CharacterCodes[I][0] = 32, AWordSpacing, 0);

    ATx := ATx + ((AW0 * WidthFactor - AStringData.Offsets[J] * 0.001) * AFontSizeFactor +
      ACharacterSpacing + ATempWordSpacing) * AScalingFactor;
    Inc(J);
    AResult[I] := ATx;
  end;
end;

procedure TdxPDFSimpleFont.UpdateGlyphs(var AGlyphs: TSmallIntDynArray);
var
  I, ALength: Integer;
  AEncoding: TdxPDFSimpleFontEncoding;
  AGlyphCode: SmallInt;
  AGlyphName: string;
  AFontProgramEncoding: TDictionary<Byte, string>;
  AMapping: TDictionary<string, SmallInt>;
begin
  if GlyphMapping <> nil then
  begin
    ALength := Length(AGlyphs);
    AEncoding := Encoding as TdxPDFSimpleFontEncoding;
    AMapping := FGlyphMapping.Mapping;
    AFontProgramEncoding := FGlyphMapping.Encoding;
    if AFontProgramEncoding = nil then
      for I := 0 to ALength - 1 do
      begin
        if not AMapping.TryGetValue(AEncoding.GetGlyphName(Byte(AGlyphs[I])), AGlyphCode) then
          AGlyphCode := 0;
        AGlyphs[I] := AGlyphCode;
      end
    else
      if AEncoding.Differences.Count = 0 then
      begin
        for I := 0 to ALength - 1 do
          if AFontProgramEncoding.TryGetValue(AGlyphs[I], AGlyphName) then
          begin
            if not AMapping.TryGetValue(AGlyphName, AGlyphCode) then
              AGlyphCode := 0;
            AGlyphs[I] := AGlyphCode;
          end
          else
            AGlyphs[I] := 0;
      end
      else
        for I := 0 to ALength - 1 do
        begin
          AGlyphCode := AGlyphs[I];
          if AEncoding.Differences.TryGetValue(AGlyphCode, AGlyphName) or AFontProgramEncoding.TryGetValue(Byte(AGlyphCode), AGlyphName) then
          begin
            if not AMapping.TryGetValue(AGlyphName, AGlyphCode) then
              AGlyphCode := 0;
            AGlyphs[I] := AGlyphCode;
          end
          else
            AGlyphs[I] := 0;
        end;
  end;
end;

function TdxPDFSimpleFont.CreateGlyphWidths: TdxPDFDoubleList;
begin
  Result := TdxPDFDoubleList.Create(ActualWidths);
end;

function TdxPDFSimpleFont.GetCharset: TDictionary<SmallInt, SmallInt>;
begin
  UpdateType1FontProgramProperties;
  Result := FSIDToGIDMapping;
end;

function TdxPDFSimpleFont.GetUseGlyphIndexes: Boolean;
begin
  UpdateType1FontProgramProperties;
  Result := FUseGlyphIndexes;
end;

procedure TdxPDFSimpleFont.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FActualWidths := TdxPDFDoubleList.Create;
end;

procedure TdxPDFSimpleFont.DestroySubClasses;
begin
  FreeAndNil(FSIDToGIDMapping);
  FreeAndNil(FGlyphMapping);
  FreeAndNil(FActualWidths);
  inherited DestroySubClasses;
end;

procedure TdxPDFSimpleFont.ReadEncoding(AObject: TdxPDFBase);
begin
  inherited ReadEncoding(AObject);
  if AObject <> nil then
    case AObject.ObjectType of
      otDictionary:
        Encoding := CreateEncoding(AObject as TdxPDFReaderDictionary);
      otName, otString:
        Encoding := dxPDFGetDocumentObjectClass(TdxPDFString(AObject).Value).Create(Self) as TdxPDFSimpleFontEncoding;
    end
  else
    if BaseFont = TdxPDFKeywords.SymbolFontName then
      Encoding := TdxPDFSymbolEncoding.Create(Self)
    else
      if BaseFont = TdxPDFKeywords.ZapfDingbatsFontName then
        Encoding := TdxPDFZapfDingbatsEncoding.Create(Self)
      else
        Encoding := TdxPDFStandartEncoding.Create(Self);
end;

procedure TdxPDFSimpleFont.ReadProperties(ADictionary: TdxPDFReaderDictionary);
begin
  inherited ReadProperties(ADictionary);
  FFirstChar := ADictionary.GetInteger(TdxPDFKeywords.FirstChar, 0);
  FLastChar := ADictionary.GetInteger(TdxPDFKeywords.LastChar, 255);
end;

procedure TdxPDFSimpleFont.ReadWidths(ADictionary: TdxPDFReaderDictionary);
var
  ACode: Word;
  AGlyphName: string;
  J: Byte;
  I, AIndex: Integer;
  AWidthArray: TdxPDFArray;
  AWidthsDictionary: TdxPDFWordDictionary;
begin
  AWidthArray := ADictionary.GetArray(TdxPDFKeywords.Widths);
  if (AWidthArray = nil) or (AWidthArray.Count = 0 )then
  begin
    if IsCourierFont then
      for I := 0 to FLastChar - FFirstChar do
        FActualWidths.Add(600)
    else
    begin
      AWidthsDictionary := GetDefaultWidthsDictionary;
      if AWidthsDictionary <> nil then
      begin
        J := Byte(FFirstChar);
        for I := 0 to FLastChar - FFirstChar do
        begin
          FActualWidths.Add(0);
          AGlyphName := (Encoding as TdxPDFSimpleFontEncoding).GetGlyphName(J);
          if AGlyphName <> '' then
          begin
            if AWidthsDictionary.TryGetValue(AGlyphName, ACode) then
              FActualWidths[I] := ACode
            else
              FActualWidths[I] := 0;
          end;
          Inc(J);
        end;
      end;
    end;
  end
  else
    for I := 0 to AWidthArray.Count - 1 do
        FActualWidths.Add(TdxPDFUtils.ConvertToDouble(AWidthArray[I]));

  if ActualWidths <> nil then
  begin
    AIndex := 0;
    for I := FFirstChar to FFirstChar + Min(FLastChar - FFirstChar, ActualWidths.Count - 1) do
    begin
      if FActualWidths[AIndex] > 0 then
        AddWidth(I, FActualWidths[AIndex]);
      Inc(AIndex);
    end;
  end;
end;

function TdxPDFSimpleFont.GetDefaultWidthsDictionary: TdxPDFWordDictionary;
begin
  Result := dxPDFSimpleFontDefaultWidths.GetDefaultWidthsDictionary(BaseFont);
end;

function TdxPDFSimpleFont.IsCourierFont: Boolean;
begin
  Result := False;
end;

function TdxPDFSimpleFont.CreateEncoding(ADictionary: TdxPDFReaderDictionary): TdxPDFSimpleFontEncoding;
var
  AEncoding: TdxPDFBase;
  AEncodingClass: TdxPDFObjectClass;
  AEncodingName: string;
begin
  if (ADictionary <> nil) then
  begin
    AEncoding := ADictionary.GetObject(TdxPDFKeywords.BaseEncoding);
    if AEncoding <> nil then
    begin
      case AEncoding.ObjectType of
        otString, otName:
          AEncodingName := TdxPDFStringAccess(AEncoding).InternalValue;
        otDictionary:
          AEncodingName := TdxPDFDictionary(AEncoding).GetString(TdxPDFKeywords.BaseEncoding);
      else
        Exit(nil);
      end;
      AEncodingClass := dxPDFGetDocumentObjectClass(AEncodingName);
      if AEncodingClass = nil then
        AEncodingClass := TdxPDFStandartEncoding;
      Result := AEncodingClass.Create(nil) as TdxPDFSimpleFontEncoding;
    end
    else
      if BaseFont = TdxPDFKeywords.SymbolFontName then
        Result := TdxPDFSymbolEncoding.Create(Self)
      else
        if BaseFont = TdxPDFKeywords.ZapfDingbatsFontName then
          Result := TdxPDFZapfDingbatsEncoding.Create(Self)
        else
          Result := TdxPDFStandartEncoding.Create(Self);
    TdxPDFObjectAccess(Result).Read(ADictionary);
  end
  else
    Result := nil;
end;

procedure TdxPDFSimpleFont.UpdateType1FontProgramProperties;
var
  AFontProgram: TdxType1FontCompactFontProgram;
  ACompactFontFileData: TBytes;
  AKey, AValue: SmallInt;
  ASidToGIDMapping:TDictionary<SmallInt, SmallInt>;
  AType1FontFileData: TdxPDFType1FontFileData;
begin
  if FGlyphMapping = nil then
  begin
    ACompactFontFileData := GetCompactFontFileData;
    if Length(ACompactFontFileData) = 0 then
    begin
      AType1FontFileData := GetType1FontFileData;
      if AType1FontFileData = nil then
        FGlyphMapping := TdxType1FontGlyphMapping.Create
      else
      begin
        FGlyphMapping := AType1FontFileData.GlyphMapping;
        FUseGlyphIndexes := True;
      end;
    end
    else
    begin
      AFontProgram := TdxType1FontCompactFontProgram.Parse(ACompactFontFileData);
      if AFontProgram <> nil then
        try
          FSIDToGIDMapping := TDictionary<SmallInt, SmallInt>.Create;
          ASidToGIDMapping := AFontProgram.Charset.SidToGIDMapping;
          for AKey in ASidToGIDMapping.Keys do
            if ASidToGIDMapping.TryGetValue(AKey, AValue) then
              FSIDToGIDMapping.AddOrSetValue(AKey, AValue);
          FGlyphMapping := AFontProgram.GetGlyphMapping(Encoding.ShouldUseEmbeddedFontEncoding);
          FUseGlyphIndexes := True;
        finally
          AFontProgram.Free;
        end;
    end;
  end;
end;

procedure TdxPDFSimpleFont.UpdateWidthsList;
var
  AActualWidths: TDoubleDynArray;
  AActualFirstChar, ANewLastChar, I, AIndex: Integer;
begin
  AActualWidths := FWidths;
  if Length(AActualWidths) > 0 then
  begin
    AActualFirstChar := FirstChar;
    ANewLastChar := AActualFirstChar + Min(LastChar - AActualFirstChar, Length(AActualWidths) - 1);
    AIndex := 0;
    I := AActualFirstChar;
    while I <= ANewLastChar do
    begin
      Widths.Add(I, AActualWidths[AIndex + 1]);
      Inc(AIndex);
      Inc(I);
    end;
  end;
end;

{ TdxPDFTrueTypeFont }

class function TdxPDFTrueTypeFont.GetSubTypeName: string;
begin
  Result := TdxPDFKeywords.TrueType;
end;

class procedure TdxPDFTrueTypeFont.UpdateGlyphs(var AGlyphs: TSmallIntDynArray; AEncoding: TdxPDFSimpleFontEncoding);
begin
  DoUpdateGlyphCodes(AEncoding, TdxFontFileUnicodeConverterAccess(dxFontFileUnicodeConverter).GlyphCodes, AGlyphs);
end;

function TdxPDFTrueTypeFont.IsCourierFont: Boolean;
begin
  Result := (BaseFont = TdxPDFKeywords.CourierNewFontName) or
    (BaseFont = TdxPDFKeywords.CourierNewFontName + ',' + TdxPDFKeywords.Bold) or
    (BaseFont = TdxPDFKeywords.CourierNewFontName + ',' + TdxPDFKeywords.Italic) or
    (BaseFont = TdxPDFKeywords.CourierNewFontName + ',' + TdxPDFKeywords.Bold + TdxPDFKeywords.Italic);
end;

function TdxPDFTrueTypeFont.GetTrueTypeFontFileData: TBytes;
begin
  Result := FPatchedFontFileData;
end;

function TdxPDFTrueTypeFont.GetType1FontFileData: TdxPDFType1FontFileData;
begin
  Result := FType1FontFileData;
end;

function TdxPDFTrueTypeFont.GetUseGlyphIndexes: Boolean;
begin
  Result := (Length(FEmbeddedFontEncoding) > 0) or inherited GetUseGlyphIndexes;
end;

procedure TdxPDFTrueTypeFont.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FGlyphMapping := nil;
  FGlyphRanges := nil;
end;

procedure TdxPDFTrueTypeFont.DestroySubClasses;
begin
  SetLength(FEmbeddedFontEncoding, 0);
  FreeAndNil(FType1FontFileData);
  FreeAndNil(FGlyphMapping);
  FreeAndNil(FGlyphRanges);
  inherited DestroySubClasses;
end;

procedure TdxPDFTrueTypeFont.Read(ADictionary: TdxPDFReaderDictionary);
begin
  inherited Read(ADictionary);
  ReadFontFileData(ADictionary.GetDictionary(TdxPDFKeywords.FontDescriptor));
end;

procedure TdxPDFTrueTypeFont.UpdateGlyphs(var AGlyphs: TSmallIntDynArray);

  function SearchRangeIndex(const ARange: TdxFontFileCMapGlyphRange; out AIndex: Integer): Boolean;
  var
    I: Integer;
    AComparer: IComparer<TdxFontFileCMapGlyphRange>;
  begin
    AComparer := TdxFontFileCMapGlyphRangeComparer.Create;
    Result := FGlyphRanges.BinarySearch(ARange, AIndex, AComparer);
    if not Result then
      for I := 0 to FGlyphRanges.Count - 1 do
        if ARange.EndValue < FGlyphRanges[I].StartValue then
        begin
          AIndex := I - 1;
          Exit(True);
        end;
  end;

var
  AFontDescriptor: TdxPDFFontDescriptor;
  AEncoding: TdxPDFSimpleFontEncoding;
  AIsSymbolic: Boolean;
  AGlyphRangeCount, ALength, I, ARangeIndex: Integer;
  AValue: SmallInt;
  ARange: TdxFontFileCMapGlyphRange;
begin
  if Length(FEmbeddedFontEncoding) = 0 then
  begin
    AFontDescriptor := FontDescriptor;
    AEncoding := Encoding as TdxPDFSimpleFontEncoding;
    AIsSymbolic := False;
    if (AFontDescriptor <> nil) and ((FFontFileData <> nil) or not (AEncoding is TdxPDFWinAnsiEncoding)) then
    begin
      AIsSymbolic := (Integer(FontDescriptor.Flags) and Integer(ffSymbolic)) = Integer(ffSymbolic);
      if AIsSymbolic and ((Integer(FontDescriptor.Flags) and Integer(ffNonSymbolic)) <> Integer(ffNonSymbolic)) then
      begin
        if FGlyphRanges <> nil then
        begin
          AGlyphRangeCount := FGlyphRanges.Count;
          ALength := Length(AGlyphs);
          for I := 0 to ALength - 1 do
          begin
            AValue := AGlyphs[I];
            if AValue >= $20 then
            begin
              ARange.StartValue := 0;
              ARange.EndValue := AValue;
              if not SearchRangeIndex(ARange, ARangeIndex) then
                if (ARangeIndex >= AGlyphRangeCount) or (AValue < FGlyphRanges[ARangeIndex].StartValue) then
                  AGlyphs[I] := $F000 + AValue;
            end;
          end;
        end;
        Exit;
      end;
    end;
    if FGlyphMapping = nil then
      DoUpdateGlyphCodes(AEncoding, TdxFontFileUnicodeConverterAccess(dxFontFileUnicodeConverter).GlyphCodes, AGlyphs)
    else
      DoUpdateGlyphCodes(AEncoding, FGlyphMapping, AGlyphs);
    if AIsSymbolic then
    begin
      ALength := Length(AGlyphs);
      for I := 0 to ALength - 1 do
      begin
        AValue := AGlyphs[I];
        if AValue >= $20 then
          AGlyphs[I] := Word($F000 + AValue);
      end;
    end;
  end
  else
    for I := 0 to Length(AGlyphs) - 1 do
      AGlyphs[I] := FEmbeddedFontEncoding[AGlyphs[I]];
end;

class procedure TdxPDFTrueTypeFont.DoUpdateGlyphCodes(AEncoding: TdxPDFSimpleFontEncoding;
  AGlyphCodes: TdxPDFWordDictionary; var AGlyphs: TSmallIntDynArray);
var
  I: Integer;
begin
  for I := 0 to Length(AGlyphs) - 1 do
    AGlyphs[I] := dxPDFUnicodeConverter.GetGlyphCode(AEncoding, AGlyphCodes, AGlyphs[I]);
end;

procedure TdxPDFTrueTypeFont.ReadFontFileData(ADictionary: TdxPDFReaderDictionary);
var
  AData: TBytes;
  AStream: TdxPDFStream;
  AFontFileInfo: TdxFontFileInfo;
begin
  if ADictionary <> nil then
  begin
    AStream := ADictionary.GetStream(TdxPDFKeywords.FontFile2);
    if AStream <> nil then
    begin
      GenerateRegistrationName;
      FFontFileData := AStream.UncompressedData;
      AFontFileInfo := TdxFontFileHelper.GetFontFileInfo(Self, FFontFileData);
      FPatchedFontFileData := AFontFileInfo.Data;
      FGlyphMapping := AFontFileInfo.GlyphMapping;
      FGlyphRanges := AFontFileInfo.GlyphRanges;
      FEmbeddedFontEncoding := AFontFileInfo.Encoding;
    end
    else
      FOpenTypeFontFileData := ReadOpenTypeFontFileData(ADictionary, True);
      if Length(FOpenTypeFontFileData) = 0 then
      begin
        AStream := ADictionary.GetStream(TdxPDFKeywords.FontFile3);
        if AStream <> nil then
        begin
          if (AStream.Dictionary <> nil) and (AStream.Dictionary.GetString(TdxPDFKeywords.Subtype) <> 'Type1C') then
            TdxPDFUtils.Abort;
          AData := AStream.UncompressedData;
          FType1FontFileData := TdxPDFType1FontFileData.Create(Self, AData, Length(AData), 0, 0);
        end;
      end
      else
      begin
        GenerateRegistrationName;
        AFontFileInfo := TdxFontFileHelper.GetFontFileInfo(Self, FOpenTypeFontFileData);
        FPatchedFontFileData := AFontFileInfo.Data;
        FGlyphMapping := AFontFileInfo.GlyphMapping;
        FGlyphRanges := AFontFileInfo.GlyphRanges;
        FEmbeddedFontEncoding := AFontFileInfo.Encoding;
      end;
  end;
end;

{ TdxPDFDeferredTrueTypeFont }

constructor TdxPDFDeferredTrueTypeFont.Create(const ABaseFont: string; AFontDescriptor: TdxPDFFontDescriptor;
  AEncoding: TdxPDFSimpleFontEncoding);
var
  AWidths: TDoubleDynArray;
begin
  SetLength(AWidths, 256);
  inherited Create(ABaseFont, AFontDescriptor, AEncoding, 0, AWidths);
end;

{ TdxPDFType3Font }

class function TdxPDFType3Font.GetSubTypeName: string;
begin
  Result := 'Type3';
end;

function TdxPDFType3Font.GetHeightFactor: Double;
begin
  Result := FHeightFactor;
end;

function TdxPDFType3Font.GetWidthFactor: Double;
begin
  Result := FWidthFactor;
end;

procedure TdxPDFType3Font.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FMatrix := TdxPDFTransformationMatrix.Create;
  FCharProcs := TObjectDictionary<string, TdxPDFReferencedObjects>.Create([doOwnsValues]);
  FResources := nil;
end;

procedure TdxPDFType3Font.DestroySubClasses;
begin
  Resources := nil;
  FreeAndNil(FCharProcs);
  FreeAndNil(FMatrix);
  inherited DestroySubClasses;
end;

procedure TdxPDFType3Font.Read(ADictionary: TdxPDFReaderDictionary);
begin
  inherited Read(ADictionary);
  if ADictionary <> nil then
  begin
    ReadFontMatrix(ADictionary.GetArray(TdxPDFKeywords.FontMatrix));
    FWidthFactor := FMatrix.Transform(dxPointF(1, 0)).X;
    FHeightFactor := FMatrix.Transform(dxPointF(0, 1)).Y;
    Resources := TdxPDFDocumentRepositoryAccess(Repository).GetResources(ADictionary);
    ReadCharProcs(ADictionary.GetDictionary(TdxPDFKeywords.CharProcs));
  end;
end;

procedure TdxPDFType3Font.ReadProperties(ADictionary: TdxPDFReaderDictionary);
begin
  inherited ReadProperties(ADictionary);
  FFontBBox := ADictionary.GetRectangle(TdxPDFKeywords.FontDescriptorBBox);
end;

procedure TdxPDFType3Font.ReadCharProcs(ADictionary: TdxPDFReaderDictionary);
var
  AKey: string;
  AObject: TdxPDFBase;
  AStream: TdxPDFStream;
begin
  if ADictionary <> nil then
    for AKey in TdxPDFDictionaryAccess(ADictionary).Items.Keys do
    begin
      AObject := ADictionary.GetObject(AKey);
      if (AObject <> nil) and (AObject.ObjectType <> otStream) then
        AStream := Repository.GetStream(TdxPDFReference(AObject).Number)
      else
        AStream := AObject as TdxPDFStream;
      if AStream <> nil then
        FCharProcs.Add(AKey, TdxPDFType3FontContentStreamParser.Parse(AStream.UncompressedData, Repository, Resources));
    end;
end;

procedure TdxPDFType3Font.ReadFontMatrix(AArray: TdxPDFArray);
begin
  if AArray <> nil then
  begin
    FMatrix.Assign(TdxPDFUtils.ConvertToDouble(AArray[0]), TdxPDFUtils.ConvertToDouble(AArray[1]),
      TdxPDFUtils.ConvertToDouble(AArray[2]), TdxPDFUtils.ConvertToDouble(AArray[3]),
      TdxPDFUtils.ConvertToDouble(AArray[4]), TdxPDFUtils.ConvertToDouble(AArray[5]));
  end;
end;

initialization
  dxPDFFontFactory.Register(TdxPDFTrueTypeFont.GetSubTypeName, TdxPDFTrueTypeFont);
  dxPDFFontFactory.Register(TdxPDFType1Font.GetSubTypeName, TdxPDFType1Font);
  dxPDFFontFactory.Register(TdxPDFMMType1Font.GetSubTypeName, TdxPDFMMType1Font);
  dxPDFFontFactory.Register(TdxPDFType3Font.GetSubTypeName, TdxPDFType3Font);
  dxPDFFontFactory.Register(TdxPDFCIDType0Font.GetSubTypeName, TdxPDFCIDType0Font);
  dxPDFFontFactory.Register(TdxPDFCIDType2Font.GetSubTypeName, TdxPDFCIDType2Font);

finalization
  FreeAndNil(dxgPDFUnicodeConverter);
  FreeAndNil(dxgPDFRegisteredFontClasses);
  FreeAndNil(dxgPDFSimpleFontDefaultWidths);

end.

