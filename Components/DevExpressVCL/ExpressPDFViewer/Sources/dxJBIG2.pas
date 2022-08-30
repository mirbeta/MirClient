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

unit dxJBIG2;

{$I cxVer.inc}

interface

uses
  Math, Classes, SysUtils, Generics.Defaults, Generics.Collections, dxPDFBase, dxJPX;

type
  TdxJBIG2Decoder = class;
  TdxJBIG2Image = class;
  TdxJBIG2RegionSegmentInfo = class;
  TdxJBIG2SegmentData = class;
  TdxJBIG2SegmentHeader = class;
  TdxJBIG2StreamReader = class;
  TdxJBIG2SymbolDictionary = class;

  TdxJBIG2Corner = (jbcBottomLeft = 0, jbcTopLeft = 1, jbcBottomRight = 2, jbcTopRight = 3);

  IdxJBIG2Image = interface
  ['{1FD7A6D4-6354-48CC-9A4A-6106159B74A1}']
    function GetData: TBytes;
    function GetGlobalSegments: TDictionary<Integer, TdxJBIG2SegmentHeader>;
    function GetHeight: Integer;
    function GetSegments: TDictionary<Integer, TdxJBIG2SegmentHeader>;
    function GetStride: Integer;
    function GetWidth: Integer;
    procedure SetData(const AValue: TBytes);

    function CreateComposeOperatorByte(AValue: Integer; A, B: Byte): Byte;
    function CreateComposeOperator(AValue: Integer; A, B: Boolean): Boolean;
    function GetPixel(X, Y: Integer): Boolean;
    function GetPixelEx(X, Y: Integer): Integer;
    procedure Composite(AImage: IdxJBIG2Image; X, Y, AComposeOperator: Integer);
    procedure CompositeFast(AImage: IdxJBIG2Image; AComposeOperator: Integer);
    procedure CompositeOrFast(AImage: IdxJBIG2Image; X, Y: Integer);
    procedure CompositeGeneral(AImage: IdxJBIG2Image; X, Y, AComposeOperator: Integer);
    procedure Clear(AColor: Boolean);
    procedure SetDimensions(AWidth, AHeight: Integer);
    procedure SetPixel(X, Y: Integer; AValue: Boolean);

    property Data: TBytes read GetData write SetData;
    property GlobalSegments: TDictionary<Integer, TdxJBIG2SegmentHeader> read GetGlobalSegments;
    property Height: Integer read GetHeight;
    property Segments: TDictionary<Integer, TdxJBIG2SegmentHeader> read GetSegments;
    property Stride: Integer read GetStride;
    property Width: Integer read GetWidth;
  end;

  { TdxJBIG2StreamReader }

  TdxJBIG2StreamReader = class(TdxBigEndianStreamReader)
  private
    FIsStreamOwner: Boolean;
  public
    constructor Create(AStream: TStream); overload; override;
    constructor Create(const AData: TBytes); reintroduce; overload;
    destructor Destroy; override;

    function CreateAdaptiveTemplates(ALength: Integer): TList<Integer>;
  end;

  { TdxJBIG2RegionSegmentInfo }

  TdxJBIG2RegionSegmentInfo = class(TdxPDFReferencedObject)
  strict private
    FComposeOperator: Integer;
    FHeight: Integer;
    FX: Integer;
    FY: Integer;
    FWidth: Integer;
  public
    constructor Create(AHelper: TdxJBIG2StreamReader); overload;
    constructor Create(AWidth, AHeight: Integer); overload;

    property ComposeOperator: Integer read FComposeOperator;
    property Height: Integer read FHeight;
    property X: Integer read FX;
    property Y: Integer read FY;
    property Width: Integer read FWidth;
  end;

  { TdxJBIG2SegmentHeader }

  TdxJBIG2SegmentHeader = class(TInterfacedObject)
  strict private
    FData: TdxJBIG2SegmentData;
    FDataLength: Integer;
    FNumber: Integer;
    FFlags: Byte;
    FPageAssociation: Integer;
    FReferredToSegments: TList<Integer>;

    procedure InitializeReferredToSegments(AStream: TStream; AHelper: TdxJBIG2StreamReader);
  protected
    property DataLength: Integer read FDataLength;
    property Flags: Byte read FFlags;
    property PageAssociation: Integer read FPageAssociation;
    property ReferredToSegments: TList<Integer> read FReferredToSegments;
  public
    constructor Create(AStream: TStream; AImage: IdxJBIG2Image);
    destructor Destroy; override;

    function IsEOF: Boolean;
    procedure Process;

    property Data: TdxJBIG2SegmentData read FData;
    property Number: Integer read FNumber;
  end;

  { TdxJBIG2Image }

  TdxJBIG2Image = class(TInterfacedObject, IdxJBIG2Image)
  strict private
    FData: TBytes;
    FGlobalSegments: TDictionary<Integer, TdxJBIG2SegmentHeader>;
    FHeight: Integer;
    FSegments: TDictionary<Integer, TdxJBIG2SegmentHeader>;
    FStride: Integer;
    FWidth: Integer;
  private
    //IdxJBIG2Image
    function GetData: TBytes; inline;
    function GetGlobalSegments: TDictionary<Integer, TdxJBIG2SegmentHeader>;
    function GetHeight: Integer; inline;
    function GetSegments: TDictionary<Integer, TdxJBIG2SegmentHeader>;
    function GetStride: Integer; inline;
    function GetWidth: Integer; inline;
    procedure SetData(const AValue: TBytes); inline;

    function CreateComposeOperatorByte(AValue: Integer; A, B: Byte): Byte; inline;
    function CreateComposeOperator(AValue: Integer; A, B: Boolean): Boolean; inline;
    function GetPixel(X, Y: Integer): Boolean; inline;
    function GetPixelEx(X, Y: Integer): Integer; inline;
    procedure Composite(AImage: IdxJBIG2Image; X, Y, AComposeOperator: Integer);
    procedure CompositeFast(AImage: IdxJBIG2Image; AComposeOperator: Integer);
    procedure CompositeOrFast(AImage: IdxJBIG2Image; X, Y: Integer);
    procedure CompositeGeneral(AImage: IdxJBIG2Image; X, Y, AComposeOperator: Integer);
    procedure Clear(AColor: Boolean);
    procedure SetDimensions(AWidth, AHeight: Integer);
    procedure SetPixel(X, Y: Integer; AValue: Boolean);

    class function CalculateGlobalSegments(const AData: TBytes): TObjectList<TdxJBIG2SegmentHeader>;
  public
    constructor Create; overload;
    constructor Create(AWidth, AHeight: Integer); overload;
    destructor Destroy; override;

    class function Decode(const AData, AGlobalData: TBytes): TBytes;
  end;

  { TdxJBIG2HeaderInfo }

  TdxJBIG2HeaderInfo = class
  public
    DataLength: Int64;
    Flags: Byte;
    ReferredToSegments: TList<Integer>;

    constructor Create(AInfo: TdxJBIG2HeaderInfo);
    destructor Destroy; override;
  end;

  { TdxJBIG2SegmentData }

  TdxJBIG2SegmentData = class(TInterfacedObject)
  private
    FDecoderRef: TdxJBIG2Decoder;
    FNeedDestroyDecoder: Boolean;
    FImageRef: IdxJBIG2Image;
    FHeaderInfo: TdxJBIG2HeaderInfo;
    FHelper: TdxJBIG2StreamReader;
  protected
    function NeedCacheData: Boolean; virtual;
    procedure CacheData(AStream: TStream); virtual;

    procedure AddGlobalGlyphs(ASymbols: TList<IdxJBIG2Image>);

    property DecoderRef: TdxJBIG2Decoder read FDecoderRef;
    property ImageRef: IdxJBIG2Image read FImageRef;
    property HeaderInfo: TdxJBIG2HeaderInfo read FHeaderInfo;
    property Helper: TdxJBIG2StreamReader read FHelper;
  public
    constructor Create(AImage: IdxJBIG2Image); overload;
    constructor Create(AStream: TStream; AInfo: TdxJBIG2HeaderInfo; AImage: IdxJBIG2Image); overload; virtual;
    destructor Destroy; override;

    class function CreateData(AStream: TStream; AInfo: TdxJBIG2HeaderInfo; AImage: IdxJBIG2Image): TdxJBIG2SegmentData;
    procedure Process; virtual;
  end;

  { TdxJBIG2GenericRegionDecoder }

  TdxJBIG2GenericRegionDecoder = class
  strict private
    FImage: IdxJBIG2Image;
  protected
    property Image: IdxJBIG2Image read FImage;
  public
    constructor Create(AImage: IdxJBIG2Image);

    procedure Decode(ADecoder: TdxJBIG2Decoder); virtual; abstract;
  end;

  { TdxJBIG2Template0Decoder }

  TdxJBIG2Template0Decoder = class(TdxJBIG2GenericRegionDecoder)
  public
    procedure Decode(ADecoder: TdxJBIG2Decoder); override;
  end;

  { TdxJBIG2Template1Decoder }

  TdxJBIG2Template1Decoder = class(TdxJBIG2GenericRegionDecoder)
  public
    procedure Decode(ADecoder: TdxJBIG2Decoder); override;
  end;

  { TdxJBIG2Template2Decoder }

  TdxJBIG2Template2Decoder = class(TdxJBIG2GenericRegionDecoder)
  public
    procedure Decode(ADecoder: TdxJBIG2Decoder); override;
  end;

  { TdxJBIG2Template2aDecoder }

  TdxJBIG2Template2aDecoder = class(TdxJBIG2GenericRegionDecoder)
  public
    procedure Decode(ADecoder: TdxJBIG2Decoder); override;
  end;

  { TdxJBIG2Template3Decoder }

  TdxJBIG2Template3Decoder = class(TdxJBIG2GenericRegionDecoder)
  strict private
    FGBAT0: Integer;
    FGBAT1: Integer;
  public
    constructor Create(AImage: IdxJBIG2Image; AGbat0, AGbat1: Integer); reintroduce;
    procedure Decode(ADecoder: TdxJBIG2Decoder); override;
  end;

  { TdxJBIG2TPGDONDecoder }

  TdxJBIG2TPGDONDecoder = class(TdxJBIG2GenericRegionDecoder)
  strict private
    FAdaptiveTemplate: TList<Integer>;
  protected
    function CreateAdaptiveTemplates(AGBAdaptiveTemplate: TList<Integer>): TList<Integer>; virtual; abstract;
    function GetInitialContext: Integer; virtual; abstract;

    property AdaptiveTemplate: TList<Integer> read FAdaptiveTemplate;
    property InitialContext: Integer read GetInitialContext;
  public
    constructor Create(AImage: IdxJBIG2Image; AGBAdaptiveTemplate: TList<Integer>); overload;
    destructor Destroy; override;

    class function CreateDecoder(AImage: IdxJBIG2Image; AGBAdaptiveTemplate: TList<Integer>; AGBTemplate: Integer): TdxJBIG2TPGDONDecoder; overload;
    procedure CopyPrevRow(AImage: IdxJBIG2Image; Y: Integer);
    procedure Decode(ADecoder: TdxJBIG2Decoder); override;
  end;

  { TdxJBIG2TPGDON0Decoder }

  TdxJBIG2TPGDON0Decoder = class(TdxJBIG2TPGDONDecoder)
  protected
    function CreateAdaptiveTemplates(AGBAdaptiveTemplate: TList<Integer>): TList<Integer>; override;
    function GetInitialContext: Integer; override;
  end;

  { TdxJBIG2TPGDON1Decoder }

  TdxJBIG2TPGDON1Decoder = class(TdxJBIG2TPGDONDecoder)
  protected
    function CreateAdaptiveTemplates(AGBAdaptiveTemplate: TList<Integer>): TList<Integer>; override;
    function GetInitialContext: Integer; override;
  end;

  { TdxJBIG2TPGDON2Decoder }

  TdxJBIG2TPGDON2Decoder = class(TdxJBIG2TPGDONDecoder)
  protected
    function CreateAdaptiveTemplates(AGBAdaptiveTemplate: TList<Integer>): TList<Integer>; override;
    function GetInitialContext: Integer; override;
  end;

  { TdxJBIG2TPGDON3Decoder }

  TdxJBIG2TPGDON3Decoder = class(TdxJBIG2TPGDONDecoder)
  protected
    function CreateAdaptiveTemplates(AGBAdaptiveTemplate: TList<Integer>): TList<Integer>; override;
    function GetInitialContext: Integer; override;
  end;

  { TdxJBIG2TextRegion }

  TdxJBIG2TextRegion = class(TdxJBIG2SegmentData)
  strict private
    FAdaptiveTemplates: TList<Integer>;
    FCombinationOperator: Integer;
    FCorner: TdxJBIG2Corner;
    FDefaultPixel: Boolean;
    FDSValuesOffset: Integer;
    FGlyphs: TList<IdxJBIG2Image>;
    LogSBStrips: Integer;
    FRefinement: Boolean;
    FRefinementTemplate: Integer;
    FRefinementAdaptiveTemplates: TList<Integer>;
    FRegionInfo: TdxJBIG2RegionSegmentInfo;
    FStrips: Integer;
    FSymbolInstanceCount: Integer;
    FTransposed: Boolean;
  protected
    function NeedCacheData: Boolean; override;

    property RegionInfo: TdxJBIG2RegionSegmentInfo read FRegionInfo;
  public
    constructor Create(AStream: TStream; AInfo: TdxJBIG2HeaderInfo; AImage: IdxJBIG2Image); overload; override;

    constructor Create(AInfo: TdxJBIG2HeaderInfo; ADecoder: TdxJBIG2Decoder; ASymbolInstanceCount, ARefinementTemplate: Integer;
      AAt, ARAt: TList<Integer>; AGlyphs: TList<IdxJBIG2Image>; AImage: IdxJBIG2Image); overload;
    destructor Destroy; override;

    procedure Process; override;

    function RefinementDecode(ADecoder: TdxJBIG2Decoder; AImage: IdxJBIG2Image; AAdaptiveTemplates: TList<Integer>): IdxJBIG2Image;
  end;

  { TdxJBIG2RefinementRegion }

  TdxJBIG2RefinementRegion = class(TdxJBIG2SegmentData)
  strict private
    FAdaptiveTemplate: TList<Integer>;
    FDx: Integer;
    FDy: Integer;
    FReferenceGlyph: IdxJBIG2Image;
    FTemplate: Integer;
  protected
    property ReferenceGlyph: IdxJBIG2Image read FReferenceGlyph;
  public
    constructor Create(AGlyph: IdxJBIG2Image; ADecoder: TdxJBIG2Decoder; ADx, ADy, ATemplate: Integer; AAt: TList<Integer>; AImage: IdxJBIG2Image); overload;
    destructor Destroy; override;

    procedure Process; override;

    function Template0Context(X: Integer; Y: Integer): Integer;
    function Template1Context(X: Integer; Y: Integer): Integer;
  end;

  { TdxJBIG2GenericRegion }

  TdxJBIG2GenericRegion = class(TdxJBIG2SegmentData)
  strict private
    FAdaptiveTemplates: TList<Integer>;
    FMMR: Boolean;
    FFlags: Byte;
    FRegionInfo: TdxJBIG2RegionSegmentInfo;
    FTemplate: Integer;
    FUseTypicalPredictionDecoding: Boolean;
  protected
    function NeedCacheData: Boolean; override;

    property AdaptiveTemplates: TList<Integer> read FAdaptiveTemplates;
    property MMR: Boolean read FMMR;
    property RegionInfo: TdxJBIG2RegionSegmentInfo read FRegionInfo;
    property UseTypicalPredictionDecoding: Boolean read FUseTypicalPredictionDecoding write FUseTypicalPredictionDecoding;
  public
    constructor Create(AStream: TStream; AInfo: TdxJBIG2HeaderInfo; AImage: IdxJBIG2Image); overload; override;
    constructor Create(AInfo: TdxJBIG2HeaderInfo; ATemplate: Integer); overload;
    destructor Destroy; override;

    procedure Process; override;

    function CreateDecoder(AImage: IdxJBIG2Image; AAdaptiveTemplates: TList<Integer>): TdxJBIG2GenericRegionDecoder;
  end;

  { TdxJBIG2UnknownSegment }

  TdxJBIG2UnknownSegment = class(TdxJBIG2SegmentData);

  { TdxJBIG2PageInfo }

  TdxJBIG2PageInfo = class(TdxJBIG2SegmentData)
  protected
    function NeedCacheData: Boolean; override;
  public
    constructor Create(AStream: TStream; AInfo: TdxJBIG2HeaderInfo; AImage: IdxJBIG2Image); override;
  end;

  { TdxJBIG2SymbolDictionary }

  TdxJBIG2SymbolDictionary = class(TdxJBIG2SegmentData)
  strict private
    FAdaptiveTemplates: TList<Integer>;
    FExportedSymbolCount: Integer;
    FGeneratedSymbolCount: Integer;
    FGlyphs: TList<IdxJBIG2Image>;
    FFlags: Integer;
    FRefinement: Boolean;
    FRefinementTemplate: Integer;
    FSymbols: TList<IdxJBIG2Image>;
    FTemplate: Integer;
    FRefinementAdaptiveTemplates: TList<Integer>;
  protected
    property AdaptiveTemplates: TList<Integer> read FAdaptiveTemplates;
    property ExportedSymbolCount: Integer read FExportedSymbolCount;
    property GeneratedSymbolCount: Integer read FGeneratedSymbolCount;
    property RefinementAdaptiveTemplates: TList<Integer> read FRefinementAdaptiveTemplates;
    property RefinementTemplate: Integer read FRefinementTemplate;
    property Template: Integer read FTemplate;
  public
    constructor Create(AStream: TStream; AInfo: TdxJBIG2HeaderInfo; AImage: IdxJBIG2Image); override;
    destructor Destroy; override;

    procedure ProcessImage;

    property Glyphs: TList<IdxJBIG2Image> read FGlyphs;
  end;

  { TdxJBIG2ArithmeticDecoderResult }

  TdxJBIG2ArithmeticDecoderResult = record
    Code: Boolean;
    Result: Integer;
  end;

  { TdxJBIG2ArithmeticIntContext }

  TdxJBIG2ArithmeticIntContext = class
  strict private
    FIax: TBytes;
  public
    constructor Create;
    destructor Destroy; override;

    function Decode(AState: TdxArithmeticState): TdxJBIG2ArithmeticDecoderResult;
  end;

  { TdxJBIG2Decoder }

  TdxJBIG2Decoder = class(TInterfacedObject)
  strict private
    FArithState: TdxArithmeticState;
    FGb: TdxArithmeticContext;
    FGr: TdxArithmeticContext;
    FIaid: TdxArithmeticContext;
    FIdLength: Integer;
    FIardw: TdxJBIG2ArithmeticIntContext;
    FIardh: TdxJBIG2ArithmeticIntContext;
    FIardx: TdxJBIG2ArithmeticIntContext;
    FIardy: TdxJBIG2ArithmeticIntContext;
    FIadh: TdxJBIG2ArithmeticIntContext;
    FIadw: TdxJBIG2ArithmeticIntContext;
    FIaex: TdxJBIG2ArithmeticIntContext;
    FIaai: TdxJBIG2ArithmeticIntContext;
    FIadt: TdxJBIG2ArithmeticIntContext;
    FIafs: TdxJBIG2ArithmeticIntContext;
    FIads: TdxJBIG2ArithmeticIntContext;
    FIait: TdxJBIG2ArithmeticIntContext;
    FIari: TdxJBIG2ArithmeticIntContext;
    FLastCode: Boolean;

    function Decode(ACtx: TdxJBIG2ArithmeticIntContext): Integer; inline;
    function FindLength(L: Integer): Integer; inline;
  public
    constructor Create(AHelper: TdxJBIG2StreamReader; AMaxId: Integer); overload;
    constructor Create(AHelper: TdxJBIG2StreamReader; AIdLength, AGbLength, AGrLength: Integer); overload;
    destructor Destroy; override;

    function DecodeAI: Integer; inline;
    function DecodeDH: Integer; inline;
    function DecodeDS: Integer; inline;
    function DecodeDT: Integer; inline;
    function DecodeDW: Integer; inline;
    function DecodeEX: Integer; inline;
    function DecodeFS: Integer; inline;
    function DecodeID: Integer; inline;
    function DecodeIT: Integer; inline;
    function DecodeGB(AContext: Integer): Boolean; inline;
    function DecodeGR(AContext: Integer): Boolean; inline;
    function DecodeRDH: Integer; inline;
    function DecodeRDX: Integer; inline;
    function DecodeRDY: Integer; inline;
    function DecodeRDW: Integer; inline;
    function DecodeRI: Integer; inline;

    property LastCode: Boolean read FLastCode;
  end;

implementation

uses
  dxCore, cxVariants, dxPDFUtils;

{ TdxJBIG2StreamReader }

constructor TdxJBIG2StreamReader.Create(AStream: TStream);
begin
  inherited Create(AStream);
  FIsStreamOwner := False;
end;

constructor TdxJBIG2StreamReader.Create(const AData: TBytes);
begin
  Create(TBytesStream.Create(AData));
  FIsStreamOwner := True;
end;

destructor TdxJBIG2StreamReader.Destroy;
begin
  if FIsStreamOwner then
    FreeStream;
  inherited Destroy;
end;

function TdxJBIG2StreamReader.CreateAdaptiveTemplates(ALength: Integer): TList<Integer>;
var
  I: Integer;
begin
  Result := TList<Integer>.Create;
  for I := 0 to ALength - 1 do
    Result.Add(ShortInt(ReadByte));
end;

{ TdxJBIG2RegionSegmentInfo }

constructor TdxJBIG2RegionSegmentInfo.Create(AWidth: Integer; AHeight: Integer);
begin
  inherited Create;
  FWidth := AWidth;
  FHeight := AHeight;
  FX := 0;
  FY := 0;
  FComposeOperator := 0;
end;

constructor TdxJBIG2RegionSegmentInfo.Create(AHelper: TdxJBIG2StreamReader);
begin
  Create;
  FWidth := AHelper.ReadInt32;
  FHeight := AHelper.ReadInt32;
  FX := AHelper.ReadInt32;
  FY := AHelper.ReadInt32;
  FComposeOperator := AHelper.ReadByte and $7;
end;

{ TdxJBIG2SegmentHeader }

constructor TdxJBIG2SegmentHeader.Create(AStream: TStream; AImage: IdxJBIG2Image);
var
  AHelper: TdxJBIG2StreamReader;
  AInfo: TdxJBIG2HeaderInfo;
begin
  inherited Create;
  FReferredToSegments := TList<Integer>.Create;

  AHelper := TdxJBIG2StreamReader.Create(AStream);
  try
    FNumber := AHelper.ReadInt32;
    FFlags := AHelper.ReadByte;
    InitializeReferredToSegments(AStream, AHelper);
    FPageAssociation := AHelper.ReadInt(IfThen((Flags and 64) = 1, 4, 1));
    FDataLength := AHelper.ReadInt32;

    AInfo := TdxJBIG2HeaderInfo.Create(nil);
    AInfo.ReferredToSegments.AddRange(FReferredToSegments);
    AInfo.Flags := FFlags;
    AInfo.DataLength := FDataLength;
    try
      FData := TdxJBIG2SegmentData.CreateData(AStream, AInfo, AImage);
    finally
      AInfo.Free;
    end;
  finally
    AHelper.Free;
  end;
end;

destructor TdxJBIG2SegmentHeader.Destroy;
begin
  FreeAndNil(FData);
  FreeAndNil(FReferredToSegments);
  inherited Destroy;
end;

function TdxJBIG2SegmentHeader.IsEOF: Boolean;
begin
  Result := (Flags and 63) = 51;
end;

procedure TdxJBIG2SegmentHeader.Process;
begin
  FData.Process;
end;

procedure TdxJBIG2SegmentHeader.InitializeReferredToSegments(AStream: TStream; AHelper: TdxJBIG2StreamReader);
var
  ASlant: Byte;
  I, ASegmentCount, ASegmentSize: Integer;
begin
  ASlant := AHelper.ReadByte;
  if (ASlant and 224) = 224 then
  begin
    AStream.Position := AStream.Position - 1;
    ASegmentCount := AHelper.ReadInt32 and $1FFFFFF;
    AStream.Position := AStream.Position + (ASegmentCount + 1) div 8;
  end
  else
    ASegmentCount := ASlant shr 5;
  ASegmentSize := IfThen(Number <= 256, 1, IfThen(Number <= 65536, 2, 4));
  if ASegmentCount <= 1000000 then
    for I := 0 to ASegmentCount - 1 do
      ReferredToSegments.Add(AHelper.ReadInt(ASegmentSize));
end;

{ TdxJBIG2Image }

constructor TdxJBIG2Image.Create;
begin
  inherited Create;
  FSegments := TDictionary<Integer, TdxJBIG2SegmentHeader>.Create;
  FGlobalSegments := TDictionary<Integer, TdxJBIG2SegmentHeader>.Create;
end;

constructor TdxJBIG2Image.Create(AWidth, AHeight: Integer);
begin
  Create;
  FWidth := AWidth;
  FHeight := AHeight;
  FStride := ((FWidth - 1) shr 3) + 1;
  SetLength(FData, FStride * FHeight);
end;

destructor TdxJBIG2Image.Destroy;
begin
  FreeAndNil(FSegments);
  FreeAndNil(FGlobalSegments);
  inherited Destroy;
end;

class function TdxJBIG2Image.CalculateGlobalSegments(const AData: TBytes): TObjectList<TdxJBIG2SegmentHeader>;

  procedure ReadHeader(AStream: TBytesStream; AImage: IdxJBIG2Image; AHeaders: TObjectList<TdxJBIG2SegmentHeader>);
  var
    AHeader: TdxJBIG2SegmentHeader;
  begin
    AHeader := TdxJBIG2SegmentHeader.Create(AStream, AImage);
    AImage.GlobalSegments.Add(AHeader.Number, AHeader);
    AHeaders.Add(AHeader);
  end;

  procedure PopulateHeaders(AHeaders: TObjectList<TdxJBIG2SegmentHeader>);
  var
    AImage: IdxJBIG2Image;
    AStream: TBytesStream;
  begin
    AImage := TdxJBIG2Image.Create;
    AStream := TBytesStream.Create(AData);
    try
      repeat
        try
          ReadHeader(AStream, AImage, Result);
        except
          Break;
        end;
      until not (not Result.Last.IsEOF and (AStream.Position <= AStream.Size - 1));
    finally
      AStream.Free;
    end;
  end;

var
  AHeader: TdxJBIG2SegmentHeader;
begin
  Result := TObjectList<TdxJBIG2SegmentHeader>.Create;
  if Length(AData) <> 0 then
  begin
    PopulateHeaders(Result);
    for AHeader in Result do
      AHeader.Process
  end;
end;

class function TdxJBIG2Image.Decode(const AData, AGlobalData: TBytes): TBytes;
var
  I: Integer;
  AImage: IdxJBIG2Image;
  AStream: TBytesStream;
  AHeader: TdxJBIG2SegmentHeader;
  AGlobalSegments: TObjectList<TdxJBIG2SegmentHeader>;
  AHeaderList: TObjectList<TdxJBIG2SegmentHeader>;
begin
  AImage := TdxJBIG2Image.Create;
  AGlobalSegments := CalculateGlobalSegments(AGlobalData);
  try
    try
      if AGlobalSegments.Count > 0 then
        for AHeader in AGlobalSegments do
          AImage.GlobalSegments.Add(AHeader.Number, AHeader);

      AStream := TBytesStream.Create(AData);
      AHeaderList := TObjectList<TdxJBIG2SegmentHeader>.Create;
      try
        try
          repeat
            AHeader := TdxJBIG2SegmentHeader.Create(AStream, AImage);
            AImage.GlobalSegments.Add(AHeader.Number, AHeader);
            AHeaderList.Add(AHeader);
          until not(not AHeader.IsEOF and (AStream.Position <= AStream.Size - 1));
        finally
          AStream.Free;
        end;
        for AHeader in AHeaderList do
          AHeader.Process;
      finally
        AHeaderList.Free;
      end;
      for I := 0 to Length(AImage.Data) - 1 do
        AImage.Data[I] := 255 - AImage.Data[I];
      Result := AImage.Data;
    except
      Result := nil;
    end;
  finally
    AGlobalSegments.Free;
  end;
end;

function TdxJBIG2Image.CreateComposeOperatorByte(AValue: Integer; A, B: Byte): Byte;
begin
  case AValue of
    0:
      Result := A or B;
    1:
      Result := A and B;
    2:
      Result := A xor B;
    3:
      Result := not(A xor B);
    4:
      Result := A;
  else
    Result := 0;
    TdxPDFUtils.RaiseTestException;
  end;
end;

function TdxJBIG2Image.CreateComposeOperator(AValue: Integer; A, B: Boolean): Boolean;
begin
  case AValue of
    0:
      Result := A or B;
    1:
      Result := A and B;
    2:
      Result := A xor B;
    3:
      Result := A = B;
    4:
      Result := A;
  else
    Result := False;
    TdxPDFUtils.RaiseTestException;
  end;
end;

function TdxJBIG2Image.GetData: TBytes;
begin
  Result := FData;
end;

function TdxJBIG2Image.GetGlobalSegments: TDictionary<Integer, TdxJBIG2SegmentHeader>;
begin
  Result := FGlobalSegments;
end;

function TdxJBIG2Image.GetHeight: Integer;
begin
  Result := FHeight;
end;

function TdxJBIG2Image.GetPixel(X, Y: Integer): Boolean;
var
  B, ABit: Integer;
begin
  B := (X shr 3) + Y * FStride;
  ABit := 7 - (X and 7);
  if ((X < 0)) or ((X >= FWidth)) or ((Y < 0)) or ((Y >= FHeight)) then
    Exit(False);
  Result := ((FData[B] shr ABit) and 1) <> 0;
end;

function TdxJBIG2Image.GetPixelEx(X, Y: Integer): Integer;
begin
  Result := IfThen(GetPixel(X, Y), 1, 0);
end;

function TdxJBIG2Image.GetSegments: TDictionary<Integer, TdxJBIG2SegmentHeader>;
begin
  Result := FSegments;
end;

function TdxJBIG2Image.GetStride: Integer;
begin
  Result := FStride;
end;

function TdxJBIG2Image.GetWidth: Integer;
begin
  Result := FWidth;
end;

procedure TdxJBIG2Image.SetData(const AValue: TBytes);
begin
  FData := AValue;
end;

procedure TdxJBIG2Image.Composite(AImage: IdxJBIG2Image; X, Y, AComposeOperator: Integer);
begin
  if (X = 0) and (Y = 0) and (AImage.Width = FWidth) and (AImage.Height = FHeight) then
    CompositeFast(AImage, AComposeOperator)
  else
    if AComposeOperator <> 0 then
      CompositeGeneral(AImage, X, Y, AComposeOperator)
    else
      CompositeOrFast(AImage, X, Y);
end;

procedure TdxJBIG2Image.CompositeFast(AImage: IdxJBIG2Image; AComposeOperator: Integer);
var
  I: Integer;
begin
  for I := 0 to Length(FData) - 1 do
    FData[I] := CreateComposeOperatorByte(AComposeOperator, FData[I], AImage.Data[I]);
end;

procedure TdxJBIG2Image.CompositeOrFast(AImage: IdxJBIG2Image; X, Y: Integer);
var
  AMask, ARightMask, W, H, ASs, S, ALeftByte, ARightByte, AShift, ADd, D, J, I: Integer;
  AOverlap: Boolean;
begin
  W := AImage.Width;
  H := AImage.Height;
  ASs := 0;
  S := ASs;
  if X < 0 then
  begin
    Inc(W, X);
    X := 0;
  end;
  if Y < 0 then
  begin
    Inc(H, Y);
    Y := 0;
  end;
  W := IfThen(X + W < FWidth, W, FWidth - X);
  H := IfThen(Y + H < FHeight, H, FHeight - Y);
  if ((W <= 0)) or ((H <= 0)) then
    Exit;
  ALeftByte := X shr 3;
  ARightByte := (X + W - 1) shr 3;
  AShift := X and 7;
  ADd := Y * FStride + ALeftByte;
  D := ADd;
  if (D < 0) or (ALeftByte > FStride) or (H * FStride < 0) or (D - ALeftByte + H * FStride > FHeight * FStride) then
    TdxPDFUtils.RaiseTestException;
  if ALeftByte = ARightByte then
  begin
    AMask := $100 - ($100 shr W);
    for J := 0 to H - 1 do
    begin
      FData[D] := FData[D] or ((AImage.Data[S] and AMask) shr AShift);
      Inc(D, FStride);
      Inc(S, AImage.Stride);
    end;
  end
  else
    if AShift = 0 then
    begin
      ARightMask := IfThen((W and 7) <> 0, $100 - (1 shl (8 - (W and 7))), $FF);
      for J := 0 to H - 1 do
      begin
        for I := ALeftByte to ARightByte - 1 do
        begin
          FData[D] := FData[D] or AImage.Data[S];
          Inc(D);
          Inc(S);
        end;
        FData[D] := FData[D] or Byte(AImage.Data[S] and ARightMask);
        Inc(Add, FStride);
        D := ADd;
        Inc(ASs, AImage.Stride);
        S := ASs;
      end;
    end
    else
    begin
      AOverlap := ((W + 7) shr 3) < (((X + W + 7) shr 3) - (X shr 3));
      AMask := $100 - (1 shl AShift);
      ARightMask := IfThen(AOverlap, ($100 - ($100 shr ((X + W) and 7))) shr (8 - AShift), $100 - ($100 shr (W and 7)));
      for J := 0 to H - 1 do
      begin
        FData[D] := FData[D] or Byte(((AImage.Data[S] and AMask) shr AShift));
        Inc(D);
        for I := ALeftByte to ARightByte - 2 do
        begin
          FData[D] := FData[D] or Byte(((AImage.Data[S] and not AMask) shl (8 - AShift)));
          FData[D] := FData[D] or Byte(((AImage.Data[S + 1] and AMask) shr AShift));
          Inc(S);
          Inc(D);
        end;
        if AOverlap then
          FData[D] := FData[D] or Byte(((AImage.Data[S] and ARightMask) shl (8 - AShift)))
        else
          FData[D] := FData[D] or (Byte(((((AImage.Data[S] and not AMask) shl (8 - AShift))) or ((AImage.Data[S + 1] and ARightMask) shr AShift))));
        Inc(Add, FStride);
        D := ADd;
        Inc(ASs, AImage.Stride);
        S := ASs;
      end;
    end;
end;

procedure TdxJBIG2Image.CompositeGeneral(AImage: IdxJBIG2Image; X, Y, AComposeOperator: Integer);
var
  ASw, ASh, ASx, ASy, J, I: Integer;
begin
  ASw := AImage.Width;
  ASh := AImage.Height;
  ASx := 0;
  ASy := 0;
  if X < 0 then
  begin
    Inc(ASx, -X);
    Dec(ASw, -X);
    X := 0;
  end;
  if Y < 0 then
  begin
    Inc(ASy, -Y);
    Dec(ASh, -Y);
    Y := 0;
  end;
  if X + ASw >= FWidth then
    ASw := FWidth - X;
  if Y + ASh >= FHeight then
    ASh := FHeight - Y;

  for J := 0 to ASh - 1 do
    for I := 0 to ASw - 1 do
      SetPixel(I + X, J + Y, CreateComposeOperator(AComposeOperator, AImage.GetPixel(I + ASx, J + ASy), GetPixel(I + X, J + Y)));
end;

procedure TdxJBIG2Image.Clear(AColor: Boolean);
var
  I: Integer;
begin
  for I := 0 to Length(FData) - 1 do
    FData[I] := IfThen(AColor, $FF, $00);
end;

procedure TdxJBIG2Image.SetDimensions(AWidth, AHeight: Integer);
begin
  FWidth := AWidth;
  FHeight := AHeight;
  FStride := ((FWidth - 1) shr 3) + 1;
  SetLength(FData, FStride * AHeight);
end;

procedure TdxJBIG2Image.SetPixel(X, Y: Integer; AValue: Boolean);
var
  AIndex, ABit, AMask, AScratch: Integer;
begin
  if (X < 0) or (X >= FWidth) or (Y < 0) or (Y >= FHeight) then
    Exit;
  AIndex := (X shr 3) + Y * FStride;
  ABit := 7 - (X and 7);
  AMask := (1 shl ABit) xor $FF;
  AScratch := FData[AIndex] and AMask;
  FData[AIndex] := AScratch or IfThen(AValue, 1 shl ABit, 0 shl ABit);
end;

{ TdxJBIG2SymbolDictionary }

constructor TdxJBIG2SymbolDictionary.Create(AStream: TStream; AInfo: TdxJBIG2HeaderInfo; AImage: IdxJBIG2Image);
var
  ASdatLength: Integer;
begin
  inherited Create(AStream, AInfo, AImage);

  FSymbols := TList<IdxJBIG2Image>.Create;
  FGlyphs := TList<IdxJBIG2Image>.Create;

  FFlags := Helper.ReadInt16;
  if (FFlags and 1) <> 0 then
    TdxPDFUtils.Abort;

  FRefinement := ((FFlags shr 1) and 1) <> 0;
  FTemplate := (FFlags shr 10) and 3;
  FRefinementTemplate := (FFlags shr 12) and 1;

  if (FFlags and $000C <> 0) or (FFlags and $0030 <> 0) or (FFlags and $0080 <> 0) or (FFlags and $0100 <> 0) then
    TdxPDFUtils.RaiseTestException;

  ASdatLength := IfThen(FTemplate = 0, 8, 2);

  FAdaptiveTemplates := Helper.CreateAdaptiveTemplates(ASdatLength);
  if FRefinement and (FRefinementTemplate = 0) then
    FRefinementAdaptiveTemplates := Helper.CreateAdaptiveTemplates(4);

  FExportedSymbolCount := Helper.ReadInt32;
  FGeneratedSymbolCount := Helper.ReadInt32;

  ProcessImage;
end;

destructor TdxJBIG2SymbolDictionary.Destroy;
begin
  FreeAndNil(FRefinementAdaptiveTemplates);
  FreeAndNil(FAdaptiveTemplates);
  FreeAndNil(FSymbols);
  FreeAndNil(FGlyphs);
  inherited Destroy;
end;

procedure TdxJBIG2SymbolDictionary.ProcessImage;
var
  ALimit, ASymwidth, AHcheight, AHcdh, ADw, ARefaggninst, AId, ARdx, ARdy, AIi, AZeroLength, AExRunLength, K: Integer;
  ARegion: TdxJBIG2GenericRegion;
  ATextRegion: TdxJBIG2TextRegion;
  ARefinement: TdxJBIG2RefinementRegion;
  AExFlag: Boolean;

  ARegionDecoder: TdxJBIG2GenericRegionDecoder;
  AGlyph: IdxJBIG2Image;
  ADecoder: TdxJBIG2Decoder;
begin
  AddGlobalGlyphs(FSymbols);

  ALimit := FSymbols.Count + GeneratedSymbolCount;
  AHcheight := 0;

  ADecoder := TdxJBIG2Decoder.Create(Helper, ALimit);
  try
    while FSymbols.Count < ALimit do
    begin
      AHcdh := ADecoder.DecodeDH;
      Inc(AHcheight, AHcdh);
      ASymwidth := 0;

      if AHcheight < 0 then
        TdxPDFUtils.RaiseTestException;

      ADw := ADecoder.DecodeDW;
      while not ADecoder.LastCode do
      begin
        if FSymbols.Count >= ALimit then
          TdxPDFUtils.RaiseTestException;

        Inc(ASymwidth, ADw);
        if ASymwidth <= 0 then
          TdxPDFUtils.RaiseTestException;

        AGlyph := TdxJBIG2Image.Create(ASymwidth, AHcheight);
        if not FRefinement then
        begin
          ARegion := TdxJBIG2GenericRegion.Create(HeaderInfo, FTemplate);
          ARegionDecoder := ARegion.CreateDecoder(AGlyph, FAdaptiveTemplates);
          try
            ARegionDecoder.Decode(ADecoder);
          finally
            ARegionDecoder.Free;
            ARegion.Free;
          end;
        end
        else
        begin
          ARefaggninst := ADecoder.DecodeAI;
          if (ADecoder.LastCode) or (ARefaggninst <= 0) then
            TdxPDFUtils.RaiseTestException;
          if ARefaggninst > 1 then
          begin
            ATextRegion := TdxJBIG2TextRegion.Create(HeaderInfo, ADecoder, ARefaggninst,
              FRefinementTemplate, FAdaptiveTemplates, FRefinementAdaptiveTemplates, FSymbols, AGlyph);
            try
              ATextRegion.Process;
            finally
              ATextRegion.Free;
            end;
          end
          else
          begin
            AId := ADecoder.DecodeID;
            ARdx := ADecoder.DecodeRDX;
            ARdy := ADecoder.DecodeRDY;
            if AId >= FSymbols.Count then
              TdxPDFUtils.RaiseTestException;
            ARefinement := TdxJBIG2RefinementRegion.Create(FSymbols[AId], ADecoder, ARdx, ARdy, FRefinementTemplate,
              FRefinementAdaptiveTemplates, AGlyph);
            try
              ARefinement.Process;
            finally
              ARefinement.Free;
            end;
          end;
        end;
        FSymbols.Add(AGlyph);
        ADw := ADecoder.DecodeDW;
      end;
    end;

    AIi := 0;
    AExFlag := False;
    AZeroLength := 0;
    while AIi < ALimit do
    begin
      AExRunLength := ADecoder.DecodeEX;
      if AExRunLength > 0 then
        AZeroLength := 0
      else
        AZeroLength := AZeroLength + 1;
      if (ADecoder.LastCode) or (AExRunLength > ALimit - AIi) or (AExRunLength < 0) or (AZeroLength > 4) or
        AExFlag and (AExRunLength > FExportedSymbolCount - FGlyphs.Count) then
        TdxPDFUtils.RaiseTestException;
      for K := 0 to AExRunLength - 1 do
      begin
        if AExFlag then
          FGlyphs.Add(FSymbols[AIi]);
        Inc(AIi);
      end;
      AExFlag := not AExFlag;
    end;
  finally
    ADecoder.Free;
  end;
end;

{ TdxJBIG2SegmentData }

constructor TdxJBIG2SegmentData.Create(AImage: IdxJBIG2Image);
begin
  inherited Create;
  FImageRef := AImage;
  FDecoderRef := nil;
  FHeaderInfo := nil;
end;

constructor TdxJBIG2SegmentData.Create(AStream: TStream; AInfo: TdxJBIG2HeaderInfo; AImage: IdxJBIG2Image);
begin
  Create(AImage);
  FHeaderInfo := TdxJBIG2HeaderInfo.Create(AInfo);
  FHelper := TdxJBIG2StreamReader.Create(AStream);
  if NeedCacheData then
    CacheData(AStream);
end;

destructor TdxJBIG2SegmentData.Destroy;
begin
  FreeAndNil(FHeaderInfo);
  FImageRef := nil;
  FreeAndNil(FHelper);
  inherited Destroy;
end;

class function TdxJBIG2SegmentData.CreateData(AStream: TStream; AInfo: TdxJBIG2HeaderInfo; AImage: IdxJBIG2Image): TdxJBIG2SegmentData;
begin
  case (AInfo.Flags and 63) of
    0:
      Result := TdxJBIG2SymbolDictionary.Create(AStream, AInfo, AImage);
    4, 6, 7:
      Result := TdxJBIG2TextRegion.Create(AStream, AInfo, AImage);
    38, 39:
      Result := TdxJBIG2GenericRegion.Create(AStream, AInfo, AImage);
    40, 42, 43:
      Result := TdxJBIG2RefinementRegion.Create(AStream, AInfo, AImage);
    48:
      Result := TdxJBIG2PageInfo.Create(AStream, AInfo, AImage);
  else
    Result := TdxJBIG2UnknownSegment.Create(AStream, AInfo, AImage);
  end;
end;

procedure TdxJBIG2SegmentData.Process;
begin
// do nothing
end;

function TdxJBIG2SegmentData.NeedCacheData: Boolean;
begin
  Result := True;
end;

procedure TdxJBIG2SegmentData.CacheData(AStream: TStream);
var
  ALength: Int64;
  AData: TBytes;
begin
  FHelper.Free;
  ALength := HeaderInfo.DataLength;
  if (ALength < 0) or (ALength > 1000000) then
    ALength := AStream.Size - AStream.Position;
  SetLength(AData, ALength);
  AStream.Read(AData[0], ALength);
  FHelper := TdxJBIG2StreamReader.Create(AData);
end;

procedure TdxJBIG2SegmentData.AddGlobalGlyphs(ASymbols: TList<IdxJBIG2Image>);
var
  I: Integer;
  AHeader: TdxJBIG2SegmentHeader;
begin
  for I := 0 to HeaderInfo.ReferredToSegments.Count - 1 do
  begin
    if not ImageRef.Segments.TryGetValue(HeaderInfo.ReferredToSegments[I], AHeader) then
      ImageRef.GlobalSegments.TryGetValue(HeaderInfo.ReferredToSegments[I], AHeader);
    if (AHeader <> nil) and (AHeader.Data is TdxJBIG2SymbolDictionary) then
      ASymbols.AddRange(TdxJBIG2SymbolDictionary(AHeader.Data).Glyphs);
  end;
end;

{ TdxJBIG2GenericRegionDecoder }

constructor TdxJBIG2GenericRegionDecoder.Create(AImage: IdxJBIG2Image);
begin
  inherited Create;
  FImage := AImage;
end;

{ TdxJBIG2TPGDONDecoder }

constructor TdxJBIG2TPGDONDecoder.Create(AImage: IdxJBIG2Image; AGBAdaptiveTemplate: TList<Integer>);
begin
  inherited Create(AImage);
  FAdaptiveTemplate := CreateAdaptiveTemplates(AGBAdaptiveTemplate);
end;

destructor TdxJBIG2TPGDONDecoder.Destroy;
begin
  FreeAndNil(FAdaptiveTemplate);
  inherited Destroy;
end;

class function TdxJBIG2TPGDONDecoder.CreateDecoder(AImage: IdxJBIG2Image; AGBAdaptiveTemplate: TList<Integer>;
  AGBTemplate: Integer): TdxJBIG2TPGDONDecoder;
begin
  case AGBTemplate of
    0:
      Exit(TdxJBIG2TPGDON0Decoder.Create(AImage, AGBAdaptiveTemplate));
    1:
      Exit(TdxJBIG2TPGDON1Decoder.Create(AImage, AGBAdaptiveTemplate));
    2:
      Exit(TdxJBIG2TPGDON2Decoder.Create(AImage, AGBAdaptiveTemplate));
    3:
      Exit(TdxJBIG2TPGDON3Decoder.Create(AImage, AGBAdaptiveTemplate));
  end;
  TdxPDFUtils.RaiseTestException;
  Result := nil;
end;

procedure TdxJBIG2TPGDONDecoder.CopyPrevRow(AImage: IdxJBIG2Image; Y: Integer);
var
  ASrc: Integer;
  AData: TBytes;
begin
  if Y = 0 then
    Exit;
  ASrc := (Y - 1) * AImage.Stride;
  AData := AImage.Data;
  TdxPDFUtils.CopyData(AData, ASrc, AData, ASrc + AImage.Stride, AImage.Stride);
end;

procedure TdxJBIG2TPGDONDecoder.Decode(ADecoder: TdxJBIG2Decoder);
var
  AImage: IdxJBIG2Image;
  AAdaptiveTemplate: TList<Integer>;
  ALtp, Y, X, AContext, I, J: Integer;
  ABit: Boolean;
begin
  AImage := Image;
  AAdaptiveTemplate := AdaptiveTemplate;
  ALtp := 0;
  for Y := 0 to AImage.Height - 1 do
  begin
    ABit := ADecoder.DecodeGB(InitialContext);
    ALtp := ALtp xor IfThen(ABit, 1, 0);
    if ALtp = 0 then
      for X := 0 to AImage.Width - 1 do
      begin
        AContext := 0;
        I := 0;
        J := 0;
        while I < AAdaptiveTemplate.Count do
        begin
          AContext := AContext or (AImage.GetPixelEx(X + AAdaptiveTemplate[I], Y + AdaptiveTemplate[I + 1]) shl J);
          Inc(I, 2);
          Inc(J);
        end;
        ABit := ADecoder.DecodeGB(AContext);
        AImage.SetPixel(X, Y, ABit);
      end
    else
      CopyPrevRow(AImage, Y);
  end;
end;

{ TdxJBIG2TPGDON0Decoder }

function TdxJBIG2TPGDON0Decoder.CreateAdaptiveTemplates(AGBAdaptiveTemplate: TList<Integer>): TList<Integer>;
begin
  Result := TList<Integer>.Create;
  Result.Add(-1);
  Result.Add(0);
  Result.Add(-2);
  Result.Add(0);
  Result.Add(-3);
  Result.Add(0);
  Result.Add(-4);
  Result.Add(0);
  Result.Add(AGBAdaptiveTemplate[0]);
  Result.Add(AGBAdaptiveTemplate[1]);
  Result.Add(2);
  Result.Add(-1);
  Result.Add(1);
  Result.Add(-1);
  Result.Add(0);
  Result.Add(-1);
  Result.Add(-1);
  Result.Add(-1);
  Result.Add(-2);
  Result.Add(-1);
  Result.Add(AGBAdaptiveTemplate[2]);
  Result.Add(AGBAdaptiveTemplate[3]);
  Result.Add(AGBAdaptiveTemplate[4]);
  Result.Add(AGBAdaptiveTemplate[5]);
  Result.Add(1);
  Result.Add(-2);
  Result.Add(0);
  Result.Add(-2);
  Result.Add(-1);
  Result.Add(-2);
  Result.Add(AGBAdaptiveTemplate[6]);
  Result.Add(AGBAdaptiveTemplate[7]);
end;

function TdxJBIG2TPGDON0Decoder.GetInitialContext: Integer;
begin
  Result := $9B25;
end;

{ TdxJBIG2TPGDON1Decoder }

function TdxJBIG2TPGDON1Decoder.CreateAdaptiveTemplates(AGBAdaptiveTemplate: TList<Integer>): TList<Integer>;
begin
  Result := TList<Integer>.Create;
  Result.Add(-1);
  Result.Add(0);
  Result.Add(-2);
  Result.Add(0);
  Result.Add(-3);
  Result.Add(0);
  Result.Add(AGBAdaptiveTemplate[0]);
  Result.Add(AGBAdaptiveTemplate[1]);
  Result.Add(2);
  Result.Add(-1);
  Result.Add(1);
  Result.Add(-1);
  Result.Add(0);
  Result.Add(-1);
  Result.Add(-1);
  Result.Add(-1);
  Result.Add(-2);
  Result.Add(-1);
  Result.Add(2);
  Result.Add(-2);
  Result.Add(1);
  Result.Add(-2);
  Result.Add(0);
  Result.Add(-2);
  Result.Add(-1);
  Result.Add(-2);
end;

function TdxJBIG2TPGDON1Decoder.GetInitialContext: Integer;
begin
  Result := $795;
end;

{ TdxJBIG2TPGDON2Decoder }

function TdxJBIG2TPGDON2Decoder.CreateAdaptiveTemplates(AGBAdaptiveTemplate: TList<Integer>): TList<Integer>;
begin
  Result := TList<Integer>.Create;
  Result.Add(-1);
  Result.Add(0);
  Result.Add(-2);
  Result.Add(0);
  Result.Add(AGBAdaptiveTemplate[0]);
  Result.Add(AGBAdaptiveTemplate[1]);
  Result.Add(1);
  Result.Add(-1);
  Result.Add(0);
  Result.Add(-1);
  Result.Add(-1);
  Result.Add(-1);
  Result.Add(-2);
  Result.Add(-1);
  Result.Add(1);
  Result.Add(-2);
  Result.Add(0);
  Result.Add(-2);
  Result.Add(-1);
  Result.Add(-2);
end;

function TdxJBIG2TPGDON2Decoder.GetInitialContext: Integer;
begin
  Result := $E5;
end;

{ TdxJBIG2TPGDON3Decoder }

function TdxJBIG2TPGDON3Decoder.CreateAdaptiveTemplates(AGBAdaptiveTemplate: TList<Integer>): TList<Integer>;
begin
  Result := TList<Integer>.Create;
  Result.Add(-1);
  Result.Add(0);
  Result.Add(-2);
  Result.Add(0);
  Result.Add(-3);
  Result.Add(0);
  Result.Add(-4);
  Result.Add(0);
  Result.Add(AGBAdaptiveTemplate[0]);
  Result.Add(AGBAdaptiveTemplate[1]);
  Result.Add(1);
  Result.Add(-1);
  Result.Add(0);
  Result.Add(-1);
  Result.Add(-1);
  Result.Add(-1);
  Result.Add(-2);
  Result.Add(-1);
  Result.Add(-3);
  Result.Add(-1);
end;

function TdxJBIG2TPGDON3Decoder.GetInitialContext: Integer;
begin
  Result := $195;
end;

{ TdxJBIG2Template0Decoder }

procedure TdxJBIG2Template0Decoder.Decode(ADecoder: TdxJBIG2Decoder);
var
  AImage: IdxJBIG2Image;
  AGbw, AGbh, ARowStride, AGbregLine, Y, ALine1, ALine2, APaddedWidth, AContext, X, AMinorWidth, AXSlide, AXMinor,
    AMinorDif: Integer;
  AData: TBytes;
  AResult, ABit: Byte;
begin
  AImage := Image;
  AGbw := AImage.Width;
  AGbh := AImage.Height;
  ARowStride := AImage.Stride;
  AGbregLine := 0;
  AData := AImage.Data;
  if AGbw <= 0 then
    TdxPDFUtils.RaiseTestException;
  for Y := 0 to AGbh - 1 do
  begin
    if Y >= 1 then
      ALine1 := AData[AGbregLine - ARowStride]
    else
      ALine1 := 0;

    if Y >= 2 then
      ALine2 := AData[AGbregLine - ARowStride * 2] shl 6
    else
      ALine2 := 0;

    APaddedWidth := (AGbw + 7) and -8;
    AContext := (ALine1 and $7F0) or (ALine2 and $F800);

    X := 0;
    while X < APaddedWidth do
    begin
      AResult := 0;
      AMinorWidth := IfThen((AGbw - X) > 8, 8, AGbw - X);

      AXSlide := X shr 3;

      if Y >= 1 then
        ALine1 := (ALine1 shl 8) or IfThen((X + 8) < AGbw, AData[AGbregLine - ARowStride + AXSlide + 1], 0);
      if Y >= 2 then
        ALine2 := (ALine2 shl 8) or IfThen((X + 8) < AGbw, AData[AGbregLine - ARowStride * 2 + AXSlide + 1] shl 6, 0);

      for AXMinor := 0 to AMinorWidth - 1 do
      begin
        AMinorDif := 7 - AXMinor;
        ABit := IfThen(ADecoder.DecodeGB(AContext), 1, 0);
        AResult := AResult or (ABit shl AMinorDif);
        AContext := ((AContext and $7BF7) shl 1) or ABit or (ALine1 shr AMinorDif) and $10 or (ALine2 shr AMinorDif) and $800;
      end;
      AData[AGbregLine + AXSlide] := AResult;
      Inc(X, 8);
    end;
    Inc(AGbregLine, ARowStride);
  end;
end;

{ TdxJBIG2Template1Decoder }

procedure TdxJBIG2Template1Decoder.Decode;
begin
  TdxPDFUtils.RaiseTestException;
end;

{ TdxJBIG2Template2Decoder }

procedure TdxJBIG2Template2Decoder.Decode(ADecoder: TdxJBIG2Decoder);
var
  AImage: IdxJBIG2Image;
  AGbw, AGbh, ARowStride, AGbregLine, Y, ALine1, ALine2, AContext, X, AMinorX: Integer;
  AData: TBytes;
  AResult, ABit: Byte;
begin
  AImage := Image;
  AGbw := AImage.Width;
  AGbh := AImage.Height;
  ARowStride := AImage.Stride;
  AGbregLine := 0;
  AData := AImage.Data;
  if AGbw <= 0 then
    TdxPDFUtils.RaiseTestException;
  for Y := 0 to AGbh - 1 do
  begin
    if (Y >= 1) then
      ALine1 := AData[AGbregLine - ARowStride]
    else
      ALine1 := 0;
    if (Y >= 2) then
      ALine2 := AData[AGbregLine - (ARowStride shl 1)] shl 4
    else
      ALine2 := 0;
    AContext := ((ALine1 shr 3) and $7C) or ((ALine2 shr 3) and $380);
    X := 0;
    while X < ((AGbw + 7) and -8) do
    begin
      AResult := 0;
      if Y >= 1 then
        ALine1 := ((ALine1 shl 8)) or IfThen(X + 8 < AGbw, AData[AGbregLine - ARowStride + (X shr 3) + 1], 0);
      if Y >= 2 then
        ALine2 := ((ALine2 shl 8)) or IfThen(X + 8 < AGbw,
          AData[AGbregLine - (ARowStride shl 1) + (X shr 3) + 1] shl 4, 0);

      for AMinorX := 0 to IfThen(AGbw - X > 8, 8, AGbw - X) - 1 do
      begin
        ABit := IfThen(ADecoder.DecodeGB(AContext), 1, 0);
        AResult := AResult or Byte((ABit shl (7 - AMinorX)));
        AContext := (((AContext and $1BD) shl 1)) or (ABit) or (((ALine1 shr (10 - AMinorX)) and $4)) or
          (((ALine2 shr (10 - AMinorX)) and $80));
      end;
      AData[AGbregLine + (X shr 3)] := AResult;
      Inc(X, 8);
    end;
    Inc(AGbregLine, ARowStride);
  end;
end;

{ TdxJBIG2Template2aDecoder }

procedure TdxJBIG2Template2aDecoder.Decode;
begin
  TdxPDFUtils.RaiseTestException;
end;

{ TdxJBIG2Template3Decoder }

constructor TdxJBIG2Template3Decoder.Create(AImage: IdxJBIG2Image; AGbat0, AGbat1: Integer);
begin
  inherited Create(AImage);
  FGBAT0 := AGbat0;
  FGBAT1 := AGbat1;
end;

procedure TdxJBIG2Template3Decoder.Decode(ADecoder: TdxJBIG2Decoder);
var
  AImage: IdxJBIG2Image;
  AContext, Y, X: Integer;
  ABit: Boolean;
begin
  AImage := Image;
  for Y := 0 to AImage.Height - 1 do
  begin
    for X := 0 to AImage.Width - 1 do
    begin
      AContext := AImage.GetPixelEx(X - 1, Y);
      AContext := AContext or Image.GetPixelEx(X - 2, Y) shl 1;
      AContext := AContext or Image.GetPixelEx(X - 3, Y) shl 2;
      AContext := AContext or Image.GetPixelEx(X - 4, Y) shl 3;
      AContext := AContext or Image.GetPixelEx(X + FGBAT0, Y + FGBAT1) shl 4;
      AContext := AContext or Image.GetPixelEx(X + 1, Y - 1) shl 5;
      AContext := AContext or Image.GetPixelEx(X + 0, Y - 1) shl 6;
      AContext := AContext or Image.GetPixelEx(X - 1, Y - 1) shl 7;
      AContext := AContext or Image.GetPixelEx(X - 2, Y - 1) shl 8;
      AContext := AContext or Image.GetPixelEx(X - 3, Y - 1) shl 9;
      ABit := ADecoder.DecodeGB(AContext);
      AImage.SetPixel(X, Y, ABit);
    end;
  end;
end;

{ TdxJBIG2TextRegion }

constructor TdxJBIG2TextRegion.Create(AInfo: TdxJBIG2HeaderInfo; ADecoder: TdxJBIG2Decoder;
  ASymbolInstanceCount, ARefinementTemplate: Integer; AAt, ARAt: TList<Integer>;
  AGlyphs: TList<IdxJBIG2Image>; AImage: IdxJBIG2Image);
begin
  inherited Create(AImage);
  FHeaderInfo := TdxJBIG2HeaderInfo.Create(AInfo);

  FGlyphs := TList<IdxJBIG2Image>.Create;
  FGlyphs.AddRange(AGlyphs);

  FAdaptiveTemplates := TList<Integer>.Create;
  FAdaptiveTemplates.AddRange(AAt);

  FRefinementAdaptiveTemplates := TList<Integer>.Create;
  FRefinementAdaptiveTemplates.AddRange(ARAt);

  FDecoderRef := ADecoder;
  FRefinementTemplate := ARefinementTemplate;
  FRegionInfo := TdxJBIG2RegionSegmentInfo.Create(AImage.Width, AImage.Height);
  FRefinement := True;
  FSymbolInstanceCount := ASymbolInstanceCount;
  FStrips := 1;
  FCorner := jbcTopLeft;
end;

constructor TdxJBIG2TextRegion.Create(AStream: TStream; AInfo: TdxJBIG2HeaderInfo; AImage: IdxJBIG2Image);
var
 AFlags: Integer;
begin
  inherited Create(AStream, AInfo, AImage);
  CacheData(AStream);
  FRegionInfo := TdxJBIG2RegionSegmentInfo.Create(Helper);
  AFlags := Helper.ReadInt16;
  if (AFlags and $0001) <> 0 then
    TdxPDFUtils.RaiseTestException;
  FRefinement := (AFlags and $0002) <> 0;
  LogSBStrips := AFlags and $000C shr 2;
  FStrips := 1 shl LogSBStrips;
  FCorner := TdxJBIG2Corner(AFlags and $0030 shr 4);
  FTransposed := (AFlags and $0040) <> 0;
  FCombinationOperator := AFlags and $0180 shr 7;
  FDefaultPixel := (AFlags and $0200) <> 0;
  FDSValuesOffset := AFlags and $7C00 shr 10;
  if FDSValuesOffset > $0F then
    FDSValuesOffset := FDSValuesOffset - $20;
  FRefinementTemplate := AFlags and $8000 shr 15;
  if FRefinement and (FRefinementTemplate = 0) then
    FRefinementAdaptiveTemplates := Helper.CreateAdaptiveTemplates(4);
  FSymbolInstanceCount := Helper.ReadInt32;
  FGlyphs := TList<IdxJBIG2Image>.Create;
  AddGlobalGlyphs(FGlyphs);
  FDecoderRef := TdxJBIG2Decoder.Create(Helper, FGlyphs.Count);
  FNeedDestroyDecoder := True;
end;

destructor TdxJBIG2TextRegion.Destroy;
begin
  if FNeedDestroyDecoder then
    FreeAndNil(FDecoderRef);
  FreeAndNil(FRegionInfo);
  FreeAndNil(FGlyphs);
  FreeAndNil(FAdaptiveTemplates);
  FreeAndNil(FRefinementAdaptiveTemplates);
  inherited Destroy;
end;

function TdxJBIG2TextRegion.NeedCacheData: Boolean;
begin
  Result := False;
end;

procedure TdxJBIG2TextRegion.Process;
var
  AResultImage, AImage: IdxJBIG2Image;
  ACurs, AStript, AFirsts, ANInstances, ADt, ACurt, T, AId, X, Y, ASs, ATt, AIds: Integer;
begin
  inherited Process;

  if FGlyphs.Count = 0 then
    Exit;
  AResultImage := TdxJBIG2Image.Create(FRegionInfo.Width, FRegionInfo.Height);
  AResultImage.Clear(FDefaultPixel);

  AStript := FDecoderRef.DecodeDT * -FStrips;
  AFirsts := 0;
  ANInstances := 0;
  while ANInstances < FSymbolInstanceCount do
  begin
    ADt := FDecoderRef.DecodeDT * FStrips;
    Inc(AStript, ADt);
    Inc(AFirsts, FDecoderRef.DecodeFS);
    ACurs := AFirsts;
    while not FDecoderRef.LastCode do
    begin
      if FStrips = 1 then
        ACurt := 0
      else
        ACurt := FDecoderRef.DecodeIT;

      T := AStript + ACurt;
      AId := FDecoderRef.DecodeID;
      AImage := FGlyphs[AId] as TdxJBIG2Image;

      if FRefinement and (FDecoderRef.DecodeRI > 0) then
        AImage := RefinementDecode(FDecoderRef, AImage, FAdaptiveTemplates);

      if (not FTransposed) and (Integer(FCorner) > 1) then
        Inc(ACurs, AImage.Width - 1)
      else
        if (FTransposed) and ((Integer(FCorner) and 1) = 0) then
          Inc(ACurs, AImage.Height - 1);

      X := 0;
      Y := 0;

      ASs := IfThen(FTransposed, T, ACurs);
      ATt := IfThen(FTransposed, ACurs, T);

      case FCorner of
        jbcTopLeft:
          begin
            X := ASs;
            Y := ATt;
          end;
        jbcTopRight:
          begin
            X := ASs - AImage.Width + 1;
            Y := ATt;
          end;
        jbcBottomLeft:
          begin
            X := ASs;
            Y := ATt - AImage.Height + 1;
          end;
        jbcBottomRight:
          begin
            X := ASs - AImage.Width + 1;
            Y := ATt - AImage.Height + 1;
          end;
      end;

      AResultImage.Composite(AImage, X, Y, FCombinationOperator);

      if not FTransposed and (Integer(FCorner) < 2) then
        Inc(ACurs, AImage.Width - 1)
      else
        if FTransposed and ((Integer(FCorner) and 1) <> 0) then
          Inc(ACurs, AImage.Height - 1);

      Inc(ANInstances);
      if ANInstances > FSymbolInstanceCount then
        Break;
      AIds := FDecoderRef.DecodeDS;
      Inc(ACurs, AIds + FDSValuesOffset);
    end;
  end;
  if (HeaderInfo.Flags and 63) <> 4 then
    ImageRef.Composite(AResultImage, FRegionInfo.X, FRegionInfo.Y, FRegionInfo.ComposeOperator);
end;

function TdxJBIG2TextRegion.RefinementDecode(ADecoder: TdxJBIG2Decoder; AImage: IdxJBIG2Image;
  AAdaptiveTemplates: TList<Integer>): IdxJBIG2Image;
var
  ARdw, ARdh, ARdx, ARdy, ADx, ADy: Integer;
  ARefinementRegion: TdxJBIG2RefinementRegion;
begin
  ARdw := ADecoder.DecodeRDW;
  ARdh := ADecoder.DecodeRDH;
  ARdx := ADecoder.DecodeRDX;
  ARdy := ADecoder.DecodeRDY;
  ADx := (ARdw shr 1) + ARdx;
  ADy := (ARdh shr 1) + ARdy;

  Result := TdxJBIG2Image.Create(AImage.Width + ARdw, AImage.Height + ARdh);

  ARefinementRegion := TdxJBIG2RefinementRegion.Create(AImage, ADecoder, ADx, ADy, FRefinementTemplate,
    FRefinementAdaptiveTemplates, Result);
  try
    ARefinementRegion.Process;
  finally
    ARefinementRegion.Free;
  end;
end;

{ TdxJBIG2RefinementRegion }

constructor TdxJBIG2RefinementRegion.Create(AGlyph: IdxJBIG2Image; ADecoder: TdxJBIG2Decoder;
  ADx, ADy, ATemplate: Integer; AAt: TList<Integer>; AImage: IdxJBIG2Image);
begin
  inherited Create(AImage);
  FDecoderRef := ADecoder;
  FAdaptiveTemplate := AAt;
  FReferenceGlyph := AGlyph;
  FDx := Max(ADx, 0);
  FDy := Max(ADy, 0);
  FTemplate := ATemplate;
end;

destructor TdxJBIG2RefinementRegion.Destroy;
begin
  FAdaptiveTemplate := nil;
  inherited Destroy;
end;

procedure TdxJBIG2RefinementRegion.Process;
var
  X, Y, AContext: Integer;
begin
  inherited Process;
  for Y := 0 to ImageRef.Height - 1 do
    for X := 0 to ImageRef.Width - 1 do
    begin
      if FTemplate = 0 then
        AContext := Template0Context(X, Y)
      else
        if FTemplate = 1 then
          AContext := Template1Context(X, Y)
        else
        begin
          AContext := 0;
          TdxPDFUtils.RaiseTestException;
        end;
      ImageRef.SetPixel(X, Y, FDecoderRef.DecodeGR(AContext));
    end;
end;

function TdxJBIG2RefinementRegion.Template0Context(X: Integer; Y: Integer): Integer;
var
  AContext: Integer;
begin
  AContext := 0;
  AContext := AContext or ImageRef.GetPixelEx(X - 1, Y + 0);
  AContext := AContext or ImageRef.GetPixelEx(X + 1, Y - 1) shl 1;
  AContext := AContext or ImageRef.GetPixelEx(X + 0, Y - 1) shl 2;
  AContext := AContext or ImageRef.GetPixelEx(X + FAdaptiveTemplate[0], Y + FAdaptiveTemplate[1]) shl 3;
  AContext := AContext or FReferenceGlyph.GetPixelEx(X - FDx + 1, Y - FDy + 1) shl 4;
  AContext := AContext or FReferenceGlyph.GetPixelEx(X - FDx + 0, Y - FDy + 1) shl 5;
  AContext := AContext or FReferenceGlyph.GetPixelEx(X - FDx - 1, Y - FDy + 1) shl 6;
  AContext := AContext or FReferenceGlyph.GetPixelEx(X - FDx + 1, Y - FDy + 0) shl 7;
  AContext := AContext or FReferenceGlyph.GetPixelEx(X - FDx + 0, Y - FDy + 0) shl 8;
  AContext := AContext or FReferenceGlyph.GetPixelEx(X - FDx - 1, Y - FDy + 0) shl 9;
  AContext := AContext or FReferenceGlyph.GetPixelEx(X - FDx + 1, Y - FDy - 1) shl 10;
  AContext := AContext or FReferenceGlyph.GetPixelEx(X - FDx + 0, Y - FDy - 1) shl 11;
  AContext := AContext or FReferenceGlyph.GetPixelEx(X - FDx + FAdaptiveTemplate[2], Y - FDy + FAdaptiveTemplate[3]) shl 12;
  Result := AContext;
end;

function TdxJBIG2RefinementRegion.Template1Context(X: Integer; Y: Integer): Integer;
var
  AContext: Integer;
begin
  AContext := 0;
  AContext := AContext or ImageRef.GetPixelEx(X - 1, Y + 0);
  AContext := AContext or ImageRef.GetPixelEx(X + 1, Y - 1) shl 1;
  AContext := AContext or ImageRef.GetPixelEx(X + 0, Y - 1) shl 2;
  AContext := AContext or ImageRef.GetPixelEx(X - 1, Y - 1) shl 3;
  AContext := AContext or FReferenceGlyph.GetPixelEx(X - FDx + 1, Y - FDy + 1) shl 4;
  AContext := AContext or FReferenceGlyph.GetPixelEx(X - FDx + 0, Y - FDy + 1) shl 5;
  AContext := AContext or FReferenceGlyph.GetPixelEx(X - FDx + 1, Y - FDy + 0) shl 6;
  AContext := AContext or FReferenceGlyph.GetPixelEx(X - FDx + 0, Y - FDy + 0) shl 7;
  AContext := AContext or FReferenceGlyph.GetPixelEx(X - FDx - 1, Y - FDy + 0) shl 8;
  AContext := AContext or FReferenceGlyph.GetPixelEx(X - FDx + 0, Y - FDy - 1) shl 9;
  Result := AContext;
end;

{ TdxJBIG2GenericRegion }

constructor TdxJBIG2GenericRegion.Create(AStream: TStream; AInfo: TdxJBIG2HeaderInfo; AImage: IdxJBIG2Image);
var
  ALength: Integer;
begin
  inherited Create(AStream, AInfo, AImage);
  FRegionInfo := TdxJBIG2RegionSegmentInfo.Create(Helper);
  FFlags := Helper.ReadByte;
  FMMR := (FFlags and 1) > 0;
  FTemplate := (FFlags and 6) shr 1;
  FUseTypicalPredictionDecoding := ((FFlags and 8) shr 3) > 0;
  if (FFlags and 1) = 0 then
  begin
    ALength := IfThen((FFlags and 6) > 0, 2, 8);
    FAdaptiveTemplates := Helper.CreateAdaptiveTemplates(ALength);
  end;
  CacheData(AStream);
  FDecoderRef := TdxJBIG2Decoder.Create(Helper, 0);
end;

constructor TdxJBIG2GenericRegion.Create(AInfo: TdxJBIG2HeaderInfo; ATemplate: Integer);
begin
  inherited Create(nil, AInfo, nil);
  FTemplate := ATemplate;
end;

destructor TdxJBIG2GenericRegion.Destroy;
begin
  FreeAndNil(FDecoderRef);
  FreeAndNil(FAdaptiveTemplates);
  FreeAndNil(FRegionInfo);
  inherited Destroy;
end;

function TdxJBIG2GenericRegion.NeedCacheData: Boolean;
begin
  Result := False;
end;

procedure TdxJBIG2GenericRegion.Process;
var
  AImage: IdxJBIG2Image;
  ADecoder2: TdxJBIG2GenericRegionDecoder;
begin
  inherited Process;
  AImage := TdxJBIG2Image.Create(FRegionInfo.Width, FRegionInfo.Height);
  ADecoder2 := CreateDecoder(AImage, FAdaptiveTemplates);
  try
    ADecoder2.Decode(FDecoderRef);
  finally
    ADecoder2.Free;
  end;
  ImageRef.Composite(AImage, FRegionInfo.X, FRegionInfo.Y, FRegionInfo.ComposeOperator);
end;

function TdxJBIG2GenericRegion.CreateDecoder(AImage: IdxJBIG2Image; AAdaptiveTemplates: TList<Integer>): TdxJBIG2GenericRegionDecoder;
begin
  Result := nil;
  if FMMR then
    TdxPDFUtils.RaiseTestException;
  if FUseTypicalPredictionDecoding then
    Result := TdxJBIG2TPGDONDecoder.CreateDecoder(AImage, AAdaptiveTemplates, FTemplate)
  else
    case FTemplate of
      0:
        if (AAdaptiveTemplates[0] = 3) and (AAdaptiveTemplates[1] = -1) and (AAdaptiveTemplates[2] = -3) and
          (AAdaptiveTemplates[3] = -1) and (AAdaptiveTemplates[4] = 2) and (AAdaptiveTemplates[5] = -2) and
          (AAdaptiveTemplates[6] = -2) and (AAdaptiveTemplates[7] = -2) then
          Result := TdxJBIG2Template0Decoder.Create(AImage);
      1:
        Result := TdxJBIG2Template1Decoder.Create(AImage);
      2:
        if (AAdaptiveTemplates[0] = 3) and (AAdaptiveTemplates[1] = -1) then
          Result := TdxJBIG2Template2aDecoder.Create(AImage)
        else
          Result := TdxJBIG2Template2Decoder.Create(AImage);
      3:
        Result := TdxJBIG2Template3Decoder.Create(AImage, AAdaptiveTemplates[0], AAdaptiveTemplates[1]);
    else
      TdxPDFUtils.RaiseTestException;
    end;
end;

{ TdxJBIG2PageInfo }

constructor TdxJBIG2PageInfo.Create(AStream: TStream; AInfo: TdxJBIG2HeaderInfo; AImage: IdxJBIG2Image);
var
  AWidth, AHeight: Integer;
  AHelper: TdxJBIG2StreamReader;
begin
  inherited Create(AStream, AInfo, AImage);

  AHelper := TdxJBIG2StreamReader.Create(AStream);
  try
    AWidth := AHelper.ReadInt32;
    AHeight := AHelper.ReadInt32;
    AHelper.ReadInt32;
    AHelper.ReadInt32;
    AHelper.ReadByte;
    AHelper.ReadInt16;
  finally
    AHelper.Free;
  end;
  AImage.SetDimensions(AWidth, AHeight);
end;

function TdxJBIG2PageInfo.NeedCacheData: Boolean;
begin
  Result := False;
end;

{ TdxJBIG2ArithmeticIntContext }

constructor TdxJBIG2ArithmeticIntContext.Create;
begin
  inherited Create;
  SetLength(FIax, 512);
end;

destructor TdxJBIG2ArithmeticIntContext.Destroy;
begin
  SetLength(FIax, 0);
  inherited Destroy;
end;

function TdxJBIG2ArithmeticIntContext.Decode(AState: TdxArithmeticState): TdxJBIG2ArithmeticDecoderResult;
var
  APrev, S, V, ABit, AN_tail, AOffset, I: Integer;
begin
  APrev := 1;
  S := AState.Decode(FIax, APrev);
  APrev := (APrev shl 1) or S;
  ABit := AState.Decode(FIax, APrev);
  APrev := (APrev shl 1) or ABit;
  if ABit <> 0 then
  begin
    ABit := AState.Decode(FIax, APrev);
    APrev := (APrev shl 1) or ABit;
    if ABit <> 0 then
    begin
      ABit := AState.Decode(FIax, APrev);
      APrev := (APrev shl 1) or ABit;
      if ABit <> 0 then
      begin
        ABit := AState.Decode(FIax, APrev);
        APrev := (APrev shl 1) or ABit;
        if ABit <> 0 then
        begin
          ABit := AState.Decode(FIax, APrev);
          APrev := (APrev shl 1) or ABit;
          if ABit <> 0 then
          begin
            AN_tail := 32;
            AOffset := 4436;
          end
          else
          begin
            AN_tail := 12;
            AOffset := 340;
          end;
        end
        else
        begin
          AN_tail := 8;
          AOffset := 84;
        end;
      end
      else
      begin
        AN_tail := 6;
        AOffset := 20;
      end;
    end
    else
    begin
      AN_tail := 4;
      AOffset := 4;
    end;
  end
  else
  begin
    AN_tail := 2;
    AOffset := 0;
  end;
  V := 0;
  for I := 0 to AN_tail - 1 do
  begin
    ABit := AState.Decode(FIax, APrev);
    APrev := ((APrev shl 1) and 511) or (APrev and 256) or ABit;
    V := (V shl 1) or ABit;
  end;
  Inc(V, AOffset);
  V := IfThen(S <> 0, -V, V);
  Result.Result := V;
  Result.Code := (S <> 0) and (V = 0);
end;

{ TdxJBIG2Decoder }

constructor TdxJBIG2Decoder.Create(AHelper: TdxJBIG2StreamReader; AMaxId: Integer);
begin
  Create(AHelper, FindLength(AMaxId), 16, 16);
end;

constructor TdxJBIG2Decoder.Create(AHelper: TdxJBIG2StreamReader; AIdLength, AGbLength, AGrLength: Integer);
begin
  inherited Create;
  FArithState := TdxArithmeticState.Create(AHelper);

  FIdLength := AIdLength;
  FIardw := TdxJBIG2ArithmeticIntContext.Create;
  FIardh := TdxJBIG2ArithmeticIntContext.Create;
  FIardx := TdxJBIG2ArithmeticIntContext.Create;
  FIardy := TdxJBIG2ArithmeticIntContext.Create;
  FIadh := TdxJBIG2ArithmeticIntContext.Create;
  FIadw := TdxJBIG2ArithmeticIntContext.Create;
  FIaex := TdxJBIG2ArithmeticIntContext.Create;
  FIaai := TdxJBIG2ArithmeticIntContext.Create;
  FIadt := TdxJBIG2ArithmeticIntContext.Create;
  FIafs := TdxJBIG2ArithmeticIntContext.Create;
  FIads := TdxJBIG2ArithmeticIntContext.Create;
  FIait := TdxJBIG2ArithmeticIntContext.Create;
  FIari := TdxJBIG2ArithmeticIntContext.Create;
  FIaid := TdxArithmeticContext.Create(FArithState, FIdLength);
  FGb := TdxArithmeticContext.Create(FArithState, AGbLength);
  FGr := TdxArithmeticContext.Create(FArithState, AGrLength);
end;

destructor TdxJBIG2Decoder.Destroy;
begin
  FreeAndNil(FIardw);
  FreeAndNil(FIardh);
  FreeAndNil(FIardx);
  FreeAndNil(FIardy);
  FreeAndNil(FIadh);
  FreeAndNil(FIadw);
  FreeAndNil(FIaex);
  FreeAndNil(FIaai);
  FreeAndNil(FIadt);
  FreeAndNil(FIafs);
  FreeAndNil(FIads);
  FreeAndNil(FIait);
  FreeAndNil(FIari);
  FreeAndNil(FIaid);
  FreeAndNil(FGb);
  FreeAndNil(FGr);
  FreeAndNil(FArithState);
  inherited Destroy;
end;

function TdxJBIG2Decoder.DecodeAI: Integer;
begin
  Result := Decode(FIaai);
end;

function TdxJBIG2Decoder.DecodeDH: Integer;
begin
  Result := Decode(FIadh);
end;

function TdxJBIG2Decoder.DecodeDS: Integer;
begin
  Result := Decode(FIads);
end;

function TdxJBIG2Decoder.DecodeDT: Integer;
begin
  Result := Decode(FIadt);
end;

function TdxJBIG2Decoder.DecodeDW: Integer;
begin
  Result := Decode(FIadw);
end;

function TdxJBIG2Decoder.DecodeEX: Integer;
begin
  Result := Decode(FIaex);
end;

function TdxJBIG2Decoder.DecodeFS: Integer;
begin
  Result := Decode(FIafs);
end;

function TdxJBIG2Decoder.DecodeID: Integer;
var
  I: Integer;
begin
  Result := 1;
  for I := 0 to FIdLength - 1 do
    Result := (Result shl 1) or FIaid.DecodeBit(Result);
  Result := Result and ((1 shl FIdLength) - 1);
  FLastCode := False;
end;

function TdxJBIG2Decoder.DecodeIT: Integer;
begin
  Result := Decode(FIait);
end;

function TdxJBIG2Decoder.DecodeGB(AContext: Integer): Boolean;
begin
  Result := FGb.DecodeBit(AContext) <> 0;
end;

function TdxJBIG2Decoder.DecodeGR(AContext: Integer): Boolean;
begin
  Result := FGr.DecodeBit(AContext) <> 0;
end;

function TdxJBIG2Decoder.DecodeRDH: Integer;
begin
  Result := Decode(FIardh);
end;

function TdxJBIG2Decoder.DecodeRDX: Integer;
begin
  Result := Decode(FIardx);
end;

function TdxJBIG2Decoder.DecodeRDY: Integer;
begin
  Result := Decode(FIardy);
end;

function TdxJBIG2Decoder.DecodeRDW: Integer;
begin
  Result := Decode(FIardw);
end;

function TdxJBIG2Decoder.DecodeRI: Integer;
begin
  Result := Decode(FIari);
end;

function TdxJBIG2Decoder.Decode(ACtx: TdxJBIG2ArithmeticIntContext): Integer;
var
  ADecodeResult: TdxJBIG2ArithmeticDecoderResult;
begin
  ADecodeResult := ACtx.Decode(FArithState);
  FLastCode := ADecodeResult.Code;
  Result := ADecodeResult.Result;
end;

function TdxJBIG2Decoder.FindLength(L: Integer): Integer;
begin
  Result := 0;
  while (1 shl Result) < L do
    Inc(Result);
end;

{ TdxJBIG2HeaderInfo }

constructor TdxJBIG2HeaderInfo.Create(AInfo: TdxJBIG2HeaderInfo);
begin
  inherited Create;
  ReferredToSegments := TList<Integer>.Create;
  if AInfo <> nil then
  begin
    DataLength := AInfo.DataLength;
    Flags := AInfo.Flags;
    ReferredToSegments.AddRange(AInfo.ReferredToSegments);
  end;
end;

destructor TdxJBIG2HeaderInfo.Destroy;
begin
  FreeAndNil(ReferredToSegments);
  inherited Destroy;
end;

end.
