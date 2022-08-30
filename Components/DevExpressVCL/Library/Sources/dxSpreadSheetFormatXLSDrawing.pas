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

unit dxSpreadSheetFormatXLSDrawing;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Types, Classes, SysUtils, Dialogs, Math, dxCore, cxClasses, Generics.Defaults, Generics.Collections, dxSpreadSheetFormatXLS,
  dxSpreadSheetFormatXLSTypes, dxSpreadSheetCore, dxSpreadSheetFormulas, dxSpreadSheetTypes, dxSpreadSheetFunctions,
  cxVariants, dxSpreadSheetFormatXLSFormulas, dxGDIPlusClasses, dxSpreadSheetFormatUtils, dxSpreadSheetUtils, ZLib,
  dxCoreGraphics, dxSpreadSheetClasses, dxHash, dxHashUtils, cxGeometry, Graphics, dxSpreadSheetContainers,
  dxSpreadSheetHyperlinks, dxSpreadSheetFormatXLSWriter, dxSpreadSheetFormatXLSReader;

const
  { Office art record types }
   oaDrawingContainer        = $F000;
   oaBlipStoreContainer      = $F001;
   oaDrawingObjectsContainer = $F002;
   oaShapeGroupContainer     = $F003;
   oaShapeContainer          = $F004;
   oaFileDrawingGroupRecord  = $F006;
   oaFileBlipStoreEntry      = $F007;
   oaFileDrawingRecord       = $F008;
   oaShapeGroupCoordinates   = $F009;
   oaFileShape               = $F00A;
   oaPropertiesTable         = $F00B;
   oaClientTextbox           = $F00D;
   oaChildAnchor             = $F00F;
   oaClientAnchor            = $F010;
   oaClientData              = $F011;
   oaSplitMenuColorContainer = $F11E;
   oaTertiaryPropertiesTable = $F122;
   //
   oaBLIPFirst               = $F018;
   oaBLIPLast                = $F117;
   //
   oaBlipEMF      = $03D4; // 03D5
   oaBlipWMF      = $0216; // 0217
   oaBlipJPG_RGB  = $046A; // 046B
   oaBlipJPG_CMYK = $06E2; // 06E3
   oaBlipPNG      = $06E0; // 06E1
   oaBlipDIB      = $07A8; // 07A9
   oaBlipTIFF     = $06E4; // 06E5

   // shape flags

   fsGroup         = $001;
   fsChild         = $002;
   fsPatriarch     = $004;
   fsDeleted       = $008;
   fsOleShape      = $010;
   fsHaveMaster    = $020;
   fsFlipH         = $040;
   fsFlipV         = $080;
   fsConnector     = $100;
   fsHaveAnchor    = $200;
   fsBackground    = $400;
   fsHaveShapeType = $800;

   //
   clDefaultFillColor = $8000041;
   clDefaultLineColor = $8000040;

   XLSDefaultWrapText = True;

type
  ToaBlipTypes = (
    btError = 0,
    btUnknown = 1,
    btEmf = 2,
    btWmf = 3,
    btMacPict = 4,
    btJpeg = 5,
    btPng = 6,
    btDib = 7,
    btTiff = $11,
    btCMYKJpeg = $12);

  TdxMSOHeader = record
    recVer: Byte;
    recInstance: Word;
    recType: Word;
    recSize: LongWord;
  end;

  PdxMSOAnchor = ^TdxMSOAnchor;
  TdxMSOAnchor = packed record
    Options: Word;
    Col1: Word;
    Col1Offset: SmallInt;
    Row1: Word;
    Row1Offset: SmallInt;
    Col2: Word;
    Col2Offset: SmallInt;
    Row2: Word;
    Row2Offset: SmallInt;
  end;

  TdxMSODrawingGroupRecord = packed record
    spidMax: Integer;
    sidcl: Integer;
    cspSaved: Integer;
    cdgSaved: Integer;
  end;

  TdxMSOBlipStoreEntry = packed record
    btWinType: Byte;
    btMacType: Byte;
    rgbUid: array[0..15] of Byte;
    tag: Word;
    size: LongWord;
    cRef: LongWord;
    foDelay: LongWord;
    unused1: Byte;
    cbName: Byte;
    unused2: Byte;
    unused3: Byte;
  end;

  PdxXLSColorRef = ^TdxXLSColorRef;
  TdxXLSColorRef = packed record
    Color: TColor;
    Position: Integer;
  end;

  TdxXLSColorRefArray = array of TdxXLSColorRef;

type
  TdxXLSFixedPoint = packed record
    Integral: Word;
    Fractional: SmallInt;
  end;

  TdxXLSFunction = function: Boolean of object;
  TdxXLSDrawingProperty = procedure(const AValue: Integer; const AComplexData: array of Byte) of object;

  { TdxSpreadSheetMSODrawingReader }

  TdxSpreadSheetMSODrawingReader = class
  strict private
    FAnchor: TdxMSOAnchor;
    FAnchorAssigned: Boolean;
    FAnchorType: TdxSpreadSheetContainerAnchorType;
    FBlipCount: Integer;
    FBlipID: Integer;
    FBrushBottomColor: TdxAlphaColor;
    FBrushColor: TdxAlphaColor;
    FBrushColors: TList<TdxXLSColorRef>;
    FBrushGradientMode: TdxGPBrushGradientMode;
    FBrushGradientModeInverseOrder: Boolean;
    FBrushStyle: TdxGPBrushStyle;
    FFillBlipID: Integer;
    FFlipHorizontally: Boolean;
    FFlipVertically: Boolean;
    FHidden: Boolean;
    FHyperlink: TdxSpreadSheetHyperlink;
    FOwner: TdxSpreadSheetXLSReader;
    FPenColor: TdxAlphaColor;
    FPenStyle: TdxGPPenStyle;
    FPenWidth: Single;
    FPropertyReaders: array[0..$FFFF] of TdxXLSDrawingProperty;
    FReader: TcxReader;
    FRecordHeader: TdxMSOHeader;
    FRecordReaders: array[$F000..$FFFF] of TdxXLSFunction;
    FRotationAngle: Double;
    FShapeFlags: Integer;
    FShapeID: Integer;
    FShapeType: Integer;
    FTextPadding: TRect;
    FWrapText: Boolean;

    function CreateContainer: TdxSpreadSheetShapeContainer;
    function GetStream: TStream; inline;
    //
    procedure Property_BooleanProtection(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DiagramBooleanProperties(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingBlackWhiteMode(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingBlipBooleanProperties(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingBlipFlags(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingBlipIdentifier(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingBlipName(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingCropFromBottom(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingCropFromLeft(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingCropFromRight(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingCropFromTop(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingFillBlipIdentifier(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingFillStyleBooleanProperties(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingGroupShape2PctHoriz(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingGroupShape2PctHorizPos(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingGroupShape2PctVert(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingGroupShape2PctVertPos(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingGroupShape2SizeRelH(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingGroupShape2SizeRelV(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingGroupShapeBooleanProperties(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingGroupShapePosH(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingGroupShapePosRelH(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingGroupShapePosRelV(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingGroupShapePosV(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingLineCapStyle(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingLineColor(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingLineCompoundType(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingLineDashing(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingLineJoinStyle(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingLineMiterLimit(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingLineStyleBooleanProperties(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingLineWidth(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingRotation(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingShadowColor(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingShadowStyleBooleanProperties(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingShapeBooleanProperties(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingShapeDescription(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingShapeHyperlink(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingShapeName(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingShapeTooltip(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingTextBottom(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingTextIdentifier(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingTextLeft(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingTextRight(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingTextTop(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingWrapBottomDistance(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingWrapLeftDistance(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingWrapRightDistance(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingWrapText(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingWrapTopDistance(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingConnectionPointsType(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingTextBooleanProperties(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingTextDirection(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingFillColor(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingFillOpacity(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingFillBackColor(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingFillBackOpacity(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingFillBWColor(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingFillBlip(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingFillBlipName(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingFillBlipFlags(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingFillWidth(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingFillHeight(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingFillAngle(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingFillFocus(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingFillToLeft(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingFillToTop(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingFillToRight(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingFillToBottom(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingFillRectLeft(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingFillRectTop(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingFillRectRight(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingFillRectBottom(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingFillDzType(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingFillShadePreset(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingFillOriginX(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingFillOriginY(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingFillShapeOriginX(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingFillShapeOriginY(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingFillShadeColors(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingFillShadeType(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingFillColorExt(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingFillReserved415(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingFillTintShade(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingFillReserved417(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingFillBackColorExt(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingFillReserved419(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingFillBackTintShade(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingFillReserved421(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingFillReserved422(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingFillReserved423(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_DrawingShapeDocument(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_OfficeDrawingFillType(const AValue: Integer; const AComplexData: array of Byte);
    procedure Property_UndefinedComplexProperty(const AValue: Integer; const AComplexData: array of Byte);
    //
    procedure ReadComplexData(ASize: Integer; var AData: array of Byte);
    //
    function HOffsetToInt(AColumnIndex, AOffset: Integer): Integer;
    function VOffsetToInt(ARowIndex, AOffset: Integer): Integer;
  protected
    procedure ApplyPropertyValues;
    procedure ClearPropertyValues;
    procedure DeleteHyperlink;
    function GetGraphic(AIndex: Integer): TdxSmartImage;
    function HasBlip: Boolean;
    function HasShape: Boolean;
    procedure Initialize; virtual;
    function LoadMetaFile(AStream: TMemoryStream; AIsEMF: Boolean): TdxSmartImage;
    function MakeAlphaColor(const AColor: TColor; AAlpha: Byte = 255; AReplaceAlpha: Boolean = False): TColor;

    function ReadBlip: Boolean;
    function ReadBlipStoreContainer: Boolean;
    function ReadBlipStoryEntry: Boolean;
    function ReadClientAnchor: Boolean;
    function ReadDrawingContainer: Boolean;
    function ReadDrawingGroupRecord: Boolean;
    function ReadDrawingObjectsContainer: Boolean;
    function ReadDrawingRecord: Boolean;
    function ReadFileShape: Boolean;
    function ReadPropertiesTable: Boolean;
    function ReadShapeContainer: Boolean;
    function ReadShapeGroupContainer: Boolean;
    function ReadShapeGroupCoordinates: Boolean;
    function ReadSplitMenuColorContainer: Boolean;

    procedure RegisterPropertyReader(AKey: Word; AProc: TdxXLSDrawingProperty);
  public
    constructor Create(AOwner: TdxSpreadSheetXLSReader); virtual;
    destructor Destroy; override;
    procedure Read;

    property BlipCount: Integer read FBlipCount;
    property Owner: TdxSpreadSheetXLSReader read FOwner;
    property Reader: TcxReader read FReader;
    property RecordHeader: TdxMSOHeader read FRecordHeader;
    property Stream: TStream read GetStream;
  end;

  { TdxSpreadSheetMSODrawingWriter }

  TdxSpreadSheetMSODrawingWriter = class
  strict private
    FBlipID: Integer;
    FContainer: TdxSpreadSheetContainer;
    FCurrentImage: TdxGpImage;
    FCurrentObjectRefCount: Integer;
    FGroupID: Integer;
    FImageBinaryData: TMemoryStream;
    FOwner: TdxSpreadSheetXLSWriter;
    FShape: TdxSpreadSheetShape;
    FStream: TMemoryStream;
    FWriter: TcxWriter;

    function CalcDrawingsCount: Integer;
    function CalcMaxShapeIdentifier: Integer;
    function CalcShapesCount: Integer;
    function GetCurrentSheet: TdxSpreadSheetTableView;
    function GetSpreadSheet: TdxCustomSpreadSheet;
    function GetTextBoxProperties: TdxSpreadSheetCustomTextBox;
    function ValidateColor(AColor, ADefault: Integer): Integer;
    function ValueToHOffset(AColumnIndex, AOffset: Integer): Integer;
    function ValueToVOffset(ARowIndex, AOffset: Integer): Integer;
  protected
    procedure EndRecord(ARecord: Integer; AIncludeSizeOfHeader: Boolean = False);
    procedure Initialize(AContainer: TdxSpreadSheetContainer);
    procedure InitializeShape(AContainer: TdxSpreadSheetContainer);
    function IsPicture: Boolean;
    function IsShape: Boolean;
    function IsTextBox: Boolean;

    procedure PrepareGradientColors(APoints: TdxGPBrushGradientPoints; var AColors: TdxXLSColorRefArray; var ABkColor: TColor);
    procedure MakeImageBinaryData;
    procedure MakeProperty(APropertyID: Word; AValue: Integer);
    function MakeRecordHeader(AInstance, ARecordType: Word): Integer; overload;
    function MakeRecordHeader(AVersion, AInstance: Byte; ARecordType: Word): Integer; overload;
    procedure WriteBlipStoryEntry;
    procedure WriteClientAnchor;
    procedure WriteCommonPropertiesTable;
    procedure WriteDrawingContainer;
    procedure WriteDrawingGroupRecord;
    procedure WriteFileDrawingRecord;
    procedure WriteFileShape(AWriteContainerSettings: Boolean);
    procedure WriteShapeContainer;
    procedure WriteShapeGroupCoordinates;
    procedure WriteShapeProperties;
    procedure WriteSplitMenuColorContainer;
    procedure WriteTextBoxProperties(ATextBox: TdxSpreadSheetCustomTextBox);

    procedure WriteProperty_DrawingBlipIdentifier(AIsPicture: Boolean);
    procedure WriteProperty_DrawingFillAngle;
    procedure WriteProperty_DrawingFillBlipIdentifier;
    procedure WriteProperty_DrawingFillBackColor(AValue: Integer);
    procedure WriteProperty_DrawingFillColor(AValue: Integer);
    procedure WriteProperty_DrawingFillShadeColors(const AValue: TdxXLSColorRefArray);
    procedure WriteProperty_DrawingFillShadeColorsHeader(const AValue: TdxXLSColorRefArray);
    procedure WriteProperty_DrawingLineColor(AValue: Integer);
    procedure WriteProperty_DrawingLineDashing(AValue: TdxGPPenStyle);
    procedure WriteProperty_DrawingLineWidth(AValue: Double);
    procedure WriteProperty_DrawingOfficeFillType;
    procedure WriteProperty_DrawingRotation(AAngle: Double);
    procedure WriteProperty_DrawingTextBooleanProperties;
    procedure WriteProperty_Hyperlink(AHyperlink: TdxSpreadSheetHyperlink);

    property BlipID: Integer read FBlipID;
    property Container: TdxSpreadSheetContainer read FContainer;
    property CurrentImage: TdxGpImage read FCurrentImage;
    property CurrentObjectRefCount: Integer read FCurrentObjectRefCount;
    property GroupID: Integer read FGroupID;
    property ImageBinaryData: TMemoryStream read FImageBinaryData;
    property Shape: TdxSpreadSheetShape read FShape;
    property TextBoxProperties: TdxSpreadSheetCustomTextBox read GetTextBoxProperties;
  public
    constructor Create(AOwner: TdxSpreadSheetXLSWriter);
    destructor Destroy; override;
    procedure WriteContainer(AContainer: TdxSpreadSheetContainer; AIsFirstOnTheSheet: Boolean; var ASize: Integer);
    procedure WriteDrawingGroups;

    property Owner: TdxSpreadSheetXLSWriter read FOwner;
    property SpreadSheet: TdxCustomSpreadSheet read GetSpreadSheet;
    property CurrentSheet: TdxSpreadSheetTableView read GetCurrentSheet;
    property Stream: TMemoryStream read FStream;
    property Writer: TcxWriter read FWriter;
  end;

implementation

uses
  dxSpreadSheetFormatXLSXReaderDrawing, dxSmartImage, dxSpreadSheetCoreStyles;

type
  TdxHashTableAccess = class(TdxHashTable);
  TdxSpreadSheetAccess = class(TdxCustomSpreadSheet);
  TdxSpreadSheetContainerAccess = class(TdxSpreadSheetContainer);
  TdxSpreadSheetCustomTextBoxAccess = class(TdxSpreadSheetCustomTextBox);
  TdxSpreadSheetCustomTextBoxContainerAccess = class(TdxSpreadSheetCustomTextBoxContainer);
  TdxSpreadSheetPictureAccess = class(TdxSpreadSheetPicture);
  TdxSpreadSheetXLSWriterAccess = class(TdxSpreadSheetXLSWriter);

const
  msoBlipEMF  = 2;
  msoBlipJPEG = 5;
  msoBlipPNG  = 6;

  ImageDataFormat2StoreFormat: array[TdxImageDataFormat] of TdxImageDataFormat =
    (dxImagePng, dxImagePng, dxImageJpeg, dxImagePng, dxImagePng,
     dxImagePng, dxImagePng{dxImageEmf}, dxImagePng, dxImagePng, dxImagePng, dxImagePng{dxImageEmf});

  BlipFormatID: array[TdxImageDataFormat] of Integer =
    (msoBlipPng, msoBlipPng, msoBlipJPEG, msoBlipPng, msoBlipPng,
     msoBlipPng, msoBlipPng{msoBlipEMF}, msoBlipPng, msoBlipPng, msoBlipPng, msoBlipPng{msoBlipEMF});

procedure MakeDibHeader(AStream: TStream; ASize: Integer);
var
  AWriter: TcxWriter;
begin
  AWriter := TcxWriter.Create(AStream);
  try
    AWriter.WriteWord($4D42);
    AWriter.WriteWord(ASize - MulDiv(ASize, 1, $FFFF));
    AWriter.WriteByte(MulDiv(ASize, 1, $FFFF));
    AWriter.WriteInteger(0);
    AWriter.WriteByte(0);
    AWriter.WriteWord($0036);
    AWriter.WriteWord(0);
  finally
    AWriter.Free;
  end;
end;

procedure RecreateStreamFromDIB(ADestStream, ASourceStream: TStream; ASize: Integer);
var
  ABitmap: TBitmap;
begin
  MakeDibHeader(ADestStream, ASize + 14);
  ADestStream.CopyFrom(ASourceStream, ASize);
  ABitmap := TBitmap.Create;
  try
    ADestStream.Position := 0;
    ABitmap.LoadFromStream(ADestStream);
    ADestStream.Size := 0;
    ABitmap.SaveToStream(ADestStream);
  finally
    ABitmap.Free;
  end;
end;

{ TdxSpreadSheetMSODrawingReader }

constructor TdxSpreadSheetMSODrawingReader.Create(AOwner: TdxSpreadSheetXLSReader);
begin
  FOwner := AOwner;
  FReader  := TcxReader.Create(Stream);
  FBrushColors := TList<TdxXLSColorRef>.Create;
  Initialize;
end;

destructor TdxSpreadSheetMSODrawingReader.Destroy;
begin
  FreeAndNil(FBrushColors);
  FreeAndNil(FReader);
  inherited Destroy;
end;

procedure TdxSpreadSheetMSODrawingReader.Read;
var
  ASavePos: Integer;
  ARecordReader: TdxXLSFunction;
begin
  ClearPropertyValues;
  while (Stream.Size - Stream.Position) >= 8 do
  begin
    FRecordHeader.recInstance := Reader.ReadWord;
    FRecordHeader.recVer := FRecordHeader.recInstance and $0F;
    FRecordHeader.recInstance := FRecordHeader.recInstance shr 4 and $0FFF;
    FRecordHeader.recType := Reader.ReadWord;
    FRecordHeader.recSize := LongWord(Reader.ReadInteger);
    if FRecordHeader.recType < $F000 then
    begin
      FRecordHeader.recType := $F000;
      Break;
    end;
    ARecordReader := FRecordReaders[FRecordHeader.recType];
    ASavePos := Stream.Position;
    if not Assigned(ARecordReader) or not ARecordReader then
      Stream.Position := ASavePos + Integer(FRecordHeader.recSize);
  end;
  ApplyPropertyValues;
end;

procedure TdxSpreadSheetMSODrawingReader.Initialize;
var
  I: Integer;
begin
  FRecordReaders[oaBlipStoreContainer] := ReadBlipStoreContainer;
  FRecordReaders[oaDrawingObjectsContainer] := ReadDrawingObjectsContainer;
  FRecordReaders[oaDrawingContainer] := ReadDrawingContainer;
  FRecordReaders[oaFileDrawingGroupRecord] := ReadDrawingGroupRecord;
  FRecordReaders[oaFileDrawingRecord] := ReadDrawingRecord;
  FRecordReaders[oaFileBlipStoreEntry] := ReadBlipStoryEntry;
  FRecordReaders[oaFileShape] := ReadFileShape;
  FRecordReaders[oaPropertiesTable] := ReadPropertiesTable;
  FRecordReaders[oaClientAnchor] := ReadClientAnchor;
  FRecordReaders[oaSplitMenuColorContainer] := ReadSplitMenuColorContainer;
  FRecordReaders[oaTertiaryPropertiesTable] := ReadPropertiesTable;
  FRecordReaders[oaShapeContainer] := ReadShapeContainer;
  FRecordReaders[oaShapeGroupContainer] := ReadShapeGroupContainer;
  FRecordReaders[oaShapeGroupCoordinates] := ReadShapeGroupCoordinates;
  for I := oaBlipFirst to oaBlipLast do
    FRecordReaders[I] := ReadBlip;

  RegisterPropertyReader($007f, Property_BooleanProtection);
  RegisterPropertyReader($0100, Property_DrawingCropFromTop);
  RegisterPropertyReader($0101, Property_DrawingCropFromBottom);
  RegisterPropertyReader($0102, Property_DrawingCropFromLeft);
  RegisterPropertyReader($0103, Property_DrawingCropFromRight);
  RegisterPropertyReader($c105, Property_DrawingBlipName);
  RegisterPropertyReader($0106, Property_DrawingBlipFlags);
  RegisterPropertyReader($013f, Property_DrawingBlipBooleanProperties);
  RegisterPropertyReader($01bf, Property_DrawingFillStyleBooleanProperties);
  RegisterPropertyReader($01cb, Property_DrawingLineWidth);
  RegisterPropertyReader($01cc, Property_DrawingLineMiterLimit);
  RegisterPropertyReader($01cd, Property_DrawingLineCompoundType);
  RegisterPropertyReader($01ce, Property_DrawingLineDashing);
  RegisterPropertyReader($01d6, Property_DrawingLineJoinStyle);
  RegisterPropertyReader($01d7, Property_DrawingLineCapStyle);
  RegisterPropertyReader($01ff, Property_DrawingLineStyleBooleanProperties);
  RegisterPropertyReader($0304, Property_DrawingBlackWhiteMode);
  RegisterPropertyReader($033f, Property_DrawingShapeBooleanProperties);
  RegisterPropertyReader($4104, Property_DrawingBlipIdentifier);
  RegisterPropertyReader($0080, Property_DrawingTextIdentifier);
  RegisterPropertyReader($c380, Property_DrawingShapeName);
  RegisterPropertyReader($c381, Property_DrawingShapeDescription);
  RegisterPropertyReader($c382, Property_DrawingShapeHyperlink);
  RegisterPropertyReader($c38d, Property_DrawingShapeTooltip);
  RegisterPropertyReader($03bf, Property_DrawingGroupShapeBooleanProperties);
  RegisterPropertyReader($053f, Property_DiagramBooleanProperties);
  RegisterPropertyReader($038F, Property_DrawingGroupShapePosH);
  RegisterPropertyReader($0390, Property_DrawingGroupShapePosRelH);
  RegisterPropertyReader($0391, Property_DrawingGroupShapePosV);
  RegisterPropertyReader($0392, Property_DrawingGroupShapePosRelV);
  RegisterPropertyReader($07c0, Property_DrawingGroupShape2PctHoriz);
  RegisterPropertyReader($07c1, Property_DrawingGroupShape2PctVert);
  RegisterPropertyReader($07c2, Property_DrawingGroupShape2PctHorizPos);
  RegisterPropertyReader($07c3, Property_DrawingGroupShape2PctVertPos);
  RegisterPropertyReader($07c4, Property_DrawingGroupShape2SizeRelH);
  RegisterPropertyReader($07c5, Property_DrawingGroupShape2SizeRelV);
  RegisterPropertyReader($0004, Property_DrawingRotation);
  RegisterPropertyReader($01c0, Property_DrawingLineColor);
  RegisterPropertyReader($0081, Property_DrawingTextLeft);
  RegisterPropertyReader($0082, Property_DrawingTextTop);
  RegisterPropertyReader($0083, Property_DrawingTextRight);
  RegisterPropertyReader($0084, Property_DrawingTextBottom);
  RegisterPropertyReader($0085, Property_DrawingWrapText);
  RegisterPropertyReader($0384, Property_DrawingWrapLeftDistance);
  RegisterPropertyReader($0385, Property_DrawingWrapTopDistance);
  RegisterPropertyReader($0386, Property_DrawingWrapRightDistance);
  RegisterPropertyReader($0387, Property_DrawingWrapBottomDistance);
  RegisterPropertyReader($4186, Property_DrawingFillBlipIdentifier);
  RegisterPropertyReader($0201, Property_DrawingShadowColor);
  RegisterPropertyReader($023f, Property_DrawingShadowStyleBooleanProperties);
  RegisterPropertyReader($0158, Property_DrawingConnectionPointsType);
  RegisterPropertyReader($00bf, Property_DrawingTextBooleanProperties);
  RegisterPropertyReader($008b, Property_DrawingTextDirection);
  RegisterPropertyReader($0180, Property_OfficeDrawingFillType);
  RegisterPropertyReader($0181, Property_DrawingFillColor);
  RegisterPropertyReader($0182, Property_DrawingFillOpacity);
  RegisterPropertyReader($0183, Property_DrawingFillBackColor);
  RegisterPropertyReader($0184, Property_DrawingFillBackOpacity);
  RegisterPropertyReader($0185, Property_DrawingFillBWColor);
  RegisterPropertyReader($0186, Property_DrawingFillBlip);
  RegisterPropertyReader($0187, Property_DrawingFillBlipName);
  RegisterPropertyReader($0188, Property_DrawingFillBlipFlags);
  RegisterPropertyReader($0189, Property_DrawingFillWidth);
  RegisterPropertyReader($018a, Property_DrawingFillHeight);
  RegisterPropertyReader($018b, Property_DrawingFillAngle);
  RegisterPropertyReader($018c, Property_DrawingFillFocus);
  RegisterPropertyReader($018d, Property_DrawingFillToLeft);
  RegisterPropertyReader($018e, Property_DrawingFillToTop);
  RegisterPropertyReader($018f, Property_DrawingFillToRight);
  RegisterPropertyReader($0190, Property_DrawingFillToBottom);
  RegisterPropertyReader($0191, Property_DrawingFillRectLeft);
  RegisterPropertyReader($0192, Property_DrawingFillRectTop);
  RegisterPropertyReader($0193, Property_DrawingFillRectRight);
  RegisterPropertyReader($0194, Property_DrawingFillRectBottom);
  RegisterPropertyReader($0195, Property_DrawingFillDzType);
  RegisterPropertyReader($0196, Property_DrawingFillShadePreset);
  RegisterPropertyReader($C197, Property_DrawingFillShadeColors);
  RegisterPropertyReader($0198, Property_DrawingFillOriginX);
  RegisterPropertyReader($0199, Property_DrawingFillOriginY);
  RegisterPropertyReader($019a, Property_DrawingFillShapeOriginX);
  RegisterPropertyReader($019b, Property_DrawingFillShapeOriginY);
  RegisterPropertyReader($019c, Property_DrawingFillShadeType);
  RegisterPropertyReader($019e, Property_DrawingFillColorExt);
  RegisterPropertyReader($019f, Property_DrawingFillReserved415);
  RegisterPropertyReader($01a0, Property_DrawingFillTintShade);
  RegisterPropertyReader($01a1, Property_DrawingFillReserved417);
  RegisterPropertyReader($01a2, Property_DrawingFillBackColorExt);
  RegisterPropertyReader($01a3, Property_DrawingFillReserved419);
  RegisterPropertyReader($01a4, Property_DrawingFillBackTintShade);
  RegisterPropertyReader($01a5, Property_DrawingFillReserved421);
  RegisterPropertyReader($01a6, Property_DrawingFillReserved422);
  RegisterPropertyReader($01a7, Property_DrawingFillReserved423);
  RegisterPropertyReader($C3A9, Property_DrawingShapeDocument);
end;

function TdxSpreadSheetMSODrawingReader.LoadMetaFile(AStream: TMemoryStream; AIsEMF: Boolean): TdxSmartImage;
var
  ASize, ASave: Integer;
  ACanvasSize: TSize;
  ABounds: TRect;
  AFilter: Byte;
  ACompression: Byte;
  AMetaFile: TMetaFile;
  ASourceBits, ADestBits: Pointer;
  AData: TMemoryStream;
begin
  Result := TdxSmartImage.Create;
  AStream.ReadBuffer(ASize, SizeOf(ASize));
  AStream.ReadBuffer(ABounds, SizeOf(ABounds));
  AStream.ReadBuffer(ACanvasSize, SizeOf(ACanvasSize));
  ACanvasSize.cx := dxEMUToPixels(ACanvasSize.cx);
  ACanvasSize.cy := dxEMUToPixels(ACanvasSize.cy);
  AStream.ReadBuffer(ASave, SizeOf(ASave));
  AStream.ReadBuffer(ACompression, SizeOf(ACompression));
  AStream.ReadBuffer(AFilter, SizeOf(AFilter));
  ASourceBits := @PByteArray(AStream.Memory)^[AStream.Position];
  AMetaFile := TMetafile.Create;
  try
    if ACompression = 0 then
    begin
      GetMem(ADestBits, ASize);
      ZDecompress(ASourceBits, ASave, ADestBits, ASize, ASize);
    end
    else
      ADestBits := ASourceBits;
    AMetaFile.SetSize(ACanvasSize.cx, ACanvasSize.cy);
    AMetaFile.Handle := SetEnhMetafileBits(ASize, ADestBits);
    AData := TMemoryStream.Create;
    try
      AMetaFile.SaveToStream(AData);
      AData.Position := 0;
      Result.LoadFromStream(AData);
      Result.HandleNeeded;
    finally
      AData.Free;
    end;
  finally
    if ACompression = 0 then
      FreeMem(ADestBits);
    AMetaFile.Free;
  end;
end;

function TdxSpreadSheetMSODrawingReader.MakeAlphaColor(
  const AColor: TColor; AAlpha: Byte = 255; AReplaceAlpha: Boolean = False): TColor;
begin
  with TRGBQuad(AColor) do
    if AReplaceAlpha then
      Result := dxMakeAlphaColor(AAlpha, rgbBlue, rgbGreen, rgbRed)
    else
      Result := dxMakeAlphaColor(Max(0, Min(255, AAlpha - rgbReserved)), rgbBlue, rgbGreen, rgbRed);
end;

procedure TdxSpreadSheetMSODrawingReader.ApplyPropertyValues;

  procedure ApplyBrushColor(ABrush: TdxGPBrush; AColor: TdxAlphaColor);
  begin
    if (AColor <> TdxAlphaColors.Empty) and (AColor <> TdxAlphaColors.Default) then
    begin
      ABrush.Style := gpbsSolid;
      ABrush.Color := AColor;
    end
    else
      ABrush.Style := gpbsClear;
  end;

var
  AContainer: TdxSpreadSheetShapeContainer;
  AContainerBounds: TRect;
  I: Integer;
begin
  if (Owner.CurrentSheet = nil) or FHidden then
    Exit;

  AContainer := CreateContainer;

  if FAnchorAssigned then
  begin
    AContainer.AnchorType := catTwoCell;
    AContainer.AnchorPoint1.Cell := Owner.CurrentSheet.CreateCell(FAnchor.Row1, FAnchor.Col1);
    AContainer.AnchorPoint1.Offset := Point(FAnchor.Col1Offset, FAnchor.Row1Offset);
    AContainer.AnchorPoint2.Cell := Owner.CurrentSheet.CreateCell(FAnchor.Row2, FAnchor.Col2);
    AContainer.AnchorPoint2.Offset := Point(FAnchor.Col2Offset, FAnchor.Row2Offset);
    AContainer.Transform.RotationAngle := FRotationAngle;

    if FAnchorType <> catTwoCell then
    begin
      AContainerBounds := TdxSpreadSheetContainerAccess(AContainer).Calculator.CalculateBounds;
      try
        AContainer.AnchorType := FAnchorType;
        if FAnchorType <> catOneCell then
          AContainer.AnchorPoint1.Cell := nil;
        AContainer.AnchorPoint2.Cell := nil;
      finally
        TdxSpreadSheetContainerAccess(AContainer).Calculator.UpdateAnchors(AContainerBounds);
      end;
    end;
  end;

  if HasShape then
    AContainer.Shape.ShapeType := TdxSpreadSheetShapeType(IFThen(FShapeType > 3, 1, FShapeType) - 1);

  AContainer.Shape.Pen.Style := FPenStyle;
  AContainer.Shape.Pen.Width := FPenWidth;
  ApplyBrushColor(AContainer.Shape.Pen.Brush, FPenColor);

  if HasShape or HasBlip then
    ApplyBrushColor(AContainer.Shape.Brush, FBrushColor)
  else
    ApplyBrushColor(AContainer.Shape.Brush, TdxAlphaColors.Default);

  if FBrushColors.Count > 0 then
  begin
    AContainer.Shape.Brush.GradientPoints.Add(0, FBrushColor);
    for I := 0 to FBrushColors.Count - 1 do
    begin
      AContainer.Shape.Brush.GradientPoints.Add(
        Min(1, FBrushColors[I].Position / (MAXWORD + 1)),
        MakeAlphaColor(FBrushColors[I].Color));
    end;
    AContainer.Shape.Brush.GradientPoints.Add(1, FBrushBottomColor);
    AContainer.Shape.Brush.Style := gpbsGradient;
    AContainer.Shape.Brush.GradientMode := FBrushGradientMode;
    if FBrushGradientModeInverseOrder then
      AContainer.Shape.Brush.GradientPoints.InvertOrder;
  end;

  if HasBlip then
  begin
    dxSpreadSheetInitializeBrushPattern(AContainer.Shape.Brush, GetGraphic(FFillBlipID),
      FBrushBottomColor, FBrushColor, Owner.Textures.IndexOf(GetGraphic(FFillBlipID)) >= 0);
  end;

  AContainer.Hyperlink := FHyperlink;
  FHyperlink := nil;
end;

procedure TdxSpreadSheetMSODrawingReader.ClearPropertyValues;
begin
  FHidden := False;
  FFillBlipID := -1;
  FBlipID := -1;
  FPenWidth := 1;
  FillChar(FAnchor, SizeOf(FAnchor), 0);
  FAnchorAssigned := False;
  FBrushStyle := gpbsClear;
  FBrushGradientMode := gpbgmVertical;
  FBrushGradientModeInverseOrder := False;
  FRotationAngle := 0;
  FPenStyle := gppsSolid;
  FBrushColors.Clear;
  FPenColor := TdxAlphaColors.Default;
  FBrushColor := TdxAlphaColors.Empty;
  FAnchorType := catTwoCell;
  FTextPadding := dxEMUToPixels(dxXLSXDefaultTextPadding);
  FWrapText := XLSDefaultWrapText;
  DeleteHyperlink;
end;

procedure TdxSpreadSheetMSODrawingReader.DeleteHyperlink;
var
  AIndex: Integer;
begin
  if FHyperlink = nil then
    Exit;
  AIndex := Owner.Hyperlinks.IndexOfObject(FHyperlink);
  FreeAndNil(FHyperlink);
  if AIndex >=0 then
    Owner.Hyperlinks.Delete(AIndex);
end;

function TdxSpreadSheetMSODrawingReader.GetGraphic(AIndex: Integer): TdxSmartImage;
begin
  if (AIndex >= 0) and (AIndex < Owner.Images.Count) then
    Result := Owner.Images[AIndex]
  else
    Result := nil;
end;

function TdxSpreadSheetMSODrawingReader.HasBlip: Boolean;
begin
  Result := (FBlipID >=0) and (FBlipID < Owner.Images.Count) and (Owner.Images[FBlipID] <> nil);
end;

function TdxSpreadSheetMSODrawingReader.HasShape: Boolean;
begin
  Result := FShapeType > 0;
end;

function TdxSpreadSheetMSODrawingReader.ReadBlip: Boolean;
var
  AStream: TMemoryStream;
  AImage: TdxSmartImage;
  AFormat: TdxImageDataFormat;
  AOffset: Integer;
begin
  Owner.Images.Count := BlipCount;
  AImage := nil;
  AFormat := dxImageUnknown;
  AOffset := 17;
  case RecordHeader.recInstance of
    oaBlipJPG_RGB, oaBlipJPG_CMYK:
      AFormat := dxImageJpeg;
    oaBlipPNG:
      AFormat := dxImagePng;
    oaBlipDIB:
      AFormat := dxImageMemoryBmp;
    oaBlipTIFF:
      AFormat := dxImageTiff;
    oaBlipEMF, oaBlipWMF:
      begin
        Dec(AOffset);
        AFormat := dxImageEmf;
      end;
  end;
  Stream.Position := Stream.Position + AOffset;
  if AFormat <> dxImageUnknown then
  begin
    AStream := TMemoryStream.Create;
    try
      if AFormat = dxImageMemoryBmp then
        RecreateStreamFromDIB(AStream, Stream, Integer(RecordHeader.recSize) - AOffset)
      else
        AStream.CopyFrom(Stream, Integer(RecordHeader.recSize) - AOffset);

      AStream.Position := 0;
      if AFormat = dxImageEmf then
        AImage := LoadMetaFile(AStream, RecordHeader.recInstance = oaBlipEMF)
      else
      begin
        AImage := TdxSmartImage.Create;
        AImage.LoadFromStream(AStream);
        if (AFormat = dxImageMemoryBmp) and (AImage.Width > 0) then
          Owner.Textures.Add(AImage);
      end;
    finally
      AStream.Free;
    end;
  end;
  Owner.Images[BlipCount - 1] := AImage;
  Result := AImage <> nil;
end;

function TdxSpreadSheetMSODrawingReader.ReadBlipStoreContainer: Boolean;
begin
  Result := True;
end;

function TdxSpreadSheetMSODrawingReader.ReadBlipStoryEntry: Boolean;
var
  AEntry: TdxMSOBlipStoreEntry;
begin
  Stream.ReadBuffer(AEntry, SizeOf(TdxMSOBlipStoreEntry));
  Inc(FBlipCount);
  Result := True;
end;

function TdxSpreadSheetMSODrawingReader.ReadClientAnchor: Boolean;
begin
  FAnchorAssigned := True;
  Stream.ReadBuffer(FAnchor, SizeOf(TdxMSOAnchor));
  case FAnchor.Options and 3 of
    2: FAnchorType := catOneCell;
    3: FAnchorType := catAbsolute;
  end;
  FAnchor.Row1Offset := VOffsetToInt(FAnchor.Row1, FAnchor.Row1Offset);
  FAnchor.Col1Offset := HOffsetToInt(FAnchor.Col1, FAnchor.Col1Offset);
  FAnchor.Row2Offset := VOffsetToInt(FAnchor.Row2, FAnchor.Row2Offset);
  FAnchor.Col2Offset := HOffsetToInt(FAnchor.Col2, FAnchor.Col2Offset);
  Result := True;
end;

function TdxSpreadSheetMSODrawingReader.ReadDrawingContainer: Boolean;
begin
  Result := True;
end;

function TdxSpreadSheetMSODrawingReader.ReadDrawingGroupRecord: Boolean;
var
  AGroup: TdxMSODrawingGroupRecord;
begin
  Result := True;
  Stream.ReadBuffer(AGroup, SizeOf(AGroup));
  Stream.Position := Stream.Position + (AGroup.sidcl - 1) * 8;
end;

function TdxSpreadSheetMSODrawingReader.ReadDrawingObjectsContainer: Boolean;
begin
  Result := True;
end;

function TdxSpreadSheetMSODrawingReader.ReadDrawingRecord: Boolean;
begin
  Result := False
end;

function TdxSpreadSheetMSODrawingReader.ReadFileShape: Boolean;
begin
// recInstance must be following
// 1 - rect
// 2 - round rect
// 3 - ellipse
// 4 - diamond shape
// 5 - triangle shape
// 6 -  right triangle
// 7 - parallelogram
// 8 - trapezoid
// 9 - hexagon
// A - octagon
// B - Plus
// C - Star
// D - Arrow
// F - irregular pentagon
//10 - cube
//11 - speech balloon
//12 - seal
//13 - curved arc
//14 - line
//15 - plaque
//16 - cylinder
//17 - donut
//202 - textbox

  FShapeFlags := Reader.ReadInteger;
  FShapeID := Reader.ReadInteger;

  FFlipHorizontally := RecordHeader.recInstance and fsFlipH = fsFlipH;
  FFlipVertically := RecordHeader.recInstance and fsFlipV = fsFlipV;

  FShapeType := RecordHeader.recInstance;
  if RecordHeader.recInstance and fsHaveShapeType = fsHaveShapeType then
    FShapeType := -1;

  Result := True;
end;

function TdxSpreadSheetMSODrawingReader.ReadPropertiesTable: Boolean;
var
  AComplexData: array of Byte;
  AComplexProperties: TList<TdxXLSDrawingProperty>;
  AComplexPropertySize: TList<Integer>;
  AKey: Word;
  ASavePos, APropIndex, AValue: Integer;
begin
  ASavePos := Stream.Position;
  AComplexProperties := TList<TdxXLSDrawingProperty>.Create;
  AComplexPropertySize := TList<Integer>.Create;
  try
    for APropIndex := 0 to RecordHeader.recInstance - 1 do
    begin
      AKey := Reader.ReadWord;
      AValue := Reader.ReadInteger;
      if AKey and $8000 <> 0 then
      begin
        if Assigned(FPropertyReaders[AKey]) then
          AComplexProperties.Add(FPropertyReaders[AKey])
        else
          AComplexProperties.Add(Property_UndefinedComplexProperty);
        AComplexPropertySize.Add(AValue);
      end
      else
        if Assigned(FPropertyReaders[AKey]) then
          FPropertyReaders[AKey](AValue, []);
    end;
    for APropIndex := 0 to AComplexProperties.Count - 1 do
    begin
      SetLength(AComplexData, AComplexPropertySize[APropIndex]);
      ReadComplexData(AComplexPropertySize[APropIndex], AComplexData);
      AComplexProperties[APropIndex](AComplexPropertySize[APropIndex], AComplexData);
    end;
  finally
    AComplexProperties.Free;
    AComplexPropertySize.Free;
    Stream.Position := ASavePos + Integer(RecordHeader.recSize);
  end;
  Result := True;
end;

function TdxSpreadSheetMSODrawingReader.ReadShapeGroupContainer: Boolean;
begin
  Result := True;
end;

function TdxSpreadSheetMSODrawingReader.ReadShapeContainer: Boolean;
begin
  Result := True;
end;

function TdxSpreadSheetMSODrawingReader.ReadShapeGroupCoordinates: Boolean;
var
  R: TRect;
begin
  R := Reader.ReadRect;
  Result := True;
end;

function TdxSpreadSheetMSODrawingReader.ReadSplitMenuColorContainer: Boolean;
begin
  Result := False;
end;

procedure TdxSpreadSheetMSODrawingReader.RegisterPropertyReader(AKey: Word; AProc: TdxXLSDrawingProperty);
begin
  FPropertyReaders[AKey] := AProc;
end;

function TdxSpreadSheetMSODrawingReader.CreateContainer: TdxSpreadSheetShapeContainer;
var
  AGraphic: TdxSmartImage;
begin
  AGraphic := GetGraphic(FBlipID);
  if AGraphic <> nil then
  begin
    Owner.CurrentSheet.Containers.Add(TdxSpreadSheetPictureContainer, Result);
    TdxSpreadSheetPictureContainer(Result).Picture.Image := AGraphic;
  end
  else
    if FShapeType = 202 then
    begin
      Owner.CurrentSheet.Containers.Add(TdxSpreadSheetTextBoxContainer, Result);
      TdxSpreadSheetTextBoxContainer(Result).TextBox.ContentOffsets := FTextPadding;
      TdxSpreadSheetTextBoxContainer(Result).TextBox.WordWrap := FWrapText;
    end
    else
      Owner.CurrentSheet.Containers.Add(TdxSpreadSheetShapeContainer, Result);
end;

function TdxSpreadSheetMSODrawingReader.GetStream: TStream;
begin
  Result := Owner.RecordReader;
end;

procedure TdxSpreadSheetMSODrawingReader.Property_BooleanProtection(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DiagramBooleanProperties(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingBlackWhiteMode(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingBlipBooleanProperties(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingBlipFlags(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingBlipIdentifier(
  const AValue: Integer; const AComplexData: array of Byte);
begin
  FBlipID := AValue - 1;
end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingBlipName(
  const AValue: Integer; const AComplexData: array of Byte);
begin
end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingConnectionPointsType(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingCropFromBottom(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingCropFromLeft(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingCropFromRight(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingCropFromTop(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingFillAngle(
  const AValue: Integer; const AComplexData: array of Byte);
var
  P: TdxXLSFixedPoint;
begin
  Move(Pointer(@AValue)^, Pointer(@P)^, SizeOf(P));
  FBrushGradientMode := dxGetNearestGradientMode(P.Fractional, FBrushGradientModeInverseOrder);
end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingFillBackColor(
  const AValue: Integer; const AComplexData: array of Byte);
begin
  FBrushBottomColor := MakeAlphaColor(AValue);
end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingFillBackColorExt(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingFillBackOpacity(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingFillBackTintShade(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingFillBlip(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingFillBlipFlags(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingFillBlipIdentifier(
  const AValue: Integer; const AComplexData: array of Byte);
begin
  FFillBlipID := AValue - 1;
end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingFillBlipName(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingFillBWColor(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingFillColor(
  const AValue: Integer; const AComplexData: array of Byte);
begin
  if AValue <> clDefaultFillColor then
  begin
    if AValue and $8000000 = $8000000 then
      FBrushColor := dxMakeAlphaColor(Owner.GetColor(AValue and $FF))
    else
      FBrushColor := dxMakeAlphaColor(AValue);
  end;
end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingFillColorExt(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingFillDzType(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingFillFocus(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingFillHeight(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingFillOpacity(
  const AValue: Integer; const AComplexData: array of Byte);
begin
  FBrushColor := dxMakeAlphaColor(dxAlphaColorToColor(FBrushColor), MulDiv(AValue, 255, 65536));
end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingFillOriginX(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingFillOriginY(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingFillRectBottom(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingFillRectLeft(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingFillRectRight(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingFillRectTop(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingFillReserved415(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingFillReserved417(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingFillReserved419(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingFillReserved421(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingFillReserved422(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingFillReserved423(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingShapeDocument(
  const AValue: Integer; const AComplexData: array of Byte);
begin
end;

procedure TdxSpreadSheetMSODrawingReader.Property_UndefinedComplexProperty(
  const AValue: Integer; const AComplexData: array of Byte);
begin
end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingFillShadeColors(
  const AValue: Integer; const AComplexData: array of Byte);
var
  I: Integer;
  AColorRef: PdxXLSColorRef;
begin
  if Length(AComplexData) = 0 then Exit;
  AColorRef := @AComplexData[6];
  for I := 0 to PWord(@AComplexData[0])^ - 1 do
  begin
    FBrushColors.Add(AColorRef^);
    Inc(AColorRef);
  end;
end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingFillShadePreset(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingFillShadeType(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingFillShapeOriginX(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingFillShapeOriginY(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingFillStyleBooleanProperties(
  const AValue: Integer; const AComplexData: array of Byte);
begin
// $100010
end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingFillTintShade(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingFillToBottom(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingFillToLeft(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingFillToRight(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingFillToTop(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingFillWidth(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingGroupShape2PctHoriz(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingGroupShape2PctHorizPos(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingGroupShape2PctVert(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingGroupShape2PctVertPos(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingGroupShape2SizeRelH(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingGroupShape2SizeRelV(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingGroupShapeBooleanProperties(
  const AValue: Integer; const AComplexData: array of Byte);
begin
  if AValue and $20000 <> 0 then
    FHidden := AValue and $2 <> 0;
end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingGroupShapePosH(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingGroupShapePosRelH(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingGroupShapePosRelV(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingGroupShapePosV(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingLineCapStyle(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingLineColor(
  const AValue: Integer; const AComplexData: array of Byte);
begin
  if AValue <> clDefaultLineColor then
    FPenColor := MakeAlphaColor(AValue);
end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingLineCompoundType(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingLineDashing(
  const AValue: Integer; const AComplexData: array of Byte);
const
  StyleToGPPenStyle: array[0..$A] of TdxGPPenStyle =
    (gppsSolid, gppsSolid, gppsDot, gppsDashDot, gppsDashDotDot, gppsDot,
     gppsDash, gppsDashDot, gppsDashDot, gppsDashDot, gppsDashDot);
begin
  FPenStyle := StyleToGPPenStyle[AValue and $F];
end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingLineJoinStyle(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingLineMiterLimit(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingLineStyleBooleanProperties(
  const AValue: Integer; const AComplexData: array of Byte);
begin
      // 524296
end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingLineWidth(
  const AValue: Integer; const AComplexData: array of Byte);
begin
  FPenWidth := dxEMUToPixels(AValue);
end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingRotation(const AValue: Integer; const AComplexData: array of Byte);
begin
  FRotationAngle := AValue / 65536;
  if FRotationAngle > 0 then
    while FRotationAngle > 360 do
      FRotationAngle := FRotationAngle - 360
  else
    while Abs(FRotationAngle) > 360 do
      FRotationAngle := FRotationAngle + 360;
end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingShadowColor(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingShadowStyleBooleanProperties(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingShapeBooleanProperties(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingShapeDescription(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingShapeHyperlink(
  const AValue: Integer; const AComplexData: array of Byte);
var
  AHelper: TdxBIFFHyperlinkHelper;
  AStream: TMemoryStream;
begin
  DeleteHyperlink;
  FHyperlink := Owner.CurrentSheet.Hyperlinks.Add(cxInvalidRect);
  AHelper := TdxBIFFHyperlinkHelper.Create(FHyperlink);
  try
    AStream := TMemoryStream.Create;
    try
      AStream.WriteBuffer(AComplexData[0], Length(AComplexData));
      AStream.Position := 0;
      AHelper.LoadFromStream(AStream);
      Owner.Hyperlinks.AddObject(AHelper.Value, FHyperlink);
    finally
      AStream.Free;
    end;
  finally
    AHelper.Free;
  end;
end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingShapeName(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingShapeTooltip(
  const AValue: Integer; const AComplexData: array of Byte);
var
  AScreentip: string;
begin
  if Length(AComplexData) > 2 then
  begin
    SetLength(AScreentip, Length(AComplexData) div 2 - 1);
    Move(AComplexData[0], AScreentip[1], Length(AComplexData) - 2);
    if FHyperlink <> nil then
      FHyperlink.ScreenTip := AScreentip;
  end;
end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingTextBooleanProperties(
  const AValue: Integer; const AComplexData: array of Byte);
begin
  if AValue and $80000 <> 0 then
  begin
    if AValue and $8 <> 0 then
      FTextPadding := cxNullRect;
  end;
end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingTextBottom(
  const AValue: Integer; const AComplexData: array of Byte);
begin
  FTextPadding.Bottom := dxEMUToPixels(AValue);
end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingTextDirection(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingTextIdentifier(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingTextLeft(
  const AValue: Integer; const AComplexData: array of Byte);
begin
  FTextPadding.Left := dxEMUToPixels(AValue);
end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingTextRight(
  const AValue: Integer; const AComplexData: array of Byte);
begin
  FTextPadding.Right := dxEMUToPixels(AValue);
end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingTextTop(
  const AValue: Integer; const AComplexData: array of Byte);
begin
  FTextPadding.Top := dxEMUToPixels(AValue);
end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingWrapBottomDistance(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingWrapLeftDistance(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingWrapRightDistance(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingWrapText(
  const AValue: Integer; const AComplexData: array of Byte);
begin
  FWrapText := AValue < 2;
end;

procedure TdxSpreadSheetMSODrawingReader.Property_DrawingWrapTopDistance(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.Property_OfficeDrawingFillType(
  const AValue: Integer; const AComplexData: array of Byte);
begin

end;

procedure TdxSpreadSheetMSODrawingReader.ReadComplexData(ASize: Integer; var AData: array of Byte);
begin
  Stream.ReadBuffer(AData[0], Length(AData));
end;

function TdxSpreadSheetMSODrawingReader.HOffsetToInt(AColumnIndex, AOffset: Integer): Integer;
begin
  if TdxSpreadSheetTableView(Owner.CurrentSheet).Columns[AColumnIndex] = nil then
    Result := TdxSpreadSheetTableView(Owner.CurrentSheet).Columns.DefaultSize
  else
    Result := TdxSpreadSheetTableView(Owner.CurrentSheet).Columns[AColumnIndex].Size;
  Result := Round(Result * AOffset / 1024);
end;

function TdxSpreadSheetMSODrawingReader.VOffsetToInt(ARowIndex, AOffset: Integer): Integer;
begin
  if TdxSpreadSheetTableView(Owner.CurrentSheet).Rows[ARowIndex] = nil then
    Result := TdxSpreadSheetTableView(Owner.CurrentSheet).Rows.DefaultSize
  else
    Result := TdxSpreadSheetTableView(Owner.CurrentSheet).Rows[ARowIndex].Size;
  Result := Round(Result * AOffset / 256);
end;

{ TdxSpreadSheetMSODrawingWriter }

constructor TdxSpreadSheetMSODrawingWriter.Create(AOwner: TdxSpreadSheetXLSWriter);
begin
  inherited Create;
  FOwner := AOwner;
  FStream := TMemoryStream.Create;
  FWriter := TcxWriter.Create(FStream);
  FImageBinaryData := TMemoryStream.Create;
  FShape := TdxSpreadSheetShape.Create(AOwner.SpreadSheet);
end;

destructor TdxSpreadSheetMSODrawingWriter.Destroy;
begin
  Owner.Writer.WriteStream(Stream);
  FreeAndNil(FStream);
  FreeAndNil(FWriter);
  FreeAndNil(FImageBinaryData);
  FreeAndNil(FShape);
  inherited Destroy;
end;

procedure TdxSpreadSheetMSODrawingWriter.WriteContainer(
  AContainer: TdxSpreadSheetContainer; AIsFirstOnTheSheet: Boolean; var ASize: Integer);
var
  ARecord, AShapeGroup, AShapeRecord: Integer;
begin
  Initialize(AContainer);
  if GroupID < 0 then
    Exit;

  if AIsFirstOnTheSheet then
  begin
    ARecord := MakeRecordHeader(15, 0, oaDrawingObjectsContainer);
    try
      WriteFileDrawingRecord;
      AShapeGroup := MakeRecordHeader(15, 0, oaShapeGroupContainer);
      try
        AShapeRecord := MakeRecordHeader(15, 0, oaShapeContainer);
        WriteShapeGroupCoordinates;
        WriteFileShape(False);
        EndRecord(AShapeRecord);
        WriteShapeContainer;
      finally
        EndRecord(AShapeGroup, IsTextBox);
      end;
    finally
      EndRecord(ARecord, IsTextBox);
    end;
  end
  else
    WriteShapeContainer;

  if IsTextBox then
    Inc(ASize, 8);
  Inc(ASize, Stream.Size);
end;

procedure TdxSpreadSheetMSODrawingWriter.WriteDrawingGroups;
var
  ARecord, ABlipContainer, I: Integer;
begin
  ARecord := MakeRecordHeader(15, 0, oaDrawingContainer);
  try
    WriteDrawingGroupRecord;
    if Owner.Pictures.Count > 0 then
    begin
      ABlipContainer := MakeRecordHeader(15, CalcDrawingsCount + 1, oaBlipStoreContainer);
      try
        for I := 0 to Owner.Pictures.Count - 1 do
        try
          Initialize(Owner.Pictures[I]);
          WriteBlipStoryEntry;
          Writer.Stream.WriteBuffer(ImageBinaryData.Memory^, ImageBinaryData.Size);
        finally
          FCurrentObjectRefCount := 0;
          FContainer := nil;
        end;
        TdxSpreadSheetXLSWriterAccess(Owner).AddObject;
      finally
        EndRecord(ABlipContainer);
      end;
    end;
    WriteSplitMenuColorContainer;
  finally
    EndRecord(ARecord);
  end;
end;

procedure TdxSpreadSheetMSODrawingWriter.EndRecord(ARecord: Integer; AIncludeSizeOfHeader: Boolean = False);
var
  ASize: Integer;
begin
  ASize := Stream.Size - ARecord - SizeOf(Integer);
  if AIncludeSizeOfHeader then
    Inc(ASize, 2 * SizeOf(Word) + SizeOf(Integer));
  PLongWord(@PByteArray(Stream.Memory)[ARecord])^ := ASize;
end;

procedure TdxSpreadSheetMSODrawingWriter.MakeImageBinaryData;
const
  DataOffset = 25;
var
  AHash: TdxMD4Byte16;
  ARecType, AType: Word;
  AWriter: TcxWriter;
begin
  ImageBinaryData.Clear;
  AWriter := TcxWriter.Create(ImageBinaryData);
  try
    FillChar(AHash, SizeOf(AHash), 0);
    AType := oaBlipPNG;
    ARecType := $F01E;
    if CurrentImage.ImageDataFormat = dxImageJpeg then
    begin
      AType := oaBlipJPG_RGB;
      ARecType := $F01D;
    end;
    AWriter.WriteWord((AType and $0FFF) shl 4);
    AWriter.WriteWord(ARecType);
    //
    AWriter.WriteInteger(0);  // reserve for size
    AWriter.Stream.WriteBuffer(AHash[0], SizeOf(AHash));
    AWriter.WriteByte($FF);
    CurrentImage.SaveToStreamByCodec(ImageBinaryData, ImageDataFormat2StoreFormat[CurrentImage.ImageDataFormat]);
    PInteger(@PByteArray(ImageBinaryData.Memory)^[4])^ := ImageBinaryData.Size - 8;
    dxMD4Calc(@PByteArray(ImageBinaryData.Memory)^[DataOffset], ImageBinaryData.Size - DataOffset, AHash);
    Move(AHash[0], PByteArray(ImageBinaryData.Memory)^[8], SizeOf(AHash));
  finally
    AWriter.Free;
  end;
end;

procedure TdxSpreadSheetMSODrawingWriter.Initialize(AContainer: TdxSpreadSheetContainer);
var
  AImageHandle: TdxSpreadSheetSharedImageHandle;
begin
  FContainer := AContainer;
  FCurrentImage := nil;
  FGroupID  := -1;
  FCurrentObjectRefCount := 1;

  InitializeShape(AContainer);

  if not Owner.Drawings.TryGetValue(AContainer, FGroupID) then
  begin
    if AContainer is TdxSpreadSheetPictureContainer then
    begin
      AImageHandle := TdxSpreadSheetPictureAccess(TdxSpreadSheetPictureContainer(AContainer).Picture).ImageHandle;
      FCurrentImage := AImageHandle.Image;
      FCurrentObjectRefCount := AImageHandle.RefCount;
      Owner.Drawings.TryGetValue(AImageHandle, FGroupID);
    end;
  end;

  FBlipID := Owner.Pictures.IndexOf(AContainer);
  if FBlipID >= 0  then
    Inc(FBlipID);

  if FCurrentImage = nil then
  begin
    if not Shape.Brush.Texture.Empty then
      FCurrentImage := Shape.Brush.Texture;
  end;
end;

procedure TdxSpreadSheetMSODrawingWriter.InitializeShape(AContainer: TdxSpreadSheetContainer);
begin
  if AContainer is TdxSpreadSheetShapeContainer then
    Shape.Assign(TdxSpreadSheetShapeContainer(AContainer).Shape)
end;

function TdxSpreadSheetMSODrawingWriter.IsPicture: Boolean;
begin
  Result := (FBlipID >= 0) and (Container is TdxSpreadSheetPictureContainer)
end;

function TdxSpreadSheetMSODrawingWriter.IsShape: Boolean;
begin
  Result := not (Container is TdxSpreadSheetPictureContainer);
end;

function TdxSpreadSheetMSODrawingWriter.IsTextBox: Boolean;
begin
  Result := TextBoxProperties <> nil;
end;

procedure TdxSpreadSheetMSODrawingWriter.PrepareGradientColors(
  APoints: TdxGPBrushGradientPoints; var AColors: TdxXLSColorRefArray; var ABkColor: TColor);
var
  I: Integer;
  AOffset: Single;
begin
  AOffset := -1;
  SetLength(AColors, APoints.Count);
  for I := 0 to APoints.Count - 1 do
  begin
    if APoints.Offsets[I] > AOffset then
    begin
      AOffset := APoints.Offsets[I];
      ABkColor := APoints.Colors[I];
    end;
    AColors[I].Color := ValidateColor(APoints.Colors[I], clDefaultFillColor);
    AColors[I].Position := Min(MAXWORD + 1, Round(APoints.Offsets[I] * (MAXWORD + 1)));
  end;
end;

procedure TdxSpreadSheetMSODrawingWriter.MakeProperty(APropertyID: Word; AValue: Integer);
begin
  Writer.WriteWord(APropertyID);
  Writer.WriteInteger(AValue);
end;

function TdxSpreadSheetMSODrawingWriter.MakeRecordHeader(AInstance, ARecordType: Word): Integer;
begin
  Writer.WriteWord(AInstance);
  Writer.WriteWord(ARecordType);
  Result := Stream.Position;
  Writer.WriteInteger(0);
end;

function TdxSpreadSheetMSODrawingWriter.MakeRecordHeader(AVersion, AInstance: Byte; ARecordType: Word): Integer;
begin
  Result := MakeRecordHeader((AVersion and $0F) or ((AInstance and $0FFF) shl 4), ARecordType);
end;

procedure TdxSpreadSheetMSODrawingWriter.WriteBlipStoryEntry;
var
  ARecord: Integer;
  AEntry: TdxMSOBlipStoreEntry;
begin
  ARecord := MakeRecordHeader(2, BlipFormatID[CurrentImage.ImageDataFormat], oaFileBlipStoreEntry);
  FillChar(AEntry, SizeOf(AEntry), 0);
  //
  AEntry.btWinType := BlipFormatID[CurrentImage.ImageDataFormat];
  AEntry.btMacType := BlipFormatID[CurrentImage.ImageDataFormat];
  // MD4 UID
  MakeImageBinaryData;
  Move(PByteArray(ImageBinaryData.Memory)^[8], AEntry.rgbUid, SizeOf(TdxMD4Byte16));
  AEntry.tag := $FF;
  AEntry.size := ImageBinaryData.Size;
  AEntry.cRef := CurrentObjectRefCount; //Ref count for images
  //
  Writer.Stream.WriteBuffer(AEntry, SizeOf(AEntry));
  PLongWord(@PByteArray(Stream.Memory)[ARecord])^ := SizeOf(AEntry) + ImageBinaryData.Size;
end;

procedure TdxSpreadSheetMSODrawingWriter.WriteClientAnchor;

  procedure CalculateAnchor(AAnchor: TdxSpreadSheetContainerAnchorPoint; const P: TPoint;
    var AAnchorPoint, AAnchorOffset: TPoint; ACanUseCell: Boolean);
  var
    ATableView: IdxSpreadSheetTableView;
  begin
    AAnchorPoint := cxNullPoint;
    AAnchorOffset := AAnchor.Offset;
    if ACanUseCell and (AAnchor.Cell <> nil) then
      AAnchorPoint := Point(AAnchor.Cell.ColumnIndex, AAnchor.Cell.RowIndex)
    else
      if Supports(CurrentSheet, IdxSpreadSheetTableView, ATableView) then
        if ATableView.GetCellAtAbsolutePoint(P, AAnchorPoint.Y, AAnchorPoint.X) then
        begin
          AAnchorPoint.Y := CheckRow(AAnchorPoint.Y);
          AAnchorPoint.X := CheckColumn(AAnchorPoint.X);
          AAnchorOffset := cxPointOffset(P, ATableView.GetAbsoluteCellBounds(AAnchorPoint.Y, AAnchorPoint.X, False).TopLeft, False);
        end;
  end;

const
  OptionsMap: array[TdxSpreadSheetContainerAnchorType] of Integer = (3, 2, 1);
var
  AAnchor: TdxMSOAnchor;
  AAnchorOffset: TPoint;
  AAnchorPoint: TPoint;
  AContainerBounds: TRect;
  ARecord: Integer;
begin
  ARecord := MakeRecordHeader(0, 0, oaClientAnchor);
  try
    FillChar(AAnchor, SizeOf(AAnchor), 0);
    AAnchor.Options := OptionsMap[Container.AnchorType];
    AContainerBounds := TdxSpreadSheetContainerAccess(Container).Calculator.CalculateBounds;

    CalculateAnchor(Container.AnchorPoint1, AContainerBounds.TopLeft, AAnchorPoint, AAnchorOffset,
      Container.AnchorType <> catAbsolute);
    AAnchor.Col1 := AAnchorPoint.X;
    AAnchor.Col1Offset := ValueToHOffset(AAnchor.Col1, AAnchorOffset.X);
    AAnchor.Row1 := AAnchorPoint.Y;
    AAnchor.Row1Offset := ValueToVOffset(AAnchor.Row1, AAnchorOffset.Y);

    CalculateAnchor(Container.AnchorPoint2, AContainerBounds.BottomRight,
      AAnchorPoint, AAnchorOffset, Container.AnchorType = catTwoCell);
    AAnchor.Col2 := AAnchorPoint.X;
    AAnchor.Col2Offset := ValueToHOffset(AAnchor.Col2, AAnchorOffset.X);
    AAnchor.Row2 := AAnchorPoint.Y;
    AAnchor.Row2Offset := ValueToVOffset(AAnchor.Row2, AAnchorOffset.Y);

    Stream.WriteBuffer(AAnchor, SizeOf(AAnchor));
  finally
    EndRecord(ARecord);
  end;
end;

procedure TdxSpreadSheetMSODrawingWriter.WriteCommonPropertiesTable;
var
  ARecord: Integer;
begin
  ARecord := MakeRecordHeader(3, 3, oaPropertiesTable);
  WriteProperty_DrawingTextBooleanProperties;
  WriteProperty_DrawingFillColor(clDefaultFillColor);
  WriteProperty_DrawingLineColor(clDefaultLineColor);
  EndRecord(ARecord);
end;

procedure TdxSpreadSheetMSODrawingWriter.WriteDrawingContainer;
begin
  // do nothing
end;

procedure TdxSpreadSheetMSODrawingWriter.WriteDrawingGroupRecord;
var
  ARecord: Integer;
  AData: TdxMSODrawingGroupRecord;
begin
  ARecord := MakeRecordHeader(0, 0, oaFileDrawingGroupRecord);
  try
    AData.spidMax := $0400 + CalcMaxShapeIdentifier;
    AData.sidcl := 2;
    AData.cspSaved := CalcShapesCount + CalcDrawingsCount + 1;
    AData.cdgSaved := GroupID;
    Writer.Stream.WriteBuffer(AData, SizeOf(AData));
    Writer.WriteInteger(GroupID);
    Writer.WriteInteger(CalcMaxShapeIdentifier);
  finally
    EndRecord(ARecord);
  end;
end;

procedure TdxSpreadSheetMSODrawingWriter.WriteFileDrawingRecord;
var
  ARecord: Integer;
begin
  ARecord := MakeRecordHeader(0, 1, oaFileDrawingRecord);
  Writer.WriteInteger(CalcShapesCount + CalcDrawingsCount + 1);
  Writer.WriteInteger($0400 + CalcMaxShapeIdentifier - 1);
  EndRecord(ARecord);
end;

procedure TdxSpreadSheetMSODrawingWriter.WriteFileShape(AWriteContainerSettings: Boolean);

  function CalculateID: Integer;
  begin
    Result := $400;
    if AWriteContainerSettings then
      Inc(Result, Owner.Containers[Container]);
  end;

  function CalculateOptions: Integer;
  begin
    if AWriteContainerSettings then
      Result := fsHaveShapeType or fsHaveAnchor
    else
      Result := fsGroup or fsPatriarch;
    if Container.Transform.FlipHorizontally then
      Result := Result or fsFlipH;
    if Container.Transform.FlipVertically then
      Result := Result or fsFlipV;
  end;

  function GetTypeID: Byte;
  const
    Types: array[TdxSpreadSheetShapeType] of Integer = (1, 2, 3);
  begin
    if IsPicture then
      Result := 75
    else
      if IsTextBox then
        Result := 202
      else
        Result := Types[Shape.ShapeType];
  end;

var
  ARecord: Integer;
begin
  ARecord := MakeRecordHeader(2, GetTypeID, oaFileShape);
  Writer.WriteInteger(CalculateID);
  Writer.WriteInteger(CalculateOptions);
  EndRecord(ARecord);
end;

procedure TdxSpreadSheetMSODrawingWriter.WriteShapeContainer;
var
  ARecord: Integer;
begin
  ARecord := MakeRecordHeader(15, 0, oaShapeContainer);
  WriteFileShape(True);
  WriteShapeProperties;
  WriteClientAnchor;
  MakeRecordHeader(0, 0, oaClientData);
  EndRecord(ARecord, IsTextBox);
end;

procedure TdxSpreadSheetMSODrawingWriter.WriteShapeGroupCoordinates;
var
  ARecord: Integer;
begin
  ARecord := MakeRecordHeader(1, 0, oaShapeGroupCoordinates);
  Writer.WriteRect(cxNullRect);
  EndRecord(ARecord);
end;

procedure TdxSpreadSheetMSODrawingWriter.WriteShapeProperties;
var
  ABkColor: TColor;
  AColors: TdxXLSColorRefArray;
  AHasGradient: Boolean;
  ARecord: Integer;
  AHyperlink: TdxSpreadSheetHyperlink;
begin
  AHyperlink := Container.Hyperlink;
  AHasGradient := Shape.Brush.GradientPoints.Count > 0;
  ARecord := MakeRecordHeader(3, 5 + Byte(IsShape) + Byte(BlipID >= 0) + Byte(AHasGradient) * 2 + 5 * Byte(IsTextBox) +
    Byte(AHyperlink <> nil) + Byte((AHyperlink <> nil) and (AHyperlink.ScreenTip <> '')), oaPropertiesTable);
  if IsTextBox then
    WriteTextBoxProperties(TextBoxProperties);
  if IsShape then
    WriteProperty_DrawingOfficeFillType;
  WriteProperty_DrawingRotation(Container.Transform.RotationAngle);
  if BlipID >= 0 then
    WriteProperty_DrawingBlipIdentifier(IsPicture);
  WriteProperty_DrawingFillColor(ValidateColor(Shape.Brush.Color, clDefaultFillColor));
  if AHasGradient then
  begin
    PrepareGradientColors(Shape.Brush.GradientPoints, AColors, ABkColor);
    WriteProperty_DrawingFillBackColor(ValidateColor(ABkColor, clDefaultFillColor));
    WriteProperty_DrawingFillShadeColorsHeader(AColors);
  end;
  WriteProperty_DrawingLineColor(ValidateColor(Shape.Pen.Brush.Color, clDefaultLineColor));
  WriteProperty_DrawingLineDashing(Shape.Pen.Style);
  WriteProperty_DrawingLineWidth(Shape.Pen.Width);
  if AHasGradient then
    WriteProperty_DrawingFillShadeColors(AColors);
  WriteProperty_Hyperlink(AHyperlink);
  EndRecord(ARecord);
end;

procedure TdxSpreadSheetMSODrawingWriter.WriteSplitMenuColorContainer;
var
  ARecord: Integer;
begin
  ARecord := MakeRecordHeader(0, 4, oaSplitMenuColorContainer);
  Writer.WriteInteger($800000D);
  Writer.WriteInteger($800000C);
  Writer.WriteInteger($8000017);
  Writer.WriteInteger($100000F7);
  EndRecord(ARecord);
end;

procedure TdxSpreadSheetMSODrawingWriter.WriteTextBoxProperties(ATextBox: TdxSpreadSheetCustomTextBox);
begin
  MakeProperty($0081, dxPixelsToEMU(TdxSpreadSheetCustomTextBoxAccess(ATextBox).ContentOffsets.Left));
  MakeProperty($0082, dxPixelsToEMU(TdxSpreadSheetCustomTextBoxAccess(ATextBox).ContentOffsets.Top));
  MakeProperty($0083, dxPixelsToEMU(TdxSpreadSheetCustomTextBoxAccess(ATextBox).ContentOffsets.Right));
  MakeProperty($0084, dxPixelsToEMU(TdxSpreadSheetCustomTextBoxAccess(ATextBox).ContentOffsets.Bottom));
  MakeProperty($0085, IfThen(TdxSpreadSheetCustomTextBoxAccess(ATextBox).WordWrap, 0, 2));
end;

procedure TdxSpreadSheetMSODrawingWriter.WriteProperty_DrawingBlipIdentifier(AIsPicture: Boolean);
const
  PropIDMap: array[Boolean] of Word = ($4186, $4104);
begin
  MakeProperty(PropIDMap[AIsPicture], FBlipID);
end;

procedure TdxSpreadSheetMSODrawingWriter.WriteProperty_DrawingFillAngle;
begin
// TODO: gradient fill now not allowed

end;

procedure TdxSpreadSheetMSODrawingWriter.WriteProperty_DrawingFillBlipIdentifier;
begin
end;

procedure TdxSpreadSheetMSODrawingWriter.WriteProperty_DrawingFillBackColor(AValue: Integer);
begin
  MakeProperty($0183, AValue);
end;

procedure TdxSpreadSheetMSODrawingWriter.WriteProperty_DrawingFillColor(AValue: Integer);
begin
  MakeProperty($0181, AValue);
end;

procedure TdxSpreadSheetMSODrawingWriter.WriteProperty_DrawingFillShadeColors(const AValue: TdxXLSColorRefArray);
var
  I: Integer;
begin
  Writer.WriteWord(Length(AValue));
  Writer.WriteWord(Length(AValue) + 1);
  Writer.WriteWord(SizeOf(TdxXLSColorRef));
  for I := 0 to Length(AValue) - 1 do
  begin
    Writer.WriteInteger(AValue[I].Color);
    Writer.WriteInteger(AValue[I].Position);
  end;
end;

procedure TdxSpreadSheetMSODrawingWriter.WriteProperty_DrawingFillShadeColorsHeader(const AValue: TdxXLSColorRefArray);
begin
  MakeProperty($c197, Length(AValue) * SizeOf(TdxXLSColorRef) + 6);
end;

procedure TdxSpreadSheetMSODrawingWriter.WriteProperty_DrawingLineColor(AValue: Integer);
begin
  MakeProperty($01c0, AValue);
end;

procedure TdxSpreadSheetMSODrawingWriter.WriteProperty_DrawingLineDashing(AValue: TdxGPPenStyle);
const
  Style: array[TdxGPPenStyle] of Byte = (0, 6, 2, 3, 4);
begin
  MakeProperty($01ce, Style[AValue]);
end;

procedure TdxSpreadSheetMSODrawingWriter.WriteProperty_DrawingLineWidth(AValue: Double);
begin
  MakeProperty($01cb, dxPixelsToEMUF(AValue));
end;

procedure TdxSpreadSheetMSODrawingWriter.WriteProperty_DrawingOfficeFillType;
const
  FillTypes: array[TdxGPBrushStyle] of Integer = (0, 4, 2, 0);
begin
  MakeProperty($0180, FillTypes[Shape.Brush.Style]);
end;

procedure TdxSpreadSheetMSODrawingWriter.WriteProperty_DrawingRotation(AAngle: Double);
begin
  if AAngle > 0 then
    while AAngle > 360 do
      AAngle := AAngle - 360
  else
    while Abs(AAngle) > 360 do
      AAngle := AAngle + 360;

  MakeProperty($0004, Trunc(AAngle * 65536));
end;

procedure TdxSpreadSheetMSODrawingWriter.WriteProperty_DrawingTextBooleanProperties;
begin
  MakeProperty($00BF, 524296);
end;

procedure TdxSpreadSheetMSODrawingWriter.WriteProperty_Hyperlink(AHyperlink: TdxSpreadSheetHyperlink);
var
  ALength: Integer;
  AScreentip: string;
  AStream: TMemoryStream;
  AHelper: TdxBIFFHyperlinkHelper;
begin
  if AHyperlink = nil then
    Exit;
  AStream := TMemoryStream.Create;
  try
    AHelper := TdxBIFFHyperlinkHelper.Create(AHyperlink);
    try
      AHelper.SaveToStream(AStream);
    finally
      AHelper.Free;
    end;
    MakeProperty($C382, AStream.Size);
    AScreentip := AHyperlink.ScreenTip;
    ALength := (Length(AScreentip) + 1) * 2;
    if ALength > 2 then
    begin
      MakeProperty($C38D, ALength);
      AStream.WriteBuffer(AScreentip[1], ALength);
    end;
    Writer.Stream.WriteBuffer(AStream.Memory^, AStream.Size);
  finally
    AStream.Free;
  end;
end;

function TdxSpreadSheetMSODrawingWriter.CalcDrawingsCount: Integer;
begin
  Result := Owner.Pictures.Count
end;

function TdxSpreadSheetMSODrawingWriter.CalcMaxShapeIdentifier: Integer;
begin
  Result := CalcShapesCount + CalcDrawingsCount * 2 + 1;
end;

function TdxSpreadSheetMSODrawingWriter.CalcShapesCount: Integer;
begin
  Result := Owner.Shapes.Count;
end;

function TdxSpreadSheetMSODrawingWriter.GetCurrentSheet: TdxSpreadSheetTableView;
begin
  Result := Owner.CurrentSheet;
end;

function TdxSpreadSheetMSODrawingWriter.GetSpreadSheet: TdxCustomSpreadSheet;
begin
  Result := Owner.SpreadSheet;
end;

function TdxSpreadSheetMSODrawingWriter.GetTextBoxProperties: TdxSpreadSheetCustomTextBox;
begin
  if Container is TdxSpreadSheetCustomTextBoxContainer then
    Result := TdxSpreadSheetCustomTextBoxContainerAccess(Container).TextBox
  else
    Result := nil;
end;

function TdxSpreadSheetMSODrawingWriter.ValidateColor(AColor, ADefault: Integer): Integer;
begin
  if (AColor = clDefault) or (TRGBQuad(AColor).rgbReserved = 0) then
    Result := ADefault
  else
    with TRGBQuad(AColor) do
      Result := dxMakeAlphaColor(255 - rgbReserved, rgbBlue, rgbGreen, rgbRed);
end;

function TdxSpreadSheetMSODrawingWriter.ValueToHOffset(AColumnIndex, AOffset: Integer): Integer;
begin
  Result := 0;
  if TdxSpreadSheetTableView(Owner.CurrentSheet).Columns[AColumnIndex] <> nil then
    Result := TdxSpreadSheetTableView(Owner.CurrentSheet).Columns[AColumnIndex].Size;
  if Result = 0 then
    Result := TdxSpreadSheetTableView(Owner.CurrentSheet).Columns.DefaultSize;
  Result := Round(AOffset * 1024 / Result);
end;

function TdxSpreadSheetMSODrawingWriter.ValueToVOffset(ARowIndex, AOffset: Integer): Integer;
begin
  Result := 0;
  if TdxSpreadSheetTableView(Owner.CurrentSheet).Rows[ARowIndex] <> nil then
    Result := TdxSpreadSheetTableView(Owner.CurrentSheet).Rows[ARowIndex].Size;
  if Result = 0 then
    Result := TdxSpreadSheetTableView(Owner.CurrentSheet).Rows.DefaultSize;
  Result := Round(AOffset * 256 / Result);
end;

end.
