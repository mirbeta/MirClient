{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
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
unit dxRichEdit.Import.Doc.OfficeArtContent;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, dxCore, dxCoreClasses,
  dxGenerics,

  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.OfficeImage,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.InlineObjectRange,
  dxRichEdit.DocumentModel.FloatingObjectFormatting,
  dxRichEdit.DocumentModel.ShapeFormatting,
  dxRichEdit.DocumentModel.FloatingObjectRange,
  dxRichEdit.Doc.Utils,
  dxRichEdit.Import.Doc.BlipContainer,
  dxRichEdit.Import.Doc.BorderDescriptor,
  dxCoreGraphics;

type
  TdxOfficeArtPropertiesBase = class;

  { TdxDocContentState }

  TdxDocContentState = (
    MainDocument,
    Footnotes,
    HeadersFootersStory,
    HeadersFootersEnd,
    Macro,
    Comments,
    Endnotes,
    TextBoxes,
    HeaderTextBoxes,
    Final);

  { IdxOfficeDrawingProperty }

  IdxOfficeDrawingProperty = interface
  ['{5BF93392-3567-4993-8097-400AA7024B36}']
    function GetSize: Integer;
    function GetComplex: Boolean;
    function GetComplexData: TBytes;
    procedure Read(AReader: TBinaryReader);
    procedure Execute(AOwner: TdxOfficeArtPropertiesBase);
    procedure Write(AWriter: TBinaryWriter);
    procedure Merge(const AOther: IdxOfficeDrawingProperty);

    property Size: Integer read GetSize;
    property Complex: Boolean read GetComplex;
    property ComplexData: TBytes read GetComplexData;
  end;

  { IdxOfficeArtPropertiesBase }

  IdxOfficeArtPropertiesBase = interface
  ['{175C1783-8C1C-4651-BB77-07C843E9F30A}']
    function GetIsBehindDoc: Boolean;
    function GetUseIsBehindDoc: Boolean;
    function GetFilled: Boolean;
    function GetUseFilled: Boolean;
    function GetLayoutInCell: Boolean;
    function GetUseLayoutInCell: Boolean;
    procedure SetIsBehindDoc(AValue: Boolean);
    procedure SetUseIsBehindDoc(AValue: Boolean);
    procedure SetFilled(AValue: Boolean);
    procedure SetUseFilled(AValue: Boolean);
    procedure SetLayoutInCell(AValue: Boolean);
    procedure SetUseLayoutInCell(AValue: Boolean);

    property IsBehindDoc: Boolean read GetIsBehindDoc write SetIsBehindDoc;
    property UseIsBehindDoc: Boolean read GetUseIsBehindDoc write SetUseIsBehindDoc;
    property Filled: Boolean read GetFilled write SetFilled;
    property UseFilled: Boolean read GetUseFilled write SetUseFilled;
    property LayoutInCell: Boolean read GetLayoutInCell write SetLayoutInCell;
    property UseLayoutInCell: Boolean read GetUseLayoutInCell write SetUseLayoutInCell;
  end;

  { IdxOfficeArtProperties }

  IdxOfficeArtProperties = interface(IdxOfficeArtPropertiesBase)
  ['{705E5E50-6985-4020-A750-01FD3CA7D2CD}']
    function GetBlipIndex: Integer;
    function GetTextIndex: Integer;
    function GetZOrder: Integer;
    function GetUseTextTop: Boolean;
    function GetUseTextBottom: Boolean;
    function GetUseTextRight: Boolean;
    function GetUseTextLeft: Boolean;
    function GetUseFitShapeToText: Boolean;
    function GetFitShapeToText: Boolean;
    function GetTextTop: Integer;
    function GetTextBottom: Integer;
    function GetTextRight: Integer;
    function GetTextLeft: Integer;
    function GetWrapLeftDistance: Integer;
    function GetUseWrapLeftDistance: Boolean;
    function GetWrapRightDistance: Integer;
    function GetUseWrapRightDistance: Boolean;
    function GetWrapTopDistance: Integer;
    function GetUseWrapTopDistance: Boolean;
    function GetWrapBottomDistance: Integer;
    function GetUseWrapBottomDistance: Boolean;
    function GetCropFromTop: Double;
    function GetCropFromBottom: Double;
    function GetCropFromLeft: Double;
    function GetCropFromRight: Double;
    function GetRotation: Double;
    function GetLine: Boolean;
    function GetUseLine: Boolean;
    function GetLineWidth: Double;
    function GetLineColor: TdxAlphaColor;
    function GetFillColor: TdxAlphaColor;

    procedure SetBlipIndex(AValue: Integer);
    procedure SetTextIndex(AValue: Integer);
    procedure SetZOrder(AValue: Integer);
    procedure SetUseTextTop(AValue: Boolean);
    procedure SetUseTextBottom(AValue: Boolean);
    procedure SetUseTextRight(AValue: Boolean);
    procedure SetUseTextLeft(AValue: Boolean);
    procedure SetUseFitShapeToText(AValue: Boolean);
    procedure SetFitShapeToText(AValue: Boolean);
    procedure SetTextTop(AValue: Integer);
    procedure SetTextBottom(AValue: Integer);
    procedure SetTextRight(AValue: Integer);
    procedure SetTextLeft(AValue: Integer);
    procedure SetWrapLeftDistance(AValue: Integer);
    procedure SetUseWrapLeftDistance(AValue: Boolean);
    procedure SetWrapRightDistance(AValue: Integer);
    procedure SetUseWrapRightDistance(AValue: Boolean);
    procedure SetWrapTopDistance(AValue: Integer);
    procedure SetUseWrapTopDistance(AValue: Boolean);
    procedure SetWrapBottomDistance(AValue: Integer);
    procedure SetUseWrapBottomDistance(AValue: Boolean);
    procedure SetCropFromTop(AValue: Double);
    procedure SetCropFromBottom(AValue: Double);
    procedure SetCropFromLeft(AValue: Double);
    procedure SetCropFromRight(AValue: Double);
    procedure SetRotation(AValue: Double);
    procedure SetLine(AValue: Boolean);
    procedure SetUseLine(AValue: Boolean);
    procedure SetLineWidth(AValue: Double);
    procedure SetLineColor(AValue: TdxAlphaColor);
    procedure SetFillColor(AValue: TdxAlphaColor);

    property BlipIndex: Integer read GetBlipIndex write SetBlipIndex;
    property TextIndex: Integer read GetTextIndex write SetTextIndex;
    property ZOrder: Integer read GetZOrder write SetZOrder;
    property UseTextTop: Boolean read GetUseTextTop write SetUseTextTop;
    property UseTextBottom: Boolean read GetUseTextBottom write SetUseTextBottom;
    property UseTextRight: Boolean read GetUseTextRight write SetUseTextRight;
    property UseTextLeft: Boolean read GetUseTextLeft write SetUseTextLeft;
    property UseFitShapeToText: Boolean read GetUseFitShapeToText write SetUseFitShapeToText;
    property FitShapeToText: Boolean read GetFitShapeToText write SetFitShapeToText;
    property TextTop: Integer read GetTextTop write SetTextTop;
    property TextBottom: Integer read GetTextBottom write SetTextBottom;
    property TextRight: Integer read GetTextRight write SetTextRight;
    property TextLeft: Integer read GetTextLeft write SetTextLeft;
    property WrapLeftDistance: Integer read GetWrapLeftDistance write SetWrapLeftDistance;
    property UseWrapLeftDistance: Boolean read GetUseWrapLeftDistance write SetUseWrapLeftDistance;
    property WrapRightDistance: Integer read GetWrapRightDistance write SetWrapRightDistance;
    property UseWrapRightDistance: Boolean read GetUseWrapRightDistance write SetUseWrapRightDistance;
    property WrapTopDistance: Integer read GetWrapTopDistance write SetWrapTopDistance;
    property UseWrapTopDistance: Boolean read GetUseWrapTopDistance write SetUseWrapTopDistance;
    property WrapBottomDistance: Integer read GetWrapBottomDistance write SetWrapBottomDistance;
    property UseWrapBottomDistance: Boolean read GetUseWrapBottomDistance write SetUseWrapBottomDistance;
    property CropFromTop: Double read GetCropFromTop write SetCropFromTop;
    property CropFromBottom: Double read GetCropFromBottom write SetCropFromBottom;
    property CropFromLeft: Double read GetCropFromLeft write SetCropFromLeft;
    property CropFromRight: Double read GetCropFromRight write SetCropFromRight;
    property Rotation: Double read GetRotation write SetRotation;
    property Line: Boolean read GetLine write SetLine;
    property UseLine: Boolean read GetUseLine write SetUseLine;
    property LineWidth: Double read GetLineWidth write SetLineWidth;
    property LineColor: TdxAlphaColor read GetLineColor write SetLineColor;
    property FillColor: TdxAlphaColor read GetFillColor write SetFillColor;
  end;

  { TdxOfficeDrawingPropertyBase }

  TdxOfficeDrawingPropertyBase = class abstract(TInterfacedObject, IdxOfficeDrawingProperty)
  public const
    OperationCodeSize = Integer(2);
    OperandSize       = Integer(4);
  strict private
    FComplex: Boolean;
    FComplexData: TBytes;
  protected
    function GetSize: Integer;
    function GetComplex: Boolean; virtual;
    function GetComplexData: TBytes;

    procedure SetComplexData(const AData: TBytes);
    procedure SetComplex(AComplex: Boolean);
  public
    constructor Create; overload; virtual;
    procedure Read(AReader: TBinaryReader); virtual; abstract;
    procedure Execute(AOwner: TdxOfficeArtPropertiesBase); virtual; abstract;
    procedure Write(AWriter: TBinaryWriter); virtual; abstract;
    procedure Merge(const AOther: IdxOfficeDrawingProperty); virtual;

    property Complex: Boolean read GetComplex;
    property ComplexData: TBytes read FComplexData;
    property Size: Integer read GetSize;
  end;
  TdxOfficeDrawingPropertyClass = class of TdxOfficeDrawingPropertyBase;

  { TdxOfficeDrawingIntPropertyBase }

  TdxOfficeDrawingIntPropertyBase = class abstract(TdxOfficeDrawingPropertyBase)
  strict private
    FValue: Integer;
  protected
    function GetFlag(AMask: Integer): Boolean; inline;
    procedure SetFlag(AMask: Integer; AFlag: Boolean); inline;
  public
    procedure Read(AReader: TBinaryReader); override;
    procedure Execute(AOwner: TdxOfficeArtPropertiesBase); override;
    procedure Write(AWriter: TBinaryWriter); override;

    property Value: Integer read FValue write FValue;
  end;

  { TdxDrawingEmpty }

  TdxDrawingEmpty = class(TdxOfficeDrawingIntPropertyBase);

  { TdxDrawingGroupShapePosH }

  TdxDrawingGroupShapePosH = class(TdxOfficeDrawingIntPropertyBase)
  public type
    TMsoph = (
      msophAbs,
      msophLeft,
      msophCenter,
      msophRight,
      msophInside,
      msophOutside
    );
  strict private
    function GetMsoPosH: TMsoph;
    procedure SetMsoPosH(const AValue: TMsoph);
  public
    constructor Create(AMsoPosH: TMsoph); overload;
    procedure Execute(AOwner: TdxOfficeArtPropertiesBase); override;

    property MsoPosH: TMsoph read GetMsoPosH write SetMsoPosH;
  end;

  { TdxDrawingGroupShapePosV }

  TdxDrawingGroupShapePosV = class(TdxOfficeDrawingIntPropertyBase)
  public type
    TMsopv = (
      msopvAbs,
      msopvTop,
      msopvCenter,
      msopvBottom,
      msopvInside,
      msopvOutside
    );
  strict private
    function GetMsoPosV: TMsopv;
    procedure SetMsoPosV(const AValue: TMsopv);
  public
    constructor Create(AMsoPosV: TMsopv); overload;
    procedure Execute(AOwner: TdxOfficeArtPropertiesBase); override;

    property MsoPosV: TMsopv read GetMsoPosV write SetMsoPosV;
  end;

  { TdxDrawingGroupShapePosRelH }

  TdxDrawingGroupShapePosRelH = class(TdxOfficeDrawingIntPropertyBase)
  public type
    TMsoprh = (
      msoprhMargin,
      msoprhPage,
      msoprhText,
      msoprhChar);
  strict private
    function GetMsoPosRelH: TMsoprh;
    procedure SetMsoPosRelH(const AValue: TMsoprh);
  public
    constructor Create; override;
    constructor Create(AMsoPosRelH: TMsoprh); overload;
    procedure Execute(AOwner: TdxOfficeArtPropertiesBase); override;

    property MsoPosRelH: TMsoprh read GetMsoPosRelH write SetMsoPosRelH;
  end;

  { TdxDrawingGroupShapePosRelV }

  TdxDrawingGroupShapePosRelV = class(TdxOfficeDrawingIntPropertyBase)
  public type
    TMsoprv = (
      msoprvMargin,
      msoprvPage,
      msoprvText,
      msoprvLine);
  strict private
    function GetMsoPosRelV: TMsoprv;
    procedure SetMsoPosRelV(const AValue: TMsoprv);
  public
    constructor Create; override;
    constructor Create(AMsoPosRelV: TMsoprv); overload;
    procedure Execute(AOwner: TdxOfficeArtPropertiesBase); override;

    property MsoPosRelV: TMsoprv read GetMsoPosRelV write SetMsoPosRelV;
  end;

  { TdxDrawingGroupShape2SizeRelH }

  TdxDrawingGroupShape2SizeRelH = class(TdxOfficeDrawingIntPropertyBase)
  public type
    TRelativeFrom = ( //2.3.5.5 - sizerelv
      Margin,
      Page,
      LeftMargin,
      RightMargin,
      InsideMargin,
      OutsideMargin);
  strict private
    function GetFrom: TRelativeFrom;
    procedure SetFrom(const AValue: TRelativeFrom);
  public
    constructor Create; override;
    constructor Create(AFrom: TRelativeFrom); overload;
    procedure Execute(AOwner: TdxOfficeArtPropertiesBase); override;

    property From: TRelativeFrom read GetFrom write SetFrom;
  end;

  { TdxDrawingGroupShape2SizeRelV }

  TdxDrawingGroupShape2SizeRelV = class(TdxOfficeDrawingIntPropertyBase)
  public type
    TRelativeFrom = (//2.3.5.6 - sizerelv
      Margin,
      Page,
      TopMargin,
      BottomMargin,
      InsideMargin,
      OutsideMargin
    );
  strict private
    function GetFrom: TRelativeFrom;
    procedure SetFrom(const AValue: TRelativeFrom);
  public
    constructor Create(AFrom: TRelativeFrom); overload;
    procedure Execute(AOwner: TdxOfficeArtPropertiesBase); override;

    property From: TRelativeFrom read GetFrom write SetFrom;
  end;

  { IdxOfficeArtTertiaryProperties }

  IdxOfficeArtTertiaryProperties = interface(IdxOfficeArtPropertiesBase)
  ['{D641DA4C-A259-4995-B401-ED87E6251C2B}']
    function GetPctHoriz: Integer;
    function GetPctHorizPos: Integer;
    function GetPctVert: Integer;
    function GetPctVertPos: Integer;
    function GetPosH: TdxDrawingGroupShapePosH.TMsoph;
    function GetPosRelH: TdxDrawingGroupShapePosRelH.TMsoprh;
    function GetPosRelV: TdxDrawingGroupShapePosRelV.TMsoprv;
    function GetPosV: TdxDrawingGroupShapePosV.TMsopv;
    function GetSizeRelH: TdxDrawingGroupShape2SizeRelH.TRelativeFrom;
    function GetSizeRelV: TdxDrawingGroupShape2SizeRelV.TRelativeFrom;
    function GetUsePosH: Boolean;
    function GetUsePosV: Boolean;
    function GetUseRelativeHeight: Boolean;
    function GetUseRelativeWidth: Boolean;
    procedure SetPctHoriz(const Value: Integer);
    procedure SetPctHorizPos(const Value: Integer);
    procedure SetPctVert(const Value: Integer);
    procedure SetPctVertPos(const Value: Integer);
    procedure SetPosH(const Value: TdxDrawingGroupShapePosH.TMsoph);
    procedure SetPosRelH(const Value: TdxDrawingGroupShapePosRelH.TMsoprh);
    procedure SetPosRelV(const Value: TdxDrawingGroupShapePosRelV.TMsoprv);
    procedure SetPosV(const Value: TdxDrawingGroupShapePosV.TMsopv);
    procedure SetSizeRelH(const Value: TdxDrawingGroupShape2SizeRelH.TRelativeFrom);
    procedure SetSizeRelV(const Value: TdxDrawingGroupShape2SizeRelV.TRelativeFrom);
    procedure SetUsePosH(const Value: Boolean);
    procedure SetUsePosV(const Value: Boolean);
    procedure SetUseRelativeHeight(const Value: Boolean);
    procedure SetUseRelativeWidth(const Value: Boolean);

    property UseRelativeWidth: Boolean read GetUseRelativeWidth write SetUseRelativeWidth;
    property UseRelativeHeight: Boolean read GetUseRelativeHeight write SetUseRelativeHeight;
    property UsePosH: Boolean read GetUsePosH write SetUsePosH;
    property UsePosV: Boolean read GetUsePosV write SetUsePosV;
    property PctHoriz: Integer read GetPctHoriz write SetPctHoriz;
    property PctVert: Integer read GetPctVert write SetPctVert;
    property PctHorizPos: Integer read GetPctHorizPos write SetPctHorizPos;
    property PctVertPos: Integer read GetPctVertPos write SetPctVertPos;
    property PosH: TdxDrawingGroupShapePosH.TMsoph read GetPosH write SetPosH;
    property PosV: TdxDrawingGroupShapePosV.TMsopv read GetPosV write SetPosV;
    property PosRelH: TdxDrawingGroupShapePosRelH.TMsoprh read GetPosRelH write SetPosRelH;
    property PosRelV: TdxDrawingGroupShapePosRelV.TMsoprv read GetPosRelV write SetPosRelV;
    property SizeRelH: TdxDrawingGroupShape2SizeRelH.TRelativeFrom read GetSizeRelH write SetSizeRelH;
    property SizeRelV: TdxDrawingGroupShape2SizeRelV.TRelativeFrom read GetSizeRelV write SetSizeRelV;
  end;

   { TdxOfficeArtConstants }

  TdxOfficeArtConstants = class
  public const
    MainDocumentDrawingLocation        = Byte(0);
    DefaultMainDocumentShapeIdentifier = Integer($0401);
    DefaultHeaderShapeIdentifier       = Integer($0800);
    DefaultLineWidthInEmus             = Integer(9525);
  end;

  { TdxOfficeArtExceptions }

  TdxOfficeArtExceptions = class
  public
    class procedure ThrowInvalidContent; static;
  end;

  { TdxOfficeArtTypeCodes }

  TdxOfficeArtTypeCodes = class
  public const
    DrawingContainer           = Integer($f000);
    BlipStoreContainer         = Integer($f001);
    DrawingObjectsContainer    = Integer($f002);
    ShapeGroupContainer        = Integer($f003);
    ShapeContainer             = Integer($f004);
    ShapeSolverContainer       = Integer($f005);
    FileDrawingGroupRecord     = Integer($f006);
    FileDrawingRecord          = Integer($f008);
    ShapeGroupCoordinateSystem = Integer($f009);
    FileShape                  = Integer($f00a);
    PropertiesTable            = Integer($f00b);
    ClientTextbox              = Integer($f00d);
    ChildAnchor                = Integer($f00f);
    ClientAnchor               = Integer($f010);
    ClientData                 = Integer($f011);
    ConnectorRule              = Integer($f012);
    ArcRule                    = Integer($f014);
    CalloutRule                = Integer($f017);
    SplitMenuColorContainer    = Integer($f11e);
    TertiaryPropertiesTable    = Integer($f122);
  end;

  { TdxOfficeArtVersions }

  TdxOfficeArtVersions = class
  public const
    DefaultHeaderVersion              = Integer($f);
    EmptyHeaderVersion                = Integer($0);
    ShapeGroupCoordinateSystemVersion = Integer($1);
    ConnectorRuleVersion              = Integer($1);
    ShapeRecordVersion                = Integer($2);
    PropertiesVersion                 = Integer($3);
  end;

  { TdxOfficeArtHeaderInfos }

  TdxOfficeArtHeaderInfos = class
  public const
    EmptyHeaderInfo             = Integer($0000);
    SplitMenuColorContainerInfo = Integer($0004);
    FileDrawingRecordInfo       = Integer($0ffe);
    NotPrimitiveShape           = Integer($0000);
    RectangleShape              = Integer($0001);
    PictureFrameShape           = Integer($004b);
  end;

  { TdxOfficeDrawingPartBase }

  TdxOfficeDrawingPartBase = class abstract (TcxIUnknownObject)
  protected
    function GetHeaderVersion: Integer; virtual; abstract;
    function GetHeaderInstanceInfo: Integer; virtual; abstract;
    function GetHeaderTypeCode: Integer; virtual; abstract;
    function GetLength: Integer; virtual;
    procedure WriteHeader(AWriter: TBinaryWriter);
    procedure WriteCore(AWriter: TBinaryWriter); overload; virtual;
    function ShouldWrite: Boolean; virtual;
    function GetSize: Integer; virtual; abstract;
  public
    procedure Write(AWriter: TBinaryWriter);

    property HeaderVersion: Integer read GetHeaderVersion;
    property HeaderInstanceInfo: Integer read GetHeaderInstanceInfo;
    property HeaderTypeCode: Integer read GetHeaderTypeCode;
    property Length: Integer read GetLength;
  end;

  { TdxCompositeOfficeDrawingPartBase }

  TdxCompositeOfficeDrawingPartBase = class abstract(TdxOfficeDrawingPartBase)
  strict private
    FItems: TdxObjectList<TdxOfficeDrawingPartBase>;
    FObjectsToDelete: TdxFastObjectList;
  protected
    procedure CreateGarbageCollector;
    procedure WriteCore(AWriter: TBinaryWriter); override;
    function GetSize: Integer; override;

    property ObjectsToDelete: TdxFastObjectList read FObjectsToDelete;
  public
    constructor Create; overload;
    destructor Destroy; override;

    property Items: TdxObjectList<TdxOfficeDrawingPartBase> read FItems;
  end;

  { TdxOfficeArtFileDrawingGroupRecord }

  TdxOfficeArtFileDrawingGroupRecord = class(TdxOfficeDrawingPartBase)
  public const
    MainDocumentClusterId = Integer(1);
    HeaderClusterId       = Integer(2);
    HeaderVersion         = Integer($0);
    HeaderInstanceInfo    = Integer($00);
    HeaderTypeCode        = Integer($f006);
    CurrentMaximumShapeID = Integer($03ffd7ff);
    IdClusterSize         = Integer($8);
    BasePartSize          = Integer($10);
  strict private
    FMainDocumentPicturesCount: Integer;
    FHeaderPicturesCount: Integer;
    function GetHasPicturesInHeader: Boolean;
  protected
    function GetHeaderInstanceInfo: Integer; override;
    function GetHeaderTypeCode: Integer; override;
    function GetHeaderVersion: Integer; override;
    procedure Read(AReader: TBinaryReader; AHeader: TdxOfficeArtRecordHeader);
    procedure WriteCore(AWriter: TBinaryWriter); override;
    procedure WriteClustersInfo(AWriter: TBinaryWriter);
    function CalcMaxShapeIdentifier: Integer;
    function CalcClustersCount: Integer;
    function CalcShapesCount: Integer;
    function CalcDrawingsCount: Integer;
    function GetSize: Integer; override;
  public
    class function FromStream(AReader: TBinaryReader; AHeader: TdxOfficeArtRecordHeader): TdxOfficeArtFileDrawingGroupRecord; static;

    property MainDocumentFloatingObjectsCount: Integer read FMainDocumentPicturesCount write FMainDocumentPicturesCount;
    property HeaderFloatingObjectsCount: Integer read FHeaderPicturesCount write FHeaderPicturesCount;
    property HasPicturesInHeader: Boolean read GetHasPicturesInHeader;
  end;

  { TdxOfficeArtBlipStoreContainer }

  TdxOfficeArtBlipStoreContainer = class(TdxOfficeDrawingPartBase)
  strict private
    FBlips: TdxList<TdxBlipBase>;
  protected
    function GetHeaderInstanceInfo: Integer; override;
    function GetHeaderTypeCode: Integer; override;
    function GetHeaderVersion: Integer; override;
    procedure Read(AReader: TBinaryReader; AEmbeddedReader: TBinaryReader;
      AHeader: TdxOfficeArtRecordHeader; const AImageCreator: IdxDocOfficeImageCreator);
    procedure WriteCore(AWriter: TBinaryWriter; AEmbeddedWriter: TBinaryWriter); overload; virtual;
    function ShouldWrite: Boolean; override;
    function GetSize: Integer; override;
  public
    constructor Create;
    destructor Destroy; override;
    class function FromStream(AReader: TBinaryReader; AEmbeddedReader: TBinaryReader;
      AHeader: TdxOfficeArtRecordHeader; const AImageCreator: IdxDocOfficeImageCreator): TdxOfficeArtBlipStoreContainer; static;
    procedure Write(AWriter: TBinaryWriter; AEmbeddedWriter: TBinaryWriter);

    property Blips: TdxList<TdxBlipBase> read FBlips;
  end;

  TdxOfficeColorUse = (
    None               = $0000,
    UseFillColor       = $00f0,
    UseLineOrFillColor = $00f1,
    UseLineColor       = $00f2,
    UseShadowColor     = $00f3,
    UseCurrentColor    = $00f4,
    UseFillBackColor   = $00f5,
    UseLineBackColor   = $00f6,
    UseFillOrLineColor = $00f7
  );

  TdxOfficeColorTransform = (
    None                = $0000,
    Darken              = $0100,
    Lighten             = $0200,
    AddGray             = $0300,
    SubtractGray        = $0400,
    ReverseSubtractGray = $0500,
    Threshold           = $0600,
    Invert              = $2000,
    ToggleHighBit       = $4000,
    ConvertToGrayscale  = $8000
  );

  { TdxOfficeColorRecord }

  TdxOfficeColorRecord = class
  public const
    MaskDefault        = Cardinal($ff000000);
    MaskColorScheme    = Cardinal($08000000);
    MaskSystemColor    = Cardinal($10000000);
    MaskSchemeIndex    = Cardinal($000000ff);
    MaskSystemIndex    = Cardinal($0000ffff);
    MaskColorUse       = Cardinal($000000ff);
    MaskTransform      = Cardinal($0000ff00);
    MaskTransformValue = Cardinal($00ff0000);
  strict private
    FData: Cardinal;
    function GetIsDefault: Boolean;
    function GetSystemColorUsed: Boolean;
    function GetColorSchemeUsed: Boolean;
    function GetColor: TdxAlphaColor;
    procedure SetColor(const AValue: TdxAlphaColor);
    function GetColorSchemeIndex: Byte;
    procedure SetColorSchemeIndex(const AValue: Byte);
    function GetSystemColorIndex: Integer;
    procedure SetSystemColorIndex(const AValue: Integer);
    function GetColorUse: TdxOfficeColorUse;
    function GetTransform: TdxOfficeColorTransform;
    function GetTransformValue: Byte;
  protected
    procedure Read(AReader: TBinaryReader);
    procedure ReadBytes(const AData: TBytes; AOffset: Integer);
  public
    constructor Create; overload;
    constructor Create(AColor: TdxAlphaColor); overload;
    constructor Create(AColorIndex: Integer); overload;
    constructor Create(AColorUse: TdxOfficeColorUse; ATransform: TdxOfficeColorTransform; ATransformValue: Byte); overload;
    class function FromStream(AReader: TBinaryReader): TdxOfficeColorRecord; static;
    class function FromBytes(const AData: TBytes; AOffset: Integer): TdxOfficeColorRecord; static;
    procedure Write(AWriter: TBinaryWriter);
    function GetBytes: TBytes;

    property IsDefault: Boolean read GetIsDefault;
    property SystemColorUsed: Boolean read GetSystemColorUsed;
    property ColorSchemeUsed: Boolean read GetColorSchemeUsed;
    property Color: TdxAlphaColor read GetColor write SetColor;
    property ColorSchemeIndex: Byte read GetColorSchemeIndex write SetColorSchemeIndex;
    property SystemColorIndex: Integer read GetSystemColorIndex write SetSystemColorIndex;
    property ColorUse: TdxOfficeColorUse read GetColorUse;
    property Transform: TdxOfficeColorTransform read GetTransform;
    property TransformValue: Byte read GetTransformValue;
  end;

  { TdxOfficeArtFileDrawingRecord }

  TdxOfficeArtFileDrawingRecord = class(TdxOfficeDrawingPartBase)
  public const
    RecordLength = Integer($0008);
  strict private
    FDrawingId: Integer;
    FNumberOfShapes: Integer;
    FLastShapeId: Integer;
  protected
    function GetHeaderInstanceInfo: Integer; override;
    function GetHeaderTypeCode: Integer; override;
    function GetHeaderVersion: Integer; override;
    function GetLength: Integer; override;
    procedure Read(AReader: TBinaryReader; AHeader: TdxOfficeArtRecordHeader);
    procedure WriteCore(AWriter: TBinaryWriter); override;
    function GetSize: Integer; override;
  public
    constructor Create(ADrawingId: Integer);
    class function FromStream(AReader: TBinaryReader; AHeader: TdxOfficeArtRecordHeader): TdxOfficeArtFileDrawingRecord; static;

    property NumberOfShapes: Integer read FNumberOfShapes write FNumberOfShapes;
    property LastShapeIdentifier: Integer read FLastShapeId write FLastShapeId;
  end;

  { TdxOfficeArtSplitMenuColorContainer }

  TdxOfficeArtSplitMenuColorContainer = class(TdxOfficeDrawingPartBase)
  public const
    RecordLength = Integer($10);
  strict private
    FFillColor: TdxOfficeColorRecord;
    FLineColor: TdxOfficeColorRecord;
    FShapeColor: TdxOfficeColorRecord;
    FColor3D: TdxOfficeColorRecord;
  protected
    function GetHeaderInstanceInfo: Integer; override;
    function GetHeaderTypeCode: Integer; override;
    function GetHeaderVersion: Integer; override;
    function GetLength: Integer; override;
    procedure Read(AReader: TBinaryReader; AHeader: TdxOfficeArtRecordHeader);
    procedure WriteCore(AWriter: TBinaryWriter); override;
    function GetSize: Integer; override;
  public
    constructor Create;
    destructor Destroy; override;
    class function FromStream(AReader: TBinaryReader; AHeader: TdxOfficeArtRecordHeader): TdxOfficeArtSplitMenuColorContainer; static;

    property FillColor: TdxOfficeColorRecord read FFillColor;
    property LineColor: TdxOfficeColorRecord read FLineColor;
    property ShapeColor: TdxOfficeColorRecord read FShapeColor;
    property Color3D: TdxOfficeColorRecord read FColor3D;
  end;

  { TdxOfficeArtDrawingContainer }

  TdxOfficeArtDrawingContainer = class(TdxCompositeOfficeDrawingPartBase)
  strict private
    FFileDrawingBlock: TdxOfficeArtFileDrawingGroupRecord;
    FBlipContainer: TdxOfficeArtBlipStoreContainer;
    FSplitMenuColorContainer: TdxOfficeArtSplitMenuColorContainer;
  protected
    function GetHeaderInstanceInfo: Integer; override;
    function GetHeaderTypeCode: Integer; override;
    function GetHeaderVersion: Integer; override;
    procedure Read(AReader: TBinaryReader; AEmbeddedReader: TBinaryReader; const AImageCreator: IdxDocOfficeImageCreator);
    procedure CheckHeader(AHeader: TdxOfficeArtRecordHeader);
  public
    constructor Create;
    class function FromStream(AReader: TBinaryReader; AEmbeddedReader: TBinaryReader;
      const AImageCreator: IdxDocOfficeImageCreator): TdxOfficeArtDrawingContainer; static;
    procedure Write(AWriter: TBinaryWriter; AEmbeddedWriter: TBinaryWriter);
    procedure ParseHeader(AReader: TBinaryReader; AEmbeddedReader: TBinaryReader;
      const AImageCreator: IdxDocOfficeImageCreator);

    property FileDrawingBlock: TdxOfficeArtFileDrawingGroupRecord read FFileDrawingBlock;
    property BlipContainer: TdxOfficeArtBlipStoreContainer read FBlipContainer;
    property SplitMenuColorContainer: TdxOfficeArtSplitMenuColorContainer read FSplitMenuColorContainer;
  end;

  { TdxOfficeArtShapeRecord }

  TdxOfficeArtShapeRecord = class(TdxOfficeDrawingPartBase)
  public const
    DefaultFlags      = Integer($0a00);
    RecordLength      = Integer(8);
    MaskGroup         = Integer($0001);
    MaskChild         = Integer($0002);
    MaskPatriarch     = Integer($0004);
    MaskDeleted       = Integer($0008);
    MaskOleShape      = Integer($0010);
    MaskHaveMaster    = Integer($0020);
    MaskFlipH         = Integer($0040);
    MaskFlipV         = Integer($0080);
    MaskConnector     = Integer($0100);
    MaskHaveAnchor    = Integer($0200);
    MaskBackground    = Integer($0400);
    MaskHaveShapeType = Integer($0800);
  strict private
    FShapeTypeCode: Integer;
    FShapeIdentifier: Integer;
    FFlags: Integer;
    function GetIsGroup: Boolean;
    procedure SetIsGroup(const AValue: Boolean);
    function GetIsChild: Boolean;
    procedure SetIsChild(const AValue: Boolean);
    function GetIsPatriarch: Boolean;
    procedure SetIsPatriarch(const AValue: Boolean);
    function GetIsDeleted: Boolean;
    procedure SetIsDeleted(const AValue: Boolean);
    function GetIsOleShape: Boolean;
    procedure SetIsOleShape(const AValue: Boolean);
    function GetHaveMaster: Boolean;
    procedure SetHaveMaster(const AValue: Boolean);
    function GetFlipH: Boolean;
    procedure SetFlipH(const AValue: Boolean);
    function GetFlipV: Boolean;
    procedure SetFlipV(const AValue: Boolean);
    function GetIsConnector: Boolean;
    procedure SetIsConnector(const AValue: Boolean);
    function GetHaveAnchor: Boolean;
    procedure SetHaveAnchor(const AValue: Boolean);
    function GetIsBackground: Boolean;
    procedure SetIsBackground(const AValue: Boolean);
    function GetHaveShapeType: Boolean;
    procedure SetHaveShapeType(const AValue: Boolean);
  protected
    function GetHeaderInstanceInfo: Integer; override;
    function GetHeaderTypeCode: Integer; override;
    function GetHeaderVersion: Integer; override;
    function GetLength: Integer; override;
    procedure Read(AReader: TBinaryReader);
    procedure WriteCore(AWriter: TBinaryWriter); override;
    function GetSize: Integer; override;
    function GetBoolValue(AMask: Integer): Boolean;
    procedure SetBoolValue(AValue: Boolean; AMask: Integer);
  public
    constructor Create(AShapeTypeCode: Integer = 0);
    class function FromStream(AReader: TBinaryReader): TdxOfficeArtShapeRecord; overload; static;
    class function FromStream(AReader: TBinaryReader; AHeader: TdxOfficeArtRecordHeader): TdxOfficeArtShapeRecord; overload; static;

    property ShapeIdentifier: Integer read FShapeIdentifier write FShapeIdentifier;
    property Flags: Integer read FFlags write FFlags;
    property IsGroup: Boolean read GetIsGroup write SetIsGroup;
    property IsChild: Boolean read GetIsChild write SetIsChild;
    property IsPatriarch: Boolean read GetIsPatriarch write SetIsPatriarch;
    property IsDeleted: Boolean read GetIsDeleted write SetIsDeleted;
    property IsOleShape: Boolean read GetIsOleShape write SetIsOleShape;
    property HaveMaster: Boolean read GetHaveMaster write SetHaveMaster;
    property FlipH: Boolean read GetFlipH write SetFlipH;
    property FlipV: Boolean read GetFlipV write SetFlipV;
    property IsConnector: Boolean read GetIsConnector write SetIsConnector;
    property HaveAnchor: Boolean read GetHaveAnchor write SetHaveAnchor;
    property IsBackground: Boolean read GetIsBackground write SetIsBackground;
    property HaveShapeType: Boolean read GetHaveShapeType write SetHaveShapeType;
  end;

  { TdxOfficeArtPropertiesBase }

  TdxOfficeArtPropertiesBase = class abstract(TdxOfficeDrawingPartBase)
  strict private
    FProperties: TList<IdxOfficeDrawingProperty>;
  protected
    function GetHeaderInstanceInfo: Integer; override;
    function GetHeaderVersion: Integer; override;
    procedure Read(AReader: TBinaryReader; AHeader: TdxOfficeArtRecordHeader);
    procedure WriteCore(AWriter: TBinaryWriter); override;
    function GetSize: Integer; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure CreateProperties; virtual; abstract;

    property Properties: TList<IdxOfficeDrawingProperty> read FProperties;
  end;

  { TdxOfficeArtProperties }

  TdxOfficeArtProperties = class(TdxOfficeArtPropertiesBase,
    IdxOfficeArtProperties,
    IdxOfficeArtPropertiesBase)
  strict private
    FBlipIndex: Integer;
    FTextIndex: Integer;
    FZOrder: Integer;
    FUseTextTop: Boolean;
    FUseTextBottom: Boolean;
    FUseTextRight: Boolean;
    FUseTextLeft: Boolean;
    FTextTop: Integer;
    FTextBottom: Integer;
    FTextRight: Integer;
    FTextLeft: Integer;
    FWrapLeftDistance: Integer;
    FWrapRightDistance: Integer;
    FWrapTopDistance: Integer;
    FWrapBottomDistance: Integer;
    FUseWrapLeftDistance: Boolean;
    FUseWrapRightDistance: Boolean;
    FUseWrapTopDistance: Boolean;
    FUseWrapBottomDistance: Boolean;
    FCropFromTop: Double;
    FCropFromBottom: Double;
    FCropFromLeft: Double;
    FCropFromRight: Double;
    FRotation: Double;
    FIsBehindDoc: Boolean;
    FUseIsBehindDoc: Boolean;
    FLine: Boolean;
    FUseLine: Boolean;
    FLayoutInCell: Boolean;
    FUseLayoutInCell: Boolean;
    FFitShapeToText: Boolean;
    FUseFitShapeToText: Boolean;
    FFilled: Boolean;
    FUseFilled: Boolean;
    FLineWidth: Double;
    FLineColor: TdxAlphaColor;
    FFillColor: TdxAlphaColor;
    FShape: TdxShape;
    FTextBoxProperties: TdxTextBoxProperties;
    FFloatingObjectProperties: TdxFloatingObjectProperties;
    FUnitConverter: TdxDocumentModelUnitConverter;
  private
    function GetBlipIndex: Integer;
    function GetCropFromBottom: Double;
    function GetCropFromLeft: Double;
    function GetCropFromRight: Double;
    function GetCropFromTop: Double;
    function GetFillColor: TdxAlphaColor;
    function GetFilled: Boolean;
    function GetFitShapeToText: Boolean;
    function GetIsBehindDoc: Boolean;
    function GetLayoutInCell: Boolean;
    function GetLine: Boolean;
    function GetLineColor: TdxAlphaColor;
    function GetLineWidth: Double;
    function GetRotation: Double;
    function GetTextBottom: Integer;
    function GetTextIndex: Integer;
    function GetTextLeft: Integer;
    function GetTextRight: Integer;
    function GetTextTop: Integer;
    function GetUseFilled: Boolean;
    function GetUseFitShapeToText: Boolean;
    function GetUseIsBehindDoc: Boolean;
    function GetUseLayoutInCell: Boolean;
    function GetUseLine: Boolean;
    function GetUseTextBottom: Boolean;
    function GetUseTextLeft: Boolean;
    function GetUseTextRight: Boolean;
    function GetUseTextTop: Boolean;
    function GetUseWrapBottomDistance: Boolean;
    function GetUseWrapLeftDistance: Boolean;
    function GetUseWrapRightDistance: Boolean;
    function GetUseWrapTopDistance: Boolean;
    function GetWrapBottomDistance: Integer;
    function GetWrapLeftDistance: Integer;
    function GetWrapRightDistance: Integer;
    function GetWrapTopDistance: Integer;
    function GetZOrder: Integer;
    procedure SetBlipIndex(AValue: Integer);
    procedure SetCropFromBottom(AValue: Double);
    procedure SetCropFromLeft(AValue: Double);
    procedure SetCropFromRight(AValue: Double);
    procedure SetCropFromTop(AValue: Double);
    procedure SetFillColor(AValue: TdxAlphaColor);
    procedure SetFilled(AValue: Boolean);
    procedure SetFitShapeToText(AValue: Boolean);
    procedure SetIsBehindDoc(AValue: Boolean);
    procedure SetLayoutInCell(AValue: Boolean);
    procedure SetLine(AValue: Boolean);
    procedure SetLineColor(AValue: TdxAlphaColor);
    procedure SetLineWidth(AValue: Double);
    procedure SetRotation(AValue: Double);
    procedure SetTextBottom(AValue: Integer);
    procedure SetTextIndex(AValue: Integer);
    procedure SetTextLeft(AValue: Integer);
    procedure SetTextRight(AValue: Integer);
    procedure SetTextTop(AValue: Integer);
    procedure SetUseFilled(AValue: Boolean);
    procedure SetUseFitShapeToText(AValue: Boolean);
    procedure SetUseIsBehindDoc(AValue: Boolean);
    procedure SetUseLayoutInCell(AValue: Boolean);
    procedure SetUseLine(AValue: Boolean);
    procedure SetUseTextBottom(AValue: Boolean);
    procedure SetUseTextLeft(AValue: Boolean);
    procedure SetUseTextRight(AValue: Boolean);
    procedure SetUseTextTop(AValue: Boolean);
    procedure SetUseWrapBottomDistance(AValue: Boolean);
    procedure SetUseWrapLeftDistance(AValue: Boolean);
    procedure SetUseWrapRightDistance(AValue: Boolean);
    procedure SetUseWrapTopDistance(AValue: Boolean);
    procedure SetWrapBottomDistance(AValue: Integer);
    procedure SetWrapLeftDistance(AValue: Integer);
    procedure SetWrapRightDistance(AValue: Integer);
    procedure SetWrapTopDistance(AValue: Integer);
    procedure SetZOrder(AValue: Integer);
  protected
    function GetHeaderTypeCode: Integer; override;
    procedure SetShapeDescriptionProperty; virtual;
    procedure SetShapeGroupBooleanProperties;
    procedure SetDrawingTextBooleanProperties;
  public
    class function FromStream(AReader: TBinaryReader; AHeader: TdxOfficeArtRecordHeader): TdxOfficeArtProperties; static;
    procedure CreateProperties; override;
    procedure SetTextBoxProperties;
    procedure SetRotationProperties;
    procedure SetFillColorProperties;
    procedure SetLineProrerties;
    procedure SetBooleanProtectionProperties;
    procedure SetBlipIndexProperty;
    procedure SetTextIndexProperty;
    procedure SetBlipBooleanProperties;
    procedure SetFillStyleBooleanProperties;
    procedure SetLineStyleBooleanProperties;
    procedure SetShapeBooleanProperties;
    procedure SetShapeNameProperty;

    property IsBehindDoc: Boolean read GetIsBehindDoc write SetIsBehindDoc;
    property UseIsBehindDoc: Boolean read GetUseIsBehindDoc write SetUseIsBehindDoc;
    property Filled: Boolean read GetFilled write SetFilled;
    property UseFilled: Boolean read GetUseFilled write SetUseFilled;
    property LayoutInCell: Boolean read GetLayoutInCell write SetLayoutInCell;
    property UseLayoutInCell: Boolean read GetUseLayoutInCell write SetUseLayoutInCell;
    property BlipIndex: Integer read GetBlipIndex write SetBlipIndex;
    property TextIndex: Integer read GetTextIndex write SetTextIndex;
    property ZOrder: Integer read GetZOrder write SetZOrder;
    property UseTextTop: Boolean read GetUseTextTop write SetUseTextTop;
    property UseTextBottom: Boolean read GetUseTextBottom write SetUseTextBottom;
    property UseTextRight: Boolean read GetUseTextRight write SetUseTextRight;
    property UseTextLeft: Boolean read GetUseTextLeft write SetUseTextLeft;
    property UseFitShapeToText: Boolean read GetUseFitShapeToText write SetUseFitShapeToText;
    property FitShapeToText: Boolean read GetFitShapeToText write SetFitShapeToText;
    property TextTop: Integer read GetTextTop write SetTextTop;
    property TextBottom: Integer read GetTextBottom write SetTextBottom;
    property TextRight: Integer read GetTextRight write SetTextRight;
    property TextLeft: Integer read GetTextLeft write SetTextLeft;
    property WrapLeftDistance: Integer read GetWrapLeftDistance write SetWrapLeftDistance;
    property UseWrapLeftDistance: Boolean read GetUseWrapLeftDistance write SetUseWrapLeftDistance;
    property WrapRightDistance: Integer read GetWrapRightDistance write SetWrapRightDistance;
    property UseWrapRightDistance: Boolean read GetUseWrapRightDistance write SetUseWrapRightDistance;
    property WrapTopDistance: Integer read GetWrapTopDistance write SetWrapTopDistance;
    property UseWrapTopDistance: Boolean read GetUseWrapTopDistance write SetUseWrapTopDistance;
    property WrapBottomDistance: Integer read GetWrapBottomDistance write SetWrapBottomDistance;
    property UseWrapBottomDistance: Boolean read GetUseWrapBottomDistance write SetUseWrapBottomDistance;
    property CropFromTop: Double read GetCropFromTop write SetCropFromTop;
    property CropFromBottom: Double read GetCropFromBottom write SetCropFromBottom;
    property CropFromLeft: Double read GetCropFromLeft write SetCropFromLeft;
    property CropFromRight: Double read GetCropFromRight write SetCropFromRight;
    property Rotation: Double read GetRotation write SetRotation;
    property Line: Boolean read GetLine write SetLine;
    property UseLine: Boolean read GetUseLine write SetUseLine;
    property LineWidth: Double read GetLineWidth write SetLineWidth;
    property LineColor: TdxAlphaColor read GetLineColor write SetLineColor;
    property FillColor: TdxAlphaColor read GetFillColor write SetFillColor;

    property Shape: TdxShape read FShape write FShape;
    property TextBoxProperties: TdxTextBoxProperties read FTextBoxProperties write FTextBoxProperties;
    property FloatingObjectProperties: TdxFloatingObjectProperties read FFloatingObjectProperties write FFloatingObjectProperties;
    property UnitConverter: TdxDocumentModelUnitConverter read FUnitConverter write FUnitConverter;
  end;

  { TdxOfficeArtTertiaryProperties }

  TdxOfficeArtTertiaryProperties = class(TdxOfficeArtPropertiesBase, IdxOfficeArtTertiaryProperties)
  public const
    DefaultFlags = Integer($0000);
  strict private
    FWriteAlways: Boolean;
    FIsBehindDoc: Boolean;
    FUseIsBehindDoc: Boolean;
    FUseRelativeWidth: Boolean;
    FUseRelativeHeight: Boolean;
    FUsePosH: Boolean;
    FUsePosV: Boolean;
    FFilled: Boolean;
    FUseFilled: Boolean;
    FLayoutInCell: Boolean;
    FUseLayoutInCell: Boolean;
    FPctHoriz: Integer;
    FPctVert: Integer;
    FPctHorizPos: Integer;
    FPctVertPos: Integer;
    FSizeRelH: TdxDrawingGroupShape2SizeRelH.TRelativeFrom;
    FSizeRelV: TdxDrawingGroupShape2SizeRelV.TRelativeFrom;
    FPosH: TdxDrawingGroupShapePosH.TMsoph;
    FPosV: TdxDrawingGroupShapePosV.TMsopv;
    FPosRelH: TdxDrawingGroupShapePosRelH.TMsoprh;
    FPosRelV: TdxDrawingGroupShapePosRelV.TMsoprv;
    function GetPctHorizPosValid: Boolean;
    function GetPctVertPosValid: Boolean;
  protected
    function GetIsBehindDoc: Boolean;
    function GetUseIsBehindDoc: Boolean;
    function GetFilled: Boolean;
    function GetUseFilled: Boolean;
    function GetLayoutInCell: Boolean;
    function GetUseLayoutInCell: Boolean;
    procedure SetIsBehindDoc(AValue: Boolean);
    procedure SetUseIsBehindDoc(AValue: Boolean);
    procedure SetFilled(AValue: Boolean);
    procedure SetUseFilled(AValue: Boolean);
    procedure SetLayoutInCell(AValue: Boolean);
    procedure SetUseLayoutInCell(AValue: Boolean);
    // IdxOfficeArtTertiaryProperties
    function GetPctHoriz: Integer;
    function GetPctHorizPos: Integer;
    function GetPctVert: Integer;
    function GetPctVertPos: Integer;
    function GetPosH: TdxDrawingGroupShapePosH.TMsoph;
    function GetPosRelH: TdxDrawingGroupShapePosRelH.TMsoprh;
    function GetPosRelV: TdxDrawingGroupShapePosRelV.TMsoprv;
    function GetPosV: TdxDrawingGroupShapePosV.TMsopv;
    function GetSizeRelH: TdxDrawingGroupShape2SizeRelH.TRelativeFrom;
    function GetSizeRelV: TdxDrawingGroupShape2SizeRelV.TRelativeFrom;
    function GetUsePosH: Boolean;
    function GetUsePosV: Boolean;
    function GetUseRelativeHeight: Boolean;
    function GetUseRelativeWidth: Boolean;
    procedure SetPctHoriz(const AValue: Integer);
    procedure SetPctHorizPos(const AValue: Integer);
    procedure SetPctVert(const AValue: Integer);
    procedure SetPctVertPos(const AValue: Integer);
    procedure SetPosH(const AValue: TdxDrawingGroupShapePosH.TMsoph);
    procedure SetPosRelH(const AValue: TdxDrawingGroupShapePosRelH.TMsoprh);
    procedure SetPosRelV(const AValue: TdxDrawingGroupShapePosRelV.TMsoprv);
    procedure SetPosV(const AValue: TdxDrawingGroupShapePosV.TMsopv);
    procedure SetSizeRelH(const AValue: TdxDrawingGroupShape2SizeRelH.TRelativeFrom);
    procedure SetSizeRelV(const AValue: TdxDrawingGroupShape2SizeRelV.TRelativeFrom);
    procedure SetUsePosH(const AValue: Boolean);
    procedure SetUsePosV(const AValue: Boolean);
    procedure SetUseRelativeHeight(const AValue: Boolean);
    procedure SetUseRelativeWidth(const AValue: Boolean);

    function GetHeaderTypeCode: Integer; override;
    function ShouldWrite: Boolean; override;
  public
    constructor Create(AWriteAlways: Boolean = False);
    destructor Destroy; override;
    class function FromStream(AReader: TBinaryReader; AHeader: TdxOfficeArtRecordHeader): TdxOfficeArtTertiaryProperties; static;
    procedure CreateProperties; override;
    procedure SetGroupShapePctPosProperties;
    procedure SetGroupShape2PctHorizProperties;
    procedure SetGroupShape2PctVertProperties;
    procedure SetGroupShape2SizeRelHProperties;
    procedure SetGroupShape2SizeRelVProperties;
    procedure SetGroupShapeBooleanProperties;
    procedure SetGroupShapePosHProperties;
    procedure SetGroupShapePosRelHProperties;
    procedure SetGroupShapePosVProperties;
    procedure SetGroupShapePosRelVProperties;
    procedure SetDiagramBooleanProperties;

    property IsBehindDoc: Boolean read GetIsBehindDoc write SetIsBehindDoc;
    property UseIsBehindDoc: Boolean read GetUseIsBehindDoc write SetUseIsBehindDoc;
    property Filled: Boolean read GetFilled write SetFilled;
    property UseFilled: Boolean read GetUseFilled write SetUseFilled;
    property LayoutInCell: Boolean read GetLayoutInCell write SetLayoutInCell;
    property UseLayoutInCell: Boolean read GetUseLayoutInCell write SetUseLayoutInCell;
    property PctHorizPosValid: Boolean read GetPctHorizPosValid;
    property PctVertPosValid: Boolean read GetPctVertPosValid;

    property UseRelativeWidth: Boolean read GetUseRelativeWidth write SetUseRelativeWidth;
    property UseRelativeHeight: Boolean read GetUseRelativeHeight write SetUseRelativeHeight;
    property UsePosH: Boolean read GetUsePosH write SetUsePosH;
    property UsePosV: Boolean read GetUsePosV write SetUsePosV;
    property PctHoriz: Integer read GetPctHoriz write SetPctHoriz;
    property PctVert: Integer read GetPctVert write SetPctVert;
    property PctHorizPos: Integer read GetPctHorizPos write SetPctHorizPos;
    property PctVertPos: Integer read GetPctVertPos write SetPctVertPos;
    property PosH: TdxDrawingGroupShapePosH.TMsoph read GetPosH write SetPosH;
    property PosV: TdxDrawingGroupShapePosV.TMsopv read GetPosV write SetPosV;
    property PosRelH: TdxDrawingGroupShapePosRelH.TMsoprh read GetPosRelH write SetPosRelH;
    property PosRelV: TdxDrawingGroupShapePosRelV.TMsoprv read GetPosRelV write SetPosRelV;
    property SizeRelH: TdxDrawingGroupShape2SizeRelH.TRelativeFrom read GetSizeRelH write SetSizeRelH;
    property SizeRelV: TdxDrawingGroupShape2SizeRelV.TRelativeFrom read GetSizeRelV write SetSizeRelV;
  end;

  { TdxOfficeArtShapeContainer }

  TdxOfficeArtShapeContainer = class(TdxCompositeOfficeDrawingPartBase)
  strict private
    FShapeRecord: TdxOfficeArtShapeRecord;
    FArtProperties: TdxOfficeArtProperties;
    FArtTertiaryProperties: TdxOfficeArtTertiaryProperties;
  protected
    function GetHeaderInstanceInfo: Integer; override;
    function GetHeaderTypeCode: Integer; override;
    function GetHeaderVersion: Integer; override;
    procedure Read(AReader: TBinaryReader; AHeader: TdxOfficeArtRecordHeader);
    procedure ParseHeader(AReader: TBinaryReader);
  public
    constructor Create(AShapeTypeCode: Integer = TdxOfficeArtHeaderInfos.PictureFrameShape);
    class function FromStream(AReader: TBinaryReader; AHeader: TdxOfficeArtRecordHeader): TdxOfficeArtShapeContainer; static;

    property ShapeRecord: TdxOfficeArtShapeRecord read FShapeRecord;
    property ArtProperties: TdxOfficeArtProperties read FArtProperties;
    property ArtTertiaryProperties: TdxOfficeArtTertiaryProperties read FArtTertiaryProperties;
  end;

  { TdxOfficeArtInlineShapeContainer }

  TdxOfficeArtInlineShapeContainer = class
  strict private
    FShapeContainer: TdxOfficeArtShapeContainer;
    FBlips: TdxList<TdxBlipBase>;
  protected
    procedure Read(AReader: TBinaryReader; ASize: Integer; const AImageCreator: IdxDocOfficeImageCreator);
  public
    constructor Create;
    destructor Destroy; override;
    class function FromStream(AReader: TBinaryReader; ASize: Integer; const AImageCreator: IdxDocOfficeImageCreator): TdxOfficeArtInlineShapeContainer; static;
    procedure Write(AWriter: TBinaryWriter);

    property ShapeContainer: TdxOfficeArtShapeContainer read FShapeContainer;
    property Blips: TdxList<TdxBlipBase> read FBlips;
  end;

  { TdxOfficeDrawingMsoArrayPropertyBase }

  TdxOfficeDrawingMsoArrayPropertyBase = class abstract(TdxOfficeDrawingPropertyBase)
  strict private
    FValue: Integer;
  protected
    function GetComplex: Boolean; override;
  public
    procedure Read(AReader: TBinaryReader); override;
    procedure Execute(AOwner: TdxOfficeArtPropertiesBase); override;
    procedure Write(AWriter: TBinaryWriter); override;

    property Value: Integer read FValue write FValue;
  end;

  { TdxDrawingTextLeft }

  TdxDrawingTextLeft = class(TdxOfficeDrawingIntPropertyBase)
  public const
    DefaultValue = Integer($00016530);
  protected
    function GetComplex: Boolean; override;
  public
    constructor Create(AValue: Integer); overload;
    constructor Create; override;
    procedure Execute(AOwner: TdxOfficeArtPropertiesBase); override;
  end;

  { TdxDrawingTextTop }

  TdxDrawingTextTop = class(TdxOfficeDrawingIntPropertyBase)
  public const
    DefaultValue = Integer($0000B298);
  protected
    function GetComplex: Boolean; override;
  public
    constructor Create(AValue: Integer); overload;
    constructor Create; override;
    procedure Execute(AOwner: TdxOfficeArtPropertiesBase); override;
  end;

  { TdxDrawingTextRight }

  TdxDrawingTextRight = class(TdxOfficeDrawingIntPropertyBase)
  public const
    DefaultValue = Integer($00016530);
  protected
    function GetComplex: Boolean; override;
  public
    constructor Create(AValue: Integer); overload;
    constructor Create; override;
    procedure Execute(AOwner: TdxOfficeArtPropertiesBase); override;
  end;

  { TdxDrawingTextBottom }

  TdxDrawingTextBottom = class(TdxOfficeDrawingIntPropertyBase)
  public const
    DefaultValue = Integer($0000B298);
  protected
    function GetComplex: Boolean; override;
  public
    constructor Create(AValue: Integer); overload;
    constructor Create; override;
    procedure Execute(AOwner: TdxOfficeArtPropertiesBase); override;
  end;

  { TdxOfficeDrawingFixedPointPropertyBase }

  TdxOfficeDrawingFixedPointPropertyBase = class(TdxOfficeDrawingPropertyBase)
  strict private
    FValue: Double;
  protected
    function GetComplex: Boolean; override;
  public
    procedure Read(AReader: TBinaryReader); override;
    procedure Execute(AOwner: TdxOfficeArtPropertiesBase); override;
    procedure Write(AWriter: TBinaryWriter); override;

    property Value: Double read FValue write FValue;
  end;

  { TdxDrawingRotation }

  TdxDrawingRotation = class(TdxOfficeDrawingFixedPointPropertyBase)
  protected
    function GetComplex: Boolean; override;
  public
    constructor Create(ARotation: Integer); overload;
    procedure Execute(AOwner: TdxOfficeArtPropertiesBase); override;
  end;

  { TdxOfficeDrawingColorPropertyBase }

  TdxOfficeDrawingColorPropertyBase = class abstract(TdxOfficeDrawingPropertyBase)
  strict private
    FColorRecord: TdxOfficeColorRecord;
  private
    procedure SetColorRecord(const Value: TdxOfficeColorRecord);
  public
    constructor Create; override;
    constructor Create(AColor: TdxAlphaColor); overload;
    destructor Destroy; override;
    procedure Read(AReader: TBinaryReader); override;
    procedure Write(AWriter: TBinaryWriter); override;

    property ColorRecord: TdxOfficeColorRecord read FColorRecord write SetColorRecord;
  end;

  { TdxDrawingFillColor }

  TdxDrawingFillColor = class(TdxOfficeDrawingColorPropertyBase)
  public
    procedure Execute(AOwner: TdxOfficeArtPropertiesBase); override;
  end;

  { TdxDrawingLineColor }

  TdxDrawingLineColor = class(TdxOfficeDrawingColorPropertyBase)
  public
    constructor Create(AColor: TdxAlphaColor); overload;
    constructor Create(AColorIndex: Integer); overload;
    procedure Execute(AOwner: TdxOfficeArtPropertiesBase); override;
  end;

  { TdxDrawingLineWidth }

  TdxDrawingLineWidth = class(TdxOfficeDrawingIntPropertyBase)
  protected
    function GetComplex: Boolean; override;
  public
    constructor Create(AWidth: Integer = TdxOfficeArtConstants.DefaultLineWidthInEmus);
    procedure Execute(AOwner: TdxOfficeArtPropertiesBase); override;
  end;

  { TdxOfficeDrawingBooleanPropertyBase }

  TdxOfficeDrawingBooleanPropertyBase = class abstract(TdxOfficeDrawingIntPropertyBase)
  public const
    UseMask = Cardinal($ffff0000);
  public
    procedure Merge(const AOther: IdxOfficeDrawingProperty); override;
  end;

  { TdxDrawingBooleanProtectionProperties }

  TdxDrawingBooleanProtectionProperties = class(TdxOfficeDrawingBooleanPropertyBase)
  public const
    CLockGroup            = Integer($00000001);
    CLockAdjustHandles    = Integer($00000002);
    CLockText             = Integer($00000004);
    CLockVertices         = Integer($00000008);
    CLockCropping         = Integer($00000010);
    CLockSelect           = Integer($00000020);
    CLockPosition         = Integer($00000040);
    CLockAspectRatio      = Integer($00000080);
    CLockRotation         = Integer($00000100);
    CLockUngroup          = Integer($00000200);
    CUseLockGroup         = Integer($00010000);
    CUseLockAdjustHandles = Integer($00020000);
    CUseLockText          = Integer($00040000);
    CUseLockVertices      = Integer($00080000);
    CUseLockCropping      = Integer($00100000);
    CUseLockSelect        = Integer($00200000);
    CUseLockPosition      = Integer($00400000);
    CUseLockAspectRatio   = Integer($00800000);
    CUseLockRotation      = Integer($01000000);
    CUseLockUngroup       = Integer($02000000);
    CDefaultFlags         = Integer($00e10080);
  strict private
    function GetLockGroup: Boolean;
    procedure SetLockGroup(const AValue: Boolean);
    function GetLockAdjustHandles: Boolean;
    procedure SetLockAdjustHandles(const AValue: Boolean);
    function GetLockText: Boolean;
    procedure SetLockText(const AValue: Boolean);
    function GetLockVertices: Boolean;
    procedure SetLockVertices(const AValue: Boolean);
    function GetLockCropping: Boolean;
    procedure SetLockCropping(const AValue: Boolean);
    function GetLockSelect: Boolean;
    procedure SetLockSelect(const AValue: Boolean);
    function GetLockPosition: Boolean;
    procedure SetLockPosition(const AValue: Boolean);
    function GetLockAspectRatio: Boolean;
    procedure SetLockAspectRatio(const AValue: Boolean);
    function GetLockRotation: Boolean;
    procedure SetLockRotation(const AValue: Boolean);
    function GetLockUngroup: Boolean;
    procedure SetLockUngroup(const AValue: Boolean);
    function GetUseLockGroup: Boolean;
    procedure SetUseLockGroup(const AValue: Boolean);
    function GetUseLockAdjustHandles: Boolean;
    procedure SetUseLockAdjustHandles(const AValue: Boolean);
    function GetUseLockText: Boolean;
    procedure SetUseLockText(const AValue: Boolean);
    function GetUseLockVertices: Boolean;
    procedure SetUseLockVertices(const AValue: Boolean);
    function GetUseLockCropping: Boolean;
    procedure SetUseLockCropping(const AValue: Boolean);
    function GetUseLockSelect: Boolean;
    procedure SetUseLockSelect(const AValue: Boolean);
    function GetUseLockPosition: Boolean;
    procedure SetUseLockPosition(const AValue: Boolean);
    function GetUseLockAspectRatio: Boolean;
    procedure SetUseLockAspectRatio(const AValue: Boolean);
    function GetUseLockRotation: Boolean;
    procedure SetUseLockRotation(const AValue: Boolean);
    function GetUseLockUngroup: Boolean;
    procedure SetUseLockUngroup(const AValue: Boolean);
  protected
    function GetComplex: Boolean; override;
  public
    constructor Create; override;

    property LockGroup: Boolean read GetLockGroup write SetLockGroup;
    property LockAdjustHandles: Boolean read GetLockAdjustHandles write SetLockAdjustHandles;
    property LockText: Boolean read GetLockText write SetLockText;
    property LockVertices: Boolean read GetLockVertices write SetLockVertices;
    property LockCropping: Boolean read GetLockCropping write SetLockCropping;
    property LockSelect: Boolean read GetLockSelect write SetLockSelect;
    property LockPosition: Boolean read GetLockPosition write SetLockPosition;
    property LockAspectRatio: Boolean read GetLockAspectRatio write SetLockAspectRatio;
    property LockRotation: Boolean read GetLockRotation write SetLockRotation;
    property LockUngroup: Boolean read GetLockUngroup write SetLockUngroup;
    property UseLockGroup: Boolean read GetUseLockGroup write SetUseLockGroup;
    property UseLockAdjustHandles: Boolean read GetUseLockAdjustHandles write SetUseLockAdjustHandles;
    property UseLockText: Boolean read GetUseLockText write SetUseLockText;
    property UseLockVertices: Boolean read GetUseLockVertices write SetUseLockVertices;
    property UseLockCropping: Boolean read GetUseLockCropping write SetUseLockCropping;
    property UseLockSelect: Boolean read GetUseLockSelect write SetUseLockSelect;
    property UseLockPosition: Boolean read GetUseLockPosition write SetUseLockPosition;
    property UseLockAspectRatio: Boolean read GetUseLockAspectRatio write SetUseLockAspectRatio;
    property UseLockRotation: Boolean read GetUseLockRotation write SetUseLockRotation;
    property UseLockUngroup: Boolean read GetUseLockUngroup write SetUseLockUngroup;
  end;

  { TdxDrawingBlipIdentifier }

  TdxDrawingBlipIdentifier = class(TdxOfficeDrawingIntPropertyBase)
  protected
    function GetComplex: Boolean; override;
  public
    procedure Execute(AOwner: TdxOfficeArtPropertiesBase); override;
  end;

  { TdxDrawingTextIdentifier }

  TdxDrawingTextIdentifier = class(TdxOfficeDrawingIntPropertyBase)
  public const
    Coeff = Integer($10000);
  protected
    function GetComplex: Boolean; override;
  public
    procedure Execute(AOwner: TdxOfficeArtPropertiesBase); override;
    procedure Write(AWriter: TBinaryWriter); override;
  end;

  { TdxDrawingBlipBooleanProperties }

  TdxDrawingBlipBooleanProperties = class(TdxOfficeDrawingBooleanPropertyBase)
  public const
    CPictureActive           = Integer($000001);
    CPictureBiLevel          = Integer($000002);
    CPictureGray             = Integer($000004);
    CNoHitTestPicture        = Integer($000008);
    CLooping                 = Integer($000010);
    CRewind                  = Integer($000020);
    CPicturePreserveGrays    = Integer($000040);
    CUsePictureActive        = Integer($010000);
    CUsePictureBiLevel       = Integer($020000);
    CUsePictureGray          = Integer($040000);
    CUseNoHitTestPicture     = Integer($080000);
    CUseLooping              = Integer($100000);
    CUseRewind               = Integer($200000);
    CUsePicturePreserveGrays = Integer($400000);
  strict private
    function GetUsePicturePreserveGrays: Boolean;
    procedure SetUsePicturePreserveGrays(const AValue: Boolean);
    function GetUseRewind: Boolean;
    procedure SetUseRewind(const AValue: Boolean);
    function GetUseLooping: Boolean;
    procedure SetUseLooping(const AValue: Boolean);
    function GetUseNoHitTestPicture: Boolean;
    procedure SetUseNoHitTestPicture(const AValue: Boolean);
    function GetUsePictureGray: Boolean;
    procedure SetUsePictureGray(const AValue: Boolean);
    function GetUsePictureBiLevel: Boolean;
    procedure SetUsePictureBiLevel(const AValue: Boolean);
    function GetUsePictureActive: Boolean;
    procedure SetUsePictureActive(const AValue: Boolean);
    function GetPicturePreserveGrays: Boolean;
    procedure SetPicturePreserveGrays(const AValue: Boolean);
    function GetRewind: Boolean;
    procedure SetRewind(const AValue: Boolean);
    function GetLooping: Boolean;
    procedure SetLooping(const AValue: Boolean);
    function GetNoHitTestPicture: Boolean;
    procedure SetNoHitTestPicture(const AValue: Boolean);
    function GetPictureGray: Boolean;
    procedure SetPictureGray(const AValue: Boolean);
    function GetPictureBiLevel: Boolean;
    procedure SetPictureBiLevel(const AValue: Boolean);
    function GetPictureActive: Boolean;
    procedure SetPictureActive(const AValue: Boolean);
  protected
    function GetComplex: Boolean; override;
  public
    property UsePicturePreserveGrays: Boolean read GetUsePicturePreserveGrays write SetUsePicturePreserveGrays;
    property UseRewind: Boolean read GetUseRewind write SetUseRewind;
    property UseLooping: Boolean read GetUseLooping write SetUseLooping;
    property UseNoHitTestPicture: Boolean read GetUseNoHitTestPicture write SetUseNoHitTestPicture;
    property UsePictureGray: Boolean read GetUsePictureGray write SetUsePictureGray;
    property UsePictureBiLevel: Boolean read GetUsePictureBiLevel write SetUsePictureBiLevel;
    property UsePictureActive: Boolean read GetUsePictureActive write SetUsePictureActive;
    property PicturePreserveGrays: Boolean read GetPicturePreserveGrays write SetPicturePreserveGrays;
    property Rewind: Boolean read GetRewind write SetRewind;
    property Looping: Boolean read GetLooping write SetLooping;
    property NoHitTestPicture: Boolean read GetNoHitTestPicture write SetNoHitTestPicture;
    property PictureGray: Boolean read GetPictureGray write SetPictureGray;
    property PictureBiLevel: Boolean read GetPictureBiLevel write SetPictureBiLevel;
    property PictureActive: Boolean read GetPictureActive write SetPictureActive;
  end;

  { TdxDrawingFillStyleBooleanProperties }

  TdxDrawingFillStyleBooleanProperties = class(TdxOfficeDrawingBooleanPropertyBase)
  public type
    TFillStyle = class
    public const
      NoFillHitTest           = $01;
      FillUseRect             = $02;
      FillShape               = $04;
      HitTestFill             = $08;
      Filled                  = $10;
      ShapeAnchor             = $20;
      RecolorFillAsPicture    = $40;
      UseNoFillHitTest        = $010000;
      UseFillUseRect          = $020000;
      UseFillShape            = $040000;
      UseHitTestFill          = $080000;
      UseFilled               = $100000;
      UseShapeAnchor          = $200000;
      UseRecolorFillAsPicture = $400000;
    end;
  strict private
    FFillStyle: Integer;
    function GetNoFillHitTest: Boolean;
    procedure SetNoFillHitTest(const AValue: Boolean);
    function GetUseNoFillHitTest: Boolean;
    procedure SetUseNoFillHitTest(const AValue: Boolean);
    function GetFillUseRect: Boolean;
    procedure SetFillUseRect(const AValue: Boolean);
    function GetUseFillUseRect: Boolean;
    procedure SetUseFillUseRect(const AValue: Boolean);
    function GetFillShape: Boolean;
    procedure SetFillShape(const AValue: Boolean);
    function GetUseFillShape: Boolean;
    procedure SetUseFillShape(const AValue: Boolean);
    function GetHitTestFill: Boolean;
    procedure SetHitTestFill(const AValue: Boolean);
    function GetUseHitTestFill: Boolean;
    procedure SetUseHitTestFill(const AValue: Boolean);
    function GetFilled: Boolean;
    procedure SetFilled(const AValue: Boolean);
    function GetUseFilled: Boolean;
    procedure SetUseFilled(const AValue: Boolean);
    function GetShapeAnchor: Boolean;
    procedure SetShapeAnchor(const AValue: Boolean);
    function GetUseShapeAnchor: Boolean;
    procedure SetUseShapeAnchor(const AValue: Boolean);
    function GetRecolorFillAsPicture: Boolean;
    procedure SetRecolorFillAsPicture(const AValue: Boolean);
    function GetUseRecolorFillAsPicture: Boolean;
    procedure SetUseRecolorFillAsPicture(const AValue: Boolean);
  protected
    function GetComplex: Boolean; override;
  public
    constructor Create; override;
    procedure Read(AReader: TBinaryReader); override;
    procedure Execute(AOwner: TdxOfficeArtPropertiesBase); override;
    procedure Write(AWriter: TBinaryWriter); override;

    property NoFillHitTest: Boolean read GetNoFillHitTest write SetNoFillHitTest;
    property UseNoFillHitTest: Boolean read GetUseNoFillHitTest write SetUseNoFillHitTest;
    property FillUseRect: Boolean read GetFillUseRect write SetFillUseRect;
    property UseFillUseRect: Boolean read GetUseFillUseRect write SetUseFillUseRect;
    property FillShape: Boolean read GetFillShape write SetFillShape;
    property UseFillShape: Boolean read GetUseFillShape write SetUseFillShape;
    property HitTestFill: Boolean read GetHitTestFill write SetHitTestFill;
    property UseHitTestFill: Boolean read GetUseHitTestFill write SetUseHitTestFill;
    property Filled: Boolean read GetFilled write SetFilled;
    property UseFilled: Boolean read GetUseFilled write SetUseFilled;
    property ShapeAnchor: Boolean read GetShapeAnchor write SetShapeAnchor;
    property UseShapeAnchor: Boolean read GetUseShapeAnchor write SetUseShapeAnchor;
    property RecolorFillAsPicture: Boolean read GetRecolorFillAsPicture write SetRecolorFillAsPicture;
    property UseRecolorFillAsPicture: Boolean read GetUseRecolorFillAsPicture write SetUseRecolorFillAsPicture;
  end;

  { TdxDrawingLineStyleBooleanProperties }

  TdxDrawingLineStyleBooleanProperties = class(TdxOfficeDrawingBooleanPropertyBase)
  public type
    TDrawingLineStyle = class
    public const
      Line           = $8;
      ArrowheadOK    = $10;
      UseLine        = $80000;
      UseArrowheadOK = $100000;
    end;
  strict private
    FLineStyle: Integer;
    function GetLine: Boolean;
    procedure SetLine(const AValue: Boolean);
    function GetUseLine: Boolean;
    procedure SetUseLine(const AValue: Boolean);
  protected
    function GetComplex: Boolean; override;
  public
    constructor Create; override;
    procedure Read(AReader: TBinaryReader); override;
    procedure Execute(AOwner: TdxOfficeArtPropertiesBase); override;
    procedure Write(AWriter: TBinaryWriter); override;

    property Line: Boolean read GetLine write SetLine;
    property UseLine: Boolean read GetUseLine write SetUseLine;
  end;

  { TdxDrawingGroupShapeBooleanProperties }

  TdxDrawingGroupShapeBooleanProperties = class(TdxOfficeDrawingBooleanPropertyBase)
  public const
    CPrint             = Integer($00000001);
    CHidden            = Integer($00000002);
    CBehindDocument    = Integer($00000020);
    CLayoutInCell      = Integer($00008000);
    CUsePrint          = Integer($00010000);
    CUseHidden         = Integer($00020000);
    CUseBehindDocument = Integer($00200000);
    CUseLayoutInCell   = Integer(MinInt);
  strict private
    function GetPrint: Boolean;
    procedure SetPrint(const AValue: Boolean);
    function GetHidden: Boolean;
    procedure SetHidden(const AValue: Boolean);
    function GetIsBehindDoc: Boolean;
    procedure SetIsBehindDoc(const AValue: Boolean);
    function GetLayoutInCell: Boolean;
    procedure SetLayoutInCell(const AValue: Boolean);
    function GetUsePrint: Boolean;
    procedure SetUsePrint(const AValue: Boolean);
    function GetUseHidden: Boolean;
    procedure SetUseHidden(const AValue: Boolean);
    function GetUseBehindDocument: Boolean;
    procedure SetUseBehindDocument(const AValue: Boolean);
    function GetUseLayoutInCell: Boolean;
    procedure SetUseLayoutInCell(const AValue: Boolean);
  protected
    function GetComplex: Boolean; override;
  public
    constructor Create; override;
    procedure Execute(AOwner: TdxOfficeArtPropertiesBase); override;

    property Print: Boolean read GetPrint write SetPrint;
    property Hidden: Boolean read GetHidden write SetHidden;
    property IsBehindDoc: Boolean read GetIsBehindDoc write SetIsBehindDoc;
    property LayoutInCell: Boolean read GetLayoutInCell write SetLayoutInCell;
    property UsePrint: Boolean read GetUsePrint write SetUsePrint;
    property UseHidden: Boolean read GetUseHidden write SetUseHidden;
    property UseBehindDocument: Boolean read GetUseBehindDocument write SetUseBehindDocument;
    property UseLayoutInCell: Boolean read GetUseLayoutInCell write SetUseLayoutInCell;
  end;

  { TdxDrawingShadowStyleBooleanProperties }

  TdxDrawingShadowStyleBooleanProperties = class(TdxOfficeDrawingBooleanPropertyBase)
  public const
    CShadowObscured    = Integer($00000001);
    CShadow            = Integer($00000002);
    CUseShadowObscured = Integer($00010000);
    CUseShadow         = Integer($00020000);
  strict private
    function GetShadowObscured: Boolean;
    procedure SetShadowObscured(const AValue: Boolean);
    function GetShadow: Boolean;
    procedure SetShadow(const AValue: Boolean);
    function GetUseShadowObscured: Boolean;
    procedure SetUseShadowObscured(const AValue: Boolean);
    function GetUseShadow: Boolean;
    procedure SetUseShadow(const AValue: Boolean);
  protected
    function GetComplex: Boolean; override;
  public
    property ShadowObscured: Boolean read GetShadowObscured write SetShadowObscured;
    property Shadow: Boolean read GetShadow write SetShadow;
    property UseShadowObscured: Boolean read GetUseShadowObscured write SetUseShadowObscured;
    property UseShadow: Boolean read GetUseShadow write SetUseShadow;
  end;

  { TdxDrawingShapeBooleanProperties }

  TdxDrawingShapeBooleanProperties = class(TdxOfficeDrawingBooleanPropertyBase)
  public const
    CBackground              = Integer($00000001);
    CLockShapeType           = Integer($00000008);
    CPreferRelativeResize    = Integer($00000010);
    CFlipVOverride           = Integer($00000040);
    CFlipHOverride           = Integer($00000080);
    CUseBackground           = Integer($00010000);
    CUseLockShapeType        = Integer($00080000);
    CUsePreferRelativeResize = Integer($00100000);
    CUseFlipVOverride        = Integer($00400000);
    CUseFlipHOverride        = Integer($00800000);
  strict private
    function GetIsBackground: Boolean;
    procedure SetIsBackground(const AValue: Boolean);
    function GetLockShapeType: Boolean;
    procedure SetLockShapeType(const AValue: Boolean);
    function GetPreferRelativeResize: Boolean;
    procedure SetPreferRelativeResize(const AValue: Boolean);
    function GetFlipVOverride: Boolean;
    procedure SetFlipVOverride(const AValue: Boolean);
    function GetFlipHOverride: Boolean;
    procedure SetFlipHOverride(const AValue: Boolean);
    function GetUseBackground: Boolean;
    procedure SetUseBackground(const AValue: Boolean);
    function GetUseLockShapeType: Boolean;
    procedure SetUseLockShapeType(const AValue: Boolean);
    function GetUsePreferRelativeResize: Boolean;
    procedure SetUsePreferRelativeResize(const AValue: Boolean);
    function GetUseFlipVOverride: Boolean;
    procedure SetUseFlipVOverride(const AValue: Boolean);
    function GetUseFlipHOverride: Boolean;
    procedure SetUseFlipHOverride(const AValue: Boolean);
  protected
    function GetComplex: Boolean; override;
  public
    constructor Create; override;
    property IsBackground: Boolean read GetIsBackground write SetIsBackground;
    property LockShapeType: Boolean read GetLockShapeType write SetLockShapeType;
    property PreferRelativeResize: Boolean read GetPreferRelativeResize write SetPreferRelativeResize;
    property FlipVOverride: Boolean read GetFlipVOverride write SetFlipVOverride;
    property FlipHOverride: Boolean read GetFlipHOverride write SetFlipHOverride;
    property UseBackground: Boolean read GetUseBackground write SetUseBackground;
    property UseLockShapeType: Boolean read GetUseLockShapeType write SetUseLockShapeType;
    property UsePreferRelativeResize: Boolean read GetUsePreferRelativeResize write SetUsePreferRelativeResize;
    property UseFlipVOverride: Boolean read GetUseFlipVOverride write SetUseFlipVOverride;
    property UseFlipHOverride: Boolean read GetUseFlipHOverride write SetUseFlipHOverride;
  end;

  { TdxOfficeDrawingStringPropertyValueBase }

  TdxOfficeDrawingStringPropertyValueBase = class abstract(TdxOfficeDrawingIntPropertyBase)
  strict private
    FData: string;
    function GetData: string;
    procedure SetData(const AValue: string);
  protected
    function GetComplex: Boolean; override;
  public
    function GetStringData: string;
    procedure SetStringData(const AValue: string);

    property Data: string read GetData write SetData;
  end;

  { TdxDrawingShapeName }

  TdxDrawingShapeName = class(TdxOfficeDrawingStringPropertyValueBase);

  { TdxDrawingTextBooleanProperties }

  TdxDrawingTextBooleanProperties = class(TdxOfficeDrawingBooleanPropertyBase)
  public const
    CFitShapeToText     = Integer($00000002);
    CAutoTextMargins    = Integer($00000008);
    CSelectText         = Integer($00000010);
    CUseFitShapeToText  = Integer($00020000);
    CUseAutoTextMargins = Integer($00080000);
    CUseSelectText      = Integer($00100000);
  strict private
    function GetFitShapeToText: Boolean;
    procedure SetFitShapeToText(const AValue: Boolean);
    function GetAutoTextMargins: Boolean;
    procedure SetAutoTextMargins(const AValue: Boolean);
    function GetSelectText: Boolean;
    procedure SetSelectText(const AValue: Boolean);
    function GetUseFitShapeToText: Boolean;
    procedure SetUseFitShapeToText(const AValue: Boolean);
    function GetUseAutoTextMargins: Boolean;
    procedure SetUseAutoTextMargins(const AValue: Boolean);
    function GetUseSelectText: Boolean;
    procedure SetUseSelectText(const AValue: Boolean);
  protected
    function GetComplex: Boolean; override;
  public
    constructor Create; override;
    procedure Execute(AOwner: TdxOfficeArtPropertiesBase); override;

    property FitShapeToText: Boolean read GetFitShapeToText write SetFitShapeToText;
    property AutoTextMargins: Boolean read GetAutoTextMargins write SetAutoTextMargins;
    property SelectText: Boolean read GetSelectText write SetSelectText;
    property UseFitShapeToText: Boolean read GetUseFitShapeToText write SetUseFitShapeToText;
    property UseAutoTextMargins: Boolean read GetUseAutoTextMargins write SetUseAutoTextMargins;
    property UseSelectText: Boolean read GetUseSelectText write SetUseSelectText;
  end;

  { TdxDrawingGroupShape2PctHorizPos }

  TdxDrawingGroupShape2PctHorizPos = class(TdxOfficeDrawingIntPropertyBase)
  public
    constructor Create(AValue: Integer); overload;
    constructor Create; override;
    procedure Execute(AOwner: TdxOfficeArtPropertiesBase); override;
  end;

  { TdxDrawingGroupShape2PctVertPos }

  TdxDrawingGroupShape2PctVertPos = class(TdxOfficeDrawingIntPropertyBase)
  public
    constructor Create(AValue: Integer); overload;
    constructor Create; override;
    procedure Execute(AOwner: TdxOfficeArtPropertiesBase); override;
  end;

  { TdxDrawingGroupShape2PctHoriz }

  TdxDrawingGroupShape2PctHoriz = class(TdxOfficeDrawingIntPropertyBase)
  public
    constructor Create(AValue: Integer); overload;
    constructor Create; override;
    procedure Execute(AOwner: TdxOfficeArtPropertiesBase); override;
  end;

  { TdxDrawingGroupShape2PctVert }

  TdxDrawingGroupShape2PctVert = class(TdxOfficeDrawingIntPropertyBase)
  public
    constructor Create(AValue: Integer); overload;
    constructor Create; override;
    procedure Execute(AOwner: TdxOfficeArtPropertiesBase); override;
  end;

  { TdxDiagramBooleanProperties }

  TdxDiagramBooleanProperties = class(TdxOfficeDrawingIntPropertyBase)
  public const
    DefaultFlags = Integer($00010000);
  public
    constructor Create; override;
  end;

  { TdxDrawingWrapLeftDistance }

  TdxDrawingWrapLeftDistance = class(TdxOfficeDrawingIntPropertyBase)
  public const
    DefaultValue = Integer($0001BE7C);
  protected
    function GetComplex: Boolean; override;
  public
    constructor Create(AValue: Integer); overload;
    constructor Create; override;
    procedure Execute(AOwner: TdxOfficeArtPropertiesBase); override;
  end;

  { TdxDrawingWrapRightDistance }

  TdxDrawingWrapRightDistance = class(TdxOfficeDrawingIntPropertyBase)
  public const
    DefaultValue = Integer($0001BE7C);
  protected
    function GetComplex: Boolean; override;
  public
    constructor Create(AValue: Integer); overload;
    constructor Create; override;
    procedure Execute(AOwner: TdxOfficeArtPropertiesBase); override;
  end;

  { TdxDrawingWrapTopDistance }

  TdxDrawingWrapTopDistance = class(TdxOfficeDrawingIntPropertyBase)
  public const
    DefaultValue = Integer($0);
  protected
    function GetComplex: Boolean; override;
  public
    constructor Create(AValue: Integer); overload;
    constructor Create; override;
    procedure Execute(AOwner: TdxOfficeArtPropertiesBase); override;
  end;

  { TdxDrawingWrapBottomDistance }

  TdxDrawingWrapBottomDistance = class(TdxOfficeDrawingIntPropertyBase)
  public const
    DefaultValue = Integer($0);
  protected
    function GetComplex: Boolean; override;
  public
    constructor Create(AValue: Integer); overload;
    constructor Create; override;
    procedure Execute(AOwner: TdxOfficeArtPropertiesBase); override;
  end;

  { TdxOfficePropertiesFactory }

  TdxOfficePropertiesFactory = class
  strict private
    class var
      FPropertyTypes: TDictionary<SmallInt, TdxOfficeDrawingPropertyClass>;
      FPropertyIdentifiers: TDictionary<TdxOfficeDrawingPropertyClass, SmallInt>;
    class constructor Initialize;
    class destructor Finalize;
    class procedure AddProperty(AId: Integer; AType: TdxOfficeDrawingPropertyClass); static;
  public
    class function CreateProperty(AReader: TBinaryReader): IdxOfficeDrawingProperty; static;
    class function GetOpcodeByType(APropertyType: TClass): SmallInt; static;
  end;

  { TdxBlipsWithProperties }

  TdxBlipsWithProperties = class
  strict private
    FBlips: TDictionary<Integer, TdxBlipBase>;
    FShapeArtProperties: TDictionary<Integer, TdxOfficeArtProperties>;
    FShapeArtTertiaryProperties: TDictionary<Integer, TdxOfficeArtTertiaryProperties>;
    FShapeRecords: TDictionary<Integer, TdxOfficeArtShapeRecord>;
  public
    constructor Create;
    destructor Destroy; override;

    property ShapeArtProperties: TDictionary<Integer, TdxOfficeArtProperties> read FShapeArtProperties;
    property ShapeArtTertiaryProperties: TDictionary<Integer, TdxOfficeArtTertiaryProperties> read FShapeArtTertiaryProperties;
    property ShapeRecords: TDictionary<Integer, TdxOfficeArtShapeRecord> read FShapeRecords;
    property Blips: TDictionary<Integer, TdxBlipBase> read FBlips;
  end;

  { TdxOfficeArtShapeGroupContainer }

  TdxOfficeArtShapeGroupContainer = class(TdxCompositeOfficeDrawingPartBase)
  protected
    function GetHeaderInstanceInfo: Integer; override;
    function GetHeaderTypeCode: Integer; override;
    function GetHeaderVersion: Integer; override;
    procedure Read(AReader: TBinaryReader; AHeader: TdxOfficeArtRecordHeader);
    function CreateShapeContainer(AReader: TBinaryReader): TdxOfficeDrawingPartBase;
  public
    constructor Create(ALocation: Byte); overload;
    class function FromStream(AReader: TBinaryReader; AHeader: TdxOfficeArtRecordHeader): TdxOfficeArtShapeGroupContainer; static;
  end;

  { TdxOfficeArtShapeGroupCoordinateSystem }

  TdxOfficeArtShapeGroupCoordinateSystem = class(TdxOfficeDrawingPartBase)
  public const
    RecordLength = Integer($10);
  strict private
    FLeft: Integer;
    FTop: Integer;
    FRight: Integer;
    FBottom: Integer;
  protected
    function GetHeaderInstanceInfo: Integer; override;
    function GetHeaderTypeCode: Integer; override;
    function GetHeaderVersion: Integer; override;
    procedure WriteCore(AWriter: TBinaryWriter); override;
    function GetSize: Integer; override;
  public
    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop write FTop;
    property Right: Integer read FRight write FRight;
    property Bottom: Integer read FBottom write FBottom;
  end;

  { TdxOfficeArtTopmostShapeContainer }

  TdxOfficeArtTopmostShapeContainer = class(TdxOfficeDrawingPartBase)
  public const
    RecordLength         = Integer($28);
    MainTopmostShapeId   = Integer($400);
    HeaderTopmostShapeId = Integer($800);
    TopmostShapeFlags    = Integer($05);
  strict private
    FCoordinateSystem: TdxOfficeArtShapeGroupCoordinateSystem;
    FShapeRecord: TdxOfficeArtShapeRecord;
  protected
    function GetHeaderInstanceInfo: Integer; override;
    function GetHeaderTypeCode: Integer; override;
    function GetHeaderVersion: Integer; override;
    procedure WriteCore(AWriter: TBinaryWriter); override;
    function GetSize: Integer; override;
  public
    constructor Create(ALocation: Byte);
    destructor Destroy; override;
    procedure InitializeShapeRecord(ALocation: Byte);

    property CoordinateSystem: TdxOfficeArtShapeGroupCoordinateSystem read FCoordinateSystem;
    property ShapeRecord: TdxOfficeArtShapeRecord read FShapeRecord;
  end;

  { TdxOfficeArtDrawingObjectsContainerBase }

  TdxOfficeArtDrawingObjectsContainerBase = class abstract(TdxCompositeOfficeDrawingPartBase)
  strict private
    FDrawingData: TdxOfficeArtFileDrawingRecord;
    FShapeGroup: TdxOfficeArtShapeGroupContainer;
  protected
    function GetBackgroundShape: TdxOfficeArtShapeContainer; virtual; abstract;
    function GetHeaderInstanceInfo: Integer; override;
    function GetHeaderTypeCode: Integer; override;
    function GetHeaderVersion: Integer; override;
    procedure Read(AReader: TBinaryReader);
    procedure ParseHeader(AReader: TBinaryReader);
    function TryRead(AReader: TBinaryReader; AHeader: TdxOfficeArtRecordHeader): Boolean; virtual;
  public
    constructor Create(ALocation: Byte; ADrawingId, AShapeIdentifier: Integer); overload;
    constructor Create; overload;
    procedure CheckHeader(AHeader: TdxOfficeArtRecordHeader);
    procedure SetBackgroundShapeProperties; virtual; abstract;

    property DrawingData: TdxOfficeArtFileDrawingRecord read FDrawingData;
    property ShapeGroup: TdxOfficeArtShapeGroupContainer read FShapeGroup;
    property BackgroundShape: TdxOfficeArtShapeContainer read GetBackgroundShape;
  end;

  { TdxOfficeArtWordDrawing }

  TdxOfficeArtWordDrawing = class
  strict private
    FLocation: Byte;
    FDrawingContainer: TdxOfficeArtDrawingObjectsContainerBase;
  protected
    procedure Read(AReader: TBinaryReader);
    function ReadDrawingObjectsContainer(AReader: TBinaryReader): TdxOfficeArtDrawingObjectsContainerBase;
  public
    constructor Create; overload;
    constructor Create(ALocation: Byte; ADrawingId: Integer; AShapeId: Integer); overload;
    destructor Destroy; override;
    class function FromStream(AReader: TBinaryReader): TdxOfficeArtWordDrawing; static;
    function CreateDrawingObjectsContainer(ADrawingId: Integer; AShapeId: Integer): TdxOfficeArtDrawingObjectsContainerBase;
    procedure Write(AWriter: TBinaryWriter);

    property Location: Byte read FLocation;
    property DrawingObjectsContainer: TdxOfficeArtDrawingObjectsContainerBase read FDrawingContainer;
  end;

  { TdxOfficeArtFloatingShapeContainer }

  TdxOfficeArtFloatingShapeContainer = class(TdxOfficeArtShapeContainer)
  public
    constructor Create;
  end;

  { TdxOfficeArtContent }

  TdxOfficeArtContent = class
  public const
    MainDocumentDrawingLocation   = Integer(0);
    HeaderDocumentDrawingLocation = Integer(1);
  protected type

    TOfficeArtContentHelper = class
    strict private
      FActiveShapes: TdxBlipsWithProperties;
      FContent: TdxOfficeArtContent;
    public
      constructor Create(AContent: TdxOfficeArtContent);
      procedure Traverse;
      procedure Process(AShapes: TdxList<TdxOfficeDrawingPartBase>);
    end;

  strict private
    FContentHelper: TOfficeArtContentHelper;
    FDrawingContainer: TdxOfficeArtDrawingContainer;
    FDrawings: TdxObjectList<TdxOfficeArtWordDrawing>;
    FMainDocumentDrawing: TdxOfficeArtWordDrawing;
    FHeaderDrawing: TdxOfficeArtWordDrawing;
    FMainDocumentBlips: TdxBlipsWithProperties;
    FHeadersBlips: TdxBlipsWithProperties;
    function GetMainDocumentDrawing: TdxOfficeArtWordDrawing;
    function GetHeaderDrawing: TdxOfficeArtWordDrawing;
  protected
    procedure InitMainDocumentDrawing;
    procedure InitHeaderDrawing;
    procedure Read(AReader: TBinaryReader; AEmbeddedReader: TBinaryReader;
      AOffset: Integer; ASize: Integer; const AImageCreator: IdxDocOfficeImageCreator);
  public
    constructor Create;
    destructor Destroy; override;
    class function FromStream(AReader, AEmbeddedReader: TBinaryReader;
      AOffset, ASize: Integer; const AImageCreator: IdxDocOfficeImageCreator): TdxOfficeArtContent; static;
    procedure SetPageBackColor(APageBackColor: TdxAlphaColor);
    procedure InsertPictureFloatingObject(AImage: TdxOfficeImageReference; AState: TdxDocContentState;
      AShapeIdentifier: Integer; ARun: TdxFloatingObjectAnchorRun; AUnitConverter: TdxDocumentModelUnitConverter);
    procedure InsertTextBoxFloatingObject(AState: TdxDocContentState; AShapeIdentifier, ATextIdentifier: Integer;
      ARun: TdxFloatingObjectAnchorRun; AUnitConverter: TdxDocumentModelUnitConverter);
    procedure InsertPictureFloatingObjectCore(AImage: TdxOfficeImageReference; AState: TdxDocContentState;
      AShapeIdentifier: Integer; ARun: TdxFloatingObjectAnchorRun; AUnitConverter: TdxDocumentModelUnitConverter);
    procedure InitializeArtProperties(AArtProperties: TdxOfficeArtProperties; ARun: TdxFloatingObjectAnchorRun;
      AUnitConverter: TdxDocumentModelUnitConverter);
    procedure InsertTextBoxFloatingObjectCore(AState: TdxDocContentState; AShapeIdentifier, ATextIdentifier: Integer;
      ARun: TdxFloatingObjectAnchorRun; AUnitConverter: TdxDocumentModelUnitConverter);
    procedure ApplyFloatingObjectDistancesProperties(AArtProperties: TdxOfficeArtProperties;
      AArtTertiaryProperties: TdxOfficeArtTertiaryProperties; AFloatingObjectProperties: TdxFloatingObjectProperties);
    procedure ApplyRelativeWidth(AArtTertiaryProperties: TdxOfficeArtTertiaryProperties; AFloatingObjectProperties: TdxFloatingObjectProperties);
    procedure ApplyRelativeHeight(AArtTertiaryProperties: TdxOfficeArtTertiaryProperties; AFloatingObjectProperties: TdxFloatingObjectProperties);
    procedure ApplyHorizontalPositionAlignment(AArtTertiaryProperties: TdxOfficeArtTertiaryProperties; AFloatingObjectProperties: TdxFloatingObjectProperties);
    procedure ApplyVerticalPositionAlignment(AArtTertiaryProperties: TdxOfficeArtTertiaryProperties; AFloatingObjectProperties: TdxFloatingObjectProperties);
    procedure ApplyPercentOffset(AArtTertiaryProperties: TdxOfficeArtTertiaryProperties; AFloatingObjectProperties: TdxFloatingObjectProperties);
    function GetPosRelH(AType: TdxFloatingObjectHorizontalPositionType): TdxDrawingGroupShapePosRelH.TMsoprh;
    function GetPosRelV(AType: TdxFloatingObjectVerticalPositionType): TdxDrawingGroupShapePosRelV.TMsoprv;
    function GetPosH(AAlignment: TdxFloatingObjectHorizontalPositionAlignment): TdxDrawingGroupShapePosH.TMsoph;
    function GetPosV(AAlignment: TdxFloatingObjectVerticalPositionAlignment): TdxDrawingGroupShapePosV.TMsopv;
    procedure ApplyTextBoxProperties(AArtProperties: TdxOfficeArtProperties; ATextBoxProperties: TdxTextBoxProperties);
    function CreateShapeContainer(AState: TdxDocContentState; AShapeIdentifier: Integer): TdxOfficeArtFloatingShapeContainer;
    function GetDrawingByState(AState: TdxDocContentState): TdxOfficeArtWordDrawing;
    procedure Write(AWriter: TBinaryWriter; AEmbeddedWriter: TBinaryWriter);

    property DrawingContainer: TdxOfficeArtDrawingContainer read FDrawingContainer;
    property MainDocumentBlips: TdxBlipsWithProperties read FMainDocumentBlips;
    property MainDocumentDrawing: TdxOfficeArtWordDrawing read GetMainDocumentDrawing;
    property HeadersBlips: TdxBlipsWithProperties read FHeadersBlips;
    property Drawings: TdxObjectList<TdxOfficeArtWordDrawing> read FDrawings;
    property HeaderDrawing: TdxOfficeArtWordDrawing read GetHeaderDrawing;
  end;

  { TdxOfficeArtDrawingObjectsContainerSupprortsBackgroundShape }

  TdxOfficeArtDrawingObjectsContainerSupprortsBackgroundShape = class(TdxOfficeArtDrawingObjectsContainerBase)
  public const
    BackgroundFlag = Integer($400);
  strict private
    FBackgroundShape: TdxOfficeArtShapeContainer;
  protected
    function GetBackgroundShape: TdxOfficeArtShapeContainer; override;
    function TryRead(AReader: TBinaryReader; AHeader: TdxOfficeArtRecordHeader): Boolean; override;
    procedure WriteCore(AWriter: TBinaryWriter); override;
  public
    constructor Create; overload;
    constructor Create(ALocation: Byte; ADrawingId: Integer; AShapeIdentifier: Integer); overload;
    class function FromStream(AReader: TBinaryReader): TdxOfficeArtDrawingObjectsContainerSupprortsBackgroundShape; static;
    function CreateBackgroundShape(AShapeIdentifier: Integer): TdxOfficeArtShapeContainer;
    procedure SetBackgroundShapeProperties; override;
  end;

  { TdxOfficeArtDrawingObjectsContainer }

  TdxOfficeArtDrawingObjectsContainer = class(TdxOfficeArtDrawingObjectsContainerBase)
  protected
    function GetBackgroundShape: TdxOfficeArtShapeContainer; override;
  public
    class function FromStream(AReader: TBinaryReader): TdxOfficeArtDrawingObjectsContainer; static;
    procedure SetBackgroundShapeProperties; override;
  end;

  { TdxOfficeClientAnchor }

  TdxOfficeClientAnchor = class(TdxOfficeDrawingPartBase)
  public const
    RecordLength = Integer(4);
    Data         = Integer($0000);
  protected
    function GetHeaderInstanceInfo: Integer; override;
    function GetHeaderTypeCode: Integer; override;
    function GetHeaderVersion: Integer; override;
    procedure WriteCore(AWriter: TBinaryWriter); override;
    function GetSize: Integer; override;
  end;

  { TdxOfficeClientData }

  TdxOfficeClientData = class(TdxOfficeDrawingPartBase)
  public const
    RecordLength = Integer(4);
    Data         = Integer($0001);
  protected
    function GetHeaderInstanceInfo: Integer; override;
    function GetHeaderTypeCode: Integer; override;
    function GetHeaderVersion: Integer; override;
    procedure WriteCore(AWriter: TBinaryWriter); override;
    function GetSize: Integer; override;
  end;

  TdxBlackWhiteMode = class
  public const
    Normal = $0000;
    Automatic = $0001;
    GrayScale = $0002;
    LightGrayScale = $0003;
    InverseGray = $0004;
    GrayOutline = $0005;
    BlackTextLine = $0006;
    HighContrast = $0007;
    Black = $0008;
    White = $0009;
    DontShow = $000a;
  end;

  { TdxDrawingBlackWhiteMode }

  TdxDrawingBlackWhiteMode = class(TdxOfficeDrawingIntPropertyBase)
  strict private
    function GetMode: Byte;
    procedure SetMode(const AValue: Byte);
  protected
    function GetComplex: Boolean; override;
  public
    constructor Create; override;
    constructor Create(AMode: Byte); overload;

    property Mode: Byte read GetMode write SetMode;
  end;

implementation

uses
  Contnrs,
  dxTypeHelpers,
  dxRichEdit.Utils.Exceptions, dxEncoding;

type
  { TdxFixedPoint }

  TdxFixedPoint = record
  public const
    FractionalCoeff = 65536.0;
  private
    FValue: Double;
    procedure Read(AReader: TBinaryReader);
  public
    class function FromStream(AReader: TBinaryReader): TdxFixedPoint; static;
    class function FromBytes(const AData: TBytes; AOffset: Integer): TdxFixedPoint; static;
    procedure Write(AWriter: TBinaryWriter);
    function GetBytes: TBytes;

    property Value: Double read FValue write FValue;
  end;

function BytesOf(const Val: Pointer; const Len: integer): TBytes;
begin
  SetLength(Result, Len);
  Move(Val^, Result[0], Len);
end;

{ TdxFixedPoint }

class function TdxFixedPoint.FromStream(AReader: TBinaryReader): TdxFixedPoint;
begin
  Result.Value := 0;
  Result.Read(AReader);
end;

class function TdxFixedPoint.FromBytes(const AData: TBytes; AOffset: Integer): TdxFixedPoint;
var
  AFractional: Word;
  AIntegral: SmallInt;
begin
  AFractional := PWord(@AData[AOffset])^;
  AIntegral := PSmallInt(@AData[AOffset + 2])^;
  Result.Value := AIntegral + (AFractional / FractionalCoeff);
end;

procedure TdxFixedPoint.Read(AReader: TBinaryReader);
var
  AFractional: Word;
  AIntegral: SmallInt;
begin
  AFractional := AReader.ReadUInt16;
  AIntegral := AReader.ReadSmallInt;
  FValue := AIntegral + (AFractional / FractionalCoeff);
end;

procedure TdxFixedPoint.Write(AWriter: TBinaryWriter);
var
  AIntegral: SmallInt;
  AFractional: Word;
begin
  AIntegral := Trunc(Value);
  if ((Value - AIntegral) <> 0.0) and (Value < 0) then
    Dec(AIntegral);
  AFractional := Trunc((Value - AIntegral) * FractionalCoeff);
  AWriter.Write(AFractional);
  AWriter.Write(AIntegral);
end;

function TdxFixedPoint.GetBytes: TBytes;
var
  AIntegral: SmallInt;
  AFractional: Word;
  AData: Cardinal;
begin
  AIntegral := Trunc(Value);
  if ((Value - AIntegral) <> 0.0) and (AIntegral < 0) then
    Dec(AIntegral);
  AFractional := Trunc((Value - AIntegral) * FractionalCoeff);
  AData := (Cardinal(AIntegral) shl 16) or AFractional;
  Result := BytesOf(@AData, SizeOf(AData));
end;

{ TdxOfficeArtExceptions }

class procedure TdxOfficeArtExceptions.ThrowInvalidContent;
begin
  raise EArgumentException.Create('Invalid OfficeArt content!');
end;

{ TdxOfficeDrawingPartBase }

function TdxOfficeDrawingPartBase.GetLength: Integer;
begin
  Result := GetSize;
end;

procedure TdxOfficeDrawingPartBase.Write(AWriter: TBinaryWriter);
begin
  if not ShouldWrite then
    Exit;
  WriteHeader(AWriter);
  WriteCore(AWriter);
end;

procedure TdxOfficeDrawingPartBase.WriteHeader(AWriter: TBinaryWriter);
var
  AHeader: TdxOfficeArtRecordHeader;
begin
  AHeader := TdxOfficeArtRecordHeader.Create;
  try
    AHeader.InstanceInfo := HeaderInstanceInfo;
    AHeader.Length := Length;
    AHeader.TypeCode := HeaderTypeCode;
    AHeader.Version := HeaderVersion;
    AHeader.Write(AWriter);
  finally
    AHeader.Free;
  end;
end;

procedure TdxOfficeDrawingPartBase.WriteCore(AWriter: TBinaryWriter);
begin
end;

function TdxOfficeDrawingPartBase.ShouldWrite: Boolean;
begin
  Result := True;
end;

{ TdxCompositeOfficeDrawingPartBase }

constructor TdxCompositeOfficeDrawingPartBase.Create;
begin
  FItems := TdxObjectList<TdxOfficeDrawingPartBase>.Create;
end;

destructor TdxCompositeOfficeDrawingPartBase.Destroy;
begin
  FItems.Free;
  FObjectsToDelete.Free;
  inherited Destroy;
end;

procedure TdxCompositeOfficeDrawingPartBase.CreateGarbageCollector;
begin
  FObjectsToDelete := TdxFastObjectList.Create;
  FObjectsToDelete.Capacity := 256;
end;

procedure TdxCompositeOfficeDrawingPartBase.WriteCore(AWriter: TBinaryWriter);
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
    Items[I].Write(AWriter);
end;

function TdxCompositeOfficeDrawingPartBase.GetSize: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Items.Count - 1 do
  begin
    if Items[I].ShouldWrite then
    begin
      Inc(Result, TdxOfficeArtRecordHeader.Size);
      Inc(Result, Items[I].GetSize);
    end;
  end;
end;

{ TdxOfficeArtFileDrawingGroupRecord }

class function TdxOfficeArtFileDrawingGroupRecord.FromStream(AReader: TBinaryReader; AHeader: TdxOfficeArtRecordHeader): TdxOfficeArtFileDrawingGroupRecord;
begin
  Result := TdxOfficeArtFileDrawingGroupRecord.Create;
  Result.Read(AReader, AHeader);
end;

function TdxOfficeArtFileDrawingGroupRecord.GetHeaderInstanceInfo: Integer;
begin
  Result := HeaderInstanceInfo;
end;

function TdxOfficeArtFileDrawingGroupRecord.GetHeaderTypeCode: Integer;
begin
  Result := HeaderTypeCode;
end;

function TdxOfficeArtFileDrawingGroupRecord.GetHeaderVersion: Integer;
begin
  Result := HeaderVersion;
end;

function TdxOfficeArtFileDrawingGroupRecord.GetHasPicturesInHeader: Boolean;
begin
  Result := HeaderFloatingObjectsCount > 0;
end;

procedure TdxOfficeArtFileDrawingGroupRecord.Read(AReader: TBinaryReader; AHeader: TdxOfficeArtRecordHeader);
var
  AMaxShapeIdentifier, AIdClustersCount: Integer;
begin
  AMaxShapeIdentifier := AReader.ReadInt32;
  if AMaxShapeIdentifier >= CurrentMaximumShapeID then
    TdxOfficeArtExceptions.ThrowInvalidContent;

  AIdClustersCount := AReader.ReadInt32 - 1;

  if AHeader.Length <> (AIdClustersCount * IdClusterSize) + BasePartSize then
    TdxOfficeArtExceptions.ThrowInvalidContent;
  AReader.ReadInt32;
  AReader.ReadInt32;
  AReader.BaseStream.Seek(AIdClustersCount * IdClusterSize, TSeekOrigin.soCurrent);
end;

procedure TdxOfficeArtFileDrawingGroupRecord.WriteCore(AWriter: TBinaryWriter);
begin
  AWriter.Write(CalcMaxShapeIdentifier);
  AWriter.Write(Integer(CalcClustersCount + 1));
  AWriter.Write(CalcShapesCount);
  AWriter.Write(CalcDrawingsCount);
  WriteClustersInfo(AWriter);
end;

procedure TdxOfficeArtFileDrawingGroupRecord.WriteClustersInfo(AWriter: TBinaryWriter);
begin
  AWriter.Write(MainDocumentClusterId);
  AWriter.Write(Integer(MainDocumentFloatingObjectsCount + 2));
  if HasPicturesInHeader then
  begin
    AWriter.Write(HeaderClusterId);
    AWriter.Write(Integer(HeaderFloatingObjectsCount + 1));
  end;
end;

function TdxOfficeArtFileDrawingGroupRecord.CalcMaxShapeIdentifier: Integer;
begin
  if HasPicturesInHeader then
    Result := TdxOfficeArtConstants.DefaultHeaderShapeIdentifier + HeaderFloatingObjectsCount + 1
  else
    Result := TdxOfficeArtConstants.DefaultMainDocumentShapeIdentifier + MainDocumentFloatingObjectsCount + 1;
end;

function TdxOfficeArtFileDrawingGroupRecord.CalcClustersCount: Integer;
begin
  if HasPicturesInHeader then
    Result := 2
  else
    Result := 1;
end;

function TdxOfficeArtFileDrawingGroupRecord.CalcShapesCount: Integer;
begin
  Result := MainDocumentFloatingObjectsCount + 1;
  if HeaderFloatingObjectsCount > 0 then
    Inc(Result, HeaderFloatingObjectsCount + 1);
end;

function TdxOfficeArtFileDrawingGroupRecord.CalcDrawingsCount: Integer;
begin
  if HasPicturesInHeader then
    Result := 2
  else
    Result := 1;
end;

function TdxOfficeArtFileDrawingGroupRecord.GetSize: Integer;
begin
  Result := (CalcClustersCount * IdClusterSize) + BasePartSize;
end;

{ TdxOfficeArtBlipStoreContainer }

constructor TdxOfficeArtBlipStoreContainer.Create;
begin
  FBlips := TdxObjectList<TdxBlipBase>.Create;
end;

destructor TdxOfficeArtBlipStoreContainer.Destroy;
begin
  FBlips.Free;
  inherited Destroy;
end;

class function TdxOfficeArtBlipStoreContainer.FromStream(AReader: TBinaryReader;
  AEmbeddedReader: TBinaryReader; AHeader: TdxOfficeArtRecordHeader;
  const AImageCreator: IdxDocOfficeImageCreator): TdxOfficeArtBlipStoreContainer;
begin
  Result := TdxOfficeArtBlipStoreContainer.Create;
  Result.Read(AReader, AEmbeddedReader, AHeader, AImageCreator);
end;

function TdxOfficeArtBlipStoreContainer.GetHeaderInstanceInfo: Integer;
begin
  Result := Blips.Count;
end;

function TdxOfficeArtBlipStoreContainer.GetHeaderTypeCode: Integer;
begin
  Result := TdxOfficeArtTypeCodes.BlipStoreContainer;
end;

function TdxOfficeArtBlipStoreContainer.GetHeaderVersion: Integer;
begin
  Result := TdxOfficeArtVersions.DefaultHeaderVersion;
end;

procedure TdxOfficeArtBlipStoreContainer.Read(AReader: TBinaryReader;
  AEmbeddedReader: TBinaryReader; AHeader: TdxOfficeArtRecordHeader;
  const AImageCreator: IdxDocOfficeImageCreator);
var
  AEnd: Int64;
begin
  AEnd := AReader.BaseStream.Position + AHeader.Length;
  FBlips.Free;
  FBlips := TdxBlipFactory.ReadAllBlips(AReader, AEmbeddedReader, AEnd, AImageCreator);
end;

procedure TdxOfficeArtBlipStoreContainer.Write(AWriter: TBinaryWriter; AEmbeddedWriter: TBinaryWriter);
begin
  if not ShouldWrite then
    Exit;
  WriteHeader(AWriter);
  WriteCore(AWriter, AEmbeddedWriter);
end;

procedure TdxOfficeArtBlipStoreContainer.WriteCore(AWriter: TBinaryWriter; AEmbeddedWriter: TBinaryWriter);
var
  I: Integer;
  AFileBlipStore: TdxFileBlipStoreEntry;
begin
  for I := 0 to Blips.Count - 1 do
  begin
    AFileBlipStore := Safe<TdxFileBlipStoreEntry>.Cast(Blips[I]);
    if AFileBlipStore <> nil then
      AFileBlipStore.Write(AWriter, AEmbeddedWriter)
    else
      Blips[I].Write(AWriter);
  end;
end;

function TdxOfficeArtBlipStoreContainer.ShouldWrite: Boolean;
begin
  Result := Blips.Count > 0;
end;

function TdxOfficeArtBlipStoreContainer.GetSize: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Blips.Count - 1 do
    Inc(Result, Blips[I].GetSize);
end;

{ TdxOfficeColorRecord }

constructor TdxOfficeColorRecord.Create(AColorUse: TdxOfficeColorUse; ATransform: TdxOfficeColorTransform; ATransformValue: Byte);
begin
  if AColorUse = TdxOfficeColorUse.None then
    raise EArgumentException.Create('colorUse');
  if ATransform = TdxOfficeColorTransform.None then
    raise EArgumentException.Create('transform');
  FData := Cardinal(MaskSystemColor or (Cardinal(ATransformValue) shl 16) or (Cardinal(ATransform) shl 8) or Cardinal(AColorUse));
end;

constructor TdxOfficeColorRecord.Create(AColorIndex: Integer);
begin
  ColorSchemeIndex := Byte(AColorIndex);
end;

constructor TdxOfficeColorRecord.Create(AColor: TdxAlphaColor);
begin
  Color := AColor;
end;

constructor TdxOfficeColorRecord.Create;
begin
  FData := $ffffffff;
end;

class function TdxOfficeColorRecord.FromStream(AReader: TBinaryReader): TdxOfficeColorRecord;
begin
  Result := TdxOfficeColorRecord.Create;
  Result.Read(AReader);
end;

class function TdxOfficeColorRecord.FromBytes(const AData: TBytes; AOffset: Integer): TdxOfficeColorRecord;
begin
  Result := TdxOfficeColorRecord.Create;
  Result.ReadBytes(AData, AOffset);
end;

function TdxOfficeColorRecord.GetIsDefault: Boolean;
begin
  Result := (FData and MaskDefault) = MaskDefault;
end;

function TdxOfficeColorRecord.GetSystemColorUsed: Boolean;
begin
  Result := not IsDefault and ((FData and MaskSystemColor) = MaskSystemColor);
end;

function TdxOfficeColorRecord.GetColorSchemeUsed: Boolean;
begin
  Result := not IsDefault and ((FData and MaskColorScheme) = MaskColorScheme);
end;

function TdxOfficeColorRecord.GetColor: TdxAlphaColor;
begin
  Result := TdxAlphaColors.FromArgb(Integer((FData and $0000ff)), Integer((FData and $00ff00)) shr 8, Integer((FData and $00ff0000)) shr 16);
end;

procedure TdxOfficeColorRecord.SetColor(const AValue: TdxAlphaColor);
begin
  FData := Cardinal(Integer(TdxAlphaColors.R(AValue)) or (Integer(TdxAlphaColors.G(AValue)) shl 8) or (Integer(TdxAlphaColors.B(AValue) shl 16)));
end;

function TdxOfficeColorRecord.GetColorSchemeIndex: Byte;
begin
  Result := Byte(FData and MaskSchemeIndex);
end;

procedure TdxOfficeColorRecord.SetColorSchemeIndex(const AValue: Byte);
begin
  FData := MaskColorScheme or AValue;
end;

function TdxOfficeColorRecord.GetSystemColorIndex: Integer;
begin
  Result := Integer(FData and MaskSystemIndex);
end;

procedure TdxOfficeColorRecord.SetSystemColorIndex(const AValue: Integer);
begin
  FData := Cardinal(MaskSystemColor or (AValue and MaskSystemIndex));
end;

function TdxOfficeColorRecord.GetColorUse: TdxOfficeColorUse;
var
  AValue: Cardinal;
begin
  if not SystemColorUsed then
    Exit(TdxOfficeColorUse.None);
  AValue := FData and MaskColorUse;
  if AValue < $00f0 then
    Exit(TdxOfficeColorUse.None);
  Result := TdxOfficeColorUse(AValue);
end;

function TdxOfficeColorRecord.GetTransform: TdxOfficeColorTransform;
begin
  if not SystemColorUsed then
    Exit(TdxOfficeColorTransform.None);
  Result := TdxOfficeColorTransform(FData and MaskTransform);
end;

function TdxOfficeColorRecord.GetTransformValue: Byte;
begin
  if not SystemColorUsed then
    Exit(0);
  Result := Byte((FData and MaskTransformValue) shr 16);
end;

procedure TdxOfficeColorRecord.Read(AReader: TBinaryReader);
begin
  FData := AReader.ReadUInt32;
end;

procedure TdxOfficeColorRecord.ReadBytes(const AData: TBytes; AOffset: Integer);
begin
  FData := PCardinal(@AData[AOffset])^;
end;

procedure TdxOfficeColorRecord.Write(AWriter: TBinaryWriter);
begin
  AWriter.Write(FData);
end;

function TdxOfficeColorRecord.GetBytes: TBytes;
begin
  Result := BytesOf(@FData, SizeOf(FData));
end;

{ TdxOfficeArtFileDrawingRecord }

constructor TdxOfficeArtFileDrawingRecord.Create(ADrawingId: Integer);
begin
  FDrawingId := ADrawingId;
end;

class function TdxOfficeArtFileDrawingRecord.FromStream(AReader: TBinaryReader; AHeader: TdxOfficeArtRecordHeader): TdxOfficeArtFileDrawingRecord;
begin
  Result := TdxOfficeArtFileDrawingRecord.Create(AHeader.InstanceInfo);
  Result.Read(AReader, AHeader);
end;

function TdxOfficeArtFileDrawingRecord.GetHeaderInstanceInfo: Integer;
begin
  Result := FDrawingId;
end;

function TdxOfficeArtFileDrawingRecord.GetHeaderTypeCode: Integer;
begin
  Result := TdxOfficeArtTypeCodes.FileDrawingRecord;
end;

function TdxOfficeArtFileDrawingRecord.GetHeaderVersion: Integer;
begin
  Result := TdxOfficeArtVersions.EmptyHeaderVersion;
end;

function TdxOfficeArtFileDrawingRecord.GetLength: Integer;
begin
  Result := RecordLength;
end;

procedure TdxOfficeArtFileDrawingRecord.Read(AReader: TBinaryReader; AHeader: TdxOfficeArtRecordHeader);
begin
  if AHeader.Length <> RecordLength then
    TdxOfficeArtExceptions.ThrowInvalidContent;
  FNumberOfShapes := AReader.ReadInt32;
  FLastShapeId := AReader.ReadInt32;
end;

procedure TdxOfficeArtFileDrawingRecord.WriteCore(AWriter: TBinaryWriter);
begin
  AWriter.Write(NumberOfShapes);
  AWriter.Write(LastShapeIdentifier);
end;

function TdxOfficeArtFileDrawingRecord.GetSize: Integer;
begin
  Result := RecordLength;
end;

{ TdxOfficeArtSplitMenuColorContainer }

constructor TdxOfficeArtSplitMenuColorContainer.Create;
begin
  FColor3D := TdxOfficeColorRecord.Create(TdxAlphaColors.Empty);
  FFillColor := TdxOfficeColorRecord.Create(TdxAlphaColors.Empty);
  FLineColor := TdxOfficeColorRecord.Create(TdxAlphaColors.Empty);
  FShapeColor := TdxOfficeColorRecord.Create(TdxAlphaColors.Empty);
end;

destructor TdxOfficeArtSplitMenuColorContainer.Destroy;
begin
  FFillColor.Free;
  FLineColor.Free;
  FShapeColor.Free;
  FColor3D.Free;
  inherited Destroy;
end;

class function TdxOfficeArtSplitMenuColorContainer.FromStream(AReader: TBinaryReader; AHeader: TdxOfficeArtRecordHeader): TdxOfficeArtSplitMenuColorContainer;
begin
  Result := TdxOfficeArtSplitMenuColorContainer.Create;
  Result.Read(AReader, AHeader);
end;

function TdxOfficeArtSplitMenuColorContainer.GetHeaderInstanceInfo: Integer;
begin
  Result := TdxOfficeArtHeaderInfos.SplitMenuColorContainerInfo;
end;

function TdxOfficeArtSplitMenuColorContainer.GetHeaderTypeCode: Integer;
begin
  Result := TdxOfficeArtTypeCodes.SplitMenuColorContainer;
end;

function TdxOfficeArtSplitMenuColorContainer.GetHeaderVersion: Integer;
begin
  Result := TdxOfficeArtVersions.EmptyHeaderVersion;
end;

function TdxOfficeArtSplitMenuColorContainer.GetLength: Integer;
begin
  Result := RecordLength;
end;

procedure TdxOfficeArtSplitMenuColorContainer.Read(AReader: TBinaryReader; AHeader: TdxOfficeArtRecordHeader);
begin
  if AHeader.Length <> RecordLength then
    TdxOfficeArtExceptions.ThrowInvalidContent;

  FFillColor.Free;
  FFillColor := TdxOfficeColorRecord.FromStream(AReader);

  FLineColor.Free;
  FLineColor := TdxOfficeColorRecord.FromStream(AReader);

  FShapeColor.Free;
  FShapeColor := TdxOfficeColorRecord.FromStream(AReader);

  FColor3D.Free;
  FColor3D := TdxOfficeColorRecord.FromStream(AReader);
end;

procedure TdxOfficeArtSplitMenuColorContainer.WriteCore(AWriter: TBinaryWriter);
begin
  FillColor.Write(AWriter);
  LineColor.Write(AWriter);
  ShapeColor.Write(AWriter);
  Color3D.Write(AWriter);
end;

function TdxOfficeArtSplitMenuColorContainer.GetSize: Integer;
begin
  Result := RecordLength;
end;

{ TdxOfficeArtDrawingContainer }

constructor TdxOfficeArtDrawingContainer.Create;
begin
  inherited Create;
  FFileDrawingBlock := TdxOfficeArtFileDrawingGroupRecord.Create;
  FBlipContainer := TdxOfficeArtBlipStoreContainer.Create;
  FSplitMenuColorContainer := TdxOfficeArtSplitMenuColorContainer.Create;
  Items.Add(FFileDrawingBlock);
  Items.Add(FBlipContainer);
  Items.Add(FSplitMenuColorContainer);
end;

class function TdxOfficeArtDrawingContainer.FromStream(AReader: TBinaryReader;
  AEmbeddedReader: TBinaryReader; const AImageCreator: IdxDocOfficeImageCreator): TdxOfficeArtDrawingContainer;
begin
  Result := TdxOfficeArtDrawingContainer.Create;
  Result.CreateGarbageCollector;
  Result.Read(AReader, AEmbeddedReader, AImageCreator);
end;

function TdxOfficeArtDrawingContainer.GetHeaderInstanceInfo: Integer;
begin
  Result := TdxOfficeArtHeaderInfos.EmptyHeaderInfo;
end;

function TdxOfficeArtDrawingContainer.GetHeaderTypeCode: Integer;
begin
  Result := TdxOfficeArtTypeCodes.DrawingContainer;
end;

function TdxOfficeArtDrawingContainer.GetHeaderVersion: Integer;
begin
  Result := TdxOfficeArtVersions.DefaultHeaderVersion;
end;

procedure TdxOfficeArtDrawingContainer.Read(AReader: TBinaryReader;
  AEmbeddedReader: TBinaryReader; const AImageCreator: IdxDocOfficeImageCreator);
var
  AHeader: TdxOfficeArtRecordHeader;
  AEndPosition: Int64;
begin
  AHeader := TdxOfficeArtRecordHeader.FromStream(AReader);
  try
    CheckHeader(AHeader);
    AEndPosition := AReader.BaseStream.Position + AHeader.Length;
    while AReader.BaseStream.Position < AEndPosition do
      ParseHeader(AReader, AEmbeddedReader, AImageCreator);
  finally
    AHeader.Free;
  end;
end;

procedure TdxOfficeArtDrawingContainer.CheckHeader(AHeader: TdxOfficeArtRecordHeader);
begin
  if (AHeader.Version <> TdxOfficeArtVersions.DefaultHeaderVersion) or
     (AHeader.InstanceInfo <> TdxOfficeArtHeaderInfos.EmptyHeaderInfo) or
     (AHeader.TypeCode <> TdxOfficeArtTypeCodes.DrawingContainer) then
    TdxOfficeArtExceptions.ThrowInvalidContent;
end;

procedure TdxOfficeArtDrawingContainer.Write(AWriter: TBinaryWriter; AEmbeddedWriter: TBinaryWriter);
begin
  WriteHeader(AWriter);
  FileDrawingBlock.Write(AWriter);
  BlipContainer.Write(AWriter, AEmbeddedWriter);
  SplitMenuColorContainer.Write(AWriter);
end;

procedure TdxOfficeArtDrawingContainer.ParseHeader(AReader: TBinaryReader; AEmbeddedReader: TBinaryReader;
  const AImageCreator: IdxDocOfficeImageCreator);
var
  AHeader: TdxOfficeArtRecordHeader;
begin
  AHeader := TdxOfficeArtRecordHeader.FromStream(AReader);
  try
    case AHeader.TypeCode of
      TdxOfficeArtTypeCodes.FileDrawingGroupRecord:
        begin
          FFileDrawingBlock := TdxOfficeArtFileDrawingGroupRecord.FromStream(AReader, AHeader);
          ObjectsToDelete.Add(FFileDrawingBlock);
        end;
      TdxOfficeArtTypeCodes.BlipStoreContainer:
        begin
          FBlipContainer := TdxOfficeArtBlipStoreContainer.FromStream(AReader, AEmbeddedReader, AHeader, AImageCreator);
          ObjectsToDelete.Add(FBlipContainer);
        end;
      TdxOfficeArtTypeCodes.SplitMenuColorContainer:
        begin
          FSplitMenuColorContainer := TdxOfficeArtSplitMenuColorContainer.FromStream(AReader, AHeader);
          ObjectsToDelete.Add(FSplitMenuColorContainer);
        end
      else
        AReader.BaseStream.Seek(AHeader.Length, TSeekOrigin.soCurrent);
    end;
  finally
    AHeader.Free;
  end;
end;

{ TdxOfficeArtShapeRecord }

constructor TdxOfficeArtShapeRecord.Create(AShapeTypeCode: Integer = 0);
begin
  FShapeTypeCode := AShapeTypeCode;
  FFlags := DefaultFlags;
end;

class function TdxOfficeArtShapeRecord.FromStream(AReader: TBinaryReader): TdxOfficeArtShapeRecord;
begin
  Result := TdxOfficeArtShapeRecord.Create;
  Result.Read(AReader);
end;

class function TdxOfficeArtShapeRecord.FromStream(AReader: TBinaryReader; AHeader: TdxOfficeArtRecordHeader): TdxOfficeArtShapeRecord;
begin
  Result := TdxOfficeArtShapeRecord.Create(AHeader.InstanceInfo);
  Result.Read(AReader);
end;

function TdxOfficeArtShapeRecord.GetHeaderInstanceInfo: Integer;
begin
  Result := FShapeTypeCode;
end;

function TdxOfficeArtShapeRecord.GetHeaderTypeCode: Integer;
begin
  Result := TdxOfficeArtTypeCodes.FileShape;
end;

function TdxOfficeArtShapeRecord.GetHeaderVersion: Integer;
begin
  Result := TdxOfficeArtVersions.ShapeRecordVersion;
end;

function TdxOfficeArtShapeRecord.GetLength: Integer;
begin
  Result := RecordLength;
end;

function TdxOfficeArtShapeRecord.GetIsGroup: Boolean;
begin
  Result := GetBoolValue(MaskGroup);
end;

procedure TdxOfficeArtShapeRecord.SetIsGroup(const AValue: Boolean);
begin
  SetBoolValue(AValue, MaskGroup);
end;

function TdxOfficeArtShapeRecord.GetIsChild: Boolean;
begin
  Result := GetBoolValue(MaskChild);
end;

procedure TdxOfficeArtShapeRecord.SetIsChild(const AValue: Boolean);
begin
  SetBoolValue(AValue, MaskChild);
end;

function TdxOfficeArtShapeRecord.GetIsPatriarch: Boolean;
begin
  Result := GetBoolValue(MaskPatriarch);
end;

procedure TdxOfficeArtShapeRecord.SetIsPatriarch(const AValue: Boolean);
begin
  SetBoolValue(AValue, MaskPatriarch);
end;

function TdxOfficeArtShapeRecord.GetIsDeleted: Boolean;
begin
  Result := GetBoolValue(MaskDeleted);
end;

procedure TdxOfficeArtShapeRecord.SetIsDeleted(const AValue: Boolean);
begin
  SetBoolValue(AValue, MaskDeleted);
end;

function TdxOfficeArtShapeRecord.GetIsOleShape: Boolean;
begin
  Result := GetBoolValue(MaskOleShape);
end;

procedure TdxOfficeArtShapeRecord.SetIsOleShape(const AValue: Boolean);
begin
  SetBoolValue(AValue, MaskOleShape);
end;

function TdxOfficeArtShapeRecord.GetHaveMaster: Boolean;
begin
  Result := GetBoolValue(MaskHaveMaster);
end;

procedure TdxOfficeArtShapeRecord.SetHaveMaster(const AValue: Boolean);
begin
  SetBoolValue(AValue, MaskHaveMaster);
end;

function TdxOfficeArtShapeRecord.GetFlipH: Boolean;
begin
  Result := GetBoolValue(MaskFlipH);
end;

procedure TdxOfficeArtShapeRecord.SetFlipH(const AValue: Boolean);
begin
  SetBoolValue(AValue, MaskFlipH);
end;

function TdxOfficeArtShapeRecord.GetFlipV: Boolean;
begin
  Result := GetBoolValue(MaskFlipV);
end;

procedure TdxOfficeArtShapeRecord.SetFlipV(const AValue: Boolean);
begin
  SetBoolValue(AValue, MaskFlipV);
end;

function TdxOfficeArtShapeRecord.GetIsConnector: Boolean;
begin
  Result := GetBoolValue(MaskConnector);
end;

procedure TdxOfficeArtShapeRecord.SetIsConnector(const AValue: Boolean);
begin
  SetBoolValue(AValue, MaskConnector);
end;

function TdxOfficeArtShapeRecord.GetHaveAnchor: Boolean;
begin
  Result := GetBoolValue(MaskHaveAnchor);
end;

procedure TdxOfficeArtShapeRecord.SetHaveAnchor(const AValue: Boolean);
begin
  SetBoolValue(AValue, MaskHaveAnchor);
end;

function TdxOfficeArtShapeRecord.GetIsBackground: Boolean;
begin
  Result := GetBoolValue(MaskBackground);
end;

procedure TdxOfficeArtShapeRecord.SetIsBackground(const AValue: Boolean);
begin
  SetBoolValue(AValue, MaskBackground);
end;

function TdxOfficeArtShapeRecord.GetHaveShapeType: Boolean;
begin
  Result := GetBoolValue(MaskHaveShapeType);
end;

procedure TdxOfficeArtShapeRecord.SetHaveShapeType(const AValue: Boolean);
begin
  SetBoolValue(AValue, MaskHaveShapeType);
end;

procedure TdxOfficeArtShapeRecord.Read(AReader: TBinaryReader);
begin
  FShapeIdentifier := AReader.ReadInt32;
  FFlags := AReader.ReadInt32;
end;

procedure TdxOfficeArtShapeRecord.WriteCore(AWriter: TBinaryWriter);
begin
  AWriter.Write(ShapeIdentifier);
  AWriter.Write(Flags);
end;

function TdxOfficeArtShapeRecord.GetSize: Integer;
begin
  Result := RecordLength;
end;

function TdxOfficeArtShapeRecord.GetBoolValue(AMask: Integer): Boolean;
begin
  Result := (FFlags and AMask) <> 0;
end;

procedure TdxOfficeArtShapeRecord.SetBoolValue(AValue: Boolean; AMask: Integer);
begin
  if AValue then
    FFlags := FFlags or AMask
  else
    FFlags := FFlags and not AMask;
end;

{ TdxOfficeArtPropertiesBase }

constructor TdxOfficeArtPropertiesBase.Create;
begin
  FProperties := TList<IdxOfficeDrawingProperty>.Create;
end;

destructor TdxOfficeArtPropertiesBase.Destroy;
begin
  FProperties.Free;
  inherited Destroy;
end;

function TdxOfficeArtPropertiesBase.GetHeaderInstanceInfo: Integer;
begin
  Result := Properties.Count;
end;

function TdxOfficeArtPropertiesBase.GetHeaderVersion: Integer;
begin
  Result := TdxOfficeArtVersions.PropertiesVersion;
end;

procedure TdxOfficeArtPropertiesBase.Read(AReader: TBinaryReader; AHeader: TdxOfficeArtRecordHeader);
var
  AStart: Int64;
  ACount, I: Integer;
  AProperty: IdxOfficeDrawingProperty;
  AIntProperty: TdxOfficeDrawingIntPropertyBase;
  AMsoArrayProperty: TdxOfficeDrawingMsoArrayPropertyBase;
  AMsoArrayHeader, AMsoArrayData, AComplexData: TBytes;
begin
  AStart := AReader.BaseStream.Position;
  ACount := AHeader.InstanceInfo;
  for I := 0 to ACount - 1 do
    Properties.Add(TdxOfficePropertiesFactory.CreateProperty(AReader));

  for I := 0 to ACount - 1 do
  begin
    AProperty := Properties[I];
    if AProperty.Complex then
    begin
      AIntProperty := TdxOfficeDrawingIntPropertyBase(AProperty);
      if AIntProperty <> nil then
        AIntProperty.SetComplexData(AReader.ReadBytes(AIntProperty.Value))
      else
      begin
        AMsoArrayProperty := TdxOfficeDrawingMsoArrayPropertyBase(AProperty);
        if AMsoArrayProperty <> nil then
        begin
          if AMsoArrayProperty.Value > 6 then
          begin
            AMsoArrayHeader := AReader.ReadBytes(6);
            if PWord(@AMsoArrayHeader[4])^ = $FFF0 then
              AMsoArrayProperty.Value := AMsoArrayProperty.Value + 6;
            AMsoArrayData := AReader.ReadBytes(AMsoArrayProperty.Value - 6);
            SetLength(AComplexData, AMsoArrayProperty.Value);
            TArray.Copy<Byte>(AMsoArrayHeader, AComplexData, 6);
            TArray.Copy<Byte>(AMsoArrayData, AComplexData, 0, 6, System.Length(AMsoArrayData));
            AMsoArrayProperty.SetComplexData(AComplexData);
          end
          else
            AMsoArrayProperty.SetComplexData(AReader.ReadBytes(AMsoArrayProperty.Value));
        end;
      end;
    end;
    AProperty.Execute(Self);
  end;
  AReader.BaseStream.Seek(AStart + AHeader.Length, TSeekOrigin.soBeginning);
end;

procedure TdxOfficeArtPropertiesBase.WriteCore(AWriter: TBinaryWriter);
var
  AComplexData: TList<TBytes>;
  I: Integer;
  ACurrent: IdxOfficeDrawingProperty;
begin
  AComplexData := TList<TBytes>.Create;
  try
    for I := 0 to Properties.Count - 1 do
    begin
      ACurrent := Properties[I];
      if ACurrent.Complex then
        AComplexData.Add(ACurrent.ComplexData);
      ACurrent.Write(AWriter);
    end;

    for I := 0 to AComplexData.Count - 1 do
      AWriter.Write(AComplexData[I]);
  finally
    AComplexData.Free;
  end;
end;

function TdxOfficeArtPropertiesBase.GetSize: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FProperties.Count - 1 do
    Inc(Result, FProperties[I].Size);
end;

{ TdxOfficeArtProperties }

class function TdxOfficeArtProperties.FromStream(AReader: TBinaryReader; AHeader: TdxOfficeArtRecordHeader): TdxOfficeArtProperties;
begin
  Result := TdxOfficeArtProperties.Create;
  Result.Read(AReader, AHeader);
end;

function TdxOfficeArtProperties.GetBlipIndex: Integer;
begin
  Result := FBlipIndex;
end;

function TdxOfficeArtProperties.GetCropFromBottom: Double;
begin
  Result := FCropFromBottom;
end;

function TdxOfficeArtProperties.GetCropFromLeft: Double;
begin
  Result := FCropFromLeft;
end;

function TdxOfficeArtProperties.GetCropFromRight: Double;
begin
  Result := FCropFromRight;
end;

function TdxOfficeArtProperties.GetCropFromTop: Double;
begin
  Result := FCropFromTop;
end;

function TdxOfficeArtProperties.GetFillColor: TdxAlphaColor;
begin
  Result := FFillColor;
end;

function TdxOfficeArtProperties.GetFilled: Boolean;
begin
  Result := FFilled;
end;

function TdxOfficeArtProperties.GetFitShapeToText: Boolean;
begin
  Result := FFitShapeToText;
end;

function TdxOfficeArtProperties.GetHeaderTypeCode: Integer;
begin
  Result := TdxOfficeArtTypeCodes.PropertiesTable;
end;

function TdxOfficeArtProperties.GetIsBehindDoc: Boolean;
begin
  Result := FIsBehindDoc;
end;

function TdxOfficeArtProperties.GetLayoutInCell: Boolean;
begin
  Result := FLayoutInCell;
end;

function TdxOfficeArtProperties.GetLine: Boolean;
begin
  Result := FLine;
end;

function TdxOfficeArtProperties.GetLineColor: TdxAlphaColor;
begin
  Result := FLineColor;
end;

function TdxOfficeArtProperties.GetLineWidth: Double;
begin
  Result := FLineWidth;
end;

function TdxOfficeArtProperties.GetRotation: Double;
begin
  Result := FRotation;
end;

function TdxOfficeArtProperties.GetTextBottom: Integer;
begin
  Result := FTextBottom;
end;

function TdxOfficeArtProperties.GetTextIndex: Integer;
begin
  Result := FTextIndex;
end;

function TdxOfficeArtProperties.GetTextLeft: Integer;
begin
  Result := FTextLeft;
end;

function TdxOfficeArtProperties.GetTextRight: Integer;
begin
  Result := FTextRight;
end;

function TdxOfficeArtProperties.GetTextTop: Integer;
begin
  Result := FTextTop;
end;

function TdxOfficeArtProperties.GetUseFilled: Boolean;
begin
  Result := FUseFilled;
end;

function TdxOfficeArtProperties.GetUseFitShapeToText: Boolean;
begin
  Result := FUseFitShapeToText;
end;

function TdxOfficeArtProperties.GetUseIsBehindDoc: Boolean;
begin
  Result := FUseIsBehindDoc;
end;

function TdxOfficeArtProperties.GetUseLayoutInCell: Boolean;
begin
  Result := FUseLayoutInCell;
end;

function TdxOfficeArtProperties.GetUseLine: Boolean;
begin
  Result := FUseLine;
end;

function TdxOfficeArtProperties.GetUseTextBottom: Boolean;
begin
  Result := FUseTextBottom;
end;

function TdxOfficeArtProperties.GetUseTextLeft: Boolean;
begin
  Result := FUseTextLeft;
end;

function TdxOfficeArtProperties.GetUseTextRight: Boolean;
begin
  Result := FUseTextRight;
end;

function TdxOfficeArtProperties.GetUseTextTop: Boolean;
begin
  Result := FUseTextTop;
end;

function TdxOfficeArtProperties.GetUseWrapBottomDistance: Boolean;
begin
  Result := FUseWrapBottomDistance;
end;

function TdxOfficeArtProperties.GetUseWrapLeftDistance: Boolean;
begin
  Result := FUseWrapLeftDistance;
end;

function TdxOfficeArtProperties.GetUseWrapRightDistance: Boolean;
begin
  Result := FUseWrapRightDistance;
end;

function TdxOfficeArtProperties.GetUseWrapTopDistance: Boolean;
begin
  Result := FUseWrapTopDistance;
end;

function TdxOfficeArtProperties.GetWrapBottomDistance: Integer;
begin
  Result := FWrapBottomDistance;
end;

function TdxOfficeArtProperties.GetWrapLeftDistance: Integer;
begin
  Result := FWrapLeftDistance;
end;

function TdxOfficeArtProperties.GetWrapRightDistance: Integer;
begin
  Result := FWrapRightDistance;
end;

function TdxOfficeArtProperties.GetWrapTopDistance: Integer;
begin
  Result := FWrapTopDistance;
end;

function TdxOfficeArtProperties.GetZOrder: Integer;
begin
  Result := FZOrder;
end;

procedure TdxOfficeArtProperties.CreateProperties;
begin
  SetRotationProperties;
  SetBooleanProtectionProperties;
  SetBlipIndexProperty;
  SetTextIndexProperty;
  SetTextBoxProperties;
  SetFillColorProperties;
  SetBlipBooleanProperties;
  SetDrawingTextBooleanProperties;
  SetFillStyleBooleanProperties;
  SetLineProrerties;
  SetLineStyleBooleanProperties;
  SetShapeBooleanProperties;
  SetShapeNameProperty;
  SetShapeDescriptionProperty;
  SetShapeGroupBooleanProperties;
end;

procedure TdxOfficeArtProperties.SetTextBottom(AValue: Integer);
begin
  FTextBottom := AValue;
end;

procedure TdxOfficeArtProperties.SetTextBoxProperties;
begin
  if TextBoxProperties = nil then
    Exit;
  if TextBoxProperties.UseLeftMargin then
    Properties.Add(TdxDrawingTextLeft.Create(UnitConverter.ModelUnitsToEmu(TextBoxProperties.LeftMargin)));
  if TextBoxProperties.UseTopMargin then
    Properties.Add(TdxDrawingTextTop.Create(UnitConverter.ModelUnitsToEmu(TextBoxProperties.TopMargin)));
  if TextBoxProperties.UseRightMargin then
    Properties.Add(TdxDrawingTextRight.Create(UnitConverter.ModelUnitsToEmu(TextBoxProperties.RightMargin)));
  if TextBoxProperties.UseBottomMargin then
    Properties.Add(TdxDrawingTextBottom.Create(UnitConverter.ModelUnitsToEmu(TextBoxProperties.BottomMargin)));
end;

procedure TdxOfficeArtProperties.SetRotation(AValue: Double);
begin
  FRotation := AValue;
end;

procedure TdxOfficeArtProperties.SetRotationProperties;
begin
  if (Shape <> nil) and Shape.UseRotation then
    Properties.Add(TdxDrawingRotation.Create(UnitConverter.ModelUnitsToDegree(Integer(Shape.Rotation))));
end;

procedure TdxOfficeArtProperties.SetFillColor(AValue: TdxAlphaColor);
begin
  FFillColor := AValue;
end;

procedure TdxOfficeArtProperties.SetFillColorProperties;
begin
  if (Shape <> nil) and Shape.UseFillColor and not TdxAlphaColors.IsTransparentOrEmpty(Shape.FillColor) then
    Properties.Add(TdxDrawingFillColor.Create(Shape.FillColor));
end;

procedure TdxOfficeArtProperties.SetFilled(AValue: Boolean);
begin
  FFilled := AValue;
end;

procedure TdxOfficeArtProperties.SetLayoutInCell(AValue: Boolean);
begin
  FLayoutInCell := AValue;
end;

procedure TdxOfficeArtProperties.SetLine(AValue: Boolean);
begin
  FLine := AValue;
end;

procedure TdxOfficeArtProperties.SetLineColor(AValue: TdxAlphaColor);
begin
  FLineColor := AValue;
end;

procedure TdxOfficeArtProperties.SetLineProrerties;
begin
  if Shape = nil then
    Exit;
  if Shape.UseOutlineColor then
    Properties.Add(TdxDrawingLineColor.Create(Shape.OutlineColor));
  if Shape.UseOutlineWidth then
    Properties.Add(TdxDrawingLineWidth.Create(UnitConverter.ModelUnitsToEmu(Integer(Shape.OutlineWidth))));
end;

procedure TdxOfficeArtProperties.SetBooleanProtectionProperties;
begin
  Properties.Add(TdxDrawingBooleanProtectionProperties.Create);
end;

procedure TdxOfficeArtProperties.SetBlipIndex(AValue: Integer);
begin
  FBlipIndex := AValue;
end;

procedure TdxOfficeArtProperties.SetCropFromBottom(AValue: Double);
begin
  FCropFromBottom := AValue;
end;

procedure TdxOfficeArtProperties.SetCropFromLeft(AValue: Double);
begin
  FCropFromLeft := AValue;
end;

procedure TdxOfficeArtProperties.SetCropFromRight(AValue: Double);
begin
  FCropFromRight := AValue;
end;

procedure TdxOfficeArtProperties.SetCropFromTop(AValue: Double);
begin
  FCropFromTop := AValue;
end;

procedure TdxOfficeArtProperties.SetBlipIndexProperty;
var
  ADrawingBlipIdentifier: TdxDrawingBlipIdentifier;
begin
  if BlipIndex = 0 then
    Exit;
  ADrawingBlipIdentifier := TdxDrawingBlipIdentifier.Create;
  ADrawingBlipIdentifier.Value := BlipIndex;
  Properties.Add(ADrawingBlipIdentifier);
end;

procedure TdxOfficeArtProperties.SetTextIndex(AValue: Integer);
begin
  FTextIndex := AValue;
end;

procedure TdxOfficeArtProperties.SetTextIndexProperty;
var
  ADrawingTextIdentifier: TdxDrawingTextIdentifier;
begin
  if TextIndex = 0 then
    Exit;
  ADrawingTextIdentifier := TdxDrawingTextIdentifier.Create;
  ADrawingTextIdentifier.Value := TextIndex;
  Properties.Add(ADrawingTextIdentifier);
end;

procedure TdxOfficeArtProperties.SetTextLeft(AValue: Integer);
begin
  FTextLeft := AValue;
end;

procedure TdxOfficeArtProperties.SetTextRight(AValue: Integer);
begin
  FTextRight := AValue;
end;

procedure TdxOfficeArtProperties.SetTextTop(AValue: Integer);
begin
  FTextTop := AValue;
end;

procedure TdxOfficeArtProperties.SetUseFilled(AValue: Boolean);
begin
  FUseFilled := AValue;
end;

procedure TdxOfficeArtProperties.SetUseFitShapeToText(AValue: Boolean);
begin
  FUseFitShapeToText := AValue;
end;

procedure TdxOfficeArtProperties.SetUseIsBehindDoc(AValue: Boolean);
begin
  FUseIsBehindDoc := AValue;
end;

procedure TdxOfficeArtProperties.SetUseLayoutInCell(AValue: Boolean);
begin
  FUseLayoutInCell := AValue;
end;

procedure TdxOfficeArtProperties.SetUseLine(AValue: Boolean);
begin
  FUseLine := AValue;
end;

procedure TdxOfficeArtProperties.SetUseTextBottom(AValue: Boolean);
begin
  FUseTextBottom := AValue;
end;

procedure TdxOfficeArtProperties.SetUseTextLeft(AValue: Boolean);
begin
  FUseTextLeft := AValue;
end;

procedure TdxOfficeArtProperties.SetUseTextRight(AValue: Boolean);
begin
  FUseTextRight := AValue;
end;

procedure TdxOfficeArtProperties.SetUseTextTop(AValue: Boolean);
begin
  FUseTextTop := AValue;
end;

procedure TdxOfficeArtProperties.SetUseWrapBottomDistance(AValue: Boolean);
begin
  FUseWrapBottomDistance := AValue;
end;

procedure TdxOfficeArtProperties.SetUseWrapLeftDistance(AValue: Boolean);
begin
  FUseWrapLeftDistance := AValue;
end;

procedure TdxOfficeArtProperties.SetUseWrapRightDistance(AValue: Boolean);
begin
  FUseWrapRightDistance := AValue;
end;

procedure TdxOfficeArtProperties.SetUseWrapTopDistance(AValue: Boolean);
begin
  FUseWrapTopDistance := AValue;
end;

procedure TdxOfficeArtProperties.SetWrapBottomDistance(AValue: Integer);
begin
  FWrapBottomDistance := AValue;
end;

procedure TdxOfficeArtProperties.SetWrapLeftDistance(AValue: Integer);
begin
  FWrapLeftDistance := AValue;
end;

procedure TdxOfficeArtProperties.SetWrapRightDistance(AValue: Integer);
begin
  FWrapRightDistance := AValue;
end;

procedure TdxOfficeArtProperties.SetWrapTopDistance(AValue: Integer);
begin
  FWrapTopDistance := AValue;
end;

procedure TdxOfficeArtProperties.SetZOrder(AValue: Integer);
begin
  FZOrder := AValue;
end;

procedure TdxOfficeArtProperties.SetBlipBooleanProperties;
begin
  if BlipIndex <> 0 then
    Properties.Add(TdxDrawingBlipBooleanProperties.Create);
end;

procedure TdxOfficeArtProperties.SetFillStyleBooleanProperties;
var
  AProperty: TdxDrawingFillStyleBooleanProperties;
begin
  AProperty := TdxDrawingFillStyleBooleanProperties.Create;
  if (Shape <> nil) and Shape.UseFillColor then
  begin
    AProperty.Filled := True;
    AProperty.UseFilled := True;
  end;
  Properties.Add(AProperty);
end;

procedure TdxOfficeArtProperties.SetFitShapeToText(AValue: Boolean);
begin
  FFitShapeToText := AValue;
end;

procedure TdxOfficeArtProperties.SetIsBehindDoc(AValue: Boolean);
begin
  FIsBehindDoc := AValue;
end;

procedure TdxOfficeArtProperties.SetLineStyleBooleanProperties;
var
  ALineStyleBooleanProperties: TdxDrawingLineStyleBooleanProperties;
begin
  ALineStyleBooleanProperties := TdxDrawingLineStyleBooleanProperties.Create;
  if (Shape <> nil) and Shape.UseOutlineColor and Shape.UseOutlineWidth then
  begin
    ALineStyleBooleanProperties.Line := True;
    ALineStyleBooleanProperties.UseLine := True;
  end;
  Properties.Add(ALineStyleBooleanProperties);
end;

procedure TdxOfficeArtProperties.SetLineWidth(AValue: Double);
begin
  FLineWidth := AValue;
end;

procedure TdxOfficeArtProperties.SetShapeBooleanProperties;
var
  AProperty: TdxDrawingShapeBooleanProperties;
begin
  AProperty := TdxDrawingShapeBooleanProperties.Create;
  AProperty.UseLockShapeType := True;
  AProperty.LockShapeType := False;
  Properties.Add(AProperty);
end;

procedure TdxOfficeArtProperties.SetShapeNameProperty;
var
  AProperty: TdxDrawingShapeName;
begin
  if BlipIndex = 0 then
    Exit;
  AProperty := TdxDrawingShapeName.Create;
  AProperty.Data := Format('Picture %d'#0, [BlipIndex]);
  Properties.Add(AProperty);
end;

procedure TdxOfficeArtProperties.SetShapeDescriptionProperty;
begin
end;

procedure TdxOfficeArtProperties.SetShapeGroupBooleanProperties;
var
  AProperty: TdxDrawingGroupShapeBooleanProperties;
begin
  AProperty := TdxDrawingGroupShapeBooleanProperties.Create;
  if FloatingObjectProperties <> nil then
  begin
    AProperty.IsBehindDoc := FloatingObjectProperties.IsBehindDoc;
    AProperty.LayoutInCell := FloatingObjectProperties.LayoutInTableCell;
    AProperty.UseLayoutInCell := FloatingObjectProperties.UseLayoutInTableCell;
  end;
  Properties.Add(AProperty);
end;

procedure TdxOfficeArtProperties.SetDrawingTextBooleanProperties;
var
  AProperty: TdxDrawingTextBooleanProperties;
begin
  AProperty := TdxDrawingTextBooleanProperties.Create;
  if (TextBoxProperties <> nil) and TextBoxProperties.UseResizeShapeToFitText then
  begin
    AProperty.FitShapeToText := TextBoxProperties.ResizeShapeToFitText;
    AProperty.UseFitShapeToText := True;
  end;
  Properties.Add(AProperty);
end;

{ TdxOfficeArtShapeContainer }

constructor TdxOfficeArtShapeContainer.Create;//(AShapeTypeCode: Integer = TdxOfficeArtHeaderInfos.PictureFrameShap);
begin
  inherited Create;
  FShapeRecord := TdxOfficeArtShapeRecord.Create(AShapeTypeCode);
  FArtProperties := TdxOfficeArtProperties.Create;
  FArtTertiaryProperties := TdxOfficeArtTertiaryProperties.Create;
  Items.Add(FShapeRecord);
  Items.Add(FArtProperties);
  Items.Add(FArtTertiaryProperties);
end;

class function TdxOfficeArtShapeContainer.FromStream(AReader: TBinaryReader; AHeader: TdxOfficeArtRecordHeader): TdxOfficeArtShapeContainer;
begin
  Result := TdxOfficeArtShapeContainer.Create;
  Result.CreateGarbageCollector;
  Result.Read(AReader, AHeader);
end;

function TdxOfficeArtShapeContainer.GetHeaderInstanceInfo: Integer;
begin
  Result := TdxOfficeArtHeaderInfos.EmptyHeaderInfo;
end;

function TdxOfficeArtShapeContainer.GetHeaderTypeCode: Integer;
begin
  Result := TdxOfficeArtTypeCodes.ShapeContainer;
end;

function TdxOfficeArtShapeContainer.GetHeaderVersion: Integer;
begin
  Result := TdxOfficeArtVersions.DefaultHeaderVersion;
end;

procedure TdxOfficeArtShapeContainer.Read(AReader: TBinaryReader; AHeader: TdxOfficeArtRecordHeader);
var
  AEndPosition: Int64;
begin
  AEndPosition := AReader.BaseStream.Position + AHeader.Length;
  if AEndPosition > AReader.BaseStream.Size then
    Exit;
  while AReader.BaseStream.Position < AEndPosition do
    ParseHeader(AReader);
end;

procedure TdxOfficeArtShapeContainer.ParseHeader(AReader: TBinaryReader);
var
  AHeader: TdxOfficeArtRecordHeader;
begin
  AHeader := TdxOfficeArtRecordHeader.FromStream(AReader);
  try
    case AHeader.TypeCode of
      TdxOfficeArtTypeCodes.FileShape:
        begin
          FShapeRecord := TdxOfficeArtShapeRecord.FromStream(AReader);
          ObjectsToDelete.Add(FShapeRecord);
        end;
      TdxOfficeArtTypeCodes.PropertiesTable:
        begin
          FArtProperties := TdxOfficeArtProperties.FromStream(AReader, AHeader);
          ObjectsToDelete.Add(FArtProperties);
        end;
      TdxOfficeArtTypeCodes.TertiaryPropertiesTable:
        begin
          FArtTertiaryProperties := TdxOfficeArtTertiaryProperties.FromStream(AReader, AHeader);
          ObjectsToDelete.Add(FArtTertiaryProperties);
        end
    else
      AReader.BaseStream.Seek(AHeader.Length, TSeekOrigin.soCurrent);
    end;
  finally
    AHeader.Free;
  end;
end;

{ TdxOfficeArtInlineShapeContainer }

constructor TdxOfficeArtInlineShapeContainer.Create;
begin
  FShapeContainer := TdxOfficeArtShapeContainer.Create;
  FBlips := TdxObjectList<TdxBlipBase>.Create;
end;

destructor TdxOfficeArtInlineShapeContainer.Destroy;
begin
  FShapeContainer.Free;
  FBlips.Free;
  inherited Destroy;
end;

class function TdxOfficeArtInlineShapeContainer.FromStream(AReader: TBinaryReader;
  ASize: Integer; const AImageCreator: IdxDocOfficeImageCreator): TdxOfficeArtInlineShapeContainer;
begin
  Result := TdxOfficeArtInlineShapeContainer.Create;
  Result.Read(AReader, ASize, AImageCreator);
end;

procedure TdxOfficeArtInlineShapeContainer.Read(AReader: TBinaryReader; ASize: Integer;
  const AImageCreator: IdxDocOfficeImageCreator);
var
  AEndPosition: Int64;
  AHeader: TdxOfficeArtRecordHeader;
begin
  AEndPosition := AReader.BaseStream.Position + ASize;
  AHeader := TdxOfficeArtRecordHeader.FromStream(AReader);
  try
    FShapeContainer.Free;
    FShapeContainer := TdxOfficeArtShapeContainer.FromStream(AReader, AHeader);
    FBlips.Free;
    FBlips := TdxBlipFactory.ReadAllBlips(AReader, AEndPosition, AImageCreator);
  finally
    AHeader.Free;
  end;
end;

procedure TdxOfficeArtInlineShapeContainer.Write(AWriter: TBinaryWriter);
var
  I: Integer;
begin
  FShapeContainer.Write(AWriter);
  for I := 0 to FBlips.Count - 1 do
    FBlips[I].Write(AWriter);
end;

{ TdxOfficeDrawingPropertyBase }

constructor TdxOfficeDrawingPropertyBase.Create;
begin
  inherited Create;
end;

function TdxOfficeDrawingPropertyBase.GetComplex: Boolean;
begin
  Result := FComplex;
end;

function TdxOfficeDrawingPropertyBase.GetComplexData: TBytes;
begin
  Result := FComplexData;
end;

function TdxOfficeDrawingPropertyBase.GetSize: Integer;
begin
  if Complex then
    Result := OperationCodeSize + OperandSize + Length(ComplexData)
  else
    Result := OperationCodeSize + OperandSize;
end;

procedure TdxOfficeDrawingPropertyBase.Merge(const AOther: IdxOfficeDrawingProperty);
begin
end;

procedure TdxOfficeDrawingPropertyBase.SetComplexData(const AData: TBytes);
begin
  FComplexData := AData;
end;

procedure TdxOfficeDrawingPropertyBase.SetComplex(AComplex: Boolean);
begin
  FComplex := AComplex;
end;

{ TdxOfficeDrawingIntPropertyBase }

procedure TdxOfficeDrawingIntPropertyBase.Read(AReader: TBinaryReader);
begin
  FValue := AReader.ReadInt32;
end;

procedure TdxOfficeDrawingIntPropertyBase.Execute(AOwner: TdxOfficeArtPropertiesBase);
begin
end;

procedure TdxOfficeDrawingIntPropertyBase.Write(AWriter: TBinaryWriter);
begin
  AWriter.Write(TdxOfficePropertiesFactory.GetOpcodeByType(ClassType));
  AWriter.Write(Value);
end;

function TdxOfficeDrawingIntPropertyBase.GetFlag(AMask: Integer): Boolean;
begin
  Result := (Value and AMask) = AMask;
end;

procedure TdxOfficeDrawingIntPropertyBase.SetFlag(AMask: Integer; AFlag: Boolean);
begin
  if AFlag then
    Value := Value or AMask
  else
    Value := Value and not AMask;
end;

{ TdxDrawingGroupShapePosH }

constructor TdxDrawingGroupShapePosH.Create(AMsoPosH: TMsoph);
begin
  MsoPosH := AMsoPosH;
end;

procedure TdxDrawingGroupShapePosH.Execute(AOwner: TdxOfficeArtPropertiesBase);
var
  AArtProperties: IdxOfficeArtTertiaryProperties;
begin
  inherited Execute(AOwner);
  if not Supports(AOwner, IdxOfficeArtTertiaryProperties, AArtProperties) then
    Exit;
  AArtProperties.PosH := MsoPosH;
  AArtProperties.UsePosH := True;
end;


function TdxDrawingGroupShapePosH.GetMsoPosH: TMsoph;
begin
  Result := TMsoph(Value);
end;

procedure TdxDrawingGroupShapePosH.SetMsoPosH(const AValue: TMsoph);
begin
  Value := Ord(AValue);
end;

{ TdxDrawingGroupShapePosRelH }

constructor TdxDrawingGroupShapePosRelH.Create(AMsoPosRelH: TMsoprh);
begin
  MsoPosRelH := AMsoPosRelH;
end;

constructor TdxDrawingGroupShapePosRelH.Create;
begin
  Create(TMsoprh.msoprhText);
end;

function TdxDrawingGroupShapePosRelH.GetMsoPosRelH: TMsoprh;
begin
  Result := TMsoprh(Value);
end;

procedure TdxDrawingGroupShapePosRelH.SetMsoPosRelH(const AValue: TMsoprh);
begin
  Value := Ord(AValue);
end;

procedure TdxDrawingGroupShapePosRelH.Execute(AOwner: TdxOfficeArtPropertiesBase);
var
  AArtProperties: IdxOfficeArtTertiaryProperties;
begin
  inherited Execute(AOwner);
  if not Supports(AOwner, IdxOfficeArtTertiaryProperties, AArtProperties) then
    Exit;
  AArtProperties.PosRelH := MsoPosRelH;
  AArtProperties.UsePosH := True;
end;

{ TdxDrawingGroupShapePosRelV }

constructor TdxDrawingGroupShapePosRelV.Create(AMsoPosRelV: TMsoprv);
begin
  MsoPosRelV := AMsoPosRelV;
end;

constructor TdxDrawingGroupShapePosRelV.Create;
begin
  Create(TMsoprv.msoprvText);
end;

function TdxDrawingGroupShapePosRelV.GetMsoPosRelV: TMsoprv;
begin
  Result := TMsoprv(Value);
end;

procedure TdxDrawingGroupShapePosRelV.SetMsoPosRelV(const AValue: TMsoprv);
begin
  Value := Ord(AValue);
end;

procedure TdxDrawingGroupShapePosRelV.Execute(AOwner: TdxOfficeArtPropertiesBase);
var
  AArtProperties: IdxOfficeArtTertiaryProperties;
begin
  inherited Execute(AOwner);
  if not Supports(AOwner, IdxOfficeArtTertiaryProperties, AArtProperties) then
    Exit;
  AArtProperties.PosRelV := MsoPosRelV;
  AArtProperties.UsePosV := True;
end;

{ TdxDrawingGroupShapePosV }

constructor TdxDrawingGroupShapePosV.Create(AMsoPosV: TMsopv);
begin
  MsoPosV := AMsoPosV;
end;

procedure TdxDrawingGroupShapePosV.Execute(AOwner: TdxOfficeArtPropertiesBase);
var
  AArtProperties: IdxOfficeArtTertiaryProperties;
begin
  inherited Execute(AOwner);
  if not Supports(AOwner, IdxOfficeArtTertiaryProperties, AArtProperties) then
    Exit;
  AArtProperties.PosV := MsoPosV;
  AArtProperties.UsePosV := True;
end;


function TdxDrawingGroupShapePosV.GetMsoPosV: TMsopv;
begin
  Result := TMsopv(Value);
end;

procedure TdxDrawingGroupShapePosV.SetMsoPosV(const AValue: TMsopv);
begin
  Value := Ord(AValue);
end;

{ TdxDrawingGroupShape2SizeRelH }

constructor TdxDrawingGroupShape2SizeRelH.Create(AFrom: TRelativeFrom);
begin
  From := AFrom;
end;

constructor TdxDrawingGroupShape2SizeRelH.Create;
begin
  Create(TRelativeFrom.Page);
end;

function TdxDrawingGroupShape2SizeRelH.GetFrom: TRelativeFrom;
begin
  Result := TRelativeFrom(Value);
end;

procedure TdxDrawingGroupShape2SizeRelH.SetFrom(const AValue: TRelativeFrom);
begin
  Value := Ord(AValue);
end;

procedure TdxDrawingGroupShape2SizeRelH.Execute(AOwner: TdxOfficeArtPropertiesBase);
var
  AArtProperties: IdxOfficeArtTertiaryProperties;
begin
  inherited Execute(AOwner);
  if not Supports(AOwner, IdxOfficeArtTertiaryProperties, AArtProperties) then
    Exit;
  AArtProperties.UseRelativeWidth := True;
  AArtProperties.SizeRelH := From;
end;

{ TdxDrawingGroupShape2SizeRelV }

constructor TdxDrawingGroupShape2SizeRelV.Create(AFrom: TRelativeFrom);
begin
  From := AFrom;
end;

function TdxDrawingGroupShape2SizeRelV.GetFrom: TRelativeFrom;
begin
  Result := TRelativeFrom(Value);
end;

procedure TdxDrawingGroupShape2SizeRelV.SetFrom(const AValue: TRelativeFrom);
begin
  Value := Integer(AValue);
end;

procedure TdxDrawingGroupShape2SizeRelV.Execute(AOwner: TdxOfficeArtPropertiesBase);
var
  AArtProperties: IdxOfficeArtTertiaryProperties;
begin
  inherited Execute(AOwner);
  if not Supports(AOwner, IdxOfficeArtTertiaryProperties, AArtProperties) then
    Exit;
  AArtProperties.UseRelativeHeight := True;
  AArtProperties.SizeRelV := From;
end;

{ TdxOfficeArtTertiaryProperties }

constructor TdxOfficeArtTertiaryProperties.Create(AWriteAlways: Boolean = False);
begin
  inherited Create;
  PosRelH := TdxDrawingGroupShapePosRelH.TMsoprh.msoprhText;
  PosRelV := TdxDrawingGroupShapePosRelV.TMsoprv.msoprvText;
  FWriteAlways := AWriteAlways;
end;

destructor TdxOfficeArtTertiaryProperties.Destroy;
begin
  inherited;
end;

class function TdxOfficeArtTertiaryProperties.FromStream(AReader: TBinaryReader;
  AHeader: TdxOfficeArtRecordHeader): TdxOfficeArtTertiaryProperties;
begin
  Result := TdxOfficeArtTertiaryProperties.Create;
  Result.Read(AReader, AHeader);
end;

function TdxOfficeArtTertiaryProperties.GetFilled: Boolean;
begin
  Result := FFilled;
end;

function TdxOfficeArtTertiaryProperties.GetHeaderTypeCode: Integer;
begin
  Result := TdxOfficeArtTypeCodes.TertiaryPropertiesTable;
end;

function TdxOfficeArtTertiaryProperties.GetIsBehindDoc: Boolean;
begin
  Result := FIsBehindDoc;
end;

function TdxOfficeArtTertiaryProperties.GetLayoutInCell: Boolean;
begin
  Result := FLayoutInCell;
end;

function TdxOfficeArtTertiaryProperties.GetPctHorizPosValid: Boolean;
begin
  Result := Cardinal(PctHorizPos) <> $FFFFD8EF;
end;

function TdxOfficeArtTertiaryProperties.GetPctVertPosValid: Boolean;
begin
  Result := Cardinal(PctVertPos) <> $FFFFD8EF;
end;

function TdxOfficeArtTertiaryProperties.ShouldWrite: Boolean;
begin
  Result := FWriteAlways or (Properties.Count > 0);
end;

procedure TdxOfficeArtTertiaryProperties.CreateProperties;
begin
  SetGroupShapePosHProperties;
  SetGroupShapePosRelHProperties;
  SetGroupShapePosVProperties;
  SetGroupShapePosRelVProperties;
  SetGroupShape2PctHorizProperties;
  SetGroupShape2PctVertProperties;
  SetGroupShape2SizeRelHProperties;
  SetGroupShape2SizeRelVProperties;
  SetGroupShapePctPosProperties;
  SetGroupShapeBooleanProperties;
  SetDiagramBooleanProperties;
end;

procedure TdxOfficeArtTertiaryProperties.SetGroupShapePctPosProperties;
begin
  if (PctHorizPos <> 0) or (PctVertPos <> 0) then
  begin
    if PctHorizPos <> 0 then
      Properties.Add(TdxDrawingGroupShape2PctHorizPos.Create(PctHorizPos))
    else
      Properties.Add(TdxDrawingGroupShape2PctHorizPos.Create);
    if PctVertPos <> 0 then
      Properties.Add(TdxDrawingGroupShape2PctVertPos.Create(PctVertPos))
    else
      Properties.Add(TdxDrawingGroupShape2PctVertPos.Create);
  end;
end;

procedure TdxOfficeArtTertiaryProperties.SetGroupShape2PctHorizProperties;
begin
  if UseRelativeWidth then
    Properties.Add(TdxDrawingGroupShape2PctHoriz.Create(PctHoriz));
end;

procedure TdxOfficeArtTertiaryProperties.SetGroupShape2PctVertProperties;
begin
  if UseRelativeHeight then
    Properties.Add(TdxDrawingGroupShape2PctVert.Create(PctVert));
end;

procedure TdxOfficeArtTertiaryProperties.SetGroupShape2SizeRelHProperties;
begin
  if UseRelativeWidth then
    Properties.Add(TdxDrawingGroupShape2SizeRelH.Create(SizeRelH));
end;

procedure TdxOfficeArtTertiaryProperties.SetGroupShape2SizeRelVProperties;
begin
  if UseRelativeHeight then
    Properties.Add(TdxDrawingGroupShape2SizeRelV.Create(SizeRelV));
end;

procedure TdxOfficeArtTertiaryProperties.SetGroupShapeBooleanProperties;
var
  AProperties: TdxDrawingGroupShapeBooleanProperties;
begin
  AProperties := TdxDrawingGroupShapeBooleanProperties.Create;
  AProperties.Value := DefaultFlags;
  Properties.Add(AProperties);
end;

procedure TdxOfficeArtTertiaryProperties.SetGroupShapePosHProperties;
begin
  if UsePosH then
    Properties.Add(TdxDrawingGroupShapePosH.Create(PosH));
end;

procedure TdxOfficeArtTertiaryProperties.SetGroupShapePosRelHProperties;
begin
  if UsePosH then
    Properties.Add(TdxDrawingGroupShapePosRelH.Create(PosRelH));
end;

procedure TdxOfficeArtTertiaryProperties.SetGroupShapePosVProperties;
begin
  if UsePosV then
    Properties.Add(TdxDrawingGroupShapePosV.Create(PosV));
end;

procedure TdxOfficeArtTertiaryProperties.SetIsBehindDoc(AValue: Boolean);
begin
  FIsBehindDoc := AValue;
end;

procedure TdxOfficeArtTertiaryProperties.SetLayoutInCell(AValue: Boolean);
begin
  FLayoutInCell := AValue;
end;

procedure TdxOfficeArtTertiaryProperties.SetGroupShapePosRelVProperties;
begin
  if UsePosV then
    Properties.Add(TdxDrawingGroupShapePosRelV.Create(PosRelV));
end;

procedure TdxOfficeArtTertiaryProperties.SetDiagramBooleanProperties;
begin
  Properties.Add(TdxDiagramBooleanProperties.Create);
end;

procedure TdxOfficeArtTertiaryProperties.SetFilled(AValue: Boolean);
begin
  FFilled := AValue;
end;

function TdxOfficeArtTertiaryProperties.GetPctHoriz: Integer;
begin
  Result := FPctHoriz;
end;

function TdxOfficeArtTertiaryProperties.GetPctHorizPos: Integer;
begin
  Result := FPctHorizPos;
end;

function TdxOfficeArtTertiaryProperties.GetPctVert: Integer;
begin
  Result := FPctVert;
end;

function TdxOfficeArtTertiaryProperties.GetPctVertPos: Integer;
begin
  Result := FPctVertPos;
end;

function TdxOfficeArtTertiaryProperties.GetPosH: TdxDrawingGroupShapePosH.TMsoph;
begin
  Result := FPosH;
end;

function TdxOfficeArtTertiaryProperties.GetPosRelH: TdxDrawingGroupShapePosRelH.TMsoprh;
begin
  Result := FPosRelH;
end;

function TdxOfficeArtTertiaryProperties.GetPosRelV: TdxDrawingGroupShapePosRelV.TMsoprv;
begin
  Result := FPosRelV;
end;

function TdxOfficeArtTertiaryProperties.GetPosV: TdxDrawingGroupShapePosV.TMsopv;
begin
  Result := FPosV;
end;

function TdxOfficeArtTertiaryProperties.GetSizeRelH: TdxDrawingGroupShape2SizeRelH.TRelativeFrom;
begin
  Result := FSizeRelH;
end;

function TdxOfficeArtTertiaryProperties.GetSizeRelV: TdxDrawingGroupShape2SizeRelV.TRelativeFrom;
begin
  Result := FSizeRelV;
end;

function TdxOfficeArtTertiaryProperties.GetUseFilled: Boolean;
begin
  Result := FUseFilled;
end;

function TdxOfficeArtTertiaryProperties.GetUseIsBehindDoc: Boolean;
begin
  Result := FUseIsBehindDoc;
end;

function TdxOfficeArtTertiaryProperties.GetUseLayoutInCell: Boolean;
begin
  Result := FUseLayoutInCell;
end;

function TdxOfficeArtTertiaryProperties.GetUsePosH: Boolean;
begin
  Result := FUsePosH;
end;

function TdxOfficeArtTertiaryProperties.GetUsePosV: Boolean;
begin
  Result := FUsePosV;
end;

function TdxOfficeArtTertiaryProperties.GetUseRelativeHeight: Boolean;
begin
  Result := FUseRelativeHeight;
end;

function TdxOfficeArtTertiaryProperties.GetUseRelativeWidth: Boolean;
begin
  Result := FUseRelativeWidth;
end;

procedure TdxOfficeArtTertiaryProperties.SetPctHoriz(const AValue: Integer);
begin
  FPctHoriz := AValue;
end;

procedure TdxOfficeArtTertiaryProperties.SetPctHorizPos(const AValue: Integer);
begin
  FPctHorizPos := AValue;
end;

procedure TdxOfficeArtTertiaryProperties.SetPctVert(const AValue: Integer);
begin
  FPctVert := AValue;
end;

procedure TdxOfficeArtTertiaryProperties.SetPctVertPos(const AValue: Integer);
begin
  FPctVertPos := AValue;
end;

procedure TdxOfficeArtTertiaryProperties.SetPosH(const AValue: TdxDrawingGroupShapePosH.TMsoph);
begin
  FPosH := AValue;
end;

procedure TdxOfficeArtTertiaryProperties.SetPosRelH(const AValue: TdxDrawingGroupShapePosRelH.TMsoprh);
begin
  FPosRelH := AValue;
end;

procedure TdxOfficeArtTertiaryProperties.SetPosRelV(const AValue: TdxDrawingGroupShapePosRelV.TMsoprv);
begin
  FPosRelV := AValue;
end;

procedure TdxOfficeArtTertiaryProperties.SetPosV(const AValue: TdxDrawingGroupShapePosV.TMsopv);
begin
  FPosV := AValue;
end;

procedure TdxOfficeArtTertiaryProperties.SetSizeRelH(const AValue: TdxDrawingGroupShape2SizeRelH.TRelativeFrom);
begin
  FSizeRelH := AValue;
end;

procedure TdxOfficeArtTertiaryProperties.SetSizeRelV(const AValue: TdxDrawingGroupShape2SizeRelV.TRelativeFrom);
begin
  FSizeRelV := AValue;
end;

procedure TdxOfficeArtTertiaryProperties.SetUseFilled(AValue: Boolean);
begin
  FUseFilled := AValue;
end;

procedure TdxOfficeArtTertiaryProperties.SetUseIsBehindDoc(AValue: Boolean);
begin
  FUseIsBehindDoc := AValue;
end;

procedure TdxOfficeArtTertiaryProperties.SetUseLayoutInCell(AValue: Boolean);
begin
  FUseLayoutInCell := AValue;
end;

procedure TdxOfficeArtTertiaryProperties.SetUsePosH(const AValue: Boolean);
begin
  FUsePosH := AValue;
end;

procedure TdxOfficeArtTertiaryProperties.SetUsePosV(const AValue: Boolean);
begin
  FUsePosV := AValue;
end;

procedure TdxOfficeArtTertiaryProperties.SetUseRelativeHeight(const AValue: Boolean);
begin
  FUseRelativeHeight := AValue;
end;

procedure TdxOfficeArtTertiaryProperties.SetUseRelativeWidth(
  const AValue: Boolean);
begin
  FUseRelativeWidth := AValue;
end;

{ TdxOfficeDrawingMsoArrayPropertyBase }

function TdxOfficeDrawingMsoArrayPropertyBase.GetComplex: Boolean;
begin
  Result := True;
end;

procedure TdxOfficeDrawingMsoArrayPropertyBase.Read(AReader: TBinaryReader);
begin
  Value := AReader.ReadInt32;
end;

procedure TdxOfficeDrawingMsoArrayPropertyBase.Execute(AOwner: TdxOfficeArtPropertiesBase);
begin
end;

procedure TdxOfficeDrawingMsoArrayPropertyBase.Write(AWriter: TBinaryWriter);
begin
  AWriter.Write(Word(TdxOfficePropertiesFactory.GetOpcodeByType(ClassType)));
  AWriter.Write(Value);
end;

{ TdxOfficePropertiesFactory }

class constructor TdxOfficePropertiesFactory.Initialize;
begin
  FPropertyIdentifiers := TDictionary<TdxOfficeDrawingPropertyClass, SmallInt>.Create;
  FPropertyTypes := TDictionary<SmallInt, TdxOfficeDrawingPropertyClass>.Create;

  AddProperty($007f, TdxDrawingBooleanProtectionProperties);
//  AddProperty($0100, TdxDrawingCropFromTop);
//  AddProperty($0101, TdxDrawingCropFromBottom);
//  AddProperty($0102, TdxDrawingCropFromLeft);
//  AddProperty($0103, TdxDrawingCropFromRight);
//  AddProperty($c105, TdxDrawingBlipName);
//  AddProperty($0106, TdxDrawingBlipFlags);
//  AddProperty($0108, TdxDrawingBlipContrast);
//  AddProperty($0109, TdxDrawingBlipBrightness);
  AddProperty($013f, TdxDrawingBlipBooleanProperties);
  AddProperty($01bf, TdxDrawingFillStyleBooleanProperties);
  AddProperty($01cb, TdxDrawingLineWidth);
//  AddProperty($01cc, TdxDrawingLineMiterLimit);
//  AddProperty($01cd, TdxDrawingLineCompoundType);
//  AddProperty($01ce, TdxDrawingLineDashing);
//  AddProperty($01d0, TdxDrawingLineStartArrowhead);
//  AddProperty($01d1, TdxDrawingLineEndArrowhead);
//  AddProperty($01d2, TdxDrawingLineStartArrowWidth);
//  AddProperty($01d3, TdxDrawingLineStartArrowLength);
//  AddProperty($01d4, TdxDrawingLineEndArrowWidth);
//  AddProperty($01d5, TdxDrawingLineEndArrowLength);
//  AddProperty($01d6, TdxDrawingLineJoinStyle);
//  AddProperty($01d7, TdxDrawingLineCapStyle);
  AddProperty($01ff, TdxDrawingLineStyleBooleanProperties);
  AddProperty($0304, TdxDrawingBlackWhiteMode);
  AddProperty($033f, TdxDrawingShapeBooleanProperties);
  AddProperty($4104, TdxDrawingBlipIdentifier);
  AddProperty($0080, TdxDrawingTextIdentifier);
  AddProperty($c380, TdxDrawingShapeName);
//  AddProperty($c381, TdxDrawingShapeDescription);
//  AddProperty($c382, TdxDrawingShapeHyperlink);
//  AddProperty($c38d, TdxDrawingShapeTooltip);
  AddProperty($03bf, TdxDrawingGroupShapeBooleanProperties);
  AddProperty($053f, TdxDiagramBooleanProperties);

  AddProperty($038F, TdxDrawingGroupShapePosH);
  AddProperty($0390, TdxDrawingGroupShapePosRelH);
  AddProperty($0391, TdxDrawingGroupShapePosV);
  AddProperty($0392, TdxDrawingGroupShapePosRelV);

  AddProperty($07c0, TdxDrawingGroupShape2PctHoriz);
  AddProperty($07c1, TdxDrawingGroupShape2PctVert);
  AddProperty($07c2, TdxDrawingGroupShape2PctHorizPos);
  AddProperty($07c3, TdxDrawingGroupShape2PctVertPos);
  AddProperty($07c4, TdxDrawingGroupShape2SizeRelH);
  AddProperty($07c5, TdxDrawingGroupShape2SizeRelV);
  AddProperty($0004, TdxDrawingRotation);
  AddProperty($01c0, TdxDrawingLineColor);
//  AddProperty($01c1, TdxDrawingLineOpacity);
  AddProperty($0081, TdxDrawingTextLeft);
  AddProperty($0082, TdxDrawingTextTop);
  AddProperty($0083, TdxDrawingTextRight);
  AddProperty($0084, TdxDrawingTextBottom);
  AddProperty($0384, TdxDrawingWrapLeftDistance);
  AddProperty($0385, TdxDrawingWrapTopDistance);
  AddProperty($0386, TdxDrawingWrapRightDistance);
  AddProperty($0387, TdxDrawingWrapBottomDistance);
//  AddProperty($4186, TdxDrawingFillBlipIdentifier);
//  AddProperty($0201, TdxDrawingShadowColor);
  AddProperty($023f, TdxDrawingShadowStyleBooleanProperties);
//  AddProperty($0158, TdxDrawingConnectionPointsType);
  AddProperty($00bf, TdxDrawingTextBooleanProperties);
//  AddProperty($008b, TdxDrawingTextDirection);

//  AddProperty($0180, TdxOfficeDrawingFillType);
  AddProperty($0181, TdxDrawingFillColor);
//  AddProperty($0182, TdxDrawingFillOpacity);
//  AddProperty($0183, TdxDrawingFillBackColor);
//  AddProperty($0184, TdxDrawingFillBackOpacity);
//  AddProperty($0185, TdxDrawingFillBWColor);
//  AddProperty($c186, TdxDrawingFillBlip);
//  AddProperty($c187, TdxDrawingFillBlipName);
//  AddProperty($0188, TdxDrawingFillBlipFlags);
//  AddProperty($0189, TdxDrawingFillWidth);
//  AddProperty($018a, TdxDrawingFillHeight);
//  AddProperty($018b, TdxDrawingFillAngle);
//  AddProperty($018c, TdxDrawingFillFocus);
//  AddProperty($018d, TdxDrawingFillToLeft);
//  AddProperty($018e, TdxDrawingFillToTop);
//  AddProperty($018f, TdxDrawingFillToRight);
//  AddProperty($0190, TdxDrawingFillToBottom);
//  AddProperty($0191, TdxDrawingFillRectLeft);
//  AddProperty($0192, TdxDrawingFillRectTop);
//  AddProperty($0193, TdxDrawingFillRectRight);
//  AddProperty($0194, TdxDrawingFillRectBottom);
//  AddProperty($0195, TdxDrawingFillDzType);
//  AddProperty($0196, TdxDrawingFillShadePreset);
//  AddProperty($C197, TdxDrawingFillShadeColors);
//  AddProperty($0198, TdxDrawingFillOriginX);
//  AddProperty($0199, TdxDrawingFillOriginY);
//  AddProperty($019a, TdxDrawingFillShapeOriginX);
//  AddProperty($019b, TdxDrawingFillShapeOriginY);
//  AddProperty($019c, TdxDrawingFillShadeType);

//  AddProperty($019e, TdxDrawingFillColorExt);
//  AddProperty($019f, TdxDrawingFillReserved415);
//  AddProperty($01a0, TdxDrawingFillTintShade);
//  AddProperty($c1a1, TdxDrawingFillReserved417);
//  AddProperty($01a2, TdxDrawingFillBackColorExt);
//  AddProperty($01a3, TdxDrawingFillReserved419);
//  AddProperty($01a4, TdxDrawingFillBackTintShade);
//  AddProperty($c1a5, TdxDrawingFillReserved421);
//  AddProperty($01a6, TdxDrawingFillReserved422);
//  AddProperty($01a7, TdxDrawingFillReserved423);

//  AddProperty($C145, TdxDrawingGeometryVertices);
//  AddProperty($C151, TdxDrawingGeometryConnectionSites);

//  AddProperty($0140, TdxDrawingGeometryLeft);
//  AddProperty($0141, TdxDrawingGeometryTop);
//  AddProperty($0142, TdxDrawingGeometryRight);
//  AddProperty($0143, TdxDrawingGeometryBottom);
//  AddProperty($0144, TdxDrawingGeometryShapePath);
//  AddProperty($C146, TdxDrawingGeometrySegmentInfo);
//  AddProperty($0147, TdxDrawingGeometryAdjustValue1);
//  AddProperty($0148, TdxDrawingGeometryAdjustValue2);
//  AddProperty($0149, TdxDrawingGeometryAdjustValue3);
//  AddProperty($014A, TdxDrawingGeometryAdjustValue4);
//  AddProperty($014B, TdxDrawingGeometryAdjustValue5);
//  AddProperty($014C, TdxDrawingGeometryAdjustValue6);
//  AddProperty($014D, TdxDrawingGeometryAdjustValue7);
//  AddProperty($014E, TdxDrawingGeometryAdjustValue8);
//  AddProperty($C152, TdxDrawingGeometryConnectionSitesDir);
//  AddProperty($017F, TdxDrawingGeometryBooleanProperties);
//  AddProperty($C3A9, TdxDrawingMetroBlob);
//  AddProperty($0303, TdxDrawingCxStyle);

//  AddProperty($0085, TdxDrawingShapeWrapText);
//  AddProperty($0087, TdxDrawingShapeAnchorText);
//  AddProperty($0088, TdxDrawingShapeTextFlow);
//  AddProperty($0089, TdxDrawingShapeFontDirection);
end;

class destructor TdxOfficePropertiesFactory.Finalize;
begin
  FPropertyTypes.Free;
  FPropertyIdentifiers.Free;
end;

class procedure TdxOfficePropertiesFactory.AddProperty(AId: Integer; AType: TdxOfficeDrawingPropertyClass);
begin
  FPropertyTypes.Add(SmallInt(AId), AType);
  FPropertyIdentifiers.Add(AType, SmallInt(AId));
end;

class function TdxOfficePropertiesFactory.CreateProperty(AReader: TBinaryReader): IdxOfficeDrawingProperty;
var
  ABitwiseField: Word;
  AOpcode: SmallInt;
  AComplex: Boolean;
  AType: TdxOfficeDrawingPropertyClass;
  AProperty: TdxOfficeDrawingPropertyBase;
begin
  ABitwiseField := AReader.ReadUInt16;
  AOpcode := SmallInt(ABitwiseField);

  if not FPropertyTypes.TryGetValue(AOpcode, AType) then
  begin
    if not FPropertyTypes.TryGetValue(SmallInt(AOpcode or $4000), AType) then
      AType := TdxDrawingEmpty;
  end;
  AProperty := AType.Create;
  AComplex := (ABitwiseField and $8000) <> 0;
  AProperty.SetComplex(AComplex);
  AProperty.Read(AReader);
  Result := AProperty;
end;

class function TdxOfficePropertiesFactory.GetOpcodeByType(APropertyType: TClass): SmallInt;
begin
  if not FPropertyIdentifiers.TryGetValue(TdxOfficeDrawingPropertyClass(APropertyType), Result) then
    Result := $0000;
end;

{ TdxDrawingTextLeft }

constructor TdxDrawingTextLeft.Create(AValue: Integer);
begin
  Value := AValue;
end;

constructor TdxDrawingTextLeft.Create;
begin
  Create(DefaultValue);
end;

function TdxDrawingTextLeft.GetComplex: Boolean;
begin
  Result := False;
end;

procedure TdxDrawingTextLeft.Execute(AOwner: TdxOfficeArtPropertiesBase);
var
  AArtProperties: IdxOfficeArtProperties;
begin
  if not Supports(AOwner, IdxOfficeArtProperties, AArtProperties) then
    Exit;
  AArtProperties.TextLeft := Value;
  AArtProperties.UseTextLeft := True;
end;

{ TdxDrawingTextTop }

constructor TdxDrawingTextTop.Create(AValue: Integer);
begin
  Value := AValue;
end;

constructor TdxDrawingTextTop.Create;
begin
  Create(DefaultValue);
end;

function TdxDrawingTextTop.GetComplex: Boolean;
begin
  Result := False;
end;

procedure TdxDrawingTextTop.Execute(AOwner: TdxOfficeArtPropertiesBase);
var
  AArtProperties: IdxOfficeArtProperties;
begin
  if not Supports(AOwner, IdxOfficeArtProperties, AArtProperties) then
    Exit;
  AArtProperties.TextTop := Value;
  AArtProperties.UseTextTop := True;
end;

{ TdxDrawingTextRight }

constructor TdxDrawingTextRight.Create(AValue: Integer);
begin
  Value := AValue;
end;

constructor TdxDrawingTextRight.Create;
begin
  Create(DefaultValue);
end;

function TdxDrawingTextRight.GetComplex: Boolean;
begin
  Result := False;
end;

procedure TdxDrawingTextRight.Execute(AOwner: TdxOfficeArtPropertiesBase);
var
  AArtProperties: IdxOfficeArtProperties;
begin
  if not Supports(AOwner, IdxOfficeArtProperties, AArtProperties) then
    Exit;
  AArtProperties.TextRight := Value;
  AArtProperties.UseTextRight := True;
end;

{ TdxDrawingTextBottom }

constructor TdxDrawingTextBottom.Create(AValue: Integer);
begin
  Value := AValue;
end;

constructor TdxDrawingTextBottom.Create;
begin
  Create(DefaultValue);
end;

function TdxDrawingTextBottom.GetComplex: Boolean;
begin
  Result := False;
end;

procedure TdxDrawingTextBottom.Execute(AOwner: TdxOfficeArtPropertiesBase);
var
  AArtProperties: IdxOfficeArtProperties;
begin
  if not Supports(AOwner, IdxOfficeArtProperties, AArtProperties) then
    Exit;
  AArtProperties.TextBottom := Value;
  AArtProperties.UseTextBottom := True;
end;

{ TdxOfficeDrawingFixedPointPropertyBase }

function TdxOfficeDrawingFixedPointPropertyBase.GetComplex: Boolean;
begin
  Result := True;
end;

procedure TdxOfficeDrawingFixedPointPropertyBase.Read(AReader: TBinaryReader);
begin
  FValue := TdxFixedPoint.FromStream(AReader).Value;
end;

procedure TdxOfficeDrawingFixedPointPropertyBase.Execute(AOwner: TdxOfficeArtPropertiesBase);
begin
end;

procedure TdxOfficeDrawingFixedPointPropertyBase.Write(AWriter: TBinaryWriter);
var
  AFixedPoint: TdxFixedPoint;
begin
  AWriter.Write(TdxOfficePropertiesFactory.GetOpcodeByType(ClassType));
  AFixedPoint.Value := Value;
  AFixedPoint.Write(AWriter);
end;

{ TdxDrawingRotation }

constructor TdxDrawingRotation.Create(ARotation: Integer);
begin
  Value := ARotation;
end;

function TdxDrawingRotation.GetComplex: Boolean;
begin
  Result := False;
end;

procedure TdxDrawingRotation.Execute(AOwner: TdxOfficeArtPropertiesBase);
var
  AArtProperties: IdxOfficeArtProperties;
begin
  if Supports(AOwner, IdxOfficeArtProperties, AArtProperties) then
    AArtProperties.Rotation := Value;
end;

{ TdxOfficeDrawingColorPropertyBase }

constructor TdxOfficeDrawingColorPropertyBase.Create;
begin
  FColorRecord := TdxOfficeColorRecord.Create(TdxAlphaColors.Empty);
end;

constructor TdxOfficeDrawingColorPropertyBase.Create(AColor: TdxAlphaColor);
begin
  FColorRecord := TdxOfficeColorRecord.Create(AColor);
end;

destructor TdxOfficeDrawingColorPropertyBase.Destroy;
begin
  FColorRecord.Free;
  inherited Destroy;
end;

procedure TdxOfficeDrawingColorPropertyBase.Read(AReader: TBinaryReader);
begin
  ColorRecord := TdxOfficeColorRecord.FromStream(AReader);
end;

procedure TdxOfficeDrawingColorPropertyBase.SetColorRecord(
  const Value: TdxOfficeColorRecord);
begin
  if FColorRecord <> Value then
  begin
    FColorRecord.Free;
    FColorRecord := Value;
  end;
end;

procedure TdxOfficeDrawingColorPropertyBase.Write(AWriter: TBinaryWriter);
begin
  AWriter.Write(TdxOfficePropertiesFactory.GetOpcodeByType(ClassType));
  ColorRecord.Write(AWriter);
end;

{ TdxDrawingFillColor }

procedure TdxDrawingFillColor.Execute(AOwner: TdxOfficeArtPropertiesBase);
var
  AArtProperties: IdxOfficeArtProperties;
begin
  if Supports(AOwner, IdxOfficeArtProperties, AArtProperties) then
    AArtProperties.FillColor := ColorRecord.Color;
end;

{ TdxDrawingLineColor }

constructor TdxDrawingLineColor.Create(AColorIndex: Integer);
begin
  ColorRecord := TdxOfficeColorRecord.Create(AColorIndex);
end;

constructor TdxDrawingLineColor.Create(AColor: TdxAlphaColor);
begin
  ColorRecord := TdxOfficeColorRecord.Create(AColor);
end;

procedure TdxDrawingLineColor.Execute(AOwner: TdxOfficeArtPropertiesBase);
var
  AArtProperties: IdxOfficeArtProperties;
begin
  if Supports(AOwner, IdxOfficeArtProperties, AArtProperties) then
    AArtProperties.LineColor := ColorRecord.Color;
end;

{ TdxDrawingLineWidth }

constructor TdxDrawingLineWidth.Create(AWidth: Integer = TdxOfficeArtConstants.DefaultLineWidthInEmus);
begin
  Value := AWidth;
end;

function TdxDrawingLineWidth.GetComplex: Boolean;
begin
  Result := False;
end;

procedure TdxDrawingLineWidth.Execute(AOwner: TdxOfficeArtPropertiesBase);
var
  AArtProperties: IdxOfficeArtProperties;
begin
  if Supports(AOwner, IdxOfficeArtProperties, AArtProperties) then
    AArtProperties.LineWidth := Value;
end;

{ TdxOfficeDrawingBooleanPropertyBase }

procedure TdxOfficeDrawingBooleanPropertyBase.Merge(const AOther: IdxOfficeDrawingProperty);
var
  AOtherProp: TdxOfficeDrawingBooleanPropertyBase;
  AOtherValue, AThisValue, AOtherValueMask: Cardinal;
begin
  AOtherProp := TdxOfficeDrawingBooleanPropertyBase(AOther);
  if AOtherProp <> nil then
  begin
    AOtherValue := Cardinal(AOtherProp.Value);
    AThisValue := Cardinal(Value);
    AOtherValue := AOtherValue and not (AThisValue and UseMask);
    AOtherValueMask := (AOtherValue and UseMask) shr 16;
    AOtherValue := AOtherValue and (UseMask) or (AOtherValueMask);
    AThisValue := AThisValue and not AOtherValueMask;
    AThisValue := AThisValue or AOtherValue;
    Value := Integer(AThisValue);
  end;
end;

{ TdxDrawingBooleanProtectionProperties }

constructor TdxDrawingBooleanProtectionProperties.Create;
begin
  Value := CDefaultFlags;
end;

function TdxDrawingBooleanProtectionProperties.GetComplex: Boolean;
begin
  Result := False;
end;

function TdxDrawingBooleanProtectionProperties.GetLockGroup: Boolean;
begin
  Result := GetFlag(CLockGroup);
end;

procedure TdxDrawingBooleanProtectionProperties.SetLockGroup(const AValue: Boolean);
begin
  SetFlag(CLockGroup, AValue);
end;

function TdxDrawingBooleanProtectionProperties.GetLockAdjustHandles: Boolean;
begin
  Result := GetFlag(CLockAdjustHandles);
end;

procedure TdxDrawingBooleanProtectionProperties.SetLockAdjustHandles(const AValue: Boolean);
begin
  SetFlag(CLockAdjustHandles, AValue);
end;

function TdxDrawingBooleanProtectionProperties.GetLockText: Boolean;
begin
  Result := GetFlag(CLockText);
end;

procedure TdxDrawingBooleanProtectionProperties.SetLockText(const AValue: Boolean);
begin
  SetFlag(CLockText, AValue);
end;

function TdxDrawingBooleanProtectionProperties.GetLockVertices: Boolean;
begin
  Result := GetFlag(CLockVertices);
end;

procedure TdxDrawingBooleanProtectionProperties.SetLockVertices(const AValue: Boolean);
begin
  SetFlag(CLockVertices, AValue);
end;

function TdxDrawingBooleanProtectionProperties.GetLockCropping: Boolean;
begin
  Result := GetFlag(CLockCropping);
end;

procedure TdxDrawingBooleanProtectionProperties.SetLockCropping(const AValue: Boolean);
begin
  SetFlag(CLockCropping, AValue);
end;

function TdxDrawingBooleanProtectionProperties.GetLockSelect: Boolean;
begin
  Result := GetFlag(CLockSelect);
end;

procedure TdxDrawingBooleanProtectionProperties.SetLockSelect(const AValue: Boolean);
begin
  SetFlag(CLockSelect, AValue);
end;

function TdxDrawingBooleanProtectionProperties.GetLockPosition: Boolean;
begin
  Result := GetFlag(CLockPosition);
end;

procedure TdxDrawingBooleanProtectionProperties.SetLockPosition(const AValue: Boolean);
begin
  SetFlag(CLockPosition, AValue);
end;

function TdxDrawingBooleanProtectionProperties.GetLockAspectRatio: Boolean;
begin
  Result := GetFlag(CLockAspectRatio);
end;

procedure TdxDrawingBooleanProtectionProperties.SetLockAspectRatio(const AValue: Boolean);
begin
  SetFlag(CLockAspectRatio, AValue);
end;

function TdxDrawingBooleanProtectionProperties.GetLockRotation: Boolean;
begin
  Result := GetFlag(CLockRotation);
end;

procedure TdxDrawingBooleanProtectionProperties.SetLockRotation(const AValue: Boolean);
begin
  SetFlag(CLockRotation, AValue);
end;

function TdxDrawingBooleanProtectionProperties.GetLockUngroup: Boolean;
begin
  Result := GetFlag(CLockUngroup);
end;

procedure TdxDrawingBooleanProtectionProperties.SetLockUngroup(const AValue: Boolean);
begin
  SetFlag(CLockUngroup, AValue);
end;

function TdxDrawingBooleanProtectionProperties.GetUseLockGroup: Boolean;
begin
  Result := GetFlag(CUseLockGroup);
end;

procedure TdxDrawingBooleanProtectionProperties.SetUseLockGroup(const AValue: Boolean);
begin
  SetFlag(CUseLockGroup, AValue);
end;

function TdxDrawingBooleanProtectionProperties.GetUseLockAdjustHandles: Boolean;
begin
  Result := GetFlag(CUseLockAdjustHandles);
end;

procedure TdxDrawingBooleanProtectionProperties.SetUseLockAdjustHandles(const AValue: Boolean);
begin
  SetFlag(CUseLockAdjustHandles, AValue);
end;

function TdxDrawingBooleanProtectionProperties.GetUseLockText: Boolean;
begin
  Result := GetFlag(CUseLockText);
end;

procedure TdxDrawingBooleanProtectionProperties.SetUseLockText(const AValue: Boolean);
begin
  SetFlag(CUseLockText, AValue);
end;

function TdxDrawingBooleanProtectionProperties.GetUseLockVertices: Boolean;
begin
  Result := GetFlag(CUseLockVertices);
end;

procedure TdxDrawingBooleanProtectionProperties.SetUseLockVertices(const AValue: Boolean);
begin
  SetFlag(CUseLockVertices, AValue);
end;

function TdxDrawingBooleanProtectionProperties.GetUseLockCropping: Boolean;
begin
  Result := GetFlag(CUseLockCropping);
end;

procedure TdxDrawingBooleanProtectionProperties.SetUseLockCropping(const AValue: Boolean);
begin
  SetFlag(CUseLockCropping, AValue);
end;

function TdxDrawingBooleanProtectionProperties.GetUseLockSelect: Boolean;
begin
  Result := GetFlag(CUseLockSelect);
end;

procedure TdxDrawingBooleanProtectionProperties.SetUseLockSelect(const AValue: Boolean);
begin
  SetFlag(CUseLockSelect, AValue);
end;

function TdxDrawingBooleanProtectionProperties.GetUseLockPosition: Boolean;
begin
  Result := GetFlag(CUseLockPosition);
end;

procedure TdxDrawingBooleanProtectionProperties.SetUseLockPosition(const AValue: Boolean);
begin
  SetFlag(CUseLockPosition, AValue);
end;

function TdxDrawingBooleanProtectionProperties.GetUseLockAspectRatio: Boolean;
begin
  Result := GetFlag(CUseLockAspectRatio);
end;

procedure TdxDrawingBooleanProtectionProperties.SetUseLockAspectRatio(const AValue: Boolean);
begin
  SetFlag(CUseLockAspectRatio, AValue);
end;

function TdxDrawingBooleanProtectionProperties.GetUseLockRotation: Boolean;
begin
  Result := GetFlag(CUseLockRotation);
end;

procedure TdxDrawingBooleanProtectionProperties.SetUseLockRotation(const AValue: Boolean);
begin
  SetFlag(CUseLockRotation, AValue);
end;

function TdxDrawingBooleanProtectionProperties.GetUseLockUngroup: Boolean;
begin
  Result := GetFlag(CUseLockUngroup);
end;

procedure TdxDrawingBooleanProtectionProperties.SetUseLockUngroup(const AValue: Boolean);
begin
  SetFlag(CUseLockUngroup, AValue);
end;

{ TdxDrawingBlipIdentifier }

function TdxDrawingBlipIdentifier.GetComplex: Boolean;
begin
  Result := False;
end;

procedure TdxDrawingBlipIdentifier.Execute(AOwner: TdxOfficeArtPropertiesBase);
var
  AArtProperties: IdxOfficeArtProperties;
begin
  if not Supports(AOwner, IdxOfficeArtProperties, AArtProperties) then
    Exit;
  AArtProperties.BlipIndex := Value;
  AArtProperties.ZOrder := Value;
end;

{ TdxDrawingTextIdentifier }

function TdxDrawingTextIdentifier.GetComplex: Boolean;
begin
  Result := False;
end;

procedure TdxDrawingTextIdentifier.Execute(AOwner: TdxOfficeArtPropertiesBase);
var
  AArtProperties: IdxOfficeArtProperties;
  AIndex: Integer;
begin
  if not Supports(AOwner, IdxOfficeArtProperties, AArtProperties) then
    Exit;
  AIndex := Value div Coeff;
  AArtProperties.TextIndex := AIndex;
  AArtProperties.ZOrder := AIndex;
end;

procedure TdxDrawingTextIdentifier.Write(AWriter: TBinaryWriter);
begin
  Value := Value * Coeff;
  inherited Write(AWriter);
end;

{ TdxDrawingBlipBooleanProperties }

function TdxDrawingBlipBooleanProperties.GetComplex: Boolean;
begin
  Result := False;
end;

function TdxDrawingBlipBooleanProperties.GetUsePicturePreserveGrays: Boolean;
begin
  Result := GetFlag(CUsePicturePreserveGrays);
end;

procedure TdxDrawingBlipBooleanProperties.SetUsePicturePreserveGrays(const AValue: Boolean);
begin
  SetFlag(CUsePicturePreserveGrays, AValue);
end;

function TdxDrawingBlipBooleanProperties.GetUseRewind: Boolean;
begin
  Result := GetFlag(CUseRewind);
end;

procedure TdxDrawingBlipBooleanProperties.SetUseRewind(const AValue: Boolean);
begin
  SetFlag(CUseRewind, AValue);
end;

function TdxDrawingBlipBooleanProperties.GetUseLooping: Boolean;
begin
  Result := GetFlag(CUseLooping);
end;

procedure TdxDrawingBlipBooleanProperties.SetUseLooping(const AValue: Boolean);
begin
  SetFlag(CUseLooping, AValue);
end;

function TdxDrawingBlipBooleanProperties.GetUseNoHitTestPicture: Boolean;
begin
  Result := GetFlag(CUseNoHitTestPicture);
end;

procedure TdxDrawingBlipBooleanProperties.SetUseNoHitTestPicture(const AValue: Boolean);
begin
  SetFlag(CUseNoHitTestPicture, AValue);
end;

function TdxDrawingBlipBooleanProperties.GetUsePictureGray: Boolean;
begin
  Result := GetFlag(CUsePictureGray);
end;

procedure TdxDrawingBlipBooleanProperties.SetUsePictureGray(const AValue: Boolean);
begin
  SetFlag(CUsePictureGray, AValue);
end;

function TdxDrawingBlipBooleanProperties.GetUsePictureBiLevel: Boolean;
begin
  Result := GetFlag(CUsePictureBiLevel);
end;

procedure TdxDrawingBlipBooleanProperties.SetUsePictureBiLevel(const AValue: Boolean);
begin
  SetFlag(CUsePictureBiLevel, AValue);
end;

function TdxDrawingBlipBooleanProperties.GetUsePictureActive: Boolean;
begin
  Result := GetFlag(CUsePictureActive);
end;

procedure TdxDrawingBlipBooleanProperties.SetUsePictureActive(const AValue: Boolean);
begin
  SetFlag(CUsePictureActive, AValue);
end;

function TdxDrawingBlipBooleanProperties.GetPicturePreserveGrays: Boolean;
begin
  Result := GetFlag(CPicturePreserveGrays);
end;

procedure TdxDrawingBlipBooleanProperties.SetPicturePreserveGrays(const AValue: Boolean);
begin
  SetFlag(CPicturePreserveGrays, AValue);
end;

function TdxDrawingBlipBooleanProperties.GetRewind: Boolean;
begin
  Result := GetFlag(CRewind);
end;

procedure TdxDrawingBlipBooleanProperties.SetRewind(const AValue: Boolean);
begin
  SetFlag(CRewind, AValue);
end;

function TdxDrawingBlipBooleanProperties.GetLooping: Boolean;
begin
  Result := GetFlag(CLooping);
end;

procedure TdxDrawingBlipBooleanProperties.SetLooping(const AValue: Boolean);
begin
  SetFlag(CLooping, AValue);
end;

function TdxDrawingBlipBooleanProperties.GetNoHitTestPicture: Boolean;
begin
  Result := GetFlag(CNoHitTestPicture);
end;

procedure TdxDrawingBlipBooleanProperties.SetNoHitTestPicture(const AValue: Boolean);
begin
  SetFlag(CNoHitTestPicture, AValue);
end;

function TdxDrawingBlipBooleanProperties.GetPictureGray: Boolean;
begin
  Result := GetFlag(CPictureGray);
end;

procedure TdxDrawingBlipBooleanProperties.SetPictureGray(const AValue: Boolean);
begin
  SetFlag(CPictureGray, AValue);
end;

function TdxDrawingBlipBooleanProperties.GetPictureBiLevel: Boolean;
begin
  Result := GetFlag(CPictureBiLevel);
end;

procedure TdxDrawingBlipBooleanProperties.SetPictureBiLevel(const AValue: Boolean);
begin
  SetFlag(CPictureBiLevel, AValue);
end;

function TdxDrawingBlipBooleanProperties.GetPictureActive: Boolean;
begin
  Result := GetFlag(CPictureActive);
end;

procedure TdxDrawingBlipBooleanProperties.SetPictureActive(const AValue: Boolean);
begin
  SetFlag(CPictureActive, AValue);
end;

{ TdxDrawingFillStyleBooleanProperties }

constructor TdxDrawingFillStyleBooleanProperties.Create;
begin
  FFillStyle := TFillStyle.Filled or TFillStyle.UseFilled;
end;

function TdxDrawingFillStyleBooleanProperties.GetNoFillHitTest: Boolean;
begin
  Result := (FFillStyle and TFillStyle.NoFillHitTest) = TFillStyle.NoFillHitTest;
end;

procedure TdxDrawingFillStyleBooleanProperties.SetNoFillHitTest(const AValue: Boolean);
begin
  if AValue then
    FFillStyle := FFillStyle or TFillStyle.NoFillHitTest
  else
    FFillStyle := FFillStyle and not TFillStyle.NoFillHitTest;
end;

function TdxDrawingFillStyleBooleanProperties.GetUseNoFillHitTest: Boolean;
begin
  Result := (FFillStyle and TFillStyle.UseNoFillHitTest) = TFillStyle.UseNoFillHitTest;
end;

procedure TdxDrawingFillStyleBooleanProperties.SetUseNoFillHitTest(const AValue: Boolean);
begin
  if AValue then
    FFillStyle := FFillStyle or TFillStyle.UseNoFillHitTest
  else
    FFillStyle := FFillStyle and not TFillStyle.UseNoFillHitTest;
end;

function TdxDrawingFillStyleBooleanProperties.GetFillUseRect: Boolean;
begin
  Result := (FFillStyle and TFillStyle.FillUseRect) = TFillStyle.FillUseRect;
end;

procedure TdxDrawingFillStyleBooleanProperties.SetFillUseRect(const AValue: Boolean);
begin
  if AValue then
    FFillStyle := FFillStyle or TFillStyle.FillUseRect
  else
    FFillStyle := FFillStyle and not TFillStyle.FillUseRect;
end;

function TdxDrawingFillStyleBooleanProperties.GetUseFillUseRect: Boolean;
begin
  Result := (FFillStyle and TFillStyle.UseFillUseRect) = TFillStyle.UseFillUseRect;
end;

procedure TdxDrawingFillStyleBooleanProperties.SetUseFillUseRect(const AValue: Boolean);
begin
  if AValue then
    FFillStyle := FFillStyle or TFillStyle.UseFillUseRect
  else
    FFillStyle := FFillStyle and not TFillStyle.UseFillUseRect;
end;

function TdxDrawingFillStyleBooleanProperties.GetFillShape: Boolean;
begin
  Result := (FFillStyle and TFillStyle.FillShape) = TFillStyle.FillShape;
end;

procedure TdxDrawingFillStyleBooleanProperties.SetFillShape(const AValue: Boolean);
begin
  if AValue then
    FFillStyle := FFillStyle or TFillStyle.FillShape
  else
    FFillStyle := FFillStyle and not TFillStyle.FillShape;
end;

function TdxDrawingFillStyleBooleanProperties.GetUseFillShape: Boolean;
begin
  Result := (FFillStyle and TFillStyle.UseFillShape) = TFillStyle.UseFillShape;
end;

procedure TdxDrawingFillStyleBooleanProperties.SetUseFillShape(const AValue: Boolean);
begin
  if AValue then
    FFillStyle := FFillStyle or TFillStyle.UseFillShape
  else
    FFillStyle := FFillStyle and not TFillStyle.UseFillShape;
end;

function TdxDrawingFillStyleBooleanProperties.GetHitTestFill: Boolean;
begin
  Result := (FFillStyle and TFillStyle.HitTestFill) = TFillStyle.HitTestFill;
end;

procedure TdxDrawingFillStyleBooleanProperties.SetHitTestFill(const AValue: Boolean);
begin
  if AValue then
    FFillStyle := FFillStyle or TFillStyle.HitTestFill
  else
    FFillStyle := FFillStyle and not TFillStyle.HitTestFill;
end;

function TdxDrawingFillStyleBooleanProperties.GetUseHitTestFill: Boolean;
begin
  Result := (FFillStyle and TFillStyle.UseHitTestFill) = TFillStyle.UseHitTestFill;
end;

procedure TdxDrawingFillStyleBooleanProperties.SetUseHitTestFill(const AValue: Boolean);
begin
  if AValue then
    FFillStyle := FFillStyle or TFillStyle.UseHitTestFill
  else
    FFillStyle := FFillStyle and not TFillStyle.UseHitTestFill;
end;

function TdxDrawingFillStyleBooleanProperties.GetFilled: Boolean;
begin
  Result := (FFillStyle and TFillStyle.Filled) = TFillStyle.Filled;
end;

procedure TdxDrawingFillStyleBooleanProperties.SetFilled(const AValue: Boolean);
begin
  if AValue then
    FFillStyle := FFillStyle or TFillStyle.Filled
  else
    FFillStyle := FFillStyle and not TFillStyle.Filled;
end;

function TdxDrawingFillStyleBooleanProperties.GetUseFilled: Boolean;
begin
  Result := (FFillStyle and TFillStyle.UseFilled) = TFillStyle.UseFilled;
end;

procedure TdxDrawingFillStyleBooleanProperties.SetUseFilled(const AValue: Boolean);
begin
  if AValue then
    FFillStyle := FFillStyle or TFillStyle.UseFilled
  else
    FFillStyle := FFillStyle and not TFillStyle.UseFilled;
end;

function TdxDrawingFillStyleBooleanProperties.GetShapeAnchor: Boolean;
begin
  Result := (FFillStyle and TFillStyle.ShapeAnchor) = TFillStyle.ShapeAnchor;
end;

procedure TdxDrawingFillStyleBooleanProperties.SetShapeAnchor(const AValue: Boolean);
begin
  if AValue then
    FFillStyle := FFillStyle or TFillStyle.ShapeAnchor
  else
    FFillStyle := FFillStyle and not TFillStyle.ShapeAnchor;
end;

function TdxDrawingFillStyleBooleanProperties.GetUseShapeAnchor: Boolean;
begin
  Result := (FFillStyle and TFillStyle.UseShapeAnchor) = TFillStyle.UseShapeAnchor;
end;

procedure TdxDrawingFillStyleBooleanProperties.SetUseShapeAnchor(const AValue: Boolean);
begin
  if AValue then
    FFillStyle := FFillStyle or TFillStyle.UseShapeAnchor
  else
    FFillStyle := FFillStyle and not TFillStyle.UseShapeAnchor;
end;

function TdxDrawingFillStyleBooleanProperties.GetRecolorFillAsPicture: Boolean;
begin
  Result := (FFillStyle and TFillStyle.RecolorFillAsPicture) = TFillStyle.RecolorFillAsPicture;
end;

procedure TdxDrawingFillStyleBooleanProperties.SetRecolorFillAsPicture(const AValue: Boolean);
begin
  if AValue then
    FFillStyle := FFillStyle or TFillStyle.RecolorFillAsPicture
  else
    FFillStyle := FFillStyle and not TFillStyle.RecolorFillAsPicture;
end;

function TdxDrawingFillStyleBooleanProperties.GetUseRecolorFillAsPicture: Boolean;
begin
  Result := (FFillStyle and TFillStyle.UseRecolorFillAsPicture) = TFillStyle.UseRecolorFillAsPicture;
end;

procedure TdxDrawingFillStyleBooleanProperties.SetUseRecolorFillAsPicture(const AValue: Boolean);
begin
  if AValue then
    FFillStyle := FFillStyle or TFillStyle.UseRecolorFillAsPicture
  else
    FFillStyle := FFillStyle and not TFillStyle.UseRecolorFillAsPicture;
end;

function TdxDrawingFillStyleBooleanProperties.GetComplex: Boolean;
begin
  Result := False;
end;

procedure TdxDrawingFillStyleBooleanProperties.Read(AReader: TBinaryReader);
begin
  inherited Read(AReader);
  FFillStyle := Value;
end;

procedure TdxDrawingFillStyleBooleanProperties.Execute(AOwner: TdxOfficeArtPropertiesBase);
var
  AArtProperties: IdxOfficeArtPropertiesBase;
begin
  if not Supports(AOwner, IdxOfficeArtPropertiesBase, AArtProperties) then
    Exit;
  AArtProperties.Filled := Filled;
  AArtProperties.UseFilled := UseFilled;
end;

procedure TdxDrawingFillStyleBooleanProperties.Write(AWriter: TBinaryWriter);
begin
  Value := FFillStyle;
  inherited Write(AWriter);
end;

{ TdxDrawingLineStyleBooleanProperties }

constructor TdxDrawingLineStyleBooleanProperties.Create;
begin
  FLineStyle := TDrawingLineStyle.UseLine;
end;

function TdxDrawingLineStyleBooleanProperties.GetComplex: Boolean;
begin
  Result := False;
end;

function TdxDrawingLineStyleBooleanProperties.GetLine: Boolean;
begin
  Result := (FLineStyle and TDrawingLineStyle.Line) = TDrawingLineStyle.Line;
end;

procedure TdxDrawingLineStyleBooleanProperties.SetLine(const AValue: Boolean);
begin
  if AValue then
    FLineStyle := FLineStyle or TDrawingLineStyle.Line
  else
    FLineStyle := FLineStyle and not TDrawingLineStyle.Line;
end;

function TdxDrawingLineStyleBooleanProperties.GetUseLine: Boolean;
begin
  Result := (FLineStyle and TDrawingLineStyle.UseLine) = TDrawingLineStyle.UseLine;
end;

procedure TdxDrawingLineStyleBooleanProperties.SetUseLine(const AValue: Boolean);
begin
  if AValue then
    FLineStyle := FLineStyle or TDrawingLineStyle.UseLine
  else
    FLineStyle := FLineStyle and not TDrawingLineStyle.UseLine;
end;

procedure TdxDrawingLineStyleBooleanProperties.Read(AReader: TBinaryReader);
begin
  inherited Read(AReader);
  FLineStyle := Value;
end;

procedure TdxDrawingLineStyleBooleanProperties.Execute(AOwner: TdxOfficeArtPropertiesBase);
var
  AArtProperties: IdxOfficeArtProperties;
begin
  if not Supports(AOwner, IdxOfficeArtProperties, AArtProperties) then
    Exit;
  AArtProperties.Line := Line;
  AArtProperties.UseLine := UseLine;
end;

procedure TdxDrawingLineStyleBooleanProperties.Write(AWriter: TBinaryWriter);
begin
  Value := FLineStyle;
  inherited Write(AWriter);
end;

{ TdxDrawingGroupShapeBooleanProperties }

constructor TdxDrawingGroupShapeBooleanProperties.Create;
begin
  Value := CUseHidden or CUseBehindDocument;
end;

function TdxDrawingGroupShapeBooleanProperties.GetComplex: Boolean;
begin
  Result := False;
end;

function TdxDrawingGroupShapeBooleanProperties.GetPrint: Boolean;
begin
  Result := GetFlag(CPrint);
end;

procedure TdxDrawingGroupShapeBooleanProperties.SetPrint(const AValue: Boolean);
begin
  SetFlag(CPrint, AValue);
end;

function TdxDrawingGroupShapeBooleanProperties.GetHidden: Boolean;
begin
  Result := GetFlag(CHidden);
end;

procedure TdxDrawingGroupShapeBooleanProperties.SetHidden(const AValue: Boolean);
begin
  SetFlag(CHidden, AValue);
end;

function TdxDrawingGroupShapeBooleanProperties.GetIsBehindDoc: Boolean;
begin
  Result := GetFlag(CBehindDocument);
end;

procedure TdxDrawingGroupShapeBooleanProperties.SetIsBehindDoc(const AValue: Boolean);
begin
  SetFlag(CBehindDocument, AValue);
end;

function TdxDrawingGroupShapeBooleanProperties.GetLayoutInCell: Boolean;
begin
  Result := GetFlag(CLayoutInCell);
end;

procedure TdxDrawingGroupShapeBooleanProperties.SetLayoutInCell(const AValue: Boolean);
begin
  SetFlag(CLayoutInCell, AValue);
end;

function TdxDrawingGroupShapeBooleanProperties.GetUsePrint: Boolean;
begin
  Result := GetFlag(CUsePrint);
end;

procedure TdxDrawingGroupShapeBooleanProperties.SetUsePrint(const AValue: Boolean);
begin
  SetFlag(CUsePrint, AValue);
end;

function TdxDrawingGroupShapeBooleanProperties.GetUseHidden: Boolean;
begin
  Result := GetFlag(CUseHidden);
end;

procedure TdxDrawingGroupShapeBooleanProperties.SetUseHidden(const AValue: Boolean);
begin
  SetFlag(CUseHidden, AValue);
end;

function TdxDrawingGroupShapeBooleanProperties.GetUseBehindDocument: Boolean;
begin
  Result := GetFlag(CUseBehindDocument);
end;

procedure TdxDrawingGroupShapeBooleanProperties.SetUseBehindDocument(const AValue: Boolean);
begin
  SetFlag(CUseBehindDocument, AValue);
end;

function TdxDrawingGroupShapeBooleanProperties.GetUseLayoutInCell: Boolean;
begin
  Result := GetFlag(CUseLayoutInCell);
end;

procedure TdxDrawingGroupShapeBooleanProperties.SetUseLayoutInCell(const AValue: Boolean);
begin
  SetFlag(CUseLayoutInCell, AValue);
end;

procedure TdxDrawingGroupShapeBooleanProperties.Execute(AOwner: TdxOfficeArtPropertiesBase);
var
  AArtProperties: IdxOfficeArtPropertiesBase;
begin
  if not Supports(AOwner, IdxOfficeArtPropertiesBase, AArtProperties) then
    Exit;
  AArtProperties.IsBehindDoc := IsBehindDoc;
  AArtProperties.UseIsBehindDoc := IsBehindDoc;
  AArtProperties.LayoutInCell := LayoutInCell;
  AArtProperties.UseLayoutInCell := UseLayoutInCell;
end;

{ TdxDrawingShadowStyleBooleanProperties }

function TdxDrawingShadowStyleBooleanProperties.GetComplex: Boolean;
begin
  Result := False;
end;

function TdxDrawingShadowStyleBooleanProperties.GetShadowObscured: Boolean;
begin
  Result := GetFlag(CShadowObscured);
end;

procedure TdxDrawingShadowStyleBooleanProperties.SetShadowObscured(const AValue: Boolean);
begin
  SetFlag(CShadowObscured, AValue);
end;

function TdxDrawingShadowStyleBooleanProperties.GetShadow: Boolean;
begin
  Result := GetFlag(CShadow);
end;

procedure TdxDrawingShadowStyleBooleanProperties.SetShadow(const AValue: Boolean);
begin
  SetFlag(CShadow, AValue);
end;

function TdxDrawingShadowStyleBooleanProperties.GetUseShadowObscured: Boolean;
begin
  Result := GetFlag(CUseShadowObscured);
end;

procedure TdxDrawingShadowStyleBooleanProperties.SetUseShadowObscured(const AValue: Boolean);
begin
  SetFlag(CUseShadowObscured, AValue);
end;

function TdxDrawingShadowStyleBooleanProperties.GetUseShadow: Boolean;
begin
  Result := GetFlag(CUseShadow);
end;

procedure TdxDrawingShadowStyleBooleanProperties.SetUseShadow(const AValue: Boolean);
begin
  SetFlag(CUseShadow, AValue);
end;

{ TdxDrawingShapeBooleanProperties }

constructor TdxDrawingShapeBooleanProperties.Create;
begin
  Value := CBackground or CUseBackground;
end;

function TdxDrawingShapeBooleanProperties.GetComplex: Boolean;
begin
  Result := False;
end;

function TdxDrawingShapeBooleanProperties.GetIsBackground: Boolean;
begin
  Result := GetFlag(CBackground);
end;

procedure TdxDrawingShapeBooleanProperties.SetIsBackground(const AValue: Boolean);
begin
  SetFlag(CBackground, AValue);
end;

function TdxDrawingShapeBooleanProperties.GetLockShapeType: Boolean;
begin
  Result := GetFlag(CLockShapeType);
end;

procedure TdxDrawingShapeBooleanProperties.SetLockShapeType(const AValue: Boolean);
begin
  SetFlag(CLockShapeType, AValue);
end;

function TdxDrawingShapeBooleanProperties.GetPreferRelativeResize: Boolean;
begin
  Result := GetFlag(CPreferRelativeResize);
end;

procedure TdxDrawingShapeBooleanProperties.SetPreferRelativeResize(const AValue: Boolean);
begin
  SetFlag(CPreferRelativeResize, AValue);
end;

function TdxDrawingShapeBooleanProperties.GetFlipVOverride: Boolean;
begin
  Result := GetFlag(CFlipVOverride);
end;

procedure TdxDrawingShapeBooleanProperties.SetFlipVOverride(const AValue: Boolean);
begin
  SetFlag(CFlipVOverride, AValue);
end;

function TdxDrawingShapeBooleanProperties.GetFlipHOverride: Boolean;
begin
  Result := GetFlag(CFlipHOverride);
end;

procedure TdxDrawingShapeBooleanProperties.SetFlipHOverride(const AValue: Boolean);
begin
  SetFlag(CFlipHOverride, AValue);
end;

function TdxDrawingShapeBooleanProperties.GetUseBackground: Boolean;
begin
  Result := GetFlag(CUseBackground);
end;

procedure TdxDrawingShapeBooleanProperties.SetUseBackground(const AValue: Boolean);
begin
  SetFlag(CUseBackground, AValue);
end;

function TdxDrawingShapeBooleanProperties.GetUseLockShapeType: Boolean;
begin
  Result := GetFlag(CUseLockShapeType);
end;

procedure TdxDrawingShapeBooleanProperties.SetUseLockShapeType(const AValue: Boolean);
begin
  SetFlag(CUseLockShapeType, AValue);
end;

function TdxDrawingShapeBooleanProperties.GetUsePreferRelativeResize: Boolean;
begin
  Result := GetFlag(CUsePreferRelativeResize);
end;

procedure TdxDrawingShapeBooleanProperties.SetUsePreferRelativeResize(const AValue: Boolean);
begin
  SetFlag(CUsePreferRelativeResize, AValue);
end;

function TdxDrawingShapeBooleanProperties.GetUseFlipVOverride: Boolean;
begin
  Result := GetFlag(CUseFlipVOverride);
end;

procedure TdxDrawingShapeBooleanProperties.SetUseFlipVOverride(const AValue: Boolean);
begin
  SetFlag(CUseFlipVOverride, AValue);
end;

function TdxDrawingShapeBooleanProperties.GetUseFlipHOverride: Boolean;
begin
  Result := GetFlag(CUseFlipHOverride);
end;

procedure TdxDrawingShapeBooleanProperties.SetUseFlipHOverride(const AValue: Boolean);
begin
  SetFlag(CUseFlipHOverride, AValue);
end;

{ TdxOfficeDrawingStringPropertyValueBase }

function TdxOfficeDrawingStringPropertyValueBase.GetComplex: Boolean;
begin
  Result := True;
end;

function TdxOfficeDrawingStringPropertyValueBase.GetData: string;
begin
  Result := GetStringData;
end;

procedure TdxOfficeDrawingStringPropertyValueBase.SetData(const AValue: string);
begin
  SetStringData(AValue);
end;

function TdxOfficeDrawingStringPropertyValueBase.GetStringData: string;
begin
  if FData = '' then
    FData := TdxEncoding.Unicode.GetString(ComplexData, 0, Length(ComplexData));
  Result := FData;
end;

procedure TdxOfficeDrawingStringPropertyValueBase.SetStringData(const AValue: string);
begin
  if FData = AValue then
    Exit;
  FData := AValue;
  SetComplexData(TdxEncoding.Unicode.GetBytes(AValue));
  Value := Length(ComplexData);
end;

{ TdxDrawingTextBooleanProperties }

constructor TdxDrawingTextBooleanProperties.Create;
begin
  Value := CAutoTextMargins or CUseAutoTextMargins;
end;

function TdxDrawingTextBooleanProperties.GetComplex: Boolean;
begin
  Result := False;
end;

function TdxDrawingTextBooleanProperties.GetFitShapeToText: Boolean;
begin
  Result := GetFlag(CFitShapeToText);
end;

procedure TdxDrawingTextBooleanProperties.SetFitShapeToText(const AValue: Boolean);
begin
  SetFlag(CFitShapeToText, AValue);
end;

function TdxDrawingTextBooleanProperties.GetAutoTextMargins: Boolean;
begin
  Result := GetFlag(CAutoTextMargins);
end;

procedure TdxDrawingTextBooleanProperties.SetAutoTextMargins(const AValue: Boolean);
begin
  SetFlag(CAutoTextMargins, AValue);
end;

function TdxDrawingTextBooleanProperties.GetSelectText: Boolean;
begin
  Result := GetFlag(CSelectText);
end;

procedure TdxDrawingTextBooleanProperties.SetSelectText(const AValue: Boolean);
begin
  SetFlag(CSelectText, AValue);
end;

function TdxDrawingTextBooleanProperties.GetUseFitShapeToText: Boolean;
begin
  Result := GetFlag(CUseFitShapeToText);
end;

procedure TdxDrawingTextBooleanProperties.SetUseFitShapeToText(const AValue: Boolean);
begin
  SetFlag(CUseFitShapeToText, AValue);
end;

function TdxDrawingTextBooleanProperties.GetUseAutoTextMargins: Boolean;
begin
  Result := GetFlag(CUseAutoTextMargins);
end;

procedure TdxDrawingTextBooleanProperties.SetUseAutoTextMargins(const AValue: Boolean);
begin
  SetFlag(CUseAutoTextMargins, AValue);
end;

function TdxDrawingTextBooleanProperties.GetUseSelectText: Boolean;
begin
  Result := GetFlag(CUseSelectText);
end;

procedure TdxDrawingTextBooleanProperties.SetUseSelectText(const AValue: Boolean);
begin
  SetFlag(CUseSelectText, AValue);
end;

procedure TdxDrawingTextBooleanProperties.Execute(AOwner: TdxOfficeArtPropertiesBase);
var
  AArtProperties: IdxOfficeArtProperties;
begin
  if not Supports(AOwner, IdxOfficeArtProperties, AArtProperties) then
    Exit;
  AArtProperties.UseFitShapeToText := UseFitShapeToText;
  AArtProperties.FitShapeToText := FitShapeToText;
end;

{ TdxDrawingGroupShape2PctHorizPos }

constructor TdxDrawingGroupShape2PctHorizPos.Create(AValue: Integer);
begin
  Value := AValue;
end;

constructor TdxDrawingGroupShape2PctHorizPos.Create;
begin
  Create(Integer($FFFFD8EF));
end;

procedure TdxDrawingGroupShape2PctHorizPos.Execute(AOwner: TdxOfficeArtPropertiesBase);
var
  AArtProperties: IdxOfficeArtTertiaryProperties;
begin
  inherited Execute(AOwner);
  if Supports(AOwner, IdxOfficeArtTertiaryProperties, AArtProperties) then
    AArtProperties.PctHorizPos := Value;
end;

{ TdxDrawingGroupShape2PctVertPos }

constructor TdxDrawingGroupShape2PctVertPos.Create(AValue: Integer);
begin
  Value := AValue;
end;

constructor TdxDrawingGroupShape2PctVertPos.Create;
begin
  Create(Integer($FFFFD8EF));
end;

procedure TdxDrawingGroupShape2PctVertPos.Execute(AOwner: TdxOfficeArtPropertiesBase);
var
  AArtProperties: IdxOfficeArtTertiaryProperties;
begin
  inherited Execute(AOwner);
  if Supports(AOwner, IdxOfficeArtTertiaryProperties, AArtProperties) then
    AArtProperties.PctVertPos := Value;
end;

{ TdxDrawingGroupShape2PctHoriz }

constructor TdxDrawingGroupShape2PctHoriz.Create(AValue: Integer);
begin
  Value := AValue;
end;

constructor TdxDrawingGroupShape2PctHoriz.Create;
begin
  Create(0)
end;

procedure TdxDrawingGroupShape2PctHoriz.Execute(AOwner: TdxOfficeArtPropertiesBase);
var
  AArtProperties: IdxOfficeArtTertiaryProperties;
begin
  inherited Execute(AOwner);
  if not Supports(AOwner, IdxOfficeArtTertiaryProperties, AArtProperties) then
    Exit;
  if not AArtProperties.UseRelativeWidth then
    AArtProperties.SizeRelH := TdxDrawingGroupShape2SizeRelH.TRelativeFrom.Page;
  AArtProperties.UseRelativeWidth := True;
  AArtProperties.PctHoriz := Value;
end;

{ TdxDrawingGroupShape2PctVert }

constructor TdxDrawingGroupShape2PctVert.Create(AValue: Integer);
begin
  Value := AValue;
end;

constructor TdxDrawingGroupShape2PctVert.Create;
begin
  Create(0);
end;

procedure TdxDrawingGroupShape2PctVert.Execute(AOwner: TdxOfficeArtPropertiesBase);
var
  AArtProperties: IdxOfficeArtTertiaryProperties;
begin
  inherited Execute(AOwner);
  if not Supports(AOwner, IdxOfficeArtTertiaryProperties, AArtProperties) then
    Exit;
  if not AArtProperties.UseRelativeHeight then
    AArtProperties.SizeRelV := TdxDrawingGroupShape2SizeRelV.TRelativeFrom.Page;
  AArtProperties.UseRelativeHeight := True;
  AArtProperties.PctVert := Value;
end;

{ TdxDiagramBooleanProperties }

constructor TdxDiagramBooleanProperties.Create;
begin
  Value := DefaultFlags;
end;

{ TdxDrawingWrapLeftDistance }

constructor TdxDrawingWrapLeftDistance.Create(AValue: Integer);
begin
  Value := AValue;
end;

constructor TdxDrawingWrapLeftDistance.Create;
begin
  Create(DefaultValue);
end;

function TdxDrawingWrapLeftDistance.GetComplex: Boolean;
begin
  Result := False;
end;

procedure TdxDrawingWrapLeftDistance.Execute(AOwner: TdxOfficeArtPropertiesBase);
var
  AArtProperties: IdxOfficeArtProperties;
begin
  if not Supports(AOwner, IdxOfficeArtPropertiesBase, AArtProperties) then
    Exit;
  AArtProperties.WrapLeftDistance := Value;
  AArtProperties.UseWrapLeftDistance := True;
end;

{ TdxDrawingWrapRightDistance }

constructor TdxDrawingWrapRightDistance.Create(AValue: Integer);
begin
  Value := AValue;
end;

constructor TdxDrawingWrapRightDistance.Create;
begin
  Create(DefaultValue);
end;

function TdxDrawingWrapRightDistance.GetComplex: Boolean;
begin
  Result := False;
end;

procedure TdxDrawingWrapRightDistance.Execute(AOwner: TdxOfficeArtPropertiesBase);
var
  AArtProperties: IdxOfficeArtProperties;
begin
  if not Supports(AOwner, IdxOfficeArtPropertiesBase, AArtProperties) then
    Exit;
  AArtProperties.WrapRightDistance := Value;
  AArtProperties.UseWrapRightDistance := True;
end;

{ TdxDrawingWrapTopDistance }

constructor TdxDrawingWrapTopDistance.Create(AValue: Integer);
begin
  Value := AValue;
end;

constructor TdxDrawingWrapTopDistance.Create;
begin
  Create(DefaultValue);
end;

function TdxDrawingWrapTopDistance.GetComplex: Boolean;
begin
  Result := False;
end;

procedure TdxDrawingWrapTopDistance.Execute(AOwner: TdxOfficeArtPropertiesBase);
var
  AArtProperties: IdxOfficeArtProperties;
begin
  if not Supports(AOwner, IdxOfficeArtPropertiesBase, AArtProperties) then
    Exit;
  AArtProperties.UseWrapTopDistance := True;
  AArtProperties.WrapTopDistance := Value;
end;

{ TdxDrawingWrapBottomDistance }

constructor TdxDrawingWrapBottomDistance.Create(AValue: Integer);
begin
  Value := AValue;
end;

constructor TdxDrawingWrapBottomDistance.Create;
begin
  Create(DefaultValue);
end;

function TdxDrawingWrapBottomDistance.GetComplex: Boolean;
begin
  Result := False;
end;

procedure TdxDrawingWrapBottomDistance.Execute(AOwner: TdxOfficeArtPropertiesBase);
var
  AArtProperties: IdxOfficeArtProperties;
begin
  if not Supports(AOwner, IdxOfficeArtPropertiesBase, AArtProperties) then
    Exit;
  AArtProperties.WrapBottomDistance := Value;
  AArtProperties.UseWrapBottomDistance := True;
end;

{ TdxBlipsWithProperties }

constructor TdxBlipsWithProperties.Create;
begin
  FBlips := TObjectDictionary<Integer, TdxBlipBase>.Create([{doOwnsValues}]);
  FShapeArtProperties := TObjectDictionary<Integer, TdxOfficeArtProperties>.Create([{doOwnsValues}]);
  FShapeArtTertiaryProperties := TObjectDictionary<Integer, TdxOfficeArtTertiaryProperties>.Create([{doOwnsValues}]);
  FShapeRecords := TObjectDictionary<Integer, TdxOfficeArtShapeRecord>.Create([{doOwnsValues}]);
end;

destructor TdxBlipsWithProperties.Destroy;
begin
  FBlips.Free;
  FShapeArtProperties.Free;
  FShapeArtTertiaryProperties.Free;
  FShapeRecords.Free;
  inherited Destroy;
end;

{ TdxOfficeArtShapeGroupContainer }

constructor TdxOfficeArtShapeGroupContainer.Create(ALocation: Byte);
begin
  inherited Create;
  Items.Add(TdxOfficeArtTopmostShapeContainer.Create(ALocation));
end;

class function TdxOfficeArtShapeGroupContainer.FromStream(AReader: TBinaryReader; AHeader: TdxOfficeArtRecordHeader): TdxOfficeArtShapeGroupContainer;
begin
  Result := TdxOfficeArtShapeGroupContainer.Create;
  Result.Read(AReader, AHeader);
end;

function TdxOfficeArtShapeGroupContainer.GetHeaderInstanceInfo: Integer;
begin
  Result := TdxOfficeArtHeaderInfos.EmptyHeaderInfo;
end;

function TdxOfficeArtShapeGroupContainer.GetHeaderTypeCode: Integer;
begin
  Result := TdxOfficeArtTypeCodes.ShapeGroupContainer;
end;

function TdxOfficeArtShapeGroupContainer.GetHeaderVersion: Integer;
begin
  Result := TdxOfficeArtVersions.DefaultHeaderVersion;
end;

procedure TdxOfficeArtShapeGroupContainer.Read(AReader: TBinaryReader; AHeader: TdxOfficeArtRecordHeader);
var
  AEndPosition: Int64;
begin
  AEndPosition := AReader.BaseStream.Position + AHeader.Length;
  while AReader.BaseStream.Position < AEndPosition do
    Items.Add(CreateShapeContainer(AReader));
end;

function TdxOfficeArtShapeGroupContainer.CreateShapeContainer(AReader: TBinaryReader): TdxOfficeDrawingPartBase;
var
  AShapeHeader: TdxOfficeArtRecordHeader;
  ATypeCode: Integer;
begin
  AShapeHeader := TdxOfficeArtRecordHeader.FromStream(AReader);
  try
    ATypeCode := AShapeHeader.TypeCode;
    if ATypeCode = TdxOfficeArtTypeCodes.ShapeGroupContainer then
      Exit(TdxOfficeArtShapeGroupContainer.FromStream(AReader, AShapeHeader));
    if ATypeCode = TdxOfficeArtTypeCodes.ShapeContainer then
      Exit(TdxOfficeArtShapeContainer.FromStream(AReader, AShapeHeader));
  finally
    AShapeHeader.Free;
  end;
  TdxOfficeArtExceptions.ThrowInvalidContent;
  Result := nil;
end;

{ TdxOfficeArtShapeGroupCoordinateSystem }

function TdxOfficeArtShapeGroupCoordinateSystem.GetHeaderInstanceInfo: Integer;
begin
  Result := TdxOfficeArtHeaderInfos.EmptyHeaderInfo;
end;

function TdxOfficeArtShapeGroupCoordinateSystem.GetHeaderTypeCode: Integer;
begin
  Result := TdxOfficeArtTypeCodes.ShapeGroupCoordinateSystem;
end;

function TdxOfficeArtShapeGroupCoordinateSystem.GetHeaderVersion: Integer;
begin
  Result := TdxOfficeArtVersions.ShapeGroupCoordinateSystemVersion;
end;

procedure TdxOfficeArtShapeGroupCoordinateSystem.WriteCore(AWriter: TBinaryWriter);
begin
  AWriter.Write(Left);
  AWriter.Write(Top);
  AWriter.Write(Right);
  AWriter.Write(Bottom);
end;

function TdxOfficeArtShapeGroupCoordinateSystem.GetSize: Integer;
begin
  Result := RecordLength;
end;

{ TdxOfficeArtTopmostShapeContainer }

constructor TdxOfficeArtTopmostShapeContainer.Create(ALocation: Byte);
begin
  FCoordinateSystem := TdxOfficeArtShapeGroupCoordinateSystem.Create;
  InitializeShapeRecord(ALocation);
end;

destructor TdxOfficeArtTopmostShapeContainer.Destroy;
begin
  FShapeRecord.Free;
  FCoordinateSystem.Free;
  inherited Destroy;
end;

function TdxOfficeArtTopmostShapeContainer.GetHeaderInstanceInfo: Integer;
begin
  Result := TdxOfficeArtHeaderInfos.EmptyHeaderInfo;
end;

function TdxOfficeArtTopmostShapeContainer.GetHeaderTypeCode: Integer;
begin
  Result := TdxOfficeArtTypeCodes.ShapeContainer;
end;

function TdxOfficeArtTopmostShapeContainer.GetHeaderVersion: Integer;
begin
  Result := TdxOfficeArtVersions.DefaultHeaderVersion;
end;

procedure TdxOfficeArtTopmostShapeContainer.InitializeShapeRecord(ALocation: Byte);
begin
  FShapeRecord := TdxOfficeArtShapeRecord.Create(TdxOfficeArtHeaderInfos.NotPrimitiveShape);
  if (ALocation = TdxOfficeArtConstants.MainDocumentDrawingLocation) then
    FShapeRecord.ShapeIdentifier := MainTopmostShapeId
  else
    FShapeRecord.ShapeIdentifier := HeaderTopmostShapeId;
  FShapeRecord.Flags := TopmostShapeFlags;
end;

procedure TdxOfficeArtTopmostShapeContainer.WriteCore(AWriter: TBinaryWriter);
begin
  CoordinateSystem.Write(AWriter);
  ShapeRecord.Write(AWriter);
end;

function TdxOfficeArtTopmostShapeContainer.GetSize: Integer;
begin
  Result := RecordLength;
end;

{ TdxOfficeArtDrawingObjectsContainerBase }

constructor TdxOfficeArtDrawingObjectsContainerBase.Create(ALocation: Byte; ADrawingId, AShapeIdentifier: Integer);
begin
  inherited Create;
  FDrawingData := TdxOfficeArtFileDrawingRecord.Create(ADrawingId);
  FShapeGroup := TdxOfficeArtShapeGroupContainer.Create(ALocation);
  Items.Add(FDrawingData);
  Items.Add(FShapeGroup);
end;

constructor TdxOfficeArtDrawingObjectsContainerBase.Create;
begin
  CreateGarbageCollector;
end;

function TdxOfficeArtDrawingObjectsContainerBase.GetHeaderInstanceInfo: Integer;
begin
  Result := TdxOfficeArtHeaderInfos.EmptyHeaderInfo;
end;

function TdxOfficeArtDrawingObjectsContainerBase.GetHeaderTypeCode: Integer;
begin
  Result := TdxOfficeArtTypeCodes.DrawingObjectsContainer;
end;

function TdxOfficeArtDrawingObjectsContainerBase.GetHeaderVersion: Integer;
begin
  Result := TdxOfficeArtVersions.DefaultHeaderVersion;
end;

procedure TdxOfficeArtDrawingObjectsContainerBase.CheckHeader(AHeader: TdxOfficeArtRecordHeader);
begin
  if (AHeader.Version <> TdxOfficeArtVersions.DefaultHeaderVersion) or
     (AHeader.InstanceInfo <> TdxOfficeArtHeaderInfos.EmptyHeaderInfo) or (AHeader.TypeCode <> TdxOfficeArtTypeCodes.DrawingObjectsContainer) then
    TdxOfficeArtExceptions.ThrowInvalidContent;
end;

procedure TdxOfficeArtDrawingObjectsContainerBase.Read(AReader: TBinaryReader);
var
  AHeader: TdxOfficeArtRecordHeader;
  AEndPosition: Int64;
begin
  AHeader := TdxOfficeArtRecordHeader.FromStream(AReader);
  try
    CheckHeader(AHeader);
    AEndPosition := AReader.BaseStream.Position + AHeader.Length;
    while AReader.BaseStream.Position < AEndPosition do
      ParseHeader(AReader);
  finally
    AHeader.Free;
  end;
end;

procedure TdxOfficeArtDrawingObjectsContainerBase.ParseHeader(AReader: TBinaryReader);
var
  AHeader: TdxOfficeArtRecordHeader;
begin
  AHeader := TdxOfficeArtRecordHeader.FromStream(AReader);
  try
    if not TryRead(AReader, AHeader) then
      AReader.BaseStream.Seek(AHeader.Length, TSeekOrigin.soCurrent);
  finally
    AHeader.Free;
  end;
end;

function TdxOfficeArtDrawingObjectsContainerBase.TryRead(AReader: TBinaryReader; AHeader: TdxOfficeArtRecordHeader): Boolean;
begin
  if AHeader.TypeCode = TdxOfficeArtTypeCodes.FileDrawingGroupRecord then
  begin
    FDrawingData := TdxOfficeArtFileDrawingRecord.FromStream(AReader, AHeader);
    ObjectsToDelete.Add(FDrawingData);
    Exit(True);
  end;
  if AHeader.TypeCode = TdxOfficeArtTypeCodes.ShapeGroupContainer then
  begin
    FShapeGroup := TdxOfficeArtShapeGroupContainer.FromStream(AReader, AHeader);
    ObjectsToDelete.Add(FShapeGroup);
    Exit(True);
  end;
  Result := False;
end;

{ TdxOfficeArtWordDrawing }

constructor TdxOfficeArtWordDrawing.Create(ALocation: Byte; ADrawingId: Integer; AShapeId: Integer);
begin
  FLocation := ALocation;
  FDrawingContainer := CreateDrawingObjectsContainer(ADrawingId, AShapeId);
end;

constructor TdxOfficeArtWordDrawing.Create;
begin
end;

destructor TdxOfficeArtWordDrawing.Destroy;
begin
  FDrawingContainer.Free;
  inherited Destroy;
end;

class function TdxOfficeArtWordDrawing.FromStream(AReader: TBinaryReader): TdxOfficeArtWordDrawing;
begin
  Result := TdxOfficeArtWordDrawing.Create;
  Result.Read(AReader);
end;

function TdxOfficeArtWordDrawing.CreateDrawingObjectsContainer(ADrawingId: Integer; AShapeId: Integer): TdxOfficeArtDrawingObjectsContainerBase;
begin
  if FLocation = TdxOfficeArtConstants.MainDocumentDrawingLocation then
    Result := TdxOfficeArtDrawingObjectsContainerSupprortsBackgroundShape.Create(FLocation, ADrawingId, AShapeId)
  else
    Result := TdxOfficeArtDrawingObjectsContainer.Create(FLocation, ADrawingId, AShapeId);
end;

procedure TdxOfficeArtWordDrawing.Read(AReader: TBinaryReader);
begin
  FLocation := AReader.ReadByte;
  FDrawingContainer.Free;
  FDrawingContainer := ReadDrawingObjectsContainer(AReader);
end;

function TdxOfficeArtWordDrawing.ReadDrawingObjectsContainer(AReader: TBinaryReader): TdxOfficeArtDrawingObjectsContainerBase;
begin
  if FLocation = TdxOfficeArtConstants.MainDocumentDrawingLocation then
    Result := TdxOfficeArtDrawingObjectsContainerSupprortsBackgroundShape.FromStream(AReader)
  else
    Result := TdxOfficeArtDrawingObjectsContainer.FromStream(AReader);
end;

procedure TdxOfficeArtWordDrawing.Write(AWriter: TBinaryWriter);
begin
  AWriter.Write(Location);
  DrawingObjectsContainer.Write(AWriter);
end;

{ TdxOfficeArtFloatingShapeContainer }

constructor TdxOfficeArtFloatingShapeContainer.Create;
begin
  inherited Create;
  Items.Add(TdxOfficeClientAnchor.Create);
  Items.Add(TdxOfficeClientData.Create);
end;

{ TdxOfficeArtContent.TOfficeArtContentHelper }

constructor TdxOfficeArtContent.TOfficeArtContentHelper.Create(AContent: TdxOfficeArtContent);
begin
  FContent := AContent;
end;

procedure TdxOfficeArtContent.TOfficeArtContentHelper.Traverse;
var
  ACount, I: Integer;
  ADrawing: TdxOfficeArtWordDrawing;
begin
  if FContent.DrawingContainer.BlipContainer = nil then
    Exit;
  ACount := FContent.Drawings.Count;
  for I := 0 to ACount - 1 do
  begin
    ADrawing := FContent.Drawings[I];
    if (ADrawing.Location = TdxOfficeArtConstants.MainDocumentDrawingLocation) then
      FActiveShapes := FContent.MainDocumentBlips
    else
      FActiveShapes := FContent.HeadersBlips;
    Process(ADrawing.DrawingObjectsContainer.ShapeGroup.Items);
  end;
end;

procedure TdxOfficeArtContent.TOfficeArtContentHelper.Process(AShapes: TdxList<TdxOfficeDrawingPartBase>);
var
  ACount, I, AShapeBlipIndex: Integer;
  AShape: TdxOfficeArtShapeContainer;
  ABlips: TdxList<TdxBlipBase>;
  AShapeGroup: TdxOfficeArtShapeGroupContainer;
begin
  ACount := AShapes.Count;
  for I := 0 to ACount - 1 do
  begin
    if AShapes[I] is TdxOfficeArtShapeContainer then
    begin
      AShape := Safe<TdxOfficeArtShapeContainer>.Cast(AShapes[I]);
      ABlips := FContent.DrawingContainer.BlipContainer.Blips;
      AShapeBlipIndex := AShape.ArtProperties.BlipIndex;

      if (AShapeBlipIndex > 0) and (AShapeBlipIndex <= ABlips.Count) then
        FActiveShapes.Blips.Add(AShape.ShapeRecord.ShapeIdentifier, ABlips[AShapeBlipIndex - 1]);
      FActiveShapes.ShapeArtProperties.Add(AShape.ShapeRecord.ShapeIdentifier, AShape.ArtProperties);
      FActiveShapes.ShapeArtTertiaryProperties.Add(AShape.ShapeRecord.ShapeIdentifier, AShape.ArtTertiaryProperties);
      FActiveShapes.ShapeRecords.Add(AShape.ShapeRecord.ShapeIdentifier, AShape.ShapeRecord);

      Continue;
    end;
    if AShapes[I] is TdxOfficeArtShapeGroupContainer then
    begin
      AShapeGroup := Safe<TdxOfficeArtShapeGroupContainer>.Cast(AShapes[I]);
      Process(AShapeGroup.Items);
    end;
  end;
end;

{ TdxOfficeArtContent }

constructor TdxOfficeArtContent.Create;
begin
  FDrawingContainer := TdxOfficeArtDrawingContainer.Create;
  FMainDocumentBlips := TdxBlipsWithProperties.Create;
  FHeadersBlips := TdxBlipsWithProperties.Create;
  FContentHelper := TOfficeArtContentHelper.Create(Self);
  FDrawings := TdxObjectList<TdxOfficeArtWordDrawing>.Create;
end;

destructor TdxOfficeArtContent.Destroy;
begin
  FDrawingContainer.Free;
  FMainDocumentBlips.Free;
  FHeadersBlips.Free;
  FContentHelper.Free;
  FDrawings.Free;
  inherited Destroy;
end;

class function TdxOfficeArtContent.FromStream(AReader, AEmbeddedReader: TBinaryReader;
  AOffset, ASize: Integer; const AImageCreator: IdxDocOfficeImageCreator): TdxOfficeArtContent;
begin
  Result := TdxOfficeArtContent.Create;
  Result.Read(AReader, AEmbeddedReader, AOffset, ASize, AImageCreator);
end;

function TdxOfficeArtContent.GetMainDocumentDrawing: TdxOfficeArtWordDrawing;
begin
  if FMainDocumentDrawing = nil then
    InitMainDocumentDrawing;
  Result := FMainDocumentDrawing;
end;

function TdxOfficeArtContent.GetHeaderDrawing: TdxOfficeArtWordDrawing;
begin
  if FMainDocumentDrawing = nil then
    InitMainDocumentDrawing;
  if FHeaderDrawing = nil then
    InitHeaderDrawing;
  Result := FHeaderDrawing;
end;

procedure TdxOfficeArtContent.InitMainDocumentDrawing;
begin
  FMainDocumentDrawing := TdxOfficeArtWordDrawing.Create(MainDocumentDrawingLocation,
    Drawings.Count + 1, TdxOfficeArtConstants.DefaultMainDocumentShapeIdentifier);
  Drawings.Add(FMainDocumentDrawing);
end;

procedure TdxOfficeArtContent.InitHeaderDrawing;
begin
  FHeaderDrawing := TdxOfficeArtWordDrawing.Create(HeaderDocumentDrawingLocation,
    Drawings.Count + 1, TdxOfficeArtConstants.DefaultHeaderShapeIdentifier);
  Drawings.Add(FHeaderDrawing);
end;

procedure TdxOfficeArtContent.SetPageBackColor(APageBackColor: TdxAlphaColor);
begin
  MainDocumentDrawing.DrawingObjectsContainer.BackgroundShape.ArtProperties.FillColor := APageBackColor;
end;

procedure TdxOfficeArtContent.InsertPictureFloatingObject(AImage: TdxOfficeImageReference; AState: TdxDocContentState;
  AShapeIdentifier: Integer; ARun: TdxFloatingObjectAnchorRun; AUnitConverter: TdxDocumentModelUnitConverter);
begin
  if (AState <> TdxDocContentState.MainDocument) and (AState <> TdxDocContentState.HeadersFootersStory) then
    Exit;
  InsertPictureFloatingObjectCore(AImage, AState, AShapeIdentifier, ARun, AUnitConverter);
end;

procedure TdxOfficeArtContent.InsertTextBoxFloatingObject(AState: TdxDocContentState;
  AShapeIdentifier, ATextIdentifier: Integer; ARun: TdxFloatingObjectAnchorRun; AUnitConverter: TdxDocumentModelUnitConverter);
begin
  if (AState <> TdxDocContentState.MainDocument) and (AState <> TdxDocContentState.HeadersFootersStory) then
    Exit;
  InsertTextBoxFloatingObjectCore(AState, AShapeIdentifier, ATextIdentifier, ARun, AUnitConverter);
end;

procedure TdxOfficeArtContent.InsertPictureFloatingObjectCore(AImage: TdxOfficeImageReference; AState: TdxDocContentState;
  AShapeIdentifier: Integer; ARun: TdxFloatingObjectAnchorRun; AUnitConverter: TdxDocumentModelUnitConverter);
var
  AShapeContainer: TdxOfficeArtFloatingShapeContainer;
begin
  AShapeContainer := CreateShapeContainer(AState, AShapeIdentifier);
  DrawingContainer.BlipContainer.Blips.Add(TdxFileBlipStoreEntry.Create(AImage, True));

  AShapeContainer.ArtProperties.BlipIndex := DrawingContainer.BlipContainer.Blips.Count;
  InitializeArtProperties(AShapeContainer.ArtProperties, ARun, AUnitConverter);
  ApplyFloatingObjectDistancesProperties(AShapeContainer.ArtProperties, AShapeContainer.ArtTertiaryProperties, ARun.FloatingObjectProperties);
  AShapeContainer.ArtProperties.CreateProperties;
  AShapeContainer.ArtTertiaryProperties.CreateProperties;
end;

procedure TdxOfficeArtContent.InitializeArtProperties(AArtProperties: TdxOfficeArtProperties;
  ARun: TdxFloatingObjectAnchorRun; AUnitConverter: TdxDocumentModelUnitConverter);
begin
  AArtProperties.Shape := ARun.Shape;
  AArtProperties.FloatingObjectProperties := ARun.FloatingObjectProperties;
  AArtProperties.UnitConverter := AUnitConverter;
end;

procedure TdxOfficeArtContent.InsertTextBoxFloatingObjectCore(AState: TdxDocContentState;
  AShapeIdentifier, ATextIdentifier: Integer; ARun: TdxFloatingObjectAnchorRun; AUnitConverter: TdxDocumentModelUnitConverter);
var
  AShapeContainer: TdxOfficeArtFloatingShapeContainer;
  ATextBoxContent: TdxTextBoxFloatingObjectContent;
begin
  AShapeContainer := CreateShapeContainer(AState, AShapeIdentifier);

  AShapeContainer.ArtProperties.TextIndex := ATextIdentifier;
  InitializeArtProperties(AShapeContainer.ArtProperties, ARun, AUnitConverter);
  ATextBoxContent := Safe<TdxTextBoxFloatingObjectContent>.Cast(ARun.Content);
  ApplyTextBoxProperties(AShapeContainer.ArtProperties, ATextBoxContent.TextBoxProperties);
  ApplyFloatingObjectDistancesProperties(AShapeContainer.ArtProperties, AShapeContainer.ArtTertiaryProperties, ARun.FloatingObjectProperties);
  AShapeContainer.ArtProperties.CreateProperties;
  AShapeContainer.ArtTertiaryProperties.CreateProperties;
end;

procedure TdxOfficeArtContent.ApplyFloatingObjectDistancesProperties(AArtProperties: TdxOfficeArtProperties;
  AArtTertiaryProperties: TdxOfficeArtTertiaryProperties; AFloatingObjectProperties: TdxFloatingObjectProperties);
begin
  AArtProperties.FloatingObjectProperties := AFloatingObjectProperties;
  if AFloatingObjectProperties.UseRelativeWidth then
    ApplyRelativeWidth(AArtTertiaryProperties, AFloatingObjectProperties);
  if AFloatingObjectProperties.UseRelativeHeight then
    ApplyRelativeHeight(AArtTertiaryProperties, AFloatingObjectProperties);
  if AFloatingObjectProperties.UseHorizontalPositionAlignment then
    ApplyHorizontalPositionAlignment(AArtTertiaryProperties, AFloatingObjectProperties);
  if AFloatingObjectProperties.UseVerticalPositionAlignment then
    ApplyVerticalPositionAlignment(AArtTertiaryProperties, AFloatingObjectProperties);
  if AFloatingObjectProperties.UsePercentOffset then
    ApplyPercentOffset(AArtTertiaryProperties, AFloatingObjectProperties);
end;

procedure TdxOfficeArtContent.ApplyRelativeWidth(AArtTertiaryProperties: TdxOfficeArtTertiaryProperties;
  AFloatingObjectProperties: TdxFloatingObjectProperties);
var
  ARelativeWidth: TdxFloatingObjectRelativeWidth;
begin
  AArtTertiaryProperties.UseRelativeWidth := True;
  ARelativeWidth := AFloatingObjectProperties.RelativeWidth;
  AArtTertiaryProperties.SizeRelH := TdxDrawingGroupShape2SizeRelH.TRelativeFrom(ARelativeWidth.From);
  AArtTertiaryProperties.PctHoriz := ARelativeWidth.Width div 100;
end;

procedure TdxOfficeArtContent.ApplyRelativeHeight(AArtTertiaryProperties: TdxOfficeArtTertiaryProperties;
  AFloatingObjectProperties: TdxFloatingObjectProperties);
var
  ARelativeHeight: TdxFloatingObjectRelativeHeight;
begin
  AArtTertiaryProperties.UseRelativeHeight := True;
  ARelativeHeight := AFloatingObjectProperties.RelativeHeight;
  AArtTertiaryProperties.SizeRelV := TdxDrawingGroupShape2SizeRelV.TRelativeFrom(ARelativeHeight.From);
  AArtTertiaryProperties.PctVert := ARelativeHeight.Height div 100;
end;

procedure TdxOfficeArtContent.ApplyHorizontalPositionAlignment(AArtTertiaryProperties: TdxOfficeArtTertiaryProperties;
  AFloatingObjectProperties: TdxFloatingObjectProperties);
begin
  AArtTertiaryProperties.PosRelH := GetPosRelH(AFloatingObjectProperties.HorizontalPositionType);
  AArtTertiaryProperties.PosH := GetPosH(AFloatingObjectProperties.HorizontalPositionAlignment);
  AArtTertiaryProperties.UsePosH := True;
end;

procedure TdxOfficeArtContent.ApplyVerticalPositionAlignment(AArtTertiaryProperties: TdxOfficeArtTertiaryProperties;
  AFloatingObjectProperties: TdxFloatingObjectProperties);
begin
  AArtTertiaryProperties.PosRelV := GetPosRelV(AFloatingObjectProperties.VerticalPositionType);
  AArtTertiaryProperties.PosV := GetPosV(AFloatingObjectProperties.VerticalPositionAlignment);
  AArtTertiaryProperties.UsePosV := True;
end;

procedure TdxOfficeArtContent.ApplyPercentOffset(AArtTertiaryProperties: TdxOfficeArtTertiaryProperties;
  AFloatingObjectProperties: TdxFloatingObjectProperties);
begin
  if (AFloatingObjectProperties.PercentOffsetX <> 0) and
    (not AFloatingObjectProperties.UseHorizontalPositionAlignment or
    (AFloatingObjectProperties.HorizontalPositionAlignment = TdxFloatingObjectHorizontalPositionAlignment.None)) then
  begin
    AArtTertiaryProperties.PctHorizPos := AFloatingObjectProperties.PercentOffsetX div 100;
    AArtTertiaryProperties.PosRelH := GetPosRelH(AFloatingObjectProperties.HorizontalPositionType);
    AArtTertiaryProperties.PosH := TdxDrawingGroupShapePosH.TMsoph.msophAbs;
    AArtTertiaryProperties.UsePosH := True;
  end;
  if (AFloatingObjectProperties.PercentOffsetY <> 0) and
    (not AFloatingObjectProperties.UseVerticalPositionAlignment or
    (AFloatingObjectProperties.VerticalPositionAlignment = TdxFloatingObjectVerticalPositionAlignment.None)) then
  begin
    AArtTertiaryProperties.PctVertPos := AFloatingObjectProperties.PercentOffsetY div 100;
    AArtTertiaryProperties.PosRelV := GetPosRelV(AFloatingObjectProperties.VerticalPositionType);
    AArtTertiaryProperties.PosV := TdxDrawingGroupShapePosV.TMsopv.msopvAbs;
    AArtTertiaryProperties.UsePosV := True;
  end;
end;

function TdxOfficeArtContent.GetPosRelH(AType: TdxFloatingObjectHorizontalPositionType): TdxDrawingGroupShapePosRelH.TMsoprh;
begin
  case AType of
    TdxFloatingObjectHorizontalPositionType.Character:
      Result := TdxDrawingGroupShapePosRelH.TMsoprh.msoprhChar;
    TdxFloatingObjectHorizontalPositionType.Margin:
      Result := TdxDrawingGroupShapePosRelH.TMsoprh.msoprhMargin;
    TdxFloatingObjectHorizontalPositionType.Page:
      Result := TdxDrawingGroupShapePosRelH.TMsoprh.msoprhPage;
    TdxFloatingObjectHorizontalPositionType.Column:
      Result := TdxDrawingGroupShapePosRelH.TMsoprh.msoprhText;
    else
      Result := TdxDrawingGroupShapePosRelH.TMsoprh.msoprhText;
  end;
end;

function TdxOfficeArtContent.GetPosRelV(AType: TdxFloatingObjectVerticalPositionType): TdxDrawingGroupShapePosRelV.TMsoprv;
begin
  case AType of
    TdxFloatingObjectVerticalPositionType.Line:
      Result := TdxDrawingGroupShapePosRelV.TMsoprv.msoprvLine;
    TdxFloatingObjectVerticalPositionType.Margin:
      Result := TdxDrawingGroupShapePosRelV.TMsoprv.msoprvMargin;
    TdxFloatingObjectVerticalPositionType.Page:
      Result := TdxDrawingGroupShapePosRelV.TMsoprv.msoprvPage;
    TdxFloatingObjectVerticalPositionType.Paragraph:
      Result := TdxDrawingGroupShapePosRelV.TMsoprv.msoprvText;
    else
      Result := TdxDrawingGroupShapePosRelV.TMsoprv.msoprvText;
  end;
end;

function TdxOfficeArtContent.GetPosH(AAlignment: TdxFloatingObjectHorizontalPositionAlignment): TdxDrawingGroupShapePosH.TMsoph;
begin
  case AAlignment of
    TdxFloatingObjectHorizontalPositionAlignment.None:
      Result := TdxDrawingGroupShapePosH.TMsoph.msophAbs;
    TdxFloatingObjectHorizontalPositionAlignment.Center:
      Result := TdxDrawingGroupShapePosH.TMsoph.msophCenter;
    TdxFloatingObjectHorizontalPositionAlignment.Inside:
      Result := TdxDrawingGroupShapePosH.TMsoph.msophInside;
    TdxFloatingObjectHorizontalPositionAlignment.Left:
      Result := TdxDrawingGroupShapePosH.TMsoph.msophLeft;
    TdxFloatingObjectHorizontalPositionAlignment.Outside:
      Result := TdxDrawingGroupShapePosH.TMsoph.msophOutside;
    TdxFloatingObjectHorizontalPositionAlignment.Right:
      Result := TdxDrawingGroupShapePosH.TMsoph.msophRight;
    else
      Result := TdxDrawingGroupShapePosH.TMsoph.msophAbs;
  end;
end;

function TdxOfficeArtContent.GetPosV(AAlignment: TdxFloatingObjectVerticalPositionAlignment): TdxDrawingGroupShapePosV.TMsopv;
begin
  case AAlignment of
    TdxFloatingObjectVerticalPositionAlignment.None:
      Result := TdxDrawingGroupShapePosV.TMsopv.msopvAbs;
    TdxFloatingObjectVerticalPositionAlignment.Bottom:
      Result := TdxDrawingGroupShapePosV.TMsopv.msopvBottom;
    TdxFloatingObjectVerticalPositionAlignment.Center:
      Result := TdxDrawingGroupShapePosV.TMsopv.msopvCenter;
    TdxFloatingObjectVerticalPositionAlignment.Inside:
      Result := TdxDrawingGroupShapePosV.TMsopv.msopvInside;
    TdxFloatingObjectVerticalPositionAlignment.Outside:
      Result := TdxDrawingGroupShapePosV.TMsopv.msopvOutside;
    TdxFloatingObjectVerticalPositionAlignment.Top:
      Result := TdxDrawingGroupShapePosV.TMsopv.msopvTop;
    else
      Result := TdxDrawingGroupShapePosV.TMsopv.msopvAbs;
  end;
end;

procedure TdxOfficeArtContent.ApplyTextBoxProperties(AArtProperties: TdxOfficeArtProperties; ATextBoxProperties: TdxTextBoxProperties);
begin
  AArtProperties.TextBoxProperties := ATextBoxProperties;
end;

function TdxOfficeArtContent.CreateShapeContainer(AState: TdxDocContentState; AShapeIdentifier: Integer): TdxOfficeArtFloatingShapeContainer;
var
  ADrawing: TdxOfficeArtWordDrawing;
  AShapes: TdxList<TdxOfficeDrawingPartBase>;
  ADrawingData: TdxOfficeArtFileDrawingRecord;
begin
  ADrawing := GetDrawingByState(AState);
  Result := TdxOfficeArtFloatingShapeContainer.Create;
  AShapes := ADrawing.DrawingObjectsContainer.ShapeGroup.Items;
  AShapes.Add(Result);
  Result.ShapeRecord.ShapeIdentifier := AShapeIdentifier;
  ADrawingData := ADrawing.DrawingObjectsContainer.DrawingData;
  ADrawingData.LastShapeIdentifier := AShapeIdentifier;
  ADrawingData.NumberOfShapes := AShapes.Count;
end;

function TdxOfficeArtContent.GetDrawingByState(AState: TdxDocContentState): TdxOfficeArtWordDrawing;
begin
  if AState = TdxDocContentState.MainDocument then
    Exit(MainDocumentDrawing);
  if AState = TdxDocContentState.HeadersFootersStory then
    Exit(HeaderDrawing);

  TdxRichEditExceptions.ThrowInternalException;
  Result := nil;
end;

procedure TdxOfficeArtContent.Read(AReader: TBinaryReader; AEmbeddedReader: TBinaryReader; AOffset: Integer;
  ASize: Integer; const AImageCreator: IdxDocOfficeImageCreator);
var
  AEndPosition: Int64;
begin
  AReader.BaseStream.Seek(AOffset, TSeekOrigin.soBeginning);
  AEndPosition := AReader.BaseStream.Position + ASize;
  FDrawingContainer.Free;
  FDrawingContainer := TdxOfficeArtDrawingContainer.FromStream(AReader, AEmbeddedReader,
    AImageCreator);
  while AReader.BaseStream.Position < AEndPosition do
    Drawings.Add(TdxOfficeArtWordDrawing.FromStream(AReader));
  FContentHelper.Traverse;
end;

procedure TdxOfficeArtContent.Write(AWriter: TBinaryWriter; AEmbeddedWriter: TBinaryWriter);
var
  ACount, I: Integer;
begin
  DrawingContainer.Write(AWriter, AEmbeddedWriter);
  ACount := Drawings.Count;
  for I := 0 to ACount - 1 do
    Drawings[I].Write(AWriter);
end;

{ TdxOfficeArtDrawingObjectsContainerSupprortsBackgroundShape }

constructor TdxOfficeArtDrawingObjectsContainerSupprortsBackgroundShape.Create(ALocation: Byte;
  ADrawingId: Integer; AShapeIdentifier: Integer);
begin
  inherited Create(ALocation, ADrawingId, AShapeIdentifier);
  FBackgroundShape := CreateBackgroundShape(AShapeIdentifier);
  Items.Add(FBackgroundShape);
end;

constructor TdxOfficeArtDrawingObjectsContainerSupprortsBackgroundShape.Create;
begin
  CreateGarbageCollector;
end;

class function TdxOfficeArtDrawingObjectsContainerSupprortsBackgroundShape.FromStream(AReader: TBinaryReader): TdxOfficeArtDrawingObjectsContainerSupprortsBackgroundShape;
begin
  Result := TdxOfficeArtDrawingObjectsContainerSupprortsBackgroundShape.Create;
  Result.Read(AReader);
end;

function TdxOfficeArtDrawingObjectsContainerSupprortsBackgroundShape.GetBackgroundShape: TdxOfficeArtShapeContainer;
begin
  Result := FBackgroundShape;
end;

function TdxOfficeArtDrawingObjectsContainerSupprortsBackgroundShape.CreateBackgroundShape(AShapeIdentifier: Integer): TdxOfficeArtShapeContainer;
begin
  Result := TdxOfficeArtShapeContainer.Create(TdxOfficeArtHeaderInfos.RectangleShape);
  Result.ShapeRecord.Flags := Result.ShapeRecord.Flags or BackgroundFlag;
  Result.ShapeRecord.ShapeIdentifier := AShapeIdentifier;
  Result.Items.Add(TdxOfficeClientData.Create);
end;

procedure TdxOfficeArtDrawingObjectsContainerSupprortsBackgroundShape.SetBackgroundShapeProperties;
var
  AProperties: TList<IdxOfficeDrawingProperty>;
  APageBackColor: TdxAlphaColor;
begin
  AProperties := BackgroundShape.ArtProperties.Properties;
  APageBackColor := BackgroundShape.ArtProperties.FillColor;
  if not TdxAlphaColors.IsEmpty(APageBackColor) then
  begin
    AProperties.Add(TdxDrawingFillColor.Create(APageBackColor));
    AProperties.Add(TdxDrawingFillStyleBooleanProperties.Create);
  end;
  AProperties.Add(TdxDrawingLineWidth.Create);
  AProperties.Add(TdxDrawingLineStyleBooleanProperties.Create);
  AProperties.Add(TdxDrawingBlackWhiteMode.Create(TdxBlackWhiteMode.White));
  AProperties.Add(TdxDrawingShapeBooleanProperties.Create);
end;

function TdxOfficeArtDrawingObjectsContainerSupprortsBackgroundShape.TryRead(AReader: TBinaryReader; AHeader: TdxOfficeArtRecordHeader): Boolean;
begin
  if AHeader.TypeCode = TdxOfficeArtTypeCodes.ShapeContainer then
  begin
    FBackgroundShape := TdxOfficeArtShapeContainer.FromStream(AReader, AHeader);
    ObjectsToDelete.Add(FBackgroundShape);
    Exit(True);
  end;
  Result := inherited TryRead(AReader, AHeader);
end;

procedure TdxOfficeArtDrawingObjectsContainerSupprortsBackgroundShape.WriteCore(AWriter: TBinaryWriter);
begin
  Assert(BackgroundShape.ArtProperties.Properties.Count > 0);
  inherited WriteCore(AWriter);
end;

{ TdxOfficeArtDrawingObjectsContainer }

class function TdxOfficeArtDrawingObjectsContainer.FromStream(AReader: TBinaryReader): TdxOfficeArtDrawingObjectsContainer;
begin
  Result := TdxOfficeArtDrawingObjectsContainer.Create;
  Result.Read(AReader);
end;

function TdxOfficeArtDrawingObjectsContainer.GetBackgroundShape: TdxOfficeArtShapeContainer;
begin
  Result := nil;
end;

procedure TdxOfficeArtDrawingObjectsContainer.SetBackgroundShapeProperties;
begin
  TdxRichEditExceptions.ThrowInternalException;
end;

{ TdxOfficeClientAnchor }

function TdxOfficeClientAnchor.GetHeaderInstanceInfo: Integer;
begin
  Result := TdxOfficeArtHeaderInfos.EmptyHeaderInfo;
end;

function TdxOfficeClientAnchor.GetHeaderTypeCode: Integer;
begin
  Result := TdxOfficeArtTypeCodes.ClientAnchor;
end;

function TdxOfficeClientAnchor.GetHeaderVersion: Integer;
begin
  Result := TdxOfficeArtVersions.EmptyHeaderVersion;
end;

procedure TdxOfficeClientAnchor.WriteCore(AWriter: TBinaryWriter);
begin
  AWriter.Write(Data);
end;

function TdxOfficeClientAnchor.GetSize: Integer;
begin
  Result := RecordLength;
end;

{ TdxOfficeClientData }

function TdxOfficeClientData.GetHeaderInstanceInfo: Integer;
begin
  Result := TdxOfficeArtHeaderInfos.EmptyHeaderInfo;
end;

function TdxOfficeClientData.GetHeaderTypeCode: Integer;
begin
  Result := TdxOfficeArtTypeCodes.ClientData;
end;

function TdxOfficeClientData.GetHeaderVersion: Integer;
begin
  Result := TdxOfficeArtVersions.EmptyHeaderVersion;
end;

procedure TdxOfficeClientData.WriteCore(AWriter: TBinaryWriter);
begin
  AWriter.Write(Data);
end;

function TdxOfficeClientData.GetSize: Integer;
begin
  Result := RecordLength;
end;

{ TdxDrawingBlackWhiteMode }

constructor TdxDrawingBlackWhiteMode.Create(AMode: Byte);
begin
  Value := AMode;
end;

constructor TdxDrawingBlackWhiteMode.Create;
begin
  Create(TdxBlackWhiteMode.Automatic);
end;

function TdxDrawingBlackWhiteMode.GetComplex: Boolean;
begin
  Result := False;
end;

function TdxDrawingBlackWhiteMode.GetMode: Byte;
begin
  Result := Value;
end;

procedure TdxDrawingBlackWhiteMode.SetMode(const AValue: Byte);
begin
  Value := AValue;
end;

end.
