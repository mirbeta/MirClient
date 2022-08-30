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

unit dxJPX;

{$I cxVer.inc}

interface

uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections, dxCore, dxCoreClasses, dxThreading;

type
  TdxBitReader = class;
  TdxJPXBaseResolutionLevel = class;
  TdxJPXBitReader = class;
  TdxJPXCodeBlock = class;
  TdxJPXCompositeResolutionLevel = class;
  EdxJPXException = class(EdxException);
  TdxJPXQuantizationComponentParameters = class;
  TdxJPXResolutionLevel = class;
  TdxJPXSubBand = class;
  TdxJPXTile = class;
  TdxJPXTileComponent = class;

  TdxJPXCodeBlockCodingStyle = (bcsSelectiveArithmeticCodingBypass = 1, bcsResetContextProbabilities = 2,
    bcsTerminationOnEachCodingPass = 4, bcsVerticallyCausalContext = 8, bcsPredictableTermination = 16,
    bcsUseSegmentationSymbols = 32, bcsUnknown = 0);
  TdxJPXCodingStyleParameter = (cspUsePrecincts= 1, cspUseSOPMarker = 2, cspUseEPHMarker = 4);
  TdxJPXProgressionOrder = (poLayerResolutionComponentPosition, poResolutionLayerComponentPosition,
    poResolutionPositionComponentLayer, poPositionComponentResolutionLayer, poComponentPositionResolutionLayer);

  { TdxBigEndianStreamReader }

  TdxBigEndianStreamReader = class(TInterfacedObject)
  strict private
    FStream: TStream;
    function GetLength: Int64;
    function GetPosition: Int64;
  protected
    function IsEOF: Boolean;
    function ReadBytes: TBytes; overload;
    function ReadBytes(ACount: Integer): TBytes; overload;
    function ReadInt16: Integer; inline;
    function ReadInt32: Integer; inline;
    function ReadInt(ACount: Integer): Integer;
    function ReadByte: Byte; inline;
    procedure FreeStream;
    procedure Skip(ACount: Integer);

    property Length: Int64 read GetLength;
    property Position: Int64 read GetPosition;
    property Stream: TStream read FStream;
  public
    constructor Create(AStream: TStream); virtual;
    destructor Destroy; override;
  end;

  { TdxArithmeticState }

  TdxArithmeticState = class
  strict private
    FA: LongWord;
    FBuffer0: LongWord;
    FBuffer1: LongWord;
    FC: LongWord;
    FCt: LongWord;
    FReader: TdxBigEndianStreamReader;
    procedure LoadBuffer; inline;
    procedure ReadByte; inline;
    procedure Renormalized; inline;
  public
    constructor Create(AReader: TdxBigEndianStreamReader);
    destructor Destroy; override;

    function Decode(const ACx: TBytes; AIndex: Integer): Integer;
  end;

  { TdxArithmeticQe }

  TdxArithmeticQe = record
  strict private
    FLpsXor: Byte;
    FMpsXor: Byte;
    FSwitch: Byte;
    FQe: LongWord;
  public
    class function Create(AQe: LongWord; AMpsXor, ALpsXor, ASwitch: Byte): TdxArithmeticQe; static;

    property LpsXor: Byte read FLpsXor;
    property MpsXor: Byte read FMpsXor;
    property Switch: Byte read FSwitch;
    property Qe: LongWord read FQe;
  end;

  { TdxArithmeticContext }

  TdxArithmeticContext = class
  strict private
    FContext: TBytes;
    FState: TdxArithmeticState;
  public
    constructor Create(AState: TdxArithmeticState; const AContext: TBytes); overload;
    constructor Create(AState: TdxArithmeticState; ALength: Integer); overload;
    destructor Destroy; override;

    function DecodeBit(ACx: Integer): Integer; inline;
  end;

  { TdxJPXPacketPosition }

  TdxJPXPacketPosition = record
  strict private
    FComponent: Integer;
    FLayer: Integer;
    FPrecinct: Integer;
    FResolutionLevel: Integer;
  public
    procedure Initialize(AComponent, ALayer, AResolutionLevel, APrecinct: Integer);

    property Component: Integer read FComponent;
    property Layer: Integer read FLayer;
    property Precinct: Integer read FPrecinct;
    property ResolutionLevel: Integer read FResolutionLevel;
  end;

  { TdxJPXPrecinctSize }

  TdxJPXPrecinctSize = record
  strict private
    FHeightExponent: Integer;
    FWidthExponent: Integer;
  public
    procedure Initialize(AValue: Integer);

    property HeightExponent: Integer read FHeightExponent;
    property WidthExponent: Integer read FWidthExponent;
  end;

  TdxJPXPrecinctSizes = array of TdxJPXPrecinctSize;

  { TdxJPXComponent }

  TdxJPXComponent = record
  strict private
    FBitsPerComponent: Integer;
    FHorizontalSeparation: Integer;
    FIsSigned: Boolean;
    FVerticalSeparation: Integer;
  public
    procedure Initialize(ABitsPerComponent, AHorizontalSeparation, AVerticalSeparation: Integer);

    property BitsPerComponent: Integer read FBitsPerComponent;
    property HorizontalSeparation: Integer read FHorizontalSeparation;
    property IsSigned: Boolean read FIsSigned;
    property VerticalSeparation: Integer read FVerticalSeparation;
  end;

  TdxJPXComponents = array of TdxJPXComponent;

  { TdxJPXDefaultCodingStyle }

  TdxJPXDefaultCodingStyle = record
  strict private
    FLayerCount: Integer;
    FMultipleTransformation: Boolean;
    FProgressionOrder: TdxJPXProgressionOrder;
  public
    procedure Initialize(AReader: TdxBigEndianStreamReader);

    property LayerCount: Integer read FLayerCount;
    property MultipleTransformation: Boolean read FMultipleTransformation;
    property ProgressionOrder: TdxJPXProgressionOrder read FProgressionOrder;
  end;

  { TdxJPXCodingStyleComponent }

  TdxJPXCodingStyleComponent = record
  strict private
    FCodeBlockCodingStyle: TdxJPXCodeBlockCodingStyle;
    FCodeBlockHeightExponent: Integer;
    FCodeBlockWidthExponent: Integer;
    FDecompositionLevelCount: Integer;
    FPrecinctSizes: TdxJPXPrecinctSizes;
    FUseWaveletTransformation: Boolean;
  public
    procedure Initialize(AReader: TdxBigEndianStreamReader; AUsePrecincts: Boolean);

    property CodeBlockCodingStyle: TdxJPXCodeBlockCodingStyle read FCodeBlockCodingStyle;
    property CodeBlockHeightExponent: Integer read FCodeBlockHeightExponent;
    property CodeBlockWidthExponent: Integer read FCodeBlockWidthExponent;
    property DecompositionLevelCount: Integer read FDecompositionLevelCount;
    property PrecinctSizes: TdxJPXPrecinctSizes read FPrecinctSizes;
    property UseWaveletTransformation: Boolean read FUseWaveletTransformation;
  end;

  TdxJPXCodingStyleComponents = array of TdxJPXCodingStyleComponent;

  { TdxJPXCodeBlockChunk }

  TdxJPXCodeBlockChunk = record
  strict private
    FCodingPassCount: Byte;
    FData: TBytes;
  public
    procedure Initialize(const AData: TBytes; ACodingPassCount: Byte);

    property CodingPassCount: Byte read FCodingPassCount;
    property Data: TBytes read FData;
  end;

  { TdxJPXTileComponentData }

  TdxJPXTileComponentData = record
  strict private
    FBitsPerComponent: Integer;
    FData: TSingleDynArray;
  public
    procedure Initialize(const AData: TSingleDynArray; ABitsPerComponent: Integer);

    property BitsPerComponent: Integer read FBitsPerComponent;
    property Data: TSingleDynArray read FData;
  end;

  TdxJPXTileComponentDataArray = array of TdxJPXTileComponentData;

  { TdxJPXSize }

  TdxJPXSize = record
  strict private
    FComponents: TdxJPXComponents;
    FGridHeight: Integer;
    FGridHorizontalOffset: Integer;
    FGridVerticalOffset: Integer;
    FGridWidth: Integer;
    FTileHeight: Integer;
    FTileHorizontalOffset: Integer;
    FTileVerticalOffset: Integer;
    FTileWidth: Integer;
  public
    procedure Initialize(AReader: TdxBigEndianStreamReader);

    property Components: TdxJPXComponents read FComponents;
    property GridHeight: Integer read FGridHeight;
    property GridHorizontalOffset: Integer read FGridHorizontalOffset;
    property GridVerticalOffset: Integer read FGridVerticalOffset;
    property GridWidth: Integer read FGridWidth;
    property TileHeight: Integer read FTileHeight;
    property TileHorizontalOffset: Integer read FTileHorizontalOffset;
    property TileVerticalOffset: Integer read FTileVerticalOffset;
    property TileWidth: Integer read FTileWidth;
  end;

  { TdxJPXQuantizationStepSize }

  TdxJPXQuantizationStepSize = record
  strict private
    FEpsilon: Byte;
    FMu: SmallInt;
  public
    procedure Initialize(AEpsilon: Byte; AMu: SmallInt); overload;
    procedure Initialize(AValue1, AValue2: Byte); overload;

    property Epsilon: Byte read FEpsilon;
    property Mu: SmallInt read FMu;
  end;

  TdxJPXQuantizationStepSizes = array of TdxJPXQuantizationStepSize;

  { TdxJPXCoefficient }

  TdxJPXCoefficient = record
  strict private const
    IsNotFirstRefinementMask = $20;
    SignificanceMask = $01;
    SignMask = $02;
  strict private
    FNeighborSignificance: Byte;

    function GetDiagonalNeighborSignificance: Byte; inline;
    function GetHorizontalNeighborSignificance: Byte; inline;
    function GetIsNotFirstRefinement: Boolean;
    function GetSign: Byte;
    function GetSignificance: Byte; inline;
    function GetVerticalNeighborSignificance: Byte; inline;
    procedure SetSign(const AValue: Byte);
    procedure SetSignificance(const AValue: Byte);
    procedure SetIsNotFirstRefinement(const AValue: Boolean);
  public
    BitsDecoded: Byte;
    CalculatedSignificancesStepIndex: Byte;
    Flags: Byte;
    Magnitude: SmallInt;
    NotZeroContextLabelPassIndex: Byte;

    function ToInteger: Integer;
    procedure IncrementDiagonalNeighborSignificance;
    procedure IncrementHorizontalNeighborSignificance;
    procedure IncrementVerticalNeighborSignificance;
    procedure Initialize;

    property DiagonalNeighborSignificance: Byte read GetDiagonalNeighborSignificance;
    property IsNotFirstRefinement: Boolean read GetIsNotFirstRefinement write SetIsNotFirstRefinement;
    property HorizontalNeighborSignificance: Byte read GetHorizontalNeighborSignificance;
    property Sign: Byte read GetSign write SetSign;
    property Significance: Byte read GetSignificance write SetSignificance;
    property VerticalNeighborSignificance: Byte read GetVerticalNeighborSignificance;
  end;

  TdxJPXCoefficients = array of TdxJPXCoefficient;

  { TdxJPXImage }

  TdxJPXImage = class(TObject)
  strict private
    FCodingStyleComponents: TdxJPXCodingStyleComponents;
    FDefaultCodingStyle: TdxJPXDefaultCodingStyle;
    FSize: TdxJPXSize;
    FTileCount: TPoint;
    FTiles: TObjectList<TdxJPXTile>;
    FQuantizationParameters: TObjectList<TdxJPXQuantizationComponentParameters>;

    function GetTiles: TObjectList<TdxJPXTile>;
    procedure SetSize(const AValue: TdxJPXSize);

    function DoDecode(const AData: TBytes; out AComponentCount: Integer): TBytes;
    function GetData: TBytes;
    procedure CreateTiles;
  protected
    procedure ClearTiles;
    procedure ReadCodingStyleComponents(AReader: TdxBigEndianStreamReader);
    procedure ReadQuantizationParameters(AReader: TdxBigEndianStreamReader; ADataLength: Integer);

    property CodingStyleComponents: TdxJPXCodingStyleComponents read FCodingStyleComponents write FCodingStyleComponents;
    property DefaultCodingStyle: TdxJPXDefaultCodingStyle read FDefaultCodingStyle write FDefaultCodingStyle;
    property Size: TdxJPXSize read FSize write SetSize;
    property TileCount: TPoint read FTileCount;
    property Tiles: TObjectList<TdxJPXTile> read GetTiles;
    property QuantizationParameters: TObjectList<TdxJPXQuantizationComponentParameters> read FQuantizationParameters;
  public
    constructor Create;
    destructor Destroy; override;

    class function Decode(const AData: TBytes; out AComponentCount: Integer): TBytes;
  end;

  { TdxJPXTagTreeNode }

  TdxJPXTagTreeNode = record
  strict private
    FInitialized: Boolean;
    FIsDefined: Boolean;
    FValue: Integer;
  public
    procedure Initialize(AValue: Integer; AIsDefined: Boolean);

    property Initialized: Boolean read FInitialized;
    property IsDefined: Boolean read FIsDefined;
    property Value: Integer read FValue;
  end;

  TdxJPXTagTreeNodeArray = array of array of TdxJPXTagTreeNode;

  { TdxJPXTagTree }

  TdxJPXTagTree = class
  strict private
    FCurrentLevel: Integer;
    FData: array of TdxJPXTagTreeNodeArray;
    FIndexStack: TStack<TPoint>;
    FLevelCount: Integer;

    function GetPreviousValue(X, Y: Integer): TdxJPXTagTreeNode;
    function SetNextValue(AValue: Integer): TdxJPXTagTreeNode;
  protected
    function Read(ABitReader: TdxBitReader; X, Y: Integer): Integer;
    function ReadInclusion(ABitReader: TdxBitReader; X, Y, AMaxValue: Integer): Integer;
  public
    constructor Create(AWidth, AHeight: Integer);
    destructor Destroy; override;
  end;

  { TdxJPXCustomBox }

  TdxJPXCustomBoxClass = class of TdxJPXCustomBox;
  TdxJPXCustomBox = class abstract
  strict private
    class var FDictionary: TDictionary<Integer, TdxJPXCustomBoxClass>;

    class function GetBoxClass(AType: Integer): TdxJPXCustomBoxClass; overload;
    class function ReadBoxType(AReader: TdxBigEndianStreamReader; var ALength: Integer): Integer;
  protected
    class procedure PopulateImage(AReader: TdxBigEndianStreamReader; ALength: Integer; AImage: TdxJPXImage); virtual;

    class procedure DoReadImage(AReader: TdxBigEndianStreamReader; AImage: TdxJPXImage; out ABoxClass: TdxJPXCustomBoxClass);
  public
    class constructor Create;
    class destructor Destroy;

    class function ID: Integer; virtual;
    class procedure ReadImage(AReader: TdxBigEndianStreamReader; AImage: TdxJPXImage);
  end;

  { TdxJPXSignatureBox }

  TdxJPXSignatureBox = class(TdxJPXCustomBox)
  protected
    class procedure PopulateImage(AReader: TdxBigEndianStreamReader; ALength: Integer; AImage: TdxJPXImage); override;
  public
    class function ID: Integer; override;
  end;

  { TdxJPXFileTypeBox }

  TdxJPXFileTypeBox = class(TdxJPXCustomBox)
  protected
    class procedure PopulateImage(AReader: TdxBigEndianStreamReader; ALength: Integer; AImage: TdxJPXImage); override;
  public
    class function ID: Integer; override;
  end;

  { TdxJPXImageHeaderBox }

  TdxJPXImageHeaderBox = class(TdxJPXCustomBox)
  protected
    class procedure PopulateImage(AReader: TdxBigEndianStreamReader; ALength: Integer; AImage: TdxJPXImage); override;
  public
    class function ID: Integer; override;
  end;

  { TdxJPXColorSpecificationBox }

  TdxJPXColorSpecificationBox = class(TdxJPXCustomBox)
  protected
    class procedure PopulateImage(AReader: TdxBigEndianStreamReader; ALength: Integer; AImage: TdxJPXImage); override;
  public
    class function ID: Integer; override;
  end;

  { TdxJPXJP2HeaderBox }

  TdxJPXJP2HeaderBox = class(TdxJPXCustomBox)
  protected
    class procedure PopulateImage(AReader: TdxBigEndianStreamReader; ALength: Integer; AImage: TdxJPXImage); override;
  public
    class function ID: Integer; override;
  end;

  { TdxJPXContiguousCodeStreamBox }

  TdxJPXContiguousCodeStreamBox = class(TdxJPXCustomBox)
  protected
    class procedure PopulateImage(AReader: TdxBigEndianStreamReader; ALength: Integer; AImage: TdxJPXImage); override;
  public
    class function ID: Integer; override;
  end;

  { TdxJPXUnknownBox }

  TdxJPXUnknownBox = class(TdxJPXCustomBox)
  protected
    class procedure PopulateImage(AReader: TdxBigEndianStreamReader; ALength: Integer; AImage: TdxJPXImage); override;
  public
    class function ID: Integer; override;
  end;

  { TdxJPXCustomMarker }

  TdxJPXCustomMarkerClass = class of TdxJPXCustomMarker;
  TdxJPXCustomMarker = class
  strict private
    FDataLength: Integer;
    class var FDictionary: TDictionary<Integer, TdxJPXCustomMarkerClass>;
    class function CreateAndRead(AReader: TdxBigEndianStreamReader; AImage: TdxJPXImage; AType: Integer;
      ATileIndex: Integer): TdxJPXCustomMarker;

    procedure DoRead(ADataLength: Integer = 0);
  protected
    class function ID: Integer; virtual;

    function GetMaxDataLength: Integer; virtual;
    procedure PopulateImage(AReader: TdxBigEndianStreamReader; AImage: TdxJPXImage; ATileIndex: Integer); virtual;

    procedure Read(AReader: TdxBigEndianStreamReader);
    procedure ReadAndCheckLength(AReader: TdxBigEndianStreamReader);

    property DataLength: Integer read FDataLength;
    property MaxDataLength: Integer read GetMaxDataLength;
  public
    class constructor Create;
    class destructor Destroy;

    class function CreateMarker(AReader: TdxBigEndianStreamReader; AImage: TdxJPXImage): TdxJPXCustomMarker; overload;
    class function CreateMarker(AReader: TdxBigEndianStreamReader; AImage: TdxJPXImage;
      ATileIndex: Integer): TdxJPXCustomMarker; overload;
  end;

  { TdxJPXStartOfCodeStreamMarker }

  TdxJPXStartOfCodeStreamMarker = class(TdxJPXCustomMarker)
  protected
    class function ID: Integer; override;
  end;

  { TdxJPXStartOfDataMarker }

  TdxJPXStartOfDataMarker = class(TdxJPXCustomMarker)
  protected
    class function ID: Integer; override;
  end;

  { TdxJPXCodeStreamEndMarker }

  TdxJPXCodeStreamEndMarker = class(TdxJPXCustomMarker)
  protected
    class function ID: Integer; override;
  end;

  { TdxJPXDefaultCodingStyleMarker }

  TdxJPXDefaultCodingStyleMarker = class(TdxJPXCustomMarker)
  protected
    class function ID: Integer; override;
    function GetMaxDataLength: Integer; override;
    procedure PopulateImage(AReader: TdxBigEndianStreamReader; AImage: TdxJPXImage; ATileIndex: Integer); override;
  end;

  { TdxJPXCodingStyleComponentMarker }

  TdxJPXCodingStyleComponentMarker = class(TdxJPXCustomMarker)
  protected
    class function ID: Integer; override;
    function GetMaxDataLength: Integer; override;
    procedure PopulateImage(AReader: TdxBigEndianStreamReader; AImage: TdxJPXImage; ATileIndex: Integer); override;
  end;

  { TdxJPXStartOfTilePartMarker }

  TdxJPXStartOfTilePartMarker = class(TdxJPXCustomMarker)
  protected
    class function ID: Integer; override;
    function GetMaxDataLength: Integer; override;
    procedure PopulateImage(AReader: TdxBigEndianStreamReader; AImage: TdxJPXImage; ATileIndex: Integer); override;
  end;

  { TdxJPXSizeMarker }

  TdxJPXSizeMarker = class(TdxJPXCustomMarker)
  protected
    class function ID: Integer; override;
    procedure PopulateImage(AReader: TdxBigEndianStreamReader; AImage: TdxJPXImage; ATileIndex: Integer); override;
  end;

  { TdxJPXDefaultQuantizationMarker }

  TdxJPXDefaultQuantizationMarker = class(TdxJPXCustomMarker)
  protected
    class function ID: Integer; override;
    function GetMaxDataLength: Integer; override;
    procedure PopulateImage(AReader: TdxBigEndianStreamReader; AImage: TdxJPXImage; ATileIndex: Integer); override;
  end;

  { TdxJPXQuantizationComponentMarker }

  TdxJPXQuantizationComponentMarker = class(TdxJPXCustomMarker)
  protected
    class function ID: Integer; override;
    function GetMaxDataLength: Integer; override;
    procedure PopulateImage(AReader: TdxBigEndianStreamReader; AImage: TdxJPXImage; ATileIndex: Integer); override;
  end;

  { TdxJPXUnknownMarker }

  TdxJPXUnknownMarker = class(TdxJPXCustomMarker)
  protected
    procedure PopulateImage(AReader: TdxBigEndianStreamReader; AImage: TdxJPXImage; ATileIndex: Integer); override;
  end;

  { TdxJPXQuantizationComponentParameters }

  TdxJPXQuantizationComponentParameters = class
  strict private
    FGuardBitCount: Integer;
  strict protected const
    R = 0.5;
  protected
    function Apply(const ACoefficient: TdxJPXCoefficient; ASubBandGainLog, ARi,
      ASubBandIndex: Integer): Single; virtual; abstract;
    function Clone: TdxJPXQuantizationComponentParameters; virtual; abstract;

    property GuardBitCount: Integer read FGuardBitCount;
  public
    constructor Create(AGuardBitCount: Integer); virtual;

    class function CreateParameters(AReader: TdxBigEndianStreamReader; AByteCount: Integer): TdxJPXQuantizationComponentParameters;
  end;

  { TdxJPXUnitaryStepSizeQuantizationComponentParameters }

  TdxJPXUnitaryStepSizeQuantizationComponentParameters = class(TdxJPXQuantizationComponentParameters)
  strict private
    FStepSizeExponents: TIntegerDynArray;
  protected
    function Apply(const ACoefficient: TdxJPXCoefficient; ASubBandGainLog, ARi, ASubBandIndex: Integer): Single; override;
    function Clone: TdxJPXQuantizationComponentParameters; override;
  public
    constructor Create(AGuardBitCount: Integer; AReader: TdxBigEndianStreamReader; AByteCount: Integer); reintroduce;
  end;

  { TdxJPXIrreversibleTransformationQuantizationComponentParameters }

  TdxJPXIrreversibleTransformationQuantizationComponentParameters = class(TdxJPXQuantizationComponentParameters)
  protected
    function Apply(const ACoefficient: TdxJPXCoefficient; ASubBandGainLog, ARi, ASubBandIndex: Integer): Single; override;
    function GetStepSize(ASubBandIndex: Integer): TdxJPXQuantizationStepSize; virtual; abstract;
  end;

  { TdxJPXScalarDerivedQuantizationComponentParameters }

  TdxJPXScalarDerivedQuantizationComponentParameters = class(TdxJPXIrreversibleTransformationQuantizationComponentParameters)
  strict private
    FStepSize: TdxJPXQuantizationStepSize;
  protected
    function Clone: TdxJPXQuantizationComponentParameters; override;
    function GetStepSize(ASubBandIndex: Integer): TdxJPXQuantizationStepSize; override;

    property StepSize: TdxJPXQuantizationStepSize read FStepSize;
  public
    constructor Create(AGuardBitCount: Integer; AReader: TdxBigEndianStreamReader); reintroduce;
  end;

  { TdxJPXScalarExpoundedQuantizationComponentParameters }

  TdxJPXScalarExpoundedQuantizationComponentParameters = class(TdxJPXIrreversibleTransformationQuantizationComponentParameters)
  strict private
    FStepSizes: TdxJPXQuantizationStepSizes;
  protected
    function Clone: TdxJPXQuantizationComponentParameters; override;
    function GetStepSize(ASubBandIndex: Integer): TdxJPXQuantizationStepSize; override;

    property StepSizes: TdxJPXQuantizationStepSizes read FStepSizes;
  public
    constructor Create(AGuardBitCount: Integer; AReader: TdxBigEndianStreamReader; AByteCount: Integer); reintroduce;
  end;

  { TdxJPXArea }

  TdxJPXArea = class
  strict private
    function GetHeight: Integer; inline;
    function GetWidth: Integer; inline;
  public
    X0: Integer;
    Y0: Integer;
    X1: Integer;
    Y1: Integer;
    property Height: Integer read GetHeight;
    property Width: Integer read GetWidth;
  end;

  { TdxJPXPacketProgressionOrder }

  TdxJPXPacketProgressionOrder = class
  strict private
    FImage: TdxJPXImage;
    FPacketEnumerator: TList<TdxJPXPacketPosition>;
    FTile: TdxJPXTile;
    function GetMaxResolutionLevel: Integer;
  protected
    function CreatePacketPositionList: TList<TdxJPXPacketPosition>; virtual;

    property Image: TdxJPXImage read FImage;
    property MaxResolutionLevel: Integer read GetMaxResolutionLevel;
    property PacketEnumerator: TList<TdxJPXPacketPosition> read FPacketEnumerator;
    property Tile: TdxJPXTile read FTile;
  public
    constructor Create(AImage: TdxJPXImage; ATile: TdxJPXTile); overload;
    destructor Destroy; override;

    class function CreatePacket(AOrder: TdxJPXProgressionOrder; AImage: TdxJPXImage;
      ATile: TdxJPXTile): TList<TdxJPXPacketPosition>;
  end;

  { TdxJPXLayerResolutionComponentPrecinctPacketProgressionOrder }

  TdxJPXLayerResolutionComponentPrecinctPacketProgressionOrder = class(TdxJPXPacketProgressionOrder)
  protected
    function CreatePacketPositionList: TList<TdxJPXPacketPosition>; override;
  end;

  { TdxJPXLayerResolutionComponentPrecinctPacketProgressionOrder }

  TdxJPXResolutionLayerComponentPrecinctPacketProgressionOrder = class(TdxJPXPacketProgressionOrder)
  protected
    function CreatePacketPositionList: TList<TdxJPXPacketPosition>; override;
  end;

  { TdxJPXResolutionPrecinctComponentLayerPacketProgressionOrder }

  TdxJPXResolutionPrecinctComponentLayerPacketProgressionOrder = class(TdxJPXPacketProgressionOrder)
  protected
    function CreatePacketPositionList: TList<TdxJPXPacketPosition>; override;
  end;

  { TdxJPXCodeBlockHeaderData }

  TdxJPXCodeBlockHeaderData = record
  strict private
    FCodeBlock: TdxJPXCodeBlock;
    FCodingPasses: Byte;
    FChunkLength: Integer;
  public
    procedure Initialize(ACodeBlock: TdxJPXCodeBlock; ACodingPasses: Byte; AChunkLength: Integer);

    property CodeBlock: TdxJPXCodeBlock read FCodeBlock;
    property CodingPasses: Byte read FCodingPasses;
    property ChunkLength: Integer read FChunkLength;
  end;

  { TdxJPXTile }

  TdxJPXTile = class(TdxJPXArea)
  strict private
    FComponents: TObjectList<TdxJPXTileComponent>;
    FCurrentPacketPositionIndex: Integer;
    FImage: TdxJPXImage;
    FPacketPositionList: TList<TdxJPXPacketPosition>;
    function GetMultipleTransformation: Boolean;
    function GetUseWaveletTransformation: Boolean;
    procedure PopulateCodeBlockHeaderDataQueue(AReader: TdxJPXBitReader; const ACurrentPosition: TdxJPXPacketPosition;
      ACodeBlockHeaderDataQueue: TQueue<TdxJPXCodeBlockHeaderData>);
  protected
    procedure AppendPacket(AStream: TStream);

    property Components: TObjectList<TdxJPXTileComponent> read FComponents;
    property MultipleTransformation: Boolean read GetMultipleTransformation;
    property UseWaveletTransformation: Boolean read GetUseWaveletTransformation;
  public
    constructor Create(AImage: TdxJPXImage; ATileIndex: Integer);
    destructor Destroy; override;

    class function ReadCodingPassCount(AReader: TdxJPXBitReader): Integer;
  end;

  { TdxJPXCodeBlock }

  TdxJPXCodeBlock = class(TdxJPXArea)
  strict private
    FCodingPassCount: Byte;
    FEncodedData: TBytes;
    FIsFirstInclusion: Boolean;
    FLBlock: Integer;
    FZeroBitPlanes: Integer;
  protected
    procedure AddChunk(const AChunk: TdxJPXCodeBlockChunk);

    property CodingPassCount: Byte read FCodingPassCount;
    property EncodedData: TBytes read FEncodedData;
    property IsFirstInclusion: Boolean read FIsFirstInclusion write FIsFirstInclusion;
    property LBlock: Integer read FLBlock write FLBlock;
    property ZeroBitPlanes: Integer read FZeroBitPlanes write FZeroBitPlanes;
  public
    constructor Create(X, Y, AWidth, AHeight: Integer);
    destructor Destroy; override;
  end;

  { TdxJPXPrecinct }

  TdxJPXPrecinct = class(TdxJPXArea)
  strict private
    FCodeBlockHorizontalCount: Integer;
    FCodeBlocks: TList<TdxJPXCodeBlock>;
    FCodeBlockVerticalCount: Integer;
    FInclusionTree: TdxJPXTagTree;
    FIndex: Integer;
    FZeroBitPlaneTree: TdxJPXTagTree;
  public
    constructor Create(ASubBand: TdxJPXSubBand; X, Y, AWidth, AHeight, AIndex: Integer);
    destructor Destroy; override;

    property CodeBlockHorizontalCount: Integer read FCodeBlockHorizontalCount;
    property CodeBlocks: TList<TdxJPXCodeBlock> read FCodeBlocks;
    property CodeBlockVerticalCount: Integer read FCodeBlockVerticalCount;
    property InclusionTree: TdxJPXTagTree read FInclusionTree;
    property Index: Integer read FIndex;
    property ZeroBitPlaneTree: TdxJPXTagTree read FZeroBitPlaneTree;
  end;

  { TdxJPXSubBandCoefficients }

  TdxJPXSubBandCoefficients = record
  strict private
    FCoefficients: TSingleDynArray;
    FHeight: Integer;
    FWidth: Integer;
  public
    function Interleave(const AHLSubBandCoefficients, ALHSubBandCoefficients,
      AHHSubBandCoefficients: TdxJPXSubBandCoefficients): TdxJPXSubBandCoefficients;
    procedure Initialize(const ACoefficients: TSingleDynArray; AWidth, AHeight: Integer);

    property Coefficients: TSingleDynArray read FCoefficients;
    property Height: Integer read FHeight;
    property Width: Integer read FWidth;
  end;

  { TdxJPXSubBand }

  TdxJPXSubBand = class(TdxJPXArea)
  strict private type
  {$REGION 'private types'}
    TCodeBlocksBuilder = class
    strict private
      FActualLeftBlockWidth: Integer;
      FBottomCodeBlockBand: Integer;
      FCodeBlockIndex: Integer;
      FCodeBlockHeight: Integer;
      FCodeBlocks: TObjectList<TdxJPXCodeBlock>;
      FCodeBlockWidth: Integer;
      FX0: Integer;
      FX1: Integer;
      FY0: Integer;
      FY1: Integer;
      FRightCodeBlockBand: Integer;
      FStartInnerHorizontalOffset: Integer;
      procedure AddRow(ATop, ACodeBlockHeight: Integer);
    public
      function Build(ASubBand: TdxJPXSubBand; ACodeBlockWidth, ACodeBlockHeight: Integer): TObjectList<TdxJPXCodeBlock>;
    end;
  {$ENDREGION}
  strict private
    FCodeBlocks: TObjectList<TdxJPXCodeBlock>;
    FCodeBlocksHigh: Integer;
    FCodeBlocksWide: Integer;
    FComponent: TdxJPXTileComponent;
    FResolutionLevelIndex: Integer;
    function GetCoefficients: TdxJPXSubBandCoefficients;
  protected
    function GetContextLabel(V, H, D: Byte): Integer; virtual; abstract;
    function GetGainLog: Integer; virtual; abstract;
    function GetHorizontalQuantity: Integer; virtual; abstract;
    function GetVerticalQuantity: Integer; virtual; abstract;

    function CalculateContextLabel(const ACoefficient: TdxJPXCoefficient): Integer; overload;

    property CodeBlocks: TObjectList<TdxJPXCodeBlock> read FCodeBlocks;
    property CodeBlocksHigh: Integer read FCodeBlocksHigh;
    property CodeBlocksWide: Integer read FCodeBlocksWide;
    property Coefficients: TdxJPXSubBandCoefficients read GetCoefficients;
    property GainLog: Integer read GetGainLog;
    property HorizontalQuantity: Integer read GetHorizontalQuantity;
    property VerticalQuantity: Integer read GetVerticalQuantity;
  public
    constructor Create(AComponent: TdxJPXTileComponent;
      AResolutionLevelIndex, ACodeBlockWidth, ACodeBlockHeight: Integer); virtual;
    destructor Destroy; override;
  end;

  { TdxJPXLowPassSubBand }

  TdxJPXLowPassSubBand = class(TdxJPXSubBand)
  protected
    function GetContextLabel(V, H, D: Byte): Integer; override;
  end;

  { TdxJPXLLSubBand }

  TdxJPXLLSubBand = class(TdxJPXLowPassSubBand)
  protected
    function GetGainLog: Integer; override;
    function GetHorizontalQuantity: Integer; override;
    function GetVerticalQuantity: Integer; override;
  end;

  { TdxJPXLHSubBand }

  TdxJPXLHSubBand = class(TdxJPXLowPassSubBand)
  protected
    function GetGainLog: Integer; override;
    function GetHorizontalQuantity: Integer; override;
    function GetVerticalQuantity: Integer; override;
  end;

  { TdxJPXHLSubBand }

  TdxJPXHLSubBand = class(TdxJPXLowPassSubBand)
  protected
    function GetContextLabel(V, H, D: Byte): Integer; override;
    function GetGainLog: Integer; override;
    function GetHorizontalQuantity: Integer; override;
    function GetVerticalQuantity: Integer; override;
  end;

  { TdxJPXHHSubBand }

  TdxJPXHHSubBand = class(TdxJPXSubBand)
  protected
    function GetContextLabel(V, H, D: Byte): Integer; override;
    function GetGainLog: Integer; override;
    function GetHorizontalQuantity: Integer; override;
    function GetVerticalQuantity: Integer; override;
  end;

  { TdxJPXCoefficientBitModel }

  TdxJPXCoefficientBitModel = class
  strict private type
    TCodeBlockProcessingProc = procedure(AIndex, X, Y: Integer; var ACoefficient: TdxJPXCoefficient) of object;
  strict private
    FArithmeticContext: TdxArithmeticContext;
    FCodeBlockHeight: Integer;
    FCodeBlockWidth: Integer;
    FCoefficients: TdxJPXCoefficients;
    FPassCount: Byte;
    FReader: TdxBigEndianStreamReader;
    FSignContextLabels: array of TBytes;
    FState: TdxArithmeticState;
    FSubBand: TdxJPXSubBand;
    FRunLengthContextIndex: Integer;
    FUniformContextIndex: Integer;
    procedure CalculateCurrentCoefficient(X, Y, AIndex: Integer; out AVContribution, AHContribution: Integer);
  protected
    class function NormalizeContribution(AContribution: Integer): Integer;
    function DoDecode(ACodeBlockCodingStyle: TdxJPXCodeBlockCodingStyle): TdxJPXCoefficients;
    function GetSignContribution(AIndex: Integer): Integer;
    function GetSignBit(X, Y, AIndex: Integer; var ACoefficient: TdxJPXCoefficient): TdxJPXCoefficient;
    procedure CleanUpPass;
    procedure DoMagnitudeRefinementPass(AIndex, X, Y: Integer; var ACoefficient: TdxJPXCoefficient);
    procedure DoSignificancePropagationPass(AIndex, X, Y: Integer; var ACoefficient: TdxJPXCoefficient);
    procedure IterateCodeBlock(AProc: TCodeBlockProcessingProc);
    procedure MagnitudeRefinementPass;
    procedure ReadSignBit(X: Integer; Y: Integer);
    procedure SignificancePropagationPass;
  public
    constructor Create(ACodeBlock: TdxJPXCodeBlock; AStream: TStream; ASubBand: TdxJPXSubBand);
    destructor Destroy; override;

    class function Decode(ACodeBlock: TdxJPXCodeBlock; ASubBand: TdxJPXSubBand;
      ACodeBlockCodingStyle: TdxJPXCodeBlockCodingStyle): TdxJPXCoefficients;
  end;

  { TdxJPXTileComponent }

  TdxJPXTileComponent = class(TdxJPXArea)
  strict private
    FBaseResolutionLevel: TdxJPXBaseResolutionLevel;
    FBitsPerComponent: Integer;
    FCodingStyle: TdxJPXCodingStyleComponent;
    FResolutionLevels: TObjectList<TdxJPXCompositeResolutionLevel>;
    FQuantizationParameters: TdxJPXQuantizationComponentParameters;
    function GetResolutionLevel(Index: Integer): TdxJPXResolutionLevel;
    procedure SetQuantizationParameters(const AValue: TdxJPXQuantizationComponentParameters);
  protected
    function Transform: TSingleDynArray;

    property BitsPerComponent: Integer read FBitsPerComponent;
    property CodingStyle: TdxJPXCodingStyleComponent read FCodingStyle;
    property QuantizationParameters: TdxJPXQuantizationComponentParameters read FQuantizationParameters
      write SetQuantizationParameters;
  public
    constructor Create(ATile: TdxJPXTile; const AComponent: TdxJPXComponent;
      const ACodingStyle: TdxJPXCodingStyleComponent; AQuantizationParameters: TdxJPXQuantizationComponentParameters);
    destructor Destroy; override;

    property ResolutionLevel[Index: Integer]: TdxJPXResolutionLevel read GetResolutionLevel; default;
  end;

  { TdxJPXResolutionLevel }

  TdxJPXResolutionLevel = class(TdxJPXArea)
    strict private
    FCodeBlockHeight: Integer;
    FCodeBlockWidth: Integer;
    FLevelIndex: Integer;
    FPrecinctCount: Integer;
    FPrecinctHeight: Integer;
    FPrecinctHighCount: Integer;
    FPrecinctWideCount: Integer;
    FPrecincts: TObjectList<TdxJPXPrecinct>;
    FPrecinctWidth: Integer;
    function GetPrecinctCount: Integer;
  protected
    procedure AppendPrecincts(ASubBand: TdxJPXSubBand);

    property CodeBlockHeight: Integer read FCodeBlockHeight;
    property CodeBlockWidth: Integer read FCodeBlockWidth;
    property LevelIndex: Integer read FLevelIndex;
    property PrecinctCount: Integer read GetPrecinctCount;
    property PrecinctHighCount: Integer read FPrecinctHighCount;
    property PrecinctWideCount: Integer read FPrecinctWideCount;
    property Precincts: TObjectList<TdxJPXPrecinct> read FPrecincts;
  public
    constructor Create(AComponent: TdxJPXTileComponent; ALevelIndex: Integer;
      const ACodingStyle: TdxJPXCodingStyleComponent); virtual;
    destructor Destroy; override;
  end;

  { TdxJPXBaseResolutionLevel }

  TdxJPXBaseResolutionLevel = class(TdxJPXResolutionLevel)
  strict private
    FLLSubBand: TdxJPXLLSubBand;
  protected
    property LLSubBand: TdxJPXLLSubBand read FLlSubBand;
  public
    constructor Create(AComponent: TdxJPXTileComponent; ALevel: Integer;
      const ACodingStyle: TdxJPXCodingStyleComponent); override;
    destructor Destroy; override;
  end;

  { TdxJPXCompositeResolutionLevel }

  TdxJPXCompositeResolutionLevel = class(TdxJPXResolutionLevel)
  strict private
    FLHSubBand: TdxJPXLHSubBand;
    FHLSubBand: TdxJPXHLSubBand;
    FHHSubBand: TdxJPXHHSubBand;
  protected
    property LHSubBand: TdxJPXLHSubBand read FLHSubBand;
    property HLSubBand: TdxJPXHLSubBand read FHLSubBand;
    property HHSubBand: TdxJPXHHSubBand read FHHSubBand;
  public
    constructor Create(AComponent: TdxJPXTileComponent; ALevel: Integer;
      const ACodingStyle: TdxJPXCodingStyleComponent); reintroduce;
    destructor Destroy; override;
  end;

  { TdxBitReader }

  TdxBitReader = class
  strict private
    FCurrentBitMask: Integer;
    FCurrentByte: Byte;
    FHighBitMask: Integer;
    FStream: TStream;
  protected
    function GoToNextByte: Boolean; virtual;

    function GetBit: Integer;
    function GetInteger(ABitCount: Integer): Integer;
    function IgnoreExtendedBits: Boolean;

    property CurrentBitMask: Integer read FCurrentBitMask write FCurrentBitMask;
    property CurrentByte: Byte read FCurrentByte;
  public
    constructor Create(AStream: TStream);
  end;

  { TdxJPXBitReader }

  TdxJPXBitReader = class(TdxBitReader)
  protected
    function GoToNextByte: Boolean; override;
    procedure AlignToByte;
  end;

  { TdxJPXTileComponentDataConstructor }

  TdxJPXTileComponentDataConstructor = class
  strict private type
  strict private
    FData: TdxJPXTileComponentDataArray;
    FTileComponents: TObjectList<TdxJPXTileComponent>;
    function CreateData: TdxJPXTileComponentDataArray;
  protected
    procedure AddTileComponentData(AComponentIndex: Integer);

    property Data: TdxJPXTileComponentDataArray read FData;
  public
    constructor Create(ATileComponents: TObjectList<TdxJPXTileComponent>);
    class function BuildData(ATileComponents: TObjectList<TdxJPXTileComponent>): TdxJPXTileComponentDataArray;
  end;

  { TdxJPXDiscreteWaveletTransformation }

  TdxJPXDiscreteWaveletTransformation = class
  strict private
    FExtendSampleCount: Integer;
    FSubBandCoefficients: TdxJPXSubBandCoefficients;
    procedure DoExtend(const ABuffer: TSingleDynArray; ASize: Integer);
  protected
    procedure Filter(const Y: TSingleDynArray; I0, I1: Integer); virtual;

    procedure Append(const AHlCoefficients, ALhCoefficients, AHhCoefficients: TdxJPXSubBandCoefficients);
    procedure HorizontalReconstruction;
    procedure VerticalReconstruction;

    property SubBandCoefficients: TdxJPXSubBandCoefficients read FSubBandCoefficients;
  public
    constructor Create; overload;
    constructor Create(const ALLSubBandCoefficients: TdxJPXSubBandCoefficients); overload;

    class procedure Extend(const ABuffer: TSingleDynArray; ASize: Integer);
  end;

  { TdxJPXIrreversibleDiscreteWaveletTransformation }

  TdxJPXIrreversibleDiscreteWaveletTransformation = class(TdxJPXDiscreteWaveletTransformation)
  protected
    procedure Filter(const Y: TSingleDynArray; I0, I1: Integer); override;
  end;

  { TdxJPXReversibleDiscreteWaveletTransformation }

  TdxJPXReversibleDiscreteWaveletTransformation = class(TdxJPXDiscreteWaveletTransformation)
  protected
    procedure Filter(const Y: TSingleDynArray; I0, I1: Integer); override;
  end;

  { TdxJPXColorTransformation }

  TdxJPXColorTransformation = class
  protected
    FComponentsData: TdxJPXTileComponentDataArray;
    FHeight: Integer;
    FShift: TIntegerDynArray;
    FWidth: Integer;

    class function Normalize(AValue: Single): Byte; static;
    function TransformColor(V, H: Integer): TBytes; virtual; abstract;

    property ComponentsData: TdxJPXTileComponentDataArray read FComponentsData;
    property Shift: TIntegerDynArray read FShift write FShift;
    property Width: Integer read FWidth;
  public
    constructor Create(AWidth, AHeight: Integer; const AComponentsData: TdxJPXTileComponentDataArray); virtual;
    destructor Destroy; override;

    class function Parse(ATile: TdxJPXTile): TdxJPXColorTransformation;

    procedure Transform(const AResult: TBytes; AStartOffset, ARowWidth: Integer);
  end;

  { TdxJPXGrayColorTransformation }

  TdxJPXGrayColorTransformation = class(TdxJPXColorTransformation)
  strict private
    FBuffer: TBytes;
  protected
    function TransformColor(V, H: Integer): TBytes; override;
  public
    constructor Create(AWidth, AHeight: Integer; const AComponentData: TdxJPXTileComponentDataArray); override;
  end;

  { TdxJPXMultipleComponentTransformation }

  TdxJPXMultipleComponentTransformation = class(TdxJPXColorTransformation)
  strict private
    FBuffer: TBytes;
    FThirdComponentData: TSingleDynArray;
    FSecondComponentData: TSingleDynArray;
    FFirstComponentsData: TSingleDynArray;
  protected
    function TransformColor(V, H: Integer): TBytes; override;
    procedure Transform(const AData: TBytes; Y0, Y1, Y2: Single); virtual;
  public
    constructor Create(AWidth, AHeight: Integer; const AComponentData: TdxJPXTileComponentDataArray); override;
  end;

  { TdxJPXReversibleColorTransformation }

  TdxJPXReversibleColorTransformation = class(TdxJPXMultipleComponentTransformation)
  protected
    procedure Transform(const AData: TBytes; Y0, Y1, Y2: Single); override;
  end;

  { TdxJPXIrreversibleColorTransformation }

  TdxJPXIrreversibleColorTransformation = class(TdxJPXMultipleComponentTransformation)
  protected
    procedure Transform(const AData: TBytes; Y0, Y1, Y2: Single); override;
  end;

function dxDoubleToByte(AValue: Double): Byte;

implementation

uses
  Math, cxGeometry, cxVariants;

const
  dxcInvalidIntegerValue = -MaxInt;
type
  TInterfacedObjectAccess = class(TInterfacedObject);

  { TdxArithmeticQeConsts }

  TdxArithmeticQeConsts = class
  strict private
    FValues: TList<TdxArithmeticQe>;
    function GetValue(AIndex: Integer): TdxArithmeticQe; inline;
  public
    constructor Create;
    destructor Destroy; override;

    property Values[Index: Integer]: TdxArithmeticQe read GetValue; default;
  end;

var
  dxgArithmeticQeConsts: TdxArithmeticQeConsts;

function IsValidIntegerValue(AValue: Integer): Boolean;
begin
  Result := AValue <> dxcInvalidIntegerValue;
end;

function dxDoubleToByte(AValue: Double): Byte;

  function ConvertPositiveValue(V: Double): Byte;
  var
    AFraction: Double;
  begin
    Result := 0;
    if V < MaxDouble then
    begin
      Result := Trunc(V);
      AFraction := V - Result;
      if (AFraction > 0.5) or (AFraction = 0.5) and ((Result and 1) <> 0) then
        Inc(Result);
    end
  end;

  function ConvertNegativeValue(V: Double): Byte;
  var
    AFraction: Double;
  begin
    Result := 0;
    if V >= -MaxDouble then
    begin
      Result := Trunc(V);
      AFraction := V - Result;
      if (AFraction < -0.5) or (AFraction = -0.5) and ((Result and 1) <> 0) then
        Dec(Result);
    end;
  end;

begin
  if AValue >= 0 then
    Result := ConvertPositiveValue(AValue)
  else
    Result := ConvertNegativeValue(AValue);
end;

procedure dxJPXRaiseException;
begin
  raise EdxJPXException.Create('Invalid format');
end;

{ TdxArithmeticQeConsts }

constructor TdxArithmeticQeConsts.Create;
begin
  inherited Create;
  FValues := TList<TdxArithmeticQe>.Create;
  FValues.Add(TdxArithmeticQe.Create($56010000, 1 shl 1, 1 shl 1, 1));
  FValues.Add(TdxArithmeticQe.Create($34010000, 2 shl 1, 6 shl 1, 0));
  FValues.Add(TdxArithmeticQe.Create($18010000, 3 shl 1, 9 shl 1, 0));
  FValues.Add(TdxArithmeticQe.Create($0AC10000, 4 shl 1, 12 shl 1, 0));
  FValues.Add(TdxArithmeticQe.Create($05210000, 5 shl 1, 29 shl 1, 0));
  FValues.Add(TdxArithmeticQe.Create($02210000, 38 shl 1, 33 shl 1, 0));
  FValues.Add(TdxArithmeticQe.Create($56010000, 7 shl 1, 6 shl 1, 1));
  FValues.Add(TdxArithmeticQe.Create($54010000, 8 shl 1, 14 shl 1, 0));
  FValues.Add(TdxArithmeticQe.Create($48010000, 9 shl 1, 14 shl 1, 0));
  FValues.Add(TdxArithmeticQe.Create($38010000, 10 shl 1, 14 shl 1, 0));
  FValues.Add(TdxArithmeticQe.Create($30010000, 11 shl 1, 17 shl 1, 0));
  FValues.Add(TdxArithmeticQe.Create($24010000, 12 shl 1, 18 shl 1, 0));
  FValues.Add(TdxArithmeticQe.Create($1C010000, 13 shl 1, 20 shl 1, 0));
  FValues.Add(TdxArithmeticQe.Create($16010000, 29 shl 1, 21 shl 1, 0));
  FValues.Add(TdxArithmeticQe.Create($56010000, 15 shl 1, 14 shl 1, 1));
  FValues.Add(TdxArithmeticQe.Create($54010000, 16 shl 1, 14 shl 1, 0));
  FValues.Add(TdxArithmeticQe.Create($51010000, 17 shl 1, 15 shl 1, 0));
  FValues.Add(TdxArithmeticQe.Create($48010000, 18 shl 1, 16 shl 1, 0));
  FValues.Add(TdxArithmeticQe.Create($38010000, 19 shl 1, 17 shl 1, 0));
  FValues.Add(TdxArithmeticQe.Create($34010000, 20 shl 1, 18 shl 1, 0));
  FValues.Add(TdxArithmeticQe.Create($30010000, 21 shl 1, 19 shl 1, 0));
  FValues.Add(TdxArithmeticQe.Create($28010000, 22 shl 1, 19 shl 1, 0));
  FValues.Add(TdxArithmeticQe.Create($24010000, 23 shl 1, 20 shl 1, 0));
  FValues.Add(TdxArithmeticQe.Create($22010000, 24 shl 1, 21 shl 1, 0));
  FValues.Add(TdxArithmeticQe.Create($1C010000, 25 shl 1, 22 shl 1, 0));
  FValues.Add(TdxArithmeticQe.Create($18010000, 26 shl 1, 23 shl 1, 0));
  FValues.Add(TdxArithmeticQe.Create($16010000, 27 shl 1, 24 shl 1, 0));
  FValues.Add(TdxArithmeticQe.Create($14010000, 28 shl 1, 25 shl 1, 0));
  FValues.Add(TdxArithmeticQe.Create($12010000, 29 shl 1, 26 shl 1, 0));
  FValues.Add(TdxArithmeticQe.Create($11010000, 30 shl 1, 27 shl 1, 0));
  FValues.Add(TdxArithmeticQe.Create($0AC10000, 31 shl 1, 28 shl 1, 0));
  FValues.Add(TdxArithmeticQe.Create($09C10000, 32 shl 1, 29 shl 1, 0));
  FValues.Add(TdxArithmeticQe.Create($08A10000, 33 shl 1, 30 shl 1, 0));
  FValues.Add(TdxArithmeticQe.Create($05210000, 34 shl 1, 31 shl 1, 0));
  FValues.Add(TdxArithmeticQe.Create($04410000, 35 shl 1, 32 shl 1, 0));
  FValues.Add(TdxArithmeticQe.Create($02A10000, 36 shl 1, 33 shl 1, 0));
  FValues.Add(TdxArithmeticQe.Create($02210000, 37 shl 1, 34 shl 1, 0));
  FValues.Add(TdxArithmeticQe.Create($01410000, 38 shl 1, 35 shl 1, 0));
  FValues.Add(TdxArithmeticQe.Create($01110000, 39 shl 1, 36 shl 1, 0));
  FValues.Add(TdxArithmeticQe.Create($00850000, 40 shl 1, 37 shl 1, 0));
  FValues.Add(TdxArithmeticQe.Create($00490000, 41 shl 1, 38 shl 1, 0));
  FValues.Add(TdxArithmeticQe.Create($00250000, 42 shl 1, 39 shl 1, 0));
  FValues.Add(TdxArithmeticQe.Create($00150000, 43 shl 1, 40 shl 1, 0));
  FValues.Add(TdxArithmeticQe.Create($00090000, 44 shl 1, 41 shl 1, 0));
  FValues.Add(TdxArithmeticQe.Create($00050000, 45 shl 1, 42 shl 1, 0));
  FValues.Add(TdxArithmeticQe.Create($00010000, 45 shl 1, 43 shl 1, 0));
  FValues.Add(TdxArithmeticQe.Create($56010000, 46 shl 1, 46 shl 1, 0));
  FValues.TrimExcess;
end;

destructor TdxArithmeticQeConsts.Destroy;
begin
  FreeAndNil(FValues);
  inherited Destroy;
end;

function TdxArithmeticQeConsts.GetValue(AIndex: Integer): TdxArithmeticQe;
begin
  Result := FValues[AIndex];
end;

{ TdxBigEndianStreamReader }

constructor TdxBigEndianStreamReader.Create(AStream: TStream);
begin
  inherited Create;
  FStream := AStream;
end;

destructor TdxBigEndianStreamReader.Destroy;
begin
  FStream := nil;
  inherited Destroy;
end;

function TdxBigEndianStreamReader.IsEOF: Boolean;
begin
  Result := Position = Length;
end;

function TdxBigEndianStreamReader.ReadBytes: TBytes;
begin
  Result := ReadBytes(FStream.Size - FStream.Position);
end;

function TdxBigEndianStreamReader.ReadBytes(ACount: Integer): TBytes;
begin
  SetLength(Result, ACount);
  FStream.ReadBuffer(Result[0], ACount);
end;

function TdxBigEndianStreamReader.ReadInt16: Integer;
begin
  Result := 256 * ReadByte + ReadByte;
end;

function TdxBigEndianStreamReader.ReadInt32: Integer;
begin
  Result := ReadInt(4);
end;

function TdxBigEndianStreamReader.ReadInt(ACount: Integer): Integer;
begin
  if not IsEOF then
  begin
    Result := ReadByte;
    if ACount > 1 then
    begin
      Result := 256 * Result + ReadByte;
      if ACount <> 2 then
      begin
        Result := 256 * Result + ReadByte;
        Result := 256 * Result + ReadByte;
      end;
    end;
  end
  else
    Result := -1;
end;

function TdxBigEndianStreamReader.ReadByte: Byte;
begin
  ReadByteProc(FStream, Result);
end;

procedure TdxBigEndianStreamReader.FreeStream;
begin
  FreeAndNil(FStream);
end;

procedure TdxBigEndianStreamReader.Skip(ACount: Integer);
begin
  FStream.Position := FStream.Position + ACount;
end;

function TdxBigEndianStreamReader.GetLength: Int64;
begin
  Result := FStream.Size;
end;

function TdxBigEndianStreamReader.GetPosition: Int64;
begin
  Result := FStream.Position;
end;

{ TdxArithmeticState }

constructor TdxArithmeticState.Create(AReader: TdxBigEndianStreamReader);

  procedure InitializeBuffer(var ABuffer: LongWord);
  begin
    ABuffer := 255;
    if not FReader.IsEOF then
      ABuffer := FReader.ReadByte;
  end;

begin
  inherited Create;
  FA := $80000000;
  FReader := AReader;
  if AReader.Length > 0 then
  begin
    InitializeBuffer(FBuffer0);
    InitializeBuffer(FBuffer1);
    FC := (FBuffer0 xor $FF) shl 16;
    ReadByte;
    FC := FC shl 7;
    FCt := FCt - 7;
  end;
end;

destructor TdxArithmeticState.Destroy;
begin
  FReader := nil;
  inherited Destroy;
end;

procedure TdxArithmeticState.LoadBuffer;
begin
  FBuffer0 := FBuffer1;
  FBuffer1 := $FF;
  if not FReader.IsEOF then
    FBuffer1 := FReader.ReadByte;
end;

procedure TdxArithmeticState.ReadByte;
begin
  if FBuffer0 = $FF then
  begin
    if FBuffer1 > $8F then
      FCt := 8
    else
    begin
      LoadBuffer;
      FC := FC + $FE00 - (FBuffer0 shl 9);
      FCt := 7;
    end;
  end
  else
  begin
    LoadBuffer;
    FC := FC + $FF00 - (FBuffer0 shl 8);
    FCt := 8;
  end;
end;

function TdxArithmeticState.Decode(const ACx: TBytes; AIndex: Integer): Integer;
var
  APqe: TdxArithmeticQe;
  AMpsCX, ACLessABit, AALessQeBit: Integer;
  AQe: LongWord;
  ACLessA, AALessQe: Boolean;
begin
  APqe := dxgArithmeticQeConsts[ACx[AIndex] shr 1];
  AMpsCX := ACx[AIndex] and 1;
  AQe := APqe.Qe;
  FA := FA - AQe;
  ACLessA := FC < FA;
  AALessQe := FA < AQe;
  ACLessABit := IfThen(ACLessA, 1, 0);
  AALessQeBit := IfThen(AALessQe, 1, 0);
  if not ACLessA then
  begin
    FC := FC - FA;
    FA := AQe;
  end
  else
    if (FA and $80000000) <> 0 then
      Exit(AMpsCX);
  ACx[AIndex] := IfThen(ACLessA xor AALessQe, APqe.MpsXor or AMpsCX, APqe.LpsXor or AMpsCX xor APqe.Switch);
  Renormalized;
  Result := AMpsCX xor (AALessQeBit xor (not ACLessABit and 1));
end;

procedure TdxArithmeticState.Renormalized;
begin
  repeat
    if FCt = 0 then
      ReadByte;
    FA := FA shl 1;
    FC := FC shl 1;
    Dec(FCt);
  until (FA and $80000000) <> 0;
end;

{ TdxArithmeticQe }

class function TdxArithmeticQe.Create(AQe: LongWord; AMpsXor, ALpsXor, ASwitch: Byte): TdxArithmeticQe;
begin
  Result.FQe := AQe;
  Result.FMpsXor := AMpsXor;
  Result.FLpsXor := ALpsXor;
  Result.FSwitch := ASwitch;
end;

{ TdxArithmeticContext }

constructor TdxArithmeticContext.Create(AState: TdxArithmeticState; const AContext: TBytes);
begin
  inherited Create;
  FState := AState;
  FContext := AContext;
end;

constructor TdxArithmeticContext.Create(AState: TdxArithmeticState; ALength: Integer);
var
  AContext: TBytes;
begin
  SetLength(AContext, 1 shl ALength);
  FillChar(AContext[0], Length(AContext), 0);
  Create(AState, AContext);
end;

destructor TdxArithmeticContext.Destroy;
begin
  SetLength(FContext, 0);
  inherited Destroy;
end;

function TdxArithmeticContext.DecodeBit(ACx: Integer): Integer;
begin
  Result := FState.Decode(FContext, ACx);
end;

{ TdxJPXPacketPosition }

procedure TdxJPXPacketPosition.Initialize(AComponent, ALayer, AResolutionLevel, APrecinct: Integer);
begin
  FComponent := AComponent;
  FLayer := ALayer;
  FPrecinct := APrecinct;
  FResolutionLevel := AResolutionLevel;
end;

{ TdxJPXPrecinctSize }

procedure TdxJPXPrecinctSize.Initialize(AValue: Integer);
begin
  FWidthExponent := AValue shr 4;
  FHeightExponent := AValue and $0F;
end;

{ TdxJPXComponent }

procedure TdxJPXComponent.Initialize(ABitsPerComponent, AHorizontalSeparation, AVerticalSeparation: Integer);
begin
  FIsSigned := (ABitsPerComponent and $80) <> 0;
  FBitsPerComponent := (ABitsPerComponent and $7F) + 1;
  FHorizontalSeparation := AHorizontalSeparation;
  FVerticalSeparation := AVerticalSeparation;
end;

{ TdxJPXDefaultCodingStyle }

procedure TdxJPXDefaultCodingStyle.Initialize(AReader: TdxBigEndianStreamReader);
begin
  FProgressionOrder := TdxJPXProgressionOrder(AReader.ReadByte);
  FLayerCount := AReader.ReadInt16;
  FMultipleTransformation := AReader.ReadByte > 0;
end;

{ TdxJPXCodingStyleComponent }

procedure TdxJPXCodingStyleComponent.Initialize(AReader: TdxBigEndianStreamReader; AUsePrecincts: Boolean);
var
  I: Integer;
  AValue: Byte;
begin
  FDecompositionLevelCount := AReader.ReadByte;
  FCodeBlockWidthExponent := (AReader.ReadByte and $F) + 2;
  FCodeBlockHeightExponent := (AReader.ReadByte and $F) + 2;
  FCodeBlockCodingStyle := TdxJPXCodeBlockCodingStyle(AReader.ReadByte);
  FUseWaveletTransformation := AReader.ReadByte > 0;
  SetLength(FPrecinctSizes, FDecompositionLevelCount + 1);
  for I := 0 to FDecompositionLevelCount + 1 - 1 do
  begin
    AValue := $FF;
    if AUsePrecincts then
      AValue := AReader.ReadByte;
    FPrecinctSizes[I].Initialize(AValue);
  end;
end;

{ TdxJPXCodeBlockChunk }

procedure TdxJPXCodeBlockChunk.Initialize(const AData: TBytes; ACodingPassCount: Byte);
begin
  FCodingPassCount := ACodingPassCount;
  FData := AData;
end;

{ TdxJPXTileComponentData }

procedure TdxJPXTileComponentData.Initialize(const AData: TSingleDynArray; ABitsPerComponent: Integer);
var
  L: Integer;
begin
  FBitsPerComponent := ABitsPerComponent;
  L := Length(AData);
  SetLength(FData, L);
  cxCopyData(@AData[0], @FData[0], SizeOF(Single) * L);
end;

{ TdxJPXSize }

procedure TdxJPXSize.Initialize(AReader: TdxBigEndianStreamReader);
var
  I, AComponentCount, ABitsPerComponent, AHorizontalSeparation, AVerticalSeparation: Integer;
begin
  FGridWidth := AReader.ReadInt32;
  FGridHeight := AReader.ReadInt32;
  FGridHorizontalOffset := AReader.ReadInt32;
  FGridVerticalOffset := AReader.ReadInt32;
  FTileWidth := AReader.ReadInt32;
  FTileHeight := AReader.ReadInt32;
  FTileHorizontalOffset := AReader.ReadInt32;
  FTileVerticalOffset := AReader.ReadInt32;
  AComponentCount := AReader.ReadInt16;
  SetLength(FComponents, AComponentCount);
  for I := 0 to AComponentCount - 1 do
  begin
    ABitsPerComponent := AReader.ReadByte;
    AHorizontalSeparation := AReader.ReadByte;
    AVerticalSeparation := AReader.ReadByte;
    FComponents[I].Initialize(ABitsPerComponent, AHorizontalSeparation, AVerticalSeparation);
  end;
  if (FGridWidth < 1) or (FGridHeight < 1) or (FTileWidth < 1) or (FTileHeight < 1) or (AComponentCount < 1) then
    dxJPXRaiseException;
end;

{ TdxJPXQuantizationStepSize }

procedure TdxJPXQuantizationStepSize.Initialize(AEpsilon: Byte; AMu: SmallInt);
begin
  FEpsilon := AEpsilon;
  FMu := AMu;
end;

procedure TdxJPXQuantizationStepSize.Initialize(AValue1, AValue2: Byte);
begin
  Initialize(AValue1 shr 3, AValue1 and $7 shl 8 or AValue2);
end;

{ TdxJPXCoefficient }

function TdxJPXCoefficient.ToInteger: Integer;
begin
  Result := Significance * Magnitude * (1 - Sign);
end;

procedure TdxJPXCoefficient.IncrementDiagonalNeighborSignificance;
begin
  FNeighborSignificance := FNeighborSignificance + 16;
end;

procedure TdxJPXCoefficient.IncrementHorizontalNeighborSignificance;
begin
  FNeighborSignificance := FNeighborSignificance + 4;
end;

procedure TdxJPXCoefficient.IncrementVerticalNeighborSignificance;
begin
  Inc(FNeighborSignificance);
end;

procedure TdxJPXCoefficient.Initialize;
begin
  Flags := 0;
  BitsDecoded := 0;
  CalculatedSignificancesStepIndex := 0;
  Flags := 0;
  Magnitude := 0;
  NotZeroContextLabelPassIndex := 0;
  FNeighborSignificance := 0;
end;

function TdxJPXCoefficient.GetDiagonalNeighborSignificance: Byte;
begin
  Result := FNeighborSignificance shr 4;
end;

function TdxJPXCoefficient.GetHorizontalNeighborSignificance: Byte;
begin
  Result := FNeighborSignificance and $C shr 2;
end;

function TdxJPXCoefficient.GetIsNotFirstRefinement: Boolean;
begin
  Result := (Flags and IsNotFirstRefinementMask) > 0;
end;

function TdxJPXCoefficient.GetSign: Byte;
begin
  Result := Flags and SignMask;
end;

function TdxJPXCoefficient.GetSignificance: Byte;
begin
  Result := Byte(Flags and SignificanceMask);
end;

function TdxJPXCoefficient.GetVerticalNeighborSignificance: Byte;
begin
  Result := FNeighborSignificance and $3;;
end;

procedure TdxJPXCoefficient.SetSign(const AValue: Byte);
begin
  if AValue <> 0 then
    Flags := Flags or SignMask;
end;

procedure TdxJPXCoefficient.SetSignificance(const AValue: Byte);
begin
  if AValue <> 0 then
    Flags := Flags or SignificanceMask;
end;

procedure TdxJPXCoefficient.SetIsNotFirstRefinement(const AValue: Boolean);
begin
  if AValue then
    Flags := Flags or IsNotFirstRefinementMask;
end;

{ TdxJPXImage }

constructor TdxJPXImage.Create;
begin
  inherited Create;
  FQuantizationParameters := TObjectList<TdxJPXQuantizationComponentParameters>.Create;
end;

destructor TdxJPXImage.Destroy;
begin
  ClearTiles;
  FreeAndNil(FQuantizationParameters);
  inherited Destroy;
end;

class function TdxJPXImage.Decode(const AData: TBytes; out AComponentCount: Integer): TBytes;
var
  AImage: TdxJPXImage;
begin
  AImage := TdxJPXImage.Create;
  try
    Result := AImage.DoDecode(AData, AComponentCount);
  finally
    AImage.Free;
  end;
end;

procedure TdxJPXImage.ClearTiles;
begin
  FreeAndNil(FTiles);
end;

procedure TdxJPXImage.ReadCodingStyleComponents(AReader: TdxBigEndianStreamReader);
var
  I, L: Integer;
  ADefaultCodingStyleComponent: TdxJPXCodingStyleComponent;
  AFlags: TdxJPXCodingStyleParameter;
begin
  AFlags := TdxJPXCodingStyleParameter(AReader.ReadByte);
  FDefaultCodingStyle.Initialize(AReader);
  ADefaultCodingStyleComponent.Initialize(AReader, AFlags = cspUsePrecincts);
  L := Length(Size.Components);
  SetLength(FCodingStyleComponents, L);
  for I := 0 to L - 1 do
    FCodingStyleComponents[I] := ADefaultCodingStyleComponent;
end;

procedure TdxJPXImage.ReadQuantizationParameters(AReader: TdxBigEndianStreamReader; ADataLength: Integer);
var
  I, AByteCount: Integer;
  AStartPosition: Int64;
  AQuantizationParameters: TdxJPXQuantizationComponentParameters;
begin
  FQuantizationParameters.Clear;
  AStartPosition := AReader.Position;
  AByteCount := ADataLength - AReader.Position + AStartPosition;
  AQuantizationParameters := TdxJPXQuantizationComponentParameters.CreateParameters(AReader, AByteCount);
  try
    if AQuantizationParameters <> nil then
      for I := 0 to Length(Size.Components) - 1 do
        FQuantizationParameters.Add(AQuantizationParameters.Clone);
  finally
    AQuantizationParameters.Free;
  end;
  if AReader.Position - AStartPosition > ADataLength then
    dxJPXRaiseException;
end;

function TdxJPXImage.GetTiles: TObjectList<TdxJPXTile>;
begin
  if FTiles = nil then
    CreateTiles;
  Result := FTiles;
end;

procedure TdxJPXImage.SetSize(const AValue: TdxJPXSize);
var
  I, ALength: Integer;
begin
  FSize := AValue;
  ALength := Length(FSize.Components);
  SetLength(FCodingStyleComponents, ALength);
  FQuantizationParameters.Clear;
  for I := 0 to ALength - 1 do
    FQuantizationParameters.Add(nil);
  FTileCount.X := Ceil((FSize.GridWidth - Max(FSize.TileHorizontalOffset, FSize.GridHorizontalOffset)) / FSize.TileWidth);
  FTileCount.Y := Ceil((FSize.GridHeight - Max(FSize.TileVerticalOffset, FSize.GridVerticalOffset)) / FSize.TileHeight);
end;

function TdxJPXImage.DoDecode(const AData: TBytes; out AComponentCount: Integer): TBytes;

  procedure InternalDecode(AReader: TdxBigEndianStreamReader);
  var
    AIsCodeStream: Boolean;
  begin
    AIsCodeStream := AReader.ReadByte = $FF;
    AReader.Stream.Position := 0;
    if AIsCodeStream then
      TdxJPXContiguousCodeStreamBox.PopulateImage(AReader, AReader.Length, Self)
    else
      while not AReader.IsEOF do
        TdxJPXCustomBox.ReadImage(AReader, Self);
  end;

var
  ABytesStream: TBytesStream;
  AReader: TdxBigEndianStreamReader;
begin
  ABytesStream := TBytesStream.Create(AData);
  try
    AReader := TdxBigEndianStreamReader.Create(ABytesStream);
    try
      InternalDecode(AReader);
      Result := GetData;
      AComponentCount := Length(Size.Components);
    finally
      AReader.Free;
    end;
  finally
    ABytesStream.Free;
  end;
end;

function TdxJPXImage.GetData: TBytes;
var
  I: Integer;
  AComponentCount, AGridWidth: Integer;
  ATile: TdxJPXTile;
  ATransformation: TdxJPXColorTransformation;
begin
  AComponentCount := Length(FCodingStyleComponents);
  AGridWidth := FSize.GridWidth;
  SetLength(Result, AComponentCount * AGridWidth * FSize.GridHeight);
  for I := 0 to FTiles.Count - 1 do
  begin
    ATile := FTiles[I];
    ATransformation := TdxJPXColorTransformation.Parse(ATile);
    try
      ATransformation.Transform(Result, AComponentCount * (ATile.X0 + ATile.Y0 * AGridWidth), AGridWidth);
    finally
      ATransformation.Free;
    end;
  end;
end;

procedure TdxJPXImage.CreateTiles;
var
  I, ATileCount: Integer;
begin
  FreeAndNil(FTiles);
  FTiles := TObjectList<TdxJPXTile>.Create;
  ATileCount := TileCount.X * TileCount.Y;
  for I := 0 to ATileCount - 1 do
    FTiles.Add(TdxJPXTile.Create(Self, I));
end;

{ TdxJPXTagTreeNode }

procedure TdxJPXTagTreeNode.Initialize(AValue: Integer; AIsDefined: Boolean);
begin
  FInitialized := True;
  FValue := AValue;
  FIsDefined := AIsDefined;
end;

{ TdxJPXTagTree }

constructor TdxJPXTagTree.Create(AWidth: Integer; AHeight: Integer);
var
  I: Integer;
begin
  inherited Create;
  FIndexStack := TStack<TPoint>.Create;
  FLevelCount := Ceil(LogN(2, Max(AWidth, AHeight))) + 1;
  SetLength(FData, FLevelCount);
  for I := FLevelCount - 1 downto 0 do
  begin
    SetLength(FData[I], AWidth, AHeight);
    AWidth := Ceil(AWidth / 2.0);
    AHeight := Ceil(AHeight / 2.0);
  end;
end;

destructor TdxJPXTagTree.Destroy;
begin
  SetLength(FData, 0);
  FreeAndNil(FIndexStack);
  inherited Destroy;
end;

function TdxJPXTagTree.Read(ABitReader: TdxBitReader; X, Y: Integer): Integer;
begin
  Result := GetPreviousValue(X, Y).Value;
  while FCurrentLevel < FLevelCount do
    if ABitReader.GetBit = 1 then
      SetNextValue(Result)
    else
      Inc(Result);
end;

function TdxJPXTagTree.ReadInclusion(ABitReader: TdxBitReader; X, Y, AMaxValue: Integer): Integer;
var
  APreviousValue: TdxJPXTagTreeNode;
  AIndex: TPoint;
begin
  APreviousValue := GetPreviousValue(X, Y);
  Result := APreviousValue.Value;
  if APreviousValue.Value <= AMaxValue then
  begin
    if (FCurrentLevel > 0) and not APreviousValue.IsDefined then
      Dec(FCurrentLevel);
    Result := APreviousValue.Value;
    while FCurrentLevel < FLevelCount do
      if ABitReader.GetBit = 1 then
        APreviousValue := SetNextValue(Result)
      else
      begin
        Inc(Result);
        if Result > AMaxValue then
        begin
          AIndex := FIndexStack.Pop;
          FData[FCurrentLevel][AIndex.X, AIndex.Y].Initialize(Result, False);
          Break;
        end;
      end;
  end;
end;

function TdxJPXTagTree.GetPreviousValue(X: Integer; Y: Integer): TdxJPXTagTreeNode;
var
  ADataValue: TdxJPXTagTreeNode;
  ATreeIndex: TPoint;
begin
  FCurrentLevel := FLevelCount - 1;
  repeat
    ADataValue := FData[FCurrentLevel][X, Y];
    if not ADataValue.Initialized or not ADataValue.IsDefined then
    begin
      ATreeIndex := cxPoint(X, Y);
      FIndexStack.Push(ATreeIndex);
    end;
    X := X shr 1;
    Y := Y shr 1;
    if not ADataValue.Initialized then
      Dec(FCurrentLevel);
  until ADataValue.Initialized or (FCurrentLevel < 0);
  Inc(FCurrentLevel);
  if ADataValue.Initialized then
    Result := ADataValue
  else
    Result.Initialize(0, True);
end;

function TdxJPXTagTree.SetNextValue(AValue: Integer): TdxJPXTagTreeNode;
var
  AIndex: TPoint;
begin
  AIndex := FIndexStack.Pop;
  FData[FCurrentLevel][AIndex.X, AIndex.Y].Initialize(AValue, True);
  Result := FData[FCurrentLevel][AIndex.X, AIndex.Y];
  Inc(FCurrentLevel);
end;

{ TdxJPXBox }

class constructor TdxJPXCustomBox.Create;
begin
  FDictionary := TDictionary<Integer, TdxJPXCustomBoxClass>.Create;
  FDictionary.Add(TdxJPXSignatureBox.ID, TdxJPXSignatureBox);
  FDictionary.Add(TdxJPXFileTypeBox.ID, TdxJPXFileTypeBox);
  FDictionary.Add(TdxJPXImageHeaderBox.ID, TdxJPXImageHeaderBox);
  FDictionary.Add(TdxJPXColorSpecificationBox.ID, TdxJPXColorSpecificationBox);
  FDictionary.Add(TdxJPXJP2HeaderBox.ID, TdxJPXJP2HeaderBox);
  FDictionary.Add(TdxJPXContiguousCodeStreamBox.ID, TdxJPXContiguousCodeStreamBox);
end;

class destructor TdxJPXCustomBox.Destroy;
begin
  FreeAndNil(FDictionary);
end;

class function TdxJPXCustomBox.ID: Integer;
begin
  Result := -1;
end;

class procedure TdxJPXCustomBox.ReadImage(AReader: TdxBigEndianStreamReader; AImage: TdxJPXImage);
var
  ABoxClass: TdxJPXCustomBoxClass;
begin
  DoReadImage(AReader, AImage, ABoxClass);
end;

class procedure TdxJPXCustomBox.PopulateImage(AReader: TdxBigEndianStreamReader; ALength: Integer; AImage: TdxJPXImage);
begin
// do nothing
end;

class procedure TdxJPXCustomBox.DoReadImage(AReader: TdxBigEndianStreamReader; AImage: TdxJPXImage;
  out ABoxClass: TdxJPXCustomBoxClass);
var
  ALength: Integer;
begin
  ABoxClass := GetBoxClass(ReadBoxType(AReader, ALength));
  ABoxClass.PopulateImage(AReader, ALength, AImage);
end;

class function TdxJPXCustomBox.GetBoxClass(AType: Integer): TdxJPXCustomBoxClass;
begin
  if not FDictionary.TryGetValue(AType, Result) then
    Result := TdxJPXUnknownBox;
end;

class function TdxJPXCustomBox.ReadBoxType(AReader: TdxBigEndianStreamReader; var ALength: Integer): Integer;
var
  AActualLength: Integer;
begin
  ALength := AReader.ReadInt32;
  Result := AReader.ReadInt32;
  AActualLength := Integer((AReader.Length - AReader.Position));
  if ALength = 0 then
    ALength := AActualLength
  else
  begin
    Dec(ALength, 8);
    if ALength > AActualLength then
      dxJPXRaiseException;
  end;
end;

{ TdxJPXSignatureBox }

class function TdxJPXSignatureBox.ID: Integer;
begin
  Result := $6A502020;
end;

class procedure TdxJPXSignatureBox.PopulateImage(AReader: TdxBigEndianStreamReader; ALength: Integer; AImage: TdxJPXImage);
const
  Content = $0D0A870A;
begin
  inherited PopulateImage(AReader, ALength, AImage);
  if (ALength <> 4) or (AReader.ReadInt32 <> Content) then
    dxJPXRaiseException;
end;

{ TdxJPXFileTypeBox }

class function TdxJPXFileTypeBox.ID: Integer;
begin
  Result := $66747970;
end;

class procedure TdxJPXFileTypeBox.PopulateImage(AReader: TdxBigEndianStreamReader; ALength: Integer; AImage: TdxJPXImage);
begin
  inherited PopulateImage(AReader, ALength, AImage);
  if (ALength < 12) or (ALength mod 4 <> 0) then
    dxJPXRaiseException;
  AReader.Skip(ALength);
end;

{ TdxJPXImageHeaderBox }

class function TdxJPXImageHeaderBox.ID: Integer;
begin
  Result := $69686472;
end;

class procedure TdxJPXImageHeaderBox.PopulateImage(AReader: TdxBigEndianStreamReader; ALength: Integer;
  AImage: TdxJPXImage);
var
  AComponentCount, AHeight, AWidth: Integer;
begin
  inherited PopulateImage(AReader, ALength, AImage);
  if ALength <> 14 then
    dxJPXRaiseException;
  AHeight := AReader.ReadInt32;
  AWidth := AReader.ReadInt32;
  AComponentCount := AReader.ReadInt16;
  if (AHeight < 1) or (AWidth < 1) or (AComponentCount < 1) then
    dxJPXRaiseException;
  AReader.ReadByte;
  AReader.Skip(3);
end;

{ TdxJPXColorSpecificationBox }

class function TdxJPXColorSpecificationBox.ID: Integer;
begin
  Result := $636F6C72;
end;

class procedure TdxJPXColorSpecificationBox.PopulateImage(AReader: TdxBigEndianStreamReader; ALength: Integer;
  AImage: TdxJPXImage);
var
  AMethod: Byte;
begin
  inherited PopulateImage(AReader, ALength, AImage);
  if ALength < 3 then
    dxJPXRaiseException;
  AMethod := AReader.ReadByte;
  AReader.Skip(2);
  if AMethod = 1 then
  begin
    if ALength <> 7 then
      dxJPXRaiseException;
    AReader.Skip(4);
  end
  else
    AReader.Skip(ALength - 3);
end;

{ TdxJPXJP2HeaderBox }

class function TdxJPXJP2HeaderBox.ID: Integer;
begin
  Result := $6A703268;
end;

class procedure TdxJPXJP2HeaderBox.PopulateImage(AReader: TdxBigEndianStreamReader; ALength: Integer;
  AImage: TdxJPXImage);

  function ReadAndCheckBox(AClass: TdxJPXCustomBoxClass): Boolean;
  var
    ABoxClass: TdxJPXCustomBoxClass;
  begin
    TdxJPXCustomBoxClass.DoReadImage(AReader, AImage, ABoxClass);
    Result := (ABoxClass <> nil) and (ABoxClass.ID = AClass.ID);
  end;

var
  AEndPosition: Int64;
  AIsValidHeader: Boolean;
begin
  AEndPosition := AReader.Position + ALength;
  if ReadAndCheckBox(TdxJPXImageHeaderBox) then
  begin
    AIsValidHeader := False;
    while AReader.Position < AEndPosition do
      if ReadAndCheckBox(TdxJPXColorSpecificationBox) then
        AIsValidHeader := True;
    if (AReader.Position > AEndPosition) or not AIsValidHeader then
      dxJPXRaiseException;
  end;
end;

{ TdxJPXContiguousCodeStreamBox }

class function TdxJPXContiguousCodeStreamBox.ID: Integer;
begin
  Result := $6A703263;
end;

class procedure TdxJPXContiguousCodeStreamBox.PopulateImage(AReader: TdxBigEndianStreamReader; ALength: Integer;
  AImage: TdxJPXImage);

  procedure AddMarker(AList: TObjectList<TdxJPXCustomMarker>; AMarkerID: Byte);
  var
    AMarker: TdxJPXCustomMarker;
  begin
    AMarker := TdxJPXCustomMarker.CreateMarker(AReader, AImage, dxcInvalidIntegerValue);
    if AMarker <> nil then
    begin
      if AMarker.ID <> AMarkerID then
      begin
        AMarker.Free;
        dxJPXRaiseException;
      end
      else
        AList.Add(AMarker);
    end;
  end;

var
  AEndPosition: Int64;
  ADefaultCodingStyleMarker: TdxJPXDefaultCodingStyleMarker;
  ADefaultQuantizationMarker: TdxJPXDefaultQuantizationMarker;
  ACurrentMarker: TdxJPXCustomMarker;
  ATempList: TObjectList<TdxJPXCustomMarker>;
begin
  AEndPosition := AReader.Position + ALength;
  ADefaultCodingStyleMarker := nil;
  ADefaultQuantizationMarker := nil;
  ATempList := TObjectList<TdxJPXCustomMarker>.Create;
  try
    AddMarker(ATempList, TdxJPXStartOfCodeStreamMarker.ID);
    AddMarker(ATempList, TdxJPXSizeMarker.ID);
    while AReader.Position < AEndPosition do
    begin
      ACurrentMarker := TdxJPXCustomMarker.CreateMarker(AReader, AImage, dxcInvalidIntegerValue);
      if ACurrentMarker <> nil then
      begin
        ATempList.Add(ACurrentMarker);
        if (ADefaultCodingStyleMarker = nil) and (ACurrentMarker is TdxJPXDefaultCodingStyleMarker) then
          ADefaultCodingStyleMarker := TdxJPXDefaultCodingStyleMarker(ACurrentMarker);

        if (ADefaultQuantizationMarker = nil) and (ACurrentMarker is TdxJPXDefaultQuantizationMarker) then
          ADefaultQuantizationMarker := TdxJPXDefaultQuantizationMarker(ACurrentMarker);
      end
      else
        Break;
    end;
    if (ADefaultCodingStyleMarker = nil) or (ADefaultQuantizationMarker = nil) or (AReader.Position > AEndPosition) then
      dxJPXRaiseException;
  finally
    ATempList.Free;
  end;
end;

{ TdxJPXUnknownBox }

class function TdxJPXUnknownBox.ID: Integer;
begin
  Result := -1;
end;

class procedure TdxJPXUnknownBox.PopulateImage(AReader: TdxBigEndianStreamReader; ALength: Integer; AImage: TdxJPXImage);
begin
  inherited PopulateImage(AReader, ALength, AImage);
  AReader.Skip(ALength);
end;

{ TdxJPXCustomMarker }

class constructor TdxJPXCustomMarker.Create;
begin
  FDictionary := TDictionary<Integer, TdxJPXCustomMarkerClass>.Create;
  FDictionary.Add(TdxJPXStartOfCodeStreamMarker.ID, TdxJPXStartOfCodeStreamMarker);
  FDictionary.Add(TdxJPXStartOfDataMarker.ID, TdxJPXStartOfDataMarker);
  FDictionary.Add(TdxJPXCodeStreamEndMarker.ID, TdxJPXCodeStreamEndMarker);
  FDictionary.Add(TdxJPXDefaultCodingStyleMarker.ID, TdxJPXDefaultCodingStyleMarker);
  FDictionary.Add(TdxJPXCodingStyleComponentMarker.ID, TdxJPXCodingStyleComponentMarker);
  FDictionary.Add(TdxJPXStartOfTilePartMarker.ID, TdxJPXStartOfTilePartMarker);
  FDictionary.Add(TdxJPXDefaultQuantizationMarker.ID, TdxJPXDefaultQuantizationMarker);
  FDictionary.Add(TdxJPXQuantizationComponentMarker.ID, TdxJPXQuantizationComponentMarker);
  FDictionary.Add(TdxJPXSizeMarker.ID, TdxJPXSizeMarker);
end;

class destructor TdxJPXCustomMarker.Destroy;
begin
  FreeAndNil(FDictionary);
end;

class function TdxJPXCustomMarker.CreateMarker(AReader: TdxBigEndianStreamReader; AImage: TdxJPXImage): TdxJPXCustomMarker;
begin
  Result := CreateMarker(AReader, AImage, dxcInvalidIntegerValue);
end;

class function TdxJPXCustomMarker.CreateMarker(AReader: TdxBigEndianStreamReader; AImage: TdxJPXImage;
  ATileIndex: Integer): TdxJPXCustomMarker;
begin
  if AReader.ReadByte <> $FF then
    Result := nil
  else
    Result := CreateAndRead(AReader, AImage, AReader.ReadByte, ATileIndex);
end;

class function TdxJPXCustomMarker.ID: Integer;
begin
  Result := -1;
end;

function TdxJPXCustomMarker.GetMaxDataLength: Integer;
begin
  Result := -1;
end;

procedure TdxJPXCustomMarker.PopulateImage(AReader: TdxBigEndianStreamReader; AImage: TdxJPXImage;
  ATileIndex: Integer);
begin
  DoRead;
end;

procedure TdxJPXCustomMarker.Read(AReader: TdxBigEndianStreamReader);
begin
  DoRead(AReader.ReadInt16 - 2);
end;

procedure TdxJPXCustomMarker.ReadAndCheckLength(AReader: TdxBigEndianStreamReader);
begin
  Read(AReader);
  if DataLength < MaxDataLength then
    dxJPXRaiseException;
end;

class function TdxJPXCustomMarker.CreateAndRead(AReader: TdxBigEndianStreamReader; AImage: TdxJPXImage; AType: Integer;
  ATileIndex: Integer): TdxJPXCustomMarker;
var
  AMarkerClass: TdxJPXCustomMarkerClass;
begin
  if not FDictionary.TryGetValue(AType, AMarkerClass) then
    AMarkerClass := TdxJPXUnknownMarker;
  Result := AMarkerClass.Create;
  try
    Result.PopulateImage(AReader, AImage, ATileIndex);
  except
    FreeAndNil(Result);
  end;
end;

procedure TdxJPXCustomMarker.DoRead(ADataLength: Integer = 0);
begin
  FDataLength := ADataLength;
end;

{ TdxJPXStartOfCodeStreamMarker }

class function TdxJPXStartOfCodeStreamMarker.ID: Integer;
begin
  Result := $4F;
end;

{ TdxJPXStartOfDataMarker }

class function TdxJPXStartOfDataMarker.ID: Integer;
begin
  Result := $93;
end;

{ TdxJPXCodeStreamEndMarker }

class function TdxJPXCodeStreamEndMarker.ID: Integer;
begin
  Result := $D9;
end;

{ TdxJPXDefaultCodingStyleMarker }

class function TdxJPXDefaultCodingStyleMarker.ID: Integer;
begin
  Result := $52;
end;

function TdxJPXDefaultCodingStyleMarker.GetMaxDataLength: Integer;
begin
  Result := 10;
end;

procedure TdxJPXDefaultCodingStyleMarker.PopulateImage(AReader: TdxBigEndianStreamReader; AImage: TdxJPXImage;
  ATileIndex: Integer);
begin
  ReadAndCheckLength(AReader);
  AImage.ReadCodingStyleComponents(AReader);
end;

{ TdxJPXStartOfTilePartMarker }

class function TdxJPXStartOfTilePartMarker.ID: Integer;
begin
  Result := $90;
end;

function TdxJPXStartOfTilePartMarker.GetMaxDataLength: Integer;
begin
  Result := 8;
end;

procedure TdxJPXStartOfTilePartMarker.PopulateImage(AReader: TdxBigEndianStreamReader; AImage: TdxJPXImage;
  ATileIndex: Integer);

  procedure ReadToStartDataMarker(ATile: Integer);
  var
    AMarker: TdxJPXCustomMarker;
  begin
    AMarker := nil;
    try
      repeat
        FreeAndNil(AMarker);
        AMarker := TdxJPXCustomMarker.CreateMarker(AReader, AImage, ATile);
      until not (not (AMarker is TdxJPXStartOfDataMarker));
    finally
      FreeAndNil(AMarker);
    end;
  end;

  procedure PopulatePackets(ATile: Integer; ATileLength: Integer; AStartPosition: Int64);
  var
    AData: TByteS;
    AStream: TBytesStream;
  begin
    AData := AReader.ReadBytes(ATileLength - DataLength - 6 - (AReader.Position - AStartPosition - 2));
    AStream := TBytesStream.Create(AData);
    try
      while AStream.Position < AStream.Size do
        AImage.Tiles[ATile].AppendPacket(AStream);
    finally
      AStream.Free;
    end;
  end;

var
  ATile: Integer;
  ATileLength: Integer;
  AStartPosition: Int64;
begin
  ReadAndCheckLength(AReader);
  ATile := AReader.ReadInt16;
  ATileLength := AReader.ReadInt32;
  AReader.ReadByte;
  AReader.ReadByte;
  AStartPosition := AReader.Position;
  ReadToStartDataMarker(ATile);
  PopulatePackets(ATile, ATileLength, AStartPosition)
end;

{ TdxJPXSizeMarker }

class function TdxJPXSizeMarker.ID: Integer;
begin
  Result := $51;
end;

procedure TdxJPXSizeMarker.PopulateImage(AReader: TdxBigEndianStreamReader; AImage: TdxJPXImage;
  ATileIndex: Integer);
var
  ASize: TdxJPXSize;
begin
  Read(AReader);
  AReader.Skip(2);
  ASize.Initialize(AReader);
  AImage.Size := ASize;
  if DataLength <> 36 + Length(AImage.Size.Components) * 3 then
    dxJPXRaiseException;
end;

{ TdxJPXDefaultQuantizationMarker }

class function TdxJPXDefaultQuantizationMarker.ID: Integer;
begin
  Result := $5C;
end;

function TdxJPXDefaultQuantizationMarker.GetMaxDataLength: Integer;
begin
  Result := 2;
end;

procedure TdxJPXDefaultQuantizationMarker.PopulateImage(AReader: TdxBigEndianStreamReader; AImage: TdxJPXImage;
  ATileIndex: Integer);
begin
  ReadAndCheckLength(AReader);
  AImage.ReadQuantizationParameters(AReader, DataLength);
end;

{ TdxJPXCodingStyleComponentMarker }

class function TdxJPXCodingStyleComponentMarker.ID: Integer;
begin
  Result := $53;
end;

function TdxJPXCodingStyleComponentMarker.GetMaxDataLength: Integer;
begin
  Result := 7;
end;

procedure TdxJPXCodingStyleComponentMarker.PopulateImage(AReader: TdxBigEndianStreamReader; AImage: TdxJPXImage;
  ATileIndex: Integer);
begin
  ReadAndCheckLength(AReader);
  AReader.Skip(DataLength);
end;

{ TdxJPXQuantizationComponentMarker }

class function TdxJPXQuantizationComponentMarker.ID: Integer;
begin
  Result := $5D;
end;

function TdxJPXQuantizationComponentMarker.GetMaxDataLength: Integer;
begin
  Result := 3;
end;

procedure TdxJPXQuantizationComponentMarker.PopulateImage(AReader: TdxBigEndianStreamReader; AImage: TdxJPXImage;
  ATileIndex: Integer);
var
  AComponent, AByteCount: Integer;
  AStartPosition: Int64;
begin
  ReadAndCheckLength(AReader);
  AStartPosition := AReader.Position;
  AComponent := AReader.ReadByte;
  AByteCount := DataLength - AReader.Position + AStartPosition;
  if IsValidIntegerValue(ATileIndex) then
    AImage.Tiles[ATileIndex].Components[AComponent].QuantizationParameters :=
      TdxJPXQuantizationComponentParameters.CreateParameters(AReader, AByteCount)
  else
    AImage.QuantizationParameters[AComponent] := TdxJPXQuantizationComponentParameters.CreateParameters(AReader, AByteCount);
end;

{ TdxJPXUnknownMarker }

procedure TdxJPXUnknownMarker.PopulateImage(AReader: TdxBigEndianStreamReader; AImage: TdxJPXImage;
  ATileIndex: Integer);
begin
  Read(AReader);
  AReader.ReadBytes(DataLength);
end;

{ TdxJPXQuantizationComponentParameters }

constructor TdxJPXQuantizationComponentParameters.Create(AGuardBitCount: Integer);
begin
  inherited Create;
  FGuardBitCount := AGuardBitCount;
end;

class function TdxJPXQuantizationComponentParameters.CreateParameters(AReader: TdxBigEndianStreamReader;
  AByteCount: Integer): TdxJPXQuantizationComponentParameters;
var
  AParameter, AQuantizationStyle, AGuardBitCount: Integer;
begin
  AParameter := AReader.ReadByte;
  AQuantizationStyle := AParameter and $1F;
  AGuardBitCount := AParameter shr 5;
  case AQuantizationStyle of
    0:
      Result := TdxJPXUnitaryStepSizeQuantizationComponentParameters.Create(AGuardBitCount, AReader, AByteCount - 1);
    1:
      Result := TdxJPXScalarDerivedQuantizationComponentParameters.Create(AGuardBitCount, AReader);
    2:
      Result := TdxJPXScalarExpoundedQuantizationComponentParameters.Create(AGuardBitCount, AReader, AByteCount - 1);
  else
    Result := nil;
  end;
end;

{ TdxJPXUnitaryStepSizeQuantizationComponentParameters }

constructor TdxJPXUnitaryStepSizeQuantizationComponentParameters.Create(AGuardBitCount: Integer;
  AReader: TdxBigEndianStreamReader; AByteCount: Integer);
var
  I: Integer;
begin
  inherited Create(AGuardBitCount);
  SetLength(FStepSizeExponents, AByteCount);
  for I := 0 to AByteCount - 1 do
    FStepSizeExponents[I] := AReader.ReadByte shr 3;
end;

function TdxJPXUnitaryStepSizeQuantizationComponentParameters.Apply(const ACoefficient: TdxJPXCoefficient;
  ASubBandGainLog, ARi, ASubBandIndex: Integer): Single;
var
  Q: Integer;
  AStepSize: Single;
begin
  Result := 0;
  Q := ACoefficient.ToInteger;
  if Q <> 0 then
  begin
    AStepSize := Power(2, GuardBitCount + FStepSizeExponents[ASubBandIndex] - ACoefficient.BitsDecoded - 1);
    Result := IfThen(AStepSize = 1, Q, (Q + IfThen(q < 0, -R, R)) * AStepSize);
  end;
end;

function TdxJPXUnitaryStepSizeQuantizationComponentParameters.Clone: TdxJPXQuantizationComponentParameters;
var
  L: Integer;
begin
  Result := TdxJPXUnitaryStepSizeQuantizationComponentParameters.Create(GuardBitCount, nil, 0);
  L := Length(FStepSizeExponents);
  SetLength(TdxJPXUnitaryStepSizeQuantizationComponentParameters(Result).FStepSizeExponents, L);
  cxCopyData(@FStepSizeExponents[0], @TdxJPXUnitaryStepSizeQuantizationComponentParameters(Result).FStepSizeExponents[0],
    L * SizeOf(Integer));
end;

{ TdxJPXIrreversibleTransformationQuantizationComponentParameters }

function TdxJPXIrreversibleTransformationQuantizationComponentParameters.Apply(const ACoefficient: TdxJPXCoefficient;
  ASubBandGainLog, ARi, ASubBandIndex: Integer): Single;
var
  Q, APow: Integer;
  AQuantizationStepSize: TdxJPXQuantizationStepSize;
  AStepSize: Single;
begin
  Result := 0;
  Q := ACoefficient.ToInteger;
  if Q <> 0 then
  begin
    AQuantizationStepSize := GetStepSize(ASubBandIndex);
    AStepSize := 1 + AQuantizationStepSize.Mu / 2048;
    APow := ASubBandGainLog + ARi - AQuantizationStepSize.Epsilon;
    AStepSize := IfThen(APow < 0, AStepSize / (1 shl -APow), AStepSize * (1 shl APow));
    APow := GuardBitCount + AQuantizationStepSize.Epsilon - ACoefficient.BitsDecoded - 1;
    if APow < 0 then
      dxJPXRaiseException;
    Result := (Q + IfThen(q < 0, -R , R)) * AStepSize * (1 shl APow);
  end;
end;

{ TdxJPXScalarDerivedQuantizationComponentParameters }

constructor TdxJPXScalarDerivedQuantizationComponentParameters.Create(AGuardBitCount: Integer; AReader: TdxBigEndianStreamReader);
var
  AV1, AV2: Byte;
begin
  inherited Create(AGuardBitCount);
  if AReader <> nil then
  begin
    AV1 := AReader.ReadByte;
    AV2 := AReader.ReadByte;
    FStepSize.Initialize(AV1, AV2);
  end;
end;

function TdxJPXScalarDerivedQuantizationComponentParameters.Clone: TdxJPXQuantizationComponentParameters;
begin
  Result := TdxJPXScalarDerivedQuantizationComponentParameters.Create(GuardBitCount, nil);
  TdxJPXScalarDerivedQuantizationComponentParameters(Result).FStepSize := FStepSize;
end;

function TdxJPXScalarDerivedQuantizationComponentParameters.GetStepSize(ASubBandIndex: Integer): TdxJPXQuantizationStepSize;
var
  I: Integer;
begin
  I := ASubBandIndex div 4;
  Result.Initialize(FStepSize.Epsilon + IfThen(I > 0, 1 - I, 0), FStepSize.Mu);
end;

{ TdxJPXScalarExpoundedQuantizationComponentParameters }

constructor TdxJPXScalarExpoundedQuantizationComponentParameters.Create(AGuardBitCount: Integer; AReader: TdxBigEndianStreamReader; AByteCount: Integer);
var
  AV1, AV2: Byte;
  AMaxDecompositionLevelCount, I: Integer;
begin
  inherited Create(AGuardBitCount);
  AMaxDecompositionLevelCount := AByteCount div 2;
  SetLength(FStepSizes, AMaxDecompositionLevelCount);
  for I := 0 to AMaxDecompositionLevelCount - 1 do
  begin
    AV1 := AReader.ReadByte;
    AV2 := AReader.ReadByte;
    FStepSizes[I].Initialize(AV1, AV2);
  end;
end;

function TdxJPXScalarExpoundedQuantizationComponentParameters.Clone: TdxJPXQuantizationComponentParameters;
begin
  Result := TdxJPXScalarExpoundedQuantizationComponentParameters.Create(GuardBitCount, nil, 0);
  SetLength(TdxJPXScalarExpoundedQuantizationComponentParameters(Result).FStepSizes, Length(FStepSizes));
  cxCopyData(@FStepSizes[0], @TdxJPXScalarExpoundedQuantizationComponentParameters(Result).FStepSizes[0],
    Length(FStepSizes) * SizeOf(TdxJPXQuantizationStepSize));
end;

function TdxJPXScalarExpoundedQuantizationComponentParameters.GetStepSize(ASubBandIndex: Integer): TdxJPXQuantizationStepSize;
begin
  Result := FStepSizes[ASubBandIndex];
end;

{ TdxJPXArea }

function TdxJPXArea.GetHeight: Integer;
begin
  Result := Y1 - Y0;
end;

function TdxJPXArea.GetWidth: Integer;
begin
  Result := X1 - X0;
end;

{ TdxJPXPacketProgressionOrder }

constructor TdxJPXPacketProgressionOrder.Create(AImage: TdxJPXImage; ATile: TdxJPXTile);
begin
  inherited Create;
  FImage := AImage;
  FTile := ATile;
  FPacketEnumerator := TList<TdxJPXPacketPosition>.Create;
end;

destructor TdxJPXPacketProgressionOrder.Destroy;
begin
  FreeAndNil(FPacketEnumerator);
  inherited Destroy;
end;

function TdxJPXPacketProgressionOrder.CreatePacketPositionList: TList<TdxJPXPacketPosition>;
begin
  Result := TList<TdxJPXPacketPosition>.Create;
end;

class function TdxJPXPacketProgressionOrder.CreatePacket(AOrder: TdxJPXProgressionOrder; AImage: TdxJPXImage;
  ATile: TdxJPXTile): TList<TdxJPXPacketPosition>;
var
  APacketProgressionOrder: TdxJPXPacketProgressionOrder;
begin
  case AOrder of
    poLayerResolutionComponentPosition:
      APacketProgressionOrder := TdxJPXLayerResolutionComponentPrecinctPacketProgressionOrder.Create(AImage, ATile);
    poResolutionLayerComponentPosition:
      APacketProgressionOrder := TdxJPXResolutionLayerComponentPrecinctPacketProgressionOrder.Create(AImage, ATile);
    poResolutionPositionComponentLayer:
      APacketProgressionOrder := TdxJPXResolutionPrecinctComponentLayerPacketProgressionOrder.Create(AImage, ATile);
  else
    APacketProgressionOrder := nil;
    dxJPXRaiseException;
  end;
  try
    Result := APacketProgressionOrder.CreatePacketPositionList;
  finally
    APacketProgressionOrder.Free;
  end;
end;

function TdxJPXPacketProgressionOrder.GetMaxResolutionLevel: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Length(Image.CodingStyleComponents) - 1 do
    Result := Max(Image.CodingStyleComponents[I].DecompositionLevelCount + 1, Result);
end;

{ TdxJPXLayerResolutionComponentPrecinctPacketProgressionOrder }

function TdxJPXLayerResolutionComponentPrecinctPacketProgressionOrder.CreatePacketPositionList: TList<TdxJPXPacketPosition>;
var
  APosition: TdxJPXPacketPosition;
  AMaxResolutionLevel, ALayer, AComponent, APrecinct, APrecinctCount, AResolutionLevel: Integer;
begin
  Result := inherited CreatePacketPositionList;
  AMaxResolutionLevel := MaxResolutionLevel;
  for ALayer := 0 to Image.DefaultCodingStyle.LayerCount - 1 do
    for AResolutionLevel := 0 to AMaxResolutionLevel - 1 do
      for AComponent := 0 to Length(Image.CodingStyleComponents) - 1 do
        if AResolutionLevel <= Image.CodingStyleComponents[AComponent].DecompositionLevelCount then
        begin
          APrecinctCount := Tile.Components[AComponent].ResolutionLevel[AResolutionLevel].PrecinctCount;
          for APrecinct := 0 to APrecinctCount - 1 do
          begin
            APosition.Initialize(AComponent, ALayer, AResolutionLevel, APrecinct);
            Result.Add(TdxJPXPacketPosition(APosition));
          end;
        end;
end;

{ TdxJPXResolutionLayerComponentPrecinctPacketProgressionOrder }

function TdxJPXResolutionLayerComponentPrecinctPacketProgressionOrder.CreatePacketPositionList: TList<TdxJPXPacketPosition>;
var
  APosition: TdxJPXPacketPosition;
  AMaxResolutionLevel, AResolution, ALayer, AComponent, APrecinctCount, APrecinct: Integer;
begin
  Result := inherited CreatePacketPositionList;
  AMaxResolutionLevel := MaxResolutionLevel;
  for AResolution := 0 to AMaxResolutionLevel - 1 do
    for ALayer := 0 to Image.DefaultCodingStyle.LayerCount - 1 do
      for AComponent := 0 to Length(Image.CodingStyleComponents) - 1 do
        if AResolution <= Image.CodingStyleComponents[AComponent].DecompositionLevelCount then
        begin
          APrecinctCount := Tile.Components[AComponent].ResolutionLevel[AResolution].PrecinctCount;
          for APrecinct := 0 to APrecinctCount - 1 do
          begin
            APosition.Initialize(AComponent, ALayer, AResolution, APrecinct);
            Result.Add(TdxJPXPacketPosition(APosition));
          end;
        end;
end;

{ TdxJPXResolutionPrecinctComponentLayerPacketProgressionOrder }

function TdxJPXResolutionPrecinctComponentLayerPacketProgressionOrder.CreatePacketPositionList: TList<TdxJPXPacketPosition>;
var
  AMaxPrecinct, AMaxResolutionLevel, AComponent, AResolution, APrecinct, ALayer: Integer;
  ACodingStyleComponent: TdxJPXCodingStyleComponent;
  APosition: TdxJPXPacketPosition;
begin
  Result := inherited CreatePacketPositionList;
  AMaxPrecinct := 0;
  AMaxResolutionLevel := MaxResolutionLevel;
  for AComponent := 0 to Length(Image.CodingStyleComponents) - 1 do
  begin
    ACodingStyleComponent := Image.CodingStyleComponents[AComponent];
    for AResolution := 0 to AMaxResolutionLevel - 1 do
      if AResolution <= ACodingStyleComponent.DecompositionLevelCount then
        AMaxPrecinct := Max(AMaxPrecinct, Tile.Components[AComponent].ResolutionLevel[AResolution].PrecinctCount);
  end;
  for AResolution := 0 to AMaxResolutionLevel - 1 do
    for APrecinct := 0 to AMaxPrecinct - 1 do
      for AComponent := 0 to Length(Image.CodingStyleComponents) - 1 do
      begin
        ACodingStyleComponent := Image.CodingStyleComponents[AComponent];
        if (AResolution <= ACodingStyleComponent.DecompositionLevelCount) and
          (APrecinct < Tile.Components[AComponent].ResolutionLevel[AResolution].PrecinctCount) then
          for ALayer := 0 to Image.DefaultCodingStyle.LayerCount - 1 do
          begin
            APosition.Initialize(AComponent, ALayer, AResolution, APrecinct);
            Result.Add(TdxJPXPacketPosition(APosition));
          end;
      end;
end;

{ TdxJPXTile }

constructor TdxJPXTile.Create(AImage: TdxJPXImage; ATileIndex: Integer);
var
  P, Q, I: Integer;
  ATileComponent: TdxJPXTileComponent;
begin
  inherited Create;
  FImage := AImage;
  P := ATileIndex mod AImage.TileCount.X;
  Q := Floor(ATileIndex / AImage.TileCount.X);
  X0 := Max(FImage.Size.TileHorizontalOffset + P * FImage.Size.TileWidth, FImage.Size.GridHorizontalOffset);
  Y0 := Max(FImage.Size.TileVerticalOffset + Q * FImage.Size.TileHeight, FImage.Size.GridVerticalOffset);
  X1 := Min(FImage.Size.TileHorizontalOffset + (P + 1) * FImage.Size.TileWidth, FImage.Size.GridWidth);
  Y1 := Min(FImage.Size.TileVerticalOffset + (Q + 1) * FImage.Size.TileHeight, FImage.Size.GridHeight);
  FComponents := TObjectList<TdxJPXTileComponent>.Create;
  for I := 0 to Length(FImage.Size.Components) - 1 do
  begin
    ATileComponent := TdxJPXTileComponent.Create(Self, FImage.Size.Components[I], FImage.CodingStyleComponents[I],
      FImage.QuantizationParameters[I]);
    FComponents.Add(ATileComponent);
  end;
  FPacketPositionList := TdxJPXPacketProgressionOrder.CreatePacket(AImage.DefaultCodingStyle.ProgressionOrder, FImage,
    Self);
end;

destructor TdxJPXTile.Destroy;
begin
  FreeAndNil(FPacketPositionList);
  FreeAndNil(FComponents);
  inherited Destroy;
end;

class function TdxJPXTile.ReadCodingPassCount(AReader: TdxJPXBitReader): Integer;
var
  AValue: Integer;
begin
  if AReader.GetBit = 0 then
    Result := 1
  else
    if AReader.GetBit = 0 then
      Result := 2
    else
    begin
      AValue := AReader.GetInteger(2);
      if AValue < 3 then
        Result := AValue + 3
      else
      begin
        AValue := AReader.GetInteger(5);
        if AValue < 31 then
          Result := AValue + 6
        else
          Result := AReader.GetInteger(7) + 37;
      end;
    end;
end;

procedure TdxJPXTile.AppendPacket(AStream: TStream);
var
  AChunk: TdxJPXCodeBlockChunk;
  ACodeBlockHeaderDataQueue: TQueue<TdxJPXCodeBlockHeaderData>;
  ACurrentPosition: TdxJPXPacketPosition;
  AData: TBytes;
  AHeaderData: TdxJPXCodeBlockHeaderData;
  AReader: TdxJPXBitReader;
begin
  if FCurrentPacketPositionIndex >= FPacketPositionList.Count then
    dxJPXRaiseException;
  Inc(FCurrentPacketPositionIndex);
  ACodeBlockHeaderDataQueue := TQueue<TdxJPXCodeBlockHeaderData>.Create;
  AReader := TdxJPXBitReader.Create(AStream);
  try
    if AReader.GetBit <> 0 then
    begin
      ACurrentPosition := FPacketPositionList[FCurrentPacketPositionIndex - 1];
      PopulateCodeBlockHeaderDataQueue(AReader, ACurrentPosition, ACodeBlockHeaderDataQueue);
      AReader.AlignToByte;
      while ACodeBlockHeaderDataQueue.Count > 0 do
      begin
        AHeaderData := ACodeBlockHeaderDataQueue.Dequeue;
        SetLength(AData, AHeaderData.ChunkLength);
        AStream.ReadBuffer(AData[0], AHeaderData.ChunkLength);
        AChunk.Initialize(AData, AHeaderData.CodingPasses);
        AHeaderData.CodeBlock.AddChunk(AChunk);
      end;
    end;
  finally
    ACodeBlockHeaderDataQueue.Free;
    AReader.Free;
  end;
end;

function TdxJPXTile.GetMultipleTransformation: Boolean;
begin
  Result := FImage.DefaultCodingStyle.MultipleTransformation;
end;

function TdxJPXTile.GetUseWaveletTransformation: Boolean;
begin
  Result := FImage.CodingStyleComponents[0].UseWaveletTransformation;
end;

procedure TdxJPXTile.PopulateCodeBlockHeaderDataQueue(AReader: TdxJPXBitReader;
  const ACurrentPosition: TdxJPXPacketPosition; ACodeBlockHeaderDataQueue: TQueue<TdxJPXCodeBlockHeaderData>);
var
  I, J, ALBlock, ADataLength: Integer;
  ACodeBlock: TdxJPXCodeBlock;
  ACodingPassCount: Byte;
  AHeaderData: TdxJPXCodeBlockHeaderData;
  APrecinct: TdxJPXPrecinct;
  AResolutionLevel: TdxJPXResolutionLevel;
begin
  AResolutionLevel := FComponents[ACurrentPosition.Component].ResolutionLevel[ACurrentPosition.ResolutionLevel];
  for APrecinct in AResolutionLevel.Precincts do
  begin
    if APrecinct.Index = ACurrentPosition.Precinct then
      for ACodeBlock in APrecinct.CodeBlocks do
      begin
        I := (ACodeBlock.X0 - APrecinct.X0) div AResolutionLevel.CodeBlockWidth;
        J := (ACodeBlock.Y0 - APrecinct.Y0) div AResolutionLevel.CodeBlockHeight;
        if ACodeBlock.IsFirstInclusion then
        begin
          if ACurrentPosition.Layer < APrecinct.InclusionTree.ReadInclusion(AReader, I, J, ACurrentPosition.Layer) then
            Continue;
          ACodeBlock.IsFirstInclusion := False;
          ACodeBlock.ZeroBitPlanes := APrecinct.ZeroBitPlaneTree.Read(AReader, I, J);
        end
        else
          if AReader.GetBit = 0 then
            Continue;
        ACodingPassCount := Byte(ReadCodingPassCount(AReader));
        ALBlock := ACodeBlock.LBlock;
        while AReader.GetBit = 1 do
          Inc(ALBlock);
        ACodeBlock.LBlock := ALBlock;
        ADataLength := AReader.GetInteger(ALBlock + Floor(LogN(2, ACodingPassCount)));
        AHeaderData.Initialize(ACodeBlock, ACodingPassCount, ADataLength);
        ACodeBlockHeaderDataQueue.Enqueue(AHeaderData);
      end;
  end;
end;
{ TdxJPXCodeBlock }

constructor TdxJPXCodeBlock.Create(X: Integer; Y: Integer; AWidth: Integer; AHeight: Integer);
begin
  inherited Create;
  FLBlock := 3;
  FIsFirstInclusion := True;
  X0 := X;
  Y0 := Y;
  X1 := X + AWidth;
  Y1 := Y + AHeight;
  SetLength(FEncodedData, 0);
end;

destructor TdxJPXCodeBlock.Destroy;
begin
  SetLength(FEncodedData, 0);
  inherited Destroy;
end;

procedure TdxJPXCodeBlock.AddChunk(const AChunk: TdxJPXCodeBlockChunk);
begin
  FEncodedData := TdxByteArray.Concatenate(FEncodedData, AChunk.Data);
  FCodingPassCount := FCodingPassCount + AChunk.CodingPassCount;
end;

{ TdxJPXCodeBlockHeaderData }

procedure TdxJPXCodeBlockHeaderData.Initialize(ACodeBlock: TdxJPXCodeBlock; ACodingPasses: Byte; AChunkLength: Integer);
begin
  FCodeBlock := ACodeBlock;
  FCodingPasses := ACodingPasses;
  FChunkLength := AChunkLength;
end;

{ TdxJPXPrecinct }

constructor TdxJPXPrecinct.Create(ASubBand: TdxJPXSubBand; X, Y, AWidth, AHeight, AIndex: Integer);
begin
  inherited Create;
  FIndex := AIndex;
  X0 := Max(ASubBand.X0, X * AWidth);
  Y0 := Max(ASubBand.Y0, Y * AHeight);
  X1 := Min(ASubBand.X1, (X + 1) * AWidth);
  Y1 := Min(ASubBand.Y1, (Y + 1) * AHeight);
  FCodeBlockHorizontalCount := ASubBand.CodeBlocksWide;
  FCodeBlockVerticalCount := ASubBand.CodeBlocksHigh;
  if FCodeBlockHorizontalCount * FCodeBlockVerticalCount <> 0 then
  begin
    FInclusionTree := TdxJPXTagTree.Create(FCodeBlockHorizontalCount, FCodeBlockVerticalCount);
    FZeroBitPlaneTree := TdxJPXTagTree.Create(FCodeBlockHorizontalCount, FCodeBlockVerticalCount);
  end;
  FCodeBlocks := TList<TdxJPXCodeBlock>.Create;
end;

destructor TdxJPXPrecinct.Destroy;
begin
  FreeAndNil(FCodeBlocks);
  FreeAndNil(FZeroBitPlaneTree);
  FreeAndNil(FInclusionTree);
  inherited Destroy;
end;

{ TdxJPXSubBandCoefficients }

function TdxJPXSubBandCoefficients.Interleave(const AHLSubBandCoefficients, ALHSubBandCoefficients,
  AHHSubBandCoefficients: TdxJPXSubBandCoefficients): TdxJPXSubBandCoefficients;
var
  ACoefficients: TSingleDynArray;
  I, J, ASource, AOffset, ALeft, ARight, AResultWidth, AResultHeight: Integer;
begin
  AResultWidth := FWidth + AHLSubBandCoefficients.Width;
  AResultHeight := FHeight + ALHSubBandCoefficients.Height;
  SetLength(ACoefficients, AResultWidth * AResultHeight);
  FillChar(ACoefficients[0], SizeOf(Single) * AResultWidth * AResultHeight, 0);
  ASource := 0;
  AOffset := 0;
  for I := 0 to FHeight - 1 do
  begin
    ALeft := AOffset;
    ARight := AOffset + AResultWidth;
    for J := 0 to FWidth - 1 do
    begin
      ACoefficients[ALeft] := FCoefficients[ASource];
      Inc(ALeft);
      Inc(ASource);
      if (I < ALHSubBandCoefficients.Height) and (J < ALHSubBandCoefficients.Width) then
        ACoefficients[ARight] := ALHSubBandCoefficients.Coefficients[I * ALHSubBandCoefficients.Width + J];
      Inc(ARight);
      if (I < AHHSubBandCoefficients.Height) and (J < AHHSubBandCoefficients.Width) then
        ACoefficients[ARight] := AHHSubBandCoefficients.Coefficients[I * AHHSubBandCoefficients.Width + J];
      if (I < AHLSubBandCoefficients.Height) and (J < AHLSubBandCoefficients.Width) then
        ACoefficients[ALeft] := AHLSubBandCoefficients.Coefficients[I * AHLSubBandCoefficients.Width + J];
      Inc(ALeft);
      Inc(ARight);
    end;
    Inc(AOffset, AResultWidth * 2);
  end;
  Result.Initialize(ACoefficients, AResultWidth, AResultHeight);
end;

procedure TdxJPXSubBandCoefficients.Initialize(const ACoefficients: TSingleDynArray; AWidth, AHeight: Integer);
begin
  FCoefficients := ACoefficients;
  FWidth := AWidth;
  FHeight := AHeight;
end;

{ TdxJPXSubBand }

constructor TdxJPXSubBand.Create(AComponent: TdxJPXTileComponent;
  AResolutionLevelIndex, ACodeBlockWidth, ACodeBlockHeight: Integer);
var
  AExp, APreviousLevelExp, AHorizontalOffset, AVerticalOffset: Int64;
  ACodeBlocksBuilder: TCodeBlocksBuilder;
begin
  inherited Create;
  FComponent := AComponent;
  FResolutionLevelIndex := AResolutionLevelIndex;
  AExp := 1 shl AResolutionLevelIndex;
  APreviousLevelExp := AExp div 2;
  AHorizontalOffset := APreviousLevelExp * HorizontalQuantity;
  AVerticalOffset := APreviousLevelExp * VerticalQuantity;
  X0 := Ceil((AComponent.X0 - AHorizontalOffset) / AExp);
  X1 := Ceil((AComponent.X1 - AHorizontalOffset) / AExp);
  Y0 := Ceil((AComponent.Y0 - AVerticalOffset) / AExp);
  Y1 := Ceil((AComponent.Y1 - AVerticalOffset) / AExp);
  if (Width <> 0) and (Height <> 0) then
  begin
    FCodeBlocksWide := Ceil(X1 / ACodeBlockWidth )- X0 div ACodeBlockWidth;
    FCodeBlocksHigh := Ceil(Y1 / ACodeBlockHeight) - Y0 div ACodeBlockHeight;
    ACodeBlocksBuilder := TCodeBlocksBuilder.Create;
    try
      FCodeBlocks := ACodeBlocksBuilder.Build(Self, ACodeBlockWidth, ACodeBlockHeight);
    finally
      ACodeBlocksBuilder.Free;
    end;
  end
  else
    FCodeBlocks := TObjectList<TdxJPXCodeBlock>.Create;
end;

destructor TdxJPXSubBand.Destroy;
begin
  FreeAndNil(FCodeBlocks);
  inherited Destroy;
end;

function TdxJPXSubBand.CalculateContextLabel(const ACoefficient: TdxJPXCoefficient): Integer;
begin
  Result := GetContextLabel(ACoefficient.VerticalNeighborSignificance, ACoefficient.HorizontalNeighborSignificance,
    ACoefficient.DiagonalNeighborSignificance);
end;

function TdxJPXSubBand.GetCoefficients: TdxJPXSubBandCoefficients;
var
  I, H, V, AWidth, AHeight, AGainLog, ASubBandIndex, AStartIndex: Integer;
  ACodeBlockCodingStyle: TdxJPXCodeBlockCodingStyle;
  ACodeBlock: TdxJPXCodeBlock;
  ACoefficients: TSingleDynArray;
  AData: TdxJPXCoefficients;
begin
  AWidth := Width;
  AHeight := Height;
  AGainLog := GainLog;
  ACodeBlockCodingStyle := FComponent.CodingStyle.CodeBlockCodingStyle;
  ASubBandIndex := (FComponent.CodingStyle.DecompositionLevelCount - FResolutionLevelIndex) * 3 +
    HorizontalQuantity + 2 * VerticalQuantity;
  SetLength(ACoefficients, AHeight * AWidth);
  for ACodeBlock in FCodeBlocks do
  begin
    AData := TdxJPXCoefficientBitModel.Decode(ACodeBlock, Self, ACodeBlockCodingStyle);
    AStartIndex := (ACodeBlock.Y0 - Y0) * AWidth + ACodeBlock.X0 - X0;
    for V := 0 to ACodeBlock.Height - 1 do
    begin
      H := V;
      I := AStartIndex;
      while H < ACodeBlock.Width * ACodeBlock.Height do
      begin
        ACoefficients[I] := FComponent.QuantizationParameters.Apply(AData[H], AGainLog, FComponent.BitsPerComponent,
          ASubBandIndex);
        Inc(H, ACodeBlock.Height);
        Inc(I);
      end;
      Inc(AStartIndex, AWidth);
    end;
  end;
  Result.Initialize(ACoefficients, AWidth, AHeight);
end;

function TdxJPXSubBand.TCodeBlocksBuilder.Build(ASubBand: TdxJPXSubBand; ACodeBlockWidth,
  ACodeBlockHeight: Integer): TObjectList<TdxJPXCodeBlock>;
var
  ATop, I: Integer;
begin
  FX0 := ASubBand.X0;
  FY0 := ASubBand.Y0;
  FX1 := ASubBand.X1;
  FY1 := ASubBand.Y1;
  FCodeBlockWidth := ACodeBlockWidth;
  FCodeBlockHeight := ACodeBlockHeight;
  FRightCodeBlockBand := ASubBand.CodeBlocksWide - 1;
  FBottomCodeBlockBand := ASubBand.CodeBlocksHigh - 1;
  FCodeBlocks := TObjectList<TdxJPXCodeBlock>.Create;
  FStartInnerHorizontalOffset := (FX0 div ACodeBlockWidth + 1) * ACodeBlockWidth;
  FActualLeftBlockWidth := Min(FStartInnerHorizontalOffset - FX0, FX1 - FX0);
  if ASubBand.CodeBlocksWide * ASubBand.CodeBlocksHigh > 0 then
  begin
    AddRow(FY0, Min((FY0 div FCodeBlockHeight + 1) * FCodeBlockHeight - FY0, FY1 - FY0));
    ATop := (FY0 div FCodeBlockHeight + 1) * FCodeBlockHeight;
    for I := 1 to FBottomCodeBlockBand - 1 do
    begin
      AddRow(ATop, FCodeBlockHeight);
      Inc(ATop, FCodeBlockHeight);
    end;
    if FBottomCodeBlockBand > 0 then
      AddRow(ATop, FY1 - ATop);
  end;
  Result := FCodeBlocks;
end;

procedure TdxJPXSubBand.TCodeBlocksBuilder.AddRow(ATop, ACodeBlockHeight: Integer);
var
  ALeft, J: Integer;
begin
  FCodeBlocks.Add(TdxJPXCodeBlock.Create(FX0, ATop, FActualLeftBlockWidth, ACodeBlockHeight));
  Inc(FCodeBlockIndex);
  ALeft := FStartInnerHorizontalOffset;
  for J := 1 to FRightCodeBlockBand - 1 do
  begin
    FCodeBlocks.Add(TdxJPXCodeBlock.Create(ALeft, ATop, FCodeBlockWidth, ACodeBlockHeight));
    Inc(FCodeBlockIndex);
    Inc(ALeft, FCodeBlockWidth);
  end;
  if FRightCodeBlockBand > 0 then
  begin
    FCodeBlocks.Add(TdxJPXCodeBlock.Create(ALeft, ATop, FX1 - ALeft, ACodeBlockHeight));
    Inc(FCodeBlockIndex);
  end;
end;

{ TdxJPXLowPassSubBand }

function TdxJPXLowPassSubBand.GetContextLabel(V, H, D: Byte): Integer;
begin
  case H of
    0:
      if V = 0 then
        Result := Min(Byte(2), D)
      else
        Result := 2 + V;
    1:
      if V > 0 then
        Result := 7
      else
        if D > 0 then
          Result := 6
        else
          Result := 5;
  else
    Result := 8;
  end;
end;

{ TdxJPXLLSubBand }

function TdxJPXLLSubBand.GetGainLog: Integer;
begin
  Result := 0;
end;

function TdxJPXLLSubBand.GetHorizontalQuantity: Integer;
begin
  Result := 0;
end;

function TdxJPXLLSubBand.GetVerticalQuantity: Integer;
begin
  Result := 0;
end;

{ TdxJPXLHSubBand }

function TdxJPXLHSubBand.GetGainLog: Integer;
begin
  Result := 1;
end;

function TdxJPXLHSubBand.GetHorizontalQuantity: Integer;
begin
  Result := 0;
end;

function TdxJPXLHSubBand.GetVerticalQuantity: Integer;
begin
  Result := 1;
end;

{ TdxJPXHLSubBand }

function TdxJPXHLSubBand.GetContextLabel(V: Byte; H: Byte; D: Byte): Integer;
begin
  Result := inherited GetContextLabel(H, V, D);
end;

function TdxJPXHLSubBand.GetGainLog: Integer;
begin
  Result := 1;
end;

function TdxJPXHLSubBand.GetHorizontalQuantity: Integer;
begin
  Result := 1;
end;

function TdxJPXHLSubBand.GetVerticalQuantity: Integer;
begin
  Result := 0;
end;

{ TdxJPXHHSubBand }

function TdxJPXHHSubBand.GetContextLabel(V, H, D: Byte): Integer;
var
  AVH: Integer;
begin
  AVH := Min(2, V + H);
  case D of
    0:
      Result := AVH;
    1:
      Result := AVH + 3;
    2:
      if AVH > 0 then
        Result := 7
      else
       Result := 6;
  else
    Result := 8;
  end;
end;

function TdxJPXHHSubBand.GetGainLog: Integer;
begin
  Result := 2;
end;

function TdxJPXHHSubBand.GetHorizontalQuantity: Integer;
begin
  Result := 1;
end;

function TdxJPXHHSubBand.GetVerticalQuantity: Integer;
begin
  Result := 1;
end;

{ TdxJPXCoefficientBitModel }

constructor TdxJPXCoefficientBitModel.Create(ACodeBlock: TdxJPXCodeBlock; AStream: TStream; ASubBand: TdxJPXSubBand);
var
  AContext: TBytes;
  AIndex: Integer;
  AZeroBitPlanes: Byte;
begin
  inherited Create;
  SetLength(FSignContextLabels, 3);
  SetLength(FSignContextLabels[0], 3);
  SetLength(FSignContextLabels[1], 3);
  SetLength(FSignContextLabels[2], 3);
  FSignContextLabels[0][0] := 13;
  FSignContextLabels[0][1] := 12;
  FSignContextLabels[0][2] := 11;
  FSignContextLabels[1][0] := 10;
  FSignContextLabels[1][1] := 9;
  FSignContextLabels[1][2] := 10;
  FSignContextLabels[2][0] := 11;
  FSignContextLabels[2][1] := 12;
  FSignContextLabels[2][2] := 13;
  FRunLengthContextIndex := 17;
  FUniformContextIndex := 18;
  FSubBand := ASubBand;
  SetLength(AContext, 19);
  AContext[0] := 8;
  AContext[17] := 6;
  AContext[18] := 92;
  FReader := TdxBigEndianStreamReader.Create(AStream);
  FState := TdxArithmeticState.Create(FReader);
  FArithmeticContext := TdxArithmeticContext.Create(FState, AContext);
  FCodeBlockWidth := ACodeBlock.Width;
  FCodeBlockHeight := ACodeBlock.Height;
  FPassCount := ACodeBlock.CodingPassCount;
  SetLength(FCoefficients, FCodeBlockWidth * FCodeBlockHeight);
  AZeroBitPlanes := Byte(ACodeBlock.ZeroBitPlanes);
  for AIndex := 0 to Length(FCoefficients) - 1 do
    FCoefficients[AIndex].BitsDecoded := AZeroBitPlanes;
end;

destructor TdxJPXCoefficientBitModel.Destroy;
begin
  FreeAndNil(FReader);
  FreeAndNil(FState);
  FreeAndNil(FArithmeticContext);
  inherited Destroy;
end;

class function TdxJPXCoefficientBitModel.Decode(ACodeBlock: TdxJPXCodeBlock; ASubBand: TdxJPXSubBand;
  ACodeBlockCodingStyle: TdxJPXCodeBlockCodingStyle): TdxJPXCoefficients;
var
  ABitModel: TdxJPXCoefficientBitModel;
  AStream: TBytesStream;
begin
  AStream := TBytesStream.Create(ACodeBlock.EncodedData);
  try
    ABitModel := TdxJPXCoefficientBitModel.Create(ACodeBlock, AStream, ASubBand);
    try
      Result := ABitModel.DoDecode(ACodeBlockCodingStyle);
    finally
      ABitModel.Free;
    end;
  finally
    AStream.Free;
  end;
end;

class function TdxJPXCoefficientBitModel.NormalizeContribution(AContribution: Integer): Integer;
begin
  Result := Min(1, Max(-1, AContribution));
end;

function TdxJPXCoefficientBitModel.DoDecode(ACodeBlockCodingStyle: TdxJPXCodeBlockCodingStyle): TdxJPXCoefficients;
var
  I, AState: Integer;
  AUseSegmentationSymbols: Boolean;
begin
  AState := 2;
  AUseSegmentationSymbols := ACodeBlockCodingStyle = bcsUseSegmentationSymbols;
  while FPassCount > 0 do
  begin
    case AState of
      0:
        SignificancePropagationPass;
      1:
        MagnitudeRefinementPass;
      2:
        CleanUpPass;
    end;
    Inc(AState);
    AState := AState mod 3;
    if AUseSegmentationSymbols and (AState = 0) then
      for I := 0 to 3 do
        FArithmeticContext.DecodeBit(FUniformContextIndex);
    Dec(FPassCount);
  end;
  Result := FCoefficients;
end;

function TdxJPXCoefficientBitModel.GetSignContribution(AIndex: Integer): Integer;
var
  ACoefficient: TdxJPXCoefficient;
begin
  ACoefficient := FCoefficients[AIndex];
  if ACoefficient.Significance = 0 then
    Result := 0
  else
    Result := IfThen(ACoefficient.Sign <> 0, -1, 1);
end;

function TdxJPXCoefficientBitModel.GetSignBit(X, Y, AIndex: Integer; var ACoefficient: TdxJPXCoefficient): TdxJPXCoefficient;
var
  AVContribution, AHContribution, AXorBit, D: Integer;
begin
  CalculateCurrentCoefficient(X, Y, AIndex, AVContribution, AHContribution);
  ACoefficient.Significance := 1;
  ACoefficient.Magnitude := 1;
  AVContribution := NormalizeContribution(AVContribution);
  AHContribution := NormalizeContribution(AHContribution);
  AXorBit := IfThen((AHContribution < 0) or (AHContribution = 0) and (AVContribution < 0), 1, 0);
  D := FArithmeticContext.DecodeBit(FSignContextLabels[AHContribution + 1, AVContribution + 1]);
  ACoefficient.Sign := Byte(D xor AXorBit);
  Result := ACoefficient;
end;

procedure TdxJPXCoefficientBitModel.CleanUpPass;

  procedure DecodeCoefficient(X, Y, AOffset, AMaxOffset, AMaxHeight: Integer; var N: Integer);
  var
    I, ACoefficientIndex, AStartIndex, ACount: Integer;
  begin
    if FArithmeticContext.DecodeBit(FRunLengthContextIndex) <> 0 then
    begin
      ACoefficientIndex := FArithmeticContext.DecodeBit(FUniformContextIndex) shl 1;
      ACoefficientIndex := ACoefficientIndex or FArithmeticContext.DecodeBit(FUniformContextIndex);
      AStartIndex := AOffset + Y;
      ACount := AStartIndex + ACoefficientIndex;
      for I := AStartIndex to ACount do
        FCoefficients[I].BitsDecoded := FCoefficients[I].BitsDecoded + 1;
      ReadSignBit(X, Y + ACoefficientIndex);
      Inc(N, ACoefficientIndex + 1);
    end
    else
    begin
      for I := AOffset + N to AMaxOffset - 1 do
        FCoefficients[I].BitsDecoded := FCoefficients[I].BitsDecoded + 1;
      N := AMaxHeight;
    end;
  end;

var
  APreviousSignificancePropagationPassIndex, Y, AMaxHeight, X, AOffset, AMaxOffset, K, N, I: Integer;
  AIsRunLength: Boolean;
  ACoefficient: TdxJPXCoefficient;
begin
  APreviousSignificancePropagationPassIndex := FPassCount + 2;
  Y := 0;
  while Y < FCodeBlockHeight do
  begin
    AMaxHeight := Min(Y + 4, FCodeBlockHeight);
    AOffset := 0;
    for X := 0 to FCodeBlockWidth - 1 do
    begin
      AIsRunLength := Y + 4 <= FCodeBlockHeight;
      AMaxOffset := AMaxHeight + AOffset;
      for K := AOffset + Y to AMaxOffset - 1 do
      begin
        ACoefficient := FCoefficients[K];
        if (ACoefficient.Significance <> 0) or
          (ACoefficient.NotZeroContextLabelPassIndex = APreviousSignificancePropagationPassIndex) or
          (FSubBand.CalculateContextLabel(ACoefficient) <> 0) then
        begin
          AIsRunLength := False;
          Break;
        end;
      end;
      N := Y;
      if AIsRunLength then
        DecodeCoefficient(X, Y, AOffset, AMaxOffset, AMaxHeight, N);
      for I := AOffset + N to AMaxOffset - 1 do
      begin
        ACoefficient := FCoefficients[I];
        if (ACoefficient.Significance = 0) and
          (ACoefficient.NotZeroContextLabelPassIndex <> APreviousSignificancePropagationPassIndex) then
        begin
          if FArithmeticContext.DecodeBit(FSubBand.CalculateContextLabel(ACoefficient)) <> 0 then
            ReadSignBit(X, N);
          FCoefficients[I].BitsDecoded := FCoefficients[I].BitsDecoded + 1;
        end;
        Inc(N);
      end;
      Inc(AOffset, FCodeBlockHeight);
    end;
    Inc(Y, 4);
  end;
end;

procedure TdxJPXCoefficientBitModel.DoMagnitudeRefinementPass(AIndex, X, Y: Integer; var ACoefficient: TdxJPXCoefficient);
var
  AContextLabelIndex: Integer;
begin
  if (ACoefficient.Significance <> 0) and (ACoefficient.CalculatedSignificancesStepIndex <> FPassCount + 1) then
  begin
    ACoefficient.Magnitude := ACoefficient.Magnitude shl 1;
    if ACoefficient.IsNotFirstRefinement then
      ACoefficient.Magnitude := ACoefficient.Magnitude or SmallInt(FArithmeticContext.DecodeBit(16))
    else
    begin
      ACoefficient.IsNotFirstRefinement := True;
      AContextLabelIndex := ACoefficient.DiagonalNeighborSignificance +
        ACoefficient.VerticalNeighborSignificance + ACoefficient.HorizontalNeighborSignificance;
      ACoefficient.Magnitude := ACoefficient.Magnitude or
        SmallInt(FArithmeticContext.DecodeBit(IfThen(AContextLabelIndex = 0, 14, 15)));
    end;
    ACoefficient.BitsDecoded := ACoefficient.BitsDecoded + 1;
    FCoefficients[AIndex] := ACoefficient;
  end;
end;

procedure TdxJPXCoefficientBitModel.DoSignificancePropagationPass(AIndex, X, Y: Integer;
  var ACoefficient: TdxJPXCoefficient);
var
  AContextLabel: Integer;
begin
  if ACoefficient.Significance = 0 then
  begin
    AContextLabel := FSubBand.CalculateContextLabel(ACoefficient);
    if AContextLabel <> 0 then
    begin
      ACoefficient.NotZeroContextLabelPassIndex := FPassCount;
      if FArithmeticContext.DecodeBit(AContextLabel) <> 0 then
      begin
        ACoefficient.CalculatedSignificancesStepIndex := FPassCount;
        ACoefficient := GetSignBit(X, Y, AIndex, ACoefficient);
      end;
      ACoefficient.BitsDecoded := ACoefficient.BitsDecoded + 1;
      FCoefficients[AIndex] := ACoefficient;
    end;
 end;
end;

procedure TdxJPXCoefficientBitModel.IterateCodeBlock(AProc: TCodeBlockProcessingProc);
var
  Y, J, AMaxHeight, X, K, AOffset: Integer;
begin
  Y := 0;
  for J := Y to FCodeBlockHeight - 1 do
  begin
    AMaxHeight := Min(Y + 4, FCodeBlockHeight);
    for X := 0 to FCodeBlockWidth - 1 do
    begin
      AOffset := X * FCodeBlockHeight + Y;
      for K := Y to AMaxHeight - 1 do
      begin
        AProc(AOffset, X, K, FCoefficients[AOffset]);
        Inc(AOffset);
      end;
    end;
    Inc(Y, 4);
    if Y >= FCodeBlockHeight then
      Break;
  end;
end;

procedure TdxJPXCoefficientBitModel.MagnitudeRefinementPass;
begin
  IterateCodeBlock(DoMagnitudeRefinementPass);
end;

procedure TdxJPXCoefficientBitModel.ReadSignBit(X: Integer; Y: Integer);
var
  AIndex: Integer;
begin
  AIndex := X * FCodeBlockHeight + Y;
  FCoefficients[AIndex] := GetSignBit(X, Y, AIndex, FCoefficients[AIndex]);
end;

procedure TdxJPXCoefficientBitModel.SignificancePropagationPass;
begin
  IterateCodeBlock(DoSignificancePropagationPass);
end;

procedure TdxJPXCoefficientBitModel.CalculateCurrentCoefficient(X, Y, AIndex: Integer; out AVContribution, AHContribution: Integer);
var
  AIsNotLeftElement, AIsNotRightElement: Boolean;
  APreviousRowIndex, ANextRowIndex, AElementIndex: Integer;
begin
  AVContribution := 0;
  AHContribution := 0;
  AIsNotLeftElement := X > 0;
  AIsNotRightElement := X < FCodeBlockWidth - 1;
  APreviousRowIndex := AIndex - FCodeBlockHeight;
  ANextRowIndex := AIndex + FCodeBlockHeight;
  if Y > 0 then
  begin
    AElementIndex := AIndex - 1;
    FCoefficients[AElementIndex].IncrementVerticalNeighborSignificance;
    Inc(AVContribution, GetSignContribution(AElementIndex));
    if AIsNotLeftElement then
      FCoefficients[APreviousRowIndex - 1].IncrementDiagonalNeighborSignificance;
    if AIsNotRightElement then
      FCoefficients[ANextRowIndex - 1].IncrementDiagonalNeighborSignificance;
  end;
  if Y < FCodeBlockHeight - 1 then
  begin
    AElementIndex := AIndex + 1;
    FCoefficients[AElementIndex].IncrementVerticalNeighborSignificance;
    Inc(AVContribution, GetSignContribution(AElementIndex));
    if AIsNotLeftElement then
      FCoefficients[APreviousRowIndex + 1].IncrementDiagonalNeighborSignificance;
    if AIsNotRightElement then
      FCoefficients[ANextRowIndex + 1].IncrementDiagonalNeighborSignificance;
  end;
  if AIsNotLeftElement then
  begin
    FCoefficients[APreviousRowIndex].IncrementHorizontalNeighborSignificance;
    Inc(AHContribution, GetSignContribution(APreviousRowIndex));
  end;
  if AIsNotRightElement then
  begin
    FCoefficients[ANextRowIndex].IncrementHorizontalNeighborSignificance;
    Inc(AHContribution, GetSignContribution(ANextRowIndex));
  end;
end;

{ TdxJPXTileComponent }

constructor TdxJPXTileComponent.Create(ATile: TdxJPXTile; const AComponent: TdxJPXComponent;
  const ACodingStyle: TdxJPXCodingStyleComponent; AQuantizationParameters: TdxJPXQuantizationComponentParameters);
var
  I: Integer;
begin
  inherited Create;
  FCodingStyle := ACodingStyle;
  if AQuantizationParameters <> nil then
    FQuantizationParameters := AQuantizationParameters.Clone;
  FBitsPerComponent := AComponent.BitsPerComponent;
  X0 := Ceil(ATile.X0 / AComponent.HorizontalSeparation);
  X1 := Ceil(ATile.X1 / AComponent.HorizontalSeparation);
  Y0 := Ceil(ATile.Y0 / AComponent.VerticalSeparation);
  Y1 := Ceil(ATile.Y1 / AComponent.VerticalSeparation);
  FBaseResolutionLevel := TdxJPXBaseResolutionLevel.Create(Self, 0, ACodingStyle);
  FResolutionLevels := TObjectList<TdxJPXCompositeResolutionLevel>.Create;
  for I := 0 to ACodingStyle.DecompositionLevelCount - 1 do
    FResolutionLevels.Add(TdxJPXCompositeResolutionLevel.Create(Self, I, ACodingStyle));
end;

destructor TdxJPXTileComponent.Destroy;
begin
  FreeAndNil(FResolutionLevels);
  FreeAndNil(FBaseResolutionLevel);
  FreeAndNil(FQuantizationParameters);
  inherited Destroy;
end;

function TdxJPXTileComponent.Transform: TSingleDynArray;
var
  ABaseResolutionLevelCoefficients: TdxJPXSubBandCoefficients;
  AResolutionLevel: TdxJPXCompositeResolutionLevel;
  ATransform: TdxJPXDiscreteWaveletTransformation;
begin
  ABaseResolutionLevelCoefficients := FBaseResolutionLevel.LLSubBand.Coefficients;
  if FCodingStyle.UseWaveletTransformation then
    ATransform := TdxJPXDiscreteWaveletTransformation(TdxJPXReversibleDiscreteWaveletTransformation.Create(ABaseResolutionLevelCoefficients))
  else
    ATransform := TdxJPXIrreversibleDiscreteWaveletTransformation.Create(ABaseResolutionLevelCoefficients);
  try
    for AResolutionLevel in FResolutionLevels do
      ATransform.Append(AResolutionLevel.HLSubBand.Coefficients, AResolutionLevel.LHSubBand.Coefficients,
        AResolutionLevel.HHSubBand.Coefficients);
    Result := ATransform.SubBandCoefficients.Coefficients;
  finally
    ATransform.Free;
  end;
end;

function TdxJPXTileComponent.GetResolutionLevel(Index: Integer): TdxJPXResolutionLevel;
begin
  if Index = 0 then
    Result := FBaseResolutionLevel
  else
    Result := FResolutionLevels[Index - 1];
end;

procedure TdxJPXTileComponent.SetQuantizationParameters(const AValue: TdxJPXQuantizationComponentParameters);
begin
  if QuantizationParameters <> AValue then
  begin
    FreeAndNil(FQuantizationParameters);
    FQuantizationParameters := AValue;
  end;
end;

{ TdxJPXResolutionLevel }

constructor TdxJPXResolutionLevel.Create(AComponent: TdxJPXTileComponent; ALevelIndex: Integer;
  const ACodingStyle: TdxJPXCodingStyleComponent);
var
  AExp: Int64;
  APrecinctSizeCorrection: Integer;
  APrecinctSize: TdxJPXPrecinctSize;
begin
  inherited Create;
  FLevelIndex := ALevelIndex;
  AExp := Int64(1) shl (ACodingStyle.DecompositionLevelCount - ALevelIndex);
  X0 := Ceil(AComponent.X0 / AExp);
  Y0 := Ceil(AComponent.Y0 / AExp);
  X1 := Ceil(AComponent.X1 / AExp);
  Y1 := Ceil(AComponent.Y1 / AExp);
  APrecinctSize := ACodingStyle.PrecinctSizes[ALevelIndex];
  FPrecinctWidth := 1 shl APrecinctSize.WidthExponent;
  FPrecinctHeight := 1 shl APrecinctSize.HeightExponent;
  APrecinctSizeCorrection := IfThen(ALevelIndex = 0, 0, 1);
  FCodeBlockWidth := 1 shl Min(ACodingStyle.CodeBlockWidthExponent, APrecinctSize.WidthExponent - APrecinctSizeCorrection);
  FCodeBlockHeight := 1 shl Min(ACodingStyle.CodeBlockHeightExponent, APrecinctSize.HeightExponent - APrecinctSizeCorrection);
  FPrecinctWideCount := Ceil(X1 / FPrecinctWidth) - Floor(X0 / FPrecinctWidth);
  FPrecinctHighCount := Ceil(Y1 / FPrecinctHeight) - Floor(Y0 / FPrecinctHeight);
  FPrecincts := TObjectList<TdxJPXPrecinct>.Create;
  FPrecincts.Capacity := FPrecinctCount;
end;

destructor TdxJPXResolutionLevel.Destroy;
begin
  FreeAndNIl(FPrecincts);
  inherited Destroy;
end;

procedure TdxJPXResolutionLevel.AppendPrecincts(ASubBand: TdxJPXSubBand);
var
  APrecinct: TdxJPXPrecinct;
begin
  APrecinct := TdxJPXPrecinct.Create(ASubBand, 0, 0, FPrecinctWidth, FPrecinctHeight, 0);
  if ASubBand.CodeBlocks <> nil then
    APrecinct.CodeBlocks.AddRange(ASubBand.CodeBlocks);
  FPrecincts.Add(APrecinct);
end;

function TdxJPXResolutionLevel.GetPrecinctCount: Integer;
begin
  Result := FPrecinctWideCount * FPrecinctHighCount;
end;

{ TdxJPXBaseResolutionLevel }

constructor TdxJPXBaseResolutionLevel.Create(AComponent: TdxJPXTileComponent; ALevel: Integer;
  const ACodingStyle: TdxJPXCodingStyleComponent);
begin
  inherited Create(AComponent, ALevel, ACodingStyle);
  FLlSubBand := TdxJPXLLSubBand.Create(AComponent, ACodingStyle.DecompositionLevelCount - LevelIndex,
    CodeBlockWidth, CodeBlockHeight);
  AppendPrecincts(FLlSubBand);
end;

destructor TdxJPXBaseResolutionLevel.Destroy;
begin
  FreeAndNil(FLlSubBand);
  inherited Destroy;
end;

{ TdxJPXCompositeResolutionLevel }

constructor TdxJPXCompositeResolutionLevel.Create(AComponent: TdxJPXTileComponent; ALevel: Integer;
  const ACodingStyle: TdxJPXCodingStyleComponent);
var
  ALevelIndex: Integer;
begin
  inherited Create(AComponent, ALevel, ACodingStyle);
  ALevelIndex := ACodingStyle.DecompositionLevelCount - ALevel;
  FLHSubBand := TdxJPXLHSubBand.Create(AComponent, ALevelIndex, CodeBlockWidth, CodeBlockHeight);
  FHLSubBand := TdxJPXHLSubBand.Create(AComponent, ALevelIndex, CodeBlockWidth, CodeBlockHeight);
  FHHSubBand := TdxJPXHHSubBand.Create(AComponent, ALevelIndex, CodeBlockWidth, CodeBlockHeight);
  AppendPrecincts(FHLSubBand);
  AppendPrecincts(FLHSubBand);
  AppendPrecincts(FHHSubBand);
end;

destructor TdxJPXCompositeResolutionLevel.Destroy;
begin
  FreeAndNil(FHLSubBand);
  FreeAndNil(FLHSubBand);
  FreeAndNil(FHHSubBand);
  inherited Destroy;
end;

{ TdxBitReader }

constructor TdxBitReader.Create(AStream: TStream);
begin
  inherited Create;
  FHighBitMask := 1 shl 7;
  FStream := AStream;
end;

function TdxBitReader.GetBit: Integer;
begin
  if (FCurrentBitMask = 0) and not GoToNextByte then
    dxJPXRaiseException;
  Result := IfThen((FCurrentByte and FCurrentBitMask) = 0, 0, 1);
  FCurrentBitMask := FCurrentBitMask shr 1;
end;

function TdxBitReader.GetInteger(ABitCount: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ABitCount - 1 do
    Result := (Result shl 1) or GetBit;
end;

function TdxBitReader.IgnoreExtendedBits: Boolean;
begin
  if FCurrentBitMask <> FHighBitMask then
    Result := GoToNextByte
  else
    Result := FStream.Position < FStream.Size;
end;

function TdxBitReader.GoToNextByte: Boolean;
var
  AByte: Byte;
  AData: Integer;
begin
  ReadByteProc(FStream, AByte);
  AData := AByte;
  Result := AData >= 0;
  if Result then
  begin
    FCurrentByte := Byte(AData);
    FCurrentBitMask := FHighBitMask;
  end;
end;

{ TdxJPXBitReader }

function TdxJPXBitReader.GoToNextByte: Boolean;
begin
  if CurrentByte = 255 then
  begin
    Result := inherited GoToNextByte;
    if Result then
      CurrentBitMask := 1 shl 6;
  end
  else
    Result := inherited GoToNextByte;
end;

procedure TdxJPXBitReader.AlignToByte;
begin
  if CurrentByte = 255 then
    GoToNextByte;
end;

{ TdxJPXTileComponentDataConstructor }

constructor TdxJPXTileComponentDataConstructor.Create(ATileComponents: TObjectList<TdxJPXTileComponent>);
begin
  inherited Create;
  FTileComponents := ATileComponents;
  SetLength(FData, ATileComponents.Count);
end;

class function TdxJPXTileComponentDataConstructor.BuildData(
  ATileComponents: TObjectList<TdxJPXTileComponent>): TdxJPXTileComponentDataArray;
var
  AConstructor: TdxJPXTileComponentDataConstructor;
begin
  AConstructor := TdxJPXTileComponentDataConstructor.Create(ATileComponents);
  try
    Result := AConstructor.CreateData;
  finally
    AConstructor.Free;
  end;
end;

procedure TdxJPXTileComponentDataConstructor.AddTileComponentData(AComponentIndex: Integer);
var
  AComponent: TdxJPXTileComponent;
begin
  AComponent := FTileComponents[AComponentIndex];
  FData[AComponentIndex].Initialize(AComponent.Transform, AComponent.BitsPerComponent);
end;

function TdxJPXTileComponentDataConstructor.CreateData: TdxJPXTileComponentDataArray;
var
  I: Integer;
begin
  for I := 0 to FTileComponents.Count - 1 do
    AddTileComponentData(I);
  Result := FData;
end;

{ TdxJPXDiscreteWaveletTransformation }

constructor TdxJPXDiscreteWaveletTransformation.Create;
begin
  inherited Create;
  FExtendSampleCount := 4;
end;

constructor TdxJPXDiscreteWaveletTransformation.Create(const ALLSubBandCoefficients: TdxJPXSubBandCoefficients);
begin
  Create;
  FSubBandCoefficients := ALLSubBandCoefficients;
end;

class procedure TdxJPXDiscreteWaveletTransformation.Extend(const ABuffer: TSingleDynArray; ASize: Integer);
var
  ATransformation: TdxJPXDiscreteWaveletTransformation;
begin
  ATransformation := TdxJPXDiscreteWaveletTransformation.Create;
  try
    ATransformation.DoExtend(ABuffer, ASize);
  finally
    ATransformation.Free;
  end;
end;

procedure TdxJPXDiscreteWaveletTransformation.Filter(const Y: TSingleDynArray; I0, I1: Integer);
begin
// do nothing
end;

procedure TdxJPXDiscreteWaveletTransformation.Append(const AHlCoefficients, ALhCoefficients,
  AHhCoefficients: TdxJPXSubBandCoefficients);
begin
  FSubBandCoefficients := FSubBandCoefficients.Interleave(AHlCoefficients, ALhCoefficients, AHhCoefficients);
  HorizontalReconstruction;
  VerticalReconstruction;
end;

procedure TdxJPXDiscreteWaveletTransformation.HorizontalReconstruction;
var
  I1, V, AIndex, ARowSize, AOffset: Integer;
  Y: TSingleDynArray;
begin
  if FSubBandCoefficients.Width > 1 then
  begin
    I1 := FSubBandCoefficients.Width + FExtendSampleCount;
    ARowSize := FSubBandCoefficients.Width * 4;
    AOffset := FExtendSampleCount * 4;
    AIndex := 0;
    SetLength(Y, FSubBandCoefficients.Width + FExtendSampleCount * 2);
    for V := 0 to FSubBandCoefficients.Height - 1 do
    begin
      cxCopyData(@FSubBandCoefficients.Coefficients[0], @Y[0], AIndex, AOffset, ARowSize);
      Extend(Y, FSubBandCoefficients.Width);
      Filter(Y, FExtendSampleCount, I1);
      cxCopyData(@Y[0], @FSubBandCoefficients.Coefficients[0], AOffset, AIndex, ARowSize);
      Inc(AIndex, ARowSize);
    end;
  end;
end;

procedure TdxJPXDiscreteWaveletTransformation.VerticalReconstruction;

  procedure InitializeColumnBuffers(AColumnBuffers: TList<TSingleDynArray>; ACount: Integer);
  var
    I: Integer;
    AArray: TSingleDynArray;
  begin
    for I := 0 to ACount - 1 do
    begin
      SetLength(AArray, FSubBandCoefficients.Height + 2 * FExtendSampleCount);
      FillChar(AArray[0], SizeOf(Single) * Length(AArray), 0);
      AColumnBuffers.Add(AArray);
    end;
  end;

var
  AFirstTileExtendIndex, I1, AWidth, ACurrentBufferIndex, ABufferCount, U, K, L, B: Integer;
  ABuffer: TSingleDynArray;
  AColumnBuffers: TList<TSingleDynArray>;
begin
  if FSubBandCoefficients.Height > 1 then
  begin
    I1 := FSubBandCoefficients.Height + FExtendSampleCount;
    AWidth := FSubBandCoefficients.Width;
    ACurrentBufferIndex := 0;
    ABufferCount := 16;
    AColumnBuffers := TList<TSingleDynArray>.Create;
    AColumnBuffers.Capacity := ABufferCount;
    try
      InitializeColumnBuffers(AColumnBuffers, ABufferCount);
      AFirstTileExtendIndex := FExtendSampleCount + FSubBandCoefficients.Height;
      for U := 0 to AWidth - 1 do
      begin
        if ACurrentBufferIndex = 0 then
        begin
          ABufferCount := Min(AWidth - U, ABufferCount);
          K := U;
          for L := FExtendSampleCount to AFirstTileExtendIndex - 1 do
          begin
            for B := 0 to ABufferCount - 1 do
              AColumnBuffers[B][L] := FSubBandCoefficients.Coefficients[K + B];
            Inc(K, AWidth);
          end;
          ACurrentBufferIndex := ABufferCount;
        end;
        Dec(ACurrentBufferIndex);
        ABuffer := AColumnBuffers[ACurrentBufferIndex];
        Extend(ABuffer, FSubBandCoefficients.Height);
        Filter(ABuffer, FExtendSampleCount, I1);
        if ACurrentBufferIndex = 0 then
        begin
          K := U - ABufferCount + 1;
          for L := FExtendSampleCount to AFirstTileExtendIndex - 1 do
          begin
            for B := 0 to ABufferCount - 1 do
              FSubBandCoefficients.Coefficients[K + B] := AColumnBuffers[B][L];
            Inc(K, AWidth);
          end;
        end;
      end;
    finally
      AColumnBuffers.Free;
    end;
  end;
end;

procedure TdxJPXDiscreteWaveletTransformation.DoExtend(const ABuffer: TSingleDynArray; ASize: Integer);
var
  I1, J1, I2, J2: Integer;
begin
  I1 := FExtendSampleCount - 1;
  J1 := FExtendSampleCount + 1;
  I2 := FExtendSampleCount + ASize - 2;
  J2 := FExtendSampleCount + ASize;
  ABuffer[I1] := ABuffer[J1]; Dec(I1); Inc(J1);
  ABuffer[J2] := ABuffer[I2]; Inc(J2); Dec(I2);
  ABuffer[I1] := ABuffer[J1]; Dec(I1); Inc(J1);
  ABuffer[J2] := ABuffer[I2]; Inc(J2); Dec(I2);
  ABuffer[I1] := ABuffer[J1]; Dec(I1); Inc(J1);
  ABuffer[J2] := ABuffer[I2]; Inc(J2); Dec(I2);
  ABuffer[I1] := ABuffer[J1];
  ABuffer[J2] := ABuffer[I2];
end;

{ TdxJPXIrreversibleDiscreteWaveletTransformation }

procedure TdxJPXIrreversibleDiscreteWaveletTransformation.Filter(const Y: TSingleDynArray; I0, I1: Integer);
const
  alpha = -1.586134342059924;
  beta = -0.052980118572961;
  gamma = 0.882911075530934;
  delta = 0.443506852043971;
  k = 1.230174104914001;
  k1 = 1 / k;
var
  I, J, ALength: Integer;
begin
  ALength := Length(Y) - 1;
  I := 1;
  for J := I to ALength - 1 do
  begin
    Y[I] := Y[I] * K1;
    Y[I + 1] := Y[I + 1] * K;
    Inc(I, 2);
    if I >= ALength then
      Break;
  end;

  if ALength mod 2 <> 0 then
    Y[ALength] := Y[ALength] * K1;

  I := 2;
  for J := I to ALength - 1 do
  begin
    Y[I] := Y[I] - delta * (Y[I - 1] + Y[I + 1]);
    Inc(I, 2);
    if I >= ALength then
      Break;
  end;

  I := 1;
  for J := I to ALength - 1 do
  begin
    Y[I] := Y[I] - gamma * (Y[I - 1] + Y[I + 1]);
    Inc(I, 2);
    if I >= ALength then
      Break;
  end;

  I := 2;
  for J := I to ALength - 1 do
  begin
    Y[I] := Y[I] - beta * (Y[I - 1] + Y[I + 1]);
    Inc(I, 2);
    if I >= ALength then
      Break;
  end;

  I := 1;
  for J := I to ALength - 1 do
  begin
    Y[I] := Y[I] - alpha * (Y[I - 1] + Y[I + 1]);
    Inc(I, 2);
    if I >= ALength then
      Break;
  end;
end;

{ TdxJPXReversibleDiscreteWaveletTransformation }

procedure TdxJPXReversibleDiscreteWaveletTransformation.Filter(const Y: TSingleDynArray; I0, I1: Integer);
const
  FastFloorConst = 255;
var
  I, J, ALength: Integer;
begin
  I := 2 * I0 div 2;
  ALength := 2 * (I1 div 2 + 1);
  for J := I to ALength - 1 do
  begin
    Y[I] := Y[I] - (Trunc((Y[I - 1] + Y[I + 1] + 2) / 4 + FastFloorConst) - FastFloorConst);
    Inc(I, 2);
    if I >= ALength then
      Break;
  end;

  I := 2 * (I0 div 2) + 1;
  ALength := 2 * (I1 div 2) + 1;
  for J := I to ALength - 1 do
  begin
    Y[I] := Y[I] + (Trunc((Y[I - 1] + Y[I + 1]) / 2 + FastFloorConst) - FastFloorConst);
    Inc(I, 2);
    if I >= ALength then
      Break;
  end;
end;

{ TdxJPXColorTransformation }

constructor TdxJPXColorTransformation.Create(AWidth: Integer; AHeight: Integer;
  const AComponentsData: TdxJPXTileComponentDataArray);
var
  I: Integer;
begin
  inherited Create;
  FWidth := AWidth;
  FHeight := AHeight;
  SetLength(FComponentsData, Length(AComponentsData));
  for I := 0 to Length(AComponentsData) - 1 do
    FComponentsData[I].Initialize(AComponentsData[I].Data, AComponentsData[I].BitsPerComponent);
  SetLength(FShift, Length(AComponentsData));
  for I := 0 to Length(FShift) - 1 do
    FShift[I] := 1 shl (AComponentsData[I].BitsPerComponent - 1);
end;

destructor TdxJPXColorTransformation.Destroy;
begin
  SetLength(FComponentsData, 0);
  inherited Destroy;
end;

class function TdxJPXColorTransformation.Parse(ATile: TdxJPXTile): TdxJPXColorTransformation;
var
  AData: TdxJPXTileComponentDataArray;
begin
  AData := TdxJPXTileComponentDataConstructor.BuildData(ATile.Components);
  case Length(AData) of
    0, 2:
      begin
        dxJPXRaiseException;
        Result := nil;
      end;
    1:
      Result := TdxJPXGrayColorTransformation.Create(ATile.Width, ATile.Height, AData);
  else
    if ATile.MultipleTransformation then
    begin
      if ATile.UseWaveletTransformation then
        Result := TdxJPXReversibleColorTransformation.Create(ATile.Width, ATile.Height, AData)
      else
        Result := TdxJPXIrreversibleColorTransformation.Create(ATile.Width, ATile.Height, AData);
    end
    else
      Result := TdxJPXMultipleComponentTransformation.Create(ATile.Width, ATile.Height, AData);
  end;
end;

procedure TdxJPXColorTransformation.Transform(const AResult: TBytes; AStartOffset, ARowWidth: Integer);
var
  V, H, AComponentCount, ARowSize, AOffset, ADestOffset: Integer;
begin
  AComponentCount := Length(FComponentsData);
  ARowSize := ARowWidth * AComponentCount;
  AOffset := AStartOffset;
  for V := 0 to FHeight - 1 do
  begin
    ADestOffset := AOffset;
    for H := 0 to FWidth - 1 do
    begin
      cxCopyData(TransformColor(V, H), AResult, 0, ADestOffset, AComponentCount);
      Inc(ADestOffset, AComponentCount);
    end;
    Inc(AOffset, ARowSize);
  end;
end;

class function TdxJPXColorTransformation.Normalize(AValue: Single): Byte;
begin
  Result := dxDoubleToByte(IfThen(AValue < 0, 0, IfThen(AValue > 255, 255, AValue)));
end;

{ TdxJPXGrayColorTransformation }

constructor TdxJPXGrayColorTransformation.Create(AWidth, AHeight: Integer;
  const AComponentData: TdxJPXTileComponentDataArray);
begin
  inherited Create(AWidth, AHeight, AComponentData);
  SetLength(FBuffer, 1);
end;

function TdxJPXGrayColorTransformation.TransformColor(V, H: Integer): TBytes;
begin
  FBuffer[0] := Normalize(ComponentsData[0].Data[V * Width + H] + Shift[0]);
  Result := FBuffer;
end;

{ TdxJPXMultipleComponentTransformation }

constructor TdxJPXMultipleComponentTransformation.Create(AWidth, AHeight: Integer;
  const AComponentData: TdxJPXTileComponentDataArray);

  procedure CopyComponentsData(const ASource: TSingleDynArray; var ADest: TSingleDynArray);
  var
    L: Integer;
  begin
    L := Length(ASource);
    SetLength(ADest, L);
    cxCopyData(@ASource[0], @ADest[0], L * SizeOf(Single));
  end;

begin
  inherited Create(AWidth, AHeight, AComponentData);
  SetLength(FBuffer, Length(AComponentData));
  CopyComponentsData(AComponentData[0].Data, FFirstComponentsData);
  CopyComponentsData(AComponentData[1].Data, FSecondComponentData);
  CopyComponentsData(AComponentData[2].Data, FThirdComponentData);
end;

function TdxJPXMultipleComponentTransformation.TransformColor(V, H: Integer): TBytes;
var
  I, AIndex: Integer;
begin
  AIndex := V * Width + H;
  Transform(FBuffer, FFirstComponentsData[AIndex], FSecondComponentData[AIndex], FThirdComponentData[AIndex]);
  for I := 3 to Length(FBuffer) - 1 do
    FBuffer[I] := Normalize(ComponentsData[I].Data[AIndex] + Shift[I]);
  Result := FBuffer;
end;

procedure TdxJPXMultipleComponentTransformation.Transform(const AData: TBytes; Y0, Y1, Y2: Single);
var
  AShift: Integer;
begin
  AShift := Shift[0];
  AData[0] := Normalize(Y0 + AShift);
  AData[1] := Normalize(Y1 + AShift);
  AData[2] := Normalize(Y2 + AShift);
end;

{ TdxJPXReversibleColorTransformation }

procedure TdxJPXReversibleColorTransformation.Transform(const AData: TBytes; Y0, Y1, Y2: Single);
var
  I1, I0, I2: Single;
begin
  I1 := Y0 - (Y1 + Y2) / 4;
  I0 := I1 + Y2;
  I2 := I1 + Y1;
  inherited Transform(AData, I0, I1, I2);
end;

{ TdxJPXIrreversibleColorTransformation }

procedure TdxJPXIrreversibleColorTransformation.Transform(const AData: TBytes; Y0, Y1, Y2: Single);
var
  I0, I1, I2: Single;
begin
  I0 := Y0 + 1.402 * Y2;
  I1 := Y0 - 0.34413 * Y1 - 0.71414 * Y2;
  I2 := Y0 + 1.772 * Y1;
  inherited Transform(AData, I0, I1, I2);
end;

initialization
  dxgArithmeticQeConsts := TdxArithmeticQeConsts.Create;

finalization
  FreeAndNil(dxgArithmeticQeConsts);

end.


