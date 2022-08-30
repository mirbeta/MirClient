{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressFlowChart                                         }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSFLOWCHART AND ALL ACCOMPANYING }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE end USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxflchrt;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, SysUtils, Classes, Windows, Graphics, Messages, StdCtrls, Forms, ImgList, Controls,
  Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, dxCoreGraphics, dxGDIPlusAPI, dxGDIPlusClasses,
  dxLines, cxGeometry, cxControls, cxGraphics, cxLookAndFeels, cxLookAndFeelPainters, dxTouch, dxForms, cxTextEdit,
  dxFlowChartShapes, dxXMLDoc, dxGuides, dxFlowChartArrows, dxSugiyamaLayout;

const
  TO_HOME = $8000000;
  TO_END = $7FFFFFF;

type
  TdxFcItem = class;
  TdxFcObject = class;
  TdxFcConnection = class;
  TdxFlowChartDragHelper = class;
  TdxCustomFlowChart = class;
  TdxFlowChart = class;
  TdxFlowChartGridOptions = class;

  TdxFcShapeType = (fcsNone, fcsRectangle, fcsEllipse, fcsRoundRect, fcsDiamond,
    fcsNorthTriangle, fcsSouthTriangle, fcsEastTriangle, fcsWestTriangle,
    fcsHexagon, fcsUser, fcsAdvanced);

  TdxFclStyle = (fclStraight, fclCurved, fclRectH, fclRectV);
  TdxFcaType = (
    fcaNone,
    fcaArrow,
    fcaEllipse,
    fcaRectangle,
    fcaClosedASMEarrow,
    fcaFilledASMEarrow,
    fcaClosedArrow,
    fcaFilledArrow,
    fcaIndentedClosedArrow,
    fcaIndentedFilledArrow,
    fcaOutdentedClosedArrow,
    fcaOutdentedFilledArrow,
    fcaClosedDoubleArrow,
    fcaFilledDoubleArrow,
    fcaDiamond,
    fcaFilledDiamond,
    fcaClosedDiamond,
    fcaFilledClosedDiamond,
    fcaDimensionLine,
    fcaBackslash,
    fcaOpenOneDash,
    fcaOpenTwoDash,
    fcaOpenThreeDash,
    fcaClosedOneDash,
    fcaClosedTwoDash,
    fcaClosedThreeDash,
    fcaFilledOneDash,
    fcaFilledTwoDash,
    fcaFilledThreeDash
  );

  TdxFcaSize = (fcasCustom, fcasSmall, fcasMedium, fcasLarge, fcasExtraLarge, fcasHuge);

  TdxFcHorzPos = (fchpLeft, fchpCenter, fchpRight);
  TdxFcVertPos = (fcvpUp, fcvpCenter, fcvpDown);
  TdxFcHitTest = set of (htNowhere, htByObject, htOnObject, htOnConnection,
    htOnConLabel, htOnArrowSrc, htOnArrowDst, htOnSelPoint, htOnRotateMark, htOnParameter);
  TdxFcOption = (fcoCanDelete, fcoCanDrag, fcoCanSelect, fcoMultiSelect,
    fcoHideSelection, fcoDelOnClick, fcoDynamicSizing, fcoDynamicMoving,
    fcoPreventAddingElbowConnections, fcoAlignWithGrid, fcoAutoRouteConnections,
    fcoCanRotate, fcoUseShapeParameters, fcoSnapToGuides);
  TdxFcOptions = set of TdxFcOption;

  TdxFcDragHandler = procedure(X, Y: Integer; State: TDragState) of object;
  TdxFcEvent = procedure(Sender: TdxCustomFlowChart; Item: TdxFcItem) of object;
  TdxFcAllowEvent = procedure(Sender: TdxCustomFlowChart; Item: TdxFcItem; var Allow: Boolean) of object;
  TdxFcEditEvent = procedure(Sender: TdxCustomFlowChart; AObject: TdxFcObject; var S: string) of object;
  TdxFcDrawEvent = procedure(Sender: TdxCustomFlowChart; AObject: TdxFcObject; R: TRect) of object;

  TdxFcDrawExEventPaintData = class
  private
    FBounds: TRect;
    FDefaultDrawText: Boolean;
    FDefaultDrawImage: Boolean;
    FImageBounds: TRect;
    FObject: TdxFcObject;
    function GetFont: TdxGPFont;
    function GetFontBrush: TdxGPBrush;
    function GetFontFormat: TdxGPStringFormat;
  protected
    constructor Create(AObject: TdxFcObject; const ABounds: TRect);
  public
    function Scale(APixels: Integer): Integer;

    property Bounds: TRect read FBounds;
    property DefaultDrawText: Boolean read FDefaultDrawText write FDefaultDrawText;
    property DefaultDrawImage: Boolean read FDefaultDrawImage write FDefaultDrawImage;
    property Font: TdxGPFont read GetFont;
    property FontBrush: TdxGPBrush read GetFontBrush;
    property FontFormat: TdxGPStringFormat read GetFontFormat;
    property ImageBounds: TRect read FImageBounds;
  end;
  TdxFcDrawExEvent = procedure(Sender: TdxCustomFlowChart; AObject: TdxFcObject; ACanvas: TdxGPCanvas; APaintData: TdxFcDrawExEventPaintData) of object;

  { IdxFlowChartDesigner }

  IdxFlowChartDesigner = interface
  ['{0AC07B89-95F8-4658-A7D1-9AAC5CE6E865}']
    function Execute(AChart: TdxFlowChart): Boolean;
  end;

  { TdxFlowChartUnionsHelper }

  TdxFlowChartUnionsHelper = class
  protected
    class function FindAllUnions(AChart: TdxFlowChart; FromUnion: TdxFcObject): TdxFcObject; static;
    class function FindUnions(AChart: TdxFlowChart; FromUnion, Obj: TdxFcObject): TdxFcObject; static;
    class function GetNumberByUnion(AChart: TdxFlowChart; Obj: TdxFcObject): Integer; static;
    class function IsChildItemInUnion(AChart: TdxFlowChart; Obj: TdxFcObject): Boolean; static;
  end;

  TdxFcObjData = packed record
    Left: Integer;
    Top: Integer;
    Width: Word;
    Height: Word;
    Edge: Word;
    Border: Word;
    HTPos: TdxFcHorzPos;
    VTPos: TdxFcVertPos;
    HIPos: TdxFcHorzPos;
    VIPos: TdxFcVertPos;
    BkColor: TColor;
    ShColor: TColor;
    Tag: Integer;
    ObjCnt: Word;
    Image: Smallint;
    Shape: TdxFcShapeType;
    ShWidth: Byte;
    ParFont: Boolean;
    Transparent: Boolean;
  end;

  TdxFcArwData = packed record
    AType: TdxFcaType;
    Width: Byte;
    Height: Byte;
    Color: TColor;
  end;

  TdxFcConData = packed record
    ObjSrc: Smallint;
    ObjDst: Smallint;
    PtCount: Word;
    Color: TColor;
    PtSrc: Byte;
    PtDst: Byte;
    Style: TdxFclStyle;
    ParFont: Boolean;
    ArwSrc: TdxFcArwData;
    ArwDst: TdxFcArwData;
  end;

  TdxFcFntData = packed record
    Height: Smallint;
    Color: TColor;
    Pitch: TFontPitch;
    Style: TFontStyles;
    Charset: TFontCharset;
  end;

  TdxFcSelectionPoints = array[0..7] of TPoint;

  { TdxFlowChartGridLineOptions }

  TdxFlowChartGridLineOptions = class(TPersistent)
  strict private
    FDefaultColor: TColor;
    FDefaultStyle: TPenStyle;
    FColor: TColor;
    FStyle: TPenStyle;
    FOwner: TdxFlowChartGridOptions;
    procedure SetColor(const Value: TColor);
    procedure SetStyle(const Value: TPenStyle);
  protected
    procedure Changed;
    function GetOwner: TPersistent; override;
    function IsColorStored: Boolean;
    function IsStyleStored: Boolean;
    function IsStored: Boolean;
    procedure SetupCanvas(ACanvas: TcxCanvas); virtual;
  public
    constructor Create(AOwner: TdxFlowChartGridOptions; AColor: TColor; AStyle: TPenStyle);
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor stored IsColorStored;
    property Style: TPenStyle read FStyle write SetStyle stored IsStyleStored;
  end;

  { TdxFlowChartGridOptions }

  TdxFlowChartGridOptions = class(TPersistent)
  public const
    DefaultMinorLineStep = 16;
    DefaultMajorLineColor = $D0D0D0;
    DefaultMajorLineStyle = psSolid;
    DefaultMinorLineColor = $E8E8E8;
    DefaultMinorLineStyle = psDot;
    DefaultOriginLineColor = clGray;
    DefaultOriginLineStyle = psSolid;
    DefaultMinZoomThresholdForMinorLines = 40;
  strict private
    FShowLines: Boolean;
    FMinorLineStep: Integer;
    FMajorLines: TdxFlowChartGridLineOptions;
    FMinorLines: TdxFlowChartGridLineOptions;
    FOriginLines: TdxFlowChartGridLineOptions;
    FOwner: TdxCustomFlowChart;
    FMinZoomThresholdForMinorLines: Integer;
    procedure SetMajorLines(const Value: TdxFlowChartGridLineOptions);
    procedure SetMinorLines(const Value: TdxFlowChartGridLineOptions);
    procedure SetMinorLineStep(Value: Integer);
    procedure SetMinZoomThresholdForMinorLines(Value: Integer);
    procedure SetOriginLines(const Value: TdxFlowChartGridLineOptions);
    procedure SetShowLines(Value: Boolean);
  protected
    procedure Changed; virtual;
    function CreateLines(AColor: TColor; AStyle: TPenStyle): TdxFlowChartGridLineOptions; virtual;
    function GetOwner: TPersistent; override;
    //paint
    function GetEdgeSizes: TSize; inline;
    function GetOrigin: TPoint; inline;
    function GetScaleFactor: TdxScaleFactor; inline;
    function NeedPaintMinorLines: Boolean; inline;
    function IsMajorLinesStored: Boolean;
    function IsMinorLinesStored: Boolean;
    function IsOriginLinesStored: Boolean;
  public
    constructor Create(AOwner: TdxCustomFlowChart);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property MajorLines: TdxFlowChartGridLineOptions read FMajorLines write SetMajorLines stored IsMajorLinesStored;
    property MinorLines: TdxFlowChartGridLineOptions read FMinorLines write SetMinorLines stored IsMinorLinesStored;
    property MinorLineStep: Integer read FMinorLineStep write SetMinorLineStep default DefaultMinorLineStep;
    property MinZoomThresholdForMinorLines: Integer read FMinZoomThresholdForMinorLines write SetMinZoomThresholdForMinorLines default DefaultMinZoomThresholdForMinorLines;
    property OriginLines: TdxFlowChartGridLineOptions read FOriginLines write SetOriginLines stored IsOriginLinesStored;
    property ShowLines: Boolean read FShowLines write SetShowLines default False;
  end;

  { TdxFlowChartPainter }

  TdxFlowChartPainter = class(TObject)
  strict private
    procedure PrepareBrush(ACanvas: TcxCanvas; AColor: TColor);
    procedure PreparePen(ACanvas: TcxCanvas; AColor: TColor; AStyle: TPenStyle; AWidth: Integer);
  strict protected
    procedure DrawHorizontalLines(ACanvas: TcxCanvas; const AClientBounds: TRect; AEdgeSize, AGridSize: Integer;
      AIsMajorLines: Boolean; AScaleFactor: TdxScaleFactor);
    procedure DrawVerticalLines(ACanvas: TcxCanvas; const AClientBounds: TRect; AEdgeSize, AGridSize: Integer;
      AIsMajorLines: Boolean; AScaleFactor: TdxScaleFactor);
    procedure DrawGridOrigin(ACanvas: TcxCanvas; const AClientBounds: TRect; const AOrigin: TPoint);
  public
    procedure DrawEdge(ACanvas: TcxCanvas; const R: TRect; AEdgeStyle, ABorderStyle: Cardinal); virtual;
    procedure DrawEllipse(ACanvas: TcxCanvas; const R: TRect; ABackgroundColor: TColor;
      AFrameColor: TColor; AFrameStyle: TPenStyle; AFrameWidth: Integer); virtual;
    procedure DrawGrid(ACanvas: TcxCanvas; const AClientBounds: TRect; AGridOptions: TdxFlowChartGridOptions); virtual;
    procedure DrawPolygon(ACanvas: TcxCanvas; const P: array of TPoint; ABackgroundColor: TColor;
      AFrameColor: TColor; AFrameStyle: TPenStyle; AFrameWidth: Integer); virtual;
    procedure DrawPolyline(ACanvas: TcxCanvas; const P: array of TPoint; AColor: TColor;
      AStyle: TPenStyle; AWidth: Integer); overload; virtual;
    procedure DrawRectangle(ACanvas: TcxCanvas; const R: TRect; ABackgroundColor: TColor;
      AFrameColor: TColor; AFrameStyle: TPenStyle; AFrameWidth: Integer); virtual;
    procedure DrawRegion(ACanvas: TcxCanvas; ARegionHandle: TcxRegionHandle;
      ABackgroundColor: TColor; AFrameColor: TColor; AFrameWidth: Integer); virtual;
    procedure DrawRoundRect(ACanvas: TcxCanvas; const R: TRect; ABackgroundColor: TColor;
      AFrameColor: TColor; AFrameStyle: TPenStyle; AFrameWidth, ARadiusX, ARadiusY: Integer); virtual;
    //new paint
    procedure DrawPolyline(AGraphics: TdxGPGraphics; const P: array of TPoint; AColor: TColor;
      AStyle: TPenStyle; AWidth, AScale: Single); overload; virtual;
  end;

  { TdxFlowChartGDIPlusPainter }

  TdxFlowChartGDIPlusPainter = class(TdxFlowChartPainter)
  public
    procedure DrawEllipse(ACanvas: TcxCanvas; const R: TRect; ABackgroundColor: TColor;
      AFrameColor: TColor; AFrameStyle: TPenStyle; AFrameWidth: Integer); override;
    procedure DrawPolygon(ACanvas: TcxCanvas; const P: array of TPoint;
      ABackgroundColor, AFrameColor: TColor; AFrameStyle: TPenStyle; AFrameWidth: Integer); override;
    procedure DrawPolyline(ACanvas: TcxCanvas; const P: array of TPoint;
      AColor: TColor; AStyle: TPenStyle; AWidth: Integer); override;
    procedure DrawRectangle(ACanvas: TcxCanvas; const R: TRect; ABackgroundColor: TColor;
      AFrameColor: TColor; AFrameStyle: TPenStyle; AFrameWidth: Integer); override;
    procedure DrawRoundRect(ACanvas: TcxCanvas; const R: TRect;
      ABackgroundColor: TColor; AFrameColor: TColor; AFrameStyle: TPenStyle;
      AFrameWidth: Integer; ARadiusX: Integer; ARadiusY: Integer); override;
  end;

  { TdxFlowChartCustomSemitransparentDragHelper }

  TdxFlowChartCustomSemitransparentDragHelper = class(TdxSemiTransparentDragHelper)
  strict private
    function GetBounds: TRect;
    function GetChart: TdxCustomFlowChart; inline;
    function GetDragHelper: TdxFlowChartDragHelper;
  protected
    procedure AfterSelectionPaint(AGraphics: TdxGPGraphics); override;
    property Chart: TdxCustomFlowChart read GetChart;
    property DragHelper: TdxFlowChartDragHelper read GetDragHelper;
  public
    property Bounds: TRect read GetBounds;
  end;

  { TdxFlowChartMovingDragHelper }

  TdxFlowChartMovingDragHelper = class(TdxFlowChartCustomSemitransparentDragHelper)
  strict private
    FObjectsToPaint: TArray<TdxFcObject>;
    function GetObjectsToPaint: TArray<TdxFcObject>;
  strict protected
    function GetSelectionBounds: TRect; override;
    procedure PaintControlSelection(ACanvas: TcxCanvas); override;
  public
    procedure BeginSpecialPaint; override;
  end;

  { TdxFlowChartSelectionDragHelper }

  TdxFlowChartSelectionDragHelper = class(TdxFlowChartCustomSemitransparentDragHelper)
  strict protected
    function GetSelectionBounds: TRect; override;
    procedure PaintControlSelection(ACanvas: TcxCanvas); override;
  end;

  { TdxFlowChartDragHelper }


  TdxFlowChartDragHelper = class
  public type
  {$SCOPEDENUMS ON}
    TFlowChartDragKind = (None, Endpoint, IntermediatePoint, Move, Resize, Selection, Rotate, Parameter);

    TDragOperation = class
    strict private
      FOwner: TdxFlowChartDragHelper;
      function CheckIsKeyPressed(const Index: Integer): Boolean;
      function GetCancelled: Boolean; inline;
      function GetChart: TdxCustomFlowChart; inline;
      function GetGuides: TdxGuidesViewData;
      function GetMouseDownPos: TPoint; inline;
    strict protected
      FBase: TPoint;
      FMobile: TPoint;
      procedure CancelGuides;
      function CreateGuideController(AExcludedObject: TdxFcObject): TdxGuideController; virtual;
      function GetCursor: TCursor; virtual;
      function GetGuideController: TdxGuideController; virtual;
      function SnapToGuides: Boolean; virtual;

      property IsAltPressed: Boolean index VK_MENU read CheckIsKeyPressed;
      property IsControlPressed: Boolean index VK_CONTROL read CheckIsKeyPressed;
      property IsShiftPressed: Boolean index VK_SHIFT read CheckIsKeyPressed;
      property Owner: TdxFlowChartDragHelper read FOwner;
    public
      constructor Create(AOwner: TdxFlowChartDragHelper); virtual;
      procedure Enter(X, Y: Integer); virtual;
      procedure Move(X, Y: Integer); virtual; abstract;
      procedure Leave(X, Y: Integer); virtual; abstract;
      function CreateSemitransparentHelper: TdxFlowChartCustomSemitransparentDragHelper; virtual;

      property Base: TPoint read FBase;
      property Cancelled: Boolean read GetCancelled;
      property Chart: TdxCustomFlowChart read GetChart;
      property Cursor: TCursor read GetCursor;
      property Mobile: TPoint read FMobile;
      property MouseDownPos: TPoint read GetMouseDownPos;
      property Guides: TdxGuidesViewData read GetGuides;
      property GuideController: TdxGuideController read GetGuideController;
    end;

    TDragOperationClass = class of TDragOperation;

    TConnectionPointOperation = class(TDragOperation)
    strict private
      function GetConnection: TdxFcConnection; inline;
    strict protected
      FDragPointIndex: Integer;
      function GetCursor: TCursor; override;

      property Connection: TdxFcConnection read GetConnection;
    public
      procedure Enter(X, Y: Integer); override;
    end;

    TEndpointOperation = class(TConnectionPointOperation)
    strict protected
      procedure SetEndpointObject(AObject: TdxFcObject; ALinkedPointIndex: Integer); virtual;
    public
      procedure Enter(X, Y: Integer); override;
      procedure Move(X, Y: Integer); override;
      procedure Leave(X, Y: Integer); override;
    end;

    TIntermediatePointOperation = class(TConnectionPointOperation)
    strict protected
      procedure SetConnectionIntermediatePoint(X, Y, AIndex: Integer);
    public
      procedure Enter(X, Y: Integer); override;
      procedure Move(X, Y: Integer); override;
      procedure Leave(X, Y: Integer); override;
    end;

    TMoveOperation = class(TDragOperation)
    strict private
      FAlignWithGrid: Boolean;
      FDragObjectBounds: TRect;
      FGuideController: TdxGuideController;
      FInitialDragObjectLocation: TPoint;
      FMouseOffset: TPoint;
      function GetChartGridPoint(const P: TPoint): TPoint; overload; inline;
      function GetChartGridPoint(X, Y: Integer): TPoint; overload;
      function GetDynamicMoving: Boolean; inline;
      procedure UpdateGuides(const ABounds: TRect);
    strict protected
      function GetCursor: TCursor; override;
      function GetGuideController: TdxGuideController; override;
      procedure OffsetObjects(ADragStartPoint, ADragFinishPoint: TPoint; AChangeObjectBounds: Boolean);

      property AlignWithGrid: Boolean read FAlignWithGrid write FAlignWithGrid;
      property DragObjectBounds: TRect read FDragObjectBounds;
      property DynamicMoving: Boolean read GetDynamicMoving;
      property InitialDragObjectLocation: TPoint read FInitialDragObjectLocation;
    public
      procedure Enter(X, Y: Integer); override;
      procedure Move(X, Y: Integer); override;
      procedure Leave(X, Y: Integer); override;
      function CreateSemitransparentHelper: TdxFlowChartCustomSemitransparentDragHelper; override;
    end;

    TRotateOperation = class(TDragOperation)
    strict protected
      FCenterPoint: TPoint;
      FObject: TdxFcObject;
      FSaveAngle: Single;
      function GetCursor: TCursor; override;
    public
      procedure Enter(X, Y: Integer); override;
      procedure Move(X, Y: Integer); override;
      procedure Leave(X, Y: Integer); override;
    end;

    TSelectionOperation = class(TDragOperation)
    strict protected
      function GetCursor: TCursor; override;
    public
      function CreateSemitransparentHelper: TdxFlowChartCustomSemitransparentDragHelper; override;
      procedure Enter(X, Y: Integer); override;
      procedure Move(X, Y: Integer); override;
      procedure Leave(X, Y: Integer); override;
    end;

    TResizeOperation = class(TDragOperation)
    strict private
      FActualMarker: TdxResizeMarker;
      FBounds: TRect;
      FCapturePoint: TPoint;
      FFixedPoint: TdxPointF;
      FInitialObjectBounds: TRect;
      FMarker: TdxResizeMarker;
      FObject: TdxFcObject;
      FGuideController: TdxGuideController;
      FTransformMatrix: TdxGPMatrix;
      FTransformMatrixInv: TdxGPMatrix;
      function GetDynamicSizing: Boolean; inline;
    strict protected
      function ContentBounds: TdxRectF;
      function GetGuideController: TdxGuideController; override;

      function CalculateResizingRectangle(const ADeltaX, ADeltaY: Single): TdxRectF;
      procedure CheckAspectUsingCornerHandles(var ADeltaX, ADeltaY: Single);
      function GetActualMarker(const ABounds: TdxRectF): TdxResizeMarker;
      function ConvertResizingRectangleToObjectBound(const ARectangle: TdxRectF): TRect;
      function GetDisplayedFixedPoint(const ABounds: TdxRectF; AMarker: TdxResizeMarker): TdxPointF;
      function NeedAlignWithGrid: Boolean; virtual;
      procedure ValidateBoundsAlignment(var ANewBounds: TdxRectF; AMarker: TdxResizeMarker);

      property DynamicSizing: Boolean read GetDynamicSizing;
      property Marker: TdxResizeMarker read FMarker;
      property Bounds: TRect read FBounds;
      property CapturePoint: TPoint read FCapturePoint;
      property TransformMatrix: TdxGPMatrix read FTransformMatrix;
      property TransformMatrixInv: TdxGPMatrix read FTransformMatrixInv;
    public
      constructor Create(AOwner: TdxFlowChartDragHelper); override;
      destructor Destroy; override;
      procedure Enter(X, Y: Integer); override;
      procedure Move(X, Y: Integer); override;
      procedure Leave(X, Y: Integer); override;
      function CreateSemitransparentHelper: TdxFlowChartCustomSemitransparentDragHelper; override;
    end;

    TParameterOperation = class(TDragOperation)
    strict protected
      FObject: TdxFcObject;
      FParameterIndex: Integer;
      FParameterValue: Single;
      FSaveParameters: TArray<Single>;
      function GetCursor: TCursor; override;
    public
      procedure Enter(X, Y: Integer); override;
      procedure Move(X, Y: Integer); override;
      procedure Leave(X, Y: Integer); override;
    end;

  {$SCOPEDENUMS OFF}
  private
    FSemitransparentHelper: TdxFlowChartCustomSemitransparentDragHelper;
    FCancelled: Boolean;
    FDragConnection: TdxFcConnection;
    FDragKind: TFlowChartDragKind;
    FDragging: Boolean;
    FDragObject: TdxFcObject;
    FDragPointIndex: Integer;
    FMouseDownPos: TPoint;
    FOperation: TDragOperation;
    FOwner: TdxCustomFlowChart;
    function GetBase: TPoint; inline;
    function GetCanvas: TcxCanvas;
    function GetConnectionAt: TdxFcConnection;
    function GetMobile: TPoint; inline;
    function GetObjectAt: TdxFcObject;
    function GetSpecialControlPainting: Boolean;
  protected
    procedure BeginControlSpecialPaint;
    procedure EndControlSpecialPaint;
    procedure PaintControl(ACanvas: TcxCanvas); virtual;
    procedure UpdateControlImage;

    function CanAddNewPointToConnection: Boolean; virtual;
    function IsDraggingConnection(AConnection: TdxFcConnection): Boolean;
    procedure SetupDragConnection(AConnection: TdxFcConnection; AIndex: Integer);

    function GetOperationClass: TDragOperationClass; virtual;

    property Base: TPoint read GetBase;
    property Cancelled: Boolean read FCancelled;
    property ConnectionAt: TdxFcConnection read GetConnectionAt;
    property DragConnection: TdxFcConnection read FDragConnection;
    property DragObject: TdxFcObject read FDragObject;
    property Index: Integer read FDragPointIndex;
    property Mobile: TPoint read GetMobile;
    property MouseDownPos: TPoint read FMouseDownPos;
    property ObjectAt: TdxFcObject read GetObjectAt;
    property SemitransparentHelper: TdxFlowChartCustomSemitransparentDragHelper read FSemitransparentHelper;
  public
    constructor Create(AOwner: TdxCustomFlowChart); virtual;
    destructor Destroy; override;
    procedure CheckDragStart(X, Y: Integer; const AShift: TShiftState; const AHitTest: TdxFcHitTest);
    procedure DragStart(X, Y: Integer; AKind: TFlowChartDragKind); virtual;
    procedure DragMove(X, Y: Integer); virtual;
    procedure DragStop(X, Y: Integer); virtual;
    procedure CancelDrag;
    procedure ToggleSysKey(AKey: Word);
    //
    property Canvas: TcxCanvas read GetCanvas;
    property Dragging: Boolean read FDragging;
    property DragKind: TFlowChartDragKind read FDragKind;
    property Owner: TdxCustomFlowChart read FOwner;
    property SpecialControlPainting: Boolean read GetSpecialControlPainting;
  end;

  { TdxFcItem }

  TdxFcItem = class(TPersistent)
  protected type
    TState = (isSelected, isHighlighted);
    TStates = set of TState;
  private
    FData: Pointer;
    FDestroying: Boolean;
    FFont: TFont;
    FOwner: TdxCustomFlowChart;
    FParentFont: Boolean;
    FRealFont: TFont;
    FState: TStates;
    FText: string;

    procedure FontChangeHandler(Sender: TObject);
    function GetHighlighted: Boolean;
    function GetSelected: Boolean;
    procedure SetFont(Value: TFont);
    procedure SetHighlighted(Value: Boolean);
    procedure SetParentFont(Value: Boolean);
    procedure SetSelected(Value: Boolean);
    procedure SyncFontWithParentFont;
  protected
    procedure AssignAttributes(ASource: TdxFcItem); virtual;
    procedure CalculateRealFont; virtual;
    function GetDisplayRect: TRect; virtual; abstract;
    function GetInvalidateRect: TRect; virtual;
    function GetSelectionRect: TRect; virtual;
    function SelList: TList; virtual; abstract;
    procedure Changed;
    procedure ChangeScale(M, D: Integer); virtual;
    procedure FontChanged; virtual;
    procedure Invalidate; virtual; abstract;
    procedure LoadFont(Stream: TStream; AIsUnicode: Boolean); overload;
    procedure LoadFont(ANode: TdxXMLNode); overload;
    // mouse events
    procedure Click;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseLeave(AControl: TControl); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;

    procedure SaveFont(Stream: TStream); overload;
    procedure SaveFont(ANode: TdxXMLNode); overload;
    procedure SetSelectedState(ASelected: Boolean);
    procedure SetHighlightedState(AHighlighted: Boolean);
    procedure SetText(Value: string); virtual; abstract;
    procedure ScaleFont; virtual;
    //
    property DisplayRect: TRect read GetDisplayRect;
    property Highlighted: Boolean read GetHighlighted write SetHighlighted;
    property InvalidateRect: TRect read GetInvalidateRect;
    property RealFont: TFont read FRealFont;
    property SelectionRect: TRect read GetSelectionRect;
  public
    constructor Create(AOwner: TdxCustomFlowChart);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    //
    property Data: Pointer read FData write FData;
    property Destroying: Boolean read FDestroying;
    property Font: TFont read FFont write SetFont;
    property Owner: TdxCustomFlowChart read FOwner;
    property ParentFont: Boolean read FParentFont write SetParentFont;
    property Selected: Boolean read GetSelected write SetSelected;
    property Text: string read FText write SetText;
  end;

  { TdxFcObject }

  TdxFcObject = class(TdxFcItem)
  {$REGION 'internal types'}
  protected type
  {$SCOPEDENUMS ON}
    THighlightedFrameMode = (None, Selected, Connect);
  {$SCOPEDENUMS OFF}
  strict protected const
    DefaultBorderStyle = BF_RECT;
    DefaultImageIndex = -1;
    DefaultShapeWidth = 1;
  strict protected type

    TLinkedPoints = TArray<TPoint>;

    TAdvancedShapeInfo = class
    strict private
      FLastSize: TSize;
      FOwner: TdxFcObject;
      FShapeDescription: TdxShapeDescription;
      FConnectionPoints: TArray<TdxPointF>;
      FParameters: TArray<Single>;
      FPaintLocations: TArray<TdxPointF>;
      FLocations: TArray<TdxPointF>;
      FViewData: TdxShapeViewData;
      function IsDefaultParameters: Boolean;
      procedure SetParameters(const Value: TArray<Single>);
    protected
      procedure CheckSize(AWidth, AHeight: Integer);
      procedure CreateViewData(const ASize: TdxSizeF; const AParameters: TArray<Single>);
      function GetBoundingRect: TRect;
      function GetBrush(AIndex: Integer): TdxGPBrush;
      function GetPen(AIndex: Integer): TdxGPPen;
      function GetScaled(AValue: Integer): Single;
      function NeedPaintParameters: Boolean;
      procedure Update(AWidth, AHeight: Integer); overload;
      procedure Update; overload;

      property Owner: TdxFcObject read FOwner;
      property ViewData: TdxShapeViewData read FViewData;
    public
      constructor Create(AOwner: TdxFcObject);
      destructor Destroy; override;
      procedure CalculatePaintLocations;
      procedure ChangeParameter(AIndex: Integer; const APoint: TdxPointF);
      function GetParameterIndex(const P: TPoint): Integer;
      function IsPointInsideShape(const P: TPoint): Boolean; virtual;
      procedure Paint(AGraphics: TdxGPGraphics; const ABounds: TRect);
      procedure PaintParameters(AGraphics: TdxGPGraphics);

      procedure ReadParameters(AAttributes: TdxXMLNodeAttributes);
      procedure WriteParameters(AAttributes: TdxXMLNodeAttributes);

      property ConnectionPoints: TArray<TdxPointF> read FConnectionPoints;
      property Locations: TArray<TdxPointF> read FLocations;
      property PaintLocations: TArray<TdxPointF> read FPaintLocations;
      property Parameters: TArray<Single> read FParameters write SetParameters;
      property ShapeDescription: TdxShapeDescription read FShapeDescription;
    end;

    TFontData = class
    strict private
      FBrush: TdxGPBrush;
      FFont: TdxGPFont;
      FFormat: TdxGPStringFormat;
      FOwner: TdxFcObject;
    protected
      property Owner: TdxFcObject read FOwner;
    public
      constructor Create(AOwner: TdxFcObject);
      destructor Destroy; override;
      procedure Update;
      procedure UpdateFontFormat;
      procedure DrawText(AGraphics: TdxGPGraphics; const ABounds: TdxRectF);

      property Brush: TdxGPBrush read FBrush;
      property Font: TdxGPFont read FFont;
      property Format: TdxGPStringFormat read FFormat;
    end;
  {$ENDREGION}
  private
    FAdvancedShapeInfo: TAdvancedShapeInfo;
    FAngle: Single;
    FBkColor: TColor;
    FBorder: Word;
    FBoundingRect: TRect;
    FConnections: TList;
    FFontData: TFontData;
    FHighlightedFrameMode: THighlightedFrameMode;
    FHighlightedLinkedPointIndex: Integer;
    FCustomData: string;
    FEdge: Word;
    FHeight: Word;
    FHorzImagePos: TdxFcHorzPos;
    FHorzTextPos: TdxFcHorzPos;
    FImageIndex: Smallint;
    FLeft: Integer;
    FLinkedObjects: TList;
    FObjects: TList;
    FRealShapeWidth: Word;
    FRealContentIndent: Integer;
    FShapeColor: TColor;
    FShapeStyle: TPenStyle;
    FShapeType: TdxFcShapeType;
    FShapeWidth: Byte;
    FShowConnectionPoints: Boolean;
    FTag: Integer;
    FTop: Integer;
    FTransparent: Boolean;
    FVertImagePos: TdxFcVertPos;
    FVertTextPos: TdxFcVertPos;
    FVisible: Boolean;
    FWidth: Word;
    FAdvancedShape: TdxFlowChartObjectAdvancedShape;

    function GetClientRect: TRect;
    function GetConnection(Index: Integer): TdxFcConnection;
    function GetConnectionCount: Integer;
    function GetIsUnion: Boolean;
    function GetLinkedObject(Index: Integer): TdxFcObject;
    function GetLinkedObjectCount: Integer;
    function GetObjectCount: Integer;
    function GetObjectValue(Index: Integer): TdxFcObject;  // renamed because of C++Builder
    function GetPainter: TdxFlowChartPainter;
    function GetPoint(const P: array of TPoint; X, Y, ACount: Integer): Integer;
    function GetZOrder: Word;
    function HasEdge: Boolean;
    function HasImage: Boolean;
    procedure ResolveObjRefs;

    procedure SetAdvanceShape(Value: TdxFlowChartObjectAdvancedShape);
    procedure SetAngle(Value: Single);
    procedure SetBkColor(Value: TColor);
    procedure SetBorder(Value: Word);
    procedure SetEdge(Value: Word);
    procedure SetHeight(Value: Word);
    procedure SetHighlightedLinkedPointIndex(Value: Integer);
    procedure SetHorzImagePos(Value: TdxFcHorzPos);
    procedure SetHorzTextPos(Value: TdxFcHorzPos);
    procedure SetImageIndex(Value: Smallint);
    procedure SetLeft(Value: Integer);
    procedure SetShapeColor(Value: TColor);
    procedure SetShapeStyle(Value: TPenStyle);
    procedure SetShapeType(Value: TdxFcShapeType);
    procedure SetShapeWidth(Value: Byte);
    procedure SetTop(Value: Integer);
    procedure SetTransparent(Value: Boolean);
    procedure SetVertImagePos(Value: TdxFcVertPos);
    procedure SetVertTextPos(Value: TdxFcVertPos);
    procedure SetVisible(Value: Boolean);
    procedure SetWidth(Value: Word);
    procedure SetZOrder(Value: Word);
    procedure UpdateConnections;
    function GetBounds: TRect;
  protected
    FRealHeight: Integer;
    FRealLeft: Integer;
    FRealTop: Integer;
    FRealWidth: Integer;

    LinkedPoints: TLinkedPoints;

    function AdvancedShapePointToLinkedPoint(const P: TdxPointF): TPoint;
    procedure AlignWithGrid; virtual;
    procedure CalulateAdvancedShapeLinkedPoints; virtual;
    procedure CalculateLinkedPointsForBounds(const ABounds: TRect);
    procedure CalculateLinkedPoints; virtual;
    procedure CalculateRealBounds; virtual;
    procedure CalculateRealShapeWidth; virtual;
    function CalculatePolygon(out APolygon: TPoints; AIndent: Integer): Boolean; virtual;
    function CalculateRegion(const ARect: TRect; AShapeWidth: Integer): TcxRegionHandle; virtual;
    function CanRotate: Boolean; virtual;
    function CanUseParameters: Boolean; virtual;
    procedure ChangeScale(M, D: Integer); override;
    function ContainsPoint(X, Y, ATolerance: Integer): Boolean;
    function CreateClientRegion: TcxRegionHandle;
    function CreateRegion: TcxRegionHandle;
    function CreateShapeRegion: TcxRegionHandle;
    class function GetBoundingRect(const APoints: array of TPoint; ACount: Integer): TRect; overload;
    function GetResizeCursor(const P: TPoint): TCursor; virtual;
    function GetDefaultSize: TSize;
    function GetDisplayRect: TRect; override;
    function GetInvalidateRect: TRect; override;
    function GetLinkedPointCount: Integer;
    function GetSelectionRect: TRect; override;
    function ControlToObjectPoint(const P: TPoint): TPoint;
    function SelList: TList; override;
    function UserRegion(R: TRect): HRgn; virtual;
    procedure Invalidate; override;
    function NeedAlignWithGrid: Boolean;
    function PointInRealDisplayRect(const P: TPoint): Boolean;
    function GetParameterIndex(const P: TPoint): Integer;
    function PointOnRotationMark(const P: TPoint): Boolean;
    function PointOnSelectionFrame(const P: TPoint): Boolean;
    procedure UpdateFontFormat;

    procedure Load(Stream: TStream; AIsUnicode: Boolean); overload; virtual;
    procedure Load(ANode: TdxXMLNode); overload; virtual;
    procedure Save(Stream: TStream); overload; virtual;
    procedure Save(ANode: TdxXMLNode); overload; virtual;

    function IsRotated: Boolean; inline;
    procedure Move(DX, DY: Integer); virtual;
    procedure ScaleFont; override;
    procedure SelPoints(var APoints: TdxFcSelectionPoints); overload;
    procedure SelPoints(var APoints: TdxFcSelectionPoints; const ABounds: TRect); overload;
    procedure SetPosition(ALeft, ATop: Integer);
    procedure SetText(Value: string); override;
    procedure UpdateFontData;
    procedure UpdateSize(AWidth, AHeight: Integer);
    procedure UserLinkedPoints; virtual;
    procedure ValidatePosition(var X, Y: Integer); virtual;
    procedure ValidateSize(var AWidth, AHeight: Integer); virtual;
    procedure ZoomChanged; virtual;
    //
    procedure CreateAdvancedShapeInfo;
    procedure FreeAdvancedShapeInfo;
    //
    function GetAdvancedShapeImageBounds(const AContentBounds: TRect): TRect;
    procedure Paint(ACanvas: TcxCanvas); virtual;
    procedure PaintAdvancedShape(ACanvas: TcxCanvas); virtual;
    procedure PaintAdvancedShapeImage(AGraphics: TdxGPGraphics; const R: TRect);
    procedure PaintAdvancedShapeText(AGraphics: TdxGPGraphics; const R: TRect);
    procedure PaintAdvancedShapeContent(AGraphics: TdxGPGraphics; const ABounds: TRect);

    procedure PaintBackground(ACanvas: TcxCanvas); virtual;
    procedure PaintConnectionPoints(AGraphics: TdxGPGraphics); virtual;
    procedure PaintHighlightedFrame(AGraphics: TdxGPGraphics; AColor: TdxAlphaColor); virtual;
    procedure PaintHighlights(ACanvas: TcxCanvas);
    //
    property AdvancedShapeInfo: TAdvancedShapeInfo read FAdvancedShapeInfo;
    property ClientRect: TRect read GetClientRect;
    property LinkedPointCount: Integer read GetLinkedPointCount;
    property FontData: TFontData read FFontData;
    property HighlightedFrameMode: THighlightedFrameMode read FHighlightedFrameMode write FHighlightedFrameMode;
    property HighlightedLinkedPointIndex: Integer read FHighlightedLinkedPointIndex write SetHighlightedLinkedPointIndex;
    property Painter: TdxFlowChartPainter read GetPainter;
    property RealContentIndent: Integer read FRealContentIndent;
    property RealShapeWidth: Word read FRealShapeWidth;
    property ShowConnectionPoints: Boolean read FShowConnectionPoints write FShowConnectionPoints;
  public
    constructor Create(AOwner: TdxCustomFlowChart);
    destructor Destroy; override;
    function GetLinkedPoint(X, Y: Integer): Integer;
    function GetSelPoint(X, Y: Integer): Integer;
    function HasInUnion(AObject: TdxFcObject): Boolean;
    function IsPointInsideShape(const P: TPoint): Boolean; virtual;
    function HitTest(const P: TPoint): TdxFcHitTest; virtual;
    function InRect(const R: TRect): Boolean;
    procedure Assign(Source: TPersistent); override;
    procedure AddToUnion(AObject: TdxFcObject);
    procedure RemoveFromUnion(AObject: TdxFcObject);
    procedure BringToFront;
    procedure ClearUnion;
    function GetBoundingRect: TRect; overload;
    function GetBoundsForRotatedRect(const R: TRect): TRect;
    procedure MakeVisible;
    procedure PaintImage(const R: TRect);
    procedure PaintText(const R: TRect);
    procedure PutInFrontOf(Value: TdxFcObject);
    procedure SelectUnion;
    procedure SendToBack;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); overload;
    procedure SetBounds(const R: TRect); overload;
    //
    property AdvancedShape: TdxFlowChartObjectAdvancedShape read FAdvancedShape write SetAdvanceShape;
    property Angle: Single read FAngle write SetAngle;
    property BkColor: TColor read FBkColor write SetBkColor;
    property BorderStyle: Word read FBorder write SetBorder;
    property Bounds: TRect read GetBounds write SetBounds;
    property ConnectionCount: Integer read GetConnectionCount;
    property Connections[Index: Integer]: TdxFcConnection read GetConnection;
    property CustomData: string read FCustomData write FCustomData;
    property EdgeStyle: Word read FEdge write SetEdge;

    property Height: Word read FHeight write SetHeight;
    property HorzImagePos: TdxFcHorzPos read FHorzImagePos write SetHorzImagePos;
    property HorzTextPos: TdxFcHorzPos read FHorzTextPos write SetHorzTextPos;
    property ImageIndex: Smallint read FImageIndex write SetImageIndex;
    property IsUnion: Boolean read GetIsUnion;
    property Left: Integer read FLeft write SetLeft;
    property LinkedObjectCount: Integer read GetLinkedObjectCount;
    property LinkedObjects[Index: Integer]: TdxFcObject read GetLinkedObject;
    property ObjectCount: Integer read GetObjectCount;
    property Objects[Index: Integer]: TdxFcObject read GetObjectValue;
    property RealHeight: Integer read FRealHeight;
    property RealLeft: Integer read FRealLeft;
    property RealTop: Integer read FRealTop;
    property RealWidth: Integer read FRealWidth;
    property ShapeColor: TColor read FShapeColor write SetShapeColor;
    property ShapeStyle: TPenStyle read FShapeStyle write SetShapeStyle;
    property ShapeType: TdxFcShapeType read FShapeType write SetShapeType;
    property ShapeWidth: Byte read FShapeWidth write SetShapeWidth;
    property Tag: Integer read FTag write FTag;
    property Top: Integer read FTop write SetTop;
    property Transparent: Boolean read FTransparent write SetTransparent;
    property VertImagePos: TdxFcVertPos read FVertImagePos write SetVertImagePos;
    property VertTextPos: TdxFcVertPos read FVertTextPos write SetVertTextPos;
    property Visible: Boolean read FVisible write SetVisible;
    property Width: Word read FWidth write SetWidth;
    property ZOrder: Word read GetZOrder write SetZOrder;
  end;

  { TdxFcConnectionArrow }

  TdxFcConnectionArrow = class(TPersistent)
  strict private class var
    FRepository: TdxFlowChartArrowShapeRepository;
  private
    FArrowType: TdxFcaType;
    FColor: TColor;
    FHeight: Byte;
    FOwner: TdxFcConnection;
    FPoints: array[0..1] of TPoint;
    FRealHeight: Word;
    FRealWidth: Word;
    FShape: TdxFlowChartArrowShape;
    FWidth: Byte;

    function CalculateSize(Value: TdxFcaSize): TSize;
    function GetActive: Boolean;
    function GetPainter: TdxFlowChartPainter;
    function GetSize: TdxFcaSize;
    procedure SetArrowType(Value: TdxFcaType);
    procedure SetColor(Value: TColor);
    procedure SetHeight(Value: Byte);
    procedure SetWidth(Value: Byte);
    procedure SetSize(Value: TdxFcaSize);
  protected
    class procedure Initialize; static;
    class procedure Finalize; static;

    procedure ClearPoints;
    procedure ChangeScale(M, D: Integer); virtual;
    function DisplayRect(Ext: Boolean): TRect;
    function GetScaled(AValue: Integer): Integer;
    function GetUnscaled(AValue: Integer): Integer;
    procedure OffsetPoints(DX, DY: Integer); virtual;
    procedure Paint(AGraphics: TdxGPGraphics); virtual;
    procedure Reset;
    procedure Load(const AData: TdxFcArwData); overload;
    procedure Load(ANode: TdxXMLNode); overload;
    procedure Save(out AData: TdxFcArwData); overload;
    procedure Save(ANode: TdxXMLNode); overload;
    procedure SetPoints(Index: Integer); virtual;
    procedure SetRealBounds; virtual;
    //
    property Active: Boolean read GetActive;
    property Painter: TdxFlowChartPainter read GetPainter;
    class property Repository: TdxFlowChartArrowShapeRepository read FRepository;
    property Shape: TdxFlowChartArrowShape read FShape;
  public
    constructor Create(AOwner: TdxFcConnection);
    procedure Assign(Source: TPersistent); override;
    //
    property Owner: TdxFcConnection read FOwner;
    property ArrowType: TdxFcaType read FArrowType write SetArrowType;
    property Height: Byte read FHeight write SetHeight;
    property Width: Byte read FWidth write SetWidth;
    property Color: TColor read FColor write SetColor;
    property Size: TdxFcaSize read GetSize write SetSize;
  end;

  { TdxFcConnection }

(*
  Point1 and Point2 properties is the linked point type.
  It may have value from 0..15.
  0   1   2   3   4
  15              5
  14              6
  13              7
  12  11  10  9   8
*)

  TdxFcConnection = class(TdxFcItem)
  protected const
    DefaultPenStyle = psSolid;
    DefaultTransparent = False;
    DefaultPenWidth = 1;
  private
    FArrowDest: TdxFcConnectionArrow;
    FArrowSource: TdxFcConnectionArrow;
    FColor: TColor;
    FMassCenter: TPoint;
    FObjectDest: TdxFcObject;
    FObjectSource: TdxFcObject;
    FPenStyle: TPenStyle;
    FPointDest: Byte;
    FPoints: TdxFcPoints;
    FPointSource: Byte;
    FStyle: TdxFclStyle;
    FTextRect: TRect;
    FTransparent: Boolean;
    FVisible: Boolean;

    function GetPoint(Index: Integer): TPoint;
    function GetPointCount: Integer;
    function GetRealCount: Integer;
    function GetRealPoint(Index: Integer): TPoint;
    function GetRealTextRect: TRect;
    function HasPoint(const P: TPoint): Boolean;
    function IndexValid(var Index: Integer; AMax: Integer): Boolean;
    function RealStyle: TdxFclStyle;
    function ScreenPoint(Index: Integer): TPoint;
    procedure DelObj(AObj, Partneur: TdxFcObject; Index: Integer);
    procedure InsObj(AObj, Partneur: TdxFcObject; Index: Integer);
    procedure InvalidateText;
    procedure NewPoint(X, Y: Integer);
    procedure OffsetRealPoints(DX, DY: Integer);
    procedure SetArrowSource(Value: TdxFcConnectionArrow);
    procedure SetArrowDest(Value: TdxFcConnectionArrow);
    procedure SetColor(Value: TColor);
    procedure SetPenStyle(Value: TPenStyle);
    procedure SetPenWidth(Value: Integer);
    procedure SetPoint(AIndex: Integer; AValue: TPoint);
    procedure SetStyle(Value: TdxFclStyle);
    procedure SetTransparent(Value: Boolean);
    procedure SetVisible(Value: Boolean);
    procedure ZoomChanged;
  protected
    FDisplayRect: TRect;
    FPenWidth: Integer;
    FRealPoints: TdxFcPoints;
    function GetDisplayRect: TRect; override;
    function SelList: TList; override;
    procedure AdjustObjectsPoints(var APointSource, APointDest: Byte); virtual;
    procedure ArrowChanged(Value: TdxFcConnectionArrow);
    procedure AssignAttributes(ASource: TdxFcItem); override;
    procedure AssignGeometry(ASource: TdxFcConnection);
    procedure Calculate;
    procedure CalculateDisplayRect; virtual;
    function CalculatePrevDirectionBasedPoint(AForSource: Boolean): TPoint;
    procedure CalculateRealPoints;
    procedure CalculateTextRect; virtual;
    procedure CalculateTextRects; virtual;
    procedure ChangeScale(M, D: Integer); override;
    procedure CheckRouting;
    procedure ClearIntermediatePoints;
    procedure ClearPoints;
    procedure ConnectionChanged;
    function CreateArrow: TdxFcConnectionArrow; virtual;
    procedure FontChanged; override;
    procedure InitializeEndpointsForAutorouting;
    procedure InternalInsertPoint(AIndex: Integer; P: TPoint);
    procedure InsertDisplayPoint(AIndex: Integer; P: TPoint);
    procedure Invalidate; override;
    procedure Load(Stream: TStream; AIsUnicode: Boolean); overload; virtual;
    procedure Load(ANode: TdxXMLNode); overload; virtual;
    procedure Move(DX, DY: Integer; const ARealOffset: TPoint); virtual;
    procedure OptimizeEndpoints;
    procedure Save(Stream: TStream); overload; virtual;
    procedure Save(ANode: TdxXMLNode); overload; virtual;
    procedure SetText(Value: string); override;
    procedure SetObjectPoints; virtual;
    //Paint
    function GetActualPenWidth: Single; virtual;
    procedure DrawContent(ACanvas: TcxCanvas); virtual;
    procedure DrawLine(AGraphics: TdxGPGraphics); virtual;
    procedure DrawText(ACanvas: TcxCanvas); virtual;
    procedure Paint(ACanvas: TcxCanvas; Upper: Boolean); virtual;
    //AutoRoute
    function CanRoute: Boolean; virtual;
    procedure GetStartEndPoints(out AStart, AEnd: TPoint); virtual;
    procedure ReRoute; virtual;
    procedure Route; virtual;
    //
    property RealCount: Integer read GetRealCount;
    property RealPoints[Index: Integer]: TPoint read GetRealPoint;
    property RealTextRect: TRect read GetRealTextRect;
  public
    constructor Create(AOwner: TdxCustomFlowChart);
    destructor Destroy; override;
    function GetNearestPoint(X, Y: Integer): Integer;
    function InRect(const R: TRect): Boolean;
    procedure Assign(Source: TPersistent); override;
    procedure AddPoint(const P: TPoint);
    procedure InsertPoint(Index: Integer; const P: TPoint);
    procedure RemovePoint(Index: Integer);
    procedure SetObjectSource(AObject: TdxFcObject; APoint: Byte);
    procedure SetObjectDest(AObject: TdxFcObject; APoint: Byte);
    //
    property ArrowSource: TdxFcConnectionArrow read FArrowSource write SetArrowSource;
    property ArrowDest: TdxFcConnectionArrow read FArrowDest write SetArrowDest;
    property Color: TColor read FColor write SetColor;
    property ObjectSource: TdxFcObject read FObjectSource;
    property ObjectDest: TdxFcObject read FObjectDest;
    property PenStyle: TPenStyle read FPenStyle write SetPenStyle;
    property PenWidth: Integer read FPenWidth write SetPenWidth;  // is not saved in the stream/file
    property PointCount: Integer read GetPointCount;
    property Points[Index: Integer]: TPoint read GetPoint write SetPoint;
    property PointSource: Byte read FPointSource;
    property PointDest: Byte read FPointDest;
    property Style: TdxFclStyle read FStyle write SetStyle;
    property Transparent: Boolean read FTransparent write SetTransparent;
    property Visible: Boolean read FVisible write SetVisible;
  end;

  { TdxDiagramRoutingStrategy }

  TdxDiagramRoutingStrategy = class
  strict private
    FChart: TdxCustomFlowChart;
    FItemMargin: Integer;
  protected
    property Chart: TdxCustomFlowChart read FChart;
    property ItemMargin: Integer read FItemMargin;
  public
    constructor Create(AChart: TdxCustomFlowChart; AItemMargin: Integer); virtual;
    function RouteConnection(AConnection: TdxFcConnection; const AStart, AEnd: TPoint): TArray<TPoint>; virtual; abstract;
  end;

  { TdxFcSelection }

  TdxFcSelection = class
  strict protected type
  {$SCOPEDENUMS ON}
    TPaintMode = (None, Connection, &Object, MultiSelection);
  {$SCOPEDENUMS OFF}
  strict private class var
    FRotationImage: TdxSmartImage;
  strict private
    FConnectionPointBrush: TdxGPBrush;
    FConnectionPointPen: TdxGPPen;
    FConnectionPointRadius: Single;
    FEndpointInnerRadius: Single;
    FGuides: TdxGuidesViewData;
    FHasEqualWidth: Boolean;
    FHasEqualHeight: Boolean;
    FOwner: TdxCustomFlowChart;
    FPaintMode: TPaintMode;
    FResizingMarkBrush: TdxGPBrush;
    FResizingMarkPen: TdxGPPen;
    FResizingMarkRadius: Single;
    FRotationMarkOffset: Single;
    FRotationMarkSize: Single;
    FSelectionFramePen: TdxGPPen;
    FSelectionIndent: Single;
  strict protected
    procedure AdjustObjectSelectionBounds(AObject: TdxFcObject; var ABounds: TRect); virtual;
    function CanResize: Boolean;
    function CanRotate: Boolean;
    function ConnectionPointToDrawPoint(AConnection: TdxFcConnection; AIndex: Integer): TPoint;
    procedure CreateGraphicTools; virtual;
    procedure DestroyGraphicTools; virtual;
    procedure DrawObjectBoundingRect(AGraphics: TdxGPGraphics; ABounds: TRect); virtual;
    procedure DrawConnectionEndPointMark(AGraphics: TdxGPGraphics; const P: TPoint); virtual;
    procedure DrawConnectionIntermediatePointMark(AGraphics: TdxGPGraphics; const P: TPoint); virtual;
    procedure DrawConnectionSelection(AGraphics: TdxGPGraphics; AConnection: TdxFcConnection); virtual;
    procedure DrawEqualityArrows(AGraphics: TdxGPGraphics; const ASize: TSize; const ABounds: TRect; APen: TdxGPPen; ABrush: TdxGPBrush); virtual;
    procedure DrawEqualitySizes(AGraphics: TdxGPGraphics; ASize: TSize; ABounds: TList<TRect>); virtual;
    procedure DrawObjectSelection(AGraphics: TdxGPGraphics; AObject: TdxFcObject); virtual;
    procedure DrawResizingMark(AGraphics: TdxGPGraphics; const P: TPoint); overload; virtual;
    procedure DrawResizingMark(AGraphics: TdxGPGraphics; X, Y: Integer); overload; virtual;
    procedure DrawRotationMark(AGraphics: TdxGPGraphics; const ACenterPoint: TPoint); virtual;
    procedure DrawSelectionFrame(AGraphics: TdxGPGraphics; const ABounds: TRect); virtual;
    function GetPaintMode: TPaintMode;
    function ShowEndpointsMarks: Boolean; virtual;
    procedure UpdateSizes; virtual;
  protected
    class procedure InitializeImages;
    procedure DrawGuides(AGraphics: TdxGPGraphics); virtual;

    property ConnectionPointBrush: TdxGPBrush read FConnectionPointBrush;
    property ConnectionPointPen: TdxGPPen read FConnectionPointPen;
    property ConnectionPointRadius: Single read FConnectionPointRadius;
    property EndpointInnerRadius: Single read FEndpointInnerRadius;
    property Guides: TdxGuidesViewData read FGuides;
    property Owner: TdxCustomFlowChart read FOwner;
    property ResizingMarkBrush: TdxGPBrush read FResizingMarkBrush;
    property ResizingMarkPen: TdxGPPen read FResizingMarkPen;
    class property RotationImage: TdxSmartImage read FRotationImage;
    property RotationMarkOffset: Single read FRotationMarkOffset;
    property RotationMarkSize: Single read FRotationMarkSize;
    property SelectionFramePen: TdxGPPen read FSelectionFramePen;
  public
    constructor Create(AOwner: TdxCustomFlowChart);
    destructor Destroy; override;
    procedure Prepare; virtual;
    procedure Paint; virtual;
    procedure UpdateGraphicTools;

    property PaintMode: TPaintMode read FPaintMode write FPaintMode;
    property ResizingMarkRadius: Single read FResizingMarkRadius;
    property SelectionIndent: Single read FSelectionIndent;
  end;

  { TdxCustomFlowChart }

  TdxCustomFlowChart = class(TcxControl)
  protected type
  {$SCOPEDENUMS ON}
    TCreateObjectReference = reference to function (): TdxFcObject;

    TPointInfo = class
    public
      &Object: TdxFcObject;
      Index: Integer;
      constructor Create(AObject: TdxFcObject; AIndex: Integer);
      procedure Update(AObject: TdxFcObject; AIndex: Integer);
    end;

    TConnectionHighlights = class
    strict private
      FChart: TdxCustomFlowChart;
      FMouseInChart: Boolean;
      FPointInfo: TPointInfo;
      FObject: TdxFcObject;
      FObjects: TList<TdxFcObject>;
      FObjectsTolerance: Integer;
      FPointsTolerance: Integer;
      function GetSquareDistance(const AObject: TdxFcObject; ALinkedPointIndex: Integer; const APosition: TPoint): Int64;
    strict protected
      function CanHighlightObject(AObject: TdxFcObject): Boolean;
      function CanHighlightObjectPoints(AObject: TdxFcObject): Boolean;
      procedure ClearObjectHighlights;
      function GetHighlightedObject(X, Y, ATolerance: Integer): TdxFcObject;
      procedure SetLinkedPointsVisibility(AHighlight: Boolean);
      procedure Populate(X, Y, ATolerance: Integer);
      procedure UpdateHighlightedObject(X, Y: Integer);

      property Chart: TdxCustomFlowChart read FChart;
    public
      constructor Create(AChart: TdxCustomFlowChart; X, Y, ABaseTolerance: Integer);
      destructor Destroy; override;
      function GetDestinationPoint(X, Y: Integer): TPoint;
      procedure Update(X, Y: Integer);

      property MouseInChart: Boolean read FMouseInChart;
      property &Object: TdxFcObject read FObject;
      property PointInfo: TPointInfo read FPointInfo;
    end;
  {$SCOPEDENUMS OFF}
  strict private class var
    FRepository: TdxFlowChartAdvancedShapeRepository;
    class constructor Initialize;
  {$HINTS OFF}
    class destructor Finalize;
  {$HINTS ON}
  private
    FAntialiasing: Boolean;
    FChangeLink: TChangeLink;
    FChartHeight: Integer;
    FChartLeft: Integer;
    FChartTop: Integer;
    FChartWidth: Integer;
    FConnectionAt: TdxFcConnection;
    FConnectionHighlights: TConnectionHighlights;
    FConnections: TList;
    FDragHelper: TdxFlowChartDragHelper;
    FDragStartPoint: TPoint;
    FSelectionSpecialCanvas: TcxCanvas;
    FGridLineOptions: TdxFlowChartGridOptions;
    FHasContentChanges: Boolean;
    FHitTestResult: TdxFcHitTest;
    FHitX: Integer;
    FHitY: Integer;
    FHitParameterIndex: Integer;
    FImages: TCustomImageList;
    FLeftEdge: Integer;
    FLoading: Boolean;
    FLockUpdates: Integer;
    FNeedUpdateRouting: Boolean;
    FObjectAt: TdxFcObject;
    FObjects: TList;
    FOptions: TdxFcOptions;
    FPainter: TdxFlowChartPainter;
    FPaintScaleFactor: TdxScaleFactor;
    FPressedItem: TdxFcItem;
    FRealZoom: Word;
    FRepaint: Boolean;
    FRoutingStrategy: TdxDiagramRoutingStrategy;
    FSelConnections: TList;
    FSelection: TdxFcSelection;
    FSelObjects: TList;
    FHighlightedObject: TdxFcItem;
    FTopEdge: Integer;
    FUpdateChartSizeLocked: Boolean;
    FZoom: Word;

    FOnChange: TdxFcEvent;
    FOnCreateItem: TdxFcEvent;
    FOnDeletion: TdxFcEvent;
    FOnDrawObject: TdxFcDrawEvent;
    FOnDrawObjectEx: TdxFcDrawExEvent;
    FOnEdited: TdxFcEditEvent;
    FOnEditing: TdxFcAllowEvent;
    FOnSelected: TdxFcEvent;
    FOnSelection: TdxFcAllowEvent;

    function GetCanvas: TcxCanvas;
    function CanPaint: Boolean;
    function GetBackgroundColor: TColor;
    function GetBorderStyle: TBorderStyle;
    function GetClientHeight: Integer;
    function GetClientWidth: Integer;
    function GetConnection(Index: Integer): TdxFcConnection; inline;
    function GetConnectionCount: Integer; inline;
    function GetObjectCount: Integer; inline;
    function GetObjectValue(Index: Integer): TdxFcObject; inline; // renamed because of C++Builder
    function GetSelConnect: TdxFcConnection;
    function GetSelectedConnection(Index: Integer): TdxFcConnection;
    function GetSelectedConnectionCount: Integer;
    function GetSelectedObject(Index: Integer): TdxFcObject;
    function GetSelectedObjectCount: Integer;
    function GetSelObj: TdxFcObject;
    function TmpSel: Integer;
    procedure AbortDrag;
    procedure CalculateChartSizes;
    procedure CalculateRealPos;
    function CanScaleByMouseWheel(AShift: TShiftState; const AMousePos: TPoint): Boolean;
    procedure DoChangeLink(Sender: TObject);
    procedure InvalidateSel;
    procedure RestoreSel(Value: Integer);
    procedure SetAntialiasing(AValue: Boolean);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetChartSizes;
    procedure SetConnection(Index: Integer; Value: TdxFcConnection);
    procedure SetGridLineOptions(Value: TdxFlowChartGridOptions);
    procedure SetHighlightedObject(AObject: TdxFcItem);
    procedure SetImages(Value: TCustomImageList);
    procedure SetLeftEdge(Value: Integer);
    procedure SetObjectValue(Index: Integer; Value: TdxFcObject);
    procedure SetOptions(Value: TdxFcOptions);
    procedure SetSelConnect(Value: TdxFcConnection);
    procedure SetSelObj(Value: TdxFcObject);
    procedure SetTopEdge(Value: Integer);
    procedure SetZoom(AValue: Word);
    //
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CMFontChanged(var Msg: TMessage); message CM_FONTCHANGED;
    procedure CNKeyDown(var Message: TWMKeyDown); message CN_KEYDOWN;
    procedure CNKeyUp(var Message: TWMKeyUp); message CN_KEYUP;
    procedure WMErase(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
  protected
    function CanSelect(Item: TdxFcItem): Boolean; virtual;
    function CanCreateNewConnectionOnDrag: Boolean; virtual;
    function CanDetachConnections: Boolean; virtual;
    function CanDragConnectionEndpoints: Boolean; virtual;
    function CanDragConnectionIntermediatePoints: Boolean; virtual;
    function CanResizeObjectsHorizontal: Boolean; virtual;
    function CanResizeObjectsVertical: Boolean; virtual;
    class function CreateArrowShapeRepository: TdxFlowChartArrowShapeRepository; virtual;
    function CreateDragHelper: TdxFlowChartDragHelper; virtual;
    function CreatePainter: TdxFlowChartPainter; virtual;
    function CreateRoutingStrategy: TdxDiagramRoutingStrategy; virtual;
    function CreateSelection: TdxFcSelection; virtual;
    procedure DoCancelMode; override;
    function GetAlignedChartPoint(X, Y: Integer): TPoint;
    function GetAlignedDisplayPoint(X, Y: Integer): TPoint;
    function GetAutoRouting: Boolean; virtual;
    function GetDisplayBounds(const ABounds: TRect): TRect;
    function GetItemBounds(const ADisplayBounds: TRect): TRect;
    function GetEndpointHighlightedObject: TdxFcObject;
    procedure HitTest(X, Y: Integer);
    procedure ImageListChanged; virtual;
    function IsDefaultGesture(AGestureID: Integer): Boolean; override;
    function InternalCreateConnection: TdxFcConnection; virtual;
    function InternalCreateObject: TdxFcObject; virtual;
    procedure LoadFromOldStream(Stream: TStream); virtual;
    procedure LoadFromXmlStream(Stream: TStream); virtual;
    function NeedAlignObjectBoundsWithGrid: Boolean;
    function NeedRedrawOnResize: Boolean; override;
    procedure ScalePoint(var P: TPoint);
    function ScrollPoint(const P: TPoint; APositiveFactor: Boolean = True): TPoint;
    function ScrollRect(const R: TRect; APositiveFactor: Boolean = True): TRect;
    procedure UnscalePoint(var P: TPoint);
    function UseSnapToGuides: Boolean;

    procedure Added(Item: TdxFcItem); virtual;
    procedure AlignObjectsWithGrid;
    function AllowTouchScrollUIMode: Boolean; override;
    procedure BoundsChanged; override;
    procedure CalculateRealZoomFactor; virtual;
    procedure Changed(Item: TdxFcItem); virtual;
    procedure ChangeScaleEx(M: Integer; D: Integer; isDpiChange: Boolean); override;
    function ChartPointToDisplayPoint(const P: TPoint): TPoint; overload;
    function ChartPointToDisplayPoint(const P: TdxPointDouble): TPoint; overload;
    procedure ContentChanged; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DefaultDrawObject(AObject: TdxFcObject; R: TRect); virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Delete(Item: TdxFcItem); virtual;
    procedure DoDrawObject(AObject: TdxFcObject; R: TRect); virtual;
    procedure DoDrawObjectEx(AObject: TdxFcObject; AGraphics: TdxGPGraphics; const APaintData: TdxFcDrawExEventPaintData); virtual;
    procedure DoUpdateRouting; virtual;
    function GetScrollContentForegroundColor: TColor; override;
    function GetScrollLineSize: Integer; virtual;
    function HasDrawObjectEx: Boolean;
    function HasSelection: Boolean;
    function HasMultiSelection: Boolean;
    procedure InitScrollBarsParameters; override;
    function InternalMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure InvalidateRouting;
    function IsLocked: Boolean;
    function IsMouseWheelHandleNeeded(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseLeave(AControl: TControl); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure OffsetObjects(DX, DY: Integer);
    procedure StartExternalDrag(const AScreenPoint: TPoint; const ACreateObject: TCreateObjectReference);

    procedure StartConnectionEndpointHighlight(X, Y: Integer);
    procedure StopConnectionEndpointHighlight;

    procedure BeginDragObjectsPaint(ACanvas: TcxCanvas);
    procedure EndDragObjectsPaint;
    procedure LockUpdateChartSize;
    procedure UnlockUpdateChartSize;

    function NeedAdjustObjectsPoints: Boolean; virtual;
    procedure NeedRepaint;
    procedure NeedRepaintObject(AItem: TdxFcItem);
    procedure DoPaint; override;
    procedure DoScrollUIModeChanged; override;
    procedure OptimizeConnectionEndpoints;
    procedure PaintBackground; virtual;
    procedure PaintConnections(AUpper: Boolean); virtual;
    procedure PaintSelection; virtual;
    procedure PrepareSelection; virtual;
    procedure ProcessMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure ScaleFactorChanged; override;
    procedure Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer); override;
    procedure Select(Item: TdxFcItem); virtual;
    procedure UpdatePaintScaleFactor; virtual;
    procedure UpdateRouting;
    procedure ValidateObjectsResizing(var ASelection: TdxFcSelectionPoints);
    procedure WndProc(var Message: TMessage); override;

    procedure ApplySugiaymaPositionInfo(AInfo: TdxSugiyamaPositionsInfo; AUseDummyEdges: Boolean); virtual;
    procedure ApplySugiaymaLayout; virtual;
    function CreateGraph: TdxGraph; virtual;
    function CreateGraphPageInfo: TdxPageInfo; virtual;
    function CreateSugiyamaLayoutSettings: TdxSugiyamaLayoutSettings; virtual;
    procedure CurveConnections; virtual;
    function UseDummyEdgesForRouting: Boolean; virtual;

    property AutoRouting: Boolean read GetAutoRouting;
    property BackgroundColor: TColor read GetBackgroundColor;
    property BorderStyle: TBorderStyle read GetBorderStyle write SetBorderStyle default bsSingle;
    property ChartHeight: Integer read FChartHeight;
    property ChartWidth: Integer read FChartWidth;
    property ClientHeight: Integer read GetClientHeight;
    property ClientWidth: Integer read GetClientWidth;
    property ConnectionAt: TdxFcConnection read FConnectionAt;
    property ConnectionHighlights: TConnectionHighlights read FConnectionHighlights;
    property DragHelper: TdxFlowChartDragHelper read FDragHelper;
    property GridLineOptions: TdxFlowChartGridOptions read FGridLineOptions write SetGridLineOptions;
    property HighlightedObject: TdxFcItem read FHighlightedObject write SetHighlightedObject; //TODO: Hide property!
    property HitTestResult: TdxFcHitTest read FHitTestResult;
    property Images: TCustomImageList read FImages write SetImages;
    property ObjectAt: TdxFcObject read FObjectAt;
    property Options: TdxFcOptions read FOptions write SetOptions;
    property Painter: TdxFlowChartPainter read FPainter;
    property PaintScaleFactor: TdxScaleFactor read FPaintScaleFactor;
    property PressedItem: TdxFcItem read FPressedItem;
    property RoutingStrategy: TdxDiagramRoutingStrategy read FRoutingStrategy;
    property Selection: TdxFcSelection read FSelection;
    property Zoom: Word read FZoom write SetZoom default 100;

    property OnChange: TdxFcEvent read FOnChange write FOnChange;
    property OnCreateItem: TdxFcEvent read FOnCreateItem write FOnCreateItem;
    property OnDeletion: TdxFcEvent read FOnDeletion write FOnDeletion;
    property OnDrawObject: TdxFcDrawEvent read FOnDrawObject write FOnDrawObject;
    property OnDrawObjectEx: TdxFcDrawExEvent read FOnDrawObjectEx write FOnDrawObjectEx;
    property OnEdited: TdxFcEditEvent read FOnEdited write FOnEdited;
    property OnEditing: TdxFcAllowEvent read FOnEditing write FOnEditing;
    property OnSelected: TdxFcEvent read FOnSelected write FOnSelected;
    property OnSelection: TdxFcAllowEvent read FOnSelection write FOnSelection;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ApplyLayeredLayout; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function ChartPoint(X, Y: Integer): TPoint; overload;
    function ChartPoint(const P: TPoint): TPoint; overload;
    function CreateObject(L, T, W, H: Integer; AShape: TdxFcShapeType): TdxFcObject; overload;
    function CreateObject(L, T, W, H: Integer; AAdvancedShape: TdxFlowChartObjectAdvancedShape): TdxFcObject; overload;
    function CreateConnection(OSrc, ODst: TdxFcObject; PSrc, PDst: Byte): TdxFcConnection;
    function GetConnectionAt(X, Y: Integer): TdxFcConnection;
    function GetObjectAt(X, Y: Integer): TdxFcObject;
    function GetHitTestAt(X, Y: Integer): TdxFcHitTest;
    function SelCount: Integer;
    procedure BeginUpdate;
    procedure CancelUpdate;
    procedure EndUpdate;
    procedure Clear;
    procedure ClearSelection;
    procedure DeleteSelection;
    procedure DeleteConnection(AConnection: TdxFcConnection);
    procedure DeleteObject(AObject: TdxFcObject);
    procedure Invalidate; override;
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream); virtual;
    procedure SelectAll;
    procedure SetLeftTop(ALeft, ATop: Integer);

    property Antialiasing: Boolean read FAntialiasing write SetAntialiasing default True;
    property Canvas: TcxCanvas read GetCanvas;
    property Color default clDefault;
    property ConnectionCount: Integer read GetConnectionCount;
    property Connections[Index: Integer]: TdxFcConnection read GetConnection write SetConnection;
    property LeftEdge: Integer read FLeftEdge write SetLeftEdge;
    property ObjectCount: Integer read GetObjectCount;
    property Objects[Index: Integer]: TdxFcObject read GetObjectValue write SetObjectValue;
    property RealZoom: Word read FRealZoom;
    property SelectedConnection: TdxFcConnection read GetSelConnect write SetSelConnect;
    property SelectedConnectionCount: Integer read GetSelectedConnectionCount;
    property SelectedConnections[Index: Integer]: TdxFcConnection read GetSelectedConnection;
    property SelectedObject: TdxFcObject read GetSelObj write SetSelObj;
    property SelectedObjectCount: Integer read GetSelectedObjectCount;
    property SelectedObjects[Index: Integer]: TdxFcObject read GetSelectedObject;
    property TopEdge: Integer read FTopEdge write SetTopEdge;
    // repository
    class property Repository: TdxFlowChartAdvancedShapeRepository read FRepository;
  end;

  { TdxFlowChart }

  TdxFlowChart = class(TdxCustomFlowChart)
  published
    property Align;
    property Anchors;
    property Antialiasing;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property Enabled;
    property Font;
    property GridLineOptions;
    property Images;
    property LookAndFeel;
    property OnChange;
    property OnCreateItem;
    property OnDeletion;
    property OnDrawObject;
    property OnDrawObjectEx;
    property OnEdited;
    property OnEditing;
    property OnSelected;
    property OnSelection;
    property Options;
    property ParentColor default False;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property TabStop default True;
    property Visible;
    property Zoom;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnDragDrop;
    property OnDragOver;
    property OnStartDrag;
    property OnEndDrag;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
  end;

  { TdxFlowChartCustomCustomizeForm }

  TdxFlowChartCustomCustomizeForm = class(TdxForm, IdxFlowChartDesigner)
  public
    function CanKeyEnter(Edit: TcxTextEdit; Key : Char; MinValue, MaxValue : Integer) : Boolean;
    function Execute(AChart: TdxFlowChart): Boolean; virtual; abstract;
    function FindAllUnions(AChart : TdxFlowChart; FromUnion : TdxFcObject) : TdxFcObject;
    function FindUnions(AChart : TdxFlowChart; FromUnion, Obj : TdxFcObject) : TdxFcObject;
    function GetNumberByUnion(AChart : TdxFlowChart; Obj : TdxFcObject) : Integer;
    function IntegerToStr(S : String) : Integer;
    function IsChildItemInUnion(AChart : TdxFlowChart; Obj : TdxFcObject) : Boolean;
  end;

  { TdxFlowChartEditorOptions }

  TdxFlowChartEditorOptions = class
  strict private
    FUseAdvancedShapesOnly: Boolean;
  public
    class function CreateLegacyOptions: TdxFlowChartEditorOptions; static;
    class function CreateOptions: TdxFlowChartEditorOptions; static;

    property UseAdvancedShapesOnly: Boolean read FUseAdvancedShapesOnly write FUseAdvancedShapesOnly;
  end;

  IdxFlowChartEditorOptions = interface
  ['{2EE9E40F-105E-4685-B3E1-7791ACA0A34C}']
    procedure SetOptions(AOptions: TdxFlowChartEditorOptions);
  end;

  { TdxFlowChartCustomizeFormManager }

  TdxFlowChartCustomizeFormManager = class(TObject)
  private
    FList: TList;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function Customize(AChart: TdxFlowChart; AEditorCaption: string; AOptions: TdxFlowChartEditorOptions = nil): Boolean;
    function GetActualClass: TdxFormClass;
    procedure Register(AClass: TdxFormClass);
    procedure Unregister(AClass: TdxFormClass);
  end;

var
  dxFlowChartCustomizeFormManager: TdxFlowChartCustomizeFormManager;
  dxFlowChartEditor: TdxForm;

function ShowFlowChartEditor(AChart: TdxFlowChart; AEditorCaption: string; AOptions: TdxFlowChartEditorOptions = nil): Boolean;

const
  crFlChartZoomIn = 2001;
  crFlChartZoomOut = 2002;
  crFlChartCross = 2003;
  crFlChartDrawFreeConnector = 2004;
  crFlChartArrow = 2005;
  crFlChartCopy = 2006;
  crFlChartCreateRectangle = 2007;
  crFlChartDragRemove = 2008;
  crFlChartDrawConnectedConnector = 2009;
  crFlChartHandDragPreview = 2010;
  crFlChartHandPreview = 2011;
  crFlChartHoverRotation = 2012;
  crFlChartMove = 2013;
  crFlChartNo = 2014;
  crFlChartRotation = 2015;
  crFlChartSizeAll = 2016;
  crFlChartSizeNESW = 2017;
  crFlChartSizeNS = 2018;
  crFlChartSizeNWSE = 2019;
  crFlChartSizeWE = 2020;

procedure ExtSelRect(var R: TRect; ASelected: Boolean; AScaleFactor: TdxScaleFactor);

var
  dxFlowChartSizeStandardObjectsAsAdvanced: Boolean = True;

implementation

{$R 'dxFlowChartTemplates.res'}
{$R 'dxFlowChartImages.res'}
{$R 'dxFlowChartCursors.res'}

uses
  Math, cxDrawTextUtils, cxScrollBar, dxDPIAwareUtils, dxTypeHelpers, dxGenerics, dxRightAngleRoutingStrategy, AnsiStrings,
  dxStringHelper;

const
  StreamDescriptionANSI: AnsiString = 'VER1.0A';
  StreamDescriptionUNICODE: AnsiString = 'VER1.0U';
  StreamDescriptionXml: AnsiString = '<?xml v';
  CurrentXMLVersion = 1;

type
  TdxFlowChartObjectAdvancedShapeHelper = class helper for TdxFlowChartObjectAdvancedShape
  private
    function GetShapeDescription: TdxShapeDescription;
  public
    property ShapeDescription: TdxShapeDescription read GetShapeDescription;
  end;

  TXMLFormatHelper = record
    class function GetNameIndex(const AName: string; const ALowerCaseNames: array of string): Integer; static;
    class procedure WriteObjectShapeType(AAttributes: TdxXMLNodeAttributes; AShapeType: TdxFcShapeType); static;
    class procedure WriteAligment(AAttributes: TdxXMLNodeAttributes; const AName: TdxXMLString;
      AHorizontal: TdxFcHorzPos; AVertical: TdxFcVertPos); static;
    class procedure WriteColor(AAttributes: TdxXMLNodeAttributes; const AName: TdxXMLString; AColor: TColor); static;
    class procedure WriteAlphaColor(AAttributes: TdxXMLNodeAttributes; const AName: TdxXMLString; AColor: TdxAlphaColor); static;
    class procedure WriteConnectionStyle(AAttributes: TdxXMLNodeAttributes; AStyle: TdxFclStyle); static;

    class function ReadObjectShapeType(AAttributes: TdxXMLNodeAttributes): TdxFcShapeType; static;
    class procedure ReadAligment(AAttributes: TdxXMLNodeAttributes; const AName: TdxXMLString;
      out AHorizontal: TdxFcHorzPos; out AVertical: TdxFcVertPos); static;
    class function ReadColor(AAttributes: TdxXMLNodeAttributes; const AName: TdxXMLString; ADefaultColor: TColor = clDefault): TColor; static;
    class function ReadAlphaColor(AAttributes: TdxXMLNodeAttributes; const AName: TdxXMLString): TdxAlphaColor; static;
    class function ReadConnectionStyle(AAttributes: TdxXMLNodeAttributes): TdxFclStyle; static;
  end;

{ TdxFlowChartObjectAdvancedShapeHelper }

function TdxFlowChartObjectAdvancedShapeHelper.GetShapeDescription: TdxShapeDescription;
begin
  Result := FShape;
end;

{ TXMLFormatHelper }

class function TXMLFormatHelper.GetNameIndex(const AName: string; const ALowerCaseNames: array of string): Integer;
var
  S: string;
  I: Integer;
begin
  S := LowerCase(AName);
  for I := Low(ALowerCaseNames) to High(ALowerCaseNames) do
    if ALowerCaseNames[I] = S then
      Exit(I);
  Result := -1;
end;

class procedure TXMLFormatHelper.ReadAligment(AAttributes: TdxXMLNodeAttributes; const AName: TdxXMLString; out AHorizontal: TdxFcHorzPos;
  out AVertical: TdxFcVertPos);
type
  TAlignmentValue = record
    V: TdxFcVertPos;
    H: TdxFcHorzPos;
  end;
const
  AlignNames: array[0..8] of string = (
    'topleft',    'topcenter',    'topright',
    'centerleft', 'centercenter', 'centerright',
    'bottomleft', 'bottomcenter', 'bottomright');
  AlignValues: array[0..8] of TAlignmentValue = (
    (V: fcvpUp; H: fchpLeft), (V: fcvpUp; H: fchpCenter), (V: fcvpUp; H: fchpRight),
    (V: fcvpCenter; H: fchpLeft), (V: fcvpCenter; H: fchpCenter), (V: fcvpCenter; H: fchpRight),
    (V: fcvpDown; H: fchpLeft), (V: fcvpDown; H: fchpCenter), (V: fcvpDown; H: fchpRight));
var
  AValues: TAlignmentValue;
  AIndex: Integer;
begin
  AIndex := GetNameIndex(AAttributes.GetValueAsString(AName), AlignNames);
  case AIndex of
    1..8: AValues := AlignValues[AIndex];
  else
    AValues := AlignValues[0];
  end;
  AHorizontal := AValues.H;
  AVertical := AValues.V;
end;

class function TXMLFormatHelper.ReadAlphaColor(AAttributes: TdxXMLNodeAttributes; const AName: TdxXMLString): TdxAlphaColor;
begin
  Result := TdxAlphaColors.FromHexCode(AAttributes.GetValueAsString(AName));
end;

class function TXMLFormatHelper.ReadColor(AAttributes: TdxXMLNodeAttributes; const AName: TdxXMLString; ADefaultColor: TColor = clDefault): TColor;
var
  AValue: string;
begin
  AValue := AAttributes.GetValueAsString(AName);
  if AValue = '' then
    Result := ADefaultColor
  else
    Result := TdxAlphaColors.ToColor(TdxAlphaColors.FromHexCode(AValue));
end;

class function TXMLFormatHelper.ReadConnectionStyle(AAttributes: TdxXMLNodeAttributes): TdxFclStyle;
begin
  case GetNameIndex(AAttributes.GetValueAsString('Style'), ['straight', 'curved', 'recth', 'rectv']) of
    1: Result := fclCurved;
    2: Result := fclRectH;
    3: Result := fclRectV;
  else
    Result := fclStraight;
  end;
end;

class function TXMLFormatHelper.ReadObjectShapeType(AAttributes: TdxXMLNodeAttributes): TdxFcShapeType;
var
  AIndex: Integer;
begin
  AIndex := GetNameIndex(AAttributes.GetValueAsString('ShapeType'), [
    'none', 'rectangle', 'ellipse', 'roundrect', 'diamond', 'northtriangle', 'southtriangle',
    'easttriangle', 'westtriangle', 'hexagon', 'user', 'advanced']);
  if AIndex < 0 then
    AIndex := 1;
  Result := TdxFcShapeType(AIndex);
end;

class procedure TXMLFormatHelper.WriteAligment(AAttributes: TdxXMLNodeAttributes; const AName: TdxXMLString; AHorizontal: TdxFcHorzPos;
  AVertical: TdxFcVertPos);
const
  VerticalAlignments: array[TdxFcVertPos] of string = ('Top', 'Center', 'Bottom');
  HorizontalAlignments: array[TdxFcHorzPos] of string = ('Left', 'Center', 'Right');
begin
  AAttributes.SetValueAsString(AName, Format('%s%s', [VerticalAlignments[AVertical], HorizontalAlignments[AHorizontal]]));
end;

class procedure TXMLFormatHelper.WriteAlphaColor(AAttributes: TdxXMLNodeAttributes; const AName: TdxXMLString; AColor: TdxAlphaColor);
begin
  AAttributes.SetValueAsString(AName, TdxAlphaColors.ToHexCode(AColor, True, '#'));
end;

class procedure TXMLFormatHelper.WriteColor(AAttributes: TdxXMLNodeAttributes; const AName: TdxXMLString; AColor: TColor);
begin
  AAttributes.SetValueAsString(AName, TdxAlphaColors.ToHexCode(TdxAlphaColors.FromColor(AColor), True, '#'));
end;

class procedure TXMLFormatHelper.WriteConnectionStyle(AAttributes: TdxXMLNodeAttributes; AStyle: TdxFclStyle);
const
  Styles: array[TdxFclStyle] of TdxXMLString = (
    'Straight', 'Curved', 'RectH', 'RectV');
begin
  AAttributes.SetValue('Style', Styles[AStyle]);
end;

class procedure TXMLFormatHelper.WriteObjectShapeType(AAttributes: TdxXMLNodeAttributes; AShapeType: TdxFcShapeType);
const
  Styles: array[TdxFcShapeType] of TdxXMLString = (
    'None', 'Rectangle', 'Ellipse', 'RoundRect', 'Diamond', 'NorthTriangle', 'SouthTriangle',
    'EastTriangle', 'WestTriangle', 'Hexagon', 'User', 'Advanced');
begin
  AAttributes.SetValue('ShapeType', Styles[AShapeType]);
end;

//////////////////////////////////////////////////////////////////////////

procedure Swap(var A, B: Integer); inline;
var
  C: Integer;
begin
  C := A;
  A := B;
  B := C;
end;

procedure ExtSelRect(var R: TRect; ASelected: Boolean; AScaleFactor: TdxScaleFactor);
var
  ASelectionFrameWidth: Integer;
begin
  if ASelected then
  begin
    ASelectionFrameWidth := AScaleFactor.Apply(8);
    R.Inflate(ASelectionFrameWidth, ASelectionFrameWidth);
  end;
end;

function GetListItem(List: TList; Index: Integer): Pointer; inline;
begin
  if (Index >= 0) and (Index < List.Count) then
    Result := List.List[Index]
  else
    Result := nil;
end;

function ReadStr(Stream: TStream; AIsUnicode: Boolean): string;
var
  L: Word;
  SA: AnsiString;
  SW: WideString;
begin
  Stream.ReadBuffer(L, SizeOf(Word));
  if AIsUnicode then
  begin
    SetLength(SW, L);
    if L > 0 then Stream.ReadBuffer(SW[1], L * 2);
    Result := SW;
  end
  else
  begin
    SetLength(SA, L);
    if L > 0 then Stream.ReadBuffer(SA[1], L);
    Result := UTF8ToWideString(SA);
  end;
end;

procedure WriteStr(Stream: TStream; const S: string);
var
  L: Integer;
{$IFDEF STREAMANSIFORMAT}
  SA: AnsiString;
{$ENDIF}
begin
  L := Length(S);
  if L > $FFFF then L := $FFFF;
  Stream.WriteBuffer(L, SizeOf(Word));
  if L > 0 then
  begin
  {$IFDEF STREAMANSIFORMAT}
    SA := UTF8Encode(S);
    Stream.WriteBuffer(SA[1], L);
  {$ELSE}
    Stream.WriteBuffer(S[1], L * SizeOf(Char));
  {$ENDIF}
  end;
end;

function AdjustRect(var R: TRect; const Bounds: TRect; HPos: TdxFcHorzPos; VPos: TdxFcVertPos): Boolean;
var
  DX, DY: Integer;
begin
  DX := Bounds.Right - Bounds.Left + R.Left - R.Right;
  DY := Bounds.Bottom - Bounds.Top + R.Top - R.Bottom;
  Result := (DX >= 0) and (DY >= 0);
  if DX < 0 then Inc(R.Right, DX);
  if DY < 0 then Inc(R.Bottom, DY);
  if (DX < 0) or (HPos = fchpLeft) then DX := 0;
  if (DY < 0) or (VPos = fcvpUp) then DY := 0;
  if HPos = fchpCenter then DX := DX shr 1;
  if VPos = fcvpCenter then DY := DY shr 1;
  DX := DX + Bounds.Left - R.Left;
  DY := DY + Bounds.Top - R.Top;
  Inc(R.Left, DX); Inc(R.Right, DX);
  Inc(R.Top, DY); Inc(R.Bottom, DY);
end;

function AdjustImageRect(var R: TRect; const Bounds: TRect; HPos: TdxFcHorzPos; VPos: TdxFcVertPos): Boolean;
var
  DX, DY: Integer;
  ARatio: Single;
begin
  DX := Bounds.Width - R.Width;
  DY := Bounds.Height - R.Height;
  Result := (DX >= 0) and (DY >= 0);
  if not Result then
  begin
    if Bounds.IsEmpty then
      Exit;
    ARatio := Min(Bounds.Width / R.Width, Bounds.Height / R.Height);
    R.Width := Round(R.Width * ARatio);
    R.Height := Round(R.Height * ARatio);
    Result := True;
    DX := Bounds.Width - R.Width;
    DY := Bounds.Height - R.Height;
  end;
  if DX < 0 then
    Inc(R.Right, DX);
  if DY < 0 then
    Inc(R.Bottom, DY);
  if (DX < 0) or (HPos = fchpLeft) then
    DX := 0;
  if (DY < 0) or (VPos = fcvpUp) then
    DY := 0;
  if HPos = fchpCenter then
    DX := DX div 2;
  if VPos = fcvpCenter then
    DY := DY div 2;
  Inc(DX, Bounds.Left - R.Left);
  Inc(DY, Bounds.Top - R.Top);
  R.Offset(DX, DY);
end;

procedure UpdateGraphicsForRotatedBounds(AGraphics: TdxGPGraphics; const ABounds: TRect; AAngle: Single);
var
  ACenterX, ACenterY: Single;
begin
  if IsZero(AAngle) then
    Exit;
  ACenterX := ABounds.Left + ABounds.Width / 2;
  ACenterY := ABounds.Top + ABounds.Height / 2;
  AGraphics.TranslateWorldTransform(ACenterX, ACenterY);
  AGraphics.RotateWorldTransform(AAngle);
  AGraphics.TranslateWorldTransform(-ACenterX, -ACenterY);
end;

function ShowFlowChartEditor(AChart: TdxFlowChart; AEditorCaption: string; AOptions: TdxFlowChartEditorOptions = nil): Boolean;
begin
  Result := dxFlowChartCustomizeFormManager.Customize(AChart, AEditorCaption, AOptions);
end;

{ TdxFlowChartUnionsHelper }

class function TdxFlowChartUnionsHelper.FindAllUnions(AChart: TdxFlowChart; FromUnion: TdxFcObject): TdxFcObject;
var
  I: Integer;
  FFind: Boolean;
begin
  Result := nil;
  FFind := FromUnion = nil;
  for i := 0 to AChart.ObjectCount - 1 do
    if AChart.Objects[i].IsUnion then
    begin
      if FFind then
      begin
        Result := AChart.Objects[i];
        break;
      end;
      FFind := FromUnion = AChart.Objects[i];
    end;
end;

class function TdxFlowChartUnionsHelper.FindUnions(AChart: TdxFlowChart; FromUnion, Obj: TdxFcObject): TdxFcObject;
var
  I: integer;
  FFind : Boolean;
begin
  Result := nil;
  FFind := FromUnion = nil;
  for i := 0 to AChart.ObjectCount - 1 do
    if AChart.Objects[i].IsUnion and (Obj <> AChart.Objects[i]) then
      if AChart.Objects[i].HasInUnion(Obj) then
      begin
        if FFind then
        begin
          Result := AChart.Objects[i];
          break;
        end;
        FFind := AChart.Objects[i] = FromUnion;
      end;
end;

class function TdxFlowChartUnionsHelper.GetNumberByUnion(AChart: TdxFlowChart; Obj: TdxFcObject): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to AChart.ObjectCount - 1 do
    if AChart.Objects[I].IsUnion then
    begin
      Inc(Result);
      if AChart.Objects[I] = Obj then
        Break;
    end;
end;

class function TdxFlowChartUnionsHelper.IsChildItemInUnion(AChart: TdxFlowChart; Obj: TdxFcObject): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to AChart.ObjectCount - 1 do
    if AChart.Objects[I].IsUnion then
      if AChart.Objects[I].HasInUnion(Obj) then
      begin
        Result := True;
        Break;
      end;
end;

{ TdxFlowChartCustomCustomizeForm }

function TdxFlowChartCustomCustomizeForm.CanKeyEnter(Edit: TcxTextEdit; Key: Char; MinValue, MaxValue: Integer): Boolean;
var
  Text: string;
  Value: Integer;
begin
  if Key >= #32 then
  begin
    if Edit.SelLength = 0 then
      Text := Edit.Text + Key
    else
      Text := Copy(Edit.Text, 1, Edit.SelStart - 1) + Key +
        Copy(Edit.Text, Edit.SelStart + Edit.SelLength,
        Length(Edit.Text) - Edit.SelStart - Edit.SelLength + 1);
  end
  else
    Text := Edit.Text;

  try
    if Text = '' then
      Value := 1
    else
      Value := StrToInt(Text);

    Result := (Value >= MinValue) and (Value <= MaxValue);
  except
    Result := False;
  end;
end;

function TdxFlowChartCustomCustomizeForm.FindAllUnions(AChart: TdxFlowChart; FromUnion: TdxFcObject): TdxFcObject;
begin
  Result := TdxFlowChartUnionsHelper.FindAllUnions(AChart, FromUnion);
end;

function TdxFlowChartCustomCustomizeForm.FindUnions(AChart: TdxFlowChart; FromUnion, Obj: TdxFcObject): TdxFcObject;
begin
  Result := TdxFlowChartUnionsHelper.FindUnions(AChart, FromUnion, Obj);
end;

function TdxFlowChartCustomCustomizeForm.GetNumberByUnion(AChart: TdxFlowChart; Obj: TdxFcObject): Integer;
begin
  Result := TdxFlowChartUnionsHelper.GetNumberByUnion(AChart, Obj);
end;

function TdxFlowChartCustomCustomizeForm.IntegerToStr(S: String): Integer;
begin
  try
    if S = '' then
      Result := 1
    else
      Result := StrToInt(S);
  except
    Result := 1;
  end;
end;

function TdxFlowChartCustomCustomizeForm.IsChildItemInUnion(AChart: TdxFlowChart; Obj: TdxFcObject): Boolean;
begin
  Result := TdxFlowChartUnionsHelper.IsChildItemInUnion(AChart, Obj);
end;

{ TdxFlowChartCustomizeFormManager }

constructor TdxFlowChartCustomizeFormManager.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TdxFlowChartCustomizeFormManager.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

function TdxFlowChartCustomizeFormManager.GetActualClass: TdxFormClass;
begin
  if FList.Count > 0 then
    Result := TdxFormClass(FList.Last)
  else
    Result := nil;
end;

function TdxFlowChartCustomizeFormManager.Customize(AChart: TdxFlowChart; AEditorCaption: string;
  AOptions: TdxFlowChartEditorOptions = nil): Boolean;
var
  AIDesigner: IdxFlowChartDesigner;
  AIOptions: IdxFlowChartEditorOptions;
begin
  Result := FList.Count > 0;
  if Result then
  begin
    dxFlowChartEditor := GetActualClass.Create(nil);
    try
      if AEditorCaption = '' then
        dxFlowChartEditor.Caption := 'ExpressFlowChart Editor'
      else
        dxFlowChartEditor.Caption := AEditorCaption;

      Result := Supports(dxFlowChartEditor, IdxFlowChartDesigner, AIDesigner);
      if Result then
      begin
        if Supports(dxFlowChartEditor, IdxFlowChartEditorOptions, AIOptions) then
          AIOptions.SetOptions(AOptions);
        Result := AIDesigner.Execute(AChart);
      end;
    finally
      AIDesigner := nil;
      AIOptions := nil;
      dxFlowChartEditor.Free;
      AOptions.Free;
    end;
  end;
end;

procedure TdxFlowChartCustomizeFormManager.Register(AClass: TdxFormClass);
begin
  FList.Add(Pointer(AClass));
end;

procedure TdxFlowChartCustomizeFormManager.Unregister(AClass: TdxFormClass);
begin
  FList.Remove(Pointer(AClass));
end;


{ TdxFlowChartGDIPlusPainter }

procedure TdxFlowChartGDIPlusPainter.DrawEllipse(ACanvas: TcxCanvas; const R: TRect;
  ABackgroundColor, AFrameColor: TColor; AFrameStyle: TPenStyle; AFrameWidth: Integer);
var
  AGraphics: TdxGPGraphics;
  APen: TdxGPPen;
  ABrush: TdxGPBrush;
begin
  AGraphics := dxGpBeginPaint(ACanvas.Handle, R);
  try
    APen := TdxGPPen.Create(TdxAlphaColors.FromColor(AFrameColor), AFrameWidth, AFrameStyle);
    ABrush := TdxGPBrush.Create;
    ABrush.Color := TdxAlphaColors.FromColor(ABackgroundColor);
    try
      AGraphics.SmoothingMode := smAntiAlias;
      AGraphics.Ellipse(R, APen, ABrush);
    finally
      ABrush.Free;
      APen.Free;
    end;
  finally
    dxGpEndPaint(AGraphics);
  end;
end;

procedure TdxFlowChartGDIPlusPainter.DrawPolygon(ACanvas: TcxCanvas; const P: array of TPoint;
  ABackgroundColor, AFrameColor: TColor; AFrameStyle: TPenStyle; AFrameWidth: Integer);
var
  AGraphics: TdxGPGraphics;
  APen: TdxGPPen;
  ABrush: TdxGPBrush;
begin
  AGraphics := dxGpBeginPaint(ACanvas.Handle, cxPointsBox(P));
  try
    APen := TdxGPPen.Create(TdxAlphaColors.FromColor(AFrameColor), AFrameWidth, AFrameStyle);
    ABrush := TdxGPBrush.Create;
    ABrush.Color := TdxAlphaColors.FromColor(ABackgroundColor);
    try
      AGraphics.SmoothingMode := smAntiAlias;
      AGraphics.Polygon(P, APen, ABrush);
    finally
      ABrush.Free;
      APen.Free;
    end;
  finally
    dxGpEndPaint(AGraphics);
  end;
end;

procedure TdxFlowChartGDIPlusPainter.DrawPolyline(ACanvas: TcxCanvas;
  const P: array of TPoint; AColor: TColor; AStyle: TPenStyle; AWidth: Integer);
var
  AGraphics: TdxGPGraphics;
  APen: TdxGPPen;
begin
  if Length(P) > 1 then
  begin
    AGraphics := dxGpBeginPaint(ACanvas.Handle, cxPointsBox(P));
    try
      APen := TdxGPPen.Create(TdxAlphaColors.FromColor(AColor), AWidth, AStyle);
      try
        AGraphics.SmoothingMode := smAntiAlias;
        AGraphics.Polyline(P, APen);
      finally
        APen.Free;
      end;
    finally
      dxGpEndPaint(AGraphics);
    end;
  end;
end;

procedure TdxFlowChartGDIPlusPainter.DrawRectangle(ACanvas: TcxCanvas;
  const R: TRect; ABackgroundColor, AFrameColor: TColor; AFrameStyle: TPenStyle;
  AFrameWidth: Integer);
var
  AGraphics: TdxGPGraphics;
  APen: TdxGPPen;
  ABrush: TdxGPBrush;
begin
  AGraphics := dxGpBeginPaint(ACanvas.Handle, R);
  try
    APen := TdxGPPen.Create(TdxAlphaColors.FromColor(AFrameColor), AFrameWidth, AFrameStyle);
    ABrush := TdxGPBrush.Create;
    ABrush.Color := TdxAlphaColors.FromColor(ABackgroundColor);
    try
      AGraphics.SmoothingMode := smAntiAlias;
      AGraphics.Rectangle(R, APen, ABrush);
    finally
      ABrush.Free;
      APen.Free;
    end;
  finally
    dxGpEndPaint(AGraphics);
  end;
end;

procedure TdxFlowChartGDIPlusPainter.DrawRoundRect(ACanvas: TcxCanvas;
  const R: TRect; ABackgroundColor: TColor; AFrameColor: TColor;
  AFrameStyle: TPenStyle; AFrameWidth: Integer; ARadiusX, ARadiusY: Integer);
var
  AGraphics: TdxGPGraphics;
  APen: TdxGPPen;
  ABrush: TdxGPBrush;
begin
  AGraphics := dxGpBeginPaint(ACanvas.Handle, R);
  try
    APen := TdxGPPen.Create(TdxAlphaColors.FromColor(AFrameColor), AFrameWidth, AFrameStyle);
    ABrush := TdxGPBrush.Create;
    ABrush.Color := TdxAlphaColors.FromColor(ABackgroundColor);
    try
      AGraphics.SmoothingMode := smAntiAlias;
      AGraphics.RoundRect(R, APen, ABrush, ARadiusX div 2, ARadiusY div 2);
    finally
      ABrush.Free;
      APen.Free;
    end;
  finally
    dxGpEndPaint(AGraphics);
  end;
end;


{ TdxFlowChartPainter }

procedure TdxFlowChartPainter.DrawEdge(
  ACanvas: TcxCanvas; const R: TRect; AEdgeStyle, ABorderStyle: Cardinal);
var
  R1: TRect;
begin
  R1 := R;
  Windows.DrawEdge(ACanvas.Handle, R1, AEdgeStyle, ABorderStyle);
end;

procedure TdxFlowChartPainter.DrawEllipse(ACanvas: TcxCanvas; const R: TRect;
  ABackgroundColor: TColor; AFrameColor: TColor; AFrameStyle: TPenStyle;
  AFrameWidth: Integer);
begin
  ACanvas.SaveState;
  try
    PrepareBrush(ACanvas, ABackgroundColor);
    PreparePen(ACanvas, AFrameColor, AFrameStyle, AFrameWidth);
    ACanvas.Canvas.Ellipse(R);
  finally
    ACanvas.RestoreState;
  end;
end;

procedure TdxFlowChartPainter.DrawHorizontalLines(ACanvas: TcxCanvas; const AClientBounds: TRect; AEdgeSize, AGridSize: Integer;
  AIsMajorLines: Boolean; AScaleFactor: TdxScaleFactor);
var
  ATop, APaintTop, AIndex: Integer;
begin
  AIndex := AEdgeSize div AGridSize;
  ATop := AIndex * AGridSize - AEdgeSize;
  Dec(AIndex);
  repeat
    APaintTop := AScaleFactor.Apply(ATop);
    Inc(ATop, AGridSize);
    Inc(AIndex);
    if AIsMajorLines <> (AIndex mod 5 = 0) then
      Continue;
    ACanvas.MoveTo(AClientBounds.Left, APaintTop);
    ACanvas.LineTo(AClientBounds.Right, APaintTop);
  until APaintTop > AClientBounds.Bottom;
end;

procedure TdxFlowChartPainter.DrawVerticalLines(ACanvas: TcxCanvas; const AClientBounds: TRect; AEdgeSize,
  AGridSize: Integer; AIsMajorLines: Boolean; AScaleFactor: TdxScaleFactor);
var
  ALeft, APaintLeft, AIndex: Integer;
begin
  AIndex := AEdgeSize div AGridSize;
  ALeft := AIndex * AGridSize - AEdgeSize;
  Dec(AIndex);
  repeat
    APaintLeft := AScaleFactor.Apply(ALeft);
    Inc(ALeft, AGridSize);
    Inc(AIndex);
    if AIsMajorLines <> (AIndex mod 5 = 0) then
      Continue;
    ACanvas.MoveTo(APaintLeft, AClientBounds.Top);
    ACanvas.LineTo(APaintLeft, AClientBounds.Bottom);
  until APaintLeft > AClientBounds.Right;
end;

procedure TdxFlowChartPainter.DrawGrid(ACanvas: TcxCanvas; const AClientBounds: TRect; AGridOptions: TdxFlowChartGridOptions);
var
  AGridSize: Integer;
  AEdgeSizes: TSize;
  AScaleFactor: TdxScaleFactor;
begin
  AGridSize := AGridOptions.MinorLineStep;
  AEdgeSizes := AGridOptions.GetEdgeSizes;
  AScaleFactor := AGridOptions.GetScaleFactor;

  if AGridOptions.NeedPaintMinorLines then
  begin
    AGridOptions.MinorLines.SetupCanvas(ACanvas);
    DrawVerticalLines(ACanvas, AClientBounds, AEdgeSizes.cx, AGridSize, False, AScaleFactor);
    DrawHorizontalLines(ACanvas, AClientBounds, AEdgeSizes.cy, AGridSize, False, AScaleFactor);
  end;

  AGridOptions.MajorLines.SetupCanvas(ACanvas);
  DrawVerticalLines(ACanvas, AClientBounds, AEdgeSizes.cx, AGridSize, True, AScaleFactor);
  DrawHorizontalLines(ACanvas, AClientBounds, AEdgeSizes.cy, AGridSize, True, AScaleFactor);

  AGridOptions.OriginLines.SetupCanvas(ACanvas);
  DrawGridOrigin(ACanvas, AClientBounds, AGridOptions.GetOrigin);
end;

procedure TdxFlowChartPainter.DrawGridOrigin(ACanvas: TcxCanvas; const AClientBounds: TRect; const AOrigin: TPoint);
begin
  if InRange(AOrigin.Y, AClientBounds.Top, AClientBounds.Bottom) then
  begin
    ACanvas.MoveTo(AClientBounds.Left, AOrigin.Y);
    ACanvas.LineTo(AClientBounds.Right, AOrigin.Y);
  end;
  if InRange(AOrigin.X, AClientBounds.Left, AClientBounds.Right) then
  begin
    ACanvas.MoveTo(AOrigin.X, AClientBounds.Top);
    ACanvas.LineTo(AOrigin.X, AClientBounds.Bottom);
  end;
end;

procedure TdxFlowChartPainter.DrawPolygon(ACanvas: TcxCanvas;
  const P: array of TPoint; ABackgroundColor, AFrameColor: TColor;
  AFrameStyle: TPenStyle; AFrameWidth: Integer);
begin
  ACanvas.SaveState;
  try
    PrepareBrush(ACanvas, ABackgroundColor);
    PreparePen(ACanvas, AFrameColor, AFrameStyle, AFrameWidth);
    ACanvas.Polygon(P);
  finally
    ACanvas.RestoreState;
  end;
end;

procedure TdxFlowChartPainter.DrawPolyline(ACanvas: TcxCanvas;
  const P: array of TPoint; AColor: TColor; AStyle: TPenStyle; AWidth: Integer);
begin
  ACanvas.SaveState;
  try
    PreparePen(ACanvas, AColor, AStyle, AWidth);
    ACanvas.Polyline(P);
  finally
    ACanvas.RestoreState;
  end;
end;

procedure TdxFlowChartPainter.DrawRectangle(ACanvas: TcxCanvas;
  const R: TRect; ABackgroundColor: TColor; AFrameColor: TColor;
  AFrameStyle: TPenStyle; AFrameWidth: Integer);
begin
  ACanvas.SaveState;
  try
    PrepareBrush(ACanvas, ABackgroundColor);
    PreparePen(ACanvas, AFrameColor, AFrameStyle, AFrameWidth);
    ACanvas.Canvas.Rectangle(R);
  finally
    ACanvas.RestoreState;
  end;
end;

procedure TdxFlowChartPainter.DrawRegion(ACanvas: TcxCanvas;
  ARegionHandle: TcxRegionHandle; ABackgroundColor: TColor;
  AFrameColor: TColor; AFrameWidth: Integer);
begin
  if ARegionHandle <> 0 then
  begin
    ACanvas.SaveState;
    try
      ACanvas.DrawRegion(ARegionHandle, ABackgroundColor, AFrameColor, AFrameWidth, AFrameWidth);
    finally
      ACanvas.RestoreState;
    end;
  end;
end;

procedure TdxFlowChartPainter.DrawRoundRect(ACanvas: TcxCanvas;
  const R: TRect; ABackgroundColor: TColor; AFrameColor: TColor;
  AFrameStyle: TPenStyle; AFrameWidth, ARadiusX, ARadiusY: Integer);
begin
  ACanvas.SaveState;
  try
    PrepareBrush(ACanvas, ABackgroundColor);
    PreparePen(ACanvas, AFrameColor, AFrameStyle, AFrameWidth);
    ACanvas.Canvas.RoundRect(R.Left, R.Top, R.Right, R.Bottom, ARadiusX, ARadiusY);
  finally
    ACanvas.RestoreState;
  end;
end;

procedure TdxFlowChartPainter.PrepareBrush(ACanvas: TcxCanvas; AColor: TColor);
begin
  if AColor <> clNone then
    ACanvas.Brush.Color := AColor
  else
    ACanvas.Brush.Style := bsClear;
end;

procedure TdxFlowChartPainter.PreparePen(
  ACanvas: TcxCanvas; AColor: TColor; AStyle: TPenStyle; AWidth: Integer);
begin
  if AColor = clNone then
    ACanvas.Pen.Style := psClear
  else
  begin
    ACanvas.Pen.Style := AStyle;
    ACanvas.Pen.Width := AWidth;
    ACanvas.Pen.Color := AColor;
  end;
end;

procedure TdxFlowChartPainter.DrawPolyline(AGraphics: TdxGPGraphics; const P: array of TPoint; AColor: TColor;
  AStyle: TPenStyle; AWidth, AScale: Single);
var
  APen: TdxGPPen;
begin
  if Length(P) > 1 then
  begin
    APen := TdxGPPen.Create(TdxAlphaColors.FromColor(AColor), AWidth, AStyle, AScale);
    try
      AGraphics.Polyline(P, APen);
    finally
      APen.Free;
    end;
  end;
end;


{ TdxCustomFlowChart.TPointInfo }

constructor TdxCustomFlowChart.TPointInfo.Create(AObject: TdxFcObject; AIndex: Integer);
begin
  &Object := AObject;
  Index := AIndex;
end;

procedure TdxCustomFlowChart.TPointInfo.Update(AObject: TdxFcObject; AIndex: Integer);
begin
  &Object := AObject;
  Index := AIndex;
end;

{ TdxCustomFlowChart.TConnectionHighlights }

constructor TdxCustomFlowChart.TConnectionHighlights.Create(AChart: TdxCustomFlowChart; X, Y, ABaseTolerance: Integer);
begin
  FChart := AChart;
  FPointsTolerance := Chart.ScaleFactor.Apply(ABaseTolerance);
  FObjectsTolerance := Chart.ScaleFactor.Apply(ABaseTolerance div 3);
  FObjects := TList<TdxFcObject>.Create;
  Update(X, Y);
end;

destructor TdxCustomFlowChart.TConnectionHighlights.Destroy;
begin
  SetLinkedPointsVisibility(False);
  ClearObjectHighlights;
  FObjects.Free;
  FPointInfo.Free;
  inherited Destroy;
end;

function TdxCustomFlowChart.TConnectionHighlights.CanHighlightObject(AObject: TdxFcObject): Boolean;
begin
  Result := AObject.Visible and (AObject.LinkedPointCount > 0);
end;

function TdxCustomFlowChart.TConnectionHighlights.CanHighlightObjectPoints(AObject: TdxFcObject): Boolean;
begin
  Result := not Chart.AutoRouting and CanHighlightObject(AObject);
end;

procedure TdxCustomFlowChart.TConnectionHighlights.ClearObjectHighlights;
begin
  if FObject <> nil then
  begin
    FObject.HighlightedFrameMode := TdxFcObject.THighlightedFrameMode.None;
    FObject.HighlightedLinkedPointIndex := -1;
  end;
end;

function TdxCustomFlowChart.TConnectionHighlights.GetHighlightedObject(X, Y, ATolerance: Integer): TdxFcObject;
var
  I: Integer;
  AObject: TdxFcObject;
  APoint: TPoint;
begin
  if not MouseInChart then
    Exit(nil);
  Result := FChart.ObjectAt;
  if (Result <> nil) and CanHighlightObject(Result) then
    Exit;
  for I := Chart.ObjectCount - 1 downto 0 do
  begin
    AObject := Chart.Objects[I];
    APoint.Init(X, Y);
    AObject.ControlToObjectPoint(APoint);
    if CanHighlightObject(AObject) and AObject.ContainsPoint(APoint.X, APoint.Y, ATolerance) then
      Exit(AObject);
  end;
end;

function TdxCustomFlowChart.TConnectionHighlights.GetDestinationPoint(X, Y: Integer): TPoint;
begin
  if MouseInChart and (PointInfo <> nil) then
    Result := PointInfo.&Object.LinkedPoints[PointInfo.Index]
  else
    Result := Chart.ScrollPoint(TPoint.Create(X, Y));
end;

procedure TdxCustomFlowChart.TConnectionHighlights.SetLinkedPointsVisibility(AHighlight: Boolean);
var
  I: Integer;
  AObject: TdxFcObject;
begin
  if Chart.AutoRouting then
    Exit;

  for I := 0 to FObjects.Count - 1 do
  begin
    AObject := FObjects[I];
    AObject.ShowConnectionPoints := AHighlight;
  end;
end;

function TdxCustomFlowChart.TConnectionHighlights.GetSquareDistance(const AObject: TdxFcObject; ALinkedPointIndex: Integer;
  const APosition: TPoint): Int64;
begin
  Result :=
    Sqr(AObject.LinkedPoints[ALinkedPointIndex].X - APosition.X) +
    Sqr(AObject.LinkedPoints[ALinkedPointIndex].Y - APosition.Y);
end;

procedure TdxCustomFlowChart.TConnectionHighlights.Populate(X, Y, ATolerance: Integer);
var
  I, J: Integer;
  AObject: TdxFcObject;
  AMinSquareDistance, ASquareDistance: Int64;
  P: TPoint;
begin
  FObjects.Clear;
  if not MouseInChart then
    Exit;

  P := Chart.ScrollPoint(TPoint.Create(X, Y));
  AMinSquareDistance := Sqr(FObjectsTolerance);

  if (PointInfo <> nil) and (GetSquareDistance(PointInfo.&Object, PointInfo.Index, P) < AMinSquareDistance) then
  begin
    FObjects.Add(PointInfo.&Object);
    Exit;
  end;

  FreeAndNil(FPointInfo);
  for I := Chart.ObjectCount - 1 downto 0 do
  begin
    AObject := Chart.Objects[I];
    if CanHighlightObject(AObject) and AObject.ContainsPoint(X, Y, ATolerance) then
    begin
      FObjects.Add(AObject);
      if CanHighlightObjectPoints(AObject) then
      begin
        for J := 0 to AObject.LinkedPointCount - 1 do
        begin
          ASquareDistance := GetSquareDistance(AObject, J, P);
          if ASquareDistance < AMinSquareDistance then
          begin
            AMinSquareDistance := ASquareDistance;
            if FPointInfo = nil then
              FPointInfo := TPointInfo.Create(AObject, J)
            else
              FPointInfo.Update(AObject, J);
          end;
        end;
      end;
    end;
  end;
end;

procedure TdxCustomFlowChart.TConnectionHighlights.UpdateHighlightedObject(X, Y: Integer);
begin
  ClearObjectHighlights;
  if PointInfo <> nil then
  begin
    FObject := PointInfo.&Object;
    FObject.HighlightedLinkedPointIndex := PointInfo.Index;
  end
  else
  begin
    FObject := GetHighlightedObject(X, Y, FObjectsTolerance);
    if FObject <> nil then
      FObject.HighlightedFrameMode := TdxFcObject.THighlightedFrameMode.Connect;
  end;
end;

procedure TdxCustomFlowChart.TConnectionHighlights.Update(X, Y: Integer);
begin
  FMouseInChart := Chart.ClientBounds.Contains(X, Y);
  SetLinkedPointsVisibility(False);
  Populate(X, Y, FPointsTolerance);
  SetLinkedPointsVisibility(True);
  UpdateHighlightedObject(X, Y);
end;

{ TdxCustomFlowChart }

constructor TdxCustomFlowChart.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := clDefault;
  ControlStyle := ControlStyle - [csCaptureMouse];
  Width := 320;
  Height := 200;
  ParentColor := False;
  TabStop := True;
  FAntialiasing := True;
  FObjects := TList.Create;
  FConnections := TList.Create;
  FSelObjects := TList.Create;
  FSelConnections := TList.Create;
  FZoom := 100;
  FRealZoom := 100;
  FDragHelper := CreateDragHelper;
  FOptions := [fcoCanDelete, fcoCanDrag, fcoCanSelect, fcoMultiSelect, fcoHideSelection, fcoDelOnClick];
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := DoChangeLink;
  FPaintScaleFactor := TdxScaleFactor.Create;
  FPainter := CreatePainter;
  FGridLineOptions := TdxFlowChartGridOptions.Create(Self);
  FSelection := CreateSelection;
  DoubleBuffered := True;
  BorderStyle := bsSingle;
  Keys := [kAll, kArrows];
end;

class function TdxCustomFlowChart.CreateArrowShapeRepository: TdxFlowChartArrowShapeRepository;
begin
  Result := TdxFlowChartArrowShapeRepository.Create;
end;

destructor TdxCustomFlowChart.Destroy;
begin
  StopConnectionEndpointHighlight;
  OnDeletion := nil;
  OnSelected := nil;
  Clear;
  FreeAndNil(FObjects);
  FreeAndNil(FConnections);
  FreeAndNil(FSelObjects);
  FreeAndNil(FDragHelper);
  FreeAndNil(FSelConnections);
  FreeAndNil(FSelection);
  FreeAndNil(FChangeLink);
  FreeAndNil(FPainter);
  FreeAndNil(FGridLineOptions);
  FreeAndNil(FPaintScaleFactor);
  inherited Destroy;
end;

procedure TdxCustomFlowChart.ApplyLayeredLayout;
begin
  ShowHourglassCursor;
  try
    ApplySugiaymaLayout;
  finally
    HideHourglassCursor;
  end;
end;

function TdxCustomFlowChart.CreateGraph: TdxGraph;
var
  I: Integer;
  ANodes: TArray<TdxDiagramItem>;
  AEdges: TArray<TdxEdge<TdxDiagramItem>>;
  AConnection: TdxFcConnection;
begin
  SetLength(ANodes, ObjectCount);
  for I := 0 to ObjectCount - 1 do
    ANodes[I] := Objects[I];

  SetLength(AEdges, ConnectionCount);
  for I := 0 to ConnectionCount - 1 do
  begin
    AConnection := Connections[I];
    if (AConnection.ObjectSource = nil) or (AConnection.ObjectDest = nil) then
      Continue;
    AEdges[I] := TdxEdge<TdxDiagramItem>.Create(AConnection.ObjectSource, AConnection.ObjectDest);
  end;

  Result := TdxGraph.Create(ANodes, AEdges);
end;

function TdxCustomFlowChart.CreateGraphPageInfo: TdxPageInfo;
begin
  Result := TdxPageInfo.Create(TdxSizeDouble.Create(500, 500), TdxRectDouble.Null);
end;

function TdxCustomFlowChart.CreateSugiyamaLayoutSettings: TdxSugiyamaLayoutSettings;
begin
  Result := TdxSugiyamaLayoutSettings.Create(GridLineOptions.DefaultMinorLineStep * 3, GridLineOptions.DefaultMinorLineStep * 3, TopToBottom);
end;

procedure TdxCustomFlowChart.ApplySugiaymaPositionInfo(AInfo: TdxSugiyamaPositionsInfo; AUseDummyEdges: Boolean);
var
  I, J: Integer;
  AMinX, AMinY: Double;
  AObject: TdxFcObject;
  APositionInfo: TdxPositionInfo;
  APoints: TArray<TdxPointDouble>;
  APosition: TdxPointDouble;
  AConnection: TdxFcConnection;
  AEdge: TdxEdge<TdxDiagramItem>;
begin
  BeginUpdate;
  try
    AMinX := MaxInt;
    AMinY := MaxInt;
    for I := 0 to High(AInfo.PositionsInfo) do
    begin
      APositionInfo := AInfo.PositionsInfo[I];
      AMinX := Min(AMinX, APositionInfo.Position.X);
      AMinY := Min(AMinY, APositionInfo.Position.Y);
    end;
    AMinX := -AMinX + GridLineOptions.DefaultMinorLineStep * 2;
    AMinY := -AMinY + GridLineOptions.DefaultMinorLineStep * 2;
    for I := 0 to High(AInfo.PositionsInfo) do
    begin
      APositionInfo := AInfo.PositionsInfo[I];
      AObject := TdxFcObject(APositionInfo.Item);
      APosition := APositionInfo.Position;
      APosition.Offset(AMinX, AMinY);
      AObject.SetBounds(Round(APosition.X), Round(APosition.Y), AObject.Width, AObject.Height);
    end;
    if AUseDummyEdges then
    begin
      for I := 0 to ConnectionCount - 1 do
      begin
        AConnection := Connections[I];
        while AConnection.PointCount > 0 do
          AConnection.RemovePoint(0);
        AEdge := TdxEdge<TdxDiagramItem>.Create(AConnection.ObjectSource, AConnection.ObjectDest);
        APoints := AInfo.GetEdgesRoutes(AEdge);
        if Length(APoints) = 0 then
        begin
          APoints := AInfo.GetEdgesRoutes(AEdge.Reverse);
          if Length(APoints) > 0 then
            TArray.Reverse<TdxPointDouble>(APoints);
        end;
        for J := Low(APoints) to High(APoints) do
          AConnection.AddPoint(APoints[J]);
        AConnection.Style := fclCurved;
      end;
      OptimizeConnectionEndpoints;
    end
    else
    begin
      DoUpdateRouting;
      FNeedUpdateRouting := False;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TdxCustomFlowChart.CurveConnections;
  function GetQuadrantIndex(const ACenter, APoint: TPoint): Integer;
  begin
    if APoint.Y <= ACenter.Y then
      if APoint.X <= ACenter.X then
        Exit(0)
      else
        Exit(1)
   else
      if APoint.X <= ACenter.X then
        Exit(2)
      else
        Exit(3)
  end;

var
  AObjectBounds: TRect;
  APoints: TArray<TPoint>;
  P1, P2, ACenterOfGraph: TPoint;
  I, AQuadrantIndex1, AQuadrantIndex2: Integer;
  AConnection: TdxFcConnection;
begin
  SetLength(APoints, ObjectCount * 2);
  for I := 0 to ObjectCount - 1 do
  begin
    AObjectBounds := Objects[I].GetBoundingRect;
    APoints[I * 2] := AObjectBounds.TopLeft;
    APoints[I * 2 + 1] := AObjectBounds.BottomRight;
  end;
  ACenterOfGraph := TdxFcObject.GetBoundingRect(APoints, Length(APoints)).CenterPoint;
  for I := 0 to ConnectionCount - 1 do
  begin
    AConnection := Connections[I];
    if (AConnection.ObjectSource = nil) or (AConnection.ObjectDest = nil) then
      continue;
    while AConnection.PointCount > 0 do
      AConnection.RemovePoint(0);
    P1 := AConnection.ObjectSource.FBoundingRect.CenterPoint;
    P2 := AConnection.ObjectDest.FBoundingRect.CenterPoint;
    if (P1.X = P2.X) or (P1.Y = P2.Y) then
      continue;
    AQuadrantIndex1 := GetQuadrantIndex(ACenterOfGraph, P1);
    AQuadrantIndex2 := GetQuadrantIndex(ACenterOfGraph, P2);
    if AQuadrantIndex1 = AQuadrantIndex2 then
    begin
      case AQuadrantIndex1 of
        0: if (P1.X <= P2.X) <> (P1.Y <= P2.Y) then
             AConnection.AddPoint(TPoint.Create(Min(P1.X, P2.X), Min(P1.Y, P2.Y)));
        1: if (P1.X <= P2.X) = (P1.Y <= P2.Y) then
             AConnection.AddPoint(TPoint.Create(Max(P1.X, P2.X), Min(P1.Y, P2.Y)));
        2: if (P1.X <= P2.X) = (P1.Y <= P2.Y) then
             AConnection.AddPoint(TPoint.Create(Min(P1.X, P2.X), Max(P1.Y, P2.Y)));
        3: if (P1.X <= P2.X) <> (P1.Y <= P2.Y) then
             AConnection.AddPoint(TPoint.Create(Max(P1.X, P2.X), Max(P1.Y, P2.Y)));
      end;
    end;
    AConnection.Style := fclCurved;
  end;
end;

function TdxCustomFlowChart.UseDummyEdgesForRouting: Boolean;
begin
  Result := True;
end;

procedure TdxCustomFlowChart.ApplySugiaymaLayout;
var
  AGraph: TdxGraph;
  ASugiyamaLayoutSettings: TdxSugiyamaLayoutSettings;
  ASettings: TdxGraphSugiyamaLayoutSettings;
  AInfo: TdxSugiyamaPositionsInfo;
  AMargin: Integer;
begin
  AMargin := GridLineOptions.DefaultMinorLineStep;
  ASugiyamaLayoutSettings := CreateSugiyamaLayoutSettings;
  AGraph := CreateGraph;
  try
    ASettings := TdxGraphSugiyamaLayoutSettings.Create(ASugiyamaLayoutSettings, CreateGraphPageInfo);
    try
      AInfo := TdxSugiyamaLayout.LayoutGraph(AGraph,
        TdxSugiyamaLayoutAlgorithm.DefaultSugiyamaSteps(
          function (AItem: TdxDiagramItem): TdxSizeDouble
          begin
            Result.Init(
              TdxFcObject(AItem).Width + 2 * AMargin,
              TdxFcObject(AItem).Height + 2 * AMargin);
          end,
          ASettings));
      try
        ApplySugiaymaPositionInfo(AInfo, UseDummyEdgesForRouting);
      finally
        AInfo.Free;
      end;
    finally
      ASettings.Free;
    end;
  finally
    AGraph.Free;
  end;
end;

class constructor TdxCustomFlowChart.Initialize;
begin
  FRepository := TdxFlowChartAdvancedShapeRepository.Create;
end;

class destructor TdxCustomFlowChart.Finalize;
begin
  FRepository.Free;
end;

procedure TdxCustomFlowChart.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.WindowClass.Style := Params.WindowClass.Style and not (CS_HREDRAW or CS_VREDRAW);
  Params.Style := Params.Style or WS_TABSTOP;
end;

procedure TdxCustomFlowChart.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FImages) then Images := nil;
end;

function TdxCustomFlowChart.GetBackgroundColor: TColor;
begin
  Result := Color;
  if Result = clDefault then
    Result := LookAndFeelPainter.DefaultEditorBackgroundColor(False);
  if Result = clDefault then
    Result := clWindow;
end;

function TdxCustomFlowChart.GetBorderStyle: TBorderStyle;
const
  BorderStyleMap: array[TcxControlBorderStyle] of TBorderStyle = (bsNone, bsSingle);
begin
  Result := BorderStyleMap[inherited BorderStyle];
end;

function TdxCustomFlowChart.GetCanvas: TcxCanvas;
begin
  if FSelectionSpecialCanvas <> nil then
    Result := FSelectionSpecialCanvas
  else
    Result := inherited Canvas;
end;

function TdxCustomFlowChart.GetClientHeight: Integer;
begin
  Result := cxRectHeight(ClientBounds);
end;

function TdxCustomFlowChart.GetClientWidth: Integer;
begin
  Result := cxRectWidth(ClientBounds)
end;

function TdxCustomFlowChart.GetConnection(Index: Integer): TdxFcConnection;
begin
  Result := TdxFcConnection(GetListItem(FConnections, Index));
end;

function TdxCustomFlowChart.GetObjectValue(Index: Integer): TdxFcObject;
begin
  Result := TdxFcObject(GetListItem(FObjects, Index));
end;

function TdxCustomFlowChart.GetConnectionCount: Integer;
begin
  Result := FConnections.Count;
end;

function TdxCustomFlowChart.GetDisplayBounds(const ABounds: TRect): TRect;
begin
  Result.Left := MulDiv(ABounds.Left, RealZoom, 100);
  Result.Top := MulDiv(ABounds.Top, RealZoom, 100);
  Result.Width := MulDiv(ABounds.Width, RealZoom, 100);
  Result.Height := MulDiv(ABounds.Height, RealZoom, 100);
  Result := ScrollRect(Result, False);
end;

function TdxCustomFlowChart.GetItemBounds(const ADisplayBounds: TRect): TRect;
begin
  Result.TopLeft := ChartPoint(ADisplayBounds.TopLeft);
  Result.BottomRight := ChartPoint(ADisplayBounds.BottomRight);
end;

function TdxCustomFlowChart.GetObjectCount: Integer;
begin
  Result := FObjects.Count;
end;

function TdxCustomFlowChart.GetSelectedObject(Index: Integer): TdxFcObject;
begin
  Result := TdxFcObject(GetListItem(FSelObjects, Index));
end;

function TdxCustomFlowChart.GetSelectedObjectCount: Integer;
begin
  Result := FSelObjects.Count;
end;

function TdxCustomFlowChart.GetSelectedConnection(Index: Integer): TdxFcConnection;
begin
  Result := TdxFcConnection(GetListItem(FSelConnections, Index));
end;

function TdxCustomFlowChart.GetSelectedConnectionCount: Integer;
begin
  Result := FSelConnections.Count;
end;

function TdxCustomFlowChart.SelCount: Integer;
begin
  Result := FSelObjects.Count + FSelConnections.Count;
end;

procedure TdxCustomFlowChart.SetAntialiasing(AValue: Boolean);
var
  AOldPainter: TdxFlowChartPainter;
begin
  if AValue <> FAntialiasing then
  begin
    FAntialiasing := AValue;
    AOldPainter := FPainter;
    FPainter := CreatePainter;
    FreeAndNil(AOldPainter);
    Invalidate;
  end;
end;

procedure TdxCustomFlowChart.SetBorderStyle(Value: TBorderStyle);
const
  BorderStyleMap: array[TBorderStyle] of TcxControlBorderStyle = (cxcbsNone, cxcbsDefault);
begin
  inherited BorderStyle := BorderStyleMap[Value];
end;

procedure TdxCustomFlowChart.SetImages(Value: TCustomImageList);
begin
  if (FImages <> Value) then
  begin
    if (FImages <> nil) and not (csDestroying in FImages.ComponentState) then
      FImages.UnRegisterChanges(FChangeLink);
    FImages := Value;
    if (FImages <> nil) then
    begin
      FImages.RegisterChanges(FChangeLink);
      FImages.FreeNotification(Self);
    end;
    if not (csDestroying in ComponentState) then
      ImageListChanged;
  end;
end;

procedure TdxCustomFlowChart.SetOptions(Value: TdxFcOptions);
const
  Sel: TdxFcOptions = [fcoCanSelect, fcoMultiSelect];
var
  ANewOptions: TdxFcOptions;
  ANeedUpdateConnections, ANeedRepaintSelection: Boolean;
begin
  if (Options <> Value) then
  begin
    ANewOptions := (Options + Value) - (Options * Value);
    ANeedUpdateConnections := (fcoAutoRouteConnections in Options) <> (fcoAutoRouteConnections in Value);
    ANeedRepaintSelection := ([fcoCanDrag, fcoCanRotate] * Options) <> ([fcoCanDrag, fcoCanRotate] * Value);
    FOptions := Value;
    Selection.UpdateGraphicTools;
    if ANeedUpdateConnections then
      InvalidateRouting;
    if (ANewOptions * Sel <> []) and (Options * Sel <> Sel) then
      ClearSelection;
    if ANeedRepaintSelection then
      Invalidate
    else if (fcoHideSelection in ANewOptions) and not Focused then
      InvalidateSel;
  end;
end;

procedure TdxCustomFlowChart.SetConnection(Index: Integer; Value: TdxFcConnection);
begin
  Connections[Index].Assign(Value);
end;

procedure TdxCustomFlowChart.SetGridLineOptions(Value: TdxFlowChartGridOptions);
begin
  FGridLineOptions.Assign(Value);
end;

procedure TdxCustomFlowChart.SetHighlightedObject(AObject: TdxFcItem);
begin
  if FHighlightedObject <> AObject then
  begin
    if FHighlightedObject <> nil then
      FHighlightedObject.Highlighted := False;
    FHighlightedObject := AObject;
    if FHighlightedObject <> nil then
      FHighlightedObject.Highlighted := True;
  end;
end;

procedure TdxCustomFlowChart.SetObjectValue(Index: Integer; Value: TdxFcObject);
begin
  Objects[Index].Assign(Value);
end;

procedure TdxCustomFlowChart.SetZoom(AValue: Word);
begin
  if AValue <> Zoom then
  begin
    FZoom := AValue;
    CalculateRealZoomFactor;
  end;
end;

procedure TdxCustomFlowChart.AlignObjectsWithGrid;
var
  I: Integer;
begin
  if ObjectCount = 0 then
    Exit;
  BeginUpdate;
  try
   for I := 0 to ObjectCount - 1 do
     Objects[I].AlignWithGrid;
  finally
    EndUpdate;
  end;
end;

procedure TdxCustomFlowChart.UpdatePaintScaleFactor;
begin
  FPaintScaleFactor.Assign(ScaleFactor);
  FPaintScaleFactor.Change(RealZoom, 100);
end;

procedure TdxCustomFlowChart.UpdateRouting;
begin
  BeginUpdate;
  try
    DoUpdateRouting;
  finally
    FNeedUpdateRouting := False;
    EndUpdate;
  end;
end;

function TdxCustomFlowChart.ChartPoint(X, Y: Integer): TPoint;
begin
  Result.X := MulDiv(X + LeftEdge, 100, RealZoom);
  Result.Y := MulDiv(Y + TopEdge, 100, RealZoom);
end;

function TdxCustomFlowChart.ChartPoint(const P: TPoint): TPoint;
begin
  Result := ChartPoint(P.X, P.Y);
end;

function TdxCustomFlowChart.ChartPointToDisplayPoint(const P: TPoint): TPoint;
begin
  Result.X := MulDiv(P.X, RealZoom, 100);
  Result.Y := MulDiv(P.Y, RealZoom, 100);
  Result := ScrollPoint(Result, False);
end;

function TdxCustomFlowChart.ChartPointToDisplayPoint(const P: TdxPointDouble): TPoint;
begin
  Result.X := Round(P.X * RealZoom / 100);
  Result.Y := Round(P.Y * RealZoom / 100);
  Result := ScrollPoint(Result, False);
end;

procedure TdxCustomFlowChart.CalculateChartSizes;

  procedure DoExtendRect(R: TRect; var ANewR: TRect);
  begin
    R := ScrollRect(R);
    dxLines.ExtendRect(ANewR, R.TopLeft);
    dxLines.ExtendRect(ANewR, R.BottomRight);
  end;

var
  AOldR, ANewR: TRect;
  I: Integer;
  AObject: TdxFcObject;
begin
  if FUpdateChartSizeLocked then
    Exit;

  ANewR := cxNullRect;
  AOldR := cxRectBounds(FChartLeft, FChartTop, FChartWidth, FChartHeight);

  for I := 0 to ObjectCount - 1 do
  begin
    AObject := Objects[I];
    DoExtendRect(AObject.GetBoundsForRotatedRect(AObject.DisplayRect), ANewR);
  end;
  for I := 0 to ConnectionCount - 1 do
    DoExtendRect(Connections[I].DisplayRect, ANewR);

  if not cxRectIsEqual(AOldR, ANewR) then
  begin
    FChartTop := ANewR.Top;
    FChartLeft := ANewR.Left;
    FChartWidth := cxRectWidth(ANewR);
    FChartHeight := cxRectHeight(ANewR);
  end;
  UpdateScrollBars;
end;

procedure TdxCustomFlowChart.CalculateRealPos;
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to ObjectCount - 1 do
      Objects[I].ZoomChanged;
    for I := 0 to ConnectionCount - 1 do
      Connections[I].ZoomChanged;
    NeedRepaint;
  finally
    EndUpdate;
  end;
end;

function TdxCustomFlowChart.CanScaleByMouseWheel(AShift: TShiftState;
  const AMousePos: TPoint): Boolean;
begin
  Result := (Zoom > 0) and (ssCtrl in AShift) and not DragHelper.Dragging;
end;

procedure TdxCustomFlowChart.DoCancelMode;
begin
  if DragHelper.Dragging then
    DragHelper.CancelDrag;
  inherited DoCancelMode;
end;

procedure TdxCustomFlowChart.DoChangeLink(Sender: TObject);
begin
  ImageListChanged;
end;

function TdxCustomFlowChart.InternalCreateObject: TdxFcObject;
begin
  Result := TdxFcObject.Create(Self);
end;

function TdxCustomFlowChart.InternalCreateConnection: TdxFcConnection;
begin
  Result := TdxFcConnection.Create(Self);
end;

function TdxCustomFlowChart.NeedRedrawOnResize: Boolean;
begin
  Result := True;
end;

function TdxCustomFlowChart.NeedAlignObjectBoundsWithGrid: Boolean;
begin
  Result := fcoAlignWithGrid in Options;
end;

procedure TdxCustomFlowChart.ScalePoint(var P: TPoint);
begin
  P.X := MulDiv(P.X, RealZoom, 100);
  P.Y := MulDiv(P.Y, RealZoom, 100);
end;

function TdxCustomFlowChart.ScrollPoint(
  const P: TPoint; APositiveFactor: Boolean = True): TPoint;
begin
  Result := cxPointOffset(P, LeftEdge, TopEdge, APositiveFactor);
end;

function TdxCustomFlowChart.ScrollRect(
  const R: TRect; APositiveFactor: Boolean = True): TRect;
begin
  Result := cxRectOffset(R, LeftEdge, TopEdge, APositiveFactor);
end;

procedure TdxCustomFlowChart.UnscalePoint(var P: TPoint);
begin
  P.X := MulDiv(P.X, 100, RealZoom);
  P.Y := MulDiv(P.Y, 100, RealZoom);
end;

function TdxCustomFlowChart.UseSnapToGuides: Boolean;
begin
  Result := fcoSnapToGuides in Options;
end;

function TdxCustomFlowChart.CreateObject(L, T, W, H: Integer; AShape: TdxFcShapeType): TdxFcObject;
begin
  Result := InternalCreateObject;
  if Assigned(OnCreateItem) then OnCreateItem(Self, Result);
  Result.FShapeType := AShape;
  Result.SetBounds(L, T, W, H);
end;

function TdxCustomFlowChart.CreateObject(L, T, W, H: Integer; AAdvancedShape: TdxFlowChartObjectAdvancedShape): TdxFcObject;
begin
  Result := InternalCreateObject;
  if Assigned(OnCreateItem) then OnCreateItem(Self, Result);
  Result.FAdvancedShape := AAdvancedShape;
  Result.FShapeType := fcsAdvanced;
  Result.CreateAdvancedShapeInfo;
  Result.SetBounds(L, T, W, H);
end;

function TdxCustomFlowChart.CreateConnection(OSrc, ODst: TdxFcObject; PSrc, PDst: Byte): TdxFcConnection;
begin
  Result := InternalCreateConnection;
  if Assigned(OnCreateItem) then OnCreateItem(Self, Result);
  with Result do
  begin
    SetObjectSource(OSrc, PSrc);
    SetObjectDest(ODst, PDst);
  end;
end;

function TdxCustomFlowChart.CreateDragHelper: TdxFlowChartDragHelper;
begin
  Result := TdxFlowChartDragHelper.Create(Self);
end;

procedure TdxCustomFlowChart.DeleteObject(AObject: TdxFcObject);
begin
  AObject.Free;
end;

procedure TdxCustomFlowChart.DeleteConnection(AConnection: TdxFcConnection);
begin
  AConnection.Free;
end;

procedure TdxCustomFlowChart.Clear;
begin
  if ObjectCount + ConnectionCount = 0 then Exit;
  BeginUpdate;
  FSelConnections.Clear;
  FSelObjects.Clear;
  while ConnectionCount > 0 do
    Connections[0].Free;
  while ObjectCount > 0 do
    Objects[0].Free;
  if IsDestroying then
    CancelUpdate
  else
    EndUpdate;
end;

procedure TdxCustomFlowChart.ClearSelection;
begin
  while SelectedConnectionCount > 0 do
    SelectedConnections[0].Selected := False;
  while SelectedObjectCount > 0 do
    SelectedObjects[0].Selected := False;
end;

procedure TdxCustomFlowChart.DeleteSelection;
begin
  while SelectedConnectionCount > 0 do
    SelectedConnections[0].Free;
  while SelectedObjectCount > 0 do
    SelectedObjects[0].Free;
end;

procedure TdxCustomFlowChart.SelectAll;
var
  I: Integer;
begin
  for I := 0 to ObjectCount - 1 do
    Objects[I].Selected := True;
  for I := 0 to ConnectionCount - 1 do
    Connections[I].Selected := True;
end;

function TdxCustomFlowChart.GetSelConnect: TdxFcConnection;
begin
  if (SelectedObjectCount = 0) and (SelectedConnectionCount = 1) then
    Result := SelectedConnections[0]
  else
    Result := nil;
end;

function TdxCustomFlowChart.GetSelObj: TdxFcObject;
begin
  if (SelectedObjectCount = 1) and (SelectedConnectionCount = 0) then
    Result := SelectedObjects[0]
  else
    Result := nil;
end;

procedure TdxCustomFlowChart.SetSelConnect(Value: TdxFcConnection);
begin
  if Value <> SelectedConnection then
  begin
    ClearSelection;
    if Value <> nil then
      Value.Selected := True;
  end;
end;

procedure TdxCustomFlowChart.SetSelObj(Value: TdxFcObject);
begin
  if Value <> SelectedObject then
  begin
    ClearSelection;
    if Value <> nil then
      Value.Selected := True;
  end;
end;

function TdxCustomFlowChart.CanPaint: Boolean;
begin
  Result := (FLockUpdates = 0) and HandleAllocated;
end;

procedure TdxCustomFlowChart.NeedRepaint;
begin
  FRepaint := True;
  if CanPaint then
    Invalidate;
end;

procedure TdxCustomFlowChart.NeedRepaintObject(AItem: TdxFcItem);
begin
  FRepaint := True;
  if CanPaint then
    InvalidateRect(AItem.InvalidateRect, True);
end;

procedure TdxCustomFlowChart.BeginUpdate;
begin
  if FLockUpdates = 0 then
    FRepaint := False;
  Inc(FLockUpdates);
end;

procedure TdxCustomFlowChart.CancelUpdate;
begin
  if FLockUpdates <> 0 then
    Dec(FLockUpdates);
end;

procedure TdxCustomFlowChart.EndUpdate;
begin
  if FLockUpdates > 0 then
  begin
    Dec(FLockUpdates);
    if FLockUpdates = 0 then
    begin
      if FRepaint then
        NeedRepaint;
      if FNeedUpdateRouting and AutoRouting then
        UpdateRouting;
      if FHasContentChanges then
        ContentChanged;
      SetChartSizes;
    end;
  end;
end;

procedure TdxCustomFlowChart.Invalidate;
begin
  if FLockUpdates > 0 then
    FRepaint := True
  else
    inherited Invalidate;
end;

procedure TdxCustomFlowChart.InvalidateSel;
var
  I: Integer;
begin
  for I := 0 to SelectedObjectCount - 1 do
    NeedRepaintObject(SelectedObjects[I]);
  for I := 0 to SelectedConnectionCount - 1 do
    SelectedConnections[I].ConnectionChanged;
end;

function TdxCustomFlowChart.HasSelection: Boolean;
begin
  Result := Focused or not (fcoHideSelection in Options);
end;

function TdxCustomFlowChart.HasMultiSelection: Boolean;
begin
  Result := SelectedConnectionCount + SelectedObjectCount > 1;
end;

procedure TdxCustomFlowChart.DoPaint;
var
  I: Integer;
  AObject: TdxFcObject;
begin
  if DragHelper.Dragging and DragHelper.SpecialControlPainting then
  begin
    DragHelper.PaintControl(Canvas);
    Exit;
  end;

  inherited DoPaint;
  PaintBackground;

  PrepareSelection;

  PaintConnections(False);
  for I := 0 to ObjectCount - 1 do
  begin
    AObject := Objects[I];
    if RectVisible(Canvas.Handle, AObject.InvalidateRect) then
      AObject.Paint(Canvas);
  end;
  PaintConnections(True);

  PaintSelection;

  for I := 0 to ObjectCount - 1 do
  begin
    AObject := Objects[I];
    if RectVisible(Canvas.Handle, AObject.InvalidateRect) then
      AObject.PaintHighlights(Canvas);
  end;
end;

procedure TdxCustomFlowChart.DoUpdateRouting;
var
  I: Integer;
begin
  FRoutingStrategy := CreateRoutingStrategy;
  try
    for I := 0 to FConnections.Count - 1 do
      TdxFcConnection(FConnections.List[I]).ReRoute;
  finally
    FreeAndNil(FRoutingStrategy);
  end;
end;

procedure TdxCustomFlowChart.PaintBackground;
begin
  Canvas.FillRect(ClientBounds, BackgroundColor);
  if GridLineOptions.ShowLines then
    Painter.DrawGrid(Canvas, ClientBounds, GridLineOptions);
end;

procedure TdxCustomFlowChart.PaintConnections(AUpper: Boolean);
var
  AConnection: TdxFcConnection;
  I: Integer;
begin
  for I := 0 to ConnectionCount - 1 do
  begin
    AConnection := Connections[I];
    if RectVisible(Canvas.Handle, AConnection.SelectionRect) then
      AConnection.Paint(Canvas, AUpper);
  end;
end;

procedure TdxCustomFlowChart.PaintSelection;
begin
  if HasSelection then
    FSelection.Paint;
end;

procedure TdxCustomFlowChart.PrepareSelection;
begin
  FSelection.Prepare;
end;

procedure TdxCustomFlowChart.DoDrawObject(AObject: TdxFcObject; R: TRect);
begin
  if Assigned(OnDrawObject) then
    OnDrawObject(Self, AObject, R)
  else
    DefaultDrawObject(AObject, R);
end;

procedure TdxCustomFlowChart.DoDrawObjectEx(AObject: TdxFcObject; AGraphics: TdxGPGraphics; const APaintData: TdxFcDrawExEventPaintData);
begin
  if HasDrawObjectEx then
    OnDrawObjectEx(Self, AObject, AGraphics, APaintData);
end;

function TdxCustomFlowChart.GetScrollContentForegroundColor: TColor;
begin
  Result := dxInvertColor(BackgroundColor);
end;

function TdxCustomFlowChart.GetScrollLineSize: Integer;
begin
  Result := GridLineOptions.DefaultMinorLineStep;
end;

function TdxCustomFlowChart.HasDrawObjectEx: Boolean;
begin
  Result := Assigned(FOnDrawObjectEx);
end;

procedure TdxCustomFlowChart.DefaultDrawObject(AObject: TdxFcObject; R: TRect);
begin
  if AObject.HasImage then
    AObject.PaintImage(R);
  if AObject.Text <> '' then
    AObject.PaintText(R);
end;

procedure TdxCustomFlowChart.InitScrollBarsParameters;

  function DoSetScrollBarInfo(AKind: TScrollBarKind; APage, AMin, ASize: Integer): Integer;
  var
    AScrollBar: IcxControlScrollBar;
  begin
    AScrollBar := GetScrollBar(AKind);
    if AScrollBar <> nil then
      Result := AScrollBar.Position
    else
      Result := 0;

    if Zoom = 0 then
      APage := ASize + 1;

    SetScrollBarInfo(AKind, AMin, AMin + ASize, 1, APage, Result, True, True);
  end;

var
  ANewX, ANewY: Integer;
begin
  if HandleAllocated then
  begin
    ANewX := DoSetScrollBarInfo(sbHorizontal, ClientWidth, FChartLeft, FChartWidth);
    ANewY := DoSetScrollBarInfo(sbVertical, ClientHeight, FChartTop, FChartHeight);
    SetLeftTop(ANewX, ANewY);
  end;
end;

function TdxCustomFlowChart.InternalMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  if DragHelper.Dragging then
    Exit(True);
  Result := CanScaleByMouseWheel(Shift, MousePos);
  if not Result then
    Result := inherited InternalMouseWheel(Shift, WheelDelta, MousePos)
  else
    if WheelDelta > 0 then
      Zoom := Zoom + 10
    else
      if Zoom > 10 then
        Zoom := Zoom - 10;
end;

procedure TdxCustomFlowChart.InvalidateRouting;
begin
  if not AutoRouting then
    Exit;
  if IsLocked or IsLoading then
    FNeedUpdateRouting := True
  else
    UpdateRouting;
end;

function TdxCustomFlowChart.IsLocked: Boolean;
begin
  Result := FLockUpdates > 0;
end;

function TdxCustomFlowChart.IsMouseWheelHandleNeeded(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result := inherited IsMouseWheelHandleNeeded(Shift, WheelDelta, MousePos) or
    CanScaleByMouseWheel(Shift, MousePos);
end;

procedure TdxCustomFlowChart.SetLeftTop(ALeft, ATop: Integer);

  procedure Adjust(AMin, AMax: Integer; var AValue: Integer);
  begin
    AValue := Max(Min(AValue, AMax), AMin);
  end;

begin
  Adjust(FChartLeft, FChartLeft + FChartWidth - ClientWidth, ALeft);
  Adjust(FChartTop, FChartTop + FChartHeight - ClientHeight, ATop);
  if (ALeft <> LeftEdge) or (ATop <> TopEdge) then
  begin
    FTopEdge := ATop;
    FLeftEdge := ALeft;
    if HandleAllocated then
    begin
      if HScrollBar <> nil then
        HScrollBar.Position := ALeft;
      if VScrollBar <> nil then
        VScrollBar.Position := ATop;
      ShowTouchScrollUI(Self, True);
      if DragHelper.Dragging and DragHelper.SpecialControlPainting then
        DragHelper.UpdateControlImage;
      Repaint;
    end;
  end;
end;

procedure TdxCustomFlowChart.ProcessMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (htOnObject in FHitTestResult) and FObjectAt.Selected and not (ssShift in Shift) then
    FObjectAt.SelectUnion
  else
    if FHitTestResult * [htOnObject, htOnConnection, htOnConLabel, htOnArrowSrc, htOnArrowDst] <> [] then
    begin
      if not (ssShift in Shift) then
      begin
        ClearSelection;
        HitTest(X, Y);
      end;
      if htOnObject in FHitTestResult then
        FObjectAt.Selected := not FObjectAt.Selected
      else
        FConnectionAt.Selected := not FConnectionAt.Selected;
    end
    else
      ClearSelection;
  if htOnObject in FHitTestResult then
  begin
    FPressedItem := FObjectAt;
    FObjectAt.MouseDown(Button, Shift, X, Y);
  end;
end;

procedure TdxCustomFlowChart.SetChartSizes;
begin
  if (FLockUpdates = 0) and not IsDestroying then
    CalculateChartSizes;
end;

procedure TdxCustomFlowChart.SetLeftEdge(Value: Integer);
begin
  if Value <> LeftEdge then
    SetLeftTop(Value, TopEdge);
end;

procedure TdxCustomFlowChart.SetTopEdge(Value: Integer);
begin
  if Value <> TopEdge then
    SetLeftTop(LeftEdge, Value);
end;

procedure TdxCustomFlowChart.HitTest(X, Y: Integer);
const
  ConnectionHitTest: TdxFcHitTest = [htOnConnection, htOnConLabel, htOnArrowSrc, htOnArrowDst];

  function IsAtPoint(const ACursorPoint, P: TPoint): Boolean;
  var
    ASquareDistance: Single;
  begin
    ASquareDistance := Sqr(Selection.ResizingMarkRadius);
    Result := (Sqr(ACursorPoint.X - P.X) + Sqr(ACursorPoint.Y - P.Y)) < ASquareDistance;
    if Result then
      Include(FHitTestResult, htOnSelPoint);
  end;

  function IsPointInItem(Item: TdxFcItem; const P: TPoint): Boolean;
  var
    AObjectPoint: TPoint;
  begin
    AObjectPoint := ScrollPoint(P, False);
    if Item is TdxFcObject then
      AObjectPoint := TdxFcObject(Item).ControlToObjectPoint(P);
    Result := (Item <> nil) and PtInRect(Item.SelectionRect, AObjectPoint);
  end;

  function IsPointInSelectedObject(const P: TPoint): Boolean;
  begin
    Result := (SelectedObject <> nil) and
      SelectedObject.PointOnSelectionFrame(P);
  end;

  function IsPointInSelectedObjectRotationMark(const P: TPoint): Boolean;
  begin
    Result := (SelectedObject <> nil) and
      SelectedObject.PointOnRotationMark(P);
  end;

  function IsPointInSelectedObjectParameterMark(const P: TPoint; out AParameterIndex: Integer): Boolean;
  begin
    if SelectedObject <> nil then
      AParameterIndex := SelectedObject.GetParameterIndex(P)
    else
      AParameterIndex := -1;
    Result := AParameterIndex >= 0;
  end;

  procedure CalculateObjectsHitTest(const P: TPoint);
  var
    AHitTest: TdxFcHitTest;
    I: Integer;
  begin
    FObjectAt := nil;
    for I := ObjectCount - 1 downto 0 do
    begin
      AHitTest := Objects[I].HitTest(P);
      if htByObject in AHitTest then
      begin
        if FHitTestResult = [htNowhere] then
        begin
          FHitTestResult := [htByObject];
          FObjectAt := Objects[I];
        end;
        if htOnObject in AHitTest then
        begin
          FHitTestResult := [htByObject, htOnObject];
          FObjectAt := Objects[I];
          Break;
        end;
      end;
    end;
  end;

  procedure CalculateConnectionsHitTest(const P: TPoint);

    function DoArrowHitTest(AArrow: TdxFcConnectionArrow): Boolean;
    begin
      if AArrow.ArrowType = fcaNone then
        Result := False
      else
        Result := PtInRect(AArrow.DisplayRect(False), ScrollPoint(P));
    end;

  var
    I: Integer;
  begin
    FConnectionAt := nil;
    for I := ConnectionCount - 1 downto 0 do
    begin
      if PtInRect(Connections[I].SelectionRect, P) then
      begin
        FConnectionAt := Connections[I];
        if FHitTestResult = [htNowhere] then FHitTestResult := [];
        if FConnectionAt.HasPoint(ScrollPoint(P)) then
          Include(FHitTestResult, htOnConnection);
        if DoArrowHitTest(FConnectionAt.ArrowSource) then
          Include(FHitTestResult, htOnArrowSrc);
        if DoArrowHitTest(FConnectionAt.ArrowDest) then
          Include(FHitTestResult, htOnArrowDst);
        if FConnectionAt.Text <> '' then
        begin
          if PtInRect(FConnectionAt.RealTextRect, P) then
            Include(FHitTestResult, htOnConLabel);
        end;
        if FHitTestResult * ConnectionHitTest <> [] then
          Break;
      end;
    end;
  end;

var
  ASelection: TdxFcSelectionPoints;
  I, AStartPointIndex, AEndPointIndex, AParameterIndex: Integer;
  P: TPoint;
begin
  if (FHitTestResult <> []) and (X = FHitX) and (Y = FHitY) then Exit;

  FHitParameterIndex := -1;
  FHitX := X;
  FHitY := Y;
  if not ClientBounds.Contains(X, Y) then
  begin
    FHitTestResult := [];
    Exit;
  end;

  FHitTestResult := [htNowhere];

  P := Point(X, Y);
  CalculateObjectsHitTest(P);
  CalculateConnectionsHitTest(P);

  if (htOnObject in FHitTestResult) and (FHitTestResult * ConnectionHitTest <> []) then
  begin
    if FConnectionAt.Transparent then
      FHitTestResult := FHitTestResult - ConnectionHitTest
    else
      Exclude(FHitTestResult, htOnObject);
  end;

  if (fcoCanRotate in Options) and IsPointInSelectedObjectRotationMark(P) then
  begin
    FObjectAt := SelectedObject;
    Include(FHitTestResult, htOnRotateMark);
    Exit;
  end;

  if (fcoUseShapeParameters in Options) and IsPointInSelectedObjectParameterMark(P, AParameterIndex) then
  begin
    FObjectAt := SelectedObject;
    FHitParameterIndex := AParameterIndex;
    Include(FHitTestResult, htOnParameter);
    Exit;
  end;

  if (fcoCanDrag in Options) and IsPointInSelectedObject(P) then
  begin
    P := ScrollPoint(P);
    SelectedObject.SelPoints(ASelection);
    ValidateObjectsResizing(ASelection);
    for I := 0 to 7 do
      if IsAtPoint(P, ASelection[I]) then
      begin
        if FObjectAt <> SelectedObject then
          Exclude(FHitTestResult, htOnObject);
        FObjectAt := SelectedObject;
        Exit;
      end;
  end
  else
    P := ScrollPoint(P);

  if CanDragConnectionEndpoints and IsPointInItem(SelectedConnection, P) then
  begin
    AStartPointIndex := 0;
    AEndPointIndex := SelectedConnection.RealCount - 1;
    if not CanDetachConnections then
    begin
      if SelectedConnection.ObjectSource <> nil then
        Inc(AStartPointIndex);
      if SelectedConnection.ObjectDest <> nil then
        Dec(AEndPointIndex);
    end;
    for I := AStartPointIndex to AEndPointIndex do
    begin
      if (I > AStartPointIndex) and (I < AEndPointIndex) and not CanDragConnectionIntermediatePoints then
         Continue;
      if IsAtPoint(P, SelectedConnection.FRealPoints[I]) then
      begin
        if FConnectionAt <> SelectedConnection then
          FHitTestResult := FHitTestResult - ConnectionHitTest;
        Include(FHitTestResult, htOnConnection);
        FConnectionAt := SelectedConnection;
      end;
    end;
  end;

  if FHitTestResult = [] then
  begin
    FHitTestResult := [htNowhere];
    FConnectionAt := nil;
  end;
end;

function TdxCustomFlowChart.GetConnectionAt(X, Y: Integer): TdxFcConnection;
begin
  HitTest(X, Y);
  Result := FConnectionAt;
end;

function TdxCustomFlowChart.GetObjectAt(X, Y: Integer): TdxFcObject;
begin
  HitTest(X, Y);
  Result := FObjectAt;
end;

function TdxCustomFlowChart.GetHitTestAt(X, Y: Integer): TdxFcHitTest;
begin
  HitTest(X, Y);
  Result := FHitTestResult;
end;

function TdxCustomFlowChart.TmpSel: Integer;
var
  I: Integer;
  Opt: TdxFcOptions;
  Sel1: TdxFcEvent;
  Sel2: TdxFcAllowEvent;
begin
  Result := SelectedObjectCount;
  if Result = 0 then Exit;
  Opt := Options; FOptions := Opt + [fcoCanSelect, fcoMultiSelect];
  Sel1 := OnSelected; OnSelected := nil;
  Sel2 := OnSelection; OnSelection := nil;
  Inc(FLockUpdates);
  for I := 0 to Result - 1 do
    SelectedObjects[I].SelectUnion;
  Dec(FLockUpdates); FOptions := Opt;
  OnSelected := Sel1; OnSelection := Sel2;
end;

procedure TdxCustomFlowChart.RestoreSel(Value: Integer);
var
  I: Integer;
begin
  for I := Value to SelectedObjectCount - 1 do
    SelectedObjects[I].SetSelectedState(False);
  FSelObjects.Count := Value;
end;

procedure TdxCustomFlowChart.OffsetObjects(DX, DY: Integer);
var
  I, EndSel: Integer;
  P: TPoint;
  AObject: TdxFcObject;
  AConnection: TdxFcConnection;
begin
  if (DX or DY = 0) or (SelectedObjectCount = 0) then
    Exit;
  LockScrollBars;
  try
    EndSel := TmpSel;
    for I := 0 to SelectedObjectCount - 1 do
    begin
      AObject := SelectedObjects[I];
      AObject.Move(DX, DY);
    end;
    P.X := DX;
    P.Y := DY;
    ScalePoint(P);
    for I := 0 to ConnectionCount - 1 do
    begin
      AConnection := Connections[I];
      AConnection.Move(DX, DY, P);
    end;
    RestoreSel(EndSel);
  finally
    UnlockScrollBars;
  end;
  InvalidateRouting;
  SetChartSizes;
end;

procedure TdxCustomFlowChart.StartExternalDrag(const AScreenPoint: TPoint; const ACreateObject: TCreateObjectReference);
var
  AObject: TdxFcObject;
  ADefaultObjectSize: TSize;
  AChartPoint, ALocalPoint: TPoint;
begin
  Windows.SetFocus(Handle);
  SetCapture(Handle);
  ALocalPoint := ScreenToClient(AScreenPoint);
  AChartPoint := ChartPoint(ALocalPoint);
  LockUpdateChartSize;
  AObject := ACreateObject;
  ADefaultObjectSize := AObject.GetDefaultSize;
  AObject.SetBounds(
    AChartPoint.X - (ADefaultObjectSize.cx div 2),
    AChartPoint.Y - (ADefaultObjectSize.cy div 2),
    ADefaultObjectSize.cx,
    ADefaultObjectSize.cy);
  UnlockUpdateChartSize;
  FDragStartPoint := ALocalPoint;
  ClearSelection;
  AObject.Selected := True;
  FHitTestResult := [htByObject, htOnObject];
  FObjectAt := AObject;
  DragHelper.DragStart(ALocalPoint.X, ALocalPoint.Y, TdxFlowChartDragHelper.TFlowChartDragKind.Move);
end;

procedure TdxCustomFlowChart.StartConnectionEndpointHighlight(X, Y: Integer);
begin
  StopConnectionEndpointHighlight;
  FConnectionHighlights := TConnectionHighlights.Create(Self, X, Y, ScaleFactor.Apply(24));
end;

procedure TdxCustomFlowChart.StopConnectionEndpointHighlight;
begin
  FreeAndNil(FConnectionHighlights);
end;

procedure TdxCustomFlowChart.KeyDown(var Key: Word; Shift: TShiftState);
  procedure Resize(DX, DY: Integer);
  var
    W, H: Integer;
  begin
    if SelectedObject = nil then Exit;
    if RealZoom < 100 then
    begin
      DX := MulDiv(DX, 100, RealZoom);
      DY := MulDiv(DY, 100, RealZoom);
    end;
    with SelectedObject do
    begin
      W := Width + DX;
      H := Height + DY;
      if (W > 0) and (H > 0) then SetBounds(Left, Top, W, H);
      MakeVisible;
    end;
  end;
  procedure SelNext(Mode: Integer);
  var
    I, DX, DY, Rate, Min: Integer; Obj: TdxFcObject;
  begin
    if SelectedObject = nil then Exit;
    Obj := nil; Min := $20000000;
    for I := 0 to ObjectCount - 1 do
    begin
      if Objects[I].Selected or not Objects[I].Visible then Continue;
      DX := Objects[I].Left - SelectedObject.Left;
      DY := Objects[I].Top - SelectedObject.Top;
      if Mode > 1 then Swap(DX, DY);
      if Mode and 1 <> 0 then DX := -DX;
      Rate := Abs(DX) + Abs(DY) shl 3 + DX shr 2;
      if Rate < Min then
      begin
        Min := Rate;
        Obj := Objects[I];
      end;
    end;
    if Obj <> nil then
    begin
      ClearSelection;
      Obj.Selected := True;
      if Obj.Selected then Obj.MakeVisible;
    end;
  end;
begin
  if DragHelper.Dragging then
    Exit;
  inherited KeyDown(Key, Shift);
  if (Shift = [ssShift]) and (fcoCanDrag in Options) then
    case Key of
      VK_RIGHT: Resize(1, 0);
      VK_LEFT: Resize(-1, 0);
      VK_DOWN: Resize(0, 1);
      VK_UP: Resize(0, -1);
      VK_NEXT: Resize(1, 1);
      VK_PRIOR: Resize(1, -1);
      VK_HOME: Resize(-1, -1);
      VK_END: Resize(-1, 1);
    end;
  if (Shift = [ssAlt]) and (fcoCanDrag in Options) then
    case Key of
      VK_RIGHT: OffsetObjects(1, 0);
      VK_LEFT: OffsetObjects(-1, 0);
      VK_DOWN: OffsetObjects(0, 1);
      VK_UP: OffsetObjects(0, -1);
      VK_NEXT: OffsetObjects(1, 1);
      VK_PRIOR: OffsetObjects(1, -1);
      VK_HOME: OffsetObjects(-1, -1);
      VK_END: OffsetObjects(-1, 1);
    end;
  if Shift = [ssCtrl] then
    case Key of
      VK_RIGHT: LeftEdge := LeftEdge + ClientWidth - 16;
      VK_LEFT: LeftEdge := LeftEdge - ClientWidth + 16;
      VK_PRIOR: TopEdge := TO_HOME;
      VK_NEXT: TopEdge := TO_END;
      VK_HOME: SetLeftTop(TO_HOME, TO_HOME);
      VK_END: SetLeftTop(TO_END, TO_END);
    end;
  if Shift = [] then
    case Key of
      VK_DELETE:
        if fcoCanDelete in Options then DeleteSelection;
      VK_NEXT: TopEdge := TopEdge + ClientHeight - 16;
      VK_PRIOR: TopEdge := TopEdge - ClientHeight + 16;
      VK_HOME: LeftEdge := TO_HOME;
      VK_END: LeftEdge := TO_END;
      VK_RIGHT: SelNext(0);
      VK_LEFT: SelNext(1);
      VK_DOWN: SelNext(2);
      VK_UP: SelNext(3);
    end;
end;

procedure TdxCustomFlowChart.LookAndFeelChanged(
  Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  inherited LookAndFeelChanged(Sender, AChangedValues);
  if HandleAllocated then
    Invalidate;
end;

procedure TdxCustomFlowChart.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SetFocus;
  if not (ssDouble in Shift) then
    SetCapture(Handle);
  if (Button = mbLeft) and (Shift * [ssCtrl] = []) then
  begin
    HitTest(X, Y);
    FDragStartPoint := Point(X, Y);
    if [htOnSelPoint, htOnRotateMark, htOnParameter] * FHitTestResult <> [] then
      DragHelper.CheckDragStart(X, Y, Shift, FHitTestResult);
    if DragHelper.Dragging then
      Exit;
    ProcessMouseDown(Button, Shift, X, Y);
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TdxCustomFlowChart.MouseLeave(AControl: TControl);
begin
  FPressedItem := nil;
  HighlightedObject := nil;
  inherited MouseLeave(AControl);
end;

procedure TdxCustomFlowChart.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if GetCapture = Handle then
  begin
    if DragHelper.Dragging then
      DragHelper.DragMove(X, Y)
    else
      if not IsScrollBarsArea(Point(X, Y)) and Focused and (ssLeft in Shift) and (Abs(X - FDragStartPoint.X) + Abs(Y - FDragStartPoint.Y) > 4) then
        DragHelper.CheckDragStart(X, Y, Shift, FHitTestResult);
  end
  else
  begin
    HitTest(X, Y);
    if htOnObject in FHitTestResult then
      FObjectAt.MouseMove(Shift, X, Y);
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TdxCustomFlowChart.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if GetCapture = Handle then
    ReleaseCapture;
  DragHelper.DragStop(X, Y);

  HitTest(X, Y);
  if htOnObject in FHitTestResult then
  begin
    FObjectAt.MouseUp(Button, Shift, X, Y);
    if FPressedItem = FObjectAt then
      FObjectAt.Click;
  end;
  FPressedItem := nil;
  inherited MouseUp(Button, Shift, X, Y);
end;

function TdxCustomFlowChart.NeedAdjustObjectsPoints: Boolean;
begin
  Result := AutoRouting;
end;

procedure TdxCustomFlowChart.BeginDragObjectsPaint(ACanvas: TcxCanvas);
begin
  FSelectionSpecialCanvas := ACanvas;
end;

procedure TdxCustomFlowChart.LockUpdateChartSize;
begin
  FUpdateChartSizeLocked := True;
end;

procedure TdxCustomFlowChart.UnlockUpdateChartSize;
begin
  FUpdateChartSizeLocked := False;
end;

procedure TdxCustomFlowChart.EndDragObjectsPaint;
begin
  FSelectionSpecialCanvas := nil;
end;

procedure TdxCustomFlowChart.AbortDrag;
begin
  if DragHelper.Dragging then
  begin
    with CalcCursorPos do
      MouseUp(mbLeft, [], X, Y);
  end;
end;

procedure TdxCustomFlowChart.LoadFromOldStream(Stream: TStream);

  function GetIsUnicodeFormat(AStream: TStream): Boolean;
  var
    B: array[0..6] of AnsiChar;
  begin
    Result := False;
    if (Stream.Size - Stream.Position) > SizeOf(B) then
    begin
      Stream.ReadBuffer(B, SizeOf(B));
      Result := B = StreamDescriptionUNICODE;
      if not Result and (B <> StreamDescriptionANSI) then
        Stream.Position := Stream.Position - SizeOf(B);
    end;
  end;

  procedure LoadObjects(AStream: TStream; AIsUnicode: Boolean);
  var
    ACount, I: Integer;
  begin
    ACount := 0;
    Stream.ReadBuffer(ACount, SizeOf(Word));
    while ACount > 0 do
    begin
      InternalCreateObject.Load(AStream, AIsUnicode);
      Dec(ACount);
    end;
    for I := 0 to ObjectCount - 1 do
      Objects[I].ResolveObjRefs;
  end;

  procedure LoadConnections(AStream: TStream; AIsUnicode: Boolean);
  var
    ACount: Integer;
  begin
    ACount := 0;
    Stream.ReadBuffer(ACount, SizeOf(Word));
    while ACount > 0 do
    begin
      InternalCreateConnection.Load(AStream, AIsUnicode);
      Dec(ACount);
    end;
  end;

var
  AIsUnicode: Boolean;
begin
  AIsUnicode := GetIsUnicodeFormat(Stream);
  LoadObjects(Stream, AIsUnicode);
  LoadConnections(Stream, AIsUnicode);
end;

procedure TdxCustomFlowChart.LoadFromXmlStream(Stream: TStream);

  procedure LoadObjects(AParentNode: TdxXMLNode);
  var
    I: Integer;
    ANode: TdxXMLNode;
  begin
    ANode := AParentNode.First;
    while ANode <> nil do
    begin
      InternalCreateObject.Load(ANode);
      ANode := ANode.Next;
    end;
    for I := 0 to ObjectCount - 1 do
      Objects[I].ResolveObjRefs;
  end;

  procedure LoadConnections(AParentNode: TdxXMLNode);
  var
    ANode: TdxXMLNode;
  begin
    ANode := AParentNode.First;
    while ANode <> nil do
    begin
      InternalCreateConnection.Load(ANode);
      ANode := ANode.Next;
    end;
  end;

var
  ADocument: TdxXMLDocument;
  AFlowChartNode: TdxXMLNode;
begin
  ADocument := TdxXMLDocument.Create;
  try
    ADocument.LoadFromStream(Stream);
    AFlowChartNode := ADocument.Root.FindChild('FlowChart');
    if AFlowChartNode <> nil then
    begin
      if AFlowChartNode.Attributes.GetValueAsInteger('Version', 1) > CurrentXMLVersion then
        Exit;
      LoadObjects(AFlowChartNode.FindChild('Objects'));
      LoadConnections(AFlowChartNode.FindChild('Connections'));
      if AFlowChartNode.Attributes.GetValueAsBoolean('AutoRoute', False) then
        Options := Options + [fcoAutoRouteConnections];
    end;
  finally
    ADocument.Free;
  end;
end;

procedure TdxCustomFlowChart.LoadFromStream(Stream: TStream);

  function IsXmlStream: Boolean;
  const
    XMLPattern: AnsiString = '<?xml version="1.0" encoding="UTF-8"?>';
  var
    ABuffer: AnsiString;
    ASize: Integer;
  begin
    Result := False;
    ASize := Length(XMLPattern);
    if (Stream.Size - Stream.Position) > ASize then
    begin
      SetLength(ABuffer, ASize);
      Stream.ReadBuffer(ABuffer[1], ASize);
      Result := AnsiSameText(XMLPattern, ABuffer);
      if not Result then
        Stream.Position := Stream.Position - ASize;
    end;
  end;

begin
  BeginUpdate;
  try
    FLoading := True;
    try
      Clear;
      if IsXmlStream then
        LoadFromXmlStream(Stream)
      else
        LoadFromOldStream(Stream);
    finally
      FLoading := False;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TdxCustomFlowChart.SaveToStream(Stream: TStream);
var
  I: Integer;
  ADocument: TdxXMLDocument;
  ARootNode, ANode: TdxXMLNode;
begin
  ADocument := TdxXMLDocument.Create;
  try
    ARootNode := ADocument.Root.AddChild('FlowChart');
    ARootNode.Attributes.SetValueAsInteger('Version', CurrentXMLVersion);
    ARootNode.Attributes.SetValueAsBoolean('AutoRoute', fcoAutoRouteConnections in Options);
    ANode := ARootNode.AddChild('Objects');
    for I := 0 to ObjectCount - 1 do
      Objects[I].Save(ANode.AddChild('Object'));
    ANode := ARootNode.AddChild('Connections');
    for I := 0 to ConnectionCount - 1 do
      Connections[I].Save(ANode.AddChild('Connection'));
    ADocument.SaveToStream(Stream);
  finally
    ADocument.Free;
  end;
end;

procedure TdxCustomFlowChart.LoadFromFile(const FileName: string);
var
  S: TStream;
begin
  S := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(S);
  finally
    S.Free;
  end;
end;

procedure TdxCustomFlowChart.SaveToFile(const FileName: string);
var
  S: TStream;
begin
  S := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(S);
  finally
    S.Free;
  end;
end;

procedure TdxCustomFlowChart.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('Items', LoadFromStream, SaveToStream,
    (ObjectCount > 0) or (ConnectionCount > 0));
end;

function TdxCustomFlowChart.CreatePainter: TdxFlowChartPainter;
begin
  if Antialiasing and CheckGdiPlus then
    Result := TdxFlowChartGDIPlusPainter.Create
  else
    Result := TdxFlowChartPainter.Create;
end;

function TdxCustomFlowChart.CreateRoutingStrategy: TdxDiagramRoutingStrategy;
var
  AItemMargin: Integer;
begin
  AItemMargin := PaintScaleFactor.Apply(GridLineOptions.MinorLineStep);
  Result := TdxRightAngleRoutingStrategy.Create(Self, AItemMargin);
end;

function TdxCustomFlowChart.CreateSelection: TdxFcSelection;
begin
  Result := TdxFcSelection.Create(Self);
end;

function TdxCustomFlowChart.GetAlignedChartPoint(X, Y: Integer): TPoint;
var
  AGridSize: Integer;
begin
  AGridSize := GridLineOptions.MinorLineStep;
  X := Round(ScaleFactor.RevertF(X) / AGridSize) * AGridSize;
  X := ScaleFactor.Apply(X);
  Y := Round(ScaleFactor.RevertF(Y) / AGridSize) * AGridSize;
  Y := ScaleFactor.Apply(Y);
  Result.Init(X, Y);
end;

function TdxCustomFlowChart.GetAlignedDisplayPoint(X, Y: Integer): TPoint;
begin
  Result := ChartPoint(X, Y);
  Result := GetAlignedChartPoint(Result.X, Result.Y);
  Result := ChartPointToDisplayPoint(Result);
end;

function TdxCustomFlowChart.GetAutoRouting: Boolean;
begin
  Result := fcoAutoRouteConnections in Options;
end;

function TdxCustomFlowChart.GetEndpointHighlightedObject: TdxFcObject;
begin
  if ConnectionHighlights = nil then
    Exit(nil);

  if AutoRouting then
    Result := ConnectionHighlights.&Object
  else
  begin
    if ConnectionHighlights.PointInfo = nil then
      Exit(nil);
    Result := ConnectionHighlights.PointInfo.&Object;
  end;
end;

procedure TdxCustomFlowChart.ImageListChanged;
begin
  NeedRepaint;
end;

function TdxCustomFlowChart.IsDefaultGesture(AGestureID: Integer): Boolean;
begin
  Result := inherited IsDefaultGesture(AGestureID) or (AGestureID = GID_ZOOM);
end;

function TdxCustomFlowChart.CanCreateNewConnectionOnDrag: Boolean;
begin
  Result := (ObjectAt <> nil) and ObjectAt.Selected and ([htOnObject, htOnSelPoint, htOnRotateMark] * FHitTestResult = []);
end;

function TdxCustomFlowChart.CanDetachConnections: Boolean;
begin
  Result := True;
end;

function TdxCustomFlowChart.CanDragConnectionEndpoints: Boolean;
begin
  Result := fcoCanDrag in Options;
end;

function TdxCustomFlowChart.CanDragConnectionIntermediatePoints: Boolean;
begin
  Result := (fcoCanDrag in Options) and not AutoRouting;
end;

function TdxCustomFlowChart.CanResizeObjectsHorizontal: Boolean;
begin
  Result := True;
end;

function TdxCustomFlowChart.CanResizeObjectsVertical: Boolean;
begin
  Result := True;
end;

function TdxCustomFlowChart.CanSelect(Item: TdxFcItem): Boolean;
begin
  Result := fcoCanSelect in Options;
  if Assigned(OnSelection) then OnSelection(Self, Item, Result);
end;

procedure TdxCustomFlowChart.Added(Item: TdxFcItem);
begin
  ContentChanged;
end;

function TdxCustomFlowChart.AllowTouchScrollUIMode: Boolean;
begin
  Result := not IsDesigning;
end;

procedure TdxCustomFlowChart.BoundsChanged;
begin
  inherited BoundsChanged;
  CalculateRealZoomFactor;
end;

procedure TdxCustomFlowChart.ContentChanged;
begin
  if (FLockUpdates = 0) and not IsDestroying then
  begin
    FHasContentChanges := False;
    CalculateChartSizes;
    CalculateRealZoomFactor;
  end
  else
    FHasContentChanges := True;
end;

procedure TdxCustomFlowChart.CalculateRealZoomFactor;
var
  APrevZoom: Word;
  W, H: Integer;
begin
  APrevZoom := FRealZoom;

  if Zoom = 0 then
  begin
    W := MulDiv(FChartWidth, 100, RealZoom);
    H := MulDiv(FChartHeight, 100, RealZoom);
    if ClientWidth * H <= ClientHeight * W then
      FRealZoom := MulDiv(ClientWidth, 95, W)
    else
      FRealZoom := MulDiv(ClientHeight, 95, H);

    FRealZoom := Max(1, Min(FRealZoom, 100));
  end
  else
    FRealZoom := Zoom;

  if APrevZoom <> FRealZoom then
  begin
    UpdatePaintScaleFactor;
    CalculateRealPos;
    CalculateChartSizes;
  end;
end;

procedure TdxCustomFlowChart.Changed(Item: TdxFcItem);
begin
  ContentChanged;
  if Assigned(OnChange) then
    OnChange(Self, Item);
end;

procedure TdxCustomFlowChart.ChangeScaleEx(M: Integer; D: Integer; isDpiChange: Boolean);
var
  I: Integer;
begin
  BeginUpdate;
  try
    inherited ChangeScaleEx(M, D, isDpiChange);
    UpdatePaintScaleFactor;
    Selection.UpdateGraphicTools;
    for I := 0 to ConnectionCount - 1 do
      Connections[I].ChangeScale(M, D);
    for I := 0 to ObjectCount - 1 do
      Objects[I].ChangeScale(M, D);
    ContentChanged;
  finally
    EndUpdate;
  end;
end;

procedure TdxCustomFlowChart.Delete(Item: TdxFcItem);
begin
  AbortDrag;
  Item.Invalidate;
  if Item.Selected then
  begin
    Item.SelList.Remove(Item);
    Item.SetSelectedState(False);
    Select(Item);
  end;
  if Assigned(OnDeletion) then
    OnDeletion(Self, Item);
  Item.FDestroying := True;
  FHitTestResult := [];
  ContentChanged;
end;

procedure TdxCustomFlowChart.ScaleFactorChanged;
begin
  inherited ScaleFactorChanged;
  Perform(CM_FONTCHANGED, 0, 0);
end;

procedure TdxCustomFlowChart.Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer);

  function DoScroll(ACode: TScrollCode; AValue, APage: Integer): Integer;
  begin
    case ACode of
      scLineDown:
        Inc(AValue, GetScrollLineSize);
      scLineUp:
        Dec(AValue, GetScrollLineSize);
      scPageDown:
        Inc(AValue, APage - GetScrollLineSize);
      scPageUp:
        Dec(AValue, APage - GetScrollLineSize);
      scTop:
        AValue := TO_HOME;
      scBottom:
        AValue := TO_END;
      scPosition, scTrack:
        AValue := AScrollPos;
    end;
    Result := AValue;
  end;

begin
  if AScrollBarKind = sbHorizontal then
  begin
    SetLeftEdge(DoScroll(AScrollCode, LeftEdge, ClientWidth));
    AScrollPos := LeftEdge;
  end
  else
  begin
    SetTopEdge(DoScroll(AScrollCode, TopEdge, ClientHeight));
    AScrollPos := TopEdge;
  end;
end;

procedure TdxCustomFlowChart.Select(Item: TdxFcItem);
begin
  if Assigned(OnSelected) then OnSelected(Self, Item);
end;

procedure TdxCustomFlowChart.ValidateObjectsResizing(var ASelection: TdxFcSelectionPoints);
var
  AHorizontal, AVertical: Boolean;
begin
  AHorizontal := CanResizeObjectsHorizontal;
  AVertical := CanResizeObjectsVertical;
  if not AHorizontal then
  begin
    ASelection[3].Init(-30000, -30000);
    ASelection[7].Init(-30000, -30000);
  end;
  if not AVertical then
  begin
    ASelection[1].Init(-30000, -30000);
    ASelection[5].Init(-30000, -30000);
  end;
  if not AHorizontal or not AVertical then
  begin
    ASelection[0].Init(-30000, -30000);
    ASelection[2].Init(-30000, -30000);
    ASelection[4].Init(-30000, -30000);
    ASelection[6].Init(-30000, -30000);
  end;
end;

procedure TdxCustomFlowChart.WndProc(var Message: TMessage);
begin
  if (Message.Msg >= WM_KEYFIRST) and (Message.Msg <= WM_KEYLAST) then
  begin
    if DragHelper.Dragging then
    begin
      case Message.Msg of
        WM_SYSKEYDOWN,
        WM_SYSKEYUP:
          DragHelper.ToggleSysKey(VK_MENU);
      end;
      Exit;
    end;
  end;
  inherited WndProc(Message);
end;

procedure TdxCustomFlowChart.WMSetFocus(var Msg: TWMSetFocus);
begin
  inherited;
  if fcoHideSelection in Options then InvalidateSel;
end;

procedure TdxCustomFlowChart.WMKillFocus(var Msg: TWMKillFocus);
begin
  if GetCapture = Handle then
    ReleaseCapture;
  AbortDrag;
  WMSetFocus(TWMSetFocus(Msg));
end;

procedure TdxCustomFlowChart.WMSetCursor(var Message: TWMSetCursor);

  function HitAtSelectedObject: Boolean;
  begin
    Result := (FObjectAt <> nil) and (FObjectAt = SelectedObject);
  end;

var
  pt: TPoint;
begin
  pt := GetMouseCursorClientPos;
  HitTest(pt.X, pt.Y);
  if htOnParameter in FHitTestResult then
  begin
    if HitAtSelectedObject then
      Windows.SetCursor(Screen.Cursors[crFlChartCross])
    else
      inherited;
  end
  else if htOnRotateMark in FHitTestResult then
  begin
    if HitAtSelectedObject then
      Windows.SetCursor(Screen.Cursors[crFlChartHoverRotation])
    else
      inherited;
  end
  else if htOnSelPoint in FHitTestResult then
  begin
    if HitAtSelectedObject then
      Windows.SetCursor(Screen.Cursors[FObjectAt.GetResizeCursor(pt)])
    else
      Windows.SetCursor(Screen.Cursors[crSize]);
  end
  else
    inherited;
end;

procedure TdxCustomFlowChart.WMErase(var Msg: TWMEraseBkgnd);
begin
  Msg.Result := 1;
end;

procedure TdxCustomFlowChart.WMNCHitTest(var Msg: TWMNCHitTest);
begin
  DefaultHandler(Msg);
  if (csDesigning in ComponentState) and not (Msg.Result in [HTHSCROLL, HTVSCROLL]) then
    Msg.Result := HTCLIENT;
end;

procedure TdxCustomFlowChart.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  if BorderStyle = bsSingle then
    RecreateWnd;
end;

procedure TdxCustomFlowChart.CMFontChanged(var Msg: TMessage);

  procedure ResetFont(AItem: TdxFcItem);
  begin
    if AItem.ParentFont then
      AItem.SyncFontWithParentFont;
  end;

var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to ObjectCount - 1 do
      ResetFont(Objects[I]);
    for I := 0 to ConnectionCount - 1 do
      ResetFont(Connections[I]);
  finally
    EndUpdate;
  end;
end;

procedure TdxCustomFlowChart.CNKeyDown(var Message: TWMKeyDown);
begin
  if DragHelper.Dragging then
  begin
    case Message.CharCode of
      VK_ESCAPE:
        begin
          ReleaseCapture;
          Perform(WM_CANCELMODE, 0, 0);
          Message.Result := 1;
          Exit;
        end;
      VK_CONTROL,
      VK_SHIFT:
        DragHelper.ToggleSysKey(Message.CharCode);
    end;
  end;
  inherited;
end;

procedure TdxCustomFlowChart.CNKeyUp(var Message: TWMKeyUp);
begin
  if DragHelper.Dragging then
  begin
    case Message.CharCode of
      VK_CONTROL,
      VK_SHIFT:
        DragHelper.ToggleSysKey(Message.CharCode);
    end;
  end;
  inherited;
end;

procedure TdxCustomFlowChart.DoScrollUIModeChanged;
begin
  UpdateScrollBars;
end;

procedure TdxCustomFlowChart.OptimizeConnectionEndpoints;
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to ConnectionCount - 1 do
      Connections[I].OptimizeEndpoints;
  finally
    EndUpdate;
  end;
end;

{ TdxFlowChartGridLineOptions }

constructor TdxFlowChartGridLineOptions.Create(AOwner: TdxFlowChartGridOptions; AColor: TColor; AStyle: TPenStyle);
begin
  inherited Create;
  FOwner := AOwner;
  FColor := AColor;
  FDefaultColor := AColor;
  FStyle := AStyle;
  FDefaultStyle := AStyle;
end;

procedure TdxFlowChartGridLineOptions.Assign(Source: TPersistent);
begin
  if Source is TdxFlowChartGridLineOptions then
  begin
    FColor := TdxFlowChartGridLineOptions(Source).Color;
    FStyle := TdxFlowChartGridLineOptions(Source).Style;
  end
  else
    inherited Assign(Source);
end;

procedure TdxFlowChartGridLineOptions.Changed;
begin
  FOwner.Changed;
end;

function TdxFlowChartGridLineOptions.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TdxFlowChartGridLineOptions.IsColorStored: Boolean;
begin
  Result := FColor <> FDefaultColor;
end;

function TdxFlowChartGridLineOptions.IsStored: Boolean;
begin
  Result := IsStyleStored or IsColorStored;
end;

function TdxFlowChartGridLineOptions.IsStyleStored: Boolean;
begin
  Result := FStyle <> FDefaultStyle;
end;

procedure TdxFlowChartGridLineOptions.SetColor(const Value: TColor);
begin
  if Value <> FColor then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TdxFlowChartGridLineOptions.SetStyle(const Value: TPenStyle);
begin
  if Value <> FStyle then
  begin
    FStyle := Value;
    Changed;
  end;
end;

procedure TdxFlowChartGridLineOptions.SetupCanvas(ACanvas: TcxCanvas);
begin
  ACanvas.Pen.Color := Color;
  ACanvas.Pen.Style := Style;
end;

{ TdxFlowChartGridOptions }

constructor TdxFlowChartGridOptions.Create(AOwner: TdxCustomFlowChart);
begin
  inherited Create;
  FOwner := AOwner;
  FShowLines := False;
  FMinorLineStep := DefaultMinorLineStep;
  FMinZoomThresholdForMinorLines := DefaultMinZoomThresholdForMinorLines;
  FMajorLines := CreateLines(DefaultMajorLineColor, DefaultMajorLineStyle);
  FMinorLines := CreateLines(DefaultMinorLineColor, DefaultMinorLineStyle);
  FOriginLines := CreateLines(DefaultOriginLineColor, DefaultOriginLineStyle);
end;

destructor TdxFlowChartGridOptions.Destroy;
begin
  FMajorLines.Free;
  FMinorLines.Free;
  FOriginLines.Free;
  inherited Destroy;
end;

procedure TdxFlowChartGridOptions.Assign(Source: TPersistent);
begin
  if Source is TdxFlowChartGridOptions then
  begin
    FShowLines := TdxFlowChartGridOptions(Source).ShowLines;
    FMinorLineStep := TdxFlowChartGridOptions(Source).MinorLineStep;
    FMajorLines := TdxFlowChartGridOptions(Source).MajorLines;
    FMinorLines := TdxFlowChartGridOptions(Source).MinorLines;
    FOriginLines := TdxFlowChartGridOptions(Source).OriginLines;
    FMinZoomThresholdForMinorLines := TdxFlowChartGridOptions(Source).MinZoomThresholdForMinorLines;
  end
  else
    inherited Assign(Source);
end;

procedure TdxFlowChartGridOptions.Changed;
begin
  FOwner.Invalidate;
end;

function TdxFlowChartGridOptions.CreateLines(AColor: TColor; AStyle: TPenStyle): TdxFlowChartGridLineOptions;
begin
  Result := TdxFlowChartGridLineOptions.Create(Self, AColor, AStyle);
end;

function TdxFlowChartGridOptions.GetEdgeSizes: TSize;
begin
  Result.cx := GetScaleFactor.Revert(FOwner.LeftEdge);
  Result.cy := GetScaleFactor.Revert(FOwner.TopEdge);
end;

function TdxFlowChartGridOptions.GetOrigin: TPoint;
begin
  Result := FOwner.ChartPointToDisplayPoint(TPoint.Null);
end;

function TdxFlowChartGridOptions.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TdxFlowChartGridOptions.GetScaleFactor: TdxScaleFactor;
begin
  Result := FOwner.PaintScaleFactor;
end;

function TdxFlowChartGridOptions.IsMajorLinesStored: Boolean;
begin
  Result := FMajorLines.IsStored;
end;

function TdxFlowChartGridOptions.IsMinorLinesStored: Boolean;
begin
  Result := FMinorLines.IsStored;
end;

function TdxFlowChartGridOptions.IsOriginLinesStored: Boolean;
begin
  Result := FOriginLines.IsStored;
end;

function TdxFlowChartGridOptions.NeedPaintMinorLines: Boolean;
begin
  Result := FOwner.RealZoom >= MinZoomThresholdForMinorLines;
end;

procedure TdxFlowChartGridOptions.SetShowLines(Value: Boolean);
begin
  if Value <> FShowLines then
  begin
    FShowLines := Value;
    Changed;
  end;
end;

procedure TdxFlowChartGridOptions.SetMinorLineStep(Value: Integer);
begin
  Value := Max(Value, 2);
  if Value <> FMinorLineStep then
  begin
    FMinorLineStep := Value;
    Changed;
  end;
end;

procedure TdxFlowChartGridOptions.SetMajorLines(const Value: TdxFlowChartGridLineOptions);
begin
  FMajorLines.Assign(Value);
end;

procedure TdxFlowChartGridOptions.SetMinorLines(const Value: TdxFlowChartGridLineOptions);
begin
  FMinorLines.Assign(Value);
end;

procedure TdxFlowChartGridOptions.SetMinZoomThresholdForMinorLines(Value: Integer);
begin
  Value := Max(Value, 1);
  if Value <> FMinZoomThresholdForMinorLines then
  begin
    FMinZoomThresholdForMinorLines := Value;
    Changed;
  end;
end;

procedure TdxFlowChartGridOptions.SetOriginLines(const Value: TdxFlowChartGridLineOptions);
begin
  FOriginLines.Assign(Value);
end;


{TdxFcConnection}

constructor TdxFcConnection.Create(AOwner: TdxCustomFlowChart);
begin
  inherited Create(AOwner);
  FPenWidth := 1;
  FVisible := True;
  FPenStyle := DefaultPenStyle;
  FTransparent := DefaultTransparent;
  FPoints := TdxFcPoints.Create;
  FRealPoints := TdxFcPoints.Create;
  FArrowSource := CreateArrow;
  FArrowDest := CreateArrow;
  AOwner.FConnections.Add(Self);
  AOwner.Added(Self);
end;

destructor TdxFcConnection.Destroy;
var
  Resize: Boolean;
begin
  Owner.Delete(Self);
  //Update linked objects for dest and source object.
  Resize := not ((ObjectSource <> nil) and ObjectSource.Destroying or (ObjectDest <> nil) and ObjectDest.Destroying);
  SetObjectSource(nil, 0);
  SetObjectDest(nil, 0);
  FreeAndNil(FPoints);
  FreeAndNil(FRealPoints);
  FreeAndNil(FArrowSource);
  FreeAndNil(FArrowDest);
  Owner.FConnections.Remove(Self);
  if Resize then
    Owner.SetChartSizes;
  inherited Destroy;
end;

function TdxFcConnection.IndexValid(var Index: Integer; AMax: Integer): Boolean;
begin
  Result := (Index >= 0) and (Index <= PointCount - AMax);
  if ObjectSource <> nil then
    Inc(Index);
end;

function TdxFcConnection.GetPoint(Index: Integer): TPoint;
begin
  if IndexValid(Index, 1) then
    Result := FPoints[Index]
  else
    Result := cxNullPoint;
end;

function TdxFcConnection.GetRealPoint(Index: Integer): TPoint;
begin
  Result := FRealPoints[Index];
end;

function TdxFcConnection.GetRealTextRect: TRect;
begin
  Result := Owner.ScrollRect(FTextRect, False);
end;

procedure TdxFcConnection.SetPoint(AIndex: Integer; AValue: TPoint);
begin
  if IndexValid(AIndex, 1) then
  begin
    ConnectionChanged;
    FPoints[AIndex] := AValue;
    Owner.ScalePoint(AValue);
    FRealPoints[AIndex] := AValue;
    CalculateDisplayRect;
    Changed;
    Owner.SetChartSizes;
  end;
end;

function TdxFcConnection.GetRealCount: Integer;
begin
  Result := FRealPoints.Count;
end;

function TdxFcConnection.GetPointCount: Integer;
begin
  Result := RealCount;
  if ObjectSource <> nil then
    Dec(Result);
  if ObjectDest <> nil then
    Dec(Result);
end;

function TdxFcConnection.SelList: TList;
begin
  Result := Owner.FSelConnections;
end;

procedure TdxFcConnection.AddPoint(const P: TPoint);
begin
  InsertPoint(PointCount, P);
end;

procedure TdxFcConnection.InsertPoint(Index: Integer; const P: TPoint);
begin
  if IndexValid(Index, 0) then
  begin
    ConnectionChanged;
    InternalInsertPoint(Index, P);
    CalculateDisplayRect;
    Changed;
    Owner.SetChartSizes;
  end;
end;

procedure TdxFcConnection.RemovePoint(Index: Integer);
begin
  if IndexValid(Index, 1) then
  begin
    ConnectionChanged;
    FPoints.Delete(Index);
    FRealPoints.Delete(Index);
    CalculateDisplayRect;
    Changed;
    Owner.SetChartSizes;
  end;
end;

procedure TdxFcConnection.SetPenStyle(Value: TPenStyle);
begin
  if FPenStyle <> Value then
  begin
    FPenStyle := Value;
    ConnectionChanged;
    Changed;
  end;
end;

procedure TdxFcConnection.SetPenWidth(Value: Integer);
begin
  if FPenWidth <> Value then
  begin
    FPenWidth := Value;
    ConnectionChanged;
    Changed;
  end;
end;

procedure TdxFcConnection.SetStyle(Value: TdxFclStyle);
begin
  if (FStyle <> Value) then
  begin
    if (ArrowSource.ArrowType = fcaArrow) or (ArrowDest.ArrowType = fcaArrow) then
      ConnectionChanged;
    FStyle := Value;
    ArrowSource.ClearPoints;
    ArrowDest.ClearPoints;
    CalculateDisplayRect;
    Changed;
  end;
end;

procedure TdxFcConnection.SetText(Value: string);
begin
  if (FText <> Value) then
  begin
    if Text <> '' then
      InvalidateText;
    FText := Value;
    if Text <> '' then
    begin
      CalculateTextRect;
      InvalidateText;
    end;
    Changed;
    Owner.SetChartSizes;
  end;
end;

procedure TdxFcConnection.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    ConnectionChanged;
    Changed;
  end;
end;

procedure TdxFcConnection.SetTransparent(Value: Boolean);
begin
  if (FTransparent <> Value) then
  begin
    FTransparent := Value;
    ConnectionChanged;
    Changed;
  end;
end;

procedure TdxFcConnection.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Selected := False;
    Owner.FHitTestResult := [];
    Owner.NeedRepaintObject(Self);
  end;
end;

procedure TdxFcConnection.SetArrowSource(Value: TdxFcConnectionArrow);
begin
  ArrowSource.Assign(Value);
end;

procedure TdxFcConnection.SetArrowDest(Value: TdxFcConnectionArrow);
begin
  ArrowDest.Assign(Value);
end;

function TdxFcConnection.GetDisplayRect: TRect;
begin
  Result := FDisplayRect;
  if Text <> '' then
    UnionRect(Result, Result, FTextRect);
  if ArrowSource.Active then
    UnionRect(Result, Result, ArrowSource.DisplayRect(True));
  if ArrowDest.Active then
    UnionRect(Result, Result, ArrowDest.DisplayRect(True));
  Result := Owner.ScrollRect(Result, False);
end;

procedure TdxFcConnection.InvalidateText;
begin
  if FPoints.Count >= 2 then
  begin
    if Owner.CanPaint then
      Owner.InvalidateRect(RealTextRect, True);
    Owner.NeedRepaintObject(Self);
  end;
end;

procedure TdxFcConnection.ChangeScale(M, D: Integer);
var
  I: Integer;
begin
  inherited ChangeScale(M, D);
  ArrowDest.ChangeScale(M, D);
  ArrowSource.ChangeScale(M, D);
  for I := 0 to PointCount - 1 do
    SetPoint(I, cxPointScale(Points[I], M, D));
  CalculateDisplayRect;
end;

procedure TdxFcConnection.CheckRouting;
begin
  Owner.InvalidateRouting;
end;

procedure TdxFcConnection.ClearIntermediatePoints;
begin
  while FPoints.Count > 2 do
    FPoints.Delete(1);
  while FRealPoints.Count > 2 do
    FRealPoints.Delete(1);
end;

procedure TdxFcConnection.ClearPoints;
begin
  FPoints.Clear;
  FRealPoints.Clear;
end;

procedure TdxFcConnection.ConnectionChanged;
begin
  if FPoints.Count >= 2 then
  begin
    ArrowSource.SetPoints(0);
    ArrowDest.SetPoints(RealCount - 1);
    Owner.NeedRepaintObject(Self);
  end;
end;

function TdxFcConnection.CreateArrow: TdxFcConnectionArrow;
begin
  Result := TdxFcConnectionArrow.Create(Self);
end;

procedure TdxFcConnection.InternalInsertPoint(AIndex: Integer; P: TPoint);
begin
  FPoints.Insert(AIndex, P);
  Owner.ScalePoint(P);
  FRealPoints.Insert(AIndex, P);
end;

procedure TdxFcConnection.InsertDisplayPoint(AIndex: Integer; P: TPoint);
begin
  FRealPoints.Insert(AIndex, P);
  Owner.UnscalePoint(P);
  FPoints.Insert(AIndex, P);
end;

procedure TdxFcConnection.Invalidate;
begin
  ConnectionChanged;
end;

procedure TdxFcConnection.Calculate;
begin
  CalculateRealPoints;
  ArrowSource.SetRealBounds;
  ArrowDest.SetRealBounds;
  SetObjectPoints;
  CalculateTextRects;
  CalculateDisplayRect;
end;

procedure TdxFcConnection.CalculateDisplayRect;
var
  I: Integer;
begin
  Owner.FHitTestResult := [];
  if Destroying or (FPoints.Count < 2) then Exit;
  FDisplayRect.TopLeft := FRealPoints[0];
  FDisplayRect.BottomRight := FDisplayRect.TopLeft;
  for I := 1 to RealCount - 1 do
    dxLines.ExtendRect(FDisplayRect, FRealPoints[I]);
  FMassCenter := FRealPoints.GetLineCenter(TLineType(Style));
  Inc(FDisplayRect.Right);
  Inc(FDisplayRect.Bottom);
  if Selected then
    FDisplayRect.Inflate(1, 1);
  CalculateTextRects;
  ConnectionChanged;
end;

function TdxFcConnection.CalculatePrevDirectionBasedPoint(AForSource: Boolean): TPoint;
var
  AEndpoint: TPoint;
begin
  if AForSource then
  begin
    AEndpoint := FRealPoints[0];
    Result := FRealPoints.Points[1];
  end
  else
  begin
    AEndpoint := FRealPoints[RealCount - 1];
    Result := FRealPoints.Points[RealCount - 2];
  end;
  case Style of
    fclRectH:
      if AForSource then
        Result.Y := AEndpoint.Y
      else
        Result.X := AEndpoint.X;
    fclRectV:
      if AForSource then
        Result.X := AEndpoint.X
      else
        Result.Y := AEndpoint.Y;
  end;
end;

procedure TdxFcConnection.CalculateRealPoints;
var
  I: Integer;
  P: TPoint;
begin
  for I := 0 to RealCount - 1 do
  begin
    P := FPoints[I];
    Owner.ScalePoint(P);
    FRealPoints[I] := P;
  end;
end;

procedure TdxFcConnection.CalculateTextRect;
var
  DX, DY: Integer;
begin
  Owner.FHitTestResult := [];
  FTextRect := cxRect(0, 0, 1, 0);
  cxTextOut(cxScreenCanvas.Handle, Text, FTextRect, CXTO_CALCRECT, RealFont);
  DX := FMassCenter.X - cxRectWidth(FTextRect) div 2;
  DY := FMassCenter.Y - cxRectHeight(FTextRect) div 2;
  OffsetRect(FTextRect, DX, DY);
end;

procedure TdxFcConnection.CalculateTextRects;
begin
  if Text <> '' then
    CalculateTextRect;
end;

procedure TdxFcConnection.FontChanged;
begin
  InvalidateText;
  CalculateTextRect;
  InvalidateText;
  Owner.SetChartSizes;
end;

procedure TdxFcConnection.ZoomChanged;
begin
  ScaleFont;
  Calculate;
  CheckRouting;
end;

procedure TdxFcConnection.SetObjectPoints;

  procedure SetPoint(AObject: TdxFcObject; Arrow: TdxFcConnectionArrow; ALinkedPointIndex, APointIndex: Integer);
  begin
    if ALinkedPointIndex < 0 then
      ALinkedPointIndex := 0;
    FRealPoints[APointIndex] := AObject.LinkedPoints[ALinkedPointIndex];
    Arrow.SetPoints(APointIndex);
  end;

begin
  if Destroying or (FPoints.Count < 1) then Exit;
  if (ObjectSource <> nil) and (ObjectDest <> nil) and Owner.NeedAdjustObjectsPoints then
    AdjustObjectsPoints(FPointSource, FPointDest);
  if ObjectSource <> nil then
    SetPoint(ObjectSource, ArrowSource, FPointSource, 0);
  if ObjectDest <> nil then
    SetPoint(ObjectDest, ArrowDest, FPointDest, RealCount - 1);
end;

procedure TdxFcConnection.DelObj(AObj, Partneur: TdxFcObject; Index: Integer);
begin
  if Partneur <> nil then
  begin
    AObj.FLinkedObjects.Remove(Partneur);
    Partneur.FLinkedObjects.Remove(AObj);
  end;
  AObj.FConnections.Remove(Self);
  FRealPoints.Delete(Index);
  FPoints.Delete(Index);
end;

procedure TdxFcConnection.InsObj(AObj, Partneur: TdxFcObject; Index: Integer);
begin
  if Partneur <> nil then
  begin
    AObj.FLinkedObjects.Add(Partneur);
    Partneur.FLinkedObjects.Add(AObj);
  end;
  AObj.FConnections.Add(Self);
  InternalInsertPoint(Index, cxNullPoint);
end;

procedure TdxFcConnection.SetObjectSource(AObject: TdxFcObject; APoint: Byte);
begin
  if AObject <> nil then
  begin
    if AObject.LinkedPointCount = 0 then
      Exit;
    APoint := Min(APoint, AObject.LinkedPointCount - 1);
  end;
  if (AObject <> ObjectSource) or (APoint <> FPointSource) then
  begin
    ConnectionChanged;
    if AObject <> ObjectSource then
    begin
      if ObjectSource <> nil then
      begin
        if AObject = nil then
        begin
          InsertDisplayPoint(1, FRealPoints[0]);
        end;
        DelObj(ObjectSource, ObjectDest, 0);
      end;
      if AObject <> nil then
        InsObj(AObject, ObjectDest, 0);
      FObjectSource := AObject;
    end;
    FPointSource := APoint;
    SetObjectPoints;
    CalculateDisplayRect;
    CheckRouting;
    Changed;
  end;
end;

procedure TdxFcConnection.SetObjectDest(AObject: TdxFcObject; APoint: Byte);
var
  AIndex: Integer;
begin
  if AObject <> nil then
  begin
    if AObject.LinkedPointCount = 0 then
      Exit;
    APoint := Min(APoint, AObject.LinkedPointCount - 1);
  end;
  if (AObject <> ObjectDest) or (APoint <> FPointDest) then
  begin
    ConnectionChanged;
    if AObject <> ObjectDest then
    begin
      if ObjectDest <> nil then
      begin
        if AObject = nil then
        begin
          AIndex := FRealPoints.Count - 1;
          InsertDisplayPoint(AIndex, FRealPoints[AIndex]);
        end;
        DelObj(ObjectDest, ObjectSource, RealCount - 1);
      end;
      if AObject <> nil then
        InsObj(AObject, ObjectSource, RealCount);
      FObjectDest := AObject;
    end;
    FPointDest := APoint;
    SetObjectPoints;
    CalculateDisplayRect;
    CheckRouting;
    Changed;
  end;
end;

procedure TdxFcConnection.InitializeEndpointsForAutorouting;
var
  ASourceEndpoint, ADestinationEndpoint: TPoint;
begin
  if ObjectSource = nil then
    ASourceEndpoint := FPoints[0]
  else
    ASourceEndpoint := TPoint.Null;
  if ObjectDest = nil then
    ADestinationEndpoint := FPoints[FPoints.Count - 1]
  else
    ADestinationEndpoint := TPoint.Null;
  ClearPoints;
  InternalInsertPoint(0, ASourceEndpoint);
  InternalInsertPoint(1, ADestinationEndpoint);
  SetObjectPoints;
end;

function TdxFcConnection.ScreenPoint(Index: Integer): TPoint;
begin
  Result := Owner.ScrollPoint(FRealPoints[Index], False);
end;

function TdxFcConnection.HasPoint(const P: TPoint): Boolean;
begin
  Result := FRealPoints.IsPointOnLine(TLineType(Style), P);
end;

function TdxFcConnection.InRect(const R: TRect): Boolean;
begin
  Result := Visible;
  if Result then
    Result := FRealPoints.IsRectOnLine(TLineType(Style), Owner.ScrollRect(R));
end;

function TdxFcConnection.GetNearestPoint(X, Y: Integer): Integer;
begin
  Result := FRealPoints.GetNearestPointIndex(Owner.ScrollPoint(Point(X, Y)));
  if ObjectSource <> nil then
    Dec(Result);
end;

procedure TdxFcConnection.AdjustObjectsPoints(var APointSource, APointDest: Byte);

  function IsValidLinkedPointIndex(AObject: TdxFcObject; AIndex: Integer): Boolean;
  begin
    if AObject.ShapeType = fcsAdvanced then
      Result := True
    else
      Result := not (AIndex in [0, 4, 8, 12]);
  end;

var
  AMinSquareDistance, ASquareDistance: Int64;
  I, J, ASourcePointIndex, ADestPointIndex: Integer;
  ASourceCenter, ADestCenter, ASourcePoint, ADestPoint: TPoint;
begin
  ASourcePointIndex := -1;
  ADestPointIndex := -1;
  AMinSquareDistance := MaxInt64;
  ASourceCenter := ObjectSource.FBoundingRect.CenterPoint;
  ADestCenter := ObjectDest.FBoundingRect.CenterPoint;
  for I := 0 to ObjectSource.LinkedPointCount - 1 do
  begin
    if not IsValidLinkedPointIndex(ObjectSource, I) then
      continue;
    for J := 0 to ObjectDest.LinkedPointCount - 1 do
    begin
      if not IsValidLinkedPointIndex(ObjectDest, J) then
        continue;

      ASourcePoint := ObjectSource.LinkedPoints[I];
      ADestPoint := ObjectDest.LinkedPoints[J];
      ASquareDistance :=
        ASourcePoint.SquareOfDistance(ASourceCenter) +
        ASourcePoint.SquareOfDistance(ADestPoint) +
        ADestPoint.SquareOfDistance(ADestCenter);

      if ASquareDistance < AMinSquareDistance then
      begin
        AMinSquareDistance := ASquareDistance;
        ASourcePointIndex := I;
        ADestPointIndex := J;
      end;
    end;
  end;
  APointSource := ASourcePointIndex;
  APointDest := ADestPointIndex;
end;

procedure TdxFcConnection.ArrowChanged(Value: TdxFcConnectionArrow);
begin
  if Value = ArrowSource then
    Value.SetPoints(0);
  if Value = ArrowDest then
    Value.SetPoints(RealCount - 1);
  ConnectionChanged;
end;

procedure TdxFcConnection.AssignAttributes(ASource: TdxFcItem);
var
  AConnection: TdxFcConnection absolute ASource;
begin
  inherited AssignAttributes(ASource);
  if ASource is TdxFcConnection then
  begin
    ArrowSource := AConnection.ArrowSource;
    ArrowDest := AConnection.ArrowDest;
    Color := AConnection.Color;
    Transparent := AConnection.Transparent;
    PenStyle := AConnection.PenStyle;
    PenWidth := AConnection.PenWidth;
    Style := AConnection.Style;
  end;
end;

procedure TdxFcConnection.AssignGeometry(ASource: TdxFcConnection);
var
  P: TPoint;
begin
  if ASource = nil then
    Exit;
  FRealPoints.Assign(ASource.FRealPoints);
  FPoints.Assign(ASource.FPoints);
  if ASource.ObjectSource <> nil then
  begin
    P := ASource.ObjectSource.LinkedPoints[ASource.PointSource];
    ASource.Owner.UnscalePoint(P);
    FPoints[0] := P;
  end;
  if ASource.ObjectDest <> nil then
  begin
    P := ASource.ObjectDest.LinkedPoints[ASource.PointDest];
    ASource.Owner.UnscalePoint(P);
    FPoints[FPoints.Count - 1] := P;
  end;
  Calculate;
end;

procedure TdxFcConnection.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TdxFcConnection then
    with TdxFcConnection(Source) do
    begin
      if Self.PointCount > 0 then Self.RemovePoint(0);
      Self.FPoints.Clear;
      Self.FRealPoints.Clear;
      Self.SetObjectSource(ObjectSource, PointSource);
      Self.SetObjectDest(ObjectDest, PointDest);
      for I := 0 to PointCount - 1 do
        Self.AddPoint(Points[I]);
    end;
  inherited Assign(Source);
end;

procedure TdxFcConnection.OffsetRealPoints(DX, DY: Integer);
begin
  FRealPoints.OffsetPoints(DX, DY);
  ArrowSource.OffsetPoints(DX, DY);
  ArrowDest.OffsetPoints(DX, DY);
end;

function FindNearestLinkedPointIndex(const P: TPoint; AObject: TdxFcObject): Byte;
var
  AMinSquareDistance, ASquareDistance: Int64;
  AObjectCenter, ALinkedPoint: TPoint;
  I: Integer;
begin
  Result := 0;
  if AObject = nil then
    Exit;
  AMinSquareDistance := MaxInt64;
  AObjectCenter := AObject.FBoundingRect.CenterPoint;
  for I := 0 to AObject.LinkedPointCount - 1 do
  begin
    ALinkedPoint := AObject.LinkedPoints[I];
    ASquareDistance :=
        ALinkedPoint.SquareOfDistance(AObjectCenter) +
        ALinkedPoint.SquareOfDistance(P);
    if ASquareDistance < AMinSquareDistance then
    begin
      AMinSquareDistance := ASquareDistance;
      Result := I;
    end;
  end;
end;

procedure TdxFcConnection.OptimizeEndpoints;
begin
  if (ObjectSource = nil) or (ObjectDest = nil) then
    Exit;
  ConnectionChanged;
  if RealCount > 2 then
  begin
    FPointSource := FindNearestLinkedPointIndex(CalculatePrevDirectionBasedPoint(True), ObjectSource);
    FPointDest := FindNearestLinkedPointIndex(CalculatePrevDirectionBasedPoint(False), ObjectDest);
  end
  else
    AdjustObjectsPoints(FPointSource, FPointDest);
  SetObjectPoints;
  CalculateDisplayRect;
  Changed;
end;

procedure TdxFcConnection.NewPoint(X, Y: Integer);
var
  I: Integer;
  P0, P1, P2: TPoint;
begin
  I := PointIndex + 1;
  if (I <= 0) or (I >= RealCount) then
  begin
    I := GetNearestPoint(X, Y);
    if ObjectSource <> nil then Inc(I);
    if I = 0 then
      Inc(I)
    else
      if I < RealCount - 1 then
      begin
        P0 := ScreenPoint(I);
        P1 := ScreenPoint(I - 1);
        P2 := ScreenPoint(I + 1);
        Dec(P0.X, X); Dec(P0.Y, Y);
        Dec(P1.X, X); Dec(P1.Y, Y);
        Dec(P2.X, X); Dec(P2.Y, Y);
        if (P0.X * P2.X) + (P0.Y * P2.Y) < (P0.X * P1.X) + (P0.Y * P1.Y) then Inc(I);
      end;
  end;
  InternalInsertPoint(I, Owner.ChartPoint(X, Y));
  Owner.DragHelper.SetupDragConnection(Self, I);
  ConnectionChanged;
end;

function TdxFcConnection.RealStyle: TdxFclStyle;
begin
  if (Style = fclCurved) and (RealCount < 3) then
    Result := fclStraight
  else
    Result := Style;
end;

procedure TdxFcConnection.DrawContent(ACanvas: TcxCanvas);
var
  AGraphics: TdxGPGraphics;
begin
  OffsetRealPoints(-Owner.LeftEdge, -Owner.TopEdge);
  try
    AGraphics := dxGpBeginPaint(ACanvas.Handle, Owner.ClientBounds);
    AGraphics.SaveClipRegion;
    try
      if Owner.Antialiasing then
        AGraphics.SmoothingMode := TdxGPSmoothingMode.smAntiAlias
      else
        AGraphics.SmoothingMode := TdxGPSmoothingMode.smNone;
      ArrowSource.Paint(AGraphics);
      ArrowDest.Paint(AGraphics);
      DrawLine(AGraphics);
    finally
      AGraphics.RestoreClipRegion;
      dxGpEndPaint(AGraphics);
    end;
  finally
    OffsetRealPoints(Owner.LeftEdge, Owner.TopEdge);
  end;
end;

function TdxFcConnection.GetActualPenWidth: Single;
begin
  Result := Owner.PaintScaleFactor.ApplyF(PenWidth);
  if Selected and Owner.HasSelection then
  begin
    if Result < 2 then
      Result := 2
    else
      Result := Result + 2;
  end;
  Result := Max(1, Result);
end;

procedure TdxFcConnection.DrawLine(AGraphics: TdxGPGraphics);
var
  P: TPoints;
begin
  FRealPoints.CalculatePolyline(TLineType(RealStyle), P);
  Owner.Painter.DrawPolyline(AGraphics, P, Color, PenStyle, GetActualPenWidth, Owner.PaintScaleFactor.ApplyF(1));
  P := nil;
end;

procedure TdxFcConnection.DrawText(ACanvas: TcxCanvas);
const
  TextIndent = 1;
var
  R: TRect;
begin
  if Text <> '' then
  begin
    R := RealTextRect;
    R.Inflate(TextIndent, 0);
    R.Offset(0, -1);
    ACanvas.FillRect(R, Owner.BackgroundColor);
    cxTextOut(ACanvas.Handle, Text, R, CXTO_CENTER_HORIZONTALLY or CXTO_CENTER_VERTICALLY, RealFont, 0, TextIndent, TextIndent);
  end;
end;

procedure TdxFcConnection.Paint(ACanvas: TcxCanvas; Upper: Boolean);
begin
  if Visible then
    if Transparent xor Upper then
    begin
      DrawContent(ACanvas);
      DrawText(ACanvas);
    end;
end;

function TdxFcConnection.CanRoute: Boolean;
begin
  Result := not Owner.IsDestroying and not Owner.DragHelper.IsDraggingConnection(Self) and (FPoints.Count >= 2);
end;

procedure TdxFcConnection.GetStartEndPoints(out AStart, AEnd: TPoint);
begin
  AStart := RealPoints[0];
  AEnd := RealPoints[FRealPoints.Count - 1];
end;

procedure TdxFcConnection.ReRoute;
begin
  if not CanRoute then
    Exit;
  ShowHourglassCursor;
  try
    InitializeEndpointsForAutorouting;
    Route;
    ConnectionChanged;
  finally
    HideHourglassCursor;
  end;
end;

procedure TdxFcConnection.Route;
var
  I: Integer;
  APoints: TArray<TPoint>;
  AStart, AEnd: TPoint;
begin
  GetStartEndPoints(AStart, AEnd);
  FRealPoints[0] := AStart;
  FRealPoints[1] := AEnd;
  APoints := Owner.RoutingStrategy.RouteConnection(Self, AStart, AEnd);
  for I := Low(APoints) to High(APoints) do
    InsertDisplayPoint(I + 1, APoints[I]);
  CalculateDisplayRect;
end;

procedure TdxFcConnection.Load(Stream: TStream; AIsUnicode: Boolean);
var
  P: TPoint;
  ConData: TdxFcConData;
begin
  Stream.ReadBuffer(ConData, SizeOf(ConData));
  Style := ConData.Style;
  Color := ConData.Color;
  ParentFont := ConData.ParFont;
  Transparent := ConData.PtDst and $80 <> 0;
  PenStyle := TPenStyle(ConData.PtSrc shr 4);
  ConData.PtDst := ConData.PtDst and $7F;
  ConData.PtSrc := ConData.PtSrc and $0F;
  SetObjectSource(Owner.Objects[ConData.ObjSrc], ConData.PtSrc);
  SetObjectDest(Owner.Objects[ConData.ObjDst], ConData.PtDst);
  while ConData.PtCount > 0 do
  begin
    Stream.ReadBuffer(P, SizeOf(P));
    P := Owner.ScaleFactor.Apply(P);
    InternalInsertPoint(PointCount + Ord(ObjectSource <> nil), P);
    Dec(ConData.PtCount);
  end;
  ArrowSource.Load(ConData.ArwSrc);
  ArrowDest.Load(ConData.ArwDst);
  SetObjectPoints;
  CalculateDisplayRect;
  CheckRouting;
  LoadFont(Stream, AIsUnicode);
  Text := ReadStr(Stream, AIsUnicode);
end;

procedure TdxFcConnection.Load(ANode: TdxXMLNode);
var
  P: TPoint;
  AAttributes: TdxXMLNodeAttributes;
  AFontNode, APointsNode, APointNode: TdxXMLNode;
begin
  AAttributes := ANode.Attributes;
  Style := TXMLFormatHelper.ReadConnectionStyle(AAttributes);
  Color := TXMLFormatHelper.ReadColor(AAttributes, 'Color');
  Transparent := AAttributes.GetValueAsBoolean('Transparent');
  PenStyle := TPenStyle(AAttributes.GetValueAsInteger('PenStyle'));
  PenWidth := AAttributes.GetValueAsInteger('PenWidth', DefaultPenWidth);
  SetObjectSource(Owner.Objects[AAttributes.GetValueAsInteger('Source')], AAttributes.GetValueAsInteger('SourcePoint'));
  SetObjectDest(Owner.Objects[AAttributes.GetValueAsInteger('Destination')], AAttributes.GetValueAsInteger('DestinationPoint'));
  APointsNode := ANode.FindChild('Points');
  if APointsNode <> nil then
  begin
    APointNode := APointsNode.First;
    while APointNode <> nil do
    begin
      P.X := APointNode.Attributes.GetValueAsInteger('X');
      P.Y := APointNode.Attributes.GetValueAsInteger('Y');
      P := Owner.ScaleFactor.Apply(P);
      InternalInsertPoint(PointCount + Ord(ObjectSource <> nil), P);
      APointNode := APointNode.Next;
    end;
  end;
  ArrowSource.Load(ANode.FindChild('ArrowSource'));
  ArrowDest.Load(ANode.FindChild('ArrowDestination'));
  SetObjectPoints;
  CalculateDisplayRect;
  CheckRouting;
  AFontNode := ANode.FindChild('Font');
  ParentFont := AFontNode = nil;
  LoadFont(AFontNode);
  Text := AAttributes.GetValueAsString('Text');
end;

procedure TdxFcConnection.Move(DX, DY: Integer; const ARealOffset: TPoint);

  function IsObjectSelected(AObject: TdxFcObject): Boolean;
  begin
    Result := (AObject <> nil) and AObject.Selected;
  end;

begin
  if IsObjectSelected(ObjectSource) or IsObjectSelected(ObjectDest) then
  begin
    ConnectionChanged;
    if IsObjectSelected(ObjectSource) and IsObjectSelected(ObjectDest) then
    begin
      FPoints.OffsetPoints(DX, DY);
      FRealPoints.OffsetPoints(ARealOffset.X, ARealOffset.Y);
    end;
    SetObjectPoints;
    CalculateDisplayRect;
    Changed;
  end;
end;

procedure TdxFcConnection.Save(Stream: TStream);

  function GetZOrder(AObject: TdxFcObject): Integer;
  begin
    if AObject = nil then
      Result := -1
    else
      Result := AObject.ZOrder;
  end;

var
  I: Integer;
  P: TPoint;
  ConData: TdxFcConData;
begin
  with ConData do
  begin
    ObjSrc := GetZOrder(ObjectSource);
    ObjDst := GetZOrder(ObjectDest);

    PtCount := Word(PointCount);
    Color := Self.Color;
    PtSrc := FPointSource;
    PtDst := FPointDest;
    Style := Self.Style;
    ParFont := ParentFont;
    PtSrc := PtSrc or Ord(PenStyle) shl 4;
    if Transparent then
      PtDst := PtDst or $80;
    ArrowSource.Save(ArwSrc);
    ArrowDest.Save(ArwDst);
  end;
  Stream.WriteBuffer(ConData, SizeOf(ConData));
  for I := 0 to PointCount - 1 do
  begin
    P := Points[I];
    P := Owner.ScaleFactor.Revert(P);
    Stream.WriteBuffer(P, SizeOf(P));
  end;
  SaveFont(Stream);
  WriteStr(Stream, Text);
end;

procedure TdxFcConnection.Save(ANode: TdxXMLNode);

  function GetZOrder(AObject: TdxFcObject): Integer;
  begin
    if AObject = nil then
      Result := -1
    else
      Result := AObject.ZOrder;
  end;

var
  I: Integer;
  P: TPoint;
  AAttributes: TdxXMLNodeAttributes;
  APointsNode, APointNode: TdxXMLNode;
begin
  AAttributes := ANode.Attributes;
  if Text <> '' then
    AAttributes.SetValueAsString('Text', Text);
  TXMLFormatHelper.WriteConnectionStyle(AAttributes, Style);
  TXMLFormatHelper.WriteColor(AAttributes, 'Color', Color);
  if Transparent <> DefaultTransparent then
    AAttributes.SetValueAsBoolean('Transparent', Transparent);
  if PenStyle <> DefaultPenStyle then
    AAttributes.SetValueAsInteger('PenStyle', Ord(PenStyle));
  if PenWidth <> DefaultPenWidth then
    AAttributes.SetValueAsInteger('PenWidth', PenWidth);
  AAttributes.SetValueAsInteger('Source', GetZOrder(ObjectSource));
  AAttributes.SetValueAsInteger('SourcePoint', FPointSource);
  AAttributes.SetValueAsInteger('Destination', GetZOrder(ObjectDest));
  AAttributes.SetValueAsInteger('DestinationPoint', FPointDest);
  if PointCount > 0 then
  begin
    APointsNode := ANode.AddChild('Points');
    for I := 0 to PointCount - 1 do
    begin
      P := Owner.ScaleFactor.Revert(Points[I]);
      APointNode := APointsNode.AddChild('Point');
      APointNode.Attributes.SetValueAsInteger('X', P.X);
      APointNode.Attributes.SetValueAsInteger('Y', P.Y);
    end;
  end;
  ArrowSource.Save(ANode.AddChild('ArrowSource'));
  ArrowDest.Save(ANode.AddChild('ArrowDestination'));
  if not ParentFont then
    SaveFont(ANode.AddChild('Font'));
end;


{ TdxDiagramRoutingStrategy }

constructor TdxDiagramRoutingStrategy.Create(AChart: TdxCustomFlowChart; AItemMargin: Integer);
begin
  FChart := AChart;
  FItemMargin := AItemMargin;
end;

{ TdxFcSelection }

constructor TdxFcSelection.Create(AOwner: TdxCustomFlowChart);
begin
  inherited Create;
  FOwner := AOwner;
  FGuides := TdxGuidesViewData.Create(AOwner);
  CreateGraphicTools;
  UpdateSizes;
end;

destructor TdxFcSelection.Destroy;
begin
  FGuides.Free;
  DestroyGraphicTools;
  inherited Destroy;
end;

function TdxFcSelection.CanResize: Boolean;
begin
  Result := fcoCanDrag in Owner.Options;
end;

function TdxFcSelection.CanRotate: Boolean;
begin
  Result := fcoCanRotate in Owner.Options;
end;

function TdxFcSelection.ConnectionPointToDrawPoint(AConnection: TdxFcConnection; AIndex: Integer): TPoint;
begin
  Result := Owner.ScrollPoint(AConnection.FRealPoints[AIndex], False);
end;

procedure TdxFcSelection.CreateGraphicTools;
begin
  FConnectionPointBrush := TdxGPBrush.Create;
  FConnectionPointBrush.Color := TdxAlphaColors.DodgerBlue;
  FResizingMarkPen := TdxGPPen.Create(TdxAlphaColors.Gray, Owner.ScaleFactor.ApplyF(2));
  FResizingMarkBrush := TdxGPBrush.Create;
  FResizingMarkBrush.Color := TdxAlphaColors.White;
  FConnectionPointPen := TdxGPPen.Create(TdxAlphaColors.White, Owner.ScaleFactor.ApplyF(1.5));
  if CanResize then
    FSelectionFramePen := TdxGPPen.Create(TdxAlphaColors.Gray, Owner.ScaleFactor.ApplyF(1))
  else
    FSelectionFramePen := TdxGPPen.Create(TdxAlphaColors.FromArgb($24, $40,  $8F), Owner.ScaleFactor.ApplyF(2), psDash);
end;

procedure TdxFcSelection.DestroyGraphicTools;
begin
  FreeAndNil(FConnectionPointBrush);
  FreeAndNil(FSelectionFramePen);
  FreeAndNil(FResizingMarkPen);
  FreeAndNil(FResizingMarkBrush);
  FreeAndNil(FConnectionPointPen);
end;

function TdxFcSelection.GetPaintMode: TPaintMode;
var
  ACount: Integer;
begin
  ACount := Owner.SelectedObjectCount + Owner.SelectedConnectionCount;
  case ACount of
    0: Result := TPaintMode.None;
    1:
      begin
        if Owner.SelectedObjectCount = 0 then
          Result := TPaintMode.Connection
        else
          Result := TPaintMode.&Object;
      end
  else
    Result := TPaintMode.MultiSelection;
  end;
end;

function TdxFcSelection.ShowEndpointsMarks: Boolean;
begin
  Result := True;
end;

procedure TdxFcSelection.Paint;
var
  AGraphics: TdxGPGraphics;
begin
  if PaintMode = TPaintMode.None then
    Exit;
  AGraphics := dxGpBeginPaint(Owner.Canvas.Handle, Owner.ClientBounds);
  try
    AGraphics.SmoothingMode := smAntiAlias;
    case PaintMode of
      TdxFcSelection.TPaintMode.Connection:
        DrawConnectionSelection(AGraphics, Owner.SelectedConnections[0]);
      TdxFcSelection.TPaintMode.Object:
        DrawObjectSelection(AGraphics, Owner.SelectedObjects[0]);
      TdxFcSelection.TPaintMode.MultiSelection: ;
    end;
  finally
    dxGpEndPaint(AGraphics);
  end;
end;

procedure TdxFcSelection.DrawObjectBoundingRect(AGraphics: TdxGPGraphics; ABounds: TRect);
var
  APen: TdxGPPen;
begin
  AGraphics.SmoothingMode := smAntiAlias;
  APen := TdxGPPen.Create(TdxAlphaColors.MediumSlateBlue, 1, psDash);
  try
    Inc(ABounds.Right);
    Inc(ABounds.Bottom);
    AGraphics.Rectangle(ABounds, APen, nil);
  finally
    APen.Free;
  end;
end;

procedure TdxFcSelection.DrawConnectionEndPointMark(AGraphics: TdxGPGraphics; const P: TPoint);
var
  R: TdxRectF;
begin
  DrawResizingMark(AGraphics, P);
  if not Owner.CanDragConnectionEndpoints then
    Exit;
  R.Left := P.X - EndpointInnerRadius;
  R.Top := P.Y - EndpointInnerRadius;
  R.Right := P.X + EndpointInnerRadius;
  R.Bottom := P.Y + EndpointInnerRadius;
  AGraphics.Ellipse(R, nil, ConnectionPointBrush);
end;

procedure TdxFcSelection.DrawConnectionIntermediatePointMark(AGraphics: TdxGPGraphics; const P: TPoint);
var
  R: TdxRectF;
begin
  R.Left := P.X - ConnectionPointRadius;
  R.Top := P.Y - ConnectionPointRadius;
  R.Right := P.X + ConnectionPointRadius;
  R.Bottom := P.Y + ConnectionPointRadius;
  AGraphics.Ellipse(R, ConnectionPointPen, ConnectionPointBrush);
end;

procedure TdxFcSelection.DrawConnectionSelection(AGraphics: TdxGPGraphics; AConnection: TdxFcConnection);
var
  I: Integer;
begin
  if ShowEndpointsMarks then
  begin
    DrawConnectionEndPointMark(AGraphics, ConnectionPointToDrawPoint(AConnection, 0));
    DrawConnectionEndPointMark(AGraphics, ConnectionPointToDrawPoint(AConnection, AConnection.RealCount - 1));
  end;
  if Owner.CanDragConnectionIntermediatePoints then
  begin
    for I := 1 to AConnection.RealCount - 2 do
      DrawConnectionIntermediatePointMark(AGraphics, ConnectionPointToDrawPoint(AConnection, I));
  end;
end;

procedure TdxFcSelection.DrawEqualityArrows(AGraphics: TdxGPGraphics; const ASize: TSize; const ABounds: TRect;
  APen: TdxGPPen; ABrush: TdxGPBrush);
var
  A: array[0..2] of TPoint;
  X, Y: Integer;
begin
  if (ASize.cx <> ABounds.Width) and (ASize.cy <> ABounds.Height) then
    Exit;

  if ASize.cx = ABounds.Width then
  begin
    Y := ABounds.Bottom + 8 + 8;
    A[0].Init(ABounds.Left + 1, Y);
    A[1].Init(ABounds.Left + 6, Y - 2);
    A[2].Init(ABounds.Left + 6, Y + 2);
    AGraphics.Polygon(A, APen, ABrush);
    AGraphics.Line(ABounds.Left + 6, Y, ABounds.Right - 6, Y, APen);
    A[0].Init(ABounds.Right - 1, Y);
    A[1].Init(ABounds.Right - 6, Y - 2);
    A[2].Init(ABounds.Right - 6, Y + 2);
    AGraphics.Polygon(A, APen, ABrush);
    AGraphics.Line(ABounds.Left, Y - 8, ABounds.Left, Y + 8, APen);
    AGraphics.Line(ABounds.Right, Y - 8, ABounds.Right, Y + 8, APen);
    FHasEqualWidth := True;
  end;
  if ASize.cy = ABounds.Height then
  begin
    X := ABounds.Right + 8 + 8;
    A[0].Init(X, ABounds.Top + 1);
    A[1].Init(X - 2, ABounds.Top + 6);
    A[2].Init(X + 2, ABounds.Top + 6);
    AGraphics.Polygon(A, APen, ABrush);
    AGraphics.Line(X, ABounds.Top + 6, X, ABounds.Bottom - 6, APen);
    A[0].Init(X, ABounds.Bottom - 1);
    A[1].Init(X - 2, ABounds.Bottom - 6);
    A[2].Init(X + 2, ABounds.Bottom - 6);
    AGraphics.Polygon(A, APen, ABrush);
    AGraphics.Line(X - 8, ABounds.Top, X + 8, ABounds.Top, APen);
    AGraphics.Line(X - 8, ABounds.Bottom, X + 8, ABounds.Bottom, APen);
    FHasEqualHeight := True;
  end;
end;

procedure TdxFcSelection.DrawEqualitySizes(AGraphics: TdxGPGraphics; ASize: TSize; ABounds: TList<TRect>);
var
  I: Integer;
  ABrush: TdxGPBrush;
  APen: TdxGPPen;
begin
  AGraphics.SmoothingMode := smAntiAlias;
  APen := TdxGPPen.Create(TdxAlphaColors.LimeGreen, 1, psSolid);
  ABrush := TdxGPBrush.Create;
  ABrush.Color := TdxAlphaColors.LimeGreen;
  try
    FHasEqualWidth := False;
    FHasEqualHeight := False;
    for I := 0 to ABounds.Count - 1 do
      DrawEqualityArrows(AGraphics, ASize, ABounds[I], APen, ABrush);
    if FHasEqualWidth or FHasEqualHeight then
    begin
      if not FHasEqualWidth then
        ASize.cx := MinInt;
      if not FHasEqualHeight then
        ASize.cy := MinInt;
      DrawEqualityArrows(AGraphics, ASize, Guides.EqualityBounds, APen, ABrush);
    end;
  finally
    ABrush.Free;
    APen.Free;
  end;
end;

procedure TdxFcSelection.DrawGuides(AGraphics: TdxGPGraphics);
var
  APen: TdxGPPen;
  I: Integer;
begin
  if Guides.Empty then
    Exit;
  if Guides.CheckSizes then
    DrawEqualitySizes(AGraphics, Guides.EqualitySize, Guides.Controller.Bounds)
  else
    DrawObjectBoundingRect(AGraphics, Guides.EqualityBounds);
  APen := TdxGPPen.Create(TdxAlphaColors.LimeGreen, 1, psDash);
  try
    for I := Low(Guides.GuideBounds) to High(Guides.GuideBounds) do
      with Guides.GuideBounds[I] do
        AGraphics.Line(Left, Top, Right, Bottom, APen);
  finally
    APen.Free;
  end;
end;

procedure TdxFcSelection.AdjustObjectSelectionBounds(AObject: TdxFcObject; var ABounds: TRect);
begin
  if not dxFlowChartSizeStandardObjectsAsAdvanced and (AObject.ShapeType <> fcsAdvanced) then
  begin
    Dec(ABounds.Right, 2);
    Dec(ABounds.Bottom, 2);
  end;
end;

procedure TdxFcSelection.DrawObjectSelection(AGraphics: TdxGPGraphics; AObject: TdxFcObject);
var
  ABounds: TRect;
  ARotationMark: TPoint;
begin
  AGraphics.SaveWorldTransform;
  try
    ABounds := AObject.DisplayRect;
    UpdateGraphicsForRotatedBounds(AGraphics, ABounds, AObject.Angle);
    AdjustObjectSelectionBounds(AObject, ABounds);
    DrawSelectionFrame(AGraphics, ABounds);
    if CanRotate and AObject.CanRotate then
    begin
      ARotationMark.Init(ABounds.Left + ABounds.Width div 2, Round(ABounds.Top - RotationMarkOffset));
      DrawRotationMark(AGraphics, ARotationMark);
    end;
    if AObject.CanUseParameters then
      AObject.AdvancedShapeInfo.PaintParameters(AGraphics);
  finally
    AGraphics.RestoreWorldTransform;
    DrawGuides(AGraphics);
  end;
end;

procedure TdxFcSelection.DrawResizingMark(AGraphics: TdxGPGraphics; X, Y: Integer);
var
  R: TdxRectF;
begin
  R.Left := X - ResizingMarkRadius;
  R.Top := Y - ResizingMarkRadius;
  R.Right := X + ResizingMarkRadius;
  R.Bottom := Y + ResizingMarkRadius;
  AGraphics.Ellipse(R, ResizingMarkPen, ResizingMarkBrush);
end;

procedure TdxFcSelection.DrawRotationMark(AGraphics: TdxGPGraphics; const ACenterPoint: TPoint);
var
  R: TdxRectF;
  AOffset: Single;
begin
  AOffset := RotationMarkSize / 2;
  AGraphics.Line(
    ACenterPoint.X, ACenterPoint.Y + Ceil(AOffset * 0.75),
    ACenterPoint.X, ACenterPoint.Y + (RotationMarkOffset - ResizingMarkRadius),
    TdxAlphaColors.Gray);
  R.Left := ACenterPoint.X - AOffset;
  R.Top := ACenterPoint.Y - AOffset;
  R.Right := ACenterPoint.X + AOffset;
  R.Bottom := ACenterPoint.Y + AOffset;
  AGraphics.Draw(RotationImage, R);
end;

procedure TdxFcSelection.DrawResizingMark(AGraphics: TdxGPGraphics; const P: TPoint);
begin
  DrawResizingMark(AGraphics, P.X, P.Y);
end;

procedure TdxFcSelection.DrawSelectionFrame(AGraphics: TdxGPGraphics; const ABounds: TRect);
begin
  AGraphics.Rectangle(ABounds.ToRectF, SelectionFramePen, nil);
  if not CanResize then
    Exit;
  if Owner.CanResizeObjectsVertical then
  begin
    DrawResizingMark(AGraphics, ABounds.Left + ABounds.Width div 2, ABounds.Top);
    DrawResizingMark(AGraphics, ABounds.Left + ABounds.Width div 2, ABounds.Bottom);
  end;
  if Owner.CanResizeObjectsHorizontal then
  begin
    DrawResizingMark(AGraphics, ABounds.Left, ABounds.Top + ABounds.Height div 2);
    DrawResizingMark(AGraphics, ABounds.Right, ABounds.Top + ABounds.Height div 2);
  end;
  if (ABounds.Width > 4 * ResizingMarkRadius) and (ABounds.Height > 4 * ResizingMarkRadius) then
  begin
    DrawResizingMark(AGraphics, ABounds.TopLeft);
    DrawResizingMark(AGraphics, ABounds.Right, ABounds.Top);
    DrawResizingMark(AGraphics, ABounds.Left, ABounds.Bottom);
    DrawResizingMark(AGraphics, ABounds.BottomRight);
  end;
end;

procedure TdxFcSelection.Prepare;
var
  I: Integer;
  AObject: TdxFcObject;
  AHasSelection: Boolean;
begin
  FPaintMode := GetPaintMode;
  AHasSelection := Owner.HasSelection;
  for I := 0 to Owner.ObjectCount - 1 do
  begin
    AObject := Owner.Objects[I];
    if not AHasSelection then
      AObject.HighlightedFrameMode := TdxFcObject.THighlightedFrameMode.None
    else
    begin
      if AObject.HighlightedFrameMode <> TdxFcObject.THighlightedFrameMode.Connect then
        if (PaintMode = TPaintMode.MultiSelection) and AObject.Selected then
          AObject.HighlightedFrameMode := TdxFcObject.THighlightedFrameMode.Selected
        else
          AObject.HighlightedFrameMode := TdxFcObject.THighlightedFrameMode.None;
    end;
  end;
end;

procedure TdxFcSelection.UpdateGraphicTools;
begin
  DestroyGraphicTools;
  CreateGraphicTools;
  UpdateSizes;
end;

procedure TdxFcSelection.UpdateSizes;
begin
  FConnectionPointRadius := Owner.ScaleFactor.ApplyF(3.0);
  FEndpointInnerRadius := Owner.ScaleFactor.ApplyF(2.0);
  FResizingMarkRadius := Owner.ScaleFactor.ApplyF(5.0);
  FSelectionIndent := Owner.ScaleFactor.ApplyF(8.0);
  FRotationMarkOffset := Owner.ScaleFactor.ApplyF(24.0);
  FRotationMarkSize := Owner.ScaleFactor.ApplyF(26.0);
end;

class procedure TdxFcSelection.InitializeImages;
var
  AStream: TResourceStream;
begin
  AStream := TResourceStream.Create(HInstance, 'RotateIcon', RT_RCDATA);
  try
    FRotationImage := TdxSmartImage.CreateFromStream(AStream);
  finally
    AStream.Free;
  end;
end;


{ TdxFcItem }

constructor TdxFcItem.Create(AOwner: TdxCustomFlowChart);
begin
  inherited Create;
  FOwner := AOwner;
  FFont := TFont.Create;
  FRealFont := TFont.Create;
  SyncFontWithParentFont;
  CalculateRealFont;
  FFont.OnChange := FontChangeHandler;
end;

destructor TdxFcItem.Destroy;
begin
  FreeAndNil(FFont);
  FreeAndNil(FRealFont);
  inherited Destroy;
end;

procedure TdxFcItem.Changed;
begin
  if not Owner.FLoading then
    Owner.Changed(Self);
end;

procedure TdxFcItem.ChangeScale(M, D: Integer);
begin
  if not ParentFont then
    FFont.Height := MulDiv(FFont.Height, M, D);
end;

procedure TdxFcItem.FontChangeHandler(Sender: TObject);
begin
  FParentFont := False;
  CalculateRealFont;
  if Text <> '' then
    FontChanged;
  Changed;
end;

function TdxFcItem.GetHighlighted: Boolean;
begin
  Result := isHighlighted in FState;
end;

function TdxFcItem.GetSelected: Boolean;
begin
  Result := isSelected in FState;
end;

procedure TdxFcItem.ScaleFont;
begin
  RealFont.Height := MulDiv(Font.Height, Owner.RealZoom, 100);
  if RealFont.Height = 0 then
    RealFont.Height := Sign(Font.Height);
end;

procedure TdxFcItem.AssignAttributes(ASource: TdxFcItem);
begin
  Data := ASource.Data;
  Text := ASource.Text;
  ParentFont := ASource.ParentFont;
  if not ParentFont then
    dxAssignFont(Font, ASource.Font, Owner.ScaleFactor, ASource.Owner.ScaleFactor);
end;

procedure TdxFcItem.CalculateRealFont;
begin
  RealFont.Assign(Font);
  ScaleFont;
end;

procedure TdxFcItem.SetFont(Value: TFont);
begin
  Font.Assign(Value);
end;

procedure TdxFcItem.SetHighlightedState(AHighlighted: Boolean);
begin
  if AHighlighted then
    Include(FState, isHighlighted)
  else
    Exclude(FState, isHighlighted);
end;

procedure TdxFcItem.FontChanged;
begin
  Invalidate;
end;

function TdxFcItem.GetSelectionRect: TRect;
begin
  Result := DisplayRect;
  ExtSelRect(Result, True, Owner.ScaleFactor);
end;

function TdxFcItem.GetInvalidateRect: TRect;
begin
  Result := SelectionRect;
end;

procedure TdxFcItem.SetParentFont(Value: Boolean);
begin
  if Value <> ParentFont then
  begin
    if Value then
      SyncFontWithParentFont;
    FParentFont := Value;
    Changed;
  end;
end;

procedure TdxFcItem.SetHighlighted(Value: Boolean);
begin
  if Value <> Highlighted then
  begin
    SetHighlightedState(Value);
    Invalidate;
  end;
end;

procedure TdxFcItem.SetSelected(Value: Boolean);
var
  AMultiselection: Boolean;
begin
  if Selected <> Value then
  begin
    if Value and not Owner.CanSelect(Self) then
      Exit;
    AMultiselection := Owner.HasMultiSelection;
    if Selected then
    begin
      Invalidate;
      SelList.Remove(Self);
    end;
    SetSelectedState(Value);
    if Selected then
    begin
      if not (fcoMultiSelect in Owner.Options) then
        Owner.ClearSelection;
      SelList.Add(Self);
      Invalidate;
    end;
    Owner.Select(Self);
    if AMultiselection <> Owner.HasMultiSelection then
      Owner.Invalidate;
  end;
end;

procedure TdxFcItem.SetSelectedState(ASelected: Boolean);
begin
  if ASelected then
    Include(FState, isSelected)
  else
    Exclude(FState, isSelected);
end;

procedure TdxFcItem.SyncFontWithParentFont;
begin
  Font.Assign(Owner.Font);
  FParentFont := True;
end;

procedure TdxFcItem.Assign(Source: TPersistent);
begin
  if Source is TdxFcItem then
    AssignAttributes(TdxFcItem(Source))
  else
    inherited Assign(Source);
end;

procedure TdxFcItem.LoadFont(Stream: TStream; AIsUnicode: Boolean);
var
  AFontData: TdxFcFntData;
  AFontName: string;
begin
  if ParentFont then Exit;

  Stream.ReadBuffer(AFontData, SizeOf(AFontData));
  AFontName := ReadStr(Stream, AIsUnicode);
  Font.OnChange := nil;
  try
    Font.Height := Owner.ScaleFactor.Apply(AFontData.Height);
    Font.Color := AFontData.Color;
    Font.Pitch := AFontData.Pitch;
    Font.Style := AFontData.Style;
    Font.Charset := AFontData.Charset;
    if AFontName <> '' then
      Font.Name := AFontName;
  finally
    Font.OnChange := FontChangeHandler;
  end;
  CalculateRealFont;
end;

procedure TdxFcItem.LoadFont(ANode: TdxXMLNode);
var
  ASaveChangeEvent: TNotifyEvent;
  AAttributes: TdxXMLNodeAttributes;
begin
  if ParentFont then Exit;
  AAttributes := ANode.Attributes;
  ASaveChangeEvent := Font.OnChange;
  try
    Font.OnChange := nil;
    if AAttributes.Exists('Name') then
      Font.Name := AAttributes.GetValueAsString('Name');
    Font.Height := Owner.ScaleFactor.Apply(AAttributes.GetValueAsInteger('Height'));
    Font.Color := TXMLFormatHelper.ReadColor(AAttributes, 'Color');
    Font.Pitch := TFontPitch(AAttributes.GetValueAsInteger('Pitch'));
    Font.Style := TFontStyles(Byte(AAttributes.GetValueAsInteger('Style')));;
    Font.Charset := AAttributes.GetValueAsInteger('Charset') and $FF;
  finally
    Font.OnChange := ASaveChangeEvent;
  end;
  CalculateRealFont;
end;

procedure TdxFcItem.Click;
begin
end;

procedure TdxFcItem.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TdxFcItem.MouseLeave(AControl: TControl);
begin
end;

procedure TdxFcItem.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TdxFcItem.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TdxFcItem.SaveFont(Stream: TStream);
var
  AFontData: TdxFcFntData;
  AFontName: string;
begin
  if ParentFont then Exit;
  if Font.Name = Owner.Font.Name then
    AFontName := ''
  else
    AFontName := Font.Name;
  AFontData.Height := Owner.ScaleFactor.Revert(Font.Height);
  AFontData.Color := Font.Color;
  AFontData.Pitch := Font.Pitch;
  AFontData.Style := Font.Style;
  AFontData.Charset := Font.Charset;
  Stream.WriteBuffer(AFontData, SizeOf(AFontData));
  WriteStr(Stream, AFontName);
end;

procedure TdxFcItem.SaveFont(ANode: TdxXMLNode);
var
  AAttributes: TdxXMLNodeAttributes;
begin
  if ParentFont then Exit;
  AAttributes := ANode.Attributes;
  if not SameText(Font.Name, Owner.Font.Name) then
    AAttributes.SetValueAsString('Name', Font.Name);
  AAttributes.SetValueAsInteger('Height', Owner.ScaleFactor.Revert(Font.Height));
  TXMLFormatHelper.WriteColor(AAttributes, 'Color', Font.Color);
  AAttributes.SetValueAsInteger('Pitch', Ord(Font.Pitch));
  AAttributes.SetValueAsInteger('Style', Byte(Font.Style));
  AAttributes.SetValueAsInteger('Charset', Font.Charset);
end;

{ TdxFcObject.TAdvancedShapeInfo }

constructor TdxFcObject.TAdvancedShapeInfo.Create(AOwner: TdxFcObject);
var
  I: Integer;
begin
  FOwner := AOwner;
  FShapeDescription := AOwner.AdvancedShape.ShapeDescription;
  FParameters := ShapeDescription.GetParameters(ShapeDescription.DefaultSize, nil);
  SetLength(FLocations, Length(FParameters));
  SetLength(FPaintLocations, Length(FParameters));
  for I := Low(FParameters) to High(FParameters) do
    FLocations[I] := ShapeDescription.GetParameterPoint(I, ShapeDescription.DefaultSize, FParameters, FParameters[I]);
  FViewData := ShapeDescription.CreateViewData(ShapeDescription.DefaultSize, FParameters);
  FConnectionPoints := ShapeDescription.GetConnectionPoints(ShapeDescription.DefaultSize, FParameters);
  SetLength(AOwner.LinkedPoints, Length(FConnectionPoints));
end;

destructor TdxFcObject.TAdvancedShapeInfo.Destroy;
begin
  FViewData.Free;
  inherited Destroy;
end;

procedure TdxFcObject.TAdvancedShapeInfo.CreateViewData(const ASize: TdxSizeF; const AParameters: TArray<Single>);
begin
  FViewData.Free;
  FViewData := ShapeDescription.CreateViewData(ASize, AParameters);
end;

procedure TdxFcObject.TAdvancedShapeInfo.CheckSize(AWidth, AHeight: Integer);
begin
  if (FLastSize.cx = AWidth) and (FLastSize.cy = AHeight) then
    Exit;
  Update(AWidth, AHeight)
end;

procedure TdxFcObject.TAdvancedShapeInfo.Update;
begin
  Update(Owner.Width, Owner.Height);
end;

procedure TdxFcObject.TAdvancedShapeInfo.Update(AWidth, AHeight: Integer);
var
  I: Integer;
  ASize: TdxSizeF;
  ANormalizedParameters: TArray<Single>;
begin
  FLastSize.Init(AWidth, AHeight);
  ASize.Init(AWidth, AHeight);
  ANormalizedParameters := ShapeDescription.GetNormalizedParameters(ASize, FParameters);
  for I := Low(FLocations) to High(FLocations) do
    FLocations[I] := ShapeDescription.GetParameterPoint(I, ASize, FParameters, ANormalizedParameters[I]);
  FConnectionPoints := ShapeDescription.GetConnectionPoints(ASize, ANormalizedParameters);
  CreateViewData(ASize, ANormalizedParameters);
end;

procedure TdxFcObject.TAdvancedShapeInfo.ReadParameters(AAttributes: TdxXMLNodeAttributes);
var
  AValue: string;
  AValues: TArray<string>;
  I: Integer;
begin
  AValue := AAttributes.GetValueAsString('Parameters');
  if AValue = '' then
    Exit;
  AValues := TdxStringHelper.Split(AValue, [' ']);
  if Length(AValues) <> Length(FParameters) then
    Exit;
  for I := Low(AValues) to High(AValues) do
    FParameters[I] := StrToFloatDef(AValues[I], 0);
  Update;
end;

procedure TdxFcObject.TAdvancedShapeInfo.SetParameters(const Value: TArray<Single>);
begin
  Owner.Owner.NeedRepaintObject(Owner);
  SetLength(FParameters, Length(Value));
  TArray.Copy<Single>(Value, FParameters, Length(Value));
  Update;
  Owner.UpdateConnections;
  Owner.Changed;
  Owner.Owner.NeedRepaintObject(Owner);
end;

procedure TdxFcObject.TAdvancedShapeInfo.WriteParameters(AAttributes: TdxXMLNodeAttributes);
var
  AValue: string;
  I: Integer;
begin
  if (Length(FParameters) = 0) or IsDefaultParameters then
    Exit;
  AValue := FloatToStr(FParameters[0]);
  for I := 1 to High(FParameters) do
    AValue := AValue + ' ' + FloatToStr(FParameters[I]);
  AAttributes.SetValueAsString('Parameters', AValue);
end;

function TdxFcObject.TAdvancedShapeInfo.GetBoundingRect: TRect;
var
  AHalfWidth, AHalfHeight, AScaleFactor: Single;
  ABounds: TRect;
  AMatrix: TdxGPMatrix;
  ACombinedPath: TdxGPPath;
  I: Integer;
begin
  ACombinedPath := TdxGPPath.Create;
  try
    for I := 0 to ViewData.Count - 1 do
      ACombinedPath.AddPath(ViewData[I].Path);
    AMatrix := TdxGPMatrix.Create;
    try
      ABounds := Owner.DisplayRect;
      AHalfWidth := ABounds.Width / 2;
      AHalfHeight := ABounds.Height / 2;
      AMatrix.Translate(ABounds.Left + AHalfWidth, ABounds.Top + AHalfHeight);
      AMatrix.Rotate(Owner.Angle);
      AMatrix.Translate(-AHalfWidth, -AHalfHeight);
      AScaleFactor := Owner.Owner.RealZoom / 100;
      AMatrix.Scale(AScaleFactor, AScaleFactor);
      ACombinedPath.Transform(AMatrix);
      Result := TRect.Round(ACombinedPath.GetBoundsF);
    finally
      AMatrix.Free;
    end;
  finally
    ACombinedPath.Free;
  end;
end;

function TdxFcObject.TAdvancedShapeInfo.GetBrush(AIndex: Integer): TdxGPBrush;
var
  AColor: TdxAlphaColor;
  AFillColor: TdxNullableValue<TdxAlphaColor>;
begin
  if ViewData[AIndex].Transparent or Owner.Transparent then
    Exit(nil);

  Result := TdxGPBrush.Create;
  AFillColor := ViewData[AIndex].FillColor;
  if AFillColor.HasValue then
    AColor := ViewData[AIndex].FillColor
  else
    AColor := TdxAlphaColors.FromColor(Owner.BkColor);
  Result.Color := TdxAlphaColors.ChangeBrightness(AColor, ViewData[AIndex].FillBrightness);
end;

function TdxFcObject.TAdvancedShapeInfo.GetParameterIndex(const P: TPoint): Integer;
var
  I, AIndent: Integer;
  APoint: TdxPointF;
  ABounds: TRect;
  AMatrix: TdxGPMatrix;
begin
  ABounds := Owner.DisplayRect;
  if Owner.IsRotated then
  begin
    AMatrix := TdxGPMatrix.Create;
    AMatrix.Rotate(Owner.Angle, TdxPointF.Create(ABounds.Left + ABounds.Width / 2, ABounds.Top + ABounds.Height / 2));
  end
  else
    AMatrix := nil;

  try
    AIndent := Owner.Owner.ScaleFactor.Apply(4);
    for I := 0 to High(FPaintLocations) do
    begin
      APoint := FPaintLocations[I];
      if AMatrix <> nil then
        APoint := AMatrix.TransformPoint(APoint);
      ABounds := TRect.Round(TdxRectF.Create(APoint.X - AIndent, APoint.Y - AIndent, APoint.X + AIndent, APoint.Y + AIndent));
      if ABounds.Contains(P) then
        Exit(I);
    end;
    Result := -1;
  finally
    AMatrix.Free;
  end;
end;

function TdxFcObject.TAdvancedShapeInfo.GetPen(AIndex: Integer): TdxGPPen;
begin
  Result := TdxGPPen.Create(TdxAlphaColors.FromColor(Owner.ShapeColor), GetScaled(Owner.ShapeWidth), Owner.ShapeStyle);
  Result.LineJoin := ViewData[AIndex].StrokeLineJoin;
  Result.MiterLimit := 4;
end;

procedure TdxFcObject.TAdvancedShapeInfo.ChangeParameter(AIndex: Integer; const APoint: TdxPointF);
var
  AParameterValue: Single;
begin
  AParameterValue := ShapeDescription.GetParameterValue(AIndex, TdxSizeF.Create(FLastSize.cx, FLastSize.cy), FParameters, APoint);
  if not SameValue(AParameterValue, FParameters[AIndex]) then
  begin
    Owner.Owner.NeedRepaintObject(Owner);
    FParameters[AIndex] := AParameterValue;
    Update(FLastSize.cx, FLastSize.cy);
    Owner.Changed;
    Owner.Owner.NeedRepaintObject(Owner);
  end;
end;

function TdxFcObject.TAdvancedShapeInfo.IsDefaultParameters: Boolean;
var
  I: Integer;
begin
  for I := Low(FParameters) to High(FParameters) do
    if not SameValue(FParameters[I], ShapeDescription.GetParameterDefaultValue(I)) then
      Exit(False);
  Result := True;
end;

function TdxFcObject.TAdvancedShapeInfo.IsPointInsideShape(const P: TPoint): Boolean;
begin
  Result := True;
end;

procedure TdxFcObject.TAdvancedShapeInfo.Paint(AGraphics: TdxGPGraphics; const ABounds: TRect);
var
  APen: TdxGPPen;
  ABrush: TdxGPBrush;
  I: Integer;
begin
  CheckSize(Owner.Width, Owner.Height);
  for I := 0 to ViewData.Count - 1 do
  begin
    APen := GetPen(I);
    ABrush := GetBrush(I);
    try
      AGraphics.Path(ViewData[I].Path, APen, ABrush);
    finally
      ABrush.Free;
      APen.Free;
    end;
  end;
end;

procedure TdxFcObject.TAdvancedShapeInfo.CalculatePaintLocations;
var
  I: Integer;
  AOrigin: TPoint;
  AFactor: Single;
  APoint: TdxPointF;
begin
  AFactor := Owner.Owner.RealZoom / 100;
  AOrigin := Owner.DisplayRect.TopLeft;
  for I := Low(FLocations) to High(FLocations) do
  begin
    APoint := FLocations[I];
    APoint.Init(APoint.X * AFactor, APoint.Y * AFactor);
    APoint.Offset(AOrigin);
    FPaintLocations[I] := APoint;
  end;
end;

procedure TdxFcObject.TAdvancedShapeInfo.PaintParameters(AGraphics: TdxGPGraphics);

  function GetParameterRectangle(const APoint: TdxPointF): TdxRectF;
  var
    AIndent: Single;
  begin
    AIndent := Owner.Owner.ScaleFactor.Apply(4);
    Result := TdxRectF.Create(APoint.X - AIndent, APoint.Y - AIndent, APoint.X + AIndent, APoint.Y + AIndent);
  end;

var
  I: Integer;
begin
  CalculatePaintLocations;
  for I := Low(PaintLocations) to High(PaintLocations) do
    AGraphics.Rectangle(GetParameterRectangle(PaintLocations[I]),
      TdxAlphaColors.SaddleBrown,
      TdxAlphaColors.NavajoWhite,
      Owner.Owner.ScaleFactor.Apply(1));
end;

function TdxFcObject.TAdvancedShapeInfo.GetScaled(AValue: Integer): Single;
begin
  Result := Owner.Owner.PaintScaleFactor.ApplyF(AValue);
end;

function TdxFcObject.TAdvancedShapeInfo.NeedPaintParameters: Boolean;
begin
  Result := (Owner.Owner.SelectedObject = Owner) and Owner.CanUseParameters;
end;

{ TdxFcObject.TFontData }

constructor TdxFcObject.TFontData.Create(AOwner: TdxFcObject);
begin
  FOwner := AOwner;
  FFormat := TdxGPStringFormat.GenericDefault.Clone;
end;

destructor TdxFcObject.TFontData.Destroy;
begin
  FFont.Free;
  FBrush.Free;
  FFormat.Free;
  inherited Destroy;
end;

procedure TdxFcObject.TFontData.DrawText(AGraphics: TdxGPGraphics; const ABounds: TdxRectF);
var
  AIndent: Integer;
  ATextBounds: TdxRectF;
begin
  ATextBounds := ABounds;
  AIndent := Owner.RealContentIndent;
  ATextBounds.Inflate(-AIndent, -AIndent);
  if ATextBounds.IsEmpty then
    Exit;
  if Font = nil then
    Update;
  AGraphics.DrawString(Owner.Text, Font, Brush, ATextBounds, Format);
end;

procedure TdxFcObject.TFontData.Update;
var
  AStyle: TdxGPFontStyle;
begin
  FFont.Free;
  FBrush.Free;
  FBrush := TdxGPBrush.Create;
  FBrush.Color := TdxAlphaColors.FromColor(Owner.RealFont.Color);
  UpdateFontFormat;
  AStyle := TdxGPFontStyle(dxFontStylesToGpFontStyles(Owner.RealFont.Style));
  FFont := TdxGPFont.SafeCreate(Owner.RealFont.Name, Owner.Font.Size * Owner.Owner.RealZoom / 100, AStyle);
end;

procedure TdxFcObject.TFontData.UpdateFontFormat;
begin
  case Owner.HorzTextPos of
    fchpLeft: FFormat.Alignment := StringAlignmentNear;
    fchpCenter: FFormat.Alignment := StringAlignmentCenter;
    fchpRight: FFormat.Alignment := StringAlignmentFar;
  end;
  case Owner.VertTextPos of
    fcvpUp: FFormat.LineAlignment := StringAlignmentNear;
    fcvpCenter: FFormat.LineAlignment := StringAlignmentCenter;
    fcvpDown: FFormat.LineAlignment := StringAlignmentFar;
  end;
end;

{ TdxFcDrawExEventPaintData }

constructor TdxFcDrawExEventPaintData.Create(AObject: TdxFcObject; const ABounds: TRect);
begin
  FObject := AObject;
  FBounds := ABounds;
  FDefaultDrawText := True;
  FDefaultDrawImage := True;
  FImageBounds := AObject.GetAdvancedShapeImageBounds(ABounds);
end;


function TdxFcDrawExEventPaintData.GetFont: TdxGPFont;
begin
  Result := FObject.FontData.Font;
end;

function TdxFcDrawExEventPaintData.GetFontBrush: TdxGPBrush;
begin
  Result := FObject.FontData.Brush;
end;

function TdxFcDrawExEventPaintData.GetFontFormat: TdxGPStringFormat;
begin
  Result := FObject.FontData.Format;
end;

function TdxFcDrawExEventPaintData.Scale(APixels: Integer): Integer;
begin
  Result := Round(APixels * FObject.Owner.RealZoom / 100);
end;

{TdxFcObject}

constructor TdxFcObject.Create(AOwner: TdxCustomFlowChart);
begin
  inherited Create(AOwner);
  FConnections := TList.Create;
  FLinkedObjects := TList.Create;
  FObjects := TList.Create;
  FBkColor := AOwner.Color;
  FVisible := True;
  FImageIndex := DefaultImageIndex;
  FShapeWidth := DefaultShapeWidth;
  FBorder := DefaultBorderStyle;
  FHighlightedLinkedPointIndex := -1;
  SetLength(LinkedPoints, 16);
  CalculateRealShapeWidth;
  AOwner.FObjects.Add(Self);
  AOwner.Added(Self);
end;

destructor TdxFcObject.Destroy;
var
  I: Integer;
begin
  Owner.Delete(Self);
  while ConnectionCount > 0 do
    Connections[0].Free;
  FreeAndNil(FConnections);
  FreeAndNil(FLinkedObjects);
  FreeAndNil(FObjects);
  FreeAdvancedShapeInfo;

  Owner.FObjects.Remove(Self);

  with Owner do
    for I := 0 to ObjectCount - 1 do
      Objects[I].FObjects.Remove(Self);

  Owner.SetChartSizes;
  FreeAndNil(FFontData);
  inherited Destroy;
end;

procedure TdxFcObject.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  ValidateSize(AWidth, AHeight);
  if (ALeft = Left) and (ATop = Top) and (AWidth = Width) and (AHeight = Height) then
    Exit;
  FTop := ATop;
  FLeft := ALeft;
  FWidth := Word(AWidth);
  FHeight := Word(AHeight);

  Owner.NeedRepaintObject(Self);
  CalculateRealBounds;
  UpdateConnections;
  Owner.NeedRepaintObject(Self);
  Owner.SetChartSizes;
end;

procedure TdxFcObject.SetBounds(const R: TRect);
begin
  SetBounds(R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top);
end;

function TdxFcObject.GetLinkedObject(Index: Integer): TdxFcObject;
begin
  Result := TdxFcObject(GetListItem(FLinkedObjects, Index));
end;

function TdxFcObject.GetConnection(Index: Integer): TdxFcConnection;
begin
  Result := TdxFcConnection(GetListItem(FConnections, Index));
end;

function TdxFcObject.GetObjectValue(Index: Integer): TdxFcObject;
begin
  Result := TdxFcObject(GetListItem(FObjects, Index));
end;

function TdxFcObject.GetPainter: TdxFlowChartPainter;
begin
  Result := Owner.Painter;
end;

function TdxFcObject.GetIsUnion: Boolean;
begin
  Result := FObjects.Count > 0;
end;

function TdxFcObject.GetConnectionCount: Integer;
begin
  Result := FConnections.Count;
end;

function TdxFcObject.GetLinkedObjectCount: Integer;
begin
  Result := FLinkedObjects.Count;
end;

function TdxFcObject.GetObjectCount: Integer;
begin
  Result := FObjects.Count;
end;

function TdxFcObject.SelList: TList;
begin
  Result := Owner.FSelObjects;
end;

function TdxFcObject.HasEdge: Boolean;
begin
  Result := (ShapeType = fcsRectangle) and (EdgeStyle <> 0);
end;

function TdxFcObject.HasImage: Boolean;
begin
  Result := IsImageAssigned(Owner.Images, ImageIndex);
end;

procedure TdxFcObject.SetAdvanceShape(Value: TdxFlowChartObjectAdvancedShape);
begin
  if FAdvancedShape <> Value then
  begin
    FAdvancedShape := Value;
    if FAdvancedShape <> nil then
    begin
      FShapeType := fcsAdvanced;
      CreateAdvancedShapeInfo;
      Owner.BeginUpdate;
      try
        SetBounds(Left, Top, Value.DefaultSize.cx, Value.DefaultSize.cy);
        CalculateLinkedPoints;
        UpdateConnections;
        Owner.NeedRepaintObject(Self);
        Changed;
      finally
        Owner.EndUpdate;
      end;
    end
    else
    begin
      FShapeType := fcsRectangle;
      FreeAdvancedShapeInfo;
      SetLength(LinkedPoints, 16);
      CalculateLinkedPoints;
      UpdateConnections;
      Owner.NeedRepaintObject(Self);
      Changed;
    end;
  end;
end;

procedure TdxFcObject.SetAngle(Value: Single);
begin
  if ShapeType <> fcsAdvanced then
    Value := 0;
  Value := dxFMod(Value, 360);
  if not SameValue(Value, FAngle) then
  begin
    FAngle := Value;
    CalculateRealBounds;
    UpdateConnections;
    Owner.Invalidate;
    Owner.SetChartSizes;
  end;
end;

procedure TdxFcObject.SetBkColor(Value: TColor);
begin
  if FBkColor <> Value then
  begin
    FBkColor := Value;
    if not Transparent and (ShapeType <> fcsNone) then
      Owner.NeedRepaintObject(Self);
    Changed;
  end;
end;

procedure TdxFcObject.SetHeight(Value: Word);
begin
  SetBounds(Left, Top, Width, Value);
end;

procedure TdxFcObject.SetHighlightedLinkedPointIndex(Value: Integer);
begin
  if InRange(Value, 0, LinkedPointCount - 1) then
    FHighlightedLinkedPointIndex := Value
  else
    FHighlightedLinkedPointIndex := -1;
end;

procedure TdxFcObject.SetHorzImagePos(Value: TdxFcHorzPos);
begin
  if (FHorzImagePos <> Value) then
  begin
    FHorzImagePos := Value;
    if HasImage then
      Owner.NeedRepaintObject(Self);
    Changed;
  end;
end;

procedure TdxFcObject.SetHorzTextPos(Value: TdxFcHorzPos);
begin
  if (FHorzTextPos <> Value) then
  begin
    FHorzTextPos := Value;
    UpdateFontFormat;
    if Text <> '' then
      Owner.NeedRepaintObject(Self);
    Changed;
  end;
end;

procedure TdxFcObject.SetImageIndex(Value: Smallint);
begin
  if (FImageIndex <> Value) then
  begin
    FImageIndex := Value;
    if Owner.Images <> nil then
      Owner.NeedRepaintObject(Self);
    Changed;
  end;
end;

procedure TdxFcObject.SetLeft(Value: Integer);
begin
  SetBounds(Value, Top, Width, Height);
end;

procedure TdxFcObject.SetShapeColor(Value: TColor);
begin
  if FShapeColor <> Value then
  begin
    FShapeColor := Value;
    if ShapeType <> fcsNone then
      Owner.NeedRepaintObject(Self);
    Changed;
  end;
end;

procedure TdxFcObject.SetShapeType(Value: TdxFcShapeType);
begin
  if (FShapeType <> Value) and (Value <> fcsAdvanced) then
  begin
    FShapeType := Value;
    SetLength(LinkedPoints, 16);
    CalculateLinkedPoints;
    UpdateConnections;
    Owner.NeedRepaintObject(Self);
    Angle := 0;
  end;
end;

procedure TdxFcObject.SetShapeStyle(Value: TPenStyle);
begin
  if FShapeStyle <> Value then
  begin
    FShapeStyle := Value;
    Owner.NeedRepaintObject(Self);
    Changed;
  end;
end;

procedure TdxFcObject.SetShapeWidth(Value: Byte);
begin
  if FShapeWidth <> Value then
  begin
    FShapeWidth := Value;
    CalculateRealShapeWidth;
    if ShapeType <> fcsNone then
      Owner.NeedRepaintObject(Self);
    Changed;
  end;
end;

procedure TdxFcObject.SetPosition(ALeft, ATop: Integer);
begin
  FLeft := ALeft;
  FTop := ATop;
  CalculateRealBounds;
  Changed;
end;

procedure TdxFcObject.SetText(Value: string);
begin
  if FText <> Value then
  begin
    FText := Value;
    Owner.NeedRepaintObject(Self);
    Changed;
  end;
end;

procedure TdxFcObject.UpdateSize(AWidth, AHeight: Integer);
begin
  FWidth := AWidth;
  FHeight := AHeight;
end;

procedure TdxFcObject.SetTransparent(Value: Boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    if ShapeType <> fcsNone then
      Owner.NeedRepaintObject(Self);
    Changed;
  end;
end;

procedure TdxFcObject.SetTop(Value: Integer);
begin
  SetBounds(Left, Value, Width, Height);
end;

procedure TdxFcObject.SetVertImagePos(Value: TdxFcVertPos);
begin
  if FVertImagePos <> Value then
  begin
    FVertImagePos := Value;
    if HasImage then
      Owner.NeedRepaintObject(Self);
    Changed;
  end;
end;

procedure TdxFcObject.SetVertTextPos(Value: TdxFcVertPos);
begin
  if FVertTextPos <> Value then
  begin
    FVertTextPos := Value;
    UpdateFontFormat;
    if Text <> '' then
      Owner.NeedRepaintObject(Self);
    Changed;
  end;
end;

procedure TdxFcObject.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Selected := False;
    Owner.FHitTestResult := [];
    Owner.NeedRepaintObject(Self);
  end;
end;

procedure TdxFcObject.SetWidth(Value: Word);
begin
  SetBounds(Left, Top, Value, Height);
end;

procedure TdxFcObject.SetBorder(Value: Word);
begin
  if Value <> FBorder then
  begin
    FBorder := Value;
    if (ShapeType = fcsRectangle) and (FEdge <> 0) then
      Owner.NeedRepaintObject(Self);
    Changed;
  end;
end;

procedure TdxFcObject.SetEdge(Value: Word);
begin
  if Value <> FEdge then
  begin
    FEdge := Value;
    if ShapeType = fcsRectangle then
      Owner.NeedRepaintObject(Self);
    Changed;
  end;
end;

procedure TdxFcObject.SetZOrder(Value: Word);
begin
  if Value >= Owner.ObjectCount then
    Value := Owner.ObjectCount - 1;
  if Value <> ZOrder then
    with Owner do
    begin
      FHitTestResult := [];
      FObjects.Remove(Self);
      FObjects.Insert(Value, Self);
      NeedRepaintObject(Self);
      Changed(Self);
    end;
end;

procedure TdxFcObject.BringToFront;
begin
  SetZOrder(Owner.ObjectCount - 1);
end;

procedure TdxFcObject.SendToBack;
begin
  SetZOrder(0);
end;

procedure TdxFcObject.PutInFrontOf(Value: TdxFcObject);
var
  Z: Integer;
begin
  Z := Value.ZOrder;
  if Z < ZOrder then Inc(Z);
  SetZOrder(Z);
end;

procedure TdxFcObject.CalculateRealBounds;
begin
  FRealLeft := MulDiv(Left, Owner.RealZoom, 100);
  FRealTop := MulDiv(Top, Owner.RealZoom, 100);
  FRealWidth := MulDiv(Width, Owner.RealZoom, 100);
  FRealHeight := MulDiv(Height, Owner.RealZoom, 100);
  CalculateLinkedPoints;
end;

procedure TdxFcObject.ZoomChanged;
begin
  CalculateRealBounds;
  CalculateRealShapeWidth;
  ScaleFont;
end;

procedure TdxFcObject.CreateAdvancedShapeInfo;
begin
  FAdvancedShapeInfo.Free;
  FAdvancedShapeInfo := TAdvancedShapeInfo.Create(Self);
end;

procedure TdxFcObject.FreeAdvancedShapeInfo;
begin
  FreeAndNil(FAdvancedShapeInfo);
end;

procedure TdxFcObject.CalulateAdvancedShapeLinkedPoints;
var
  AConnectionPoints: TArray<TdxPointF>;
  I: Integer;
begin
  AdvancedShapeInfo.CheckSize(Width, Height);
  AConnectionPoints := AdvancedShapeInfo.ConnectionPoints;
  for I := Low(AConnectionPoints) to High(AConnectionPoints) do
    LinkedPoints[I] := AdvancedShapePointToLinkedPoint(AConnectionPoints[I]);
end;

procedure TdxFcObject.CalculateLinkedPointsForBounds(const ABounds: TRect);
var
  AQuarterWidth, AQuarterHeight: Integer;
  DX, DY: Integer;

  procedure IncP(Index, IX, IY: Integer);
  begin
    Inc(LinkedPoints[Index].X, IX);
    Inc(LinkedPoints[Index].Y, IY);
  end;

  procedure Trio(I1, I2, I3: Integer);
  begin
    LinkedPoints[I1].X := LinkedPoints[I2].X;
    LinkedPoints[I1].Y := LinkedPoints[I3].Y;
    LinkedPoints[I2].Y := LinkedPoints[I1].Y;
    LinkedPoints[I3].X := LinkedPoints[I1].X;
  end;

  procedure TrioX(I1, I2, I3, I4: Integer);
  begin
    LinkedPoints[I1].X := LinkedPoints[I2].X + DX;
    LinkedPoints[I3].X := LinkedPoints[I4].X - DX;
  end;

  procedure TrioY(I1, I2, I3, I4: Integer);
  begin
    LinkedPoints[I1].Y := LinkedPoints[I2].Y + DY;
    LinkedPoints[I3].Y := LinkedPoints[I4].Y - DY;
  end;

  function Scale(AValue, ACoef: Integer): Integer;
  begin
    Result := (AValue * ACoef + 512) shr 10;
  end;

begin
  AQuarterWidth := ABounds.Width div 4;
  AQuarterHeight := ABounds.Height div 4;
  for DX := 4 to 8 do
    LinkedPoints[DX].X := ABounds.Right;
  for DX := 12 to 16 do
    LinkedPoints[DX and 15].X := ABounds.Left;
  for DX := 0 to 4 do
    LinkedPoints[DX].Y := ABounds.Top;
  for DX := 8 to 12 do
    LinkedPoints[DX].Y := ABounds.Bottom;
  LinkedPoints[1].X := ABounds.Left + AQuarterWidth;
  LinkedPoints[2].X := ABounds.Left + ABounds.Width shr 1;
  LinkedPoints[3].X := ABounds.Right - AQuarterWidth;
  LinkedPoints[5].Y := ABounds.Top + AQuarterHeight;
  LinkedPoints[6].Y := ABounds.Top + ABounds.Height shr 1;
  LinkedPoints[7].Y := ABounds.Bottom - AQuarterHeight;
  LinkedPoints[9].X := LinkedPoints[3].X;
  LinkedPoints[10].X := LinkedPoints[2].X;
  LinkedPoints[11].X := LinkedPoints[1].X;
  LinkedPoints[13].Y := LinkedPoints[7].Y;
  LinkedPoints[14].Y := LinkedPoints[6].Y;
  LinkedPoints[15].Y := LinkedPoints[5].Y;

  if not dxFlowChartSizeStandardObjectsAsAdvanced then
  begin
    for DX := 4 to 8 do
      Dec(LinkedPoints[DX].X);
    for DX := 8 to 12 do
      Dec(LinkedPoints[DX].Y);
  end;

  DX := AQuarterWidth shr 1; DY := AQuarterHeight shr 1;
  case ShapeType of
    fcsUser: UserLinkedPoints;
    fcsNorthTriangle:
      begin
        Trio(0, 1, 14);
        TrioX(13, 13, 15, 2);
        Trio(4, 3, 6);
        TrioX(5, 2, 7, 7);
      end;
    fcsSouthTriangle:
      begin
        Trio(12, 11, 14);
        TrioX(15, 15, 13, 10);
        Trio(8, 9, 6);
        TrioX(7, 10, 5, 5);
      end;
    fcsEastTriangle:
      begin
        Trio(4, 2, 5);
        TrioY(1, 1, 3, 6);
        Trio(8, 10, 7);
        TrioY(9, 6, 11, 11);
      end;
    fcsWestTriangle:
      begin
        Trio(0, 2, 15);
        TrioY(3, 3, 1, 14);
        Trio(12, 10, 13);
        TrioY(11, 14, 9, 9);
      end;
    fcsHexagon:
      begin
        IncP(0, DX, AQuarterHeight);
        IncP(4, -DX, AQuarterHeight);
        IncP(8, -DX, -AQuarterHeight);
        IncP(12, DX, -AQuarterHeight);
        IncP(13, DX, 0);
        IncP(15, DX, 0);
        IncP(5, -DX, 0);
        IncP(7, -DX, 0);
      end;
    fcsDiamond:
      begin
        IncP(0, AQuarterWidth, AQuarterHeight);
        IncP(4, -AQuarterWidth, AQuarterHeight);
        IncP(8, -AQuarterWidth, -AQuarterHeight);
        IncP(12, AQuarterWidth, -AQuarterHeight);
        IncP(1, DX, DY);
        IncP(3, -DX, DY);
        IncP(5, -DX, DY);
        IncP(7, -DX, -DY);
        IncP(9, -DX, -DY);
        IncP(11, DX, -DY);
        IncP(13, DX, -DY);
        IncP(15, DX, DY);
      end;
    fcsRoundRect:
      begin
        DX := Scale(ABounds.Width, 75);
        DY := Scale(ABounds.Height, 75);
        IncP(0, DX, DY);
        IncP(4, -DX, DY);
        IncP(8, -DX, -DY);
        IncP(12, DX, -DY);
      end;
    fcsEllipse:
      begin
        DX := Scale(ABounds.Width, 150);
        DY := Scale(ABounds.Height, 150);
        IncP(0, DX, DY);
        IncP(4, -DX, DY);
        IncP(8, -DX, -DY);
        IncP(12, DX, -DY);
        DX := Scale(ABounds.Width, 68);
        DY := Scale(ABounds.Height, 68);
        IncP(13, DX, 0);
        IncP(15, DX, 0);
        IncP(5, -DX, 0);
        IncP(7, -DX, 0);
        IncP(1, 0, DY);
        IncP(3, 0, DY);
        IncP(9, 0, -DY);
        IncP(11, 0, -DY);
      end;
  end;
end;

procedure TdxFcObject.CalculateLinkedPoints;
var
  AMatrix: TdxGPMatrix;
begin
  Owner.FHitTestResult := [];
  if ShapeType = fcsAdvanced then
  begin
    CalulateAdvancedShapeLinkedPoints;
    if IsRotated then
    begin
      AMatrix := TdxGPMatrix.Create;
      try
        AMatrix.Rotate(Angle, TdxPointF.Create(RealLeft + RealWidth / 2, RealTop + RealHeight / 2));
        AMatrix.TransformPoints(LinkedPoints);
      finally
        AMatrix.Free;
      end;
    end;
    FBoundingRect.InitSize(RealLeft, RealTop, RealWidth, RealHeight);
  end
  else
  begin
    CalculateLinkedPointsForBounds(TRect.CreateSize(RealLeft, RealTop, RealWidth, RealHeight));
    FBoundingRect := GetBoundingRect(LinkedPoints, Length(LinkedPoints));
  end;
end;

procedure TdxFcObject.CalculateRealShapeWidth;
begin
  FRealShapeWidth := (ShapeWidth * Owner.RealZoom + 50) div 100;
  if (RealShapeWidth = 0) and (ShapeWidth <> 0) then
    FRealShapeWidth := 1;
  FRealContentIndent := FRealShapeWidth div 2;
  if not dxFlowChartSizeStandardObjectsAsAdvanced then
    Inc(FRealContentIndent);
end;

procedure TdxFcObject.UpdateConnections;
var
  I: Integer;
begin
  Changed;
  for I := 0 to ConnectionCount - 1 do
    with Connections[I] do
    begin
      ConnectionChanged;
      SetObjectPoints;
      CalculateDisplayRect;
      CheckRouting;
      Changed;
    end;
end;

procedure TdxFcObject.Invalidate;
begin
  Owner.NeedRepaintObject(Self);
end;

function TdxFcObject.NeedAlignWithGrid: Boolean;
begin
  Result := fcoAlignWithGrid in Owner.Options;
end;

function TdxFcObject.GetBounds: TRect;
begin
  Result.InitSize(FLeft, FTop, FWidth, FHeight);
end;

class function TdxFcObject.GetBoundingRect(const APoints: array of TPoint; ACount: Integer): TRect;
var
  P: TPoint;
  I: Integer;
begin
  with Result do
  begin
    Init(MaxInt, MaxInt, MinInt, MinInt);
    for I := 0 to ACount - 1 do
    begin
      P := APoints[I];
      Init(Min(Left, P.X), Min(Top, P.Y), Max(Right, P.X), Max(Bottom, P.Y));
    end;
  end;
end;

function TdxFcObject.GetClientRect: TRect;
begin
  if ShapeType <> fcsAdvanced then
  begin
    Result.TopLeft := LinkedPoints[0];
    Result.BottomRight := LinkedPoints[8];
    case ShapeType of
      fcsNorthTriangle:
        Result.Right := LinkedPoints[4].X;
      fcsSouthTriangle:
        Result.Left := LinkedPoints[1].X;
      fcsEastTriangle:
        Result.Top := LinkedPoints[15].Y;
      fcsWestTriangle:
        Result.Bottom := LinkedPoints[7].Y;
    end;
  end
  else
    Result := FBoundingRect;
  InflateRect(Result, -RealShapeWidth, -RealShapeWidth);
  Result := Owner.ScrollRect(Result, False);
end;

function TdxFcObject.GetDisplayRect: TRect;
begin
  Result := TRect.CreateSize(RealLeft, RealTop, RealWidth, RealHeight);
  Result := Owner.ScrollRect(Result, False);
end;

function TdxFcObject.GetInvalidateRect: TRect;
begin
  Result := GetBoundsForRotatedRect(inherited GetInvalidateRect);
  Inc(Result.Bottom);
  Inc(Result.Right);
end;

function TdxFcObject.GetLinkedPointCount: Integer;
begin
  if AdvancedShapeInfo <> nil then
    Result := Length(AdvancedShapeInfo.ConnectionPoints)
  else
    Result := 16;
end;

function TdxFcObject.GetSelectionRect: TRect;
var
  ASelectionIndent: Integer;
begin
  Result := DisplayRect;
  if Selected then
  begin
    ASelectionIndent := Ceil(Owner.Selection.SelectionIndent) + 1;
    Result.Inflate(ASelectionIndent, ASelectionIndent);
    if CanRotate then
      Dec(Result.Top, Ceil(Owner.Selection.RotationMarkOffset) + Ceil(Owner.Selection.RotationMarkSize / 2) + 1);
  end;
end;

function TdxFcObject.ControlToObjectPoint(const P: TPoint): TPoint;
var
  AMatrix: TdxGPMatrix;
  ABounds: TRect;
begin
  if IsRotated then
  begin
    AMatrix := TdxGPMatrix.Create;
    try
      ABounds := DisplayRect;
      AMatrix.Rotate(-Angle, TdxPointF.Create(ABounds.Left + ABounds.Width / 2, ABounds.Top + ABounds.Height / 2));
      Result := AMatrix.TransformPoint(P);
    finally
      AMatrix.Free;
    end;
  end
  else
    Result := P;
end;

function TdxFcObject.GetBoundsForRotatedRect(const R: TRect): TRect;
var
  AMatrix: TdxGPMatrix;
  APoints: TArray<TPoint>;
begin
  if IsRotated then
  begin
    AMatrix := TdxGPMatrix.Create;
    try
      AMatrix.Rotate(Angle, DisplayRect.ToRectF.CenterPoint);
      SetLength(APoints, 4);
      APoints[0].X := R.Left;
      APoints[0].Y := R.Top;
      APoints[1].X := R.Right;
      APoints[1].Y := R.Top;
      APoints[2].X := R.Left;
      APoints[2].Y := R.Bottom;
      APoints[3].X := R.Right;
      APoints[3].Y := R.Bottom;
      AMatrix.TransformPoints(APoints);
      Result := GetBoundingRect(APoints, 4);
    finally
      AMatrix.Free;
    end;
  end
  else
    Result := R;
end;

function TdxFcObject.GetBoundingRect: TRect;
begin
  if ShapeType = fcsAdvanced then
    Result := AdvancedShapeInfo.GetBoundingRect
  else
    Result := DisplayRect;
end;

function TdxFcObject.GetZOrder: Word;
begin
  Result := Word(Owner.FObjects.IndexOf(Self));
end;

function TdxFcObject.CalculatePolygon(out APolygon: TPoints; AIndent: Integer): Boolean;

  function AdjustPoint(APointPosition: Integer): TPoint;
  begin
    Result := LinkedPoints[APointPosition];
    if (APointPosition >= 0) and (APointPosition <= 4) then
      Inc(Result.Y, AIndent);
    if (APointPosition >= 4) and (APointPosition <= 8) then
      Dec(Result.X, AIndent);
    if (APointPosition >= 8) and (APointPosition <= 12) then
      Dec(Result.Y, AIndent);
    if (APointPosition >= 12) and (APointPosition <= 15) or (APointPosition = 0) then
      Inc(Result.X, AIndent);
  end;

  procedure SetPolygon(const AIndexes: array of Integer);
  var
    ALength: Integer;
    I: Integer;
  begin
    ALength := Length(AIndexes);
    SetLength(APolygon, ALength);
    for I := 0 to ALength - 1 do
      APolygon[I] := Owner.ScrollPoint(AdjustPoint(AIndexes[I]), False);
  end;

begin
  case ShapeType of
    fcsDiamond:
      SetPolygon([2, 6, 10, 14]);
    fcsHexagon:
      SetPolygon([1, 3, 6, 9, 11, 14]);
    fcsNorthTriangle:
      SetPolygon([2, 8, 12]);
    fcsSouthTriangle:
      SetPolygon([0, 4, 10]);
    fcsEastTriangle:
      SetPolygon([0, 6, 12]);
    fcsWestTriangle:
      SetPolygon([4, 8, 14]);
    else
      SetLength(APolygon, 0);
  end;
  Result := Length(APolygon) > 0;
end;

function TdxFcObject.CalculateRegion(const ARect: TRect; AShapeWidth: Integer): TcxRegionHandle;
var
  P: TPoints;
  R: TRect;
begin
  Result := 0;
  R := ARect;
  InflateRect(R, -AShapeWidth, -AShapeWidth);
  case ShapeType of
    fcsUser:
      Result := UserRegion(R);
    fcsRectangle, fcsNone, fcsAdvanced:
      Result := CreateRectRgnIndirect(R);
    fcsEllipse:
      Result := CreateEllipticRgnIndirect(R);
    fcsRoundRect:
      Result := CreateRoundRectRgn(R.Left, R.Top, R.Right, R.Bottom, cxRectWidth(R) div 2, cxRectHeight(R) div 2);
    else
      if CalculatePolygon(P, AShapeWidth) then
      begin
        Result := CreatePolygonRgn(P[0], Length(P), ALTERNATE);
        P := nil;
      end;
  end;
end;

function TdxFcObject.CanRotate: Boolean;
begin
  Result := (ShapeType = fcsAdvanced) and (fcoCanRotate in Owner.Options);
end;

function TdxFcObject.CanUseParameters: Boolean;
begin
  Result := (ShapeType = fcsAdvanced) and (fcoUseShapeParameters in Owner.Options);
end;

procedure TdxFcObject.ChangeScale(M, D: Integer);
begin
  inherited ChangeScale(M, D);
  Height := MulDiv(Height, M, D);
  Left := MulDiv(Left, M, D);
  Top := MulDiv(Top, M, D);
  Width := MulDiv(Width, M, D);
  CalculateRealBounds;
end;

function TdxFcObject.ContainsPoint(X, Y, ATolerance: Integer): Boolean;
var
  R: TRect;
begin
  if not Visible then
    Exit(False);
  R := GetBoundsForRotatedRect(DisplayRect);
  R.Inflate(ATolerance, ATolerance);
  Result := R.Contains(X, Y);
end;

function TdxFcObject.CreateClientRegion: TcxRegionHandle;
begin
  Result := CalculateRegion(DisplayRect, RealShapeWidth);
end;

function TdxFcObject.CreateRegion: TcxRegionHandle;
begin
  Result := CalculateRegion(DisplayRect, 0);
end;

function TdxFcObject.CreateShapeRegion: TcxRegionHandle;
var
  ARgn: TcxRegionHandle;
begin
  Result := CreateRegion;
  ARgn := CreateClientRegion;
  CombineRgn(Result, Result, ARgn, RGN_DIFF);
  DeleteObject(ARgn);
end;

function TdxFcObject.GetResizeCursor(const P: TPoint): TCursor;
const
  Cursors: array[0..3] of TCursor = (crSizeNWSE, crSizeNS, crSizeNESW, crSizeWE);
var
  AIndex: Integer;
  A: Single;
begin
  A := Angle;
  if A < 0 then
    A := Angle + 360;
  AIndex := GetSelPoint(P.X, P.Y);
  Result := Cursors[(Round(A / 45) + AIndex) and 3];
end;

function TdxFcObject.GetDefaultSize: TSize;
begin
  if AdvancedShape <> nil then
    Result := AdvancedShape.DefaultSize
  else
    Result.Init(80, 80);
end;

procedure TdxFcObject.SelPoints(var APoints: TdxFcSelectionPoints);
begin
  SelPoints(APoints, TRect.CreateSize(FRealLeft, FRealTop, FRealWidth, FRealHeight));
end;

procedure TdxFcObject.SelPoints(var APoints: TdxFcSelectionPoints; const ABounds: TRect);
var
  I: Integer;
  AMatrix: TdxGPMatrix;
begin
  for I := 2 to 4 do
    APoints[I].X := ABounds.Left + ABounds.Width - 1;
  for I := 6 to 8 do
    APoints[I and 7].X := ABounds.Left;
  for I := 0 to 2 do
    APoints[I].Y := ABounds.Top;
  for I := 4 to 6 do
    APoints[I].Y := ABounds.Top + ABounds.Height - 1;
  APoints[1].X := ABounds.Left + ABounds.Width shr 1;
  APoints[5].X := APoints[1].X;
  APoints[3].Y := ABounds.Top + ABounds.Height shr 1;
  APoints[7].Y := APoints[3].Y;
  if IsRotated then
  begin
    AMatrix := TdxGPMatrix.Create;
    try
      AMatrix.Rotate(Angle, TdxPointF.Create(ABounds.Left + ABounds.Width / 2, ABounds.Top + ABounds.Height / 2));
      AMatrix.TransformPointsI(@APoints[0], 8);
    finally
      AMatrix.Free;
    end;
  end;
end;

function TdxFcObject.GetPoint(const P: array of TPoint; X, Y, ACount: Integer): Integer;
var
  I: Integer;
  AMin, ADistance: Int64;
begin
  Result := 0;
  AMin := MaxInt64;
  Inc(X, Owner.LeftEdge);
  Inc(Y, Owner.TopEdge);
  for I := 0 to ACount - 1 do
  begin
    ADistance := Sqr(P[I].X - X) + Sqr(P[I].Y - Y);
    if ADistance < AMin then
    begin
      Result := I;
      AMin := ADistance;
    end;
  end;
end;

function TdxFcObject.GetSelPoint(X, Y: Integer): Integer;
var
  P: TdxFcSelectionPoints;
begin
  SelPoints(P);
  Result := GetPoint(P, X, Y, 8);
end;

function TdxFcObject.IsPointInsideShape(const P: TPoint): Boolean;
var
  ARegion: TcxRegionHandle;
begin
  if ShapeType = fcsAdvanced then
    Result := AdvancedShapeInfo.IsPointInsideShape(P)
  else
  begin
    ARegion := CreateClientRegion;
    Result := cxPtInRegion(ARegion, P);
    DeleteObject(ARegion);
  end;
end;

function TdxFcObject.PointInRealDisplayRect(const P: TPoint): Boolean;
begin
  Result := DisplayRect.Contains(ControlToObjectPoint(P))
end;

function TdxFcObject.GetParameterIndex(const P: TPoint): Integer;
begin
  if not CanUseParameters then
    Exit(-1);
  Result := AdvancedShapeInfo.GetParameterIndex(P);
end;

function TdxFcObject.PointOnRotationMark(const P: TPoint): Boolean;
var
  AObjectPoint: TPoint;
  ARotationMarkCenter: TPoint;
  ABounds: TRect;
begin
  if not CanRotate then
    Exit(False);
  ABounds := DisplayRect;
  AObjectPoint := ControlToObjectPoint(P);
  ARotationMarkCenter.Init(
    ABounds.Left + ABounds.Width div 2,
    ABounds.Top - Ceil(Owner.Selection.RotationMarkOffset));
  Result :=
    Sqr(ARotationMarkCenter.X - AObjectPoint.X) + Sqr(ARotationMarkCenter.Y - AObjectPoint.Y) <
    Sqr(Owner.Selection.RotationMarkSize * 0.39);
end;

function TdxFcObject.PointOnSelectionFrame(const P: TPoint): Boolean;
begin
  Result := SelectionRect.Contains(ControlToObjectPoint(P));
end;

procedure TdxFcObject.UpdateFontFormat;
begin
  if FontData <> nil then
    FontData.UpdateFontFormat;
end;

function TdxFcObject.HitTest(const P: TPoint): TdxFcHitTest;
begin
  if Visible and PointInRealDisplayRect(P) then
  begin
    Result := [htByObject];
    if IsPointInsideShape(P) then
      Include(Result, htOnObject);
  end
  else
    Result := [];
end;

function TdxFcObject.InRect(const R: TRect): Boolean;
var
  ARegion: TcxRegionHandle;
begin
  Result := Visible;
  if Result then
  begin
    ARegion := CreateClientRegion;
    Result := RectInRegion(ARegion, R);
    DeleteObject(ARegion);
  end;
end;

function TdxFcObject.GetLinkedPoint(X, Y: Integer): Integer;
begin
  if LinkedPointCount = 0 then
    Result := -1
  else
    Result := GetPoint(LinkedPoints, X, Y, LinkedPointCount);
end;

function TdxFcObject.HasInUnion(AObject: TdxFcObject): Boolean;
var
  I: Integer;
begin
  Result := (AObject = nil) or (AObject = Self);
  if Result then Exit;
  for I := 0 to ObjectCount - 1 do
  begin
    Result := Objects[I].HasInUnion(AObject);
    if Result then Exit;
  end;
end;

procedure TdxFcObject.AddToUnion(AObject: TdxFcObject);
begin
  if not HasInUnion(AObject) then FObjects.Add(AObject);
end;

procedure TdxFcObject.RemoveFromUnion(AObject: TdxFcObject);
begin
  FObjects.Remove(AObject);
end;

procedure TdxFcObject.ClearUnion;
begin
  FObjects.Clear;
end;

procedure TdxFcObject.SelectUnion;
var
  I: Integer;
begin
  Selected := True;
  if fcoMultiSelect in Owner.Options then
  begin
    for I := 0 to ObjectCount - 1 do
      Objects[I].SelectUnion;
  end;
end;

procedure TdxFcObject.MakeVisible;
var
  R: TRect;
  X, Y: Integer;
begin
  Visible := True;
  if (RealWidth > Owner.ClientWidth) or (RealHeight > Owner.ClientHeight) then
    R := ClientRect
  else
    R := DisplayRect;

  X := R.Left;
  Y := R.Top;
  if R.Right > Owner.ClientWidth then
    Inc(X, Owner.ClientWidth - R.Right);
  if R.Bottom > Owner.ClientHeight then
    Inc(Y, Owner.ClientHeight - R.Bottom);

  X := Max(X, 0);
  Y := Max(Y, 0);
  if (X <> R.Left) or (Y <> R.Top) then
    Owner.SetLeftTop(Owner.LeftEdge + R.Left - X, Owner.TopEdge + R.Top - Y);
end;

function TdxFcObject.IsRotated: Boolean;
begin
  Result := not IsZero(Angle);
end;

procedure TdxFcObject.Move(DX, DY: Integer);
begin
  Owner.NeedRepaintObject(Self);
  SetPosition(Left + DX, Top + DY);
  Owner.NeedRepaintObject(Self);
end;

procedure TdxFcObject.ScaleFont;
begin
  inherited ScaleFont;
  UpdateFontData;
end;

procedure TdxFcObject.UpdateFontData;
begin
  if FFontData = nil then
    FFontData := TFontData.Create(Self);
  FontData.Update;
end;

procedure TdxFcObject.Paint(ACanvas: TcxCanvas);
begin
  if Visible then
  begin
    if ShapeType = fcsAdvanced then
      PaintAdvancedShape(ACanvas)
    else
    begin
      PaintBackground(ACanvas);
      Owner.DoDrawObject(Self, ClientRect);
    end;
  end;
end;

function TdxFcObject.GetAdvancedShapeImageBounds(const AContentBounds: TRect): TRect;
var
  ABounds, AImageRect: TRect;
  AScaleFactor: TdxScaleFactor;
begin
  Result.Empty;
  if Owner.Images <> nil then
  begin
    AScaleFactor := TdxScaleFactor.Create;
    try
      AScaleFactor.Assign(Owner.ScaleFactor);
      AScaleFactor.Change(Owner.RealZoom, 100);
      AImageRect := cxRect(dxGetImageSize(Owner.Images, AScaleFactor));
      ABounds := AContentBounds;
      ABounds.Inflate(-RealShapeWidth, -RealShapeWidth);
      if AdjustImageRect(AImageRect, ABounds, HorzImagePos, VertImagePos) then
        Exit(AImageRect);
    finally
      AScaleFactor.Free;
    end;
  end;
end;

procedure TdxFcObject.PaintAdvancedShapeImage(AGraphics: TdxGPGraphics; const R: TRect);
var
  AImageRect: TRect;
begin
  if Owner.Images <> nil then
  begin
    AImageRect := GetAdvancedShapeImageBounds(R);
    if not AImageRect.IsEmpty then
      TdxImageListPaintCache.Draw(AGraphics, AImageRect, Owner.Images, ImageIndex, idmNormal, False, nil);
  end;
end;

procedure TdxFcObject.PaintAdvancedShapeText(AGraphics: TdxGPGraphics; const R: TRect);
begin
  FontData.DrawText(AGraphics, R.ToRectF);
end;

procedure TdxFcObject.PaintAdvancedShapeContent(AGraphics: TdxGPGraphics; const ABounds: TRect);
var
  AHasCustomDraw: Boolean;
  APaintData: TdxFcDrawExEventPaintData;
begin
  AHasCustomDraw := Owner.HasDrawObjectEx;
  if not AHasCustomDraw then
  begin
    if HasImage then
      PaintAdvancedShapeImage(AGraphics, ABounds);
    if Text <> '' then
      PaintAdvancedShapeText(AGraphics, ABounds);
  end
  else
  begin
    APaintData := TdxFcDrawExEventPaintData.Create(Self, ABounds);
    try
      UpdateFontData;
      Owner.DoDrawObjectEx(Self, AGraphics, APaintData);
      if HasImage and APaintData.DefaultDrawImage then
        PaintAdvancedShapeImage(AGraphics, ABounds);
      if (Text <> '') and APaintData.DefaultDrawText then
        PaintAdvancedShapeText(AGraphics, ABounds);
    finally
      APaintData.Free;
    end;
  end;
end;

procedure TdxFcObject.PaintAdvancedShape(ACanvas: TcxCanvas);
var
  AGraphics: TdxGPGraphics;
  AHalfWidth, AHalfHeight, AScaleFactor: Single;
  APaintShapeBounds, ABounds: TRect;
begin
  APaintShapeBounds := DisplayRect;
  AGraphics := dxGpBeginPaint(ACanvas.Handle, APaintShapeBounds);
  try
    AGraphics.SmoothingMode := smAntiAlias;
    AGraphics.SaveWorldTransform;
    try
      AHalfWidth := APaintShapeBounds.Width / 2;
      AHalfHeight := APaintShapeBounds.Height / 2;
      AGraphics.TranslateWorldTransform(APaintShapeBounds.Left + AHalfWidth, APaintShapeBounds.Top + AHalfHeight);
      AGraphics.RotateWorldTransform(Angle);
      AGraphics.TranslateWorldTransform(-AHalfWidth, -AHalfHeight);
      AScaleFactor := Owner.RealZoom / 100;
      AGraphics.ScaleWorldTransform(AScaleFactor, AScaleFactor);

      AdvancedShapeInfo.Paint(AGraphics, APaintShapeBounds);
      AGraphics.ScaleWorldTransform(1 / AScaleFactor, 1 / AScaleFactor);

      ABounds.InitSize(APaintShapeBounds.Size);
      PaintAdvancedShapeContent(AGraphics, ABounds);
    finally
      AGraphics.RestoreWorldTransform;
    end;
  finally
    dxGpEndPaint(AGraphics);
  end;
end;

procedure TdxFcObject.PaintBackground(ACanvas: TcxCanvas);

  function GetBackgroundColor: TColor;
  begin
    if Transparent then
      Result := clNone
    else
      Result := BkColor;
  end;

  function GetFrameColor: TColor;
  begin
    if HasEdge then
      Result := clNone
    else
      Result := ShapeColor;
  end;

var
  P: TPoints;
  R, AVisibleBounds: TRect;
begin
  if ShapeType in [fcsNone, fcsAdvanced] then Exit;

  AVisibleBounds := DisplayRect;
  R := AVisibleBounds;
  if not dxFlowChartSizeStandardObjectsAsAdvanced then
    R.Inflate(-RealContentIndent, -RealContentIndent)
  else
  begin
    Inc(R.Right);
    Inc(R.Bottom);
    Inc(AVisibleBounds.Right);
    Inc(AVisibleBounds.Bottom);
  end;
  case ShapeType of
    fcsUser:
      Painter.DrawRegion(ACanvas, UserRegion(R), GetBackgroundColor, GetFrameColor, RealShapeWidth);
    fcsEllipse:
      Painter.DrawEllipse(ACanvas, R, GetBackgroundColor, GetFrameColor, ShapeStyle, RealShapeWidth);
    fcsRectangle:
      Painter.DrawRectangle(ACanvas, R, GetBackgroundColor, GetFrameColor, ShapeStyle, RealShapeWidth);
    fcsRoundRect:
      Painter.DrawRoundRect(ACanvas, R, GetBackgroundColor,
        GetFrameColor, ShapeStyle, RealShapeWidth, RealWidth shr 1, RealHeight shr 1);
    fcsDiamond, fcsNorthTriangle, fcsSouthTriangle, fcsEastTriangle, fcsWestTriangle, fcsHexagon:
      if CalculatePolygon(P, RealContentIndent) then
      try
        Painter.DrawPolygon(ACanvas, P, GetBackgroundColor, GetFrameColor, ShapeStyle, RealShapeWidth);
      finally
        P := nil;
      end;
  end;

  if HasEdge then
    Painter.DrawEdge(ACanvas, AVisibleBounds, EdgeStyle, BorderStyle);
end;

procedure TdxFcObject.PaintHighlights(ACanvas: TcxCanvas);
var
  R: TRect;
  W: Integer;
  AGraphics: TdxGPGraphics;
begin
  R := DisplayRect;
  W := Owner.ScaleFactor.Apply(3);
  R.inflate(W, W);
  AGraphics := dxGpBeginPaint(ACanvas.Handle, R);
  try
    AGraphics.SmoothingMode := smAntiAlias;
    case HighlightedFrameMode of
      THighlightedFrameMode.Selected:
        PaintHighlightedFrame(AGraphics, TdxAlphaColors.FromArgb($24, $40,  $8F));
      THighlightedFrameMode.Connect:
        PaintHighlightedFrame(AGraphics, TdxAlphaColors.LimeGreen);
    end;
    if ShowConnectionPoints then
      PaintConnectionPoints(AGraphics);
  finally
    dxGpEndPaint(AGraphics);
  end;
end;

procedure TdxFcObject.PaintHighlightedFrame(AGraphics: TdxGPGraphics; AColor: TdxAlphaColor);
var
  ABounds: TRect;
  APen: TdxGPPen;
begin
  AGraphics.SaveWorldTransform;
  try
    APen := TdxGPPen.Create(AColor, Owner.ScaleFactor.Apply(3));
    try
      ABounds := DisplayRect;
      UpdateGraphicsForRotatedBounds(AGraphics, ABounds, Angle);
      AGraphics.Rectangle(ABounds, APen, nil);
    finally
      APen.Free;
    end;
  finally
    AGraphics.RestoreWorldTransform;
  end;
end;

procedure TdxFcObject.PaintConnectionPoints(AGraphics: TdxGPGraphics);
var
  R: TRect;
  P: TPoint;
  I, W: Integer;
  APen: TdxGPPen;
  ABrush: TdxGPBrush;
begin
  W := Owner.ScaleFactor.Apply(6);
  APen := TdxGPPen.Create(TdxAlphaColors.Black, 1.5);
  ABrush := TdxGPBrush.Create;
  ABrush.Color := TdxAlphaColors.AntiqueWhite;
  try
    for I := 0 to LinkedPointCount - 1 do
    begin
      P := LinkedPoints[I];
      P.Offset(-Owner.LeftEdge, -Owner.TopEdge);
      R := TRect.CreateSize(P, W, W);
      R.Offset(-W div 2, -W div 2);
      AGraphics.Ellipse(R, APen, ABrush);
    end;
  finally
    ABrush.Free;
    APen.Free;
  end;
  if HighlightedLinkedPointIndex >= 0 then
  begin
    APen := TdxGPPen.Create(TdxAlphaColors.LimeGreen, Owner.ScaleFactor.Apply(3));
    try
      P := LinkedPoints[HighlightedLinkedPointIndex];
      P.Offset(-Owner.LeftEdge, -Owner.TopEdge);
      R.Init(P, P);
      W := Owner.ScaleFactor.Apply(6);
      R.Inflate(W, W);
      AGraphics.Rectangle(R, APen, nil);
    finally
      APen.Free;
    end;
  end;
end;

procedure TdxFcObject.PaintImage(const R: TRect);
var
  AImageRect: TRect;
  AScaleFactor: TdxScaleFactor;
  AGraphics: TdxGPGraphics;
begin
  if Owner.Images <> nil then
  begin
    AGraphics := dxGpBeginPaint(Owner.Canvas.Handle, R);
    try
      AScaleFactor := TdxScaleFactor.Create;
      try
        AScaleFactor.Assign(Owner.ScaleFactor);
        AScaleFactor.Change(Owner.RealZoom, 100);
        AImageRect := cxRect(dxGetImageSize(Owner.Images, AScaleFactor));
        if AdjustImageRect(AImageRect, R, HorzImagePos, VertImagePos) then
          TdxImageListPaintCache.Draw(AGraphics, AImageRect, Owner.Images, ImageIndex, idmNormal, False, nil);
      finally
        AScaleFactor.Free;
      end;
    finally
      dxGpEndPaint(AGraphics);
    end;
  end;
end;

procedure TdxFcObject.PaintText(const R: TRect);
const
  Aligns: array[TdxFcHorzPos] of Word = (CXTO_LEFT, CXTO_CENTER_HORIZONTALLY, CXTO_RIGHT);
var
  AFlags: Cardinal;
  ATextRect: TRect;
begin
  ATextRect := R;
  AFlags := CXTO_EXPANDTABS or CXTO_WORDBREAK or Aligns[HorzTextPos];
  if VertTextPos <> fcvpUp then
  begin
    cxTextOut(Owner.Canvas.Handle, Text, ATextRect, AFlags or CXTO_CALCRECT, RealFont);
    AdjustRect(ATextRect, R, HorzTextPos, VertTextPos);
  end;
  cxTextOut(Owner.Canvas.Handle, Text, ATextRect, AFlags + CXTO_NOCLIP, RealFont);
end;

procedure TdxFcObject.ResolveObjRefs;
var
  I: Integer;
begin
  for I := 0 to ObjectCount - 1 do
    FObjects[I] := Owner.Objects[Integer(FObjects[I])];
end;

procedure TdxFcObject.Assign(Source: TPersistent);
begin
  if Source is TdxFcObject then
    with TdxFcObject(Source) do
    begin
      Self.AdvancedShape := AdvancedShape;
      Self.Tag := Tag;
      Self.CustomData := CustomData;
      Self.SetBounds(Left, Top, Width, Height);
      Self.EdgeStyle := EdgeStyle;
      Self.BorderStyle := BorderStyle;
      Self.HorzTextPos := HorzTextPos;
      Self.VertTextPos := VertTextPos;
      Self.HorzImagePos := HorzImagePos;
      Self.VertImagePos := VertImagePos;
      Self.BkColor := BkColor;
      Self.ShapeColor := ShapeColor;
      Self.ImageIndex := ImageIndex;
      Self.Transparent := Transparent;
      Self.ShapeType := ShapeType;
      Self.ShapeWidth := ShapeWidth;
      Self.Angle := Angle;
      if Self.AdvancedShape <> nil then
        Self.AdvancedShapeInfo.Parameters := AdvancedShapeInfo.Parameters;
    end;
  inherited Assign(Source);
end;

procedure TdxFcObject.Load(Stream: TStream; AIsUnicode: Boolean);
var
  I: Integer;
  ObjData: TdxFcObjData;
begin
  Stream.ReadBuffer(ObjData, SizeOf(ObjData));

  SetBounds(
    Owner.ScaleFactor.Apply(ObjData.Left),
    Owner.ScaleFactor.Apply(ObjData.Top),
    Owner.ScaleFactor.Apply(ObjData.Width),
    Owner.ScaleFactor.Apply(ObjData.Height));

  EdgeStyle := ObjData.Edge;
  BorderStyle := ObjData.Border;
  HorzTextPos := ObjData.HTPos;
  VertTextPos := ObjData.VTPos;
  HorzImagePos := ObjData.HIPos;
  VertImagePos := ObjData.VIPos;
  Self.BkColor := ObjData.BkColor;
  ShapeColor := ObjData.ShColor;
  Self.Tag := ObjData.Tag;
  ImageIndex := ObjData.Image;
  ShapeType := ObjData.Shape;
  ShapeWidth := ObjData.ShWidth;
  ParentFont := ObjData.ParFont;
  Transparent := ObjData.Transparent;

  while ObjData.ObjCnt > 0 do
  begin
    I := 0;
    Dec(ObjData.ObjCnt);
    Stream.ReadBuffer(I, SizeOf(Word));
    FObjects.Add(Pointer(I));
  end;

  LoadFont(Stream, AIsUnicode);
  Text := ReadStr(Stream, AIsUnicode);
  CustomData := ReadStr(Stream, AIsUnicode);
end;

procedure TdxFcObject.Load(ANode: TdxXMLNode);
var
  AAttributes: TdxXMLNodeAttributes;
  AObjectsNode, AObjectNode, AFontNode: TdxXMLNode;
begin
  AAttributes := ANode.Attributes;
  Text := AAttributes.GetValueAsString('Text');
  CustomData := AAttributes.GetValueAsString('CustomData');

  EdgeStyle := AAttributes.GetValueAsInteger('Edge');
  BorderStyle := AAttributes.GetValueAsInteger('Border', DefaultBorderStyle);

  TXMLFormatHelper.ReadAligment(AAttributes, 'TextAlignment', FHorzTextPos, FVertTextPos);
  UpdateFontFormat;

  TXMLFormatHelper.ReadAligment(AAttributes, 'ImageAlignment', FHorzImagePos, FVertImagePos);

  BkColor := TXMLFormatHelper.ReadColor(AAttributes, 'BkColor');
  ShapeColor := TXMLFormatHelper.ReadColor(AAttributes, 'ShapeColor');
  Tag := AAttributes.GetValueAsInteger('Tag');
  ImageIndex := AAttributes.GetValueAsInteger('ImageIndex', DefaultImageIndex);
  ShapeType := TXMLFormatHelper.ReadObjectShapeType(AAttributes);
  ShapeWidth := AAttributes.GetValueAsInteger('ShapeWidth', DefaultShapeWidth);
  ShapeStyle := TPenStyle(AAttributes.GetValueAsInteger('ShapeStyle', 0));
  AdvancedShape := TdxFlowChart.Repository.FindShapeByID(AAttributes.GetValueAsString('ShapeID'));
  Transparent := AAttributes.GetValueAsBoolean('Transparent', False);

  SetBounds(
    Owner.ScaleFactor.Apply(AAttributes.GetValueAsInteger('Left')),
    Owner.ScaleFactor.Apply(AAttributes.GetValueAsInteger('Top')),
    Owner.ScaleFactor.Apply(AAttributes.GetValueAsInteger('Width')),
    Owner.ScaleFactor.Apply(AAttributes.GetValueAsInteger('Height')));

  Angle := AAttributes.GetValueAsFloat('Angle', 0);

  if AdvancedShape <> nil then
  begin
    AdvancedShapeInfo.ReadParameters(AAttributes);
    CalculateLinkedPoints;
    UpdateConnections;
  end;

  AObjectsNode := ANode.FindChild('Objects');
  if AObjectsNode <> nil then
  begin
    AObjectNode := AObjectsNode.First;
    while AObjectNode <> nil do
    begin
      FObjects.Add(Pointer(AObjectNode.Attributes.GetValueAsInteger('ZOrder')));
      AObjectNode := AObjectNode.Next;
    end;
  end;

  AFontNode := ANode.FindChild('Font');
  ParentFont := AFontNode = nil;
  if AFontNode <> nil then
    LoadFont(AFontNode);
end;

procedure TdxFcObject.Save(Stream: TStream);
var
  I: Integer;
  W: Word;
  ObjData: TdxFcObjData;
begin
  ObjData.Left := Owner.ScaleFactor.Revert(Left);
  ObjData.Top := Owner.ScaleFactor.Revert(Top);
  ObjData.Width := Owner.ScaleFactor.Revert(Width);
  ObjData.Height := Owner.ScaleFactor.Revert(Height);
  ObjData.Edge := EdgeStyle;
  ObjData.Border := BorderStyle;
  ObjData.HTPos := HorzTextPos;
  ObjData.VTPos := VertTextPos;
  ObjData.HIPos := HorzImagePos;
  ObjData.VIPos := VertImagePos;
  ObjData.BkColor := BkColor;
  ObjData.ShColor := ShapeColor;
  ObjData.Tag := Tag;
  ObjData.Image := ImageIndex;
  ObjData.Shape := ShapeType;
  ObjData.ShWidth := ShapeWidth;
  ObjData.ParFont := ParentFont;
  ObjData.Transparent := Transparent;
  ObjData.ObjCnt := Word(ObjectCount);

  Stream.WriteBuffer(ObjData, SizeOf(ObjData));
  for I := 0 to ObjectCount - 1 do
  begin
    W := Objects[I].ZOrder;
    Stream.WriteBuffer(W, SizeOf(W));
  end;
  SaveFont(Stream);
  WriteStr(Stream, Text);
  WriteStr(Stream, CustomData);
end;

procedure TdxFcObject.Save(ANode: TdxXMLNode);
var
  I: Integer;
  AAttributes: TdxXMLNodeAttributes;
  AObjectsNode: TdxXMLNode;
begin
  AAttributes := ANode.Attributes;
  if Text <>'' then
    AAttributes.SetValueAsString('Text', Text);
  if CustomData <> '' then
    AAttributes.SetValueAsString('CustomData', CustomData);

  AAttributes.SetValueAsInteger('Left', Owner.ScaleFactor.Revert(Left));
  AAttributes.SetValueAsInteger('Top', Owner.ScaleFactor.Revert(Top));
  AAttributes.SetValueAsInteger('Width', Owner.ScaleFactor.Revert(Width));
  AAttributes.SetValueAsInteger('Height', Owner.ScaleFactor.Revert(Height));
  if IsRotated then
    AAttributes.SetValueAsFloat('Angle', Angle);
  if HasEdge then
    AAttributes.SetValueAsInteger('Edge', EdgeStyle);
  if BorderStyle <> DefaultBorderStyle then
    AAttributes.SetValueAsInteger('Border', BorderStyle);
  if (HorzTextPos <> fchpLeft) or (VertTextPos <> fcvpUp) then
    TXMLFormatHelper.WriteAligment(AAttributes, 'TextAlignment', HorzTextPos, VertTextPos);
  if (HorzImagePos <> fchpLeft) or (VertImagePos <> fcvpUp) then
    TXMLFormatHelper.WriteAligment(AAttributes, 'ImageAlignment', HorzImagePos, VertImagePos);
  TXMLFormatHelper.WriteColor(AAttributes, 'BkColor', BkColor);
  TXMLFormatHelper.WriteColor(AAttributes, 'ShapeColor', ShapeColor);

  if Tag <> 0 then
    AAttributes.SetValueAsInteger('Tag', Tag);
  if ImageIndex <> DefaultImageIndex then
    AAttributes.SetValueAsInteger('ImageIndex', ImageIndex);
  TXMLFormatHelper.WriteObjectShapeType(AAttributes, ShapeType);
  if ShapeStyle <> psSolid then
    AAttributes.SetValueAsInteger('ShapeStyle', Ord(ShapeStyle));
  if AdvancedShape <> nil then
  begin
    AAttributes.SetValueAsString('ShapeID', AdvancedShape.ID);
    AdvancedShapeInfo.WriteParameters(AAttributes);
  end;
  if ShapeWidth <> DefaultShapeWidth then
    AAttributes.SetValueAsInteger('ShapeWidth', ShapeWidth);
  if Transparent then
    AAttributes.SetValueAsBoolean('Transparent', Transparent);
  if ObjectCount > 0 then
  begin
    AObjectsNode := ANode.AddChild('Objects');
    for I := 0 to ObjectCount - 1 do
      AObjectsNode.AddChild('Object').Attributes.SetValueAsInteger('ZOrder', Objects[I].ZOrder);
  end;
  if not ParentFont then
    SaveFont(ANode.AddChild('Font'));
end;

function TdxFcObject.UserRegion(R: TRect): HRgn;
begin
  Result := 0;
end;

procedure TdxFcObject.UserLinkedPoints;
begin
end;

function TdxFcObject.AdvancedShapePointToLinkedPoint(const P: TdxPointF): TPoint;
begin
  Result := Owner.ChartPointToDisplayPoint(P);
  Result.Offset(FRealLeft, FRealTop);
  Result.Offset(Owner.LeftEdge, Owner.TopEdge);
end;

procedure TdxFcObject.AlignWithGrid;
var
  ALeft, ATop, ABottom, ARight, AWidth, AHeight: Integer;
begin
  ALeft := Left;
  ATop := Top;
  ValidatePosition(ALeft, ATop);
  ABottom := ATop + Height;
  ARight := ALeft + Width;
  ValidatePosition(ARight, ABottom);
  AWidth := ARight - ALeft;
  AHeight := ABottom - ATop;
  SetBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TdxFcObject.ValidatePosition(var X, Y: Integer);
var
  P: TPoint;
begin
  if Owner.NeedAlignObjectBoundsWithGrid then
  begin
    P := Owner.GetAlignedChartPoint(X, Y);
    X := P.X;
    Y := P.Y;
  end;
end;

procedure TdxFcObject.ValidateSize(var AWidth, AHeight: Integer);
begin
end;

{TdxFcConnectionArrow}

constructor TdxFcConnectionArrow.Create(AOwner: TdxFcConnection);
begin
  inherited Create;
  FOwner := AOwner;
  FColor := AOwner.Owner.BackgroundColor;
end;

class procedure TdxFcConnectionArrow.Initialize;
begin
  FRepository := TdxCustomFlowChart.CreateArrowShapeRepository;
end;

class procedure TdxFcConnectionArrow.Finalize;
begin
  FreeAndNil(FRepository);
end;

procedure TdxFcConnectionArrow.ClearPoints;
begin
  FPoints[0].Init(0, 0);
  FPoints[1].Init(0, 0);
end;

procedure TdxFcConnectionArrow.ChangeScale(M, D: Integer);
begin
  Height := MulDiv(Height, M, D);
  Width := MulDiv(Width, M, D);
end;

procedure TdxFcConnectionArrow.Reset;
begin
  ClearPoints;
  Owner.ArrowChanged(Self);
  Owner.Changed;
end;

procedure TdxFcConnectionArrow.Load(const AData: TdxFcArwData);
begin
  ArrowType := AData.AType;
  Color := AData.Color;
  Height := GetScaled(AData.Height);
  Width := GetScaled(AData.Width);
end;

procedure TdxFcConnectionArrow.Load(ANode: TdxXMLNode);
var
  AAttributes: TdxXMLNodeAttributes;
begin
  if ANode = nil then
    Exit;
  AAttributes := ANode.Attributes;
  Color := TXMLFormatHelper.ReadColor(AAttributes, 'Color', Owner.Color);
  ArrowType := TdxFcaType(AAttributes.GetValueAsInteger('Type'));
  Height := GetScaled(AAttributes.GetValueAsInteger('Height'));
  Width := GetScaled(AAttributes.GetValueAsInteger('Width'));
end;

procedure TdxFcConnectionArrow.Save(out AData: TdxFcArwData);
begin
  AData.AType := ArrowType;
  AData.Width := GetUnscaled(Width);
  AData.Height := GetUnscaled(Height);
  AData.Color := Color;
end;

procedure TdxFcConnectionArrow.Save(ANode: TdxXMLNode);
var
  AAttributes: TdxXMLNodeAttributes;
begin
  AAttributes := ANode.Attributes;
  if Color <> Owner.Color then
    TXMLFormatHelper.WriteColor(AAttributes, 'Color', Color);
  AAttributes.SetValueAsInteger('Type', Ord(ArrowType));
  AAttributes.SetValueAsInteger('Height', GetUnscaled(Height));
  AAttributes.SetValueAsInteger('Width', GetUnscaled(Width));
end;

procedure TdxFcConnectionArrow.SetRealBounds;
var
  S: TSize;
begin
  if FShape <> nil then
    S := FShape.GetActualSize(TSize.Create(Width, Height), Owner.PenWidth, Owner.Owner.PaintScaleFactor.ApplyF(1), False)
  else
    S.Init(MulDiv(Width, Owner.Owner.RealZoom, 100), MulDiv(Height, Owner.Owner.RealZoom, 100));
  FRealWidth := S.cx;
  FRealHeight := S.cy;
end;

function TdxFcConnectionArrow.CalculateSize(Value: TdxFcaSize): TSize;
const
  SizeScales: array[fcasSmall..fcasHuge] of Single = (0.6, 1, 1.4, 2, 4);
begin
  if (FShape <> nil) and (Value in [fcasSmall..fcasHuge]) then
  begin
    Result := FShape.GetActualSize(FShape.DefaultSize, 1, SizeScales[Value], True);
    if ArrowType > fcaRectangle then
    begin
      if not Odd(Result.cy) then
      begin
        Inc(Result.cx);
        Inc(Result.cy);
      end;
    end;
  end
  else
    Result.Init(Width, Height);
end;

procedure TdxFcConnectionArrow.SetSize(Value: TdxFcaSize);
var
  ASize: TSize;
begin
  ASize := CalculateSize(Value);
  if (ASize.cx <> Width) or (ASize.cy <> Height) then
  begin
    Owner.ArrowChanged(Self);
    FWidth := ASize.cx;
    FHeight := ASize.cy;
    SetRealBounds;
    Reset;
  end;
end;

procedure TdxFcConnectionArrow.SetArrowType(Value: TdxFcaType);
var
  ASize: TdxFcaSize;
begin
  if (FArrowType <> Value) then
  begin
    Owner.ArrowChanged(Self);
    ASize := Size;
    FArrowType := Value;
    if Value = fcaNone then
      FShape := nil
    else
    begin
      FShape := Repository.GetShapeByID(Ord(Value));
      Assert(FShape <> nil);
      if (Width = 0) and (Height = 0) then
      begin
        FWidth := FShape.DefaultSize.cx;
        FHeight := FShape.DefaultSize.cy;
      end
      else
        if ASize <> fcasCustom then
          Size := ASize;
    end;
    Reset;
  end;
end;

procedure TdxFcConnectionArrow.SetHeight(Value: Byte);
begin
  if (FHeight <> Value) then
  begin
    Owner.ArrowChanged(Self);
    FHeight := Value;
    SetRealBounds;
    Reset;
  end;
end;

procedure TdxFcConnectionArrow.SetWidth(Value: Byte);
begin
  if (FWidth <> Value) then
  begin
    Owner.ArrowChanged(Self);
    FWidth := Value;
    SetRealBounds;
    Reset;
  end;
end;

procedure TdxFcConnectionArrow.SetColor(Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    if ArrowType <> fcaNone then
      Owner.ArrowChanged(Self);
    Owner.Changed;
  end;
end;

function TdxFcConnectionArrow.GetActive: Boolean;
begin
  Result := (ArrowType <> fcaNone) and (Owner.RealCount > 1);
end;

function TdxFcConnectionArrow.GetPainter: TdxFlowChartPainter;
begin
  Result := Owner.Owner.Painter;
end;

function TdxFcConnectionArrow.DisplayRect(Ext: Boolean): TRect;
begin
  with Result do
  begin
    Left := FPoints[0].X - FRealWidth shr 1;
    Top := FPoints[0].Y - FRealHeight shr 1;
    Right := Left + FRealWidth;
    Bottom := Top + FRealHeight;
  end;
  ExtSelRect(Result, Ext, Owner.Owner.ScaleFactor);
end;

function TdxFcConnectionArrow.GetScaled(AValue: Integer): Integer;
begin
  Result := Owner.Owner.ScaleFactor.Apply(AValue);
end;

function TdxFcConnectionArrow.GetSize: TdxFcaSize;

  function SizeEquals(const ASize: TSize): Boolean;
  begin
    Result := (ASize.cx = Width) and (ASize.cy = Height);
  end;

var
  I: TdxFcaSize;
begin
  for I := Succ(fcasCustom) to High(TdxFcaSize) do
    if SizeEquals(CalculateSize(I)) then
      Exit(I);
  Result := fcasCustom;
end;

function TdxFcConnectionArrow.GetUnscaled(AValue: Integer): Integer;
begin
  Result := Owner.Owner.ScaleFactor.Revert(AValue);
end;

procedure TdxFcConnectionArrow.SetPoints(Index: Integer);
begin
  if Active then
  begin
    FPoints[0] := Owner.FRealPoints[Index];
    FPoints[1] := Owner.CalculatePrevDirectionBasedPoint(Index = 0);
  end;
  SetRealBounds;
end;

procedure TdxFcConnectionArrow.OffsetPoints(DX, DY: Integer);
var
  I: Integer;
begin
  if Active then
  begin
    for I := Low(FPoints) to Length(FPoints) - 1 do
    begin
      Inc(FPoints[I].X, DX);
      Inc(FPoints[I].Y, DY);
    end;
  end;
end;

procedure TdxFcConnectionArrow.Assign(Source: TPersistent);
begin
  if Source is TdxFcConnectionArrow then
    with TdxFcConnectionArrow(Source) do
    begin
      Self.ArrowType := ArrowType;
      Self.Color := Color;
      Self.Height := Height;
      Self.Width := Width;
    end
  else
    inherited Assign(Source);
end;

procedure TdxFcConnectionArrow.Paint(AGraphics: TdxGPGraphics);
begin
  if not Active then
    Exit;
  FShape.Draw(AGraphics, FPoints[0], FPoints[1], TSize.Create(GetUnscaled(Width), GetUnscaled(Height)),
    Owner.Owner.PaintScaleFactor.ApplyF(1), Owner.PenWidth, TdxAlphaColors.FromColor(Owner.Color), TdxAlphaColors.FromColor(Color));
end;


{ TdxFlowChartDragHelper.TDragOperation }

constructor TdxFlowChartDragHelper.TDragOperation.Create(AOwner: TdxFlowChartDragHelper);
begin
  FOwner := AOwner;
end;

procedure TdxFlowChartDragHelper.TDragOperation.CancelGuides;
begin
  Chart.Selection.Guides.Reset;
end;

function TdxFlowChartDragHelper.TDragOperation.CreateGuideController(
  AExcludedObject: TdxFcObject): TdxGuideController;
begin
  Result := TdxGuideController.Create(
    procedure (AController: TdxGuideController)
    var
      I: Integer;
      AObject: TdxFcObject;
    begin
      for I := 0 to Chart.ObjectCount - 1 do
      begin
        AObject := Chart.Objects[I];
        if AObject <> AExcludedObject then
          AController.AddItemBounds(AObject.GetBoundingRect);
      end;
    end, Chart.ObjectCount, Chart.ScaleFactor.Apply(6), Chart.ScaleFactor.Apply(10));
end;

procedure TdxFlowChartDragHelper.TDragOperation.Enter(X, Y: Integer);
begin
end;

function TdxFlowChartDragHelper.TDragOperation.CheckIsKeyPressed(const Index: Integer): Boolean;
begin
  Result := GetAsyncKeyState(Index) < 0;
end;

function TdxFlowChartDragHelper.TDragOperation.GetCancelled: Boolean;
begin
  Result := FOwner.Cancelled;
end;

function TdxFlowChartDragHelper.TDragOperation.GetChart: TdxCustomFlowChart;
begin
  Result := FOwner.Owner;
end;

function TdxFlowChartDragHelper.TDragOperation.GetGuides: TdxGuidesViewData;
begin
  Result := Chart.Selection.Guides;
end;

function TdxFlowChartDragHelper.TDragOperation.GetMouseDownPos: TPoint;
begin
  Result := Owner.MouseDownPos;
end;

function TdxFlowChartDragHelper.TDragOperation.CreateSemitransparentHelper: TdxFlowChartCustomSemitransparentDragHelper;
begin
  Result := nil;
end;

function TdxFlowChartDragHelper.TDragOperation.GetCursor: TCursor;
begin
  Result := crDefault;
end;

function TdxFlowChartDragHelper.TDragOperation.GetGuideController: TdxGuideController;
begin
  Result := nil;
end;

function TdxFlowChartDragHelper.TDragOperation.SnapToGuides: Boolean;
begin
  Result := (GuideController <> nil) and not IsAltPressed;
end;

{ TdxFlowChartDragHelper.TConnectionPointOperation }

procedure TdxFlowChartDragHelper.TConnectionPointOperation.Enter(X, Y: Integer);
begin
  FDragPointIndex := Owner.FDragPointIndex;
end;

function TdxFlowChartDragHelper.TConnectionPointOperation.GetConnection: TdxFcConnection;
begin
  Result := Owner.DragConnection;
end;

function TdxFlowChartDragHelper.TConnectionPointOperation.GetCursor: TCursor;
begin
  Result := crFlChartCross;
end;

{ TdxFlowChartDragHelper.TEndpointOperation }

procedure TdxFlowChartDragHelper.TEndpointOperation.Enter(X, Y: Integer);
begin
  inherited Enter(X, Y);

  if (FDragPointIndex = 0) and (Connection.ObjectSource <> nil) or
    (FDragPointIndex <> 0) and (Connection.ObjectDest <> nil) then
  begin
    FBase := Connection.RealPoints[FDragPointIndex];
    if FDragPointIndex = 0 then
      Connection.SetObjectSource(nil, 0)
    else
      Connection.SetObjectDest(nil, 0);
    if Chart.AutoRouting then
    begin
      Connection.ClearIntermediatePoints;
      if FDragPointIndex > 0 then
        FDragPointIndex := 1;
    end;
  end;
  Chart.StartConnectionEndpointHighlight(X, Y);
  Connection.ConnectionChanged;
  Chart.Invalidate;
end;

procedure TdxFlowChartDragHelper.TEndpointOperation.Move(X, Y: Integer);
begin
  Chart.HitTest(X, Y);
  Chart.ConnectionHighlights.Update(X, Y);
  Connection.FRealPoints[FDragPointIndex] := Chart.ConnectionHighlights.GetDestinationPoint(X, Y);
  Connection.ConnectionChanged;
  Chart.Invalidate;
end;

procedure TdxFlowChartDragHelper.TEndpointOperation.SetEndpointObject(AObject: TdxFcObject;
  ALinkedPointIndex: Integer);
begin
  Connection.FPoints.Delete(FDragPointIndex);
  Connection.FRealPoints.Delete(FDragPointIndex);
  if FDragPointIndex = 0 then
    Connection.SetObjectSource(AObject, ALinkedPointIndex)
  else
    Connection.SetObjectDest(AObject, ALinkedPointIndex);
end;

procedure TdxFlowChartDragHelper.TEndpointOperation.Leave(X, Y: Integer);
var
  ALinkedPointIndex: Integer;
  AObject: TdxFcObject;
begin
  Chart.ConnectionHighlights.Update(X, Y);
  if Chart.AutoRouting and (Chart.ConnectionHighlights.&Object <> nil) then
  begin
    AObject := Chart.ConnectionHighlights.&Object;
    SetEndpointObject(AObject, AObject.GetLinkedPoint(X, Y));
  end
  else if Chart.ConnectionHighlights.PointInfo = nil then
  begin
    Dec(FDragPointIndex, Ord(Connection.ObjectSource <> nil));
    Connection.Points[FDragPointIndex] := Chart.ChartPoint(X, Y);
  end
  else
  begin
    AObject := Chart.ConnectionHighlights.PointInfo.&Object;
    ALinkedPointIndex := Chart.ConnectionHighlights.PointInfo.Index;
    SetEndpointObject(AObject, ALinkedPointIndex);
  end;
  Chart.StopConnectionEndpointHighlight;
  Chart.FHitTestResult := [];
  Chart.Invalidate;
end;

{ TdxFlowChartDragHelper.TIntermediatePointOperation }

procedure TdxFlowChartDragHelper.TIntermediatePointOperation.Enter(X, Y: Integer);
begin
  inherited Enter(X, Y);
  FBase.X := 0;
  FMobile := Connection.FRealPoints.Points[FDragPointIndex];
end;

procedure TdxFlowChartDragHelper.TIntermediatePointOperation.Move(X, Y: Integer);
begin
  FBase.X := 1;
  SetConnectionIntermediatePoint(X, Y, FDragPointIndex);
  Chart.Invalidate;
end;

procedure TdxFlowChartDragHelper.TIntermediatePointOperation.Leave(X, Y: Integer);

  function CanRemovePoint(X, Y: Integer): Boolean;
  var
    ACurPoint: TPoint;
    P: array[0..1] of TPoint;
  begin
    Result := Cancelled or ((fcoDelOnClick in Chart.Options) and (FBase.X = 0));
    if not Result then
    begin
      ACurPoint := Chart.ChartPoint(X, Y);
      FBase := Connection.RealPoints[FDragPointIndex - 1];
      FMobile := Connection.RealPoints[FDragPointIndex + 1];
      case Connection.Style of
        fclRectH:
          Result := (ACurPoint.X = FMobile.X) or (ACurPoint.Y = FBase.Y);
        fclRectV:
          Result := (ACurPoint.X = FBase.X) or (ACurPoint.Y = FMobile.Y);
        fclStraight:
          begin
            P[0] := FBase;
            P[1] := FMobile;
            Result := PtOnLine(liStraight, P[0], Length(P),
              Screen.Width shr 9, ACurPoint.X, ACurPoint.Y);
          end;
      end;
    end;
  end;

var
  AIndex: Integer;
begin
  AIndex := FDragPointIndex;
  SetConnectionIntermediatePoint(FMobile.X, FMobile.Y, FDragPointIndex);
  if Connection.ObjectSource <> nil then
    Dec(AIndex);
  if CanRemovePoint(X, Y) then
    Connection.RemovePoint(AIndex)
  else
    Connection.Points[AIndex] := Chart.ChartPoint(X, Y);
  Chart.Invalidate;
end;

procedure TdxFlowChartDragHelper.TIntermediatePointOperation.SetConnectionIntermediatePoint(X, Y, AIndex: Integer);
begin
  Connection.FRealPoints.Points[AIndex] := Chart.ScrollPoint(Point(X, Y));
  Connection.ConnectionChanged;
end;

{ TdxFlowChartDragHelper.TMoveOperation }

procedure TdxFlowChartDragHelper.TMoveOperation.OffsetObjects(ADragStartPoint, ADragFinishPoint: TPoint; AChangeObjectBounds: Boolean);
begin
  if AChangeObjectBounds then
  begin
    ADragStartPoint := Chart.ChartPoint(ADragStartPoint);
    ADragFinishPoint := Chart.ChartPoint(ADragFinishPoint);
    Chart.OffsetObjects(ADragFinishPoint.X - ADragStartPoint.X, ADragFinishPoint.Y - ADragStartPoint.Y);
    if GuideController <> nil then
      GuideController.UpdateGuides;
  end
  else
    Owner.SemitransparentHelper.Offset(ADragFinishPoint.X - ADragStartPoint.X, ADragFinishPoint.Y - ADragStartPoint.Y);
end;

procedure TdxFlowChartDragHelper.TMoveOperation.Enter(X, Y: Integer);
begin
  FBase.Init(X, Y);
  FMobile.Init(X, Y);
  if not DynamicMoving then
    Owner.BeginControlSpecialPaint;
  FDragObjectBounds := Owner.DragObject.GetBoundingRect;
  FInitialDragObjectLocation := DragObjectBounds.TopLeft;
  FMouseOffset.Init(InitialDragObjectLocation.X - X, InitialDragObjectLocation.Y - Y);
  if Chart.UseSnapToGuides and (Chart.SelectedObject <> nil) then
    FGuideController := CreateGuideController(Chart.SelectedObject);
end;

procedure TdxFlowChartDragHelper.TMoveOperation.Move(X, Y: Integer);
var
  ADeltaX, ADeltaY, AGuideDeltaX, AGuideDeltaY: Integer;
  ABounds: TRect;
  ALocation, ANewLocation: TPoint;
begin
  ABounds := DragObjectBounds;
  FMobile.Init(X, Y);
  ANewLocation.Init(X, Y);
  ANewLocation.Offset(FMouseOffset);
  FAlignWithGrid := Chart.NeedAlignObjectBoundsWithGrid and not IsAltPressed;
  ALocation := GetChartGridPoint(ANewLocation);
  ADeltaX := ANewLocation.X - ALocation.X;
  ADeltaY := ANewLocation.Y - ALocation.Y;
  if SnapToGuides then
  begin
    ABounds.Location := ANewLocation;
    GuideController.GetMoveDeltas(ABounds, AGuideDeltaX, AGuideDeltaY);
    if (not AlignWithGrid and (AGuideDeltaX <> MaxInt)) or (Abs(AGuideDeltaX) < Abs(ADeltaX)) then
      ADeltaX := AGuideDeltaX;
    if (not AlignWithGrid and (AGuideDeltaY <> MaxInt)) or (Abs(AGuideDeltaY) < Abs(ADeltaY)) then
      ADeltaY := AGuideDeltaY;
  end;
  ANewLocation.Offset(-ADeltaX, -ADeltaY);
  try
    if ANewLocation.IsEqual(DragObjectBounds.Location) then
      Exit;
    ABounds.Location := ANewLocation;
    OffsetObjects(DragObjectBounds.Location, ABounds.Location, DynamicMoving);
    if DynamicMoving then
      ABounds := Owner.DragObject.GetBoundingRect;
    FDragObjectBounds := ABounds;
  finally
    UpdateGuides(DragObjectBounds);
  end;
end;

procedure TdxFlowChartDragHelper.TMoveOperation.UpdateGuides(const ABounds: TRect);
begin
  if GuideController <> nil then
  begin
    if IsAltPressed then
      Guides.Reset
    else
      Guides.UpdateByMoving(GuideController, ABounds);
  end;
end;

procedure TdxFlowChartDragHelper.TMoveOperation.Leave(X, Y: Integer);
begin
  CancelGuides;
  if DynamicMoving then
  begin
    if Cancelled then
      OffsetObjects(DragObjectBounds.TopLeft, InitialDragObjectLocation, True);
  end
  else
  begin
    Owner.EndControlSpecialPaint;
    if not Cancelled then
      OffsetObjects(InitialDragObjectLocation, DragObjectBounds.TopLeft, True);
  end;
  if GuideController <> nil then
    Chart.Invalidate;
  FreeAndNil(FGuideController);
end;

function TdxFlowChartDragHelper.TMoveOperation.CreateSemitransparentHelper: TdxFlowChartCustomSemitransparentDragHelper;
begin
  Result := TdxFlowChartMovingDragHelper.Create(Chart, cxDragAndDropWindowTransparency);
end;

function TdxFlowChartDragHelper.TMoveOperation.GetCursor: TCursor;
begin
  Result := crFlChartMove;
end;

function TdxFlowChartDragHelper.TMoveOperation.GetChartGridPoint(const P: TPoint): TPoint;
begin
  Result := GetChartGridPoint(P.X, P.Y)
end;

function TdxFlowChartDragHelper.TMoveOperation.GetChartGridPoint(X, Y: Integer): TPoint;
begin
  if AlignWithGrid then
    Result := Chart.GetAlignedDisplayPoint(X, Y)
  else
    Result.Init(X, Y);
end;

function TdxFlowChartDragHelper.TMoveOperation.GetGuideController: TdxGuideController;
begin
  Result := FGuideController;
end;

function TdxFlowChartDragHelper.TMoveOperation.GetDynamicMoving: Boolean;
begin
  Result := fcoDynamicMoving in Chart.Options;
end;

{ TdxFlowChartDragHelper.TRotateOperation }

procedure TdxFlowChartDragHelper.TRotateOperation.Enter(X, Y: Integer);
begin
  FObject := Owner.ObjectAt;
  FCenterPoint := FObject.DisplayRect.CenterPoint;
  FSaveAngle := FObject.Angle;
end;

procedure TdxFlowChartDragHelper.TRotateOperation.Move(X, Y: Integer);

  function GetAngleABC(const A, B, C: TPoint): Single;
  var
    AB, CB: TPoint;
    ADotProduct, ACrossProduct, Alpha: Single;
  begin
    AB.Init(B.X - A.X, B.Y - A.Y);
    CB.Init(B.X - C.X, B.Y - C.Y);
    ADotProduct := (AB.X * CB.X + AB.Y * CB.Y);
    ACrossProduct := (AB.X * CB.Y - AB.Y * CB.X);
    Alpha := ArcTan2(ACrossProduct, ADotProduct);
    Result := Alpha * 180 / PI;
  end;

var
  AAngle: Single;
begin
  AAngle := FSaveAngle + GetAngleABC(MouseDownPos, FCenterPoint, TPoint.Create(X, Y));
  if not IsAltPressed then
    AAngle := Round(AAngle / 10) * 10;
  FObject.Angle := AAngle;
end;

procedure TdxFlowChartDragHelper.TRotateOperation.Leave(X, Y: Integer);
begin
  if Cancelled then
    FObject.Angle := FSaveAngle;
end;

function TdxFlowChartDragHelper.TRotateOperation.GetCursor: TCursor;
begin
  Result := crFlChartRotation;
end;

{ TdxFlowChartDragHelper.TSelectionOperation }

procedure TdxFlowChartDragHelper.TSelectionOperation.Enter(X, Y: Integer);
begin
  FBase.Init(X, Y);
  FMobile := FBase;
  Owner.BeginControlSpecialPaint;
end;

procedure TdxFlowChartDragHelper.TSelectionOperation.Move(X, Y: Integer);
begin
  FMobile.Init(X, Y);
  Owner.SemitransparentHelper.Resize;
end;

procedure TdxFlowChartDragHelper.TSelectionOperation.Leave(X, Y: Integer);
var
  I: Integer;
  ABounds: TRect;
begin
  if not Cancelled then
  begin
    Chart.BeginUpdate;
    try
      ABounds := Owner.SemitransparentHelper.Bounds;
      for I := 0 to Chart.ObjectCount - 1 do
      begin
        if Chart.Objects[i].InRect(ABounds) then
          Chart.Objects[i].Selected := not Chart.Objects[i].Selected;
      end;
      for I := 0 to Chart.ConnectionCount - 1 do
      begin
        if Chart.Connections[i].InRect(ABounds) then
          Chart.Connections[i].Selected := not Chart.Connections[i].Selected;
      end;
    finally
      Chart.EndUpdate;
    end;
  end;
  Owner.EndControlSpecialPaint;
end;

function TdxFlowChartDragHelper.TSelectionOperation.CreateSemitransparentHelper: TdxFlowChartCustomSemitransparentDragHelper;
begin
  Result := TdxFlowChartSelectionDragHelper.Create(Chart, cxDragAndDropWindowTransparency);
end;

function TdxFlowChartDragHelper.TSelectionOperation.GetCursor: TCursor;
begin
  Result := crFlChartCross;
end;

{ TdxFlowChartDragHelper.TResizeOperation }

constructor TdxFlowChartDragHelper.TResizeOperation.Create(AOwner: TdxFlowChartDragHelper);
begin
  inherited Create(AOwner);
  FTransformMatrix := TdxGPMatrix.Create;
  FTransformMatrixInv := TdxGPMatrix.Create;
end;

function TdxFlowChartDragHelper.TResizeOperation.CreateSemitransparentHelper: TdxFlowChartCustomSemitransparentDragHelper;
begin
  Result := TdxFlowChartMovingDragHelper.Create(Chart, cxDragAndDropWindowTransparency);
end;

destructor TdxFlowChartDragHelper.TResizeOperation.Destroy;
begin
  FTransformMatrix.Free;
  FTransformMatrixInv.Free;
  inherited Destroy;
end;

procedure TdxFlowChartDragHelper.TResizeOperation.Enter(X, Y: Integer);
const
  Markers: array[0..7] of TdxResizeMarker = (
    TdxResizeMarker.TopLeft, TdxResizeMarker.Top, TdxResizeMarker.TopRight, TdxResizeMarker.Right,
    TdxResizeMarker.BottomRight, TdxResizeMarker.Bottom, TdxResizeMarker.BottomLeft, TdxResizeMarker.Left);
begin
  FObject := Chart.SelectedObject;
  FInitialObjectBounds := FObject.Bounds;
  FBounds := FObject.DisplayRect;
  FMarker := Markers[FObject.GetSelPoint(X, Y)];
  TransformMatrix.Rotate(FObject.Angle, Bounds.CenterPoint);
  TransformMatrixInv.Rotate(-FObject.Angle, Bounds.CenterPoint);
  FCapturePoint := TransformMatrixInv.TransformPoint(TPoint.Create(X, Y));
  FFixedPoint := GetDisplayedFixedPoint(Bounds, Marker);
  if not DynamicSizing then
    Owner.BeginControlSpecialPaint;
  Chart.LockUpdateChartSize;
  if Chart.UseSnapToGuides and not FObject.IsRotated then
    FGuideController := CreateGuideController(FObject);
end;

procedure TdxFlowChartDragHelper.TResizeOperation.Move(X, Y: Integer);
var
  ADeltaX, ADeltaY: Single;
  R: TdxRectF;
  P: TdxPointF;
  ANewBounds: TRect;
begin
  P := TransformMatrixInv.TransformPoint(TdxPointF.Create(X, Y));
  ADeltaX := P.X - FCapturePoint.X;
  ADeltaY := P.Y - FCapturePoint.Y;

  if IsShiftPressed then
  begin
    if Marker in [TdxResizeMarker.TopLeft, TdxResizeMarker.TopRight, TdxResizeMarker.BottomRight, TdxResizeMarker.BottomLeft] then
      CheckAspectUsingCornerHandles(ADeltaX, ADeltaY);
  end;

  R := CalculateResizingRectangle(ADeltaX, ADeltaY);
  ANewBounds := ConvertResizingRectangleToObjectBound(R);
  FObject.Bounds := ANewBounds;

  if not DynamicSizing then
    Owner.SemitransparentHelper.Resize
  else
  begin
    Chart.InvalidateRouting;
    Chart.Invalidate;
  end;
end;

procedure TdxFlowChartDragHelper.TResizeOperation.Leave(X, Y: Integer);
begin
  CancelGuides;
  Owner.EndControlSpecialPaint;
  if Cancelled then
    FObject.Bounds := FInitialObjectBounds;
  Chart.UnlockUpdateChartSize;
  Chart.SetChartSizes;
  FGuideController.Free;
  if DynamicSizing then
    Chart.Invalidate;
end;

function TdxFlowChartDragHelper.TResizeOperation.CalculateResizingRectangle(const ADeltaX, ADeltaY: Single): TdxRectF;
begin
  Result := Bounds;
  if Marker in [TdxResizeMarker.Right, TdxResizeMarker.TopRight, TdxResizeMarker.BottomRight] then
    Result.Right := Result.Right + ADeltaX;
  if Marker in [TdxResizeMarker.Top, TdxResizeMarker.TopLeft, TdxResizeMarker.TopRight] then
    Result.Top := Result.Top + ADeltaY;
  if Marker in [TdxResizeMarker.Left, TdxResizeMarker.TopLeft, TdxResizeMarker.BottomLeft] then
    Result.Left := Result.Left + ADeltaX;
  if Marker in [TdxResizeMarker.Bottom, TdxResizeMarker.BottomLeft, TdxResizeMarker.BottomRight] then
    Result.Bottom := Result.Bottom + ADeltaY;
end;

function TdxFlowChartDragHelper.TResizeOperation.ConvertResizingRectangleToObjectBound(
  const ARectangle: TdxRectF): TRect;
var
  ANewBounds: TdxRectF;
  ANewFixedPoint: TdxPointF;
begin
  ANewBounds := ARectangle;
  ANewBounds.Normalize;
  FActualMarker := GetActualMarker(ARectangle);
  ValidateBoundsAlignment(ANewBounds, FActualMarker);
  ANewFixedPoint := GetDisplayedFixedPoint(ANewBounds, FActualMarker);
  ANewBounds.Offset(FFixedPoint.X - ANewFixedPoint.X, FFixedPoint.Y - ANewFixedPoint.Y);
  Result := Chart.GetItemBounds(TRect.Round(ANewBounds));
end;

procedure TdxFlowChartDragHelper.TResizeOperation.CheckAspectUsingCornerHandles(var ADeltaX, ADeltaY: Single);
var
  ARatio: Single;
  ARect: TdxRectF;
  ASign: Integer;
begin
  if (Bounds.Width = 0) or (Bounds.Height = 0) then
    Exit;

  ARatio := Bounds.Height / Bounds.Width;
  ASign := IfThen(Marker in [TdxResizeMarker.BottomLeft, TdxResizeMarker.TopRight], -1, 1);
  ARect := CalculateResizingRectangle(ADeltaX, ASign * ADeltaX * ARatio);

  case Marker of
    TdxResizeMarker.TopLeft,
    TdxResizeMarker.TopRight:
      ARect.Top := ARect.Top + CapturePoint.Y - Bounds.Top;
    TdxResizeMarker.BottomLeft,
    TdxResizeMarker.BottomRight:
      ARect.Bottom := ARect.Bottom + CapturePoint.Y - Bounds.Bottom;
  end;

  if (CapturePoint.Y + ADeltaY < ARect.Top) or (CapturePoint.Y + ADeltaY > ARect.Bottom) then
    ADeltaX := ASign * ADeltaY / ARatio
  else
    ADeltaY := ASign * ADeltaX * ARatio;
end;

function TdxFlowChartDragHelper.TResizeOperation.GetActualMarker(const ABounds: TdxRectF): TdxResizeMarker;
begin
  Result := Marker;
  if (ABounds.Height < 0) and (ABounds.Width < 0) then
  begin
    case Marker of
      TdxResizeMarker.TopLeft: Result := TdxResizeMarker.BottomRight;
      TdxResizeMarker.Top: Result := TdxResizeMarker.Bottom;
      TdxResizeMarker.TopRight: Result := TdxResizeMarker.BottomLeft;
      TdxResizeMarker.Left: Result := TdxResizeMarker.Right;
      TdxResizeMarker.Right: Result := TdxResizeMarker.Left;
      TdxResizeMarker.BottomRight: Result := TdxResizeMarker.TopLeft;
      TdxResizeMarker.Bottom: Result := TdxResizeMarker.Top;
      TdxResizeMarker.BottomLeft: Result := TdxResizeMarker.TopRight;
    end;
  end
  else
  begin
    if ABounds.Height < 0 then
    begin
      case Marker of
        TdxResizeMarker.TopLeft: Result := TdxResizeMarker.BottomLeft;
        TdxResizeMarker.Top: Result := TdxResizeMarker.Bottom;
        TdxResizeMarker.TopRight: Result := TdxResizeMarker.BottomRight;
        TdxResizeMarker.BottomRight: Result := TdxResizeMarker.TopRight;
        TdxResizeMarker.Bottom: Result := TdxResizeMarker.Top;
        TdxResizeMarker.BottomLeft: Result := TdxResizeMarker.TopLeft;
      end;
    end
    else
      if ABounds.Width < 0 then
      begin
        case Marker of
          TdxResizeMarker.TopLeft: Result := TdxResizeMarker.TopRight;
          TdxResizeMarker.TopRight: Result := TdxResizeMarker.TopLeft;
          TdxResizeMarker.Left: Result := TdxResizeMarker.Right;
          TdxResizeMarker.Right: Result := TdxResizeMarker.Left;
          TdxResizeMarker.BottomRight: Result := TdxResizeMarker.BottomLeft;
          TdxResizeMarker.BottomLeft: Result := TdxResizeMarker.BottomRight;
        end;
      end
  end;
end;

function TdxFlowChartDragHelper.TResizeOperation.GetDisplayedFixedPoint(const ABounds: TdxRectF; AMarker: TdxResizeMarker): TdxPointF;
begin
  case AMarker of
    TdxResizeMarker.TopLeft:
      Result := ABounds.BottomRight;
    TdxResizeMarker.Top:
      Result := TdxPointF.Create((ABounds.Left + ABounds.Right) / 2, ABounds.Bottom);
    TdxResizeMarker.TopRight:
      Result := ABounds.BottomLeft;
    TdxResizeMarker.Right:
      Result := TdxPointF.Create(ABounds.Left, (ABounds.Top + ABounds.Bottom) / 2);
    TdxResizeMarker.Left:
      Result := TdxPointF.Create(ABounds.Right, (ABounds.Top + ABounds.Bottom) / 2);
    TdxResizeMarker.BottomRight:
      Result := ABounds.TopLeft;
    TdxResizeMarker.Bottom:
      Result := TdxPointF.Create((ABounds.Left + ABounds.Right) / 2, ABounds.Top);
  else
    Result := ABounds.TopRight;
  end;
  TransformMatrix.Reset;
  TransformMatrix.Rotate(FObject.Angle, ABounds.CenterPoint);
  Result := TransformMatrix.TransformPoint(Result);
end;

function TdxFlowChartDragHelper.TResizeOperation.NeedAlignWithGrid: Boolean;
begin
  Result := Chart.NeedAlignObjectBoundsWithGrid and not IsAltPressed;
end;

procedure TdxFlowChartDragHelper.TResizeOperation.ValidateBoundsAlignment(var ANewBounds: TdxRectF; AMarker: TdxResizeMarker);
var
  ALeftTop, ABottomRight: TPoint;
  ABounds: TRect;
begin
  ABounds := TRect.Round(ANewBounds);
  if (ABounds.Width = 0) or (ABounds.Height = 0) then
    Exit;

  if not NeedAlignWithGrid then
  begin
    ALeftTop := ABounds.TopLeft;
    ABottomRight := ABounds.BottomRight;
    if SnapToGuides then
      GuideController.GetResizePoints(ABounds, AMarker, ALeftTop, ABottomRight);
  end
  else
  begin
    ALeftTop := Chart.GetAlignedDisplayPoint(ABounds.Left, ABounds.Top);
    ABottomRight := Chart.GetAlignedDisplayPoint(ABounds.Right, ABounds.Bottom);
    if SnapToGuides then
      GuideController.GetSnappedResizePoints(ABounds, AMarker, ALeftTop, ABottomRight);
  end;
  case AMarker of
    TdxResizeMarker.Left:
      ANewBounds.Left := Min(ANewBounds.Right, ALeftTop.X);
    TdxResizeMarker.Right:
      ANewBounds.Right := Max(ANewBounds.Left, ABottomRight.X);
    TdxResizeMarker.Top:
      ANewBounds.Top := Min(ANewBounds.Bottom, ALeftTop.Y);
    TdxResizeMarker.Bottom:
      ANewBounds.Bottom := Max(ANewBounds.Top, ABottomRight.Y);
  end;

  try
    if IsShiftPressed then
      Exit;

    if AMarker in [TdxResizeMarker.TopLeft, TdxResizeMarker.BottomLeft] then
      ANewBounds.Left := Min(ANewBounds.Right, ALeftTop.X);
    if AMarker in [TdxResizeMarker.TopLeft, TdxResizeMarker.TopRight] then
      ANewBounds.Top := Min(ANewBounds.Bottom, ALeftTop.Y);
    if AMarker in [TdxResizeMarker.BottomRight, TdxResizeMarker.TopRight] then
      ANewBounds.Right := Max(ANewBounds.Left, ABottomRight.X);
    if AMarker in [TdxResizeMarker.BottomLeft, TdxResizeMarker.BottomRight] then
      ANewBounds.Bottom := Max(ANewBounds.Top, ABottomRight.Y);
  finally
    if GuideController <> nil then
    begin
      if SnapToGuides then
        Chart.Selection.Guides.UpdateByResizing(GuideController, TRect.Round(ANewBounds), AMarker)
      else
        Chart.Selection.Guides.Reset;
    end;
  end;
end;

function TdxFlowChartDragHelper.TResizeOperation.GetDynamicSizing: Boolean;
begin
  Result := fcoDynamicSizing in Chart.Options;
end;

function TdxFlowChartDragHelper.TResizeOperation.GetGuideController: TdxGuideController;
begin
  Result := FGuideController;
end;

function TdxFlowChartDragHelper.TResizeOperation.ContentBounds: TdxRectF;
begin
  Result := FObject.DisplayRect;
end;

{ TdxFlowChartDragHelper.TParameterOperation }

procedure TdxFlowChartDragHelper.TParameterOperation.Enter(X, Y: Integer);
var
  L: Integer;
begin
  FObject := Owner.ObjectAt;
  FParameterIndex := Chart.FHitParameterIndex;
  L := Length(FObject.AdvancedShapeInfo.Parameters);
  SetLength(FSaveParameters, L);
  TArray.Copy<Single>(FObject.AdvancedShapeInfo.Parameters, FSaveParameters, L);
end;

function TdxFlowChartDragHelper.TParameterOperation.GetCursor: TCursor;
begin
  Result := crFlChartCross;
end;

procedure TdxFlowChartDragHelper.TParameterOperation.Leave(X, Y: Integer);
begin
  if Cancelled then
    FObject.AdvancedShapeInfo.Parameters := FSaveParameters
  else
  begin
    FObject.CalculateLinkedPoints;
    FObject.UpdateConnections;
  end;
end;

procedure TdxFlowChartDragHelper.TParameterOperation.Move(X, Y: Integer);
var
  APoint: TdxPointF;
  ABounds: TRect;
  AMatrix: TdxGPMatrix;
  AFactor: Double;
begin
  ABounds := FObject.DisplayRect;
  if FObject.IsRotated then
  begin
    AMatrix := TdxGPMatrix.Create;
    AMatrix.Rotate(-FObject.Angle, TdxPointF.Create(ABounds.Left + ABounds.Width / 2, ABounds.Top + ABounds.Height / 2));
  end
  else
    AMatrix := nil;
  try
    APoint.Init(X, Y);
    if AMatrix <> nil then
      APoint := AMatrix.TransformPoint(APoint);
    APoint.Offset(-ABounds.Left, -ABounds.Top);
    AFactor := 100 / Chart.RealZoom;
    FObject.AdvancedShapeInfo.ChangeParameter(FParameterIndex,
      TdxPointDouble.Create(APoint.X * AFactor, APoint.Y * AFactor));
  finally
    AMatrix.Free;
  end;
end;

{ TdxFlowChartDragHelper }

constructor TdxFlowChartDragHelper.Create(AOwner: TdxCustomFlowChart);
begin
  inherited Create;
  FOwner := AOwner;
end;

destructor TdxFlowChartDragHelper.Destroy;
begin
  EndControlSpecialPaint;
  inherited Destroy;
end;

procedure TdxFlowChartDragHelper.DragStart(X, Y: Integer; AKind: TFlowChartDragKind);
var
  ACursor: TCursor;
begin
  FCancelled := False;
  FDragging := True;
  FDragKind := AKind;
  Assert(AKind <> TFlowChartDragKind.None);
  FOperation := GetOperationClass.Create(Self);
  ACursor := FOperation.Cursor;
  if ACursor <> crDefault then
    Windows.SetCursor(Screen.Cursors[ACursor]);
  FMouseDownPos.Init(X, Y);
  FDragObject := Owner.ObjectAt;
  FOperation.Enter(X, Y);
end;

procedure TdxFlowChartDragHelper.DragStop(X, Y: Integer);
begin
  FDragKind := TFlowChartDragKind.None;
  if Dragging then
  begin
    FDragging := False;
    FOperation.Leave(X, Y);
  end;
  FreeAndNil(FOperation);
end;

procedure TdxFlowChartDragHelper.SetupDragConnection(AConnection: TdxFcConnection; AIndex: Integer);
begin
  FDragConnection := AConnection;
  FDragPointIndex := AIndex;
end;

function TdxFlowChartDragHelper.GetOperationClass: TDragOperationClass;
begin
  case DragKind of
    TFlowChartDragKind.Endpoint:
      Result := TEndpointOperation;
    TFlowChartDragKind.IntermediatePoint:
      Result := TIntermediatePointOperation;
    TFlowChartDragKind.Move:
      Result := TMoveOperation;
    TFlowChartDragKind.Resize:
      Result := TResizeOperation;
    TFlowChartDragKind.Selection:
      Result := TSelectionOperation;
    TFlowChartDragKind.Rotate:
      Result := TRotateOperation;
    TFlowChartDragKind.Parameter:
      Result := TParameterOperation;
  else
    Result := nil;
  end;
end;

procedure TdxFlowChartDragHelper.BeginControlSpecialPaint;
begin
  FSemitransparentHelper := FOperation.CreateSemitransparentHelper;
  if FSemitransparentHelper = nil then
    raise EInvalidOperation.Create('CreateSemitransparentHelper failed!');
  FSemitransparentHelper.BeginSpecialPaint;
end;

procedure TdxFlowChartDragHelper.EndControlSpecialPaint;
begin
  FreeAndNil(FSemitransparentHelper);
end;

function TdxFlowChartDragHelper.CanAddNewPointToConnection: Boolean;
begin
  Result := not (fcoPreventAddingElbowConnections in Owner.Options) and not Owner.AutoRouting;
end;

procedure TdxFlowChartDragHelper.CancelDrag;
begin
  FCancelled := True;
  DragStop(FMouseDownPos.X, FMouseDownPos.Y);
end;

procedure TdxFlowChartDragHelper.ToggleSysKey(AKey: Word);
var
  P: TPoint;
begin
  if FOperation <> nil then
  begin
    P := Owner.ScreenToClient(GetMouseCursorPos);
    FOperation.Move(P.X, P.Y);
  end;
end;

procedure TdxFlowChartDragHelper.CheckDragStart(X, Y: Integer;
  const AShift: TShiftState; const AHitTest: TdxFcHitTest);

  function CheckNewConnectionDragMode: Boolean;
  var
    APoint: Byte;
  begin
    Result := Owner.CanCreateNewConnectionOnDrag;
    if Result then
    begin
      FDragPointIndex := 1;
      APoint := ObjectAt.GetLinkedPoint(X, Y);
      FDragConnection := Owner.CreateConnection(ObjectAt, ObjectAt, APoint, APoint);
      DragStart(X, Y, TFlowChartDragKind.Endpoint);
    end;
  end;

  procedure CheckPointDragStart;
  begin
    if Owner.SelectedObjectCount <> 1 then
    begin
      FDragConnection := ConnectionAt;
      FDragPointIndex := FDragConnection.GetNearestPoint(X, Y);
      if FDragConnection.ObjectSource <> nil then
        Inc(FDragPointIndex);
      if (FDragPointIndex = 0) or (FDragPointIndex = FDragConnection.RealCount - 1) then
        DragStart(X, Y, TFlowChartDragKind.Endpoint)
      else
        DragStart(X, Y, TFlowChartDragKind.IntermediatePoint);
    end
    else
      DragStart(X, Y, TFlowChartDragKind.Resize);
  end;

  procedure AddNewConnectionPoint;
  begin
    ConnectionAt.NewPoint(X, Y);
    DragStart(X, Y, TFlowChartDragKind.IntermediatePoint)
  end;

begin
  if ssLeft in AShift then
  begin
    if (htOnParameter in AHitTest) and ObjectAt.Selected then
      DragStart(X, Y, TFlowChartDragKind.Parameter)
    else if (htOnRotateMark in AHitTest) and ObjectAt.Selected then
      DragStart(X, Y, TFlowChartDragKind.Rotate)
    else if (fcoCanDrag in Owner.Options) and not CheckNewConnectionDragMode then
    begin
      if htOnSelPoint in AHitTest then
        CheckPointDragStart
      else if (htOnObject in AHitTest) and ObjectAt.Selected then
        DragStart(X, Y, TFlowChartDragKind.Move)
      else if (htOnConnection in AHitTest) and (ConnectionAt = Owner.SelectedConnection) and CanAddNewPointToConnection then
        AddNewConnectionPoint
      else if (AHitTest = [htNowhere]) and ([fcoMultiSelect, fcoCanSelect] * Owner.Options = [fcoMultiSelect, fcoCanSelect]) then
        DragStart(X, Y, TFlowChartDragKind.Selection)
    end;
  end;
end;

procedure TdxFlowChartDragHelper.DragMove(X, Y: Integer);
begin
  if Dragging then
    FOperation.Move(X, Y);
end;

function TdxFlowChartDragHelper.IsDraggingConnection(AConnection: TdxFcConnection): Boolean;
begin
  Result := (DragKind = TFlowChartDragKind.IntermediatePoint) and (DragConnection = AConnection);
end;

procedure TdxFlowChartDragHelper.PaintControl(ACanvas: TcxCanvas);
begin
  FSemitransparentHelper.PaintControl(ACanvas);
end;

procedure TdxFlowChartDragHelper.UpdateControlImage;
begin
  if SpecialControlPainting then
    FSemitransparentHelper.UpdateBackground;
end;

function TdxFlowChartDragHelper.GetBase: TPoint;
begin
  Result := FOperation.Base;
end;

function TdxFlowChartDragHelper.GetCanvas: TcxCanvas;
begin
  Result := FOwner.Canvas;
end;

function TdxFlowChartDragHelper.GetConnectionAt: TdxFcConnection;
begin
  Result := FOwner.FConnectionAt;
end;

function TdxFlowChartDragHelper.GetMobile: TPoint;
begin
  Result := FOperation.Mobile;
end;

function TdxFlowChartDragHelper.GetObjectAt: TdxFcObject;
begin
  Result := FOwner.FObjectAt;
end;

function TdxFlowChartDragHelper.GetSpecialControlPainting: Boolean;
begin
  Result := (FSemitransparentHelper <> nil) and FSemitransparentHelper.SpecialControlPainting;
end;


{ TdxFlowChartCustomSemitransparentDragHelper }

procedure TdxFlowChartCustomSemitransparentDragHelper.AfterSelectionPaint(AGraphics: TdxGPGraphics);
begin
  inherited;
  Chart.Selection.DrawGuides(AGraphics);
end;

function TdxFlowChartCustomSemitransparentDragHelper.GetBounds: TRect;
begin
  Result := GetSelectionBounds;
end;

function TdxFlowChartCustomSemitransparentDragHelper.GetChart: TdxCustomFlowChart;
begin
  Result := TdxCustomFlowChart(Control);
end;

function TdxFlowChartCustomSemitransparentDragHelper.GetDragHelper: TdxFlowChartDragHelper;
begin
  Result := Chart.DragHelper;
end;

{ TdxFlowChartMovingDragHelper }

function TdxFlowChartMovingDragHelper.GetObjectsToPaint: TArray<TdxFcObject>;

  procedure CheckObject(AObject: TdxFcObject; ASet: TdxHashSet<TdxFcObject>);
  var
    I: Integer;
  begin
    ASet.Include(AObject);
    if AObject.IsUnion then
      for I := 0 to AObject.ObjectCount - 1 do
        CheckObject(AObject.Objects[I], ASet);
  end;

var
  I: Integer;
  AObjects: TdxHashSet<TdxFcObject>;
begin
  AObjects := TdxHashSet<TdxFcObject>.Create;
  try
    for I := 0 to Chart.SelectedObjectCount - 1 do
      CheckObject(Chart.SelectedObjects[I], AObjects);
    Result := AObjects.ToArray;
  finally
    AObjects.Free;
  end;
end;

function TdxFlowChartMovingDragHelper.GetSelectionBounds: TRect;

  function GetObjectPaintingBounds(AObject: TdxFcObject): TRect;
  begin
    Result := AObject.InvalidateRect;
  end;

var
  AObject: TdxFcObject;
begin
  Result.Init(MaxInt div 2, MaxInt div 2, MinInt div 2, MinInt div 2);
  for AObject in FObjectsToPaint do
    Result.Union(GetObjectPaintingBounds(AObject));
end;

procedure TdxFlowChartMovingDragHelper.PaintControlSelection(ACanvas: TcxCanvas);
var
  AObject: TdxFcObject;
begin
  Chart.BeginDragObjectsPaint(ACanvas);
  try
    for AObject in FObjectsToPaint do
      AObject.Paint(ACanvas);
  finally
    Chart.EndDragObjectsPaint;
  end;
end;

procedure TdxFlowChartMovingDragHelper.BeginSpecialPaint;
begin
  FObjectsToPaint := GetObjectsToPaint;
  inherited BeginSpecialPaint;
end;

{ TdxFlowChartSelectionDragHelper }

function TdxFlowChartSelectionDragHelper.GetSelectionBounds: TRect;
begin
  Result.Init(DragHelper.Base, DragHelper.Mobile, True);
  Result.Width := Max(Result.Width, 1);
  Result.Height := Max(Result.Height, 1);
end;

procedure TdxFlowChartSelectionDragHelper.PaintControlSelection(ACanvas: TcxCanvas);
var
  AGraphics: TdxGPGraphics;
  APen: TdxGPPen;
  ABrush: TdxGPBrush;
  R: TRect;
begin
  R := SelectionsBounds;
  AGraphics := dxGpBeginPaint(ACanvas.Handle, R);
  try
    APen := TdxGPPen.Create(TdxAlphaColors.Thistle, 2);
    ABrush := TdxGPBrush.Create;
    ABrush.Color := TdxAlphaColors.Lavender;
    try
      AGraphics.SmoothingMode := smAntiAlias;
      AGraphics.Rectangle(R, APen, ABrush);
    finally
      ABrush.Free;
      APen.Free;
    end;
  finally
    dxGpEndPaint(AGraphics);
  end;
end;

procedure InitializeCursors;
begin
  Screen.Cursors[crFlChartCross] := LoadCursor(HInstance, 'CROSS');
  Screen.Cursors[crFlChartDrawFreeConnector] := LoadCursor(HInstance, 'DRAWFREECONNECTOR');
  Screen.Cursors[crFlChartArrow] := LoadCursor(HInstance, 'ARROW');
  Screen.Cursors[crFlChartCopy] := LoadCursor(HInstance, 'COPY');
  Screen.Cursors[crFlChartCreateRectangle] := LoadCursor(HInstance, 'CREATERECTANGLE');
  Screen.Cursors[crFlChartDragRemove] := LoadCursor(HInstance, 'DRAGREMOVE');
  Screen.Cursors[crFlChartDrawConnectedConnector] := LoadCursor(HInstance, 'DRAWCONNECTEDCONNECTOR');
  Screen.Cursors[crFlChartHandDragPreview] := LoadCursor(HInstance, 'HANDDRAGPREVIEW');
  Screen.Cursors[crFlChartHandPreview] := LoadCursor(HInstance, 'HANDPREVIEW');
  Screen.Cursors[crFlChartHoverRotation] := LoadCursor(HInstance, 'HOVERROTATION');
  Screen.Cursors[crFlChartMove] := LoadCursor(HInstance, 'MOVE');
  Screen.Cursors[crFlChartNo] := LoadCursor(HInstance, 'NO');
  Screen.Cursors[crFlChartRotation] := LoadCursor(HInstance, 'ROTATION');
  Screen.Cursors[crFlChartSizeAll] := LoadCursor(HInstance, 'SIZEALL');
  Screen.Cursors[crFlChartSizeNESW] := LoadCursor(HInstance, 'SIZENESW');
  Screen.Cursors[crFlChartSizeNS] := LoadCursor(HInstance, 'SIZENS');
  Screen.Cursors[crFlChartSizeNWSE] := LoadCursor(HInstance, 'SIZENWSE');
  Screen.Cursors[crFlChartSizeWE] := LoadCursor(HInstance, 'SIZEWE');
end;

procedure Initialize;
begin
  InitializeCursors;
  TdxFcConnectionArrow.Initialize;
  dxFlowChartCustomizeFormManager := TdxFlowChartCustomizeFormManager.Create;
  TdxFcSelection.InitializeImages;
end;

procedure Finalize;
begin
  FreeAndNil(dxFlowChartCustomizeFormManager);
  TdxFcSelection.RotationImage.Free;
  TdxFcConnectionArrow.Finalize;
end;

{ TdxFlowChartEditorOptions }

class function TdxFlowChartEditorOptions.CreateLegacyOptions: TdxFlowChartEditorOptions;
begin
  Result := TdxFlowChartEditorOptions.Create;
  Result.UseAdvancedShapesOnly := False;
end;

class function TdxFlowChartEditorOptions.CreateOptions: TdxFlowChartEditorOptions;
begin
  Result := TdxFlowChartEditorOptions.Create;
  Result.UseAdvancedShapesOnly := True;
end;

initialization
  dxUnitsLoader.AddUnit(@Initialize, @Finalize);

finalization
  dxUnitsLoader.RemoveUnit(@Finalize);

end.

