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

unit dxFlowChartShapes;

interface

{$I cxVer.inc}
{$SCOPEDENUMS ON}

uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, dxCoreGraphics, dxGDIPlusAPI, dxGDIPlusClasses, dxXMLDoc,
  cxGeometry, dxEvaluator;

type
  TdxSweepDirectionKind = (Counterclockwise, Clockwise);

  TdxFlowChartAdvancedShapeRepository = class;
  TdxFlowChartAdvancedShapeStencil = class;

  { TdxShapePathProperties }

  TdxShapePathProperties = class
  strict private
    FStrokeLineJoin: TdxGpLineJoin;
    FFillBrightness: Single;
    FFillColor: TdxNullableValue<TdxAlphaColor>;
    FStrokeColor: TdxNullableValue<TdxAlphaColor>;
    FStrokeThickness: TdxNullableValue<Single>;
    FStrokeDashArray: TArray<Single>;
    FTransparent: Boolean;
  public
    constructor Create(ATransparent: Boolean = False);
    property StrokeLineJoin: TdxGpLineJoin read FStrokeLineJoin write FStrokeLineJoin;
    property FillBrightness: Single read FFillBrightness write FFillBrightness;
    property FillColor: TdxNullableValue<TdxAlphaColor> read FFillColor write FFillColor;
    property StrokeColor: TdxNullableValue<TdxAlphaColor> read FStrokeColor write FStrokeColor;
    property StrokeThickness: TdxNullableValue<Single> read FStrokeThickness write FStrokeThickness;
    property StrokeDashArray: TArray<Single> read FStrokeDashArray write FStrokeDashArray;
    property Transparent: Boolean read FTransparent write FTransparent;
  end;

  { TdxShapeElementViewData }

  TdxShapeElementViewData = class
  strict private
    FPath: TdxGPPath;
    FProperties: TdxShapePathProperties;
    function GetFillBrightness: Single; inline;
    function GetFillColor: TdxNullableValue<TdxAlphaColor>; inline;
    function GetStrokeColor: TdxNullableValue<TdxAlphaColor>; inline;
    function GetStrokeDashArray: TArray<Single>; inline;
    function GetStrokeLineJoin: TdxGpLineJoin; inline;
    function GetStrokeThickness: TdxNullableValue<Single>; inline;
  private
    function GetTransparent: Boolean;
  public
    constructor Create(APath: TdxGPPath; AProperties: TdxShapePathProperties);
    destructor Destroy; override;

    property Path: TdxGPPath read FPath;
    property StrokeLineJoin: TdxGpLineJoin read GetStrokeLineJoin;
    property FillBrightness: Single read GetFillBrightness;
    property FillColor: TdxNullableValue<TdxAlphaColor> read GetFillColor;
    property StrokeColor: TdxNullableValue<TdxAlphaColor> read GetStrokeColor;
    property StrokeThickness: TdxNullableValue<Single> read GetStrokeThickness;
    property StrokeDashArray: TArray<Single> read GetStrokeDashArray;
    property Transparent: Boolean read GetTransparent;
  {$IFNDEF DELPHIXE}
    property Properties: TdxShapePathProperties read FProperties;
  {$ENDIF}
  end;

  { TdxShapeViewData }

  TdxShapeViewData = class(TObjectList<TdxShapeElementViewData>);

  { TdxShapeDescription }

  TdxShapeDescription = class
  strict private
    FDefaultSize: TdxSizeF;
    FID: string;
    FUseBackgroundAsForeground: Boolean;
  public
    constructor Create(const AID: string; const ADefaultSize: TSize; AUseBackgroundAsForeground: Boolean); overload;

    function CreateViewData(const ASize: TdxSizeF; const AParameters: TArray<Single>): TdxShapeViewData; virtual; abstract;
    function GetConnectionPoints(const ASize: TdxSizeF; const AParameters: TArray<Single>): TArray<TdxPointF>; virtual; abstract;
    function GetNormalizedParameters(const ASize: TdxSizeF; const AParameters: TArray<Single>): TArray<Single>; virtual; abstract;
    function GetParameterDefaultValue(AParameterIndex: Integer): Single; virtual; abstract;
    function GetParameterPoint(AParameterIndex: Integer; const ASize: TdxSizeF; const AParameters: TArray<Single>; AValue: Single): TdxPointF; virtual; abstract;
    function GetParameterValue(AParameterIndex: Integer; const ASize: TdxSizeF; const AParameters: TArray<Single>; const ALocalPoint: TdxPointF): Single; virtual; abstract;
    function GetParameters(const ASize: TdxSizeF; const AParameters: TArray<Single>): TArray<Single>; virtual; abstract;
    procedure PaintDefaultShape(AGraphics: TdxGPCanvas; const ABounds: TRect; APenColor, ABrushColor: TdxAlphaColor); virtual;

    property DefaultSize: TdxSizeF read FDefaultSize write FDefaultSize;
    property ID: string read FID write FID;
    property UseBackgroundAsForeground: Boolean read FUseBackgroundAsForeground write FUseBackgroundAsForeground;
  end;

  { TdxFlowChartObjectAdvancedShape }

  TdxFlowChartObjectAdvancedShape = class
  strict private
    FLegacyReplacement: Boolean;
    FStencil: TdxFlowChartAdvancedShapeStencil;
    function GetDefaultSize: TSize;
    function GetID: string;
  protected
    FShape: TdxShapeDescription;
    constructor Create(AStencil: TdxFlowChartAdvancedShapeStencil; AShape: TdxShapeDescription; ALegacyReplacement: Boolean = False);
    function GetCaption: string; virtual;
    procedure SetCaption(const Value: string); virtual;

    property LegacyReplacement: Boolean read FLegacyReplacement;
    property Stencil: TdxFlowChartAdvancedShapeStencil read FStencil;
  public
    property Caption: string read GetCaption write SetCaption;
    property DefaultSize: TSize read GetDefaultSize;
    property ID: string read GetID;
  end;

  { TdxFlowChartAdvancedShapeStencil }

  TdxFlowChartAdvancedShapeStencil = class
  strict private
    FCaption: Pointer;
    FCustomCaption: string;
    FID: string;
    FPredefined: Boolean;
    FOwner: TdxFlowChartAdvancedShapeRepository;
    FShapes: TObjectList<TdxFlowChartObjectAdvancedShape>;
    FShapeDescriptions: TObjectDictionary<string, TdxShapeDescription>;
    FVisible: Boolean;
    function GetCaption: string;
    function GetCount: Integer;
    function GetShape(Index: Integer): TdxFlowChartObjectAdvancedShape;
    procedure SetCaption(const Value: string);
  protected
    constructor Create(AOwner: TdxFlowChartAdvancedShapeRepository; const AResourceName: string; ACaption: Pointer); overload;
    function AddPredefinedShape(const AShapeID: string; ACaption: Pointer; ALegacyReplacement: Boolean = False): TdxFlowChartObjectAdvancedShape; overload;
    function AddPredefinedShape(AShapeDescription: TdxShapeDescription; ACaption: Pointer; ALegacyReplacement: Boolean = False): TdxFlowChartObjectAdvancedShape; overload;
    function AddUserShape(AShapeID: string; const ACaption: string): TdxFlowChartObjectAdvancedShape;
    procedure CreateShapes; virtual;
    procedure RemoveShape(AShape: TdxFlowChartObjectAdvancedShape);

    property Owner: TdxFlowChartAdvancedShapeRepository read FOwner;
    property ShapeDescriptions: TObjectDictionary<string, TdxShapeDescription> read FShapeDescriptions;
  public
    constructor Create(const AID, ACaption: string); overload;
    destructor Destroy; override;
    function HasVisibleShapes: Boolean; // for internal use

    property Caption: string read GetCaption write SetCaption;
    property Count: Integer read GetCount;
    property ID: string read FID;
    property Shapes[Index: Integer]: TdxFlowChartObjectAdvancedShape read GetShape; default;
    property Visible: Boolean read FVisible write FVisible;
  end;

  { TdxFlowChartBasicShapes }

  TdxFlowChartBasicShapes = class(TdxFlowChartAdvancedShapeStencil)
  strict private
    FRectangle: TdxFlowChartObjectAdvancedShape;
    FEllipse: TdxFlowChartObjectAdvancedShape;
    FTriangle: TdxFlowChartObjectAdvancedShape;
    FRightTriangle: TdxFlowChartObjectAdvancedShape;
    FPentagon: TdxFlowChartObjectAdvancedShape;
    FHexagon: TdxFlowChartObjectAdvancedShape;
    FHeptagon: TdxFlowChartObjectAdvancedShape;
    FOctagon: TdxFlowChartObjectAdvancedShape;
    FDecagon: TdxFlowChartObjectAdvancedShape;
    FCan: TdxFlowChartObjectAdvancedShape;
    FParallelogram: TdxFlowChartObjectAdvancedShape;
    FTrapezoid: TdxFlowChartObjectAdvancedShape;
    FDiamond: TdxFlowChartObjectAdvancedShape;
    FCross: TdxFlowChartObjectAdvancedShape;
    FChevron: TdxFlowChartObjectAdvancedShape;
    FCube: TdxFlowChartObjectAdvancedShape;
    FStar4: TdxFlowChartObjectAdvancedShape;
    FStar5: TdxFlowChartObjectAdvancedShape;
    FStar6: TdxFlowChartObjectAdvancedShape;
    FStar7: TdxFlowChartObjectAdvancedShape;
    FStar16: TdxFlowChartObjectAdvancedShape;
    FStar24: TdxFlowChartObjectAdvancedShape;
    FStar32: TdxFlowChartObjectAdvancedShape;
    FRoundedRectangleShape: TdxFlowChartObjectAdvancedShape;
    FSingleSnipCornerRectangleShape: TdxFlowChartObjectAdvancedShape;
    FSnipSameSideCornerRectangleShape: TdxFlowChartObjectAdvancedShape;
    FSnipDiagonalCornerRectangleShape: TdxFlowChartObjectAdvancedShape;
    FSingleRoundCornerRectangleShape: TdxFlowChartObjectAdvancedShape;
    FRoundSameSideCornerRectangleShape: TdxFlowChartObjectAdvancedShape;
    FRoundDiagonalCornerRectangleShape: TdxFlowChartObjectAdvancedShape;
    FSnipAndRoundSingleCornerRectangleShape:TdxFlowChartObjectAdvancedShape;
    FSnipCornerRectangleShape: TdxFlowChartObjectAdvancedShape;
    FRoundCornerRectangleShape: TdxFlowChartObjectAdvancedShape;
    FSnipAndRoundCornerRectangleShape: TdxFlowChartObjectAdvancedShape;
    FPlaque: TdxFlowChartObjectAdvancedShape;
    FFrame: TdxFlowChartObjectAdvancedShape;
    FFrameCorner: TdxFlowChartObjectAdvancedShape;
    FLShape: TdxFlowChartObjectAdvancedShape;
    FDiagonalStripe: TdxFlowChartObjectAdvancedShape;
    FDonut: TdxFlowChartObjectAdvancedShape;
    FNoSymbol: TdxFlowChartObjectAdvancedShape;
    FLeftParenthesis: TdxFlowChartObjectAdvancedShape;
    FRightParenthesis: TdxFlowChartObjectAdvancedShape;
    FLeftBrace: TdxFlowChartObjectAdvancedShape;
    FRightBrace: TdxFlowChartObjectAdvancedShape;
  strict protected
    procedure CreateShapes; override;
  public
    property Rectangle: TdxFlowChartObjectAdvancedShape read FRectangle;
    property Ellipse: TdxFlowChartObjectAdvancedShape read FEllipse;
    property Triangle: TdxFlowChartObjectAdvancedShape read FTriangle;
    property RightTriangle: TdxFlowChartObjectAdvancedShape read FRightTriangle;
    property Pentagon: TdxFlowChartObjectAdvancedShape read FPentagon;
    property Hexagon: TdxFlowChartObjectAdvancedShape read FHexagon;
    property Heptagon: TdxFlowChartObjectAdvancedShape read FHeptagon;
    property Octagon: TdxFlowChartObjectAdvancedShape read FOctagon;
    property Decagon: TdxFlowChartObjectAdvancedShape read FDecagon;
    property Can: TdxFlowChartObjectAdvancedShape read FCan;
    property Parallelogram: TdxFlowChartObjectAdvancedShape read FParallelogram;
    property Trapezoid: TdxFlowChartObjectAdvancedShape read FTrapezoid;
    property Diamond: TdxFlowChartObjectAdvancedShape read FDiamond;
    property Cross: TdxFlowChartObjectAdvancedShape read FCross;
    property Chevron: TdxFlowChartObjectAdvancedShape read FChevron;
    property Cube: TdxFlowChartObjectAdvancedShape read FCube;

    property Star4: TdxFlowChartObjectAdvancedShape read FStar4;
    property Star5: TdxFlowChartObjectAdvancedShape read FStar5;
    property Star6: TdxFlowChartObjectAdvancedShape read FStar6;
    property Star7: TdxFlowChartObjectAdvancedShape read FStar7;
    property Star16: TdxFlowChartObjectAdvancedShape read FStar16;
    property Star24: TdxFlowChartObjectAdvancedShape read FStar24;
    property Star32: TdxFlowChartObjectAdvancedShape read FStar32;
    property RoundedRectangleShape: TdxFlowChartObjectAdvancedShape read FRoundedRectangleShape;
    property SingleSnipCornerRectangleShape: TdxFlowChartObjectAdvancedShape read FSingleSnipCornerRectangleShape;
    property SnipSameSideCornerRectangleShape: TdxFlowChartObjectAdvancedShape read FSnipSameSideCornerRectangleShape;
    property SnipDiagonalCornerRectangleShape: TdxFlowChartObjectAdvancedShape read FSnipDiagonalCornerRectangleShape;
    property SingleRoundCornerRectangleShape: TdxFlowChartObjectAdvancedShape read FSingleRoundCornerRectangleShape;
    property RoundSameSideCornerRectangleShape: TdxFlowChartObjectAdvancedShape read FRoundSameSideCornerRectangleShape;
    property RoundDiagonalCornerRectangleShape: TdxFlowChartObjectAdvancedShape read FRoundDiagonalCornerRectangleShape;
    property SnipAndRoundSingleCornerRectangleShape: TdxFlowChartObjectAdvancedShape read FSnipAndRoundSingleCornerRectangleShape;
    property SnipCornerRectangleShape: TdxFlowChartObjectAdvancedShape read FSnipCornerRectangleShape;
    property RoundCornerRectangleShape: TdxFlowChartObjectAdvancedShape read FRoundCornerRectangleShape;
    property SnipAndRoundCornerRectangleShape: TdxFlowChartObjectAdvancedShape read FSnipAndRoundCornerRectangleShape;
    property Plaque: TdxFlowChartObjectAdvancedShape read FPlaque;
    property Frame: TdxFlowChartObjectAdvancedShape read FFrame;
    property FrameCorner: TdxFlowChartObjectAdvancedShape read FFrameCorner;
    property LShape: TdxFlowChartObjectAdvancedShape read FLShape;
    property DiagonalStripe: TdxFlowChartObjectAdvancedShape read FDiagonalStripe;
    property Donut: TdxFlowChartObjectAdvancedShape read FDonut;
    property NoSymbol: TdxFlowChartObjectAdvancedShape read FNoSymbol;

    property LeftParenthesis: TdxFlowChartObjectAdvancedShape read FLeftParenthesis;
    property RightParenthesis: TdxFlowChartObjectAdvancedShape read FRightParenthesis;
    property LeftBrace: TdxFlowChartObjectAdvancedShape read FLeftBrace;
    property RightBrace: TdxFlowChartObjectAdvancedShape read FRightBrace;
  end;

  { TdxFlowChartBasicFlowchartShapes }

  TdxFlowChartBasicFlowchartShapes = class(TdxFlowChartAdvancedShapeStencil)
  strict private
    FProcess: TdxFlowChartObjectAdvancedShape;
    FDecision: TdxFlowChartObjectAdvancedShape;
    FSubprocess: TdxFlowChartObjectAdvancedShape;
    FDocument: TdxFlowChartObjectAdvancedShape;
    FOnPageReference: TdxFlowChartObjectAdvancedShape;
    FOffPageReference: TdxFlowChartObjectAdvancedShape;
  strict protected
    procedure CreateShapes; override;
  public
    property Process: TdxFlowChartObjectAdvancedShape read FProcess;
    property Decision: TdxFlowChartObjectAdvancedShape read FDecision;
    property Subprocess: TdxFlowChartObjectAdvancedShape read FSubprocess;
    property Document: TdxFlowChartObjectAdvancedShape read FDocument;
    property OnPageReference: TdxFlowChartObjectAdvancedShape read FOnPageReference;
    property OffPageReference: TdxFlowChartObjectAdvancedShape read FOffPageReference;
  end;

  { TdxFlowChartArrowShapes }

  TdxFlowChartArrowShapes = class(TdxFlowChartAdvancedShapeStencil)
  strict private
    FSimpleArrow: TdxFlowChartObjectAdvancedShape;
    FSimpleDoubleArrow: TdxFlowChartObjectAdvancedShape;
    FModernArrow: TdxFlowChartObjectAdvancedShape;
    FFlexibleArrow: TdxFlowChartObjectAdvancedShape;
    FBentArrow: TdxFlowChartObjectAdvancedShape;
    FUTurnArrow: TdxFlowChartObjectAdvancedShape;
    FSharpBentArrow: TdxFlowChartObjectAdvancedShape;
    FCurvedRightArrow: TdxFlowChartObjectAdvancedShape;
    FCurvedLeftArrow: TdxFlowChartObjectAdvancedShape;
    FNotchedArrow: TdxFlowChartObjectAdvancedShape;
    FStripedArrow: TdxFlowChartObjectAdvancedShape;
    FBlockArrow: TdxFlowChartObjectAdvancedShape;
    FCircularArrow: TdxFlowChartObjectAdvancedShape;
    FQuadArrow: TdxFlowChartObjectAdvancedShape;
    FLeftRightUpArrow: TdxFlowChartObjectAdvancedShape;
    FLeftRightArrowBlock: TdxFlowChartObjectAdvancedShape;
    FQuadArrowBlock: TdxFlowChartObjectAdvancedShape;
  strict protected
    procedure CreateShapes; override;
  public
    property SimpleArrow: TdxFlowChartObjectAdvancedShape read FSimpleArrow;
    property SimpleDoubleArrow: TdxFlowChartObjectAdvancedShape read FSimpleDoubleArrow;
    property ModernArrow: TdxFlowChartObjectAdvancedShape read FModernArrow;
    property FlexibleArrow: TdxFlowChartObjectAdvancedShape read FFlexibleArrow;
    property BentArrow: TdxFlowChartObjectAdvancedShape read FBentArrow;
    property UTurnArrow: TdxFlowChartObjectAdvancedShape read FUTurnArrow;
    property SharpBentArrow: TdxFlowChartObjectAdvancedShape read FSharpBentArrow;
    property CurvedRightArrow: TdxFlowChartObjectAdvancedShape read FCurvedRightArrow;
    property CurvedLeftArrow: TdxFlowChartObjectAdvancedShape read FCurvedLeftArrow;
    property NotchedArrow: TdxFlowChartObjectAdvancedShape read FNotchedArrow;
    property StripedArrow: TdxFlowChartObjectAdvancedShape read FStripedArrow;
    property BlockArrow: TdxFlowChartObjectAdvancedShape read FBlockArrow;
    property CircularArrow: TdxFlowChartObjectAdvancedShape read FCircularArrow;
    property QuadArrow: TdxFlowChartObjectAdvancedShape read FQuadArrow;
    property LeftRightUpArrow: TdxFlowChartObjectAdvancedShape read FLeftRightUpArrow;
    property LeftRightArrowBlock: TdxFlowChartObjectAdvancedShape read FLeftRightArrowBlock;
    property QuadArrowBlock: TdxFlowChartObjectAdvancedShape read FQuadArrowBlock;
  end;

  { TdxFlowChartDecorativeShapes }

  TdxFlowChartDecorativeShapes = class(TdxFlowChartAdvancedShapeStencil)
  strict private
    FLightningBolt: TdxFlowChartObjectAdvancedShape;
    FMoon: TdxFlowChartObjectAdvancedShape;
    FWave: TdxFlowChartObjectAdvancedShape;
    FDoubleWave: TdxFlowChartObjectAdvancedShape;
    FVerticalScroll: TdxFlowChartObjectAdvancedShape;
    FHorizontalScroll: TdxFlowChartObjectAdvancedShape;
    FHeart: TdxFlowChartObjectAdvancedShape;
    FDownRibbon: TdxFlowChartObjectAdvancedShape;
    FUpRibbon: TdxFlowChartObjectAdvancedShape;
    FCloud: TdxFlowChartObjectAdvancedShape;
  strict protected
    procedure CreateShapes; override;
  public
    property LightningBolt: TdxFlowChartObjectAdvancedShape read FLightningBolt;
    property Moon: TdxFlowChartObjectAdvancedShape read FMoon;
    property Wave: TdxFlowChartObjectAdvancedShape read FWave;
    property DoubleWave: TdxFlowChartObjectAdvancedShape read FDoubleWave;
    property VerticalScroll: TdxFlowChartObjectAdvancedShape read FVerticalScroll;
    property HorizontalScroll: TdxFlowChartObjectAdvancedShape read FHorizontalScroll;
    property Heart: TdxFlowChartObjectAdvancedShape read FHeart;
    property DownRibbon: TdxFlowChartObjectAdvancedShape read FDownRibbon;
    property UpRibbon: TdxFlowChartObjectAdvancedShape read FUpRibbon;
    property Cloud: TdxFlowChartObjectAdvancedShape read FCloud;
  end;

  { TdxFlowChartSDLDiagramShapes }

  TdxFlowChartSDLDiagramShapes = class(TdxFlowChartAdvancedShapeStencil)
  strict private
    FStart: TdxFlowChartObjectAdvancedShape;
    FVariableStart: TdxFlowChartObjectAdvancedShape;
    FProcedure: TdxFlowChartObjectAdvancedShape;
    FVariableProcedure: TdxFlowChartObjectAdvancedShape;
    FCreateRequest: TdxFlowChartObjectAdvancedShape;
    FAlternative: TdxFlowChartObjectAdvancedShape;
    FDocument: TdxFlowChartObjectAdvancedShape;
    FReturn: TdxFlowChartObjectAdvancedShape;
    FDecision1: TdxFlowChartObjectAdvancedShape;
    FMessageFromUser: TdxFlowChartObjectAdvancedShape;
    FPrimitiveFromCallControl: TdxFlowChartObjectAdvancedShape;
    FDecision2: TdxFlowChartObjectAdvancedShape;
    FMessageToUser: TdxFlowChartObjectAdvancedShape;
    FPrimitiveToCallControl: TdxFlowChartObjectAdvancedShape;
    FSave: TdxFlowChartObjectAdvancedShape;
    FOnPageReference: TdxFlowChartObjectAdvancedShape;
    FOffPageReference: TdxFlowChartObjectAdvancedShape;
    FDiskStorage: TdxFlowChartObjectAdvancedShape;
    FDividedProcess: TdxFlowChartObjectAdvancedShape;
    FDividedEvent: TdxFlowChartObjectAdvancedShape;
    FTerminator: TdxFlowChartObjectAdvancedShape;
  strict protected
    procedure CreateShapes; override;
  public
    property Start: TdxFlowChartObjectAdvancedShape read FStart;
    property VariableStart: TdxFlowChartObjectAdvancedShape read FVariableStart;
    property &Procedure: TdxFlowChartObjectAdvancedShape read FProcedure;
    property VariableProcedure: TdxFlowChartObjectAdvancedShape read FVariableProcedure;
    property CreateRequest: TdxFlowChartObjectAdvancedShape read FCreateRequest;
    property Alternative: TdxFlowChartObjectAdvancedShape read FAlternative;
    property Document: TdxFlowChartObjectAdvancedShape read FDocument;
    property Return: TdxFlowChartObjectAdvancedShape read FReturn;
    property Decision1: TdxFlowChartObjectAdvancedShape read FDecision1;
    property MessageFromUser: TdxFlowChartObjectAdvancedShape read FMessageFromUser;
    property PrimitiveFromCallControl: TdxFlowChartObjectAdvancedShape read FPrimitiveFromCallControl;
    property Decision2: TdxFlowChartObjectAdvancedShape read FDecision2;
    property MessageToUser: TdxFlowChartObjectAdvancedShape read FMessageToUser;
    property PrimitiveToCallControl: TdxFlowChartObjectAdvancedShape read FPrimitiveToCallControl;
    property Save: TdxFlowChartObjectAdvancedShape read FSave;
    property OnPageReference: TdxFlowChartObjectAdvancedShape read FOnPageReference;
    property OffPageReference: TdxFlowChartObjectAdvancedShape read FOffPageReference;
    property DiskStorage: TdxFlowChartObjectAdvancedShape read FDiskStorage;
    property DividedProcess: TdxFlowChartObjectAdvancedShape read FDividedProcess;
    property DividedEvent: TdxFlowChartObjectAdvancedShape read FDividedEvent;
    property Terminator: TdxFlowChartObjectAdvancedShape read FTerminator;
  end;

  { TdxFlowChartSoftwareIcons }

  TdxFlowChartSoftwareIcons = class(TdxFlowChartAdvancedShapeStencil)
  strict private
    FBack: TdxFlowChartObjectAdvancedShape;
    FForward: TdxFlowChartObjectAdvancedShape;
    FExpand: TdxFlowChartObjectAdvancedShape;
    FCollapse: TdxFlowChartObjectAdvancedShape;
    FAdd: TdxFlowChartObjectAdvancedShape;
    FRemove: TdxFlowChartObjectAdvancedShape;
    FZoomIn: TdxFlowChartObjectAdvancedShape;
    FZoomOut: TdxFlowChartObjectAdvancedShape;
    FLock: TdxFlowChartObjectAdvancedShape;
    FPermission: TdxFlowChartObjectAdvancedShape;
    FSort: TdxFlowChartObjectAdvancedShape;
    FFilter: TdxFlowChartObjectAdvancedShape;
    FTools: TdxFlowChartObjectAdvancedShape;
    FProperties: TdxFlowChartObjectAdvancedShape;
    FCalendar: TdxFlowChartObjectAdvancedShape;
    FDocument: TdxFlowChartObjectAdvancedShape;
    FDatabase: TdxFlowChartObjectAdvancedShape;
    FHardDrive: TdxFlowChartObjectAdvancedShape;
    FNetwork: TdxFlowChartObjectAdvancedShape;
  strict protected
    procedure CreateShapes; override;
  public
    property Back: TdxFlowChartObjectAdvancedShape read FBack;
    property Forward: TdxFlowChartObjectAdvancedShape read FForward;
    property Expand: TdxFlowChartObjectAdvancedShape read FExpand;
    property Collapse: TdxFlowChartObjectAdvancedShape read FCollapse;
    property Add: TdxFlowChartObjectAdvancedShape read FAdd;
    property Remove: TdxFlowChartObjectAdvancedShape read FRemove;
    property ZoomIn: TdxFlowChartObjectAdvancedShape read FZoomIn;
    property ZoomOut: TdxFlowChartObjectAdvancedShape read FZoomOut;
    property Lock: TdxFlowChartObjectAdvancedShape read FLock;
    property Permission: TdxFlowChartObjectAdvancedShape read FPermission;
    property Sort: TdxFlowChartObjectAdvancedShape read FSort;
    property Filter: TdxFlowChartObjectAdvancedShape read FFilter;
    property Tools: TdxFlowChartObjectAdvancedShape read FTools;
    property Properties: TdxFlowChartObjectAdvancedShape read FProperties;
    property Calendar: TdxFlowChartObjectAdvancedShape read FCalendar;
    property Document: TdxFlowChartObjectAdvancedShape read FDocument;
    property Database: TdxFlowChartObjectAdvancedShape read FDatabase;
    property HardDrive: TdxFlowChartObjectAdvancedShape read FHardDrive;
    property Network: TdxFlowChartObjectAdvancedShape read FNetwork;
  end;

  { TdxFlowChartAdvancedShapeRepository }

  TdxFlowChartAdvancedShapeRepository = class
  strict private
    FShapes: TDictionary<string, TdxFlowChartObjectAdvancedShape>;
    FStencils: TObjectList<TdxFlowChartAdvancedShapeStencil>;
    //
    FArrowShapes: TdxFlowChartArrowShapes;
    FBasicFlowchartShapes: TdxFlowChartBasicFlowchartShapes;
    FBasicShapes: TdxFlowChartBasicShapes;
    FDecorativeShapes: TdxFlowChartDecorativeShapes;
    FSDLDiagramShapes: TdxFlowChartSDLDiagramShapes;
    FSoftwareIcons: TdxFlowChartSoftwareIcons;
    function GetStencil(Index: Integer): TdxFlowChartAdvancedShapeStencil;
    function GetStencilCount: Integer;
  protected
    procedure AddStencil(AStencil: TdxFlowChartAdvancedShapeStencil);
    procedure CreateDefaultStencils;
    procedure RemoveUserShapeByID(const AID: string);

    property Shapes: TDictionary<string, TdxFlowChartObjectAdvancedShape> read FShapes;
  public
    constructor Create;
    destructor Destroy; override;
    function FindShapeByID(const AID: string): TdxFlowChartObjectAdvancedShape;
    function FindStencilByID(const AID: string): TdxFlowChartAdvancedShapeStencil;
    procedure LoadShapesFromFile(const AFileName, AStencilID: string; const AStencilCaption: string = '');
    procedure LoadShapesFromStream(const AStream: TStream; const AStencilID: string; const AStencilCaption: string = '');

    property Stencils[Index: Integer]: TdxFlowChartAdvancedShapeStencil read GetStencil;
    property StencilCount: Integer read GetStencilCount;
    //predefined stencils
    property ArrowShapes: TdxFlowChartArrowShapes read FArrowShapes;
    property BasicFlowchartShapes: TdxFlowChartBasicFlowchartShapes read FBasicFlowchartShapes;
    property BasicShapes: TdxFlowChartBasicShapes read FBasicShapes;
    property DecorativeShapes: TdxFlowChartDecorativeShapes read FDecorativeShapes;
    property SDLDiagramShapes: TdxFlowChartSDLDiagramShapes read FSDLDiagramShapes;
    property SoftwareIcons: TdxFlowChartSoftwareIcons read FSoftwareIcons;
  end;

implementation

uses
  RTLConsts, Windows, Math, dxStringHelper, AnsiStrings, dxSVGCoreParsers, dxTypeHelpers, dxFcStrs,
  dxflchrt, dxFlowChartShapeDescriptions.Code, dxFlowChartShapeDescriptions.Xml;

type

  { TdxFlowChartObjectPredefinedAdvancedShape }

  TdxFlowChartObjectPredefinedAdvancedShape = class(TdxFlowChartObjectAdvancedShape)
  strict private
    FCaption: Pointer;
  protected
    constructor Create(AStencil: TdxFlowChartAdvancedShapeStencil;
      AShape: TdxShapeDescription; ACaption: Pointer; ALegacyReplacement: Boolean = False);
    function GetCaption: string; override;
    procedure SetCaption(const Value: string); override;
  end;

  { TFlowChartObjectUserAdvancedShape }

  TFlowChartObjectUserAdvancedShape = class(TdxFlowChartObjectAdvancedShape)
  strict private
    FCaption: string;
  protected
    constructor Create(AStencil: TdxFlowChartAdvancedShapeStencil; AShape: TdxShapeDescription; const ACaption: string);
    function GetCaption: string; override;
    procedure SetCaption(const Value: string); override;
  end;

{ TdxFlowChartObjectAdvancedShape }

constructor TdxFlowChartObjectAdvancedShape.Create(AStencil: TdxFlowChartAdvancedShapeStencil;
  AShape: TdxShapeDescription; ALegacyReplacement: Boolean = False);
begin
  FLegacyReplacement := ALegacyReplacement;
  FStencil := AStencil;
  FShape := AShape;
end;

function TdxFlowChartObjectAdvancedShape.GetCaption: string;
begin
  Result := '';
end;

function TdxFlowChartObjectAdvancedShape.GetDefaultSize: TSize;
begin
  Result.Init(Round(FShape.DefaultSize.Width), Round(FShape.DefaultSize.Height));
end;

function TdxFlowChartObjectAdvancedShape.GetID: string;
begin
  Result := Format('%s.%s', [FStencil.ID, FShape.ID]);
end;

procedure TdxFlowChartObjectAdvancedShape.SetCaption(const Value: string);
begin
end;

{ TdxFlowChartObjectPredefinedAdvancedShape }

constructor TdxFlowChartObjectPredefinedAdvancedShape.Create(AStencil: TdxFlowChartAdvancedShapeStencil;
  AShape: TdxShapeDescription; ACaption: Pointer; ALegacyReplacement: Boolean = False);
begin
  inherited Create(AStencil, AShape, ALegacyReplacement);
  FCaption := ACaption;
end;

function TdxFlowChartObjectPredefinedAdvancedShape.GetCaption: string;
begin
  Result := cxGetResourceString(FCaption);
end;

procedure TdxFlowChartObjectPredefinedAdvancedShape.SetCaption(const Value: string);
begin
  cxSetResourceString(FCaption, Value);
end;

{ TFlowChartObjectUserAdvancedShape }

constructor TFlowChartObjectUserAdvancedShape.Create(AStencil: TdxFlowChartAdvancedShapeStencil;
  AShape: TdxShapeDescription; const ACaption: string);
begin
  inherited Create(AStencil, AShape);
  FCaption := ACaption;
end;

function TFlowChartObjectUserAdvancedShape.GetCaption: string;
begin
  Result := FCaption;
end;

procedure TFlowChartObjectUserAdvancedShape.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

{ TdxShapePathProperties }

constructor TdxShapePathProperties.Create(ATransparent: Boolean = False);
begin
  inherited Create;
  FTransparent := ATransparent;
end;

{ TdxFlowChartAdvancedShapeStencil }

constructor TdxFlowChartAdvancedShapeStencil.Create(AOwner: TdxFlowChartAdvancedShapeRepository; const AResourceName: string; ACaption: Pointer);
var
  AStream: TResourceStream;
  ALoader: TdxTemplateLoader;
begin
  inherited Create;
  FPredefined := True;
  FOwner := AOwner;
  FID := AResourceName;
  FCaption := ACaption;
  FShapes := TObjectList<TdxFlowChartObjectAdvancedShape>.Create;
  FShapeDescriptions := TObjectDictionary<string, TdxShapeDescription>.Create([doOwnsValues], TdxIStringComparer.Ordinal);
  AStream := TResourceStream.Create(HInstance, AResourceName, RT_RCDATA);
  try
    ALoader := TdxTemplateLoader.Create;
    try
      ALoader.LoadFromStream(AStream,
        procedure (ATemplate: TdxShapeXmlDescription)
        begin
          FShapeDescriptions.Add(ATemplate.ID, ATemplate);
        end);
      CreateShapes;
    finally
      ALoader.Free;
    end;
  finally
    AStream.Free;
  end;
  Owner.AddStencil(Self);
end;

constructor TdxFlowChartAdvancedShapeStencil.Create(const AID, ACaption: string);
var
  I: Integer;
begin
  inherited Create;
  FPredefined := False;
  FOwner := TdxFlowChart.Repository;
  for I := 0 to Owner.StencilCount - 1 do
    if SameText(Owner.Stencils[I].ID, AID) then
      raise EListError.CreateRes(@SGenericDuplicateItem);
  FID := AID;
  FCustomCaption := ACaption;
  FShapes := TObjectList<TdxFlowChartObjectAdvancedShape>.Create;
  FShapeDescriptions := TObjectDictionary<string, TdxShapeDescription>.Create([doOwnsValues], TdxIStringComparer.Ordinal);
  Owner.AddStencil(Self);
end;

destructor TdxFlowChartAdvancedShapeStencil.Destroy;
begin
  FShapeDescriptions.Free;
  FShapes.Free;
  inherited Destroy;
end;

function TdxFlowChartAdvancedShapeStencil.HasVisibleShapes: Boolean;
begin
  Result := Count > 0;
end;

function TdxFlowChartAdvancedShapeStencil.AddPredefinedShape(const AShapeID: string; ACaption: Pointer;
  ALegacyReplacement: Boolean = False): TdxFlowChartObjectAdvancedShape;
var
  AShapeDescription: TdxShapeDescription;
begin
  if FShapeDescriptions.TryGetValue(AShapeID, AShapeDescription) then
  begin
    Result := TdxFlowChartObjectPredefinedAdvancedShape.Create(Self, AShapeDescription, ACaption, ALegacyReplacement);
    FShapes.Add(Result);
    Owner.Shapes.Add(Result.ID, Result);
  end
  else
    raise EdxException.CreateFmt('Shape description with ID="%s" not found!', [AShapeDescription.ID]);
end;

function TdxFlowChartAdvancedShapeStencil.AddPredefinedShape(AShapeDescription: TdxShapeDescription; ACaption: Pointer;
  ALegacyReplacement: Boolean = False): TdxFlowChartObjectAdvancedShape;
begin
  if not FShapeDescriptions.ContainsKey(AShapeDescription.ID) then
  begin
    Result := TdxFlowChartObjectPredefinedAdvancedShape.Create(Self, AShapeDescription, ACaption, ALegacyReplacement);
    FShapes.Add(Result);
    Owner.Shapes.Add(Result.ID, Result);
    FShapeDescriptions.Add(AShapeDescription.ID, AShapeDescription);
  end
  else
    raise EdxException.CreateFmt('Shape description with ID="%s" already exists!', [AShapeDescription.ID]);
end;

function TdxFlowChartAdvancedShapeStencil.AddUserShape(AShapeID: string;
  const ACaption: string): TdxFlowChartObjectAdvancedShape;
var
  AShapeDescription: TdxShapeDescription;
begin
  if FShapeDescriptions.TryGetValue(AShapeID, AShapeDescription) then
  begin
    Result := TFlowChartObjectUserAdvancedShape.Create(Self, AShapeDescription, ACaption);
    FShapes.Add(Result);
    Owner.Shapes.Add(Result.ID, Result);
  end
  else
    raise EdxException.CreateFmt('Shape description with ID="%s" not found!', [AShapeDescription.ID]);
end;

procedure TdxFlowChartAdvancedShapeStencil.CreateShapes;
begin
end;

procedure TdxFlowChartAdvancedShapeStencil.RemoveShape(AShape: TdxFlowChartObjectAdvancedShape);
var
  AShapeDescription: TdxShapeDescription;
begin
  if FShapeDescriptions.TryGetValue(AShape.FShape.ID, AShapeDescription) then
  begin
    FShapeDescriptions.Remove(AShapeDescription.ID);
    FShapes.Remove(AShape);
  end
  else
    raise EdxException.CreateFmt('Shape description with ID="%s" not found!', [AShapeDescription.ID]);
end;

function TdxFlowChartAdvancedShapeStencil.GetCount: Integer;
begin
  Result := FShapes.Count;
end;

function TdxFlowChartAdvancedShapeStencil.GetCaption: string;
begin
  if FPredefined then
    Result := cxGetResourceString(FCaption)
  else
    Result := FCustomCaption;
end;

function TdxFlowChartAdvancedShapeStencil.GetShape(Index: Integer): TdxFlowChartObjectAdvancedShape;
begin
  Result := FShapes[Index];
end;

procedure TdxFlowChartAdvancedShapeStencil.SetCaption(const Value: string);
begin
  if FPredefined then
    cxSetResourceString(FCaption, Value)
  else
    FCustomCaption := Value;
end;

{ TdxFlowChartBasicShapes }

procedure TdxFlowChartBasicShapes.CreateShapes;
begin
  FRectangle := AddPredefinedShape(TdxBasicCodeShapes.CreateRectangleShape, @sdxFlowChart_BasicShapes_Rectangle, True);
  FEllipse := AddPredefinedShape(TdxBasicCodeShapes.CreateEllipseShape, @sdxFlowChart_BasicShapes_Ellipse, True);
  FTriangle := AddPredefinedShape(TdxBasicCodeShapes.CreateTriangleShape, @sdxFlowChart_BasicShapes_Triangle, True);
  FRightTriangle := AddPredefinedShape(TdxBasicCodeShapes.CreateRightTriangleShape, @sdxFlowChart_BasicShapes_RightTriangle);
  FPentagon := AddPredefinedShape(TdxBasicCodeShapes.CreatePentagonShape, @sdxFlowChart_BasicShapes_Pentagon);
  FHexagon := AddPredefinedShape(TdxBasicCodeShapes.CreateHexagonShape, @sdxFlowChart_BasicShapes_Hexagon, True);
  FHeptagon := AddPredefinedShape(TdxBasicCodeShapes.CreateHeptagonShape, @sdxFlowChart_BasicShapes_Heptagon);
  FOctagon := AddPredefinedShape(TdxBasicCodeShapes.CreateOctagonShape, @sdxFlowChart_BasicShapes_Octagon);
  FDecagon := AddPredefinedShape(TdxBasicCodeShapes.CreateDecagonShape, @sdxFlowChart_BasicShapes_Decagon);
  FCan := AddPredefinedShape(TdxBasicCodeShapes.CreateCanShape, @sdxFlowChart_BasicShapes_Can);
  FParallelogram := AddPredefinedShape(TdxBasicCodeShapes.CreateParallelogramShape, @sdxFlowChart_BasicShapes_Parallelogram);
  FTrapezoid := AddPredefinedShape(TdxBasicCodeShapes.CreateTrapezoidShape, @sdxFlowChart_BasicShapes_Trapezoid);
  FDiamond := AddPredefinedShape(TdxBasicCodeShapes.CreateDiamondShape, @sdxFlowChart_BasicShapes_Diamond, True);
  FCross := AddPredefinedShape('Cross', @sdxFlowChart_BasicShapes_Cross);
  FCube := AddPredefinedShape(TdxBasicCodeShapes.CreateCubeShape, @sdxFlowChart_BasicShapes_Cube);
  FChevron := AddPredefinedShape(TdxBasicCodeShapes.CreateChevronShape, @sdxFlowChart_BasicShapes_Chevron);
  FStar4 := AddPredefinedShape(TdxBasicCodeShapes.CreateStar4Shape, @sdxFlowChart_BasicShapes_Star4);
  FStar5 := AddPredefinedShape(TdxBasicCodeShapes.CreateStar5Shape, @sdxFlowChart_BasicShapes_Star5);
  FStar6 := AddPredefinedShape(TdxBasicCodeShapes.CreateStar6Shape, @sdxFlowChart_BasicShapes_Star6);
  FStar7 := AddPredefinedShape(TdxBasicCodeShapes.CreateStar7Shape, @sdxFlowChart_BasicShapes_Star7);
  FStar16 := AddPredefinedShape(TdxBasicCodeShapes.CreateStar16Shape, @sdxFlowChart_BasicShapes_Star16);
  FStar24 := AddPredefinedShape(TdxBasicCodeShapes.CreateStar24Shape, @sdxFlowChart_BasicShapes_Star24);
  FStar32 := AddPredefinedShape(TdxBasicCodeShapes.CreateStar32Shape, @sdxFlowChart_BasicShapes_Star32);
  FRoundedRectangleShape := AddPredefinedShape(TdxBasicCodeShapes.CreateRoundedRectangleShape, @sdxFlowChart_BasicShapes_RoundedRectangle, True);
  FSingleSnipCornerRectangleShape := AddPredefinedShape(TdxBasicCodeShapes.CreateSingleSnipCornerRectangleShape, @sdxFlowChart_BasicShapes_SingleSnipCornerRectangle);
  FSnipSameSideCornerRectangleShape := AddPredefinedShape(TdxBasicCodeShapes.CreateSnipSameSideCornerRectangleShape, @sdxFlowChart_BasicShapes_SnipSameSideCornerRectangle);
  FSnipDiagonalCornerRectangleShape := AddPredefinedShape(TdxBasicCodeShapes.CreateSnipDiagonalCornerRectangleShape, @sdxFlowChart_BasicShapes_SnipDiagonalCornerRectangle);
  FSingleRoundCornerRectangleShape := AddPredefinedShape(TdxBasicCodeShapes.CreateSingleRoundCornerRectangleShape, @sdxFlowChart_BasicShapes_SingleRoundCornerRectangle);
  FRoundSameSideCornerRectangleShape := AddPredefinedShape(TdxBasicCodeShapes.CreateRoundSameSideCornerRectangleShape, @sdxFlowChart_BasicShapes_RoundSameSideCornerRectangle);
  FRoundDiagonalCornerRectangleShape := AddPredefinedShape(TdxBasicCodeShapes.CreateRoundDiagonalCornerRectangleShape, @sdxFlowChart_BasicShapes_RoundDiagonalCornerRectangle);
  FSnipAndRoundSingleCornerRectangleShape := AddPredefinedShape(TdxBasicCodeShapes.CreateSnipAndRoundSingleCornerRectangleShape, @sdxFlowChart_BasicShapes_SnipAndRoundSingleCornerRectangle);
  FSnipCornerRectangleShape := AddPredefinedShape(TdxBasicCodeShapes.CreateSnipCornerRectangleShape, @sdxFlowChart_BasicShapes_SnipCornerRectangle);
  FRoundCornerRectangleShape := AddPredefinedShape(TdxBasicCodeShapes.CreateRoundCornerRectangleShape, @sdxFlowChart_BasicShapes_RoundCornerRectangle);
  FSnipAndRoundCornerRectangleShape := AddPredefinedShape(TdxBasicCodeShapes.CreateSnipAndRoundCornerRectangleShape, @sdxFlowChart_BasicShapes_SnipAndRoundCornerRectangle);
  FPlaque := AddPredefinedShape(TdxBasicCodeShapes.CreatePlaqueShape, @sdxFlowChart_BasicShapes_Plaque);
  FFrame := AddPredefinedShape(TdxBasicCodeShapes.CreateFrameShape, @sdxFlowChart_BasicShapes_Frame);
  FFrameCorner := AddPredefinedShape(TdxBasicCodeShapes.CreateFrameCornerShape, @sdxFlowChart_BasicShapes_FrameCorner);
  FLShape := AddPredefinedShape(TdxBasicCodeShapes.CreateLShapeShape, @sdxFlowChart_BasicShapes_LShape);
  FDiagonalStripe := AddPredefinedShape(TdxBasicCodeShapes.CreateDiagonalStripeShape, @sdxFlowChart_BasicShapes_DiagonalStripe);
  FDonut := AddPredefinedShape(TdxBasicCodeShapes.CreateDonutShape, @sdxFlowChart_BasicShapes_Donut);
  FNoSymbol := AddPredefinedShape(TdxBasicCodeShapes.CreateNoSymbolShape, @sdxFlowChart_BasicShapes_NoSymbol);
  FLeftParenthesis := AddPredefinedShape('LeftParenthesis', @sdxFlowChart_BasicShapes_LeftParenthesis);
  FRightParenthesis := AddPredefinedShape('RightParenthesis', @sdxFlowChart_BasicShapes_RightParenthesis);
  FLeftBrace := AddPredefinedShape('LeftBrace', @sdxFlowChart_BasicShapes_LeftBrace);
  FRightBrace := AddPredefinedShape('RightBrace', @sdxFlowChart_BasicShapes_RightBrace);
end;

{ TdxFlowChartBasicFlowchartShapes }

procedure TdxFlowChartBasicFlowchartShapes.CreateShapes;
begin
  FProcess := AddPredefinedShape('Process', @sdxFlowChart_BasicFlowchartShapes_Process);
  FDecision := AddPredefinedShape('Decision', @sdxFlowChart_BasicFlowchartShapes_Decision);
  FSubprocess := AddPredefinedShape('Subprocess', @sdxFlowChart_BasicFlowchartShapes_Subprocess);
  FDocument := AddPredefinedShape('Document', @sdxFlowChart_BasicFlowchartShapes_Document);
  FOnPageReference := AddPredefinedShape('OnPageReference', @sdxFlowChart_BasicFlowchartShapes_OnPageReference);
  FOffPageReference := AddPredefinedShape('OffPageReference', @sdxFlowChart_BasicFlowchartShapes_OffPageReference);
end;

{ TdxFlowChartArrowShapes }

procedure TdxFlowChartArrowShapes.CreateShapes;
begin
  FSimpleArrow := AddPredefinedShape('SimpleArrow', @sdxFlowChart_ArrowShapes_SimpleArrow);
  FSimpleDoubleArrow := AddPredefinedShape('SimpleDoubleArrow', @sdxFlowChart_ArrowShapes_SimpleDoubleArrow);
  FModernArrow := AddPredefinedShape('ModernArrow', @sdxFlowChart_ArrowShapes_ModernArrow);
  FFlexibleArrow := AddPredefinedShape('FlexibleArrow', @sdxFlowChart_ArrowShapes_FlexibleArrow);
  FBentArrow := AddPredefinedShape('BentArrow', @sdxFlowChart_ArrowShapes_BentArrow);
  FUTurnArrow := AddPredefinedShape('UTurnArrow', @sdxFlowChart_ArrowShapes_UTurnArrow);
  FSharpBentArrow := AddPredefinedShape('SharpBentArrow', @sdxFlowChart_ArrowShapes_SharpBentArrow);
  FCurvedRightArrow := AddPredefinedShape('CurvedRightArrow', @sdxFlowChart_ArrowShapes_CurvedRightArrow);
  FCurvedLeftArrow := AddPredefinedShape('CurvedLeftArrow', @sdxFlowChart_ArrowShapes_CurvedLeftArrow);
  FNotchedArrow := AddPredefinedShape('NotchedArrow', @sdxFlowChart_ArrowShapes_NotchedArrow);
  FStripedArrow := AddPredefinedShape('StripedArrow', @sdxFlowChart_ArrowShapes_StripedArrow);
  FBlockArrow := AddPredefinedShape('BlockArrow', @sdxFlowChart_ArrowShapes_BlockArrow);
  FCircularArrow := AddPredefinedShape('CircularArrow', @sdxFlowChart_ArrowShapes_CircularArrow);
  FQuadArrow := AddPredefinedShape('QuadArrow', @sdxFlowChart_ArrowShapes_QuadArrow);
  FLeftRightUpArrow := AddPredefinedShape('LeftRightUpArrow', @sdxFlowChart_ArrowShapes_LeftRightUpArrow);
  FLeftRightArrowBlock := AddPredefinedShape('LeftRightArrowBlock', @sdxFlowChart_ArrowShapes_LeftRightArrowBlock);
  FQuadArrowBlock := AddPredefinedShape('QuadArrowBlock', @sdxFlowChart_ArrowShapes_QuadArrowBlock);
end;

{ TdxFlowChartDecorativeShapes }

procedure TdxFlowChartDecorativeShapes.CreateShapes;
begin
  FLightningBolt := AddPredefinedShape('LightningBolt', @sdxFlowChart_DecorativeShapes_LightningBolt);
  FMoon := AddPredefinedShape('Moon', @sdxFlowChart_DecorativeShapes_Moon);
  FWave := AddPredefinedShape('Wave', @sdxFlowChart_DecorativeShapes_Wave);
  FDoubleWave := AddPredefinedShape('DoubleWave', @sdxFlowChart_DecorativeShapes_DoubleWave);
  FVerticalScroll := AddPredefinedShape('VerticalScroll', @sdxFlowChart_DecorativeShapes_VerticalScroll);
  FHorizontalScroll := AddPredefinedShape('HorizontalScroll', @sdxFlowChart_DecorativeShapes_HorizontalScroll);
  FHeart := AddPredefinedShape('Heart', @sdxFlowChart_DecorativeShapes_Heart);
  FDownRibbon := AddPredefinedShape('DownRibbon', @sdxFlowChart_DecorativeShapes_DownRibbon);
  FUpRibbon := AddPredefinedShape('UpRibbon', @sdxFlowChart_DecorativeShapes_UpRibbon);
  FCloud := AddPredefinedShape('Cloud', @sdxFlowChart_DecorativeShapes_Cloud);
end;

{ TdxFlowChartSDLDiagramShapes }

procedure TdxFlowChartSDLDiagramShapes.CreateShapes;
begin
  FStart := AddPredefinedShape('Start', @sdxFlowChart_SDLDiagramShapes_Start);
  FVariableStart := AddPredefinedShape('VariableStart', @sdxFlowChart_SDLDiagramShapes_VariableStart);
  FProcedure := AddPredefinedShape('Procedure', @sdxFlowChart_SDLDiagramShapes_Procedure);
  FVariableProcedure := AddPredefinedShape('VariableProcedure', @sdxFlowChart_SDLDiagramShapes_VariableProcedure);
  FCreateRequest := AddPredefinedShape('CreateRequest', @sdxFlowChart_SDLDiagramShapes_CreateRequest);
  FAlternative := AddPredefinedShape('Alternative', @sdxFlowChart_SDLDiagramShapes_Alternative);
  FReturn := AddPredefinedShape('Return', @sdxFlowChart_SDLDiagramShapes_Return);
  FDecision1 := AddPredefinedShape('Decision1', @sdxFlowChart_SDLDiagramShapes_Decision1);
  FMessageFromUser := AddPredefinedShape('MessageFromUser', @sdxFlowChart_SDLDiagramShapes_MessageFromUser);
  FPrimitiveFromCallControl := AddPredefinedShape('PrimitiveFromCallControl', @sdxFlowChart_SDLDiagramShapes_PrimitiveFromCallControl);
  FDecision2 := AddPredefinedShape('Decision2', @sdxFlowChart_SDLDiagramShapes_Decision2);
  FMessageToUser := AddPredefinedShape('MessageToUser', @sdxFlowChart_SDLDiagramShapes_MessageToUser);
  FPrimitiveToCallControl := AddPredefinedShape('PrimitiveToCallControl', @sdxFlowChart_SDLDiagramShapes_PrimitiveToCallControl);
  FSave := AddPredefinedShape('Save', @sdxFlowChart_SDLDiagramShapes_Save);
  FOnPageReference := AddPredefinedShape('OnPageReference', @sdxFlowChart_SDLDiagramShapes_OnPageReference);
  FOffPageReference := AddPredefinedShape('OffPageReference', @sdxFlowChart_SDLDiagramShapes_OffPageReference);
  FDocument := AddPredefinedShape('Document', @sdxFlowChart_SDLDiagramShapes_Document);
  FDiskStorage := AddPredefinedShape('DiskStorage', @sdxFlowChart_SDLDiagramShapes_DiskStorage);
  FDividedProcess := AddPredefinedShape('DividedProcess', @sdxFlowChart_SDLDiagramShapes_DividedProcess);
  FDividedEvent := AddPredefinedShape('DividedEvent', @sdxFlowChart_SDLDiagramShapes_DividedEvent);
  FTerminator := AddPredefinedShape('Terminator', @sdxFlowChart_SDLDiagramShapes_Terminator);
end;

{ TdxFlowChartSoftwareIcons }

procedure TdxFlowChartSoftwareIcons.CreateShapes;
begin
  FBack := AddPredefinedShape('Back', @sdxFlowChart_SoftwareIcons_Back);
  FForward := AddPredefinedShape('Forward', @sdxFlowChart_SoftwareIcons_Forward);
  FExpand := AddPredefinedShape('Expand', @sdxFlowChart_SoftwareIcons_Expand);
  FCollapse := AddPredefinedShape('Collapse', @sdxFlowChart_SoftwareIcons_Collapse);
  FAdd := AddPredefinedShape('Add', @sdxFlowChart_SoftwareIcons_Add);
  FRemove := AddPredefinedShape('Remove', @sdxFlowChart_SoftwareIcons_Remove);
  FZoomIn := AddPredefinedShape('ZoomIn', @sdxFlowChart_SoftwareIcons_ZoomIn);
  FZoomOut := AddPredefinedShape('ZoomOut', @sdxFlowChart_SoftwareIcons_ZoomOut);
  FLock := AddPredefinedShape('Lock', @sdxFlowChart_SoftwareIcons_Lock);
  FPermission := AddPredefinedShape('Permission', @sdxFlowChart_SoftwareIcons_Permission);
  FSort := AddPredefinedShape('Sort', @sdxFlowChart_SoftwareIcons_Sort);
  FFilter := AddPredefinedShape('Filter', @sdxFlowChart_SoftwareIcons_Filter);
  FTools := AddPredefinedShape('Tools', @sdxFlowChart_SoftwareIcons_Tools);
  FProperties := AddPredefinedShape('Properties', @sdxFlowChart_SoftwareIcons_Properties);
  FCalendar := AddPredefinedShape('Calendar', @sdxFlowChart_SoftwareIcons_Calendar);
  FDocument := AddPredefinedShape('Document', @sdxFlowChart_SoftwareIcons_Document);
  FDatabase := AddPredefinedShape('Database', @sdxFlowChart_SoftwareIcons_Database);
  FHardDrive := AddPredefinedShape('HardDrive', @sdxFlowChart_SoftwareIcons_HardDrive);
  FNetwork := AddPredefinedShape('Network', @sdxFlowChart_SoftwareIcons_Network);
end;

{ TdxFlowChartAdvancedShapeRepository }

constructor TdxFlowChartAdvancedShapeRepository.Create;
begin
  FStencils := TObjectList<TdxFlowChartAdvancedShapeStencil>.Create;
  FShapes := TDictionary<string, TdxFlowChartObjectAdvancedShape>.Create(TdxIStringComparer.Ordinal);
  CreateDefaultStencils;
end;

destructor TdxFlowChartAdvancedShapeRepository.Destroy;
begin
  FShapes.Free;
  FStencils.Free;
  inherited Destroy;
end;

function TdxFlowChartAdvancedShapeRepository.FindShapeByID(const AID: string): TdxFlowChartObjectAdvancedShape;
begin
  if not FShapes.TryGetValue(AID, Result) then
    Result := nil;
end;

function TdxFlowChartAdvancedShapeRepository.FindStencilByID(const AID: string): TdxFlowChartAdvancedShapeStencil;
var
  I: Integer;
begin
  for I := 0 to StencilCount - 1 do
    if SameText(Stencils[I].ID, AID) then
      Exit(Stencils[I]);
  Result := nil;
end;

procedure TdxFlowChartAdvancedShapeRepository.AddStencil(AStencil: TdxFlowChartAdvancedShapeStencil);
begin
  FStencils.Add(AStencil);
end;

procedure TdxFlowChartAdvancedShapeRepository.CreateDefaultStencils;
begin
  FArrowShapes := TdxFlowChartArrowShapes.Create(Self, 'ArrowShapes', @sdxFlowChart_ArrowShapesCaption);
  FBasicFlowchartShapes := TdxFlowChartBasicFlowchartShapes.Create(Self, 'BasicFlowchartShapes', @sdxFlowChart_BasicFlowchartShapesCaption);
  FBasicShapes := TdxFlowChartBasicShapes.Create(Self, 'BasicShapes', @sdxFlowChart_BasicShapesCaption);
  FDecorativeShapes := TdxFlowChartDecorativeShapes.Create(Self, 'DecorativeShapes', @sdxFlowChart_DecorativeShapesCaption);
  FSDLDiagramShapes := TdxFlowChartSDLDiagramShapes.Create(Self, 'SDLDiagramShapes', @sdxFlowChart_SDLDiagramShapesCaption);
  FSoftwareIcons := TdxFlowChartSoftwareIcons.Create(Self, 'SoftwareIcons', @sdxFlowChart_SoftwareIconsCaption);
end;

procedure TdxFlowChartAdvancedShapeRepository.RemoveUserShapeByID(const AID: string);
var
  AShape: TdxFlowChartObjectAdvancedShape;
begin
  if FShapes.TryGetValue(AID, AShape) then
  begin
    if not (AShape is TFlowChartObjectUserAdvancedShape) then
      raise EdxException.CreateFmt('Can''t remove a predefined shape with ID="%s" not found!', [AID]);
    FShapes.Remove(AID);
    AShape.Stencil.RemoveShape(AShape);
  end
  else
    raise EdxException.CreateFmt('Shape description with ID="%s" not found!', [AID]);
end;

function TdxFlowChartAdvancedShapeRepository.GetStencil(Index: Integer): TdxFlowChartAdvancedShapeStencil;
begin
  Result := FStencils[Index];
end;

function TdxFlowChartAdvancedShapeRepository.GetStencilCount: Integer;
begin
  Result := FStencils.Count;
end;

procedure TdxFlowChartAdvancedShapeRepository.LoadShapesFromFile(const AFileName, AStencilID: string; const AStencilCaption: string = '');
var
  AStream: TStream;
begin
  AStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    LoadShapesFromStream(AStream, AStencilID, AStencilCaption);
  finally
    AStream.Free;
  end;
end;

procedure TdxFlowChartAdvancedShapeRepository.LoadShapesFromStream(const AStream: TStream; const AStencilID: string;
  const AStencilCaption: string = '');
var
  ALoader: TdxTemplateLoader;
  AStencil: TdxFlowChartAdvancedShapeStencil;
begin
  AStencil := FindStencilByID(AStencilID);
  if AStencil = nil then
    AStencil := TdxFlowChartAdvancedShapeStencil.Create(AStencilID, AStencilCaption);
  ALoader := TdxTemplateLoader.Create;
  try
    ALoader.LoadFromStream(AStream,
      procedure (ATemplate: TdxShapeXmlDescription)
      begin
        AStencil.ShapeDescriptions.Add(ATemplate.ID, ATemplate);
        AStencil.AddUserShape(ATemplate.ID, ATemplate.Caption);
      end);
  finally
    ALoader.Free;
  end;
end;

{ TdxShapeElementViewData }

constructor TdxShapeElementViewData.Create(APath: TdxGPPath; AProperties: TdxShapePathProperties);
begin
  FPath := APath;
  FProperties := AProperties;
end;

destructor TdxShapeElementViewData.Destroy;
begin
  FPath.Free;
  inherited Destroy;
end;

function TdxShapeElementViewData.GetFillBrightness: Single;
begin
  Result := FProperties.FillBrightness;
end;

function TdxShapeElementViewData.GetFillColor: TdxNullableValue<TdxAlphaColor>;
begin
  Result := FProperties.FillColor;
end;

function TdxShapeElementViewData.GetStrokeColor: TdxNullableValue<TdxAlphaColor>;
begin
  Result := FProperties.StrokeColor;
end;

function TdxShapeElementViewData.GetStrokeDashArray: TArray<Single>;
begin
  Result := FProperties.StrokeDashArray;
end;

function TdxShapeElementViewData.GetStrokeLineJoin: TdxGpLineJoin;
begin
  Result := FProperties.StrokeLineJoin;
end;

function TdxShapeElementViewData.GetStrokeThickness: TdxNullableValue<Single>;
begin
  Result := FProperties.StrokeThickness;
end;

function TdxShapeElementViewData.GetTransparent: Boolean;
begin
  Result := FProperties.Transparent;
end;

{ TdxShapeDescription }

constructor TdxShapeDescription.Create(const AID: string; const ADefaultSize: TSize; AUseBackgroundAsForeground: Boolean);
begin
  inherited Create;
  FDefaultSize.Init(ADefaultSize.cx, ADefaultSize.cy);
  FID := AID;
  FUseBackgroundAsForeground := AUseBackgroundAsForeground;
end;

procedure TdxShapeDescription.PaintDefaultShape(AGraphics: TdxGPCanvas; const ABounds: TRect; APenColor,
  ABrushColor: TdxAlphaColor);

  function GetBrush(AElement: TdxShapeElementViewData): TdxGPBrush;
  var
    AColor: TdxAlphaColor;
    AFillColor: TdxNullableValue<TdxAlphaColor>;
  begin
    if AElement.Transparent then
      Exit(nil);
    Result := TdxGPBrush.Create;
  {$IFDEF DELPHIXE}
    AFillColor := AElement.FillColor;
  {$ELSE}
    AFillColor := AElement.Properties.FillColor;
  {$ENDIF}
    if AFillColor.HasValue then
      AColor := AFillColor
    else
      AColor := ABrushColor;
    Result.Color := TdxAlphaColors.ChangeBrightness(AColor, AElement.FillBrightness);
  end;

  function GetPen(ALineJoin: TdxGpLineJoin): TdxGPPen;
  begin
    Result := TdxGPPen.Create(APenColor);
    Result.LineJoin := ALineJoin;
    Result.MiterLimit := 4;
  end;

var
  APen: TdxGPPen;
  ABrush: TdxGPBrush;
  AViewData: TdxShapeViewData;
  I, W, H: Integer;
  ASize: TdxSizeF;
  AParameters: TArray<Single>;
  ACoeff: Single;
begin
  ASize := DefaultSize;
  ACoeff := Min(ABounds.Width / ASize.Width, ABounds.Height / ASize.Height);
  W := Round(ASize.Width * ACoeff);
  H := Round(ASize.Height * ACoeff);
  AParameters := GetParameters(ASize, nil);
  AViewData := CreateViewData(ASize, AParameters);
  try
    AGraphics.SmoothingMode := smAntiAlias;
    AGraphics.SaveWorldTransform;
    try
      AGraphics.TranslateWorldTransform(
        ABounds.Left + ((ABounds.Width - W) div 2),
        ABounds.Top + ((ABounds.Height - H) div 2));
      AGraphics.ScaleWorldTransform(ACoeff, ACoeff);
      for I := 0 to AViewData.Count - 1 do
      begin
        APen := GetPen(AViewData[I].StrokeLineJoin);
        ABrush := GetBrush(AViewData[I]);
        try
          AGraphics.Path(AViewData[I].Path, APen, ABrush);
        finally
          ABrush.Free;
          APen.Free;
        end;
      end;
    finally
      AGraphics.RestoreWorldTransform;
    end;
  finally
    AViewData.Free;
  end;
end;

end.

