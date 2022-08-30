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

unit dxFlowChartShapeDescriptions.Code;

{$IFDEF WIN32}
  {$HPPEMIT '#pragma link "dxFlowChartShapeDescriptions.Code.obj"'}
{$ELSE}
  {$HPPEMIT '#pragma link "dxFlowChartShapeDescriptions.Code.o"'}
{$ENDIF}

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, SysUtils, Classes,
  Generics.Collections, Generics.Defaults, cxGeometry, dxGDIPlusAPI, dxGDIPlusClasses, dxCore,
  dxCoreClasses, dxCoreGraphics, dxflchrt, dxFlowChartShapes;

type

  TdxDiagramItemStyleId = string;

  { TdxShapeGeometry }

  TdxShapeGeometry = class(TdxShapeViewData)
  strict private
    FProperties: TObjectList<TdxShapePathProperties>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddSegment(APath: TdxGPPath; AProperties: TdxShapePathProperties);

    property Properties: TObjectList<TdxShapePathProperties> read FProperties;
  end;

  { TdxShapeCodeDescription }

  TdxShapeCodeDescription = class(TdxShapeDescription)
  {$REGION 'internal types'}
  protected type

    TGetShape = TFunc<Single, Single, TdxShapeGeometry>;
    TGetConnectionPoints = TFunc<Single, Single, TArray<TdxPointF>>;
    TParameterPointGetter = reference to function (const ASize: TdxSizeF; const AParameters: TArray<Single>; AValue: Single): TdxPointF;
    TParameterValueGetter = reference to function (const ASize: TdxSizeF; const AParameters: TArray<Single>; const ALocalPoint: TdxPointF): Single;
    TShapeGetter = reference to function (AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry;
    TShapeConnectionPointsGetter = reference to function (AWidth, AHeight: Single; const APs: TArray<Single>): TArray<TdxPointF>;

    TParameter = class
    strict private
      FDefaultValue: Single;
      FValue: Single;
      FGetValue: TParameterValueGetter;
      FGetPoint: TParameterPointGetter;
    public
      constructor Create(const AGetValue: TParameterValueGetter; const AGetPoint: TParameterPointGetter; ADefaultValue: Single);
      function GetValue(const ASize: TdxSizeF; const AParameters: TArray<Single>; const ALocalPoint: TdxPointF): Single;
      function GetPoint(const ASize: TdxSizeF; const AParameters: TArray<Single>; AValue: Single): TdxPointF;

      property DefaultValue: Single read FDefaultValue write FDefaultValue;
      property Value: Single read FValue write FValue;
    end;

    TParameters = class(TObjectList<TParameter>);

    TGetParameters = TFunc<TParameters>;

  {$ENDREGION}
  strict private
    FParameters: TParameters;
    FGetParameters: TGetParameters;
    FGetShape: TShapeGetter;
    FGetConnectionPoints: TShapeConnectionPointsGetter;
    function GetParametersCore: TParameters;
  strict protected
    property Parameters: TParameters read GetParametersCore;
  public
    constructor Create(const AId: string; const ADefaultSize: TSize;
      const AGetShape: TGetShape;
      const AGetConnectionPoints: TGetConnectionPoints;
      AStyleId: TdxDiagramItemStyleId = ''; AUseBackgroundAsForeground: Boolean = False); overload;
    constructor Create(const AId: string; const ADefaultSize: TSize;
      const AGetShape: TShapeGetter;
      const AGetConnectionPoints: TShapeConnectionPointsGetter;
      const AGetParameters: TGetParameters;
      AStyleId: TdxDiagramItemStyleId = ''; AUseBackgroundAsForeground: Boolean = False); overload;
    destructor Destroy; override;
    function CreateViewData(const ASize: TdxSizeF; const AParameters: TArray<Single>): TdxShapeViewData; override;
    function GetConnectionPoints(const ASize: TdxSizeF; const AParameters: TArray<Single>): TArray<TdxPointF>; override;
    function GetNormalizedParameters(const ASize: TdxSizeF; const AParameters: TArray<Single>): TArray<Single>; override;
    function GetParameterDefaultValue(AParameterIndex: Integer): Single; override;
    function GetParameterPoint(AParameterIndex: Integer; const ASize: TdxSizeF; const AParameters: TArray<Single>; AValue: Single): TdxPointF; override;
    function GetParameterValue(AParameterIndex: Integer; const ASize: TdxSizeF; const AParameters: TArray<Single>; const ALocalPoint: TdxPointF): Single; override;
    function GetParameters(const ASize: TdxSizeF; const AParameters: TArray<Single>): TArray<Single>; override;
  end;

  { TdxBasicCodeShapes }

  TdxBasicCodeShapes = class
  public type
    TParameter = TdxShapeCodeDescription.TParameter;
    TParameterCollection = TdxShapeCodeDescription.TParameters;
    TShapeGetter = TdxShapeCodeDescription.TShapeGetter;
    TShapeConnectionPointsGetter = TdxShapeCodeDescription.TShapeConnectionPointsGetter;
    TParameterPointGetter = TdxShapeCodeDescription.TParameterPointGetter;
    TParameterValueGetter = TdxShapeCodeDescription.TParameterValueGetter;

    TRectangleParameter<T> = record
    private
      FLeftTop: T;
      FRightTop: T;
      FRightBottom: T;
      FLeftBottom: T;
    public
      constructor Create(const AValue: T); overload;
      constructor Create(const ALeftTop, ARightTop, ARightBottom, ALeftBottom: T); overload;

      property LeftTop: T read FLeftTop write FLeftTop;
      property RightTop: T read FRightTop write FRightTop;
      property RightBottom: T read FRightBottom write FRightBottom;
      property LeftBottom: T read FLeftBottom write FLeftBottom;
    end;

  public const
    DefaultSnip = 0.2;
    DefaultRounding = 0.2;
    DefaultCanRadius = 0.2;
    DefaultFrameThickness = 10;
    DefaultLShapeThickness = 0.2;
    DefaultChevronThickness = 0.5;
    DefaultCubeDepth = 0.2;
    DefaultDonutThickness = 0.5;
    DefaultNoSymbolThickness = 0.3;
    DefaultStarAngle = PI / 5;
    DefaultEditorBoundsHeight = 20;
  public
    class procedure AddArcSegment(ASegments: TdxGPPath; const AStart, AEnd: TdxPointF;
      ARadiusX, ARadiusY, AAngle: Single; ASweep: Boolean); static;
    class function GetParameter(const AParameters: TArray<Single>; AIndex: Integer): Single; static;
    class function GetNormalizedParameter(const P: TArray<Single>; AIndex: Integer): Single; static;

    class function GetRectanglePoints(AWidth, AHeight: Single): TdxShapeGeometry; overload; static;
    class function GetRectangleSegments(AWidth, AHeight: Single; const AOffset: TdxPointF): TdxGPPath; overload; static;
    class function GetRectangleSegments(AWidth, AHeight: Single): TdxGPPath; overload; static;
    class function GetRectangleConnectionPoints(AWidth, AHeight: Single): TArray<TdxPointF>; static;
    class function GetCirclePoints(AWidth, AHeight: Single; APointCount: Integer; AStartPhase: Single): TArray<TdxPointF>; overload; static;
    class function GetCartesianPointByPolarPoint(AMagnitude, APhase: Single): TdxPointF; static;
    class function GetCircleConnectionPoints(AWidth, AHeight: Single): TArray<TdxPointF>; static;
    class function GetEllipsePoints(AWidth, AHeight: Single): TdxShapeGeometry; static;
    class function GetEllipsePath(AWidth, AHeight: Single; const AOffset: TdxPointF): TdxGPPath; overload; static;
    class function GetEllipsePath(AWidth, AHeight: Single): TdxGPPath; overload; static;
    class function GetEllipseConnectionPoints(AWidth, AHeight: Single): TArray<TdxPointF>; static;
    class function GetRightTrianglePoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry; static;
    class function GetRightTriangleParameters: TFunc<TParameterCollection>; static;
    class function GetRightTriangleConnectionPoints(AWidth, AHeight: Single; const APs: TArray<Single>): TArray<TdxPointF>; static;
    class function GetTrianglePoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry; static;
    class function GetTriangleParameters: TdxShapeCodeDescription.TGetParameters; overload; static;
    class function GetTriangleParameters(ADefaultValue: Single): TFunc<TParameterCollection>; overload; static;
    class function GetTriangleConnectionPoints(AWidth, AHeight: Single; const APs: TArray<Single>): TArray<TdxPointF>; static;
    class function GetCircleShapePoints(ARadius: Single; APointCount: Integer; AStartPhase: Single): TdxShapeGeometry; static;
    class function GetCircleShape(AWidth, AHeight: Single; APointCount: Integer; AStartPhase: Single): TdxShapeGeometry; static;
    class function CreatePolygon(AWidth, AHeight: Single; const APoints: TArray<TdxPointF>): TdxShapeGeometry; static;
  {$REGION 'Regular polygons'}
    class function GetPentagonPoints(AWidth, AHeight: Single): TdxShapeGeometry; static;
    class function GetHexagonPoints(AWidth, AHeight: Single): TdxShapeGeometry; static;
    class function GetHeptagonPoints(AWidth, AHeight: Single): TdxShapeGeometry; static;
    class function GetOctagonPointsCore(AWidth, AHeight: Single): TArray<TdxPointF>; static;
    class function GetOctagonPoints(AWidth, AHeight: Single): TdxShapeGeometry; static;
    class function GetDecagonPoints(AWidth, AHeight: Single): TdxShapeGeometry; static;
    class function GetPentagonConnectionPoints(AWidth, AHeight: Single): TArray<TdxPointF>; static;
    class function GetHexagonConnectionPoints(AWidth, AHeight: Single): TArray<TdxPointF>; static;
    class function GetHeptagonConnectionPoints(AWidth, AHeight: Single): TArray<TdxPointF>; static;
    class function GetOctagonConnectionPoints(AWidth, AHeight: Single): TArray<TdxPointF>; static;
    class function GetDecagonConnectionPoints(AWidth, AHeight: Single): TArray<TdxPointF>; static;
    class function GetCanRadius(AHeight: Single; P: TArray<Single>): Single; static;
    class function GetCanPoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry; static;
    class function GetCanConnectionPoints(AWidth, AHeight: Single; const P: TArray<Single>): TArray<TdxPointF>; static;
    class function GetCanParameters: TFunc<TParameterCollection>; static;
    class function GetParallelogramOffset(AWidth: Single; const P: TArray<Single>): Single; static;
    class function GetParallelogramPoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry; static;
    class function GetParallelogramConnectionPoints(AWidth, AHeight: Single; const P: TArray<Single>): TArray<TdxPointF>; static;
    class function GetParallelogramParameters: TFunc<TParameterCollection>; overload; static;
    class function GetParallelogramParameters(ADefaultValue: Single): TFunc<TParameterCollection>; overload; static;
    class function GetTrapezoidOffset(AWidth: Single; const P: TArray<Single>): Single; static;
    class function GetTrapezoidPoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry; static;
    class function GetTrapezoidConnectionPoints(AWidth, AHeight: Single; const P: TArray<Single>): TArray<TdxPointF>; static;
    class function GetTrapezoidParameters: TFunc<TParameterCollection>; static;
    class function GetDiamondPoints(AWidth, AHeight: Single): TdxShapeGeometry; static;
    class function GetDiamondConnectionPoints(AWidth, AHeight: Single): TArray<TdxPointF>; static;
    class function GetCubeDepth(AWidth, AHeight: Single; const P: TArray<Single>): Single; static;
    class function GetCubePoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry; static;
    class function GetCubeConnectionPoints(AWidth, AHeight: Single; const P: TArray<Single>): TArray<TdxPointF>; static;
    class function GetCubeParameters: TFunc<TParameterCollection>; static;
    class function GetChevronPoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry; static;
    class function GetChevronConnectionPoints(AWidth, AHeight: Single; const P: TArray<Single>): TArray<TdxPointF>; static;
    class function GetChevronParameters: TFunc<TParameterCollection>; static;
  {$ENDREGION}
  {$REGION 'Stars'}
    class function GetStar4Points(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry; static;
    class function GetStar5Points(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry; static;
    class function GetStar6Points(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry; static;
    class function GetStar7Points(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry; static;
    class function GetStar16Points(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry; static;
    class function GetStar24Points(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry; static;
    class function GetStar32Points(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry; static;
    class function GetStarPoints(APointCount: Integer): TShapeGetter; overload; static;
    class function GetStarPointsCore(APointCount: Integer; AWidth, AHeight: Single; const P: TArray<Single>): TArray<TdxPointF>; static;
    class function GetStarPoints(AOuterRadius: Single; APointCount: Integer; AAngle: Single): TArray<TdxPointF>; overload; static;
    class function GetStarParameters(APointCount: Integer; AAngle: Single): TFunc<TParameterCollection>; static;
    class function GetStarParameterPoint(APointCount: Integer): TParameterPointGetter; static;
    class function GetStarParameterValue(APointCount: Integer): TParameterValueGetter; static;
    class function GetStarAngleByPoint(APointCount: Integer; AWidth, AHeight: Single; const P: TdxPointF): Single; static;
    class function GetStar4Parameters: TFunc<TParameterCollection>; static;
    class function GetStar5Parameters: TFunc<TParameterCollection>; static;
    class function GetStar6Parameters: TFunc<TParameterCollection>; static;
    class function GetStar7Parameters: TFunc<TParameterCollection>; static;
    class function GetStar16Parameters: TFunc<TParameterCollection>; static;
    class function GetStar24Parameters: TFunc<TParameterCollection>; static;
    class function GetStar32Parameters: TFunc<TParameterCollection>; static;
    class function GetStarConnectionPoints(APointCount: Integer): TShapeConnectionPointsGetter; static;
    class function GetStar4ConnectionPoints(AWidth, AHeight: Single; const APs: TArray<Single>): TArray<TdxPointF>; static;
    class function GetStar5ConnectionPoints(AWidth, AHeight: Single; const APs: TArray<Single>): TArray<TdxPointF>; static;
    class function GetStar6ConnectionPoints(AWidth, AHeight: Single; const APs: TArray<Single>): TArray<TdxPointF>; static;
    class function GetStar7ConnectionPoints(AWidth, AHeight: Single; const APs: TArray<Single>): TArray<TdxPointF>; static;
    class function GetStar16ConnectionPoints(AWidth, AHeight: Single; const APs: TArray<Single>): TArray<TdxPointF>; static;
    class function GetStar24ConnectionPoints(AWidth, AHeight: Single; const APs: TArray<Single>): TArray<TdxPointF>; static;
    class function GetStar32ConnectionPoints(AWidth, AHeight: Single; const APs: TArray<Single>): TArray<TdxPointF>; static;
  {$ENDREGION}
  {$REGION 'Rectangles'}
    class function GetRectanglePoints(AWidth, AHeight: Single; ARoundings: TRectangleParameter<Single>; AArcs: TRectangleParameter<Boolean>): TdxShapeGeometry; overload; static;
    class function GetMaxSnip(W, H: Single): Single; static;
    class function GetRectanglePoints(AWidth, AHeight: Single;
      {$IFDEF DELPHIXE}const{$ENDIF} ARoundings: TRectangleParameter<Single>;
      {$IFDEF DELPHIXE}const{$ENDIF} AArcs: TRectangleParameter<TdxNullableValue<TdxSweepDirectionKind>>): TdxShapeGeometry; overload; static;
    class procedure AddRectangleSegment(ASegments: TdxGPPath; const ALastPoint, AEnd: TdxPointF;
      {$IFDEF DELPHIXE}const{$ENDIF} ASweepDirection: TdxNullableValue<TdxSweepDirectionKind>); static;
    class function GetRectangleParameterCollection(AParam: Single): TFunc<TParameterCollection>; overload; static;
    class function GetRectangleParameterByIndex(AIndex: Integer; ADefaultValue: Single): TParameter; static;
    class function GetRectangleParameterCollection(APs: TArray<Single>): TFunc<TParameterCollection>; overload; static;
    class function GetRoundedRectanglePoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry; static;
    class function GetRoundedRectangleParameters: TFunc<TParameterCollection>; static;
    class function GetSingleSnipCornerRectanglePoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry; static;
    class function GetSingleSnipCornerRectangleParameters: TFunc<TParameterCollection>; static;
    class function GetSnipSameSideCornerRectanglePoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry; static;
    class function GetSnipSameSideCornerRectangleParameters: TFunc<TParameterCollection>; static;
    class function GetSnipDiagonalCornerRectanglePoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry; static;
    class function GetSnipDiagonalCornerRectangleParameters: TFunc<TParameterCollection>; static;
    class function GetSingleRoundCornerRectanglePoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry; static;
    class function GetSingleRoundCornerRectangleParameters: TFunc<TParameterCollection>; static;
    class function GetRoundSameSideCornerRectanglePoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry; static;
    class function GetRoundSameSideCornerRectangleParameters: TFunc<TParameterCollection>; static;
    class function GetRoundDiagonalCornerRectanglePoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry; static;
    class function GetRoundDiagonalCornerRectangleParameters: TFunc<TParameterCollection>; static;
    class function GetSnipAndRoundSingleCornerRectanglePoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry; static;
    class function GetSnipAndRoundSingleCornerRectangleParameters: TFunc<TParameterCollection>; static;
    class function GetSnipCornerRectanglePoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry; static;
    class function GetSnipCornerRectangleParameters: TFunc<TParameterCollection>; static;
    class function GetRoundCornerRectanglePoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry; static;
    class function GetRoundCornerRectangleParameters: TFunc<TParameterCollection>; static;
    class function GetSnipAndRoundCornerRectanglePoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry; static;
    class function GetSnipAndRoundCornerRectangleParameters: TFunc<TParameterCollection>; static;
    class function GetRoundedRectangleConnectionPoints(AWidth, AHeight: Single; const APs: TArray<Single>): TArray<TdxPointF>; static;
    class function GetSingleSnipCornerRectangleConnectionPoints(AWidth, AHeight: Single; const APs: TArray<Single>): TArray<TdxPointF>; static;
    class function GetSnipSameSideCornerRectangleConnectionPoints(AWidth, AHeight: Single; const APs: TArray<Single>): TArray<TdxPointF>; static;
    class function GetSnipDiagonalCornerRectangleConnectionPoints(AWidth, AHeight: Single; const APs: TArray<Single>): TArray<TdxPointF>; static;
    class function GetSingleRoundCornerRectangleConnectionPoints(AWidth, AHeight: Single; const APs: TArray<Single>): TArray<TdxPointF>; static;
    class function GetRoundSameSideCornerRectangleConnectionPoints(AWidth, AHeight: Single; const APs: TArray<Single>): TArray<TdxPointF>; static;
    class function GetRoundDiagonalCornerRectangleConnectionPoints(AWidth, AHeight: Single; const APs: TArray<Single>): TArray<TdxPointF>; static;
    class function GetSnipAndRoundSingleCornerRectangleConnectionPoints(AWidth, AHeight: Single; const APs: TArray<Single>): TArray<TdxPointF>; static;
    class function GetSnipCornerRectangleConnectionPoints(AWidth, AHeight: Single; const APs: TArray<Single>): TArray<TdxPointF>; static;
    class function GetRoundCornerRectangleConnectionPoints(AWidth, AHeight: Single; const APs: TArray<Single>): TArray<TdxPointF>; static;
    class function GetSnipAndRoundCornerRectangleConnectionPoints(AWidth, AHeight: Single; const APs: TArray<Single>): TArray<TdxPointF>; static;
  {$ENDREGION}
    class function GetPlaquePoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry; static;
    class function GetPlaqueConnectionPoints(AWidth, AHeight: Single; const P: TArray<Single>): TArray<TdxPointF>; static;
    class function GetPlaqueParameters: TFunc<TParameterCollection>; static;
    class function GetFramePoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry; static;
    class function NormalizeFrameThickness(AWidth, AHeight, ABaseThickness: Single): Single; static;
    class function GetFrameConnectionPoints(AWidth, AHeight: Single; const P: TArray<Single>): TArray<TdxPointF>; static;
    class function GetFrameParameters: TFunc<TParameterCollection>; static;
    class function GetFrameCornerPoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry; static;
    class function GetFrameCornerConnectionPoints(AWidth, AHeight: Single; const P: TArray<Single>): TArray<TdxPointF>; static;
    class function GetFrameCornerParameters: TFunc<TParameterCollection>; static;
    class function GetLShapeHorizontalOffset(AWidth: Single; const P: TArray<Single>): Single; static;
    class function GetLShapeVerticalOffset(AHeight: Single; const P: TArray<Single>): Single; static;
    class function GetLShapePoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry; static;
    class function GetLShapeConnectionPoints(AWidth, AHeight: Single; const P: TArray<Single>): TArray<TdxPointF>; static;
    class function GetLShapeParameters: TFunc<TParameterCollection>; static;
    class function GetDiagonalStripeOffset(X: Single; const P: TArray<Single>): Single; static;
    class function GetDiagonalStripePoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry; static;
    class function GetDiagonalStripeConnectionPoints(AWidth, AHeight: Single; const P: TArray<Single>): TArray<TdxPointF>; static;
    class function GetDiagonalStripeParameters: TFunc<TParameterCollection>; static;
    class function GetDonutPoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry; static;
    class function GetDonutConnectionPoints(AWidth, AHeight: Single; const P: TArray<Single>): TArray<TdxPointF>; static;
    class function GetDonutParameters: TFunc<TParameterCollection>; overload; static;
    class function GetDonutParameters(ADefaultValue: Single): TFunc<TParameterCollection>; overload; static;
    class function GetNoSymbolPoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry; static;
    class function GetNoSymbolConnectionPoints(AWidth, AHeight: Single; const P: TArray<Single>): TArray<TdxPointF>; static;
    class function GetNoSymbolParameters: TFunc<TParameterCollection>; static;
  {$REGION 'Shape Registration'}
    class function CreateRectangleShape: TdxShapeDescription; static;
    class function CreateEllipseShape: TdxShapeDescription; static;
    class function CreateTriangleShape: TdxShapeDescription; static;
    class function CreateRightTriangleShape: TdxShapeDescription; static;
    class function CreatePentagonShape: TdxShapeDescription; static;
    class function CreateHexagonShape: TdxShapeDescription; static;
    class function CreateHeptagonShape: TdxShapeDescription; static;
    class function CreateOctagonShape: TdxShapeDescription; static;
    class function CreateDecagonShape: TdxShapeDescription; static;
    class function CreateCanShape: TdxShapeDescription; static;
    class function CreateParallelogramShape: TdxShapeDescription; static;
    class function CreateTrapezoidShape: TdxShapeDescription; static;
    class function CreateDiamondShape: TdxShapeDescription; static;
    class function CreateCubeShape: TdxShapeDescription; static;
    class function CreateChevronShape: TdxShapeDescription; static;
    class function CreateStar4Shape: TdxShapeDescription; static;
    class function CreateStar5Shape: TdxShapeDescription; static;
    class function CreateStar6Shape: TdxShapeDescription; static;
    class function CreateStar7Shape: TdxShapeDescription; static;
    class function CreateStar16Shape: TdxShapeDescription; static;
    class function CreateStar24Shape: TdxShapeDescription; static;
    class function CreateStar32Shape: TdxShapeDescription; static;
    class function CreateRoundedRectangleShape: TdxShapeDescription; static;
    class function CreateSingleSnipCornerRectangleShape: TdxShapeDescription; static;
    class function CreateSnipSameSideCornerRectangleShape: TdxShapeDescription; static;
    class function CreateSnipDiagonalCornerRectangleShape: TdxShapeDescription; static;
    class function CreateSingleRoundCornerRectangleShape: TdxShapeDescription; static;
    class function CreateRoundSameSideCornerRectangleShape: TdxShapeDescription; static;
    class function CreateRoundDiagonalCornerRectangleShape: TdxShapeDescription; static;
    class function CreateSnipAndRoundSingleCornerRectangleShape: TdxShapeDescription; static;
    class function CreateSnipCornerRectangleShape: TdxShapeDescription; static;
    class function CreateRoundCornerRectangleShape: TdxShapeDescription; static;
    class function CreateSnipAndRoundCornerRectangleShape: TdxShapeDescription; static;
    class function CreatePlaqueShape: TdxShapeDescription; static;
    class function CreateFrameShape: TdxShapeDescription; static;
    class function CreateFrameCornerShape: TdxShapeDescription; static;
    class function CreateLShapeShape: TdxShapeDescription; static;
    class function CreateDiagonalStripeShape: TdxShapeDescription; static;
    class function CreateDonutShape: TdxShapeDescription; static;
    class function CreateNoSymbolShape: TdxShapeDescription; static;
  {$ENDREGION}
  end;

implementation

uses
  Math, dxFcStrs, dxTypeHelpers, dxSVGCoreParsers;

function Normalize(Value: Single): Single;
begin
  Result := EnsureRange(Value, 0.0, 1.0);
end;

{ TdxBasicCodeShapes.TRectangleParameter<T> }

constructor TdxBasicCodeShapes.TRectangleParameter<T>.Create(const AValue: T);
begin
  Create(AValue, AValue, AValue, AValue);
end;

constructor TdxBasicCodeShapes.TRectangleParameter<T>.Create(const ALeftTop, ARightTop, ARightBottom, ALeftBottom: T);
begin
  LeftTop := ALeftTop;
  RightTop := ARightTop;
  RightBottom := ARightBottom;
  LeftBottom := ALeftBottom;
end;

{ TdxBasicCodeShapes }

class procedure TdxBasicCodeShapes.AddArcSegment(ASegments: TdxGPPath; const AStart, AEnd: TdxPointF;
  ARadiusX, ARadiusY, AAngle: Single; ASweep: Boolean);
var
  AArcCommand: TdxSVGParserPathCommandArc;
begin
  AArcCommand := TdxSVGParserPathCommandArc.Create(AStart, AEnd, TdxPointF.Create(Abs(ARadiusX), Abs(ARadiusY)), AAngle, ASweep, False);
  try
    AArcCommand.Append(ASegments);
  finally
    AArcCommand.Free;
  end;
end;


class function TdxBasicCodeShapes.GetParameter(const AParameters: TArray<Single>; AIndex: Integer): Single;
begin
  if (AParameters <> nil) and (Length(AParameters) > AIndex) then
    Result := AParameters[AIndex]
  else
    raise EArgumentOutOfRangeException.Create('Invalid parameter index');
end;

class function TdxBasicCodeShapes.GetNormalizedParameter(const P: TArray<Single>; AIndex: Integer): Single;
begin
  Result := Normalize(GetParameter(P, AIndex));
end;

class function TdxBasicCodeShapes.GetRectanglePoints(AWidth, AHeight: Single): TdxShapeGeometry;
begin
  Result := TdxShapeGeometry.Create;
  Result.AddSegment(GetRectangleSegments(AWidth, AHeight), TdxShapePathProperties.Create);
end;

class function TdxBasicCodeShapes.GetRectangleSegments(AWidth, AHeight: Single): TdxGPPath;
begin
  Result := GetRectangleSegments(AWidth, AHeight, TdxPointF.Null);
end;

class function TdxBasicCodeShapes.GetRectangleSegments(AWidth, AHeight: Single; const AOffset: TdxPointF): TdxGPPath;
begin
  Result := TdxGPPath.Create;
  Result.AddRect(TdxRectF.Create(AOffset.X, AOffset.Y, AWidth - AOffset.X, AHeight - AOffset.Y));
end;

class function TdxBasicCodeShapes.GetRectangleConnectionPoints(AWidth, AHeight: Single): TArray<TdxPointF>;
begin
  Result := TArray<TdxPointF>.Create(TdxPointF.Create(AWidth / 2, 0), TdxPointF.Create(AWidth, AHeight / 2),
    TdxPointF.Create(AWidth / 2, AHeight), TdxPointF.Create(0, AHeight / 2));
end;

class function TdxBasicCodeShapes.GetCirclePoints(AWidth, AHeight: Single; APointCount: Integer; AStartPhase: Single): TArray<TdxPointF>;
var
  ADiameter, ARadius, APhase: Single;
  AScale: TdxSizeF;
  I: Integer;
  P: TdxPointF;
begin
  ADiameter := Min(AWidth, AHeight);
  ARadius := ADiameter / 2;
  AScale := TdxSizeF.Create(1, 1);
  if ADiameter > 0 then
    AScale := TdxSizeF.Create(AWidth / ADiameter, AHeight / ADiameter);
  APhase := AStartPhase;
  SetLength(Result, APointCount);
  for I := 0 to APointCount - 1 do
  begin
    P := GetCartesianPointByPolarPoint(ARadius, APhase);
    P.Offset(ARadius, ARadius);
    P.Scale(AScale);
    Result[I] := P;
    APhase := APhase + (2 * PI / APointCount);
  end;
end;

class function TdxBasicCodeShapes.GetCartesianPointByPolarPoint(AMagnitude, APhase: Single): TdxPointF;
begin
  Result.Init(AMagnitude * Cos(APhase), AMagnitude * Sin(APhase));
end;

class function TdxBasicCodeShapes.GetCircleConnectionPoints(AWidth, AHeight: Single): TArray<TdxPointF>;
begin
  Result := GetCirclePoints(AWidth, AHeight, 8, -PI / 2);
end;

class function TdxBasicCodeShapes.GetEllipsePoints(AWidth, AHeight: Single): TdxShapeGeometry;
begin
  Result := TdxShapeGeometry.Create;
  Result.AddSegment(GetEllipsePath(AWidth, AHeight), TdxShapePathProperties.Create);
end;

class function TdxBasicCodeShapes.GetEllipsePath(AWidth, AHeight: Single): TdxGPPath;
begin
  Result := GetEllipsePath(AWidth, AHeight, TdxPointF.Null);
end;

class function TdxBasicCodeShapes.GetEllipsePath(AWidth, AHeight: Single; const AOffset: TdxPointF): TdxGPPath;
begin
  Result := TdxGPPath.Create;
  Result.AddEllipse(TdxRectF.CreateSize(AOffset.X, AOffset.Y, AWidth, AHeight));
end;

class function TdxBasicCodeShapes.GetEllipseConnectionPoints(AWidth, AHeight: Single): TArray<TdxPointF>;
begin
  Result := GetRectangleConnectionPoints(AWidth, AHeight);
end;

class function TdxBasicCodeShapes.GetRightTrianglePoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry;
begin
  Result := GetTrianglePoints(AWidth, AHeight, P);
end;

class function TdxBasicCodeShapes.GetRightTriangleParameters: TFunc<TParameterCollection>;
begin
  Result := GetTriangleParameters(0);
end;

class function TdxBasicCodeShapes.GetRightTriangleConnectionPoints(AWidth, AHeight: Single; const APs: TArray<Single>): TArray<TdxPointF>;
begin
  Result := GetTriangleConnectionPoints(AWidth, AHeight, APs);
end;

class function TdxBasicCodeShapes.GetTrianglePoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry;
var
  AOffset: Single;
  ASegment: TdxGPPath;
begin
  AOffset := GetParameter(P, 0);
  Result := TdxShapeGeometry.Create;
  ASegment := TdxGPPath.Create;
  ASegment.AddPolygon(TArray<TdxPointF>.Create(
    TdxPointF.Create(AWidth * AOffset, 0),
    TdxPointF.Create(AWidth, AHeight),
    TdxPointF.Create(0, AHeight)));
  ASegment.FigureFinish;
  Result.AddSegment(ASegment, TdxShapePathProperties.Create);
end;

class function TdxBasicCodeShapes.GetTriangleParameters: TdxShapeCodeDescription.TGetParameters;
begin
  Result := GetTriangleParameters(0.5);
end;

class function TdxBasicCodeShapes.GetTriangleParameters(ADefaultValue: Single): TFunc<TParameterCollection>;
begin
  Result :=
    function (): TParameterCollection
    begin
      Result := TParameterCollection.Create;
      Result.Add(TParameter.Create(
        function (const ASize: TdxSizeF; const AParameters: TArray<Single>; const ALocalPoint: TdxPointF): Single
        begin
          Result := Normalize(ALocalPoint.X / ASize.Width);
        end,
        function (const ASize: TdxSizeF; const AParameters: TArray<Single>; AValue: Single): TdxPointF
        begin
          Result.Init(ASize.Width * Normalize(AValue), 0);
        end,
        ADefaultValue)
      );
    end;
end;

class function TdxBasicCodeShapes.GetTriangleConnectionPoints(AWidth, AHeight: Single; const APs: TArray<Single>): TArray<TdxPointF>;
var
  AOffset: Single;
begin
  AOffset := GetParameter(APs, 0);
  Result := TArray<TdxPointF>.Create(
    TdxPointF.Create(AWidth * AOffset, 0),
    TdxPointF.Create(AWidth, AHeight),
    TdxPointF.Create(0, AHeight));
end;

class function TdxBasicCodeShapes.GetCircleShapePoints(ARadius: Single; APointCount: Integer; AStartPhase: Single): TdxShapeGeometry;
begin
  Result := GetCircleShape(2 * ARadius, 2 * ARadius, APointCount, AStartPhase);
end;

class function TdxBasicCodeShapes.GetCircleShape(AWidth, AHeight: Single; APointCount: Integer; AStartPhase: Single): TdxShapeGeometry;
var
  APoints: TArray<TdxPointF>;
begin
  APoints := GetCirclePoints(AWidth, AHeight, APointCount, AStartPhase);
  Result := CreatePolygon(AWidth, AHeight, APoints);
end;

class function TdxBasicCodeShapes.CreatePolygon(AWidth, AHeight: Single; const APoints: TArray<TdxPointF>): TdxShapeGeometry;
var
  APath: TdxGPPath;
  APolygonPoints: TArray<TdxPointF>;
  I: Integer;
begin
  APath := TdxGPPath.Create;
  SetLength(APolygonPoints, Length(APoints));
  for I := Low(APoints) to High(APoints) do
    APolygonPoints[I] := APoints[I];
  APath.AddPolygon(APolygonPoints);
  APath.FigureFinish;
  Result := TdxShapeGeometry.Create;
  Result.AddSegment(APath, TdxShapePathProperties.Create);
end;

class function TdxBasicCodeShapes.GetPentagonPoints(AWidth, AHeight: Single): TdxShapeGeometry;
begin
  Result := GetCircleShape(AWidth, AHeight, 5, -PI / 2);
end;

class function TdxBasicCodeShapes.GetHexagonPoints(AWidth, AHeight: Single): TdxShapeGeometry;
begin
  Result := GetCircleShape(AWidth, AHeight, 6, 0);
end;

class function TdxBasicCodeShapes.GetHeptagonPoints(AWidth, AHeight: Single): TdxShapeGeometry;
begin
  Result := GetCircleShape(AWidth, AHeight, 7, -PI / 2);
end;

class function TdxBasicCodeShapes.GetOctagonPointsCore(AWidth, AHeight: Single): TArray<TdxPointF>;
var
  AMatrix: TdxGPMatrix;
  AScale: Single;
  I: Integer;
begin
  Result := GetCirclePoints(AWidth, AHeight, 8, PI / 8);
  AScale := 1 / Cos(PI / 8);
  AMatrix := TdxGPMatrix.Create;
  try
    AMatrix.Scale(TdxPointF.Create(AScale, AScale), TdxPointF.Create(AWidth / 2, AHeight / 2));
    for I := Low(Result) to High(Result) do
      Result[I] := AMatrix.TransformPoint(Result[I]);
  finally
    AMatrix.Free;
  end;
end;

class function TdxBasicCodeShapes.GetOctagonPoints(AWidth, AHeight: Single): TdxShapeGeometry;
begin
  Result := CreatePolygon(AWidth, AHeight, GetOctagonPointsCore(AWidth, AHeight));
end;

class function TdxBasicCodeShapes.GetDecagonPoints(AWidth, AHeight: Single): TdxShapeGeometry;
begin
  Result := GetCircleShape(AWidth, AHeight, 10, 0);
end;

class function TdxBasicCodeShapes.GetPentagonConnectionPoints(AWidth, AHeight: Single): TArray<TdxPointF>;
begin
  Result := GetCirclePoints(AWidth, AHeight, 5, -PI / 2);
end;

class function TdxBasicCodeShapes.GetHexagonConnectionPoints(AWidth, AHeight: Single): TArray<TdxPointF>;
begin
  Result := GetCirclePoints(AWidth, AHeight, 6, 0);
end;

class function TdxBasicCodeShapes.GetHeptagonConnectionPoints(AWidth, AHeight: Single): TArray<TdxPointF>;
begin
  Result := GetCirclePoints(AWidth, AHeight, 7, -PI / 2);
end;

class function TdxBasicCodeShapes.GetOctagonConnectionPoints(AWidth, AHeight: Single): TArray<TdxPointF>;
begin
  Result := GetOctagonPointsCore(AWidth, AHeight);
end;

class function TdxBasicCodeShapes.GetDecagonConnectionPoints(AWidth, AHeight: Single): TArray<TdxPointF>;
begin
  Result := GetCirclePoints(AWidth, AHeight, 10, 0);
end;

class function TdxBasicCodeShapes.GetCanRadius(AHeight: Single; P: TArray<Single>): Single;
begin
  Result := GetParameter(P, 0) * AHeight / 2;
end;

class function TdxBasicCodeShapes.GetCanPoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry;
var
  ARadius: Single;
  APath: TdxGPPath;
begin
  Result := TdxShapeGeometry.Create;

  ARadius := GetCanRadius(AHeight, P);

  APath := TdxGPPath.Create;
  APath.AddLine(0, ARadius, 0, AHeight - ARadius);
  APath.AddArc(0, AHeight - 2 * ARadius, Max(1, AWidth), Max(1, ARadius * 2), 180, -180);
  APath.AddLine(AWidth, AHeight - ARadius, AWidth, ARadius);
  APath.AddArc(0, 0, Max(1, AWidth), Max(1, ARadius * 2), 0, -180);
  APath.FigureFinish;

  Result.AddSegment(APath, TdxShapePathProperties.Create);

  APath := TdxGPPath.Create;
  APath.AddArc(0, 0, Max(1, AWidth), Max(1, ARadius * 2), 180, -180);

  Result.AddSegment(APath, TdxShapePathProperties.Create(True));
end;

class function TdxBasicCodeShapes.GetCanConnectionPoints(AWidth, AHeight: Single; const P: TArray<Single>): TArray<TdxPointF>;
var
  ARadius: Single;
begin
  ARadius := GetCanRadius(AHeight, P);
  Result := TArray<TdxPointF>.Create(
    TdxPointF.Create(AWidth / 2, 0),
    TdxPointF.Create(AWidth / 2, 2 * ARadius),
    TdxPointF.Create(AWidth, AHeight / 2),
    TdxPointF.Create(AWidth / 2, AHeight),
    TdxPointF.Create(0, AHeight / 2));
end;

class function TdxBasicCodeShapes.GetCanParameters: TFunc<TParameterCollection>;
begin
  Result :=
    function (): TParameterCollection
    begin
      Result := TParameterCollection.Create;
      Result.Add(TParameter.Create(
        function (const ASize: TdxSizeF; const AParameters: TArray<Single>; const ALocalPoint: TdxPointF): Single
        begin
          Result := Normalize(ALocalPoint.Y / ASize.Height);
        end,
        function (const ASize: TdxSizeF; const AParameters: TArray<Single>; AValue: Single): TdxPointF
        begin
          Result.Init(ASize.Width / 2, 2 * GetCanRadius(ASize.Height, TArray<Single>.Create(AValue)));
        end,
        DefaultCanRadius));
    end;
end;

class function TdxBasicCodeShapes.GetParallelogramOffset(AWidth: Single; const P: TArray<Single>): Single;
var
  ASideOffsetFactor: Single;
begin
  ASideOffsetFactor := GetNormalizedParameter(P, 0);
  Result := AWidth * ASideOffsetFactor;
end;

class function TdxBasicCodeShapes.GetParallelogramPoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry;
var
  APath: TdxGPPath;
  ASideOffset: Single;
begin
  ASideOffset := GetParallelogramOffset(AWidth, P);
  APath := TdxGPPath.Create;
  APath.AddPolygon(TArray<TdxPointF>.Create(
    TdxPointF.Create(ASideOffset, 0),
    TdxPointF.Create(AWidth, 0),
    TdxPointF.Create(AWidth - ASideOffset, AHeight),
    TdxPointF.Create(0, AHeight)));
  APath.FigureFinish;
  Result := TdxShapeGeometry.Create;
  Result.AddSegment(APath, TdxShapePathProperties.Create);
end;

class function TdxBasicCodeShapes.GetParallelogramConnectionPoints(AWidth, AHeight: Single; const P: TArray<Single>): TArray<TdxPointF>;
var
  ASideOffset: Single;
begin
  ASideOffset := GetParallelogramOffset(AWidth, P);
  Result := TArray<TdxPointF>.Create(
    TdxPointF.Create((AWidth + ASideOffset) / 2, 0),
    TdxPointF.Create(AWidth - ASideOffset / 2, AHeight / 2),
    TdxPointF.Create((AWidth - ASideOffset) / 2, AHeight),
    TdxPointF.Create(ASideOffset / 2, AHeight / 2));
end;

class function TdxBasicCodeShapes.GetParallelogramParameters: TFunc<TParameterCollection>;
begin
  Result := GetParallelogramParameters(DefaultSnip);
end;

class function TdxBasicCodeShapes.GetParallelogramParameters(ADefaultValue: Single): TFunc<TParameterCollection>;
begin
  Result :=
    function (): TParameterCollection
    begin
      Result := TParameterCollection.Create;
      Result.Add(TParameter.Create(
        function (const ASize: TdxSizeF; const AParameters: TArray<Single>; const ALocalPoint: TdxPointF): Single
        begin
          Result := Normalize(ALocalPoint.X / ASize.Width);
        end,
        function (const ASize: TdxSizeF; const AParameters: TArray<Single>; AValue: Single): TdxPointF
        begin
          Result.Init(ASize.Width * AValue, 0);
        end,
        ADefaultValue));
    end;
end;

class function TdxBasicCodeShapes.GetTrapezoidOffset(AWidth: Single; const P: TArray<Single>): Single;
var
  ASideOffsetFactor: Single;
begin
  ASideOffsetFactor := GetNormalizedParameter(P, 0);
  Result := (AWidth / 2) * ASideOffsetFactor;
end;

class function TdxBasicCodeShapes.GetTrapezoidPoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry;
var
  APath: TdxGPPath;
  ASideOffset: Single;
begin
  ASideOffset := GetTrapezoidOffset(AWidth, P);
  APath := TdxGPPath.Create;
  APath.AddPolygon(TArray<TdxPointF>.Create(
    TdxPointF.Create(ASideOffset, 0),
    TdxPointF.Create(AWidth - ASideOffset, 0),
    TdxPointF.Create(AWidth, AHeight),
    TdxPointF.Create(0, AHeight)));
  APath.FigureFinish;
  Result := TdxShapeGeometry.Create;
  Result.AddSegment(APath, TdxShapePathProperties.Create);
end;

class function TdxBasicCodeShapes.GetTrapezoidConnectionPoints(AWidth, AHeight: Single; const P: TArray<Single>): TArray<TdxPointF>;
var
  ASideOffset: Single;
begin
  ASideOffset := GetTrapezoidOffset(AWidth, P);
  Result := TArray<TdxPointF>.Create(
    TdxPointF.Create(AWidth / 2, 0),
    TdxPointF.Create(AWidth - ASideOffset / 2, AHeight / 2),
    TdxPointF.Create(AWidth / 2, AHeight),
    TdxPointF.Create(ASideOffset / 2, AHeight / 2));
end;

class function TdxBasicCodeShapes.GetTrapezoidParameters: TFunc<TParameterCollection>;
begin
  Result :=
    function (): TParameterCollection
    begin
      Result := TParameterCollection.Create;
      Result.Add(TParameter.Create(
        function (const ASize: TdxSizeF; const AParameters: TArray<Single>; const ALocalPoint: TdxPointF): Single
        begin
          Result := Normalize(ALocalPoint.X / (ASize.Width / 2));
        end,
        function (const ASize: TdxSizeF; const AParameters: TArray<Single>; AValue: Single): TdxPointF
        begin
          Result.Init(ASize.Width * AValue / 2, 0);
        end,
        DefaultSnip * 2));
    end;
end;

class function TdxBasicCodeShapes.GetDiamondPoints(AWidth, AHeight: Single): TdxShapeGeometry;
var
  APath: TdxGPPath;
begin
  APath := TdxGPPath.Create;
  APath.AddPolygon(TArray<TdxPointF>.Create(
    TdxPointF.Create(AWidth / 2, 0),
    TdxPointF.Create(AWidth, AHeight / 2),
    TdxPointF.Create(AWidth / 2, AHeight),
    TdxPointF.Create(0, AHeight / 2)));
  APath.FigureFinish;
  Result := TdxShapeGeometry.Create;
  Result.AddSegment(APath, TdxShapePathProperties.Create);
end;

class function TdxBasicCodeShapes.GetDiamondConnectionPoints(AWidth, AHeight: Single): TArray<TdxPointF>;
begin
  Result := GetRectangleConnectionPoints(AWidth, AHeight);
end;

class function TdxBasicCodeShapes.GetCubeDepth(AWidth, AHeight: Single; const P: TArray<Single>): Single;
var
  ADepthFactor: Single;
begin
  ADepthFactor := GetParameter(P, 0);
  Result := Min(AWidth, AHeight * ADepthFactor);
end;

class function TdxBasicCodeShapes.GetCubePoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry;
var
  ADepth: Single;
  APath: TdxGPPath;
  AProperties: TdxShapePathProperties;
begin
  ADepth := GetCubeDepth(AWidth, AHeight, P);

  Result := TdxShapeGeometry.Create;
  APath := TdxGPPath.Create;
  APath.AddPolygon(TArray<TdxPointF>.Create(
    TdxPointF.Create(0, ADepth),
    TdxPointF.Create(AWidth - ADepth, ADepth),
    TdxPointF.Create(AWidth - ADepth, AHeight),
    TdxPointF.Create(0, AHeight))
  );
  AProperties := TdxShapePathProperties.Create;
  AProperties.StrokeLineJoin := LineJoinRound;
  Result.AddSegment(APath, AProperties);

  APath := TdxGPPath.Create;
  APath.AddPolygon(TArray<TdxPointF>.Create(
    TdxPointF.Create(0, ADepth),
    TdxPointF.Create(ADepth, 0),
    TdxPointF.Create(AWidth, 0),
    TdxPointF.Create(AWidth - ADepth, ADepth))
  );

  AProperties := TdxShapePathProperties.Create;
  AProperties.StrokeLineJoin := LineJoinRound;
  AProperties.FillBrightness := 0.4;
  Result.AddSegment(APath, AProperties);

  APath := TdxGPPath.Create;
  APath.AddPolygon(TArray<TdxPointF>.Create(
    TdxPointF.Create(AWidth - ADepth, ADepth),
    TdxPointF.Create(AWidth, 0),
    TdxPointF.Create(AWidth, AHeight - ADepth),
    TdxPointF.Create(AWidth - ADepth, AHeight))
  );

  AProperties := TdxShapePathProperties.Create;
  AProperties.StrokeLineJoin := LineJoinRound;
  AProperties.FillBrightness := -0.4;
  Result.AddSegment(APath, AProperties);
end;

class function TdxBasicCodeShapes.GetCubeConnectionPoints(AWidth, AHeight: Single; const P: TArray<Single>): TArray<TdxPointF>;
var
  ADepth: Single;
begin
  ADepth := GetCubeDepth(AWidth, AHeight, P);
  Result := TArray<TdxPointF>.Create(
    TdxPointF.Create((AWidth - ADepth) / 2, ADepth),
    TdxPointF.Create(AWidth - ADepth, (ADepth + AHeight) / 2),
    TdxPointF.Create((AWidth - ADepth) / 2, AHeight),
    TdxPointF.Create(0, (ADepth + AHeight) / 2),
    TdxPointF.Create((ADepth + AWidth) / 2, 0),
    TdxPointF.Create(AWidth, (AHeight - ADepth) / 2));
end;

class function TdxBasicCodeShapes.GetCubeParameters: TFunc<TParameterCollection>;
begin
  Result :=
    function (): TParameterCollection
    begin
      Result := TParameterCollection.Create;
      Result.Add(TParameter.Create(
        function (const ASize: TdxSizeF; const AParameters: TArray<Single>; const ALocalPoint: TdxPointF): Single
        begin
          Result := Normalize(ALocalPoint.Y / ASize.Height);
        end,
        function (const ASize: TdxSizeF; const AParameters: TArray<Single>; AValue: Single): TdxPointF
        begin
          Result.Init(0, ASize.Height * AValue);
        end,
        DefaultCubeDepth));
    end;
end;

class function TdxBasicCodeShapes.GetChevronPoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry;
var
  ASideOffsetFactor, ASideOffset: Single;
  APath: TdxGPPath;
begin
  ASideOffsetFactor := GetParameter(P, 0);
  ASideOffset := AWidth * ASideOffsetFactor;
  Result := TdxShapeGeometry.Create;
  APath := TdxGPPath.Create;
  APath.AddPolygon(TArray<TdxPointF>.Create(
    TdxPointF.Create(0, 0),
    TdxPointF.Create(ASideOffset, 0),
    TdxPointF.Create(AWidth, AHeight / 2),
    TdxPointF.Create(ASideOffset, AHeight),
    TdxPointF.Create(0, AHeight),
    TdxPointF.Create(AWidth - ASideOffset, AHeight / 2))
  );
  Result.AddSegment(APath, TdxShapePathProperties.Create);
end;

class function TdxBasicCodeShapes.GetChevronConnectionPoints(AWidth, AHeight: Single; const P: TArray<Single>): TArray<TdxPointF>;
var
  ASideOffsetFactor, ASideOffset: Single;
begin
  ASideOffsetFactor := GetParameter(P, 0);
  ASideOffset := AWidth * ASideOffsetFactor;
  Result := TArray<TdxPointF>.Create(
    TdxPointF.Create(ASideOffset / 2, 0),
    TdxPointF.Create(AWidth, AHeight / 2),
    TdxPointF.Create(ASideOffset / 2, AHeight),
    TdxPointF.Create(AWidth - ASideOffset, AHeight / 2));
end;

class function TdxBasicCodeShapes.GetChevronParameters: TFunc<TParameterCollection>;
begin
  Result := GetParallelogramParameters(DefaultChevronThickness);
end;

class function TdxBasicCodeShapes.GetStar4Points(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry;
begin
  Result := GetStarPoints(4)(AWidth, AHeight, P);
end;

class function TdxBasicCodeShapes.GetStar5Points(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry;
begin
  Result := GetStarPoints(5)(AWidth, AHeight, P);
end;

class function TdxBasicCodeShapes.GetStar6Points(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry;
begin
  Result := GetStarPoints(6)(AWidth, AHeight, P);
end;

class function TdxBasicCodeShapes.GetStar7Points(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry;
begin
  Result := GetStarPoints(7)(AWidth, AHeight, P);
end;

class function TdxBasicCodeShapes.GetStar16Points(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry;
begin
  Result := GetStarPoints(16)(AWidth, AHeight, P);
end;

class function TdxBasicCodeShapes.GetStar24Points(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry;
begin
  Result := GetStarPoints(24)(AWidth, AHeight, P);
end;

class function TdxBasicCodeShapes.GetStar32Points(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry;
begin
  Result := GetStarPoints(32)(AWidth, AHeight, P);
end;

class function TdxBasicCodeShapes.GetStarPoints(APointCount: Integer): TShapeGetter;
begin
  Result :=
    function (AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry
    var
      APoints: TArray<TdxPointF>;
      APath: TdxGPPath;
    begin
      APoints := GetStarPointsCore(APointCount, AWidth, AHeight, P);
      Result := TdxShapeGeometry.Create;
      APath := TdxGPPath.Create;
      APath.AddPolygon(APoints);
      APath.FigureFinish;
      Result.AddSegment(APath, TdxShapePathProperties.Create);
    end;
end;

class function TdxBasicCodeShapes.GetStarPointsCore(APointCount: Integer; AWidth, AHeight: Single; const P: TArray<Single>): TArray<TdxPointF>;
var
  APhase, AOuterRadius, AMinSize: Single;
  AScale: TdxSizeF;
  I: Integer;
begin
  APhase := GetParameter(P, 0);
  AOuterRadius := Min(AWidth, AHeight) / 2;
  AMinSize := Max(1, Min(AWidth, AHeight));
  AScale.Init(AWidth / AMinSize, AHeight / AMinSize);
  Result := GetStarPoints(AOuterRadius, APointCount, APhase);
  for I := Low(Result) to High(Result) do
    Result[I].Scale(AScale);
end;

class function TdxBasicCodeShapes.GetStarPoints(AOuterRadius: Single; APointCount: Integer; AAngle: Single): TArray<TdxPointF>;
var
  ADefaultPhase, AInnerRadius, AOffset: Single;
  AOuterPoints, AInnerPoints: TArray<TdxPointF>;
  I: Integer;
begin
  ADefaultPhase := PI / APointCount;
  AInnerRadius := AOuterRadius * Sin(AAngle / 2) / Sin(PI - (ADefaultPhase + AAngle / 2));
  AOuterPoints := GetCirclePoints(AOuterRadius * 2, AOuterRadius * 2, APointCount, -PI / 2);
  AOffset := AOuterRadius - AInnerRadius;
  AInnerPoints := GetCirclePoints(AInnerRadius * 2, AInnerRadius * 2, APointCount, -PI / 2 + ADefaultPhase);
  for I := Low(AInnerPoints) to High(AInnerPoints) do
    AInnerPoints[I].Offset(AOffset, AOffset);

  SetLength(Result, APointCount * 2);
  for I := 0 to APointCount - 1 do
  begin
    Result[I * 2] := AOuterPoints[I];
    Result[I * 2 + 1] := AInnerPoints[I];
  end;
end;

class function TdxBasicCodeShapes.GetStarParameters(APointCount: Integer; AAngle: Single): TFunc<TParameterCollection>;
begin
  Result :=
    function (): TParameterCollection
    begin
      Result := TParameterCollection.Create;
      Result.Add(TParameter.Create(
        GetStarParameterValue(APointCount),
        GetStarParameterPoint(APointCount),
        AAngle));
    end;
end;

class function TdxBasicCodeShapes.GetStarParameterPoint(APointCount: Integer): TParameterPointGetter;
begin
  Result :=
    function (const ASize: TdxSizeF; const AParameters: TArray<Single>; AValue: Single): TdxPointF
    var
      APoints: TArray<TdxPointF>;
    begin
      APoints := GetStarPointsCore(APointCount, ASize.Width, ASize.Height, TArray<Single>.Create(AVAlue));
      Result := APoints[1];
    end;
end;

class function TdxBasicCodeShapes.GetStarParameterValue(APointCount: Integer): TParameterValueGetter;
begin
  Result :=
    function (const ASize: TdxSizeF; const AParameters: TArray<Single>; const ALocalPoint: TdxPointF): Single
    begin
      Result := GetStarAngleByPoint(APointCount, ASize.Width, ASize.Height, ALocalPoint);
    end;
end;

class function TdxBasicCodeShapes.GetStarAngleByPoint(APointCount: Integer; AWidth, AHeight: Single; const P: TdxPointF): Single;
var
  ARadius, Y, AAngle, X: Single;
begin
  ARadius := Min(AWidth, AHeight) / 2;
  Y := ARadius - EnsureRange(P.Y * Min(AWidth, AHeight) / AHeight, 0, ARadius);
  AAngle := PI / APointCount;
  X := Tan(AAngle) * Y;
  Y := ARadius - Y;
  if IsZero(Y) then
    Result := 1
  else
    Result := 2 * ArcTan(X / Y);
end;

class function TdxBasicCodeShapes.GetStar4Parameters: TFunc<TParameterCollection>;
begin
  Result := GetStarParameters(4, DefaultStarAngle);
end;

class function TdxBasicCodeShapes.GetStar5Parameters: TFunc<TParameterCollection>;
begin
  Result := GetStarParameters(5, DefaultStarAngle);
end;

class function TdxBasicCodeShapes.GetStar6Parameters: TFunc<TParameterCollection>;
begin
  Result := GetStarParameters(6, DefaultStarAngle);
end;

class function TdxBasicCodeShapes.GetStar7Parameters: TFunc<TParameterCollection>;
begin
  Result := GetStarParameters(7, DefaultStarAngle);
end;

class function TdxBasicCodeShapes.GetStar16Parameters: TFunc<TParameterCollection>;
begin
  Result := GetStarParameters(16, DefaultStarAngle);
end;

class function TdxBasicCodeShapes.GetStar24Parameters: TFunc<TParameterCollection>;
begin
  Result := GetStarParameters(24, DefaultStarAngle);
end;

class function TdxBasicCodeShapes.GetStar32Parameters: TFunc<TParameterCollection>;
begin
  Result := GetStarParameters(32, DefaultStarAngle);
end;

class function TdxBasicCodeShapes.GetStarConnectionPoints(APointCount: Integer): TShapeConnectionPointsGetter;
begin
  Result :=
    function (AWidth, AHeight: Single; const APs: TArray<Single>): TArray<TdxPointF>
    begin
      Result := GetStarPointsCore(APointCount, AWidth, AHeight, APs);
    end;
end;

class function TdxBasicCodeShapes.GetStar4ConnectionPoints(AWidth, AHeight: Single; const APs: TArray<Single>): TArray<TdxPointF>;
begin
  Result := GetStarConnectionPoints(4)(AWidth, AHeight, APs);
end;

class function TdxBasicCodeShapes.GetStar5ConnectionPoints(AWidth, AHeight: Single; const APs: TArray<Single>): TArray<TdxPointF>;
begin
  Result := GetStarConnectionPoints(5)(AWidth, AHeight, APs);
end;

class function TdxBasicCodeShapes.GetStar6ConnectionPoints(AWidth, AHeight: Single; const APs: TArray<Single>): TArray<TdxPointF>;
begin
  Result := GetStarConnectionPoints(6)(AWidth, AHeight, APs);
end;

class function TdxBasicCodeShapes.GetStar7ConnectionPoints(AWidth, AHeight: Single; const APs: TArray<Single>): TArray<TdxPointF>;
begin
  Result := GetStarConnectionPoints(7)(AWidth, AHeight, APs);
end;

class function TdxBasicCodeShapes.GetStar16ConnectionPoints(AWidth, AHeight: Single; const APs: TArray<Single>): TArray<TdxPointF>;
begin
  Result := GetStarConnectionPoints(16)(AWidth, AHeight, APs);
end;

class function TdxBasicCodeShapes.GetStar24ConnectionPoints(AWidth, AHeight: Single; const APs: TArray<Single>): TArray<TdxPointF>;
begin
  Result := GetStarConnectionPoints(24)(AWidth, AHeight, APs);
end;

class function TdxBasicCodeShapes.GetStar32ConnectionPoints(AWidth, AHeight: Single; const APs: TArray<Single>): TArray<TdxPointF>;
begin
  Result := GetStarConnectionPoints(32)(AWidth, AHeight, APs);
end;

class function TdxBasicCodeShapes.GetRectanglePoints(AWidth, AHeight: Single; ARoundings: TRectangleParameter<Single>; AArcs: TRectangleParameter<Boolean>): TdxShapeGeometry;

  function GetDirection(AHasValue: Boolean): TdxNullableValue<TdxSweepDirectionKind>;
  begin
    if AHasValue then
      Result := TdxSweepDirectionKind.Clockwise
    else
      Result.Reset;
  end;

var
  ADirections: TRectangleParameter<TdxNullableValue<TdxSweepDirectionKind>>;
begin
  ADirections := TRectangleParameter<TdxNullableValue<TdxSweepDirectionKind>>.Create(
    GetDirection(AArcs.LeftTop), GetDirection(AArcs.RightTop), GetDirection(AArcs.RightBottom), GetDirection(AArcs.LeftBottom));
  Result := GetRectanglePoints(AWidth, AHeight, ARoundings, ADirections);
end;

class function TdxBasicCodeShapes.GetMaxSnip(W, H: Single): Single;
begin
  Result := Min(W, H) / 2;
end;

class function TdxBasicCodeShapes.GetRectanglePoints(AWidth, AHeight: Single;
  {$IFDEF DELPHIXE}const{$ENDIF} ARoundings: TRectangleParameter<Single>;
  {$IFDEF DELPHIXE}const{$ENDIF} AArcs: TRectangleParameter<TdxNullableValue<TdxSweepDirectionKind>>): TdxShapeGeometry;
var
  AMaxSnip, ALeftTopSnip, ARightTopSnip, ARightBottomSnip, ALeftBottomSnip: Single;
  APath: TdxGPPath;
  P1, P2: TdxPointF;
begin
  AMaxSnip := GetMaxSnip(AWidth, AHeight);
  ALeftTopSnip := ARoundings.LeftTop * AMaxSnip;
  ARightTopSnip := ARoundings.RightTop * AMaxSnip;
  ARightBottomSnip := ARoundings.RightBottom * AMaxSnip;
  ALeftBottomSnip := ARoundings.LeftBottom * AMaxSnip;

  Result := TdxShapeGeometry.Create;
  APath := TdxGPPath.Create;
  P1.Init(ALeftTopSnip, 0);
  AddRectangleSegment(APath, TdxPointF.Create(0, ALeftTopSnip), P1, AArcs.LeftTop);
  P2.Init(AWidth - ARightTopSnip, 0);
  APath.AddLine(P1.X, P1.Y, P2.X, P2.Y);
  P1.Init(AWidth, ARightTopSnip);
  AddRectangleSegment(APath, P2, P1, AArcs.RightTop);
  P2.Init(AWidth, AHeight - ARightBottomSnip);
  APath.AddLine(P1.X, P1.Y, P2.X, P2.Y);
  P1.Init(AWidth - ARightBottomSnip, AHeight);
  AddRectangleSegment(APath, P2, P1, AArcs.RightBottom);
  P2.Init(ALeftBottomSnip, AHeight);
  APath.AddLine(P1.X, P1.Y, P2.X, P2.Y);
  P1.Init(0, AHeight - ALeftBottomSnip);
  AddRectangleSegment(APath, P2, P1, AArcs.LeftBottom);
  APath.FigureFinish;
  Result.AddSegment(APath, TdxShapePathProperties.Create);
end;

class procedure TdxBasicCodeShapes.AddRectangleSegment(ASegments: TdxGPPath; const ALastPoint, AEnd: TdxPointF;
  {$IFDEF DELPHIXE}const{$ENDIF} ASweepDirection: TdxNullableValue<TdxSweepDirectionKind>);
begin
  if ASweepDirection.HasValue then
    AddArcSegment(ASegments, ALastPoint, AEnd, ALastPoint.X - AEnd.X, ALastPoint.Y - AEnd.Y, 0,
      ASweepDirection.Value = TdxSweepDirectionKind.Clockwise)
  else
    ASegments.AddLine(ALastPoint.X, ALastPoint.Y, AEnd.X, AEnd.Y);
end;

class function TdxBasicCodeShapes.GetRectangleParameterCollection(AParam: Single): TFunc<TParameterCollection>;
begin
  Result :=
    function (): TParameterCollection
    begin
      Result := TParameterCollection.Create;
      Result.Add(TParameter.Create(
        function (const ASize: TdxSizeF; const AParameters: TArray<Single>; const ALocalPoint: TdxPointF): Single
        begin
          Result := Normalize((ASize.Width - ALocalPoint.X) / GetMaxSnip(ASize.Width, ASize.Height));
        end,
        function (const ASize: TdxSizeF; const AParameters: TArray<Single>; AValue: Single): TdxPointF
        begin
          Result.Init(ASize.Width - AValue * GetMaxSnip(ASize.Width, ASize.Height), 0);
        end,
        AParam));
    end;
end;

class function TdxBasicCodeShapes.GetRectangleParameterByIndex(AIndex: Integer; ADefaultValue: Single): TParameter;
var
  ALeft, ATop: Boolean;
begin
  ALeft := (AIndex = 0) or (AIndex = 3);
  ATop := (AIndex = 0) or (AIndex = 1);
  Result := TParameter.Create(
    function (const ASize: TdxSizeF; const AParameters: TArray<Single>; const ALocalPoint: TdxPointF): Single
    begin
      Result := Normalize(IfThen(ALeft, ALocalPoint.X, ASize.Width - ALocalPoint.X) / GetMaxSnip(ASize.Width, ASize.Height));
    end,
    function (const ASize: TdxSizeF; const AParameters: TArray<Single>; AValue: Single): TdxPointF
    var
      V: Single;
    begin
      V := AValue * GetMaxSnip(ASize.Width, ASize.Height);
      Result.Init(IfThen(ALeft, V, ASize.Width - V), IfThen(ATop, 0, ASize.Height));
    end,
    ADefaultValue);
end;

class function TdxBasicCodeShapes.GetRectangleParameterCollection(APs: TArray<Single>): TFunc<TParameterCollection>;
begin
  Result :=
    function (): TParameterCollection
    var
      I: Integer;
    begin
      Result := TParameterCollection.Create;
      for I := Low(APs) to High(APs) do
        Result.Add(GetRectangleParameterByIndex(I, APs[I]));
    end;
end;

class function TdxBasicCodeShapes.GetRoundedRectanglePoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry;
var
  ARounding: Single;
begin
  ARounding := GetParameter(P, 0);
  Result := GetRectanglePoints(AWidth, AHeight, TRectangleParameter<Single>.Create(ARounding), TRectangleParameter<Boolean>.Create(True));
end;

class function TdxBasicCodeShapes.GetRoundedRectangleParameters: TFunc<TParameterCollection>;
begin
  Result :=
    function (): TParameterCollection
    begin
      Result := TParameterCollection.Create;
      Result.Add(GetRectangleParameterByIndex(1, DefaultRounding));
    end;
end;

class function TdxBasicCodeShapes.GetSingleSnipCornerRectanglePoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry;
var
  ASnip: Single;
begin
  ASnip := GetParameter(P, 0);
  Result := GetRectanglePoints(AWidth, AHeight, TRectangleParameter<Single>.Create(0, ASnip, 0, 0), TRectangleParameter<Boolean>.Create(False));
end;

class function TdxBasicCodeShapes.GetSingleSnipCornerRectangleParameters: TFunc<TParameterCollection>;
begin
  Result :=
    function (): TParameterCollection
    begin
      Result := TParameterCollection.Create;
      Result.Add(GetRectangleParameterByIndex(1, DefaultSnip));
    end;
end;

class function TdxBasicCodeShapes.GetSnipSameSideCornerRectanglePoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry;
var
  ASnip: Single;
begin
  ASnip := GetParameter(P, 0);
  Result := GetRectanglePoints(AWidth, AHeight, TRectangleParameter<Single>.Create(ASnip, ASnip, 0, 0), TRectangleParameter<Boolean>.Create(False));
end;

class function TdxBasicCodeShapes.GetSnipSameSideCornerRectangleParameters: TFunc<TParameterCollection>;
begin
  Result :=
    function (): TParameterCollection
    begin
      Result := TParameterCollection.Create;
      Result.Add(GetRectangleParameterByIndex(1, DefaultSnip));
    end;
end;

class function TdxBasicCodeShapes.GetSnipDiagonalCornerRectanglePoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry;
var
  ASnip: Single;
begin
  ASnip := GetParameter(P, 0);
  Result := GetRectanglePoints(AWidth, AHeight, TRectangleParameter<Single>.Create(0, ASnip, 0, ASnip), TRectangleParameter<Boolean>.Create(False));
end;

class function TdxBasicCodeShapes.GetSnipDiagonalCornerRectangleParameters: TFunc<TParameterCollection>;
begin
  Result :=
    function (): TParameterCollection
    begin
      Result := TParameterCollection.Create;
      Result.Add(GetRectangleParameterByIndex(1, DefaultSnip));
    end;
end;

class function TdxBasicCodeShapes.GetSingleRoundCornerRectanglePoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry;
var
  ASnip: Single;
begin
  ASnip := GetParameter(P, 0);
  Result := GetRectanglePoints(AWidth, AHeight, TRectangleParameter<Single>.Create(0, ASnip, 0, 0), TRectangleParameter<Boolean>.Create(True));
end;

class function TdxBasicCodeShapes.GetSingleRoundCornerRectangleParameters: TFunc<TParameterCollection>;
begin
  Result :=
    function (): TParameterCollection
    begin
      Result := TParameterCollection.Create;
      Result.Add(GetRectangleParameterByIndex(1, DefaultRounding));
    end;
end;

class function TdxBasicCodeShapes.GetRoundSameSideCornerRectanglePoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry;
var
  ASnip: Single;
begin
  ASnip := GetParameter(P, 0);
  Result := GetRectanglePoints(AWidth, AHeight, TRectangleParameter<Single>.Create(ASnip, ASnip, 0, 0), TRectangleParameter<Boolean>.Create(True));
end;

class function TdxBasicCodeShapes.GetRoundSameSideCornerRectangleParameters: TFunc<TParameterCollection>;
begin
  Result :=
    function (): TParameterCollection
    begin
      Result := TParameterCollection.Create;
      Result.Add(GetRectangleParameterByIndex(1, DefaultRounding));
    end;
end;

class function TdxBasicCodeShapes.GetRoundDiagonalCornerRectanglePoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry;
var
  ASnip: Single;
begin
  ASnip := GetParameter(P, 0);
  Result := GetRectanglePoints(AWidth, AHeight, TRectangleParameter<Single>.Create(ASnip, 0, ASnip, 0), TRectangleParameter<Boolean>.Create(True));
end;

class function TdxBasicCodeShapes.GetRoundDiagonalCornerRectangleParameters: TFunc<TParameterCollection>;
begin
  Result :=
    function (): TParameterCollection
    begin
      Result := TParameterCollection.Create;
      Result.Add(GetRectangleParameterByIndex(2, DefaultRounding));
    end;
end;

class function TdxBasicCodeShapes.GetSnipAndRoundSingleCornerRectanglePoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry;
var
  ARounding, ASnip: Single;
begin
  ARounding := GetParameter(P, 0);
  ASnip := GetParameter(P, 1);
  Result := GetRectanglePoints(AWidth, AHeight, TRectangleParameter<Single>.Create(ARounding, ASnip, 0, 0),
    TRectangleParameter<Boolean>.Create(True, False, False, False));
end;

class function TdxBasicCodeShapes.GetSnipAndRoundSingleCornerRectangleParameters: TFunc<TParameterCollection>;
begin
  Result := GetRectangleParameterCollection(TArray<Single>.Create(DefaultRounding, DefaultSnip));
end;

class function TdxBasicCodeShapes.GetSnipCornerRectanglePoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry;
var
  ALeftTop, ARightTop, ARightBottom, ALeftBottom: Single;
begin
  ALeftTop := GetParameter(P, 0);
  ARightTop := GetParameter(P, 1);
  ARightBottom := GetParameter(P, 2);
  ALeftBottom := GetParameter(P, 3);
  Result := GetRectanglePoints(AWidth, AHeight, TRectangleParameter<Single>.Create(ALeftTop, ARightTop, ARightBottom, ALeftBottom),
    TRectangleParameter<Boolean>.Create(False));
end;

class function TdxBasicCodeShapes.GetSnipCornerRectangleParameters: TFunc<TParameterCollection>;
begin
  Result := GetRectangleParameterCollection(
    TArray<Single>.Create(DefaultSnip, DefaultSnip, DefaultSnip, DefaultSnip));
end;

class function TdxBasicCodeShapes.GetRoundCornerRectanglePoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry;
var
  ALeftTop, ARightTop, ARightBottom, ALeftBottom: Single;
begin
  ALeftTop := GetParameter(P, 0);
  ARightTop := GetParameter(P, 1);
  ARightBottom := GetParameter(P, 2);
  ALeftBottom := GetParameter(P, 3);
  Result := GetRectanglePoints(AWidth, AHeight, TRectangleParameter<Single>.Create(ALeftTop, ARightTop, ARightBottom, ALeftBottom),
    TRectangleParameter<Boolean>.Create(True));
end;

class function TdxBasicCodeShapes.GetRoundCornerRectangleParameters: TFunc<TParameterCollection>;
begin
  Result := GetRectangleParameterCollection(
    TArray<Single>.Create(DefaultRounding, DefaultRounding, DefaultRounding, DefaultRounding));
end;

class function TdxBasicCodeShapes.GetSnipAndRoundCornerRectanglePoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry;
begin
  Result := GetRectanglePoints(AWidth, AHeight, TRectangleParameter<Single>.Create(GetParameter(P, 0), GetParameter(P, 1),
    GetParameter(P, 2), GetParameter(P, 3)), TRectangleParameter<Boolean>.Create(True, False, True, False));
end;

class function TdxBasicCodeShapes.GetSnipAndRoundCornerRectangleParameters: TFunc<TParameterCollection>;
begin
  Result := GetRectangleParameterCollection(
    TArray<Single>.Create(DefaultRounding, DefaultSnip, DefaultRounding, DefaultSnip));
end;

class function TdxBasicCodeShapes.GetRoundedRectangleConnectionPoints(AWidth, AHeight: Single; const APs: TArray<Single>): TArray<TdxPointF>;
begin
  Result := GetRectangleConnectionPoints(AWidth, AHeight);
end;

class function TdxBasicCodeShapes.GetSingleSnipCornerRectangleConnectionPoints(AWidth, AHeight: Single; const APs: TArray<Single>): TArray<TdxPointF>;
begin
  Result := GetRectangleConnectionPoints(AWidth, AHeight);
end;

class function TdxBasicCodeShapes.GetSnipSameSideCornerRectangleConnectionPoints(AWidth, AHeight: Single; const APs: TArray<Single>): TArray<TdxPointF>;
begin
  Result := GetRectangleConnectionPoints(AWidth, AHeight);
end;

class function TdxBasicCodeShapes.GetSnipDiagonalCornerRectangleConnectionPoints(AWidth, AHeight: Single; const APs: TArray<Single>): TArray<TdxPointF>;
begin
  Result := GetRectangleConnectionPoints(AWidth, AHeight);
end;

class function TdxBasicCodeShapes.GetSingleRoundCornerRectangleConnectionPoints(AWidth, AHeight: Single; const APs: TArray<Single>): TArray<TdxPointF>;
begin
  Result := GetRectangleConnectionPoints(AWidth, AHeight);
end;

class function TdxBasicCodeShapes.GetRoundSameSideCornerRectangleConnectionPoints(AWidth, AHeight: Single; const APs: TArray<Single>): TArray<TdxPointF>;
begin
  Result := GetRectangleConnectionPoints(AWidth, AHeight);
end;

class function TdxBasicCodeShapes.GetRoundDiagonalCornerRectangleConnectionPoints(AWidth, AHeight: Single; const APs: TArray<Single>): TArray<TdxPointF>;
begin
  Result := GetRectangleConnectionPoints(AWidth, AHeight);
end;

class function TdxBasicCodeShapes.GetSnipAndRoundSingleCornerRectangleConnectionPoints(AWidth, AHeight: Single; const APs: TArray<Single>): TArray<TdxPointF>;
begin
  Result := GetRectangleConnectionPoints(AWidth, AHeight);
end;

class function TdxBasicCodeShapes.GetSnipCornerRectangleConnectionPoints(AWidth, AHeight: Single; const APs: TArray<Single>): TArray<TdxPointF>;
begin
  Result := GetRectangleConnectionPoints(AWidth, AHeight);
end;

class function TdxBasicCodeShapes.GetRoundCornerRectangleConnectionPoints(AWidth, AHeight: Single; const APs: TArray<Single>): TArray<TdxPointF>;
begin
  Result := GetRectangleConnectionPoints(AWidth, AHeight);
end;

class function TdxBasicCodeShapes.GetSnipAndRoundCornerRectangleConnectionPoints(AWidth, AHeight: Single; const APs: TArray<Single>): TArray<TdxPointF>;
begin
  Result := GetRectangleConnectionPoints(AWidth, AHeight);
end;

class function TdxBasicCodeShapes.GetPlaquePoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry;
var
  ARoundingPercent: Single;
begin
  ARoundingPercent := GetParameter(P, 0);
  Result := GetRectanglePoints(AWidth, AHeight,
    TRectangleParameter<Single>.Create(ARoundingPercent),
    TRectangleParameter<TdxNullableValue<TdxSweepDirectionKind>>.Create(TdxSweepDirectionKind.Counterclockwise));
end;

class function TdxBasicCodeShapes.GetPlaqueConnectionPoints(AWidth, AHeight: Single; const P: TArray<Single>): TArray<TdxPointF>;
begin
  Result := GetRectangleConnectionPoints(AWidth, AHeight);
end;

class function TdxBasicCodeShapes.GetPlaqueParameters: TFunc<TParameterCollection>;
begin
  Result := GetRectangleParameterCollection(DefaultRounding);
end;

class function TdxBasicCodeShapes.GetFramePoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry;
var
  AThickness: Single;
  APath: TdxGPPath;
  R: TdxRectF;
begin
  AThickness := NormalizeFrameThickness(AWidth, AHeight, GetParameter(P, 0));
  APath := TdxGPPath.Create;
  R.InitSize(0, 0, AWidth, AHeight);
  APath.AddRect(R);
  R.Inflate(-AThickness, -AThickness);
  APath.AddRect(R);
  Result := TdxShapeGeometry.Create;
  Result.AddSegment(APath, TdxShapePathProperties.Create);
end;

class function TdxBasicCodeShapes.NormalizeFrameThickness(AWidth, AHeight, ABaseThickness: Single): Single;
begin
  Result := EnsureRange(ABaseThickness, 0, Min(AWidth, AHeight) / 2);
end;

class function TdxBasicCodeShapes.GetFrameConnectionPoints(AWidth, AHeight: Single; const P: TArray<Single>): TArray<TdxPointF>;
begin
  Result := GetRectangleConnectionPoints(AWidth, AHeight);
end;

class function TdxBasicCodeShapes.GetFrameParameters: TFunc<TParameterCollection>;
begin
  Result :=
    function (): TParameterCollection
    begin
      Result := TParameterCollection.Create;
      Result.Add(TParameter.Create(
        function (const ASize: TdxSizeF; const AParameters: TArray<Single>; const ALocalPoint: TdxPointF): Single
        begin
          Result := NormalizeFrameThickness(ASize.Width, ASize.Height, ALocalPoint.X);
        end,
        function (const ASize: TdxSizeF; const AParameters: TArray<Single>; AValue: Single): TdxPointF
        begin
          Result.Init(NormalizeFrameThickness(ASize.Width, ASize.Height, AValue), 0);
        end,
        DefaultFrameThickness));
    end;
end;

class function TdxBasicCodeShapes.GetFrameCornerPoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry;
var
  AThickness: Single;
  APath: TdxGPPath;
begin
  AThickness := GetParameter(P, 0);
  APath := TdxGPPath.Create;
  APath.AddLine(0, 0, AWidth, 0);
  APath.AddLine(AWidth, 0, AWidth - AThickness, AThickness);
  APath.AddLine(AWidth - AThickness, AThickness, AThickness, AThickness);
  APath.AddLine(AThickness, AThickness, AThickness, AHeight - AThickness);
  APath.AddLine(AThickness, AHeight - AThickness, 0, AHeight);
  APath.FigureFinish;
  Result := TdxShapeGeometry.Create;
  Result.AddSegment(APath, TdxShapePathProperties.Create);
end;

class function TdxBasicCodeShapes.GetFrameCornerConnectionPoints(AWidth, AHeight: Single; const P: TArray<Single>): TArray<TdxPointF>;
var
  AThickness: Single;
begin
  AThickness := GetParameter(P, 0);
  Result := TArray<TdxPointF>.Create(
    TdxPointF.Create(AWidth / 2, 0),
    TdxPointF.Create(AWidth - AThickness / 2, AThickness / 2),
    TdxPointF.Create(AWidth / 2, AThickness),
    TdxPointF.Create(AThickness, AHeight / 2),
    TdxPointF.Create(AThickness / 2, AHeight - AThickness / 2),
    TdxPointF.Create(0, AHeight / 2));
end;

class function TdxBasicCodeShapes.GetFrameCornerParameters: TFunc<TParameterCollection>;
begin
  Result :=
    function (): TParameterCollection
    begin
      Result := TParameterCollection.Create;
      Result.Add(TParameter.Create(
        function (const ASize: TdxSizeF; const AParameters: TArray<Single>; const ALocalPoint: TdxPointF): Single
        begin
          Result := EnsureRange(ALocalPoint.X, 0, Min(ASize.Width, ASize.Height) / 2);
        end,
        function (const ASize: TdxSizeF; const AParameters: TArray<Single>; AValue: Single): TdxPointF
        begin
          Result.Init(AValue, 0);
        end,
        DefaultFrameThickness));
    end;
end;

class function TdxBasicCodeShapes.GetLShapeHorizontalOffset(AWidth: Single; const P: TArray<Single>): Single;
var
  AHorizontalOffset: Single;
begin
  AHorizontalOffset := GetParameter(P, 0);
  Result := AHorizontalOffset * AWidth;
end;

class function TdxBasicCodeShapes.GetLShapeVerticalOffset(AHeight: Single; const P: TArray<Single>): Single;
var
  AVerticalOffset: Single;
begin
  AVerticalOffset := GetParameter(P, 1);
  Result := AHeight - AVerticalOffset * AHeight;
end;

class function TdxBasicCodeShapes.GetLShapePoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry;
var
  AWidth1, AHeight1: Single;
  APath: TdxGPPath;
begin
  AWidth1 := GetLShapeHorizontalOffset(AWidth, P);
  AHeight1 := GetLShapeVerticalOffset(AHeight, P);

  APath := TdxGPPath.Create;
  APath.AddLine(0, 0, AWidth1, 0);
  APath.AddLine(AWidth1, 0, AWidth1, AHeight1);
  APath.AddLine(AWidth1, AHeight1, AWidth, AHeight1);
  APath.AddLine(AWidth, AHeight1, AWidth, AHeight);
  APath.AddLine(AWidth, AHeight, 0, AHeight);
  APath.FigureFinish;

  Result := TdxShapeGeometry.Create;
  Result.AddSegment(APath, TdxShapePathProperties.Create);
end;

class function TdxBasicCodeShapes.GetLShapeConnectionPoints(AWidth, AHeight: Single; const P: TArray<Single>): TArray<TdxPointF>;
var
  AWidth1, AHeight1: Single;
begin
  AWidth1 := GetLShapeHorizontalOffset(AWidth, P);
  AHeight1 := GetLShapeVerticalOffset(AHeight, P);
  Result := TArray<TdxPointF>.Create(
    TdxPointF.Create(AWidth1 / 2, 0),
    TdxPointF.Create(AWidth1, AHeight1 / 2),
    TdxPointF.Create((AWidth + AWidth1) / 2, AHeight1),
    TdxPointF.Create(AWidth, (AHeight + AHeight1) / 2),
    TdxPointF.Create(AWidth / 2, AHeight),
    TdxPointF.Create(0, AHeight / 2));
end;

class function TdxBasicCodeShapes.GetLShapeParameters: TFunc<TParameterCollection>;
begin
  Result :=
    function (): TParameterCollection
    begin
      Result := TParameterCollection.Create;
      Result.Add(TParameter.Create(
        function (const ASize: TdxSizeF; const AParameters: TArray<Single>; const ALocalPoint: TdxPointF): Single
        begin
          Result := Normalize(ALocalPoint.X / ASize.Width);
        end,
        function (const ASize: TdxSizeF; const AParameters: TArray<Single>; AValue: Single): TdxPointF
        begin
          Result.Init(ASize.Width * AValue, 0);
        end,
        DefaultLShapeThickness));

      Result.Add(TParameter.Create(
        function (const ASize: TdxSizeF; const AParameters: TArray<Single>; const ALocalPoint: TdxPointF): Single
        begin
          Result := 1 - Normalize(ALocalPoint.Y / ASize.Height);
        end,
        function (const ASize: TdxSizeF; const AParameters: TArray<Single>; AValue: Single): TdxPointF
        begin
          Result.Init(0, ASize.Height * (1 - AValue));
        end,
        DefaultLShapeThickness));
    end;
end;

class function TdxBasicCodeShapes.GetDiagonalStripeOffset(X: Single; const P: TArray<Single>): Single;
var
  AOffsetPercent: Single;
begin
  AOffsetPercent := GetParameter(P, 0);
  Result := X * AOffsetPercent;
end;

class function TdxBasicCodeShapes.GetDiagonalStripePoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry;
var
  AOffsetWidth, AOffsetHeight: Single;
  APath: TdxGPPath;
begin
  AOffsetWidth := GetDiagonalStripeOffset(AWidth, P);
  AOffsetHeight := GetDiagonalStripeOffset(AHeight, P);

  APath := TdxGPPath.Create;
  APath.AddLine(0, AHeight - AOffsetHeight, AWidth - AOffsetWidth, 0);
  APath.AddLine(AWidth - AOffsetWidth, 0, AWidth, 0);
  APath.AddLine(AWidth, 0, 0, AHeight);
  APath.AddLine(0, AHeight, 0, AHeight - AOffsetHeight);
  APath.FigureFinish;

  Result := TdxShapeGeometry.Create;
  Result.AddSegment(APath, TdxShapePathProperties.Create);
end;

class function TdxBasicCodeShapes.GetDiagonalStripeConnectionPoints(AWidth, AHeight: Single; const P: TArray<Single>): TArray<TdxPointF>;
var
  AOffsetWidth, AOffsetHeight: Single;
begin
  AOffsetWidth := GetDiagonalStripeOffset(AWidth, P);
  AOffsetHeight := GetDiagonalStripeOffset(AHeight, P);
  Result := TArray<TdxPointF>.Create(
    TdxPointF.Create(AWidth - AOffsetWidth / 2, 0),
    TdxPointF.Create(AWidth / 2, AHeight / 2),
    TdxPointF.Create(0, AHeight - AOffsetHeight / 2),
    TdxPointF.Create((AWidth - AOffsetWidth) / 2, (AHeight - AOffsetHeight) / 2));
end;

class function TdxBasicCodeShapes.GetDiagonalStripeParameters: TFunc<TParameterCollection>;
begin
  Result :=
    function (): TParameterCollection
    begin
      Result := TParameterCollection.Create;
      Result.Add(TParameter.Create(
        function (const ASize: TdxSizeF; const AParameters: TArray<Single>; const ALocalPoint: TdxPointF): Single
        begin
          Result := 1 - Normalize(ALocalPoint.Y / ASize.Height);
        end,
        function (const ASize: TdxSizeF; const AParameters: TArray<Single>; AValue: Single): TdxPointF
        begin
          Result.Init(0, ASize.Height - ASize.Height * AValue);
        end,
        DefaultLShapeThickness));
    end;
end;

class function TdxBasicCodeShapes.GetDonutPoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry;
var
  AInnerWidth, AInnerHeight: Single;
  AInnerEllipse, AOuterEllipse: TdxGPPath;
begin
  AInnerWidth := (1 - GetParameter(P, 0)) * AWidth;
  AInnerHeight := (1 - GetParameter(P, 0)) * AHeight;

  AOuterEllipse := GetEllipsePath(AWidth, AHeight);
  AInnerEllipse := GetEllipsePath(AInnerWidth, AInnerHeight, TdxPointF.Create((AWidth - AInnerWidth) / 2, (AHeight - AInnerHeight) / 2));
  try
    AOuterEllipse.AddPath(AInnerEllipse);
  finally
    AInnerEllipse.Free;
  end;
  Result := TdxShapeGeometry.Create;
  Result.AddSegment(AOuterEllipse, TdxShapePathProperties.Create);
end;

class function TdxBasicCodeShapes.GetDonutConnectionPoints(AWidth, AHeight: Single; const P: TArray<Single>): TArray<TdxPointF>;
begin
  Result := GetCircleConnectionPoints(AWidth, AHeight);
end;

class function TdxBasicCodeShapes.GetDonutParameters: TFunc<TParameterCollection>;
begin
  Result := GetDonutParameters(DefaultDonutThickness);
end;

class function TdxBasicCodeShapes.GetDonutParameters(ADefaultValue: Single): TFunc<TParameterCollection>;
begin
  Result :=
    function (): TParameterCollection
    begin
      Result := TParameterCollection.Create;
      Result.Add(TParameter.Create(
        function (const ASize: TdxSizeF; const AParameters: TArray<Single>; const ALocalPoint: TdxPointF): Single
        begin
          Result := Normalize(ALocalPoint.X / (ASize.Width / 2));
        end,
        function (const ASize: TdxSizeF; const AParameters: TArray<Single>; AValue: Single): TdxPointF
        begin
          Result.Init(ASize.Width * AValue / 2, ASize.Height / 2);
        end,
        ADefaultValue));
    end;
end;

class function TdxBasicCodeShapes.GetNoSymbolPoints(AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry;
var
  AOuterDiameter, AThickness, AInnerDiameter, AInnerRadius, AAngle, AAngle1, AAngle2, AAngle3, AAngle4, AOffset: Single;
  AOuterEllipse, AInnerEllipse: TdxGPPath;
  AOffsetPoint, P1, P2: TdxPointF;
begin
  AOuterDiameter := Min(AWidth, AHeight);
  AThickness := GetParameter(P, 0) * AOuterDiameter;
  AInnerDiameter := AOuterDiameter - AThickness;
  AOuterEllipse := GetEllipsePath(AOuterDiameter, AOuterDiameter);

  if (AInnerDiameter > 0) and (AThickness / AInnerDiameter / 2 < 1.0) then
  begin
    AAngle := ArcCos(AThickness / AInnerDiameter / 2);
    AAngle1 := PI / 4 - AAngle;
    AAngle2 := PI / 4 + AAngle;
    AAngle3 := AAngle1 + PI;
    AAngle4 := AAngle2 + PI;
    AInnerRadius := AInnerDiameter / 2;
    AOffset := AOuterDiameter / 2;
    AOffsetPoint.Init(AOffset, AOffset);
    AInnerEllipse := TdxGPPath.Create;
    try
      P1 := GetCartesianPointByPolarPoint(AInnerRadius, AAngle1);
      P1.Offset(AOffsetPoint);
      P2 := GetCartesianPointByPolarPoint(AInnerRadius, AAngle2);
      P2.Offset(AOffsetPoint);
      AddArcSegment(AInnerEllipse, P1, P2, AInnerRadius, AInnerRadius, 0, True);
      AInnerEllipse.AddLine(P2.X, P2.Y, P1.X, P1.Y);
      AInnerEllipse.FigureFinish;
      AInnerEllipse.FigureStart;
      P1 := GetCartesianPointByPolarPoint(AInnerRadius, AAngle3);
      P1.Offset(AOffsetPoint);
      P2 := GetCartesianPointByPolarPoint(AInnerRadius, AAngle4);
      P2.Offset(AOffsetPoint);
      AddArcSegment(AInnerEllipse, P1, P2, AInnerRadius, AInnerRadius, 0, True);
      AInnerEllipse.AddLine(P2.X, P2.Y, P1.X, P1.Y);
      AInnerEllipse.FigureFinish;
      AOuterEllipse.AddPath(AInnerEllipse);
    finally
      AInnerEllipse.Free;
    end;
  end;
  Result := TdxShapeGeometry.Create;
  Result.AddSegment(AOuterEllipse, TdxShapePathProperties.Create);
end;

class function TdxBasicCodeShapes.GetNoSymbolConnectionPoints(AWidth, AHeight: Single; const P: TArray<Single>): TArray<TdxPointF>;
begin
  Result := GetCircleConnectionPoints(AWidth, AHeight);
end;

class function TdxBasicCodeShapes.GetNoSymbolParameters: TFunc<TParameterCollection>;
begin
  Result := GetDonutParameters(DefaultNoSymbolThickness);
end;

//////////////////////////////////////////////////////////////////////////////////

class function TdxBasicCodeShapes.CreateRectangleShape: TdxShapeDescription;
begin
  Result := TdxShapeCodeDescription.Create('Rectangle',
    TSize.Create(100,75),
    GetRectanglePoints,
    GetRectangleConnectionPoints);
end;

class function TdxBasicCodeShapes.CreateEllipseShape: TdxShapeDescription;
begin
  Result := TdxShapeCodeDescription.Create('Ellipse',
    TSize.Create(100,75),
    GetEllipsePoints,
    GetEllipseConnectionPoints);
end;

class function TdxBasicCodeShapes.CreateTriangleShape: TdxShapeDescription;
begin
  Result := TdxShapeCodeDescription.Create('Triangle',
    TSize.Create(100,90),
    GetTrianglePoints,
    GetTriangleConnectionPoints,
    GetTriangleParameters());
end;

class function TdxBasicCodeShapes.CreateRightTriangleShape: TdxShapeDescription;
begin
  Result := TdxShapeCodeDescription.Create('RightTriangle',
    TSize.Create(100, 75),
    GetRightTrianglePoints,
    GetRightTriangleConnectionPoints,
    GetRightTriangleParameters());
end;

class function TdxBasicCodeShapes.CreatePentagonShape: TdxShapeDescription;
begin
  Result := TdxShapeCodeDescription.Create('Pentagon',
    TSize.Create(100,100),
    GetPentagonPoints,
    GetPentagonConnectionPoints);
end;

class function TdxBasicCodeShapes.CreateHexagonShape: TdxShapeDescription;
begin
  Result := TdxShapeCodeDescription.Create('Hexagon',
    TSize.Create(100,100),
    GetHexagonPoints,
    GetHexagonConnectionPoints);
end;

class function TdxBasicCodeShapes.CreateHeptagonShape: TdxShapeDescription;
begin
  Result := TdxShapeCodeDescription.Create('Heptagon',
    TSize.Create(100,100),
    GetHeptagonPoints,
    GetHeptagonConnectionPoints);
end;

class function TdxBasicCodeShapes.CreateOctagonShape: TdxShapeDescription;
begin
  Result := TdxShapeCodeDescription.Create('Octagon',
    TSize.Create(100,100),
    GetOctagonPoints,
    GetOctagonConnectionPoints);
end;

class function TdxBasicCodeShapes.CreateDecagonShape: TdxShapeDescription;
begin
  Result := TdxShapeCodeDescription.Create('Decagon',
    TSize.Create(100,100),
    GetDecagonPoints,
    GetDecagonConnectionPoints);
end;

class function TdxBasicCodeShapes.CreateCanShape: TdxShapeDescription;
begin
  Result := TdxShapeCodeDescription.Create('Can',
    TSize.Create(70,80),
    GetCanPoints,
    GetCanConnectionPoints,
    GetCanParameters());
end;

class function TdxBasicCodeShapes.CreateParallelogramShape: TdxShapeDescription;
begin
  Result := TdxShapeCodeDescription.Create('Parallelogram',
    TSize.Create(100,75),
    GetParallelogramPoints,
    GetParallelogramConnectionPoints,
    GetParallelogramParameters());
end;

class function TdxBasicCodeShapes.CreateTrapezoidShape: TdxShapeDescription;
begin
  Result := TdxShapeCodeDescription.Create('Trapezoid',
    TSize.Create(100,75),
    GetTrapezoidPoints,
    GetTrapezoidConnectionPoints,
    GetTrapezoidParameters());
end;

class function TdxBasicCodeShapes.CreateDiamondShape: TdxShapeDescription;
begin
  Result := TdxShapeCodeDescription.Create('Diamond',
    TSize.Create(100,75),
    GetDiamondPoints,
    GetDiamondConnectionPoints);
end;

class function TdxBasicCodeShapes.CreateCubeShape: TdxShapeDescription;
begin
  Result := TdxShapeCodeDescription.Create('Cube',
    TSize.Create(100,75),
    GetCubePoints,
    GetCubeConnectionPoints,
    GetCubeParameters());
end;


class function TdxBasicCodeShapes.CreateChevronShape: TdxShapeDescription;
begin
  Result := TdxShapeCodeDescription.Create('Chevron',
    TSize.Create(80,80),
    GetChevronPoints,
    GetChevronConnectionPoints,
    GetChevronParameters());
end;

class function TdxBasicCodeShapes.CreateStar4Shape: TdxShapeDescription;
begin
  Result := TdxShapeCodeDescription.Create('Star4',
    TSize.Create(100, 100),
    GetStar4Points,
    GetStar4ConnectionPoints,
    GetStar4Parameters());
end;

class function TdxBasicCodeShapes.CreateStar5Shape: TdxShapeDescription;
begin
  Result := TdxShapeCodeDescription.Create('Star5',
    TSize.Create(100, 100),
    GetStar5Points,
    GetStar5ConnectionPoints,
    GetStar5Parameters());
end;

class function TdxBasicCodeShapes.CreateStar6Shape: TdxShapeDescription;
begin
  Result := TdxShapeCodeDescription.Create('Star6',
    TSize.Create(100, 100),
    GetStar6Points,
    GetStar6ConnectionPoints,
    GetStar6Parameters());
end;

class function TdxBasicCodeShapes.CreateStar7Shape: TdxShapeDescription;
begin
  Result := TdxShapeCodeDescription.Create('Star7',
    TSize.Create(100, 100),
    GetStar7Points,
    GetStar7ConnectionPoints,
    GetStar7Parameters());
end;

class function TdxBasicCodeShapes.CreateStar16Shape: TdxShapeDescription;
begin
  Result := TdxShapeCodeDescription.Create('Star16',
    TSize.Create(100, 100),
    GetStar16Points,
    GetStar16ConnectionPoints,
    GetStar16Parameters());
end;

class function TdxBasicCodeShapes.CreateStar24Shape: TdxShapeDescription;
begin
  Result := TdxShapeCodeDescription.Create('Star24',
    TSize.Create(100, 100),
    GetStar24Points,
    GetStar24ConnectionPoints,
    GetStar24Parameters());
end;

class function TdxBasicCodeShapes.CreateStar32Shape: TdxShapeDescription;
begin
  Result := TdxShapeCodeDescription.Create('Star32',
    TSize.Create(100, 100),
    GetStar32Points,
    GetStar32ConnectionPoints,
    GetStar32Parameters());
end;

class function TdxBasicCodeShapes.CreateRoundedRectangleShape: TdxShapeDescription;
begin
  Result := TdxShapeCodeDescription.Create('RoundedRectangle',
    TSize.Create(100, 75),
    GetRoundedRectanglePoints,
    GetRoundedRectangleConnectionPoints,
    GetRoundedRectangleParameters());
end;

class function TdxBasicCodeShapes.CreateSingleSnipCornerRectangleShape: TdxShapeDescription;
begin
  Result := TdxShapeCodeDescription.Create('SingleSnipCornerRectangle',
    TSize.Create(100, 75),
    GetSingleSnipCornerRectanglePoints,
    GetSingleSnipCornerRectangleConnectionPoints,
    GetSingleSnipCornerRectangleParameters());
end;

class function TdxBasicCodeShapes.CreateSnipSameSideCornerRectangleShape: TdxShapeDescription;
begin
  Result := TdxShapeCodeDescription.Create('SnipSameSideCornerRectangle',
    TSize.Create(100, 75),
    GetSnipSameSideCornerRectanglePoints,
    GetSnipSameSideCornerRectangleConnectionPoints,
    GetSnipSameSideCornerRectangleParameters());
end;

class function TdxBasicCodeShapes.CreateSnipDiagonalCornerRectangleShape: TdxShapeDescription;
begin
  Result := TdxShapeCodeDescription.Create('SnipDiagonalCornerRectangle',
    TSize.Create(100, 75),
    GetSnipDiagonalCornerRectanglePoints,
    GetSnipDiagonalCornerRectangleConnectionPoints,
    GetSnipDiagonalCornerRectangleParameters());
end;

class function TdxBasicCodeShapes.CreateSingleRoundCornerRectangleShape: TdxShapeDescription;
begin
  Result := TdxShapeCodeDescription.Create('SingleRoundCornerRectangle',
    TSize.Create(100, 75),
    GetSingleRoundCornerRectanglePoints,
    GetSingleRoundCornerRectangleConnectionPoints,
    GetSingleRoundCornerRectangleParameters());
end;

class function TdxBasicCodeShapes.CreateRoundSameSideCornerRectangleShape: TdxShapeDescription;
begin
  Result := TdxShapeCodeDescription.Create('RoundSameSideCornerRectangle',
    TSize.Create(100, 75),
    GetRoundSameSideCornerRectanglePoints,
    GetRoundSameSideCornerRectangleConnectionPoints,
    GetRoundSameSideCornerRectangleParameters());
end;

class function TdxBasicCodeShapes.CreateRoundDiagonalCornerRectangleShape: TdxShapeDescription;
begin
  Result := TdxShapeCodeDescription.Create('RoundDiagonalCornerRectangle',
    TSize.Create(100, 75),
    GetRoundDiagonalCornerRectanglePoints,
    GetRoundDiagonalCornerRectangleConnectionPoints,
    GetRoundDiagonalCornerRectangleParameters());
end;

class function TdxBasicCodeShapes.CreateSnipAndRoundSingleCornerRectangleShape: TdxShapeDescription;
begin
  Result := TdxShapeCodeDescription.Create('SnipAndRoundSingleCornerRectangle',
    TSize.Create(100, 75),
    GetSnipAndRoundSingleCornerRectanglePoints,
    GetSnipAndRoundSingleCornerRectangleConnectionPoints,
    GetSnipAndRoundSingleCornerRectangleParameters());
end;

class function TdxBasicCodeShapes.CreateSnipCornerRectangleShape: TdxShapeDescription;
begin
  Result := TdxShapeCodeDescription.Create('SnipCornerRectangle',
    TSize.Create(100, 75),
    GetSnipCornerRectanglePoints,
    GetSnipCornerRectangleConnectionPoints,
    GetSnipCornerRectangleParameters());
end;

class function TdxBasicCodeShapes.CreateRoundCornerRectangleShape: TdxShapeDescription;
begin
  Result := TdxShapeCodeDescription.Create('RoundCornerRectangle',
    TSize.Create(100, 75),
    GetRoundCornerRectanglePoints,
    GetRoundCornerRectangleConnectionPoints,
    GetRoundCornerRectangleParameters());
end;

class function TdxBasicCodeShapes.CreateSnipAndRoundCornerRectangleShape: TdxShapeDescription;
begin
  Result := TdxShapeCodeDescription.Create('SnipAndRoundCornerRectangle',
    TSize.Create(100, 75),
    GetSnipAndRoundCornerRectanglePoints,
    GetSnipAndRoundCornerRectangleConnectionPoints,
    GetSnipAndRoundCornerRectangleParameters());
end;

class function TdxBasicCodeShapes.CreatePlaqueShape: TdxShapeDescription;
begin
  Result := TdxShapeCodeDescription.Create('Plaque',
    TSize.Create(100, 75),
    GetPlaquePoints,
    GetPlaqueConnectionPoints,
    GetPlaqueParameters());
end;

class function TdxBasicCodeShapes.CreateFrameShape: TdxShapeDescription;
begin
  Result := TdxShapeCodeDescription.Create('Frame',
    TSize.Create(100, 75),
    GetFramePoints,
    GetFrameConnectionPoints,
    GetFrameParameters());
end;

class function TdxBasicCodeShapes.CreateFrameCornerShape: TdxShapeDescription;
begin
  Result := TdxShapeCodeDescription.Create('FrameCorner',
    TSize.Create(100, 100),
    GetFrameCornerPoints,
    GetFrameCornerConnectionPoints,
    GetFrameCornerParameters());
end;

class function TdxBasicCodeShapes.CreateLShapeShape: TdxShapeDescription;
begin
  Result := TdxShapeCodeDescription.Create('LShape',
    TSize.Create(100, 100),
    GetLShapePoints,
    GetLShapeConnectionPoints,
    GetLShapeParameters());
end;

class function TdxBasicCodeShapes.CreateDiagonalStripeShape: TdxShapeDescription;
begin
  Result := TdxShapeCodeDescription.Create('DiagonalStripe',
    TSize.Create(100, 100),
    GetDiagonalStripePoints,
    GetDiagonalStripeConnectionPoints,
    GetDiagonalStripeParameters());
end;

class function TdxBasicCodeShapes.CreateDonutShape: TdxShapeDescription;
begin
  Result := TdxShapeCodeDescription.Create('Donut',
    TSize.Create(100, 100),
    GetDonutPoints,
    GetDonutConnectionPoints,
    GetDonutParameters());
end;

class function TdxBasicCodeShapes.CreateNoSymbolShape: TdxShapeDescription;
begin
  Result := TdxShapeCodeDescription.Create('NoSymbol',
    TSize.Create(100, 100),
    GetNoSymbolPoints,
    GetNoSymbolConnectionPoints,
    GetNoSymbolParameters());
end;

{ TdxShapeGeometry }

constructor TdxShapeGeometry.Create;
begin
  inherited Create;
  FProperties := TObjectList<TdxShapePathProperties>.Create;
end;

destructor TdxShapeGeometry.Destroy;
begin
  FProperties.Free;
  inherited Destroy;
end;

procedure TdxShapeGeometry.AddSegment(APath: TdxGPPath; AProperties: TdxShapePathProperties);
begin
  FProperties.Add(AProperties);
  Add(TdxShapeElementViewData.Create(APath, AProperties));
end;

{ TdxShapeCodeDescription.TParameter }

constructor TdxShapeCodeDescription.TParameter.Create(const AGetValue: TParameterValueGetter; const AGetPoint: TParameterPointGetter; ADefaultValue: Single);
begin
  FGetValue := AGetValue;
  FGetPoint := AGetPoint;
  FDefaultValue := ADefaultValue;
end;

function TdxShapeCodeDescription.TParameter.GetPoint(const ASize: TdxSizeF;
  const AParameters: TArray<Single>; AValue: Single): TdxPointF;
begin
  Result := FGetPoint(ASize, AParameters, AValue);
end;

function TdxShapeCodeDescription.TParameter.GetValue(const ASize: TdxSizeF;
  const AParameters: TArray<Single>; const ALocalPoint: TdxPointF): Single;
begin
  Result := FGetValue(ASize, AParameters, ALocalPoint);
end;

{ TdxShapeCodeDescription }

constructor TdxShapeCodeDescription.Create(const AId: string; const ADefaultSize: TSize; const AGetShape: TGetShape;
  const AGetConnectionPoints: TGetConnectionPoints; AStyleId: TdxDiagramItemStyleId = ''; AUseBackgroundAsForeground: Boolean = False);
begin
  Create(AId, ADefaultSize,
    function (AWidth, AHeight: Single; const P: TArray<Single>): TdxShapeGeometry
    begin
      Result := AGetShape(AWidth, AHeight);
    end,
    function (AWidth, AHeight: Single; const APs: TArray<Single>): TArray<TdxPointF>
    begin
      Result := AGetConnectionPoints(AWidth, AHeight);
    end,
    nil, AStyleId, AUseBackgroundAsForeground);
end;

constructor TdxShapeCodeDescription.Create(const AId: string; const ADefaultSize: TSize; const AGetShape: TShapeGetter;
  const AGetConnectionPoints: TShapeConnectionPointsGetter; const AGetParameters: TGetParameters;
  AStyleId: TdxDiagramItemStyleId = ''; AUseBackgroundAsForeground: Boolean = False);
begin
  inherited Create(AId, ADefaultSize, AUseBackgroundAsForeground);
  FGetShape := AGetShape;
  FGetConnectionPoints := AGetConnectionPoints;
  FGetParameters := AGetParameters;
end;

destructor TdxShapeCodeDescription.Destroy;
begin
  FParameters.Free;
  inherited Destroy;
end;

function TdxShapeCodeDescription.CreateViewData(const ASize: TdxSizeF; const AParameters: TArray<Single>): TdxShapeViewData;
begin
  Result := FGetShape(ASize.Width, ASize.Height, AParameters);
end;

function TdxShapeCodeDescription.GetConnectionPoints(const ASize: TdxSizeF;
  const AParameters: TArray<Single>): TArray<TdxPointF>;
begin
  Result := FGetConnectionPoints(ASize.Width, ASize.Height, AParameters);
end;

function TdxShapeCodeDescription.GetNormalizedParameters(const ASize: TdxSizeF;
  const AParameters: TArray<Single>): TArray<Single>;
begin
  Result := AParameters;
end;

function TdxShapeCodeDescription.GetParameterDefaultValue(AParameterIndex: Integer): Single;
begin
  Result := Parameters[AParameterIndex].DefaultValue;
end;

function TdxShapeCodeDescription.GetParameters(const ASize: TdxSizeF; const AParameters: TArray<Single>): TArray<Single>;
var
  I, ACount: Integer;
begin
  ACount := Parameters.Count;
  SetLength(Result, ACount);
  if AParameters = nil then
  begin
    for I := 0 to ACount - 1 do
      Result[I] := FParameters[I].DefaultValue;
    Exit;
  end;
  Result := AParameters;
end;

function TdxShapeCodeDescription.GetParametersCore: TParameters;
begin
  if FParameters = nil then
  begin
    if Assigned(FGetParameters) then
      FParameters := FGetParameters;
    if FParameters = nil then
      FParameters := TParameters.Create;
  end;
  Result := FParameters;
end;

function TdxShapeCodeDescription.GetParameterPoint(AParameterIndex: Integer; const ASize: TdxSizeF;
  const AParameters: TArray<Single>; AValue: Single): TdxPointF;
begin
  Result := FParameters[AParameterIndex].GetPoint(ASize, AParameters, AValue);
end;

function TdxShapeCodeDescription.GetParameterValue(AParameterIndex: Integer; const ASize: TdxSizeF;
  const AParameters: TArray<Single>; const ALocalPoint: TdxPointF): Single;
begin
  Result := Parameters[AParameterIndex].GetValue(ASize, AParameters, ALocalPoint);
end;

end.

