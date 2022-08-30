{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express Cross Platform Library graphics classes          }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL   }
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

unit dxShapePrimitives;

interface

{$I cxVer.inc}

uses
  SysUtils, Classes, TypInfo, Contnrs,
  dxCore, dxCoreGraphics, cxClasses, cxGeometry, dxGDIPlusAPI,
  dxGDIPlusClasses, dxXMLDoc;

type
  TdxShapeCustomBrush = class;
  TdxShapeBrushClass = class of TdxGPCustomBrush;
  TdxShapeCustomTransformation = class;
  TdxShapeTransformations = class;
  TdxShapeObject = class;
  TdxShapeObjectClass = class of TdxShapeObject;
  TdxShapeObjectReader = class;
  TdxShapeObjectReaderClass = class of TdxShapeObjectReader;

  TdxShapeTransformationType = (dxsttScale, dxsttSkew, dxsttRotate, dxsttTranslate);

  { TdxShapeList }

  TdxShapeList = class(TcxObjectList)
  protected
    procedure FreeItem(AIndex: Integer); override;
  end;

  { TdxShapeObject }

  TdxShapeObject = class(TPersistent)
  private
    FLockCount: Integer;
    FParent: TdxShapeObject;

    FTransformations: TdxShapeTransformations;
    FTransformMatrix: TdxGPMatrix;
    FTransformOrigin: TdxPointF;

    FOnChange: TNotifyEvent;

    function GetRenderTransformOrigin: string;
    procedure SetRenderTransformOrigin(const AValue: string);

    procedure TransformationsChanged(ASender: TObject);
  protected
    procedure Calculate; virtual;
    procedure Changed; virtual;
    procedure SetTransformMatrix(const AValue: TdxGPMatrix); virtual;

    property Parent: TdxShapeObject read FParent write FParent;
    property Transformations: TdxShapeTransformations read FTransformations;
    property TransformMatrix: TdxGPMatrix read FTransformMatrix write SetTransformMatrix;
  public
    constructor Create; overload; virtual;
    constructor Create(AParent: TdxShapeObject); overload; virtual;
    destructor Destroy; override;

    procedure Assign(ASource: TPersistent); override;

    procedure BeginUpdate;
    procedure EndUpdate;
    procedure CancelUpdate;

    class function GetName: string; virtual;
    class function GetReaderClass: TdxShapeObjectReaderClass; virtual;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property RenderTransformOrigin: string read GetRenderTransformOrigin write SetRenderTransformOrigin;
  end;

  { TdxCustomVectorShape }

  TdxCustomVectorShape = class(TdxShapeObject)
  private
    FBrush: TdxShapeCustomBrush;
    FGPPen: TdxGPPen;
    FName: string;
    FPath: TdxGPPath;
    FRefCount: Integer;

    FBounds: TdxRectF;
    FHeight: Single;
    FTopLeft: TdxPointF;
    FWidth: Single;

    procedure SetBrush(const AValue: TdxShapeCustomBrush);
    procedure SetLeft(const AValue: Single);
    procedure SetHeight(const AValue: Single);
    procedure SetStroke(const AValue: string);
    procedure SetStrokeThickness(const AValue: Single);
    procedure SetTop(const AValue: Single);
    procedure SetWidth(const AValue: Single);

    function CreatePath: TdxGPPath;
    function GetPath: TdxGPPath;
    function NeedCreatePath: Boolean;
    procedure ApplyTransformation(ATransformation: TdxShapeCustomTransformation);
    procedure BrushChangeHandler(ASender: TObject);
    procedure CalculateTransformMatrix;
    procedure DoApplyTransformation(AType: TdxShapeTransformationType; M11, M12, M21, M22, DX, DY: Single);
  protected
    procedure SetTransformMatrix(const AValue: TdxGPMatrix); override;

    function GetContentBounds: TdxRectF; virtual;
    procedure Calculate; override;
    procedure CreatePathContent(var APath: TdxGPPath); virtual;
    procedure Draw(AGraphics: TdxGPGraphics); virtual;
    procedure SetColor(AAlphaColor: TdxAlphaColor); virtual;
    procedure Reference;
    procedure Release;

    property Bounds: TdxRectF read FBounds;
    property TransformMatrix;
    property Pen: TdxGPPen read FGPPen;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(ASource: TPersistent); override;

    property ContentBounds: TdxRectF read GetContentBounds;
  published
    property Brush: TdxShapeCustomBrush read FBrush write SetBrush;
    property Left: Single read FTopLeft.X write SetLeft;
    property Name: string read FName write FName;
    property Height: Single read FHeight write SetHeight;
    property Top: Single read FTopLeft.Y write SetTop;
    property Width: Single read FWidth write SetWidth;
    property Stroke: string write SetStroke;
    property StrokeThickness: Single write SetStrokeThickness;
  end;

  { TdxShapeRectangle }

  TdxShapeRectangle = class(TdxCustomVectorShape)
  private
    FRadius: TdxPointF;

    procedure SetRadiusX(const AValue: Single);
    procedure SetRadiusY(const AValue: Single);
  protected
    procedure CreatePathContent(var APath: TdxGPPath); override;
  public
    constructor Create; override;
    class function GetName: string; override;
  published
    property RadiusX: Single read FRadius.X write SetRadiusX;
    property RadiusY: Single read FRadius.Y write SetRadiusY;
  end;

  { TdxShapeEllipse }

  TdxShapeEllipse = class(TdxCustomVectorShape)
  protected
    procedure CreatePathContent(var APath: TdxGPPath); override;
  public
    class function GetName: string; override;
  end;

  { TdxShapePath }

  TdxShapePath = class(TdxCustomVectorShape)
  private
    FData: AnsiString;
    FNormalizedPoints: TdxGpPointFDynArray;
    FPoints: TdxGpPointFDynArray;
    FPointTypes: TBytes;

    procedure SetData(const AValue: AnsiString);

    function GetPathBounds: TdxRectF;
    procedure CalculatePoints;
  protected
    function GetContentBounds: TdxRectF; override;
    procedure Calculate; override;
    procedure CreatePathContent(var APath: TdxGPPath); override;
  public
    class function GetName: string; override;
  published
    property Data: AnsiString read FData write SetData;
  end;

  { TdxShapeCanvas }

  TdxShapeCanvas = class(TdxCustomVectorShape)
  private
    FIsRoot: Boolean;
    FShapes: TdxShapeList;

    function GetCount: Integer;
    function GetShape(AIndex: Integer): TdxCustomVectorShape;
  protected
    function GetContentBounds: TdxRectF; override;
    procedure Add(AShape: TdxCustomVectorShape);
    procedure Calculate; override;
    procedure Draw(AGraphics: TdxGPGraphics); override;
    procedure SetShapeColor(AAlphaColor: TdxAlphaColor; const AShapeName: string);

    property IsRoot: Boolean read FIsRoot write FIsRoot;
  public
    constructor Create; override;
    destructor Destroy; override;

    class function GetName: string; override;
    class function GetReaderClass: TdxShapeObjectReaderClass; override;

    property Count: Integer read GetCount;
    property Shapes[Index: Integer]: TdxCustomVectorShape read GetShape;
  end;

  { TdxShapeViewBox }

  TdxShapeViewBox = class(TdxShapeCanvas)
  public
    class function GetName: string; override;
  end;

  { TdxShapeFakeBrush }

  TdxShapeFakeBrush = class(TdxShapeObject)
  public
    class function GetName: string; override;
    class function GetReaderClass: TdxShapeObjectReaderClass; override;
  end;

  { TdxShapeFakeGradientStops }

  TdxShapeFakeGradientStops = class(TdxShapeObject)
  public
    class function GetName: string; override;
    class function GetReaderClass: TdxShapeObjectReaderClass; override;
  end;

  { TdxShapeFakeCustomTransformation }

  TdxShapeFakeCustomTransformation = class(TdxShapeObject)
  public
    class function GetReaderClass: TdxShapeObjectReaderClass; override;
  end;

  { TdxShapeFakeRenderTransformation }

  TdxShapeFakeRenderTransformation = class(TdxShapeFakeCustomTransformation)
  public
    class function GetName: string; override;
  end;

  { TdxShapeFakeRelativeTransformation }

  TdxShapeFakeRelativeTransformation = class(TdxShapeFakeCustomTransformation)
  public
    class function GetName: string; override;
  end;

  { TdxShapeObjectReader }

  TdxShapeObjectReader = class
  private
    function GetShapePropertyName(const AAttributeName: string): string;
    procedure ReadAttribute(ABaseShape: TdxShapeObject; APropList: PPropList; APropCount: Integer; AAttribute: TdxXMLNodeAttribute);
  protected
    function CreateShapeFromNode(AParentShape: TdxShapeObject; ANode: TdxXMLNode): TdxShapeObject;
    procedure ReadAttributes(ABaseShape: TdxShapeObject; ANode: TdxXMLNode); virtual;
    procedure ReadChildNodes(AParentShape: TdxShapeObject; ANode: TdxXMLNode); virtual;
  public
    procedure ReadFromNode(ABaseObject: TdxShapeObject; ANode: TdxXMLNode);
  end;

  { TdxShapeTransformations }

  TdxShapeTransformations = class(TPersistent)
  private
    FItems: TObjectList;
    FOnChange: TNotifyEvent;

    function GetCount: Integer;
    function GetTransformation(AIndex: Integer): TdxShapeCustomTransformation;

    procedure Changed;
  protected
    procedure AddTransformation(ATransformation: TdxShapeObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(ASource: TPersistent); override;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TdxShapeCustomTransformation read GetTransformation;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { TdxShapeCustomBrush }

  TdxShapeCustomBrush = class(TdxShapeObject)
  private
    FGPBrush: TdxGPCustomBrush;

    procedure BrushChangeHandler(ASender: TObject);
  protected
    function GetBrushClass: TdxShapeBrushClass; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    property GPBrush: TdxGPCustomBrush read FGPBrush;
  end;

  { TdxShapeCustomTransformation }

  TdxShapeCustomTransformation = class(TdxShapeObject)
  protected
    FType: TdxShapeTransformationType;
  public
    property TransformationType: TdxShapeTransformationType read FType write FType;
  end;

  { TdxShapePathDataParser }

  TdxShapePathPointType = (dxpsptStart= 0, dxpsptLine = 1, dxpsptBezier = 3,
    dxpsptPathTypeMask = 7, dxpsptDashMode = 16, dxpsptPathMarker = 32, dxpsptCloseSubpath = 128);

  TdxShapePathDataParser = class
  private
    FData: string;
    FDataSize: Integer;
    FLastPointType: TdxShapePathPointType;
    FPosition: Integer;

    function GetCoordinate(var ACommand: Char): Single;
    function GetDigits: string;
    function GetNextPosition: Integer;
    function GetPointType(ACommand: Char): TdxShapePathPointType;
    function IsNumberChar(AChar: Char): Boolean;
  public
    procedure Parse(const AData: AnsiString; var APoints: TdxGpPointFDynArray; var APointTypes: TBytes);
  end;

  { TdxShapesFactory }

  TdxShapesFactory = class
  private
    FClasses: TStringList;

    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function GetShapeClass(AIndex: Integer): TdxShapeObjectClass; overload;
    function GetShapeClass(const AName: TdxXMLString): TdxShapeObjectClass; overload;
    procedure RegisterShape(AShapeClass: TdxShapeObjectClass);
    procedure UnregisterShape(AShapeClass: TdxShapeObjectClass);

    property Count: Integer read GetCount;
  end;

function dxColorNameToAlphaColor(const AColorName: string): TdxAlphaColor;
function dxShapesFactory: TdxShapesFactory;

function dxPointFToStr(const P: TdxPointF): string;
function dxStrToPointF(const S: string): TdxPointF;

implementation

uses
  Types, StrUtils, Graphics, Math, cxGraphics, dxCustomTree, dxShapeReaders, dxShapeTransformations, dxShapeBrushes;

const
  CoordinateSeparator = ',';

var
  AShapesFactory: TdxShapesFactory;

function dxColorNameToAlphaColor(const AColorName: string): TdxAlphaColor;
var
  AColor: TColor;
begin
  if Length(AColorName) > 0 then
  begin
    if AColorName[1] = '#' then
      Result := TdxColorHelper.HexCodeToAlphaColor(Copy(AColorName, 2, Length(AColorName) - 1), False)
    else
      if SameText(AColorName, 'Transparent') then
        Result := dxMakeAlphaColor(0, 0, 0, 0)
      else
      begin
        cxColorByName(AColorName, AColor);
        Result := dxColorToAlphaColor(AColor);
      end;
  end
  else
    Result := dxColorToAlphaColor(0);
end;

function dxPointFToStr(const P: TdxPointF): string;
begin
  Result := dxFloatToStr(P.X) + CoordinateSeparator + dxFloatToStr(P.Y);
end;

function dxStrToPointF(const S: string): TdxPointF;

  function GetCoordinate(const AString: string; AFirstCoordidate: Boolean): Single;
  var
    ASeparatorPosition: Integer;
  begin
    ASeparatorPosition := PosEx(CoordinateSeparator, AString);
    if AFirstCoordidate then
      Result := dxStrToFloat(Copy(AString, 1, ASeparatorPosition - 1))
    else
      Result := dxStrToFloat(Copy(AString, ASeparatorPosition + 1, Length(AString) - ASeparatorPosition + 1));
  end;

begin
  Result.X := GetCoordinate(S, True);
  Result.Y := GetCoordinate(S, False);
end;

{ TdxShapeList }

procedure TdxShapeList.FreeItem(AIndex: Integer);
begin
  (Items[AIndex] as TdxCustomVectorShape).Release;
end;

{ TdxShapeObject }

constructor TdxShapeObject.Create;
begin
  inherited;
  FTransformMatrix := TdxGPMatrix.Create;
  FTransformations := TdxShapeTransformations.Create;
  FTransformations.OnChange := TransformationsChanged;
end;

constructor TdxShapeObject.Create(AParent: TdxShapeObject);
begin
  Create;
  FParent := AParent;
end;

destructor TdxShapeObject.Destroy;
begin
  FreeAndNil(FTransformations);
  FreeAndNil(FTransformMatrix);
  inherited;
end;

procedure TdxShapeObject.Assign(ASource: TPersistent);
begin
  inherited;
  if ASource is TdxShapeObject then
    Parent := TdxShapeObject(ASource).Parent;
end;

procedure TdxShapeObject.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TdxShapeObject.EndUpdate;
begin
  Dec(FLockCount);
  if FLockCount = 0 then
    Calculate;
end;

procedure TdxShapeObject.CancelUpdate;
begin
  Dec(FLockCount);
end;

class function TdxShapeObject.GetName: string;
begin
  Result := 'BaseShapeObject';
end;

class function TdxShapeObject.GetReaderClass: TdxShapeObjectReaderClass;
begin
  Result := TdxShapeObjectReader;
end;

procedure TdxShapeObject.Calculate;
begin
// do nothing
end;

procedure TdxShapeObject.Changed;
begin
  if FLockCount = 0 then
    Calculate;
end;

procedure TdxShapeObject.SetTransformMatrix(const AValue: TdxGPMatrix);
begin
  FTransformMatrix.Assign(AValue);
end;

function TdxShapeObject.GetRenderTransformOrigin: string;
begin
  Result := dxPointFToStr(FTransformOrigin);
end;

procedure TdxShapeObject.SetRenderTransformOrigin(const AValue: string);
var
  APoint: TdxPointF;
begin
  APoint := dxStrToPointF(AValue);
  if (FTransformOrigin.X <> APoint.X) or (FTransformOrigin.Y <> APoint.Y) then
  begin
    FTransformOrigin := APoint;
    Changed;
  end;
end;

procedure TdxShapeObject.TransformationsChanged(ASender: TObject);
begin
  Changed;
end;

{ TdxCustomVectorShape }

constructor TdxCustomVectorShape.Create;
begin
  inherited;
  FGPPen := TdxGPPen.Create;
  FBrush := TdxShapeCustomBrush.Create;
  FBrush.OnChange := BrushChangeHandler;
  FPath := nil;
end;

destructor TdxCustomVectorShape.Destroy;
begin
  FreeAndNil(FPath);
  FreeAndNil(FBrush);
  FreeAndNil(FGPPen);
  inherited;
end;

procedure TdxCustomVectorShape.Assign(ASource: TPersistent);
begin
  inherited;
  if ASource is TdxCustomVectorShape then
  begin
    BeginUpdate;
    FTopLeft.X := TdxCustomVectorShape(ASource).Left;
    FTopLeft.Y := TdxCustomVectorShape(ASource).Top;
    Width := TdxCustomVectorShape(ASource).Width;
    Height := TdxCustomVectorShape(ASource).Height;
    Name := TdxCustomVectorShape(ASource).Name;
    Brush.Assign(TdxCustomVectorShape(ASource).Brush);
    Pen.Assign(TdxCustomVectorShape(ASource).Pen);
    EndUpdate;
  end;
end;

procedure TdxCustomVectorShape.SetTransformMatrix(const AValue: TdxGPMatrix);
begin
  inherited SetTransformMatrix(AValue);
  FreeAndNil(FPath);
  CreatePathContent(FPath);
end;

function TdxCustomVectorShape.GetContentBounds: TdxRectF;
begin
  Result.Left := FTopLeft.X;
  Result.Top := FTopLeft.Y;
  Result.Right := Result.Left + cxRectWidth(Bounds);
  Result.Bottom := Result.Top + cxRectHeight(Bounds);
end;

procedure TdxCustomVectorShape.Calculate;
begin
  FBounds := cxRectF(0, 0, FWidth, FHeight);
  CalculateTransformMatrix;
end;

procedure TdxCustomVectorShape.CreatePathContent(var APath: TdxGPPath);
begin
  APath := TdxGPPath.Create;
end;

procedure TdxCustomVectorShape.Draw(AGraphics: TdxGPGraphics);
begin
  AGraphics.Path(GetPath, FGPPen, FBrush.GPBrush);
end;

procedure TdxCustomVectorShape.SetColor(AAlphaColor: TdxAlphaColor);
begin
  if FBrush is TdxShapeSolidBrush then
    TdxShapeSolidBrush(FBrush).Color := AAlphaColor;
end;

procedure TdxCustomVectorShape.Reference;
begin
  Inc(FRefCount);
end;

procedure TdxCustomVectorShape.Release;
begin
  Dec(FRefCount);
  if FRefCount = 0 then
    Free;
end;

procedure TdxCustomVectorShape.SetBrush(const AValue: TdxShapeCustomBrush);
begin
  FreeAndNil(FBrush);
  FBrush := AValue;
  FBrush.OnChange := BrushChangeHandler;
  Changed;
end;

procedure TdxCustomVectorShape.SetLeft(const AValue: Single);
begin
  if FTopLeft.X <> AValue then
  begin
    FTopLeft.X := AValue;
    Changed;
  end;
end;

procedure TdxCustomVectorShape.SetHeight(const AValue: Single);
begin
  if FHeight <> AValue then
  begin
    FHeight := AValue;
    Changed;
  end;
end;

procedure TdxCustomVectorShape.SetStroke(const AValue: string);
begin
  FGPPen.Brush.Color := dxColorNameToAlphaColor(AValue);
end;

procedure TdxCustomVectorShape.SetStrokeThickness(const AValue: Single);
begin
  if FGPPen.Width <> AValue then
  begin
    FGPPen.Width := AValue;
    Changed;
  end;
end;

procedure TdxCustomVectorShape.SetTop(const AValue: Single);
begin
  if FTopLeft.Y <> AValue then
  begin
    FTopLeft.Y := AValue;
    Changed;
  end;
end;

procedure TdxCustomVectorShape.SetWidth(const AValue: Single);
begin
  if FWidth <> AValue then
  begin
    FWidth := AValue;
    Changed;
  end;
end;

function TdxCustomVectorShape.CreatePath: TdxGPPath;
begin
  CreatePathContent(FPath);
  GdipCheck(GdipTransformPath(FPath.Handle, TransformMatrix.Handle));
  Result := FPath;
end;

function TdxCustomVectorShape.GetPath: TdxGPPath;
begin
  if NeedCreatePath then
    Result := CreatePath
  else
    Result := FPath;
end;

function TdxCustomVectorShape.NeedCreatePath: Boolean;
begin
  Result := (FPath = nil) or not Assigned(FPath.Handle);
end;

procedure TdxCustomVectorShape.ApplyTransformation(ATransformation: TdxShapeCustomTransformation);

  function IsTransformationAvailable: Boolean;
  begin
    Result := ATransformation <> nil;
  end;

var
  APoint: TdxPointF;
begin
  case ATransformation.TransformationType of
    dxsttScale:
      if IsTransformationAvailable then
        DoApplyTransformation(dxsttScale, TdxShapeScaleTransformation(ATransformation).ScaleX, 0, 0,
          TdxShapeScaleTransformation(ATransformation).ScaleY, 0, 0);

    dxsttTranslate:
      if IsTransformationAvailable then
        DoApplyTransformation(dxsttTranslate, 1, 0, 0, 1, TdxShapeTranslateTransformation(ATransformation).X + FBounds.Left,
          TdxShapeTranslateTransformation(ATransformation).Y + FBounds.Top);

    dxsttSkew:
      if IsTransformationAvailable then
        DoApplyTransformation(dxsttSkew, 1, 0, 0, 1, DegToRad(TdxShapeSkewTransformation(ATransformation).AngleX),
          DegToRad(TdxShapeSkewTransformation(ATransformation).AngleY));

    dxsttRotate:
      if IsTransformationAvailable then
      begin
        APoint := FTransformOrigin;
        if FTransformOrigin.X = 0 then
          APoint.X := TdxShapeRotateTransformation(ATransformation).CenterX;
        if FTransformOrigin.Y = 0 then
          APoint.Y := TdxShapeRotateTransformation(ATransformation).CenterY;
        APoint := dxPointF(APoint.X * cxRectWidth(FBounds), APoint.Y * cxRectHeight(FBounds));
        DoApplyTransformation(dxsttRotate, TdxShapeRotateTransformation(ATransformation).Angle, 0, 0, 1, APoint.X, APoint.Y);
      end;
  end;
end;

procedure TdxCustomVectorShape.BrushChangeHandler(ASender: TObject);
begin
  Changed;
end;

procedure TdxCustomVectorShape.CalculateTransformMatrix;

  procedure DoTranslateTransformation;
  var
    ATranslate: TdxShapeTranslateTransformation;
  begin
    ATranslate := TdxShapeTranslateTransformation.Create;
    try
      ATranslate.X := FTopLeft.X ;
      ATranslate.Y := FTopLeft.Y ;
      DoApplyTransformation(dxsttTranslate, 1, 0, 0, 1, ATranslate.X + FBounds.Left, ATranslate.Y + FBounds.Top);
    finally
      FreeAndNil(ATranslate);
    end;
  end;

var
  I: Integer;

begin
  BeginUpdate;
  FTransformMatrix.Reset;
  if FTransformations.Count > 0 then
    for I := 0 to FTransformations.Count - 1 do
      ApplyTransformation(TdxShapeCustomTransformation(FTransformations.Items[I]))
  else
    DoTranslateTransformation;
  CancelUpdate;
end;

procedure TdxCustomVectorShape.DoApplyTransformation(AType: TdxShapeTransformationType; M11, M12, M21, M22, DX, DY: Single);
var
  AMatrix: TdxGPMatrix;
begin
  AMatrix := TdxGPMatrix.Create;
  case AType of
    dxsttSkew:
      AMatrix.Shear(DX, DY);
    dxsttRotate:
      AMatrix.Rotate(M11, dxPointF(DX, DY));
    dxsttTranslate:
        AMatrix.Translate(DX, DY);
    else
      AMatrix.SetElements(M11, M12, M21, M22, DX, DY);
  end;
  if not AMatrix.IsIdentity then
    FTransformMatrix.Multiply(AMatrix, MatrixOrderAppend);
  FreeAndNil(AMatrix);
end;

{ TdxShapeRectangle }

constructor TdxShapeRectangle.Create;
begin
  inherited Create;
  FRadius := dxPointF(cxNullPoint);
end;

class function TdxShapeRectangle.GetName: string;
begin
  Result := 'Rectangle';
end;

procedure TdxShapeRectangle.CreatePathContent(var APath: TdxGPPath);
begin
  inherited;
  APath.FigureStart;
  APath.AddRoundRect(FBounds, FRadius.X, FRadius.Y);
  APath.FigureFinish;
end;

procedure TdxShapeRectangle.SetRadiusX(const AValue: Single);
begin
  if FRadius.X <> AValue then
  begin
    FRadius.X := AValue;
    Changed;
  end;
end;

procedure TdxShapeRectangle.SetRadiusY(const AValue: Single);
begin
  if FRadius.Y <> AValue then
  begin
    FRadius.Y := AValue;
    Changed;
  end;
end;

{ TdxShapeEllipse }

class function TdxShapeEllipse.GetName: string;
begin
  Result := 'Ellipse';
end;

procedure TdxShapeEllipse.CreatePathContent(var APath: TdxGPPath);
begin
  inherited;
  APath.FigureStart;
  APath.AddEllipse(FBounds);
  APath.FigureFinish;
end;

{ TdxShapePath }

class function TdxShapePath.GetName: string;
begin
  Result := 'Path';
end;

function TdxShapePath.GetContentBounds: TdxRectF;
begin
  Result := cxRectF(FTopLeft, cxPointOffset(FTopLeft, dxPointF(Width, Height)));
end;

procedure TdxShapePath.Calculate;
begin
  inherited;
  CalculatePoints;
end;

procedure TdxShapePath.CreatePathContent(var APath: TdxGPPath);
begin
  APath := TdxGPPath.Create(FNormalizedPoints, FPointTypes);
end;

procedure TdxShapePath.SetData(const AValue: AnsiString);
begin
  if not SameText(dxAnsiStringToString(FData), dxAnsiStringToString(AValue)) then
  begin
    FData := AValue;
    Changed;
  end;
end;

function TdxShapePath.GetPathBounds: TdxRectF;
var
  APath: TdxGPPath;
begin
  Result := FBounds;
  if Length(FPoints) > 0 then
  begin
    APath := TdxGPPath.Create(FPoints, FPointTypes);
    APath.Flatten(nil, 0.15);
    Result := APath.GetBoundsF;
    FreeAndNil(APath);
  end;
end;

procedure TdxShapePath.CalculatePoints;
var
  I: Integer;
  ADataParser: TdxShapePathDataParser;
  ABounds: TdxRectF;
begin
  ADataParser := TdxShapePathDataParser.Create;
  try
    SetLength(FPoints, 0);
    SetLength(FPointTypes, 0);
    ADataParser.Parse(FData, FPoints, FPointTypes);
    ABounds := GetPathBounds;
    SetLength(FNormalizedPoints, Length(FPoints));
    for I := 0 to Length(FPoints) - 1 do
    begin
      FNormalizedPoints[I].X := FPoints[I].X - ABounds.Left;
      FNormalizedPoints[I].Y := FPoints[I].Y - ABounds.Top;
    end;
  finally
    FreeAndNil(ADataParser);
  end;
end;

{ TdxShapeCanvas }

constructor TdxShapeCanvas.Create;
begin
  inherited;
  FShapes := TdxShapeList.Create;
end;

destructor TdxShapeCanvas.Destroy;
begin
  FreeAndNil(FShapes);
  inherited Destroy;
end;

class function TdxShapeCanvas.GetName: string;
begin
  Result := 'Canvas';
end;

class function TdxShapeCanvas.GetReaderClass: TdxShapeObjectReaderClass;
begin
  Result := TdxShapeCanvasReader;
end;

function TdxShapeCanvas.GetContentBounds: TdxRectF;
var
  I: Integer;
  AContentRect: TdxRectF;
begin
  Result := dxRectF(cxNullRect);
  for I := 0 to FShapes.Count - 1 do
  begin
    AContentRect := (FShapes[I] as TdxCustomVectorShape).GetContentBounds;
    if cxRectIsEmpty(Result) then
      Result := AContentRect
    else
      Result := cxRectUnion(Result, AContentRect);
  end;
end;

procedure TdxShapeCanvas.Add(AShape: TdxCustomVectorShape);
begin
  if not FIsRoot then
  begin
    AShape.BeginUpdate;
    AShape.Left := AShape.Left + FBounds.Left;
    AShape.Top := AShape.Top + FBounds.Top;
    AShape.EndUpdate;
  end;
  AShape.Reference;
  FShapes.Add(AShape);
end;

procedure TdxShapeCanvas.Calculate;
var
  I: Integer;
begin
  inherited;
  for I := 0 to FShapes.Count - 1 do
    Shapes[I].Calculate;
end;

procedure TdxShapeCanvas.Draw(AGraphics: TdxGPGraphics);
var
  I: Integer;
begin
  for I := 0 to FShapes.Count - 1 do
    Shapes[I].Draw(AGraphics);
end;

procedure TdxShapeCanvas.SetShapeColor(AAlphaColor: TdxAlphaColor; const AShapeName: string);
var
  I: Integer;
begin
  for I := 0 to FShapes.Count - 1 do
    if Shapes[I] is TdxShapeCanvas then
      TdxShapeCanvas(Shapes[I]).SetShapeColor(AAlphaColor, AShapeName)
    else
      if SameText(Shapes[I].Name, AShapeName) then
        Shapes[I].SetColor(AAlphaColor);
end;

function TdxShapeCanvas.GetCount: Integer;
begin
  Result := FShapes.Count;
end;

function TdxShapeCanvas.GetShape(AIndex: Integer): TdxCustomVectorShape;
begin
  if (AIndex >= 0) and (AIndex < FShapes.Count) then
    Result := FShapes[AIndex] as TdxCustomVectorShape
  else
    Result := nil;
end;

{ TdxShapeViewBox }

class function TdxShapeViewBox.GetName: string;
begin
  Result := 'Viewbox';
end;

{ TdxShapeFakeBrush }

class function TdxShapeFakeBrush.GetName: string;
begin
  Result := '.Fill';
end;

class function TdxShapeFakeBrush.GetReaderClass: TdxShapeObjectReaderClass;
begin
  Result := TdxShapeFakeBrushReader;
end;

{ TdxShapeFakeGradientStops }

class function TdxShapeFakeGradientStops.GetName: string;
begin
  Result := '.GradientStops';
end;

class function TdxShapeFakeGradientStops.GetReaderClass: TdxShapeObjectReaderClass;
begin
  Result := TdxShapeFakeGradientStopsReader;
end;

{ TdxShapeFakeCustomTransformation }

class function TdxShapeFakeCustomTransformation.GetReaderClass: TdxShapeObjectReaderClass;
begin
  Result := TdxShapeTransformationReader;
end;

{ TdxShapeFakeRenderTransformation }

class function TdxShapeFakeRenderTransformation.GetName: string;
begin
  Result := '.RenderTransform';
end;

{ TdxShapeFakeRelativeTransformation }

class function TdxShapeFakeRelativeTransformation.GetName: string;
begin
  Result := '.RelativeTransformation';
end;

{ TdxShapeObjectReader }

procedure TdxShapeObjectReader.ReadFromNode(ABaseObject: TdxShapeObject; ANode: TdxXMLNode);
begin
  ReadAttributes(ABaseObject, ANode);
  ReadChildNodes(ABaseObject, ANode);
end;

function TdxShapeObjectReader.CreateShapeFromNode(AParentShape: TdxShapeObject; ANode: TdxXMLNode): TdxShapeObject;
var
  AReader: TdxShapeObjectReader;
begin
  Result := dxShapesFactory.GetShapeClass(ANode.Name).Create(AParentShape);
  AReader := Result.GetReaderClass.Create;
  AReader.ReadFromNode(Result, ANode);
  FreeAndNil(AReader);
end;

procedure TdxShapeObjectReader.ReadAttributes(ABaseShape: TdxShapeObject; ANode: TdxXMLNode);
var
  I: Integer;
  AShapePropList: PPropList;
  AShapePropCount: Integer;
  AAttribute: TdxXMLNodeAttribute;
begin
  AShapePropCount := GetPropList(ABaseShape.ClassInfo, AShapePropList);
  if AShapePropCount > 0 then
  try
    ANode.Attributes.First;
    AAttribute := ANode.Attributes.First;
    for I := 0 to ANode.Attributes.Count - 1 do
    begin
      ReadAttribute(ABaseShape, AShapePropList, AShapePropCount, AAttribute);
      AAttribute := TdxXMLNodeAttribute(AAttribute.Next);
    end;
  finally
    FreeMem(AShapePropList)
  end;
end;

procedure TdxShapeObjectReader.ReadChildNodes(AParentShape: TdxShapeObject; ANode: TdxXMLNode);
begin
  if ANode <> nil then
    ANode.ForEach(
      procedure (ANode: TdxXMLNode; AUserData: Pointer)
      var
        AReader: TdxShapeObjectReader;
        AShape: TdxShapeObject;
      begin
        AShape := dxShapesFactory.GetShapeClass(ANode.Name).Create(AParentShape);
        try
          AReader := AShape.GetReaderClass.Create;
          try
            AReader.ReadFromNode(AShape, ANode);
          finally
            FreeAndNil(AReader);
           end;
        finally
          FreeAndNil(AShape);
        end;
      end);
end;

function TdxShapeObjectReader.GetShapePropertyName(const AAttributeName: string): string;
var
  AIndex: Integer;
begin
  AIndex := PosEx('.', AAttributeName);
  if AIndex = 0 then
    Result := AAttributeName
  else
    Result := Copy(AAttributeName, AIndex + 1, Length(AAttributeName) - AIndex + 1);
end;

procedure TdxShapeObjectReader.ReadAttribute(ABaseShape: TdxShapeObject; APropList: PPropList; APropCount: Integer; AAttribute: TdxXMLNodeAttribute);

  function IsSolidBrushProperty(const APropertyName: string): Boolean;
  begin
    Result := SameText(APropertyName, TdxShapeSolidBrush.GetName);
  end;

  procedure SetSolidBrush(const AColor: string);
  var
    ASolidBrush: TdxShapeSolidBrush;
  begin
    ASolidBrush := TdxShapeSolidBrush.Create;
    ASolidBrush.Color := dxColorNameToAlphaColor(AColor);
    TdxCustomVectorShape(ABaseShape).Brush := ASolidBrush;
  end;

  function GetPropertyIndex(const APropName: string): Integer;
  var
    I: Integer;
  begin
    Result := -1;
    for I := 0 to APropCount - 1 do
      if SameText(dxShortStringToString(APropList[I].Name), APropName) then
      begin
        Result := I;
        Break;
      end;
  end;

var
  AIndex: Integer;
  APropName: string;
begin
  ABaseShape.BeginUpdate;
  APropName := GetShapePropertyName(dxXMLStringToString(AAttribute.Name));
  if IsSolidBrushProperty(APropName) then
    SetSolidBrush(AAttribute.ValueAsString)
  else
  begin
    AIndex := GetPropertyIndex(APropName);
    if AIndex <> -1 then
      case APropList[AIndex].PropType^.Kind of
        tkInteger, tkFloat:
          SetFloatProp(ABaseShape, APropName, AAttribute.ValueAsFloat);
        tkUString, tkString, tkLString:
          SetStrProp(ABaseShape, APropName, AAttribute.ValueAsString);
      end;
  end;
  ABaseShape.CancelUpdate;
end;

{ TdxShapeTransformations }

constructor TdxShapeTransformations.Create;
begin
  FItems := TObjectList.Create(True);
end;

destructor TdxShapeTransformations.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TdxShapeTransformations.Assign(ASource: TPersistent);
begin
  inherited;
  if ASource is TdxShapeTransformations then
  begin
    FItems.Assign(TdxShapeTransformations(ASource).FItems);
    Changed;
  end;
end;

procedure TdxShapeTransformations.AddTransformation(ATransformation: TdxShapeObject);
begin
  FItems.Add(ATransformation);
  Changed;
end;

function TdxShapeTransformations.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TdxShapeTransformations.GetTransformation(AIndex: Integer): TdxShapeCustomTransformation;
begin
  if (AIndex >= 0) and (AIndex < Count) then
    Result := FItems[AIndex] as TdxShapeCustomTransformation
  else
    Result := nil;
end;

procedure TdxShapeTransformations.Changed;
begin
  dxCallNotify(OnChange, Self);
end;

{ TdxShapeCustomBrush }

constructor TdxShapeCustomBrush.Create;
begin
  inherited;
  FGPBrush := GetBrushClass.Create;
  FGPBrush.OnChange := BrushChangeHandler;
end;

destructor TdxShapeCustomBrush.Destroy;
begin
  FreeAndNil(FGPBrush);
  inherited;
end;

function TdxShapeCustomBrush.GetBrushClass: TdxShapeBrushClass;
begin
  Result := TdxGPSolidBrush;
end;

procedure TdxShapeCustomBrush.BrushChangeHandler(ASender: TObject);
begin
  Changed;
end;

{ TdxShapePathDataParser }

procedure TdxShapePathDataParser.Parse(const AData: AnsiString; var APoints: TdxGpPointFDynArray; var APointTypes: TBytes);
type
  TdxShapePathPoint = record
    X: Single;
    Y: Single;
    PointType: TdxShapePathPointType;
  end;

  procedure AddPoint(APoint: TdxShapePathPoint);
  var
    L: Integer;
  begin
    L := Length(APoints);
    SetLength(APoints, L + 1);
    SetLength(APointTypes, L + 1);
    APoints[L].X := APoint.X;
    APoints[L].Y := APoint.Y;
    APointTypes[L] := Byte(APoint.PointType);
  end;

var
  ACommand: Char;
  APoint: TdxShapePathPoint;
begin
  if AData <> '' then
  begin
    FData := AnsiLowerCase(dxAnsiStringToString(AData));
    FDataSize := Length(FData);
    FPosition := PosEx('m', FData);
    while FPosition <= FDataSize do
    begin
      APoint.X := GetCoordinate(ACommand);
      if ACommand <> 'z' then
      begin
        APoint.Y := GetCoordinate(ACommand);
        APoint.PointType := GetPointType(ACommand);
        AddPoint(APoint);
      end
      else
        APointTypes[Length(APointTypes) - 1] := Byte(APointTypes[Length(APointTypes) - 1]) or Byte(dxpsptCloseSubpath);
    end;
  end;
end;

function TdxShapePathDataParser.GetCoordinate(var ACommand: Char): Single;
begin
  Result := 0.0;
  FPosition := GetNextPosition;
  if (FPosition <= FDataSize) and not IsNumberChar(FData[FPosition]) then
  begin
    ACommand := FData[FPosition];
    if FPosition <= FDataSize then
      Inc(FPosition);
  end;
  FPosition := GetNextPosition;
  if (FPosition <= FDataSize) and IsNumberChar(FData[FPosition]) then
    Result := dxStrToFloat(GetDigits);
end;

function TdxShapePathDataParser.GetDigits: string;
var
  AIndex: Integer;
begin
  AIndex := FPosition;
  while (AIndex <= FDataSize) and IsNumberChar(FData[AIndex]) do
    Inc(AIndex);
  Result := Copy(FData, FPosition, AIndex - FPosition);
  Inc(FPosition, Length(Result));
end;

function TdxShapePathDataParser.GetNextPosition: Integer;
begin
  Result := FPosition;
  while (Result <= FDataSize) and ((FData[Result] = ' ') or (FData[Result] = ',') or (FData[Result] = '\r') or (FData[Result] = '\n') or (FData[Result] = '\t')) do
    Inc(Result);
  if (Result <= FDataSize) and (FData[Result] = '&') then
    while (Result <= FDataSize) and (FData[Result] <> ';') do
      Inc(Result);
end;

function TdxShapePathDataParser.GetPointType(ACommand: Char): TdxShapePathPointType;
begin
  case ACommand of
    'c':
      FLastPointType := dxpsptBezier;
    'l':
      FLastPointType := dxpsptLine;
    'z':
      FLastPointType := dxpsptCloseSubpath;
  end;
  if ACommand <> 'm' then
    Result := FLastPointType
  else
    Result := dxpsptStart;
end;

function TdxShapePathDataParser.IsNumberChar(AChar: Char): Boolean;
begin
  Result := dxCharInSet(AChar, ['0'..'9']) or (AChar = '.') or (AChar = '-') or (AChar = '+') or (AChar = 'e');
end;

{ TdxShapesFactory }

function dxShapesFactory: TdxShapesFactory;
begin
  if AShapesFactory = nil then
    AShapesFactory := TdxShapesFactory.Create;
  Result := AShapesFactory;
end;

constructor TdxShapesFactory.Create;
begin
  inherited;
  FClasses := TStringList.Create;
end;

destructor TdxShapesFactory.Destroy;
begin
  FreeAndNil(FClasses);
  inherited;
end;

function TdxShapesFactory.GetShapeClass(AIndex: Integer): TdxShapeObjectClass;
begin
  Result := TdxShapeObjectClass(FClasses.Objects[AIndex]);
end;

function TdxShapesFactory.GetShapeClass(const AName: TdxXMLString): TdxShapeObjectClass;

  function GetName(const S: string): string;
  var
    APosition: Integer;
  begin
    APosition := PosEx('.', S);
    if APosition > 0 then
      Result := Copy(S, APosition, Length(S) - APosition + 1)
    else
      Result := S;
  end;

var
  AIndex: Integer;
begin
  if FClasses.Find(GetName(dxXMLStringToString(AName)), AIndex) then
    Result := TdxShapeObjectClass(FClasses.Objects[AIndex])
  else
    Result := TdxShapeObject;
end;

procedure TdxShapesFactory.RegisterShape(AShapeClass: TdxShapeObjectClass);
begin
  if FClasses.IndexOf(AShapeClass.GetName) = -1 then
    FClasses.AddObject(AShapeClass.GetName, TObject(AShapeClass));
  FClasses.Sort;
end;

procedure TdxShapesFactory.UnregisterShape(AShapeClass: TdxShapeObjectClass);
var
  AIndex: Integer;
begin
  if FClasses.Find(AShapeClass.GetName, AIndex) then
    FClasses.Delete(AIndex);
end;

function TdxShapesFactory.GetCount: Integer;
begin
  Result := FClasses.Count;
end;

initialization
  dxShapesFactory.RegisterShape(TdxShapeCanvas);
  dxShapesFactory.RegisterShape(TdxShapeViewBox);
  dxShapesFactory.RegisterShape(TdxShapeEllipse);
  dxShapesFactory.RegisterShape(TdxShapePath);
  dxShapesFactory.RegisterShape(TdxShapeRectangle);

  dxShapesFactory.RegisterShape(TdxShapeFakeBrush);
  dxShapesFactory.RegisterShape(TdxShapeFakeGradientStops);
  dxShapesFactory.RegisterShape(TdxShapeSolidBrush);
  dxShapesFactory.RegisterShape(TdxShapeLinearGradientBrush);
  dxShapesFactory.RegisterShape(TdxShapeRadialGradientBrush);
  dxShapesFactory.RegisterShape(TdxShapeBrushGradientStop);

  dxShapesFactory.RegisterShape(TdxShapeFakeRenderTransformation);
  dxShapesFactory.RegisterShape(TdxShapeFakeRelativeTransformation);
  dxShapesFactory.RegisterShape(TdxShapeScaleTransformation);
  dxShapesFactory.RegisterShape(TdxShapeSkewTransformation);
  dxShapesFactory.RegisterShape(TdxShapeRotateTransformation);
  dxShapesFactory.RegisterShape(TdxShapeTranslateTransformation);

finalization
  dxShapesFactory.UnregisterShape(TdxShapeCanvas);
  dxShapesFactory.UnregisterShape(TdxShapeViewBox);
  dxShapesFactory.UnregisterShape(TdxShapeEllipse);
  dxShapesFactory.UnregisterShape(TdxShapePath);
  dxShapesFactory.UnregisterShape(TdxShapeRectangle);

  dxShapesFactory.UnregisterShape(TdxShapeFakeBrush);
  dxShapesFactory.UnregisterShape(TdxShapeFakeGradientStops);
  dxShapesFactory.UnregisterShape(TdxShapeSolidBrush);
  dxShapesFactory.UnregisterShape(TdxShapeLinearGradientBrush);
  dxShapesFactory.UnregisterShape(TdxShapeRadialGradientBrush);
  dxShapesFactory.UnregisterShape(TdxShapeBrushGradientStop);

  dxShapesFactory.UnregisterShape(TdxShapeFakeRenderTransformation);
  dxShapesFactory.UnregisterShape(TdxShapeFakeRelativeTransformation);
  dxShapesFactory.UnregisterShape(TdxShapeScaleTransformation);
  dxShapesFactory.UnregisterShape(TdxShapeSkewTransformation);
  dxShapesFactory.UnregisterShape(TdxShapeRotateTransformation);
  dxShapesFactory.UnregisterShape(TdxShapeTranslateTransformation);

  FreeAndNil(AShapesFactory);

end.

