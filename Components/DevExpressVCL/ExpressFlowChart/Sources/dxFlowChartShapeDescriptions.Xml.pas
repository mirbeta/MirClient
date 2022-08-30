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

unit dxFlowChartShapeDescriptions.Xml;

{$IFDEF WIN32}
  {$HPPEMIT '#pragma link "dxFlowChartShapeDescriptions.Xml.obj"'}
{$ELSE}
  {$HPPEMIT '#pragma link "dxFlowChartShapeDescriptions.Xml.o"'}
{$ENDIF}

interface

{$I cxVer.inc}
{$SCOPEDENUMS ON}

uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, dxCoreGraphics, dxGDIPlusAPI, dxGDIPlusClasses, dxXMLDoc,
  cxGeometry, dxEvaluator, dxFlowChartShapes;

type
  EdxFlowChartShapeParserError = class(EdxException);

  TdxDoubleEvaluator = class;

  { TdxEvaluatorContext }

  TdxEvaluatorContext = class
  public type
    TParameters = array[0..9] of Variant;
  strict private
    FWidth: Variant;
    FHeight: Variant;
    FParameters: TParameters;
    FP: Variant;
    FPX: Variant;
    FPY: Variant;
  public
    constructor Create(const ASize: TdxSizeF; const AParameters: array of Single); overload;
    constructor Create(const ASize: TdxSizeF); overload;
    constructor Create(const ASize: TdxSizeF; const AParameter: Single); overload;
    constructor Create(const ASize: TdxSizeF; const APoint: TdxPointF); overload;

    property Width: Variant read FWidth;
    property Height: Variant read FHeight;
    property Parameters: TParameters read FParameters;

    property P: Variant read FP;
    property PX: Variant read FPX;
    property PY: Variant read FPY;
  end;

  { TdxDoubleEvaluator }

  TdxDoubleEvaluator = class(TdxMathExpression)
  strict private
    FContext: TdxEvaluatorContext;
    function P0H(AParams: TdxExpressionElements): Variant;
    function P0W(AParams: TdxExpressionElements): Variant;
    function P1W(AParams: TdxExpressionElements): Variant;
    function P1H(AParams: TdxExpressionElements): Variant;
    function P2W(AParams: TdxExpressionElements): Variant;
    function P2H(AParams: TdxExpressionElements): Variant;
    function P3W(AParams: TdxExpressionElements): Variant;
    function P3H(AParams: TdxExpressionElements): Variant;
    function P4W(AParams: TdxExpressionElements): Variant;
    function P4H(AParams: TdxExpressionElements): Variant;
    function P5W(AParams: TdxExpressionElements): Variant;
    function P5H(AParams: TdxExpressionElements): Variant;
    function P6W(AParams: TdxExpressionElements): Variant;
    function P6H(AParams: TdxExpressionElements): Variant;
    function P7W(AParams: TdxExpressionElements): Variant;
    function P7H(AParams: TdxExpressionElements): Variant;
    function P8W(AParams: TdxExpressionElements): Variant;
    function P8H(AParams: TdxExpressionElements): Variant;
    function P9W(AParams: TdxExpressionElements): Variant;
    function P9H(AParams: TdxExpressionElements): Variant;
  public
    constructor Create(const AExpression: string); reintroduce; virtual;
    procedure UpdateContext(AContext: TdxEvaluatorContext);
  end;

  { TdxCriteriaValue }

  TdxCriteriaValue = class
  strict private
    FEvaluator: TdxDoubleEvaluator;
  public
    constructor Create(const AExpression: string);
    destructor Destroy; override;
    class function Parse(const AExpression: string): TdxCriteriaValue;

    function EvaluateDouble(AContext: TdxEvaluatorContext): Single; virtual;
  end;

  { TdxShapeSegment }

  TdxShapeSegment = class
  strict private
    FX: TdxCriteriaValue;
    FY: TdxCriteriaValue;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AddToPath(APath: TdxGpPath); virtual; abstract;
    procedure Load(ANode: TdxXmlNode); virtual;

    property X: TdxCriteriaValue read FX;
    property Y: TdxCriteriaValue read FY;
  end;
  TdxShapeSegmentClass = class of TdxShapeSegment;

  { TUnit }

  TUnitType = (Star, CriteriaOperator);

  TUnit = class
  strict private
    FValue: TdxCriteriaValue;
    FUnitType: TUnitType;
  public
    constructor Create(AUnitType: TUnitType; AValue: TdxCriteriaValue);
    destructor Destroy; override;
    function ToString: string; override;
    class function Parse(const ASource: string): TUnit; static;

    property Value: TdxCriteriaValue read FValue write FValue;
    property UnitType: TUnitType read FUnitType write FUnitType;
  end;

  { TUnitCollection }

  TUnitCollection = class
  strict private class var
    FDefault: TUnitCollection;
    class constructor Initialize;
  {$HINTS OFF}
    class destructor Finalize;
  {$HINTS ON}
  strict private
    FUnits: TArray<TUnit>;
    constructor CreateDefault; overload;
  public
    constructor Create(const AUnits: TList<TUnit>); overload;
    destructor Destroy; override;
    function ToString: string; override;
    class function Parse(const ASource: string): TUnitCollection; static;

    class property Default: TUnitCollection read FDEfault;
    property Units: TArray<TUnit> read FUnits;
  end;

  { TdxShapeXmlDescription }

  TdxShapeXmlDescription = class(TdxShapeDescription)
  {$REGION 'internal types'}
  protected type

    TConnectionPoint = class
    strict private
      FRelative: Boolean;
      FX: TdxCriteriaValue;
      FY: TdxCriteriaValue;
    public
      constructor Create(ANode: TdxXmlNode);
      destructor Destroy; override;

      property Relative: Boolean read FRelative;
      property X: TdxCriteriaValue read FX;
      property Y: TdxCriteriaValue read FY;
    end;

    TConnectionPoints = class(TObjectList<TConnectionPoint>)
    public
      class function Load(ANode: TdxXMLNode): TConnectionPoints;
    end;

    TParameter = class
    strict private
      FDefaultValue: Single;
      FValue: TdxCriteriaValue;
      FPoint: TdxCriteriaValue;
      FMin: TdxCriteriaValue;
      FMax: TdxCriteriaValue;
    public
      constructor Create(ANode: TdxXMLNode);
      destructor Destroy; override;
      class function Evaluate(AContext: TdxEvaluatorContext; AOp: TdxCriteriaValue): Single; static;
      function GetValue(const ASize: TdxSizeF; const AParameters: TArray<Single>; const ALocalPoint: TdxPointF): Single;
      function GetPoint(const ASize: TdxSizeF; const AParameters: TArray<Single>; AValue: Single): TdxPointF;
      function Normalize(const ASize: TdxSizeF; const AParameters: TArray<Single>; AValue: Single): Single;

      property DefaultValue: Single read FDefaultValue write FDefaultValue;
      property Value: TdxCriteriaValue read FValue write FValue;
      property Point: TdxCriteriaValue read FPoint write FPoint;
      property Min: TdxCriteriaValue read FMin write FMin;
      property Max: TdxCriteriaValue read FMax write FMax;
    end;

    TParameters = class(TObjectList<TParameter>)
    public
      class function Load(ANode: TdxXMLNode): TParameters;
    end;
  {$ENDREGION}
  strict private
    FCaption: string;
    FColumns: TUnitCollection;
    FParameters: TParameters;
    FConnectionPoints: TConnectionPoints;
    FRows: TUnitCollection;
    FSegments: TObjectList<TdxShapeSegment>;
    function GetCaption: string;
    function GetDefaultSize(AAttributes: TdxXMLNodeAttributes): TdxSizeF;
    procedure ParseParameters(ANode: TdxXMLNode);
    procedure ParseConnectionPoints(ANode: TdxXMLNode);
  strict protected
    procedure ParseSegment(ANode: TdxXmlNode; ASegmentClass: TdxShapeSegmentClass);
    function ParseTemplateNode(ANode: TdxXMLNode): Boolean; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function CreateViewData(const ASize: TdxSizeF; const AParameters: TArray<Single>): TdxShapeViewData; override;
    function GetConnectionPoints(const ASize: TdxSizeF; const AParameters: TArray<Single>): TArray<TdxPointF>; override;
    function GetNormalizedParameters(const ASize: TdxSizeF; const AParameters: TArray<Single>): TArray<Single>; override;
    function GetParameterDefaultValue(AParameterIndex: Integer): Single; override;
    function GetParameterPoint(AParameterIndex: Integer; const ASize: TdxSizeF; const AParameters: TArray<Single>; AValue: Single): TdxPointF; override;
    function GetParameterValue(AParameterIndex: Integer; const ASize: TdxSizeF; const AParameters: TArray<Single>; const ALocalPoint: TdxPointF): Single; override;
    function GetParameters(const ASize: TdxSizeF; const AParameters: TArray<Single>): TArray<Single>; override;

    procedure Load(ANode: TdxXmlNode); virtual;

    property Caption: string read GetCaption;
    property Columns: TUnitCollection read FColumns;
    property ConnectionPoints: TConnectionPoints read FConnectionPoints;
    property Parameters: TParameters read FParameters;
    property Rows: TUnitCollection read FRows;
    property Segments: TObjectList<TdxShapeSegment> read FSegments;
  end;

  { TdxTemplateLoader }

  TdxTemplateLoader = class
  public type
    TAddShapeProcedure = reference to procedure (ATemplate: TdxShapeXmlDescription);
  strict private
    FAddShape: TAddShapeProcedure;
  strict protected
    procedure ParseShapeTemplate(ANode: TdxXmlNode);
    procedure ParseShapes(ANode: TdxXmlNode);
  public
    procedure LoadFromStream(AStream: TStream; const AAddShape: TAddShapeProcedure);
  end;

implementation

uses
  RTLConsts, Windows, Math, dxStringHelper, AnsiStrings, dxSVGCoreParsers, dxTypeHelpers, dxFcStrs, dxflchrt;

type
  TTemplateType = (Shape, Arrow);
  TGeometryKind = (Closed, Filled);
  TGeometryKinds = set of TGeometryKind;
  TOrientationKind = (Horizontal, Vertical);

  { TConstantValue }

  TConstantValue = class(TdxCriteriaValue)
  strict private
    FValue: Single;
  public
    constructor Create(AValue: Single);
    function EvaluateDouble(AContext: TdxEvaluatorContext): Single; override;
  end;

  { TMultipleOperandsValue }

  TMultipleOperandsValue = class(TdxCriteriaValue)
  strict private
    FValues: TArray<TdxCriteriaValue>;
  strict protected
    property Values: TArray<TdxCriteriaValue> read FValues;
  public
    constructor Create(AValueCount: Integer; const AExpression: string; const AProcedureName: string);
    destructor Destroy; override;
  end;

  { TSizeValue }

  TSizeValue = class(TMultipleOperandsValue)
  public
    constructor Create(const AExpression: string);
    function EvaluateSize(AContext: TdxEvaluatorContext): TdxSizeF;
  end;

  { TPointValue }

  TPointValue = class(TMultipleOperandsValue)
  public
    constructor Create(const AExpression: string);
    function EvaluatePoint(AContext: TdxEvaluatorContext): TdxPointF;
  end;

  { TRectValue }

  TRectValue = class(TMultipleOperandsValue)
  public
    constructor Create(const AExpression: string);
    function EvaluateRect(AContext: TdxEvaluatorContext): TdxRectDouble;
  end;

  { TAttributeReader }

  TAttributeReader = record
  public
    class function GetArrayOfDouble(AAttributes: TdxXMLNodeAttributes; const AArrayName: TdxXMLString; const ASeparator: string): TArray<Single>; static;
    class function GetNullableColor(AAttributes: TdxXMLNodeAttributes; const AColorName: TdxXMLString): TdxNullableValue<TdxAlphaColor>; static;
    class function GetNullableDouble(AAttributes: TdxXMLNodeAttributes; const ADoubleName: TdxXMLString): TdxNullableValue<Single>; static;
    class function GetKind(AAttributes: TdxXMLNodeAttributes): TGeometryKinds; static;
  end;

  { TShapeContext }

  TShapeContext = class(TdxEvaluatorContext)
  public
    function EvaluateDouble(AOp: TdxCriteriaValue): Single;
    function EvaluatePoint(AOp: TdxCriteriaValue): TdxPointF;
    function EvaluateRect(AOp: TdxCriteriaValue): TdxRectDouble;
    function EvaluateSize(AOp: TdxCriteriaValue): TdxSizeF;
  end;

 { TShapeLayoutCalculator }

  TShapeLayoutCalculator = class
  strict private
    FSize: TdxSizeF;
    FRows: TArray<Single>;
    FColumns: TArray<Single>;
    FContext: TShapeContext;
  public
    constructor Create(ARows: TUnitCollection; AColumns: TUnitCollection; const ASize: TdxSizeF; AContext: TShapeContext);
    function EvaluateDouble(AOp: TdxCriteriaValue): Single;
    function EvaluatePoint(AOp: TdxCriteriaValue): TdxPointF;
    function EvaluateRect(AOp: TdxCriteriaValue): TdxRectDouble;
    function EvaluateSize(AOp: TdxCriteriaValue): TdxSizeF;
    function GetConnectionPoint(APoint: TdxShapeXmlDescription.TConnectionPoint): TdxPointF;
    function GetPoint(AXOperator: TdxCriteriaValue; AYOperator: TdxCriteriaValue): TdxPointF;
    function GetUnitValue(AUnit: TUnit): Single;
    function ConvertUnits(const AUnits: TArray<TUnit>; ATotalLength: Single): TArray<Single>;
    function GetOffsetByFactor(AFactor: Single; AOrientation: TOrientationKind): Single;

    property Rows: TArray<Single> read FRows;
    property Columns: TArray<Single> read FColumns;
  end;

  { TShapeBuilder }

  TShapeBuilder = record
    class function CreateShape(ASegments: TObjectList<TdxShapeSegment>; AContext: TShapeLayoutCalculator): TdxShapeViewData; static;
  end;

  { TStart }

  TStart = class(TdxShapeSegment)
  strict private
    FKind: TGeometryKinds;
    FIsNewShape: Boolean;
    FProperties: TdxShapePathProperties;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Load(ANode: TdxXmlNode); override;

    property Kind: TGeometryKinds read FKind;
    property IsNewShape: Boolean read FIsNewShape;
    property Properties: TdxShapePathProperties read FProperties;
  end;

  { TLine }

  TLine = class(TdxShapeSegment);

  { TArc }

  TArc = class(TdxShapeSegment)
  strict private
    FDirection: TdxSweepDirectionKind;
    FSize: TdxCriteriaValue;
    class function GetDirection(AAttributes: TdxXMLNodeAttributes): TdxSweepDirectionKind; static;
  public
    destructor Destroy; override;
    procedure Load(ANode: TdxXmlNode); override;

    property Direction: TdxSweepDirectionKind read FDirection write FDirection;
    property Size: TdxCriteriaValue read FSize;
  end;

function MatchName(ANode: TdxXmlNode; const AName: TdxXMLString): Boolean;
begin
  Result := AnsiSameText(ANode.Name, AName);
end;

function MatchValue(const AVariable, AValue: TdxXMLString): Boolean;
begin
  Result := AnsiSameText(AVariable, AValue);
end;

{ TdxEvaluatorParameters }

constructor TdxEvaluatorContext.Create(const ASize: TdxSizeF);
begin
  FWidth := ASize.Width;
  FHeight := ASize.Height;
end;

constructor TdxEvaluatorContext.Create(const ASize: TdxSizeF; const AParameters: array of Single);
var
  I: Integer;
begin
  Create(ASize);
  for I := 0 to Min(10, Length(AParameters)) - 1 do
    FParameters[I] := AParameters[I];
end;

constructor TdxEvaluatorContext.Create(const ASize: TdxSizeF; const AParameter: Single);
begin
  Create(ASize);
  FP := AParameter;
end;

constructor TdxEvaluatorContext.Create(const ASize: TdxSizeF; const APoint: TdxPointF);
begin
  Create(ASize);
  FPX := APoint.X;
  FPY := APoint.Y;
end;

{ TdxDoubleEvaluator }

constructor TdxDoubleEvaluator.Create(const AExpression: string);
begin
  inherited Create;
  RegisterVariable('W', nil);
  RegisterVariable('H', nil);
  RegisterVariable('P0', nil);
  RegisterVariable('P1', nil);
  RegisterVariable('P2', nil);
  RegisterVariable('P3', nil);
  RegisterVariable('P4', nil);
  RegisterVariable('P5', nil);
  RegisterVariable('P6', nil);
  RegisterVariable('P7', nil);
  RegisterVariable('P8', nil);
  RegisterVariable('P9', nil);
  RegisterVariable('P.X', nil);
  RegisterVariable('P.Y', nil);
  RegisterVariable('P', nil);
  RegisterFunction('P0H', P0H);
  RegisterFunction('P0W', P0W);
  RegisterFunction('P1H', P1H);
  RegisterFunction('P1W', P1W);
  RegisterFunction('P2H', P2H);
  RegisterFunction('P2W', P2W);
  RegisterFunction('P3H', P3H);
  RegisterFunction('P3W', P3W);
  RegisterFunction('P4H', P4H);
  RegisterFunction('P4W', P4W);
  RegisterFunction('P5H', P5H);
  RegisterFunction('P5W', P5W);
  RegisterFunction('P6H', P6H);
  RegisterFunction('P6W', P6W);
  RegisterFunction('P7H', P7H);
  RegisterFunction('P7W', P7W);
  RegisterFunction('P8H', P8H);
  RegisterFunction('P8W', P8W);
  RegisterFunction('P9H', P9H);
  RegisterFunction('P9W', P9W);

  Compile(AExpression);
  Optimize;
end;

function TdxDoubleEvaluator.P0H(AParams: TdxExpressionElements): Variant;
begin
  Result := FContext.Parameters[0] * FContext.Height;
end;

function TdxDoubleEvaluator.P0W(AParams: TdxExpressionElements): Variant;
begin
  Result := FContext.Parameters[0] * FContext.Width;
end;

function TdxDoubleEvaluator.P1H(AParams: TdxExpressionElements): Variant;
begin
  Result := FContext.Parameters[1] * FContext.Height;
end;

function TdxDoubleEvaluator.P1W(AParams: TdxExpressionElements): Variant;
begin
  Result := FContext.Parameters[1] * FContext.Width;
end;

function TdxDoubleEvaluator.P2H(AParams: TdxExpressionElements): Variant;
begin
  Result := FContext.Parameters[2] * FContext.Height;
end;

function TdxDoubleEvaluator.P2W(AParams: TdxExpressionElements): Variant;
begin
  Result := FContext.Parameters[2] * FContext.Width;
end;

function TdxDoubleEvaluator.P3H(AParams: TdxExpressionElements): Variant;
begin
  Result := FContext.Parameters[3] * FContext.Height;
end;

function TdxDoubleEvaluator.P3W(AParams: TdxExpressionElements): Variant;
begin
  Result := FContext.Parameters[3] * FContext.Width;
end;

function TdxDoubleEvaluator.P4H(AParams: TdxExpressionElements): Variant;
begin
  Result := FContext.Parameters[4] * FContext.Height;
end;

function TdxDoubleEvaluator.P4W(AParams: TdxExpressionElements): Variant;
begin
  Result := FContext.Parameters[4] * FContext.Width;
end;

function TdxDoubleEvaluator.P5H(AParams: TdxExpressionElements): Variant;
begin
  Result := FContext.Parameters[5] * FContext.Height;
end;

function TdxDoubleEvaluator.P5W(AParams: TdxExpressionElements): Variant;
begin
  Result := FContext.Parameters[5] * FContext.Width;
end;

function TdxDoubleEvaluator.P6H(AParams: TdxExpressionElements): Variant;
begin
  Result := FContext.Parameters[6] * FContext.Height;
end;

function TdxDoubleEvaluator.P6W(AParams: TdxExpressionElements): Variant;
begin
  Result := FContext.Parameters[6] * FContext.Width;
end;

function TdxDoubleEvaluator.P7H(AParams: TdxExpressionElements): Variant;
begin
  Result := FContext.Parameters[7] * FContext.Height;
end;

function TdxDoubleEvaluator.P7W(AParams: TdxExpressionElements): Variant;
begin
  Result := FContext.Parameters[7] * FContext.Width;
end;

function TdxDoubleEvaluator.P8H(AParams: TdxExpressionElements): Variant;
begin
  Result := FContext.Parameters[8] * FContext.Height;
end;

function TdxDoubleEvaluator.P8W(AParams: TdxExpressionElements): Variant;
begin
  Result := FContext.Parameters[8] * FContext.Width;
end;

function TdxDoubleEvaluator.P9H(AParams: TdxExpressionElements): Variant;
begin
  Result := FContext.Parameters[9] * FContext.Height;
end;

function TdxDoubleEvaluator.P9W(AParams: TdxExpressionElements): Variant;
begin
  Result := FContext.Parameters[9] * FContext.Width;
end;

procedure TdxDoubleEvaluator.UpdateContext(AContext: TdxEvaluatorContext);
begin
  FContext := AContext;
  RegisterVariable('W', @AContext.Width);
  RegisterVariable('H', @AContext.Height);
  RegisterVariable('P0', @AContext.Parameters[0]);
  RegisterVariable('P1', @AContext.Parameters[1]);
  RegisterVariable('P2', @AContext.Parameters[2]);
  RegisterVariable('P3', @AContext.Parameters[3]);
  RegisterVariable('P4', @AContext.Parameters[4]);
  RegisterVariable('P5', @AContext.Parameters[5]);
  RegisterVariable('P6', @AContext.Parameters[6]);
  RegisterVariable('P7', @AContext.Parameters[7]);
  RegisterVariable('P8', @AContext.Parameters[8]);
  RegisterVariable('P9', @AContext.Parameters[9]);
  RegisterVariable('P.X', @AContext.PX);
  RegisterVariable('P.Y', @AContext.PY);
  RegisterVariable('P', @AContext.P);
end;

{ TdxCriteriaValue }

constructor TdxCriteriaValue.Create(const AExpression: string);
begin
  FEvaluator := TdxDoubleEvaluator.Create(AExpression);
end;

destructor TdxCriteriaValue.Destroy;
begin
  FEvaluator.Free;
  inherited Destroy;
end;

function TdxCriteriaValue.EvaluateDouble(AContext: TdxEvaluatorContext): Single;
begin
  FEvaluator.UpdateContext(AContext);
  Result := FEvaluator.Evaluate;
end;

class function TdxCriteriaValue.Parse(const AExpression: string): TdxCriteriaValue;
var
  ADouble: Single;
begin
  if AExpression <> '' then
  begin
    if TryStrToFloat(AExpression, ADouble) then
      Result := TConstantValue.Create(ADouble)
    else
      if TdxStringHelper.StartsWith(AExpression, 'CreateSize(')  then
        Result := TSizeValue.Create(AExpression)
      else if TdxStringHelper.StartsWith(AExpression, 'CreatePoint(')  then
        Result := TPointValue.Create(AExpression)
      else if TdxStringHelper.StartsWith(AExpression, 'CreateRect(')  then
        Result := TRectValue.Create(AExpression)
      else
        Result := TdxCriteriaValue.Create(AExpression);
  end
  else
    Result := nil;
end;

{ TConstantValue }

constructor TConstantValue.Create(AValue: Single);
begin
  FValue := AValue;
end;

function TConstantValue.EvaluateDouble(AContext: TdxEvaluatorContext): Single;
begin
  Result := FValue;
end;

{ TMultipleOperandsValue }

constructor TMultipleOperandsValue.Create(AValueCount: Integer; const AExpression: string; const AProcedureName: string);
var
  AExpressions: TArray<string>;
  I, AStripLength: Integer;
begin
  AStripLength := Length(AProcedureName) + 2;
  AExpressions :=  TdxStringHelper.Split(Copy(AExpression, AStripLength, Length(AExpression) - AStripLength), TArray<string>.Create(','));
  if Length(AExpressions) <> AValueCount then
    Abort;
  SetLength(FValues, AValueCount);
  for I := Low(AExpressions) to High(AExpressions) do
    FValues[I] := TdxCriteriaValue.Parse(AExpressions[I]);
end;

destructor TMultipleOperandsValue.Destroy;
var
  I: Integer;
begin
  for I := Low(FValues) to High(FValues) do
    FValues[I].Free;
  SetLength(FValues, 0);
  inherited Destroy;
end;

{ TSizeValue }

constructor TSizeValue.Create(const AExpression: string);
begin
  inherited Create(2, AExpression, 'CreateSize');
end;

function TSizeValue.EvaluateSize(AContext: TdxEvaluatorContext): TdxSizeF;
begin
  Result := TdxSizeF.Create(Values[0].EvaluateDouble(AContext), Values[1].EvaluateDouble(AContext));
end;

{ TPointValue }

constructor TPointValue.Create(const AExpression: string);
begin
  inherited Create(2, AExpression, 'CreatePoint');
end;

function TPointValue.EvaluatePoint(AContext: TdxEvaluatorContext): TdxPointF;
begin
  Result := TdxPointF.Create(Values[0].EvaluateDouble(AContext), Values[1].EvaluateDouble(AContext));
end;

{ TRectValue }

constructor TRectValue.Create(const AExpression: string);
begin
  inherited Create(4, AExpression, 'CreateRect');
end;

function TRectValue.EvaluateRect(AContext: TdxEvaluatorContext): TdxRectDouble;
begin
  Result := TdxRectDouble.Create(
    Values[0].EvaluateDouble(AContext),
    Values[1].EvaluateDouble(AContext),
    Values[0].EvaluateDouble(AContext),
    Values[1].EvaluateDouble(AContext)
  );
end;

{ TAttributeReader }

class function TAttributeReader.GetArrayOfDouble(AAttributes: TdxXMLNodeAttributes;
  const AArrayName: TdxXMLString; const ASeparator: string): TArray<Single>;
var
  AValues: TArray<string>;
  I, AIndex, ALength: Integer;
  AValue: Single;
begin
  AValues := TdxStringHelper.Split(AAttributes.GetValueAsString(AArrayName), TArray<string>.Create(ASeparator));
  ALength := Length(AValues);
  if ALength = 0 then
    Exit(nil);
  SetLength(Result, ALength);
  AIndex := 0;
  for I := Low(AValues) to High(AValues) do
    if TryStrToFloat(Trim(AValues[I]), AValue) then
    begin
      Result[AIndex] := AValue;
      Inc(AIndex);
    end;
  if AIndex <> ALength then
    SetLength(Result, AIndex);
end;

class function TAttributeReader.GetKind(AAttributes: TdxXMLNodeAttributes): TGeometryKinds;
var
  AValue: TdxXMLString;
begin
  AValue := AAttributes.GetValue('Kind');
  if MatchValue(AValue, 'None') then
    Result := []
  else if MatchValue(AValue, 'Closed') then
    Result := [TGeometryKind.Closed]
  else if MatchValue(AValue, 'Filled') then
    Result := [TGeometryKind.Filled]
  else
    Result := [TGeometryKind.Closed, TGeometryKind.Filled];
end;

class function TAttributeReader.GetNullableColor(AAttributes: TdxXMLNodeAttributes; const AColorName: TdxXMLString): TdxNullableValue<TdxAlphaColor>;
var
  AValue: string;
  AColor: TdxAlphaColor;
begin
  Result.Reset;
  AValue := AAttributes.GetValueAsString(AColorName);
  if AValue <> '' then
  begin
    AColor := TdxAlphaColors.FromHtml(AValue);
    if not TdxAlphaColors.IsEmpty(AColor) then
      Result := AColor;
  end;
end;

class function TAttributeReader.GetNullableDouble(AAttributes: TdxXMLNodeAttributes;
  const ADoubleName: TdxXMLString): TdxNullableValue<Single>;
var
  AValue: string;
  ADouble: Single;
begin
  Result.Reset;
  AValue := AAttributes.GetValueAsString(ADoubleName);
  if AValue <> '' then
  begin
    if TryStrToFloat(AValue, ADouble) then
      Result := ADouble;
  end;
end;

{ TUnit }

constructor TUnit.Create(AUnitType: TUnitType; AValue: TdxCriteriaValue);
begin
  FUnitType := AUnitType;
  FValue := AValue;
end;

destructor TUnit.Destroy;
begin
  FValue.Free;
  inherited Destroy;
end;

class function TUnit.Parse(const ASource: string): TUnit;
var
  AUnitType: TUnitType;
  AValue: TdxCriteriaValue;
  ANumberString: string;
begin
  if TdxStringHelper.EndsWith(ASource, '*') then
  begin
    AUnitType := TUnitType.Star;
    ANumberString := TdxStringHelper.Substring(ASource, 0, Length(ASource) - 1);
    if ANumberString = '' then
      AValue := TConstantValue.Create(1)
    else
      AValue := TdxCriteriaValue.Parse(ANumberString);
  end
  else
  begin
    AUnitType := TUnitType.CriteriaOperator;
    if ASource = '' then
      AValue := TConstantValue.Create(1)
    else
      AValue := TdxCriteriaValue.Parse(ASource);
  end;
  Result := TUnit.Create(AUnitType, AValue);
end;

function TUnit.ToString: string;
begin
  if Value = nil then
    Exit('');
  Result := Value.ToString;
  if (Result = '1') and (FUnitType = TUnitType.Star) then
    Exit('*');
  if FUnitType = TUnitType.Star then
    Result := Result + '*';
end;

{ TUnitCollection }

constructor TUnitCollection.Create(const AUnits: TList<TUnit>);
{$IFNDEF DELPHIXE}
var
  I: Integer;
{$ENDIF}
begin
{$IFDEF DELPHIXE}
  FUnits := AUnits.ToArray;
{$ELSE}
  SetLength(FUnits, AUnits.Count);
  for I := 0 to AUnits.Count - 1 do
    FUnits[I] := AUnits[I];
{$ENDIF}
end;

constructor TUnitCollection.CreateDefault;
begin
  SetLength(FUnits, 1);
  FUnits[0] := TUnit.Create(TUnitType.Star, TConstantValue.Create(1));
end;

destructor TUnitCollection.Destroy;
var
  I: Integer;
begin
  for I := Low(Units) to High(Units) do
    Units[I].Free;
  inherited Destroy;
end;

class constructor TUnitCollection.Initialize;
begin
  FDefault := TUnitCollection.CreateDefault;
end;

class destructor TUnitCollection.Finalize;
begin
  FDefault.Free;
end;

function TUnitCollection.ToString: string;
var
  I: Integer;
begin
  if Length(Units) = 0 then
    Exit('');
  Result := Units[0].ToString;
  for I := 1 to High(Units) do
    Result := Result + ';' + Units[I].ToString;
end;

class function TUnitCollection.Parse(const ASource: string): TUnitCollection;
var
  ATokens: TArray<string>;
  AUnits: TList<TUnit>;
  AUnit: TUnit;
  I: Integer;
begin
  ATokens := TdxStringHelper.Split(ASource, [';']);
  AUnits := TList<TUnit>.Create;
  try
    for I := Low(ATokens) to High(ATokens) do
    begin
      AUnit := TUnit.Parse(ATokens[I]);
      if AUnit <> nil then
        AUnits.Add(AUnit);
    end;
    Result := TUnitCollection.Create(AUnits);
  finally
    AUnits.Free;
  end;
end;

{ TShapeContext }

function TShapeContext.EvaluateDouble(AOp: TdxCriteriaValue): Single;
begin
  Result := AOp.EvaluateDouble(Self);
end;

function TShapeContext.EvaluateSize(AOp: TdxCriteriaValue): TdxSizeF;
begin
  Assert(AOp.ClassType = TSizeValue);
  Result := TSizeValue(AOp).EvaluateSize(Self);
end;

function TShapeContext.EvaluatePoint(AOp: TdxCriteriaValue): TdxPointF;
begin
  Assert(AOp.ClassType = TPointValue);
  Result := TPointValue(AOp).EvaluatePoint(Self);
end;

function TShapeContext.EvaluateRect(AOp: TdxCriteriaValue): TdxRectDouble;
begin
  Assert(AOp.ClassType = TRectValue);
  Result := TRectValue(AOp).EvaluateRect(Self);
end;

{ TShapeLayoutCalculator }

constructor TShapeLayoutCalculator.Create(ARows: TUnitCollection; AColumns: TUnitCollection; const ASize: TdxSizeF; AContext: TShapeContext);
begin
  FSize := ASize;
  FContext := AContext;
  if ARows = nil then
    ARows := TUnitCollection.Default;
  if AColumns = nil then
    AColumns := TUnitCollection.Default;
  FRows := ConvertUnits(ARows.Units, ASize.Height);
  FColumns := ConvertUnits(AColumns.Units, ASize.Width);
end;

function TShapeLayoutCalculator.EvaluateDouble(AOp: TdxCriteriaValue): Single;
begin
  Result := FContext.EvaluateDouble(AOp);
end;

function TShapeLayoutCalculator.EvaluatePoint(AOp: TdxCriteriaValue): TdxPointF;
begin
  Result := FContext.EvaluatePoint(AOp);
end;

function TShapeLayoutCalculator.EvaluateRect(AOp: TdxCriteriaValue): TdxRectDouble;
begin
  Result := FContext.EvaluateRect(AOp);
end;

function TShapeLayoutCalculator.EvaluateSize(AOp: TdxCriteriaValue): TdxSizeF;
begin
  Result := FContext.EvaluateSize(AOp);
end;

function TShapeLayoutCalculator.GetConnectionPoint(APoint: TdxShapeXmlDescription.TConnectionPoint): TdxPointF;
begin
  if APoint.Relative then
    Result := GetPoint(APoint.X, APoint.Y)
  else
    Result := TdxPointF.Create(EvaluateDouble(APoint.X), EvaluateDouble(APoint.Y));
end;

function TShapeLayoutCalculator.GetPoint(AXOperator: TdxCriteriaValue; AYOperator: TdxCriteriaValue): TdxPointF;
var
  AXFactor, X, AYFactor, Y: Single;
begin
  AXFactor := EvaluateDouble(AXOperator);
  X := GetOffsetByFactor(AXFactor, TOrientationKind.Horizontal);

  AYFactor := EvaluateDouble(AYOperator);
  Y := GetOffsetByFactor(AYFactor, TOrientationKind.Vertical);

  Result := TdxPointF.Create(X, Y);
end;

function TShapeLayoutCalculator.GetUnitValue(AUnit: TUnit): Single;
begin
  Result := EvaluateDouble(AUnit.Value);
end;

function TShapeLayoutCalculator.ConvertUnits(const AUnits: TArray<TUnit>; ATotalLength: Single): TArray<Single>;
var
  ALengths: TArray<Single>;
  AFixedLength, AStarCount, ALength, ALengthRest: Single;
  AIndex: Integer;
  AUnit: TUnit;
begin
  SetLength(ALengths, Length(AUnits));
  AFixedLength := 0;
  AStarCount := 0;

  for AIndex := 0 to Length(AUnits) - 1 do
  begin
    AUnit := AUnits[AIndex];
    if AUnit.UnitType = TUnitType.CriteriaOperator then
    begin
      ALength := GetUnitValue(AUnit);
      ALengths[AIndex] := ALength;
      AFixedLength := AFixedLength + ALength;
    end
    else
      if AUnit.UnitType = TUnitType.Star then
        AStarCount := AStarCount + GetUnitValue(AUnit);
  end;

  ALengthRest := ATotalLength - AFixedLength;
  for AIndex := 0 to Length(AUnits) - 1 do
  begin
    AUnit := AUnits[AIndex];
    if AUnit.UnitType = TUnitType.Star then
      ALengths[AIndex] := GetUnitValue(AUnit) / AStarCount * ALengthRest;
  end;
  Result := ALengths;
end;

function TShapeLayoutCalculator.GetOffsetByFactor(AFactor: Single; AOrientation: TOrientationKind): Single;
var
  AIntervals: TArray<Single>;
  I, AIntervalIndex: Integer;
begin
  if IsNaN(AFactor) or IsInfinite(AFactor) then
    Exit(0);
  if AOrientation = TOrientationKind.Horizontal then
    AIntervals := Columns
  else
    AIntervals := Rows;
  if AFactor >= Length(AIntervals) then
    if AOrientation = TOrientationKind.Horizontal then
      Exit(FSize.Width)
    else
      Exit(FSize.Height);
  AFactor := Max(0, AFactor);
  AIntervalIndex := Trunc(Floor(AFactor));
  Result := 0;
  for I := 0 to AIntervalIndex - 1 do
    Result := Result + AIntervals[I];
  Result := Result + AIntervals[AIntervalIndex] * (AFactor - AIntervalIndex);
end;

{ TShapeBuilder }

class function TShapeBuilder.CreateShape(ASegments: TObjectList<TdxShapeSegment>; AContext: TShapeLayoutCalculator): TdxShapeViewData;
var
  AShape: TdxGPPath;
  ASize: TdxSizeF;
  AIsClosed: Boolean;
  ALastPoint, P: TdxPointF;
  ASegment: TdxShapeSegment;
  AStart: TStart absolute ASegment;
  ALine: TLine absolute ASegment;
  AArc: TArc absolute ASegment;
  AArcCommand: TdxSVGParserPathCommandArc;
begin
  Result := TdxShapeViewData.Create;
  AIsClosed := False;
  AShape := nil;
  for ASegment in ASegments do
  begin
    if ASegment.ClassType = TLine then
    begin
      P := AContext.GetPoint(ALine.X, ALine.Y);
      AShape.AddLine(ALastPoint.X, ALastPoint.Y, P.X, P.Y);
      ALastPoint := P;
    end
    else if ASegment.ClassType = TArc then
    begin
      P := AContext.GetPoint(AArc.X, AArc.Y);
      if AArc.Size = nil then
        ASize := TdxSizeF.Create(Abs(ALastPoint.X - P.X), Abs(ALastPoint.Y - P.Y))
      else
        ASize := AContext.EvaluateSize(AArc.Size);
      AArcCommand := TdxSVGParserPathCommandArc.Create(
        ALastPoint, P, TdxPointF.Create(ASize.Width, ASize.Height), 0, AArc.Direction = TdxSweepDirectionKind.Clockwise, False);
      try
        AArcCommand.Append(AShape);
      finally
        AArcCommand.Free;
      end;
      ALastPoint := P;
    end
    else if ASegment.ClassType = TStart then
    begin
      if AIsClosed and (AShape.GetPointCount > 0) then
        AShape.FigureFinish;
      AIsClosed := TGeometryKind.Closed in AStart.Kind;
      ALastPoint := AContext.GetPoint(AStart.X, AStart.Y);
      if AStart.IsNewShape then
      begin
        AShape := TdxGPPath.Create;
        Result.Add(TdxShapeElementViewData.Create(AShape, AStart.Properties));
      end;
    end;
  end;
  if (AShape <> nil) and AIsClosed then
    AShape.FigureFinish;
end;

{ TdxShapeSegment }

constructor TdxShapeSegment.Create;
begin
  inherited Create;
end;

destructor TdxShapeSegment.Destroy;
begin
  FX.Free;
  FY.Free;
  inherited Destroy;
end;

procedure TdxShapeSegment.Load(ANode: TdxXmlNode);
var
  AAttributes: TdxXMLNodeAttributes;
begin
  AAttributes := ANode.Attributes;
  FX := TdxCriteriaValue.Parse(AAttributes.GetValueAsString('X'));
  FY := TdxCriteriaValue.Parse(AAttributes.GetValueAsString('Y'));
end;

{ TStart }

constructor TStart.Create;
begin
  inherited Create;
  FKind := [TGeometryKind.Closed, TGeometryKind.Filled];
  FIsNewShape := True;
  FProperties := TdxShapePathProperties.Create;
end;

destructor TStart.Destroy;
begin
  FProperties.Free;
  inherited Destroy;
end;

procedure TStart.Load(ANode: TdxXmlNode);
var
  AAttributes: TdxXMLNodeAttributes;
begin
  inherited Load(ANode);
  AAttributes := ANode.Attributes;
  FKind := TAttributeReader.GetKind(AAttributes);
  FIsNewShape := AAttributes.GetValueAsBoolean('IsNewShape', True);
  if AAttributes.GetValueAsBoolean('IsSmoothJoin', False) then
    Properties.StrokeLineJoin := TdxGpLineJoin.LineJoinRound
  else
    Properties.StrokeLineJoin := TdxGpLineJoin.LineJoinMiter;
  Properties.FillBrightness := AAttributes.GetValueAsFloat('FillBrightness', 0.0);
  Properties.FillColor := TAttributeReader.GetNullableColor(AAttributes, 'FillColor');
  Properties.StrokeColor := TAttributeReader.GetNullableColor(AAttributes, 'StrokeColor');
  Properties.StrokeThickness := TAttributeReader.GetNullableDouble(AAttributes, 'StrokeThickness');
  Properties.StrokeDashArray := TAttributeReader.GetArrayOfDouble(AAttributes, 'StrokeDashArray', ' ');
  Properties.Transparent := not (TGeometryKind.Filled in FKind);
end;

{ TArc }

destructor TArc.Destroy;
begin
  FSize.Free;
  inherited Destroy;
end;

class function TArc.GetDirection(AAttributes: TdxXMLNodeAttributes): TdxSweepDirectionKind;
begin
  if MatchValue(AAttributes.GetValue('Direction'), 'Clockwise') then
    Result := TdxSweepDirectionKind.Clockwise
  else
    Result := TdxSweepDirectionKind.Counterclockwise;
end;

procedure TArc.Load(ANode: TdxXmlNode);
begin
  inherited Load(ANode);
  FDirection := GetDirection(ANode.Attributes);
  FSize := TdxCriteriaValue.Parse(ANode.Attributes.GetValueAsString('Size'));
end;

{ TdxShapeXmlDescription.TConnectionPoint }

constructor TdxShapeXmlDescription.TConnectionPoint.Create;
var
  AAttributes: TdxXMLNodeAttributes;
begin
  inherited Create;
  AAttributes := ANode.Attributes;
  FRelative := not SameText(AAttributes.GetValueAsString('Kind'), 'Absolute');
  FX := TdxCriteriaValue.Parse(AAttributes.GetValueAsString('X'));
  FY := TdxCriteriaValue.Parse(AAttributes.GetValueAsString('Y'));
end;

destructor TdxShapeXmlDescription.TConnectionPoint.Destroy;
begin
  FX.Free;
  FY.Free;
  inherited Destroy;
end;

{ TdxShapeXmlDescription.TConnectionPoints }

class function TdxShapeXmlDescription.TConnectionPoints.Load(ANode: TdxXMLNode): TConnectionPoints;
var
  AConnectionPointNode: TdxXMLNode;
begin
  Result := nil;
  AConnectionPointNode := ANode.First;
  while AConnectionPointNode <> nil do
  begin
    if not MatchName(AConnectionPointNode, 'ShapePoint') then
      continue;
    if Result = nil then
      Result := TConnectionPoints.Create;
    Result.Add(TConnectionPoint.Create(AConnectionPointNode));
    AConnectionPointNode := AConnectionPointNode.Next;
  end;
end;

{ TdxShapeXmlDescription.TParameter }

constructor TdxShapeXmlDescription.TParameter.Create(ANode: TdxXMLNode);
begin
  FDefaultValue := ANode.Attributes.GetValueAsFloat('DefaultValue', 100);
  FPoint := TdxCriteriaValue.Parse(ANode.Attributes.GetValueAsString('Point'));
  FMin := TdxCriteriaValue.Parse(ANode.Attributes.GetValueAsString('Min'));
  FMax := TdxCriteriaValue.Parse(ANode.Attributes.GetValueAsString('Max'));
  FValue := TdxCriteriaValue.Parse(ANode.Attributes.GetValueAsString('Value'));
end;

destructor TdxShapeXmlDescription.TParameter.Destroy;
begin
  FValue.Free;
  FPoint.Free;
  FMin.Free;
  FMax.Free;
  inherited Destroy;
end;

class function TdxShapeXmlDescription.TParameter.Evaluate(AContext: TdxEvaluatorContext; AOp: TdxCriteriaValue): Single;
begin
  Result := AOp.EvaluateDouble(AContext);
end;

function TdxShapeXmlDescription.TParameter.GetValue(const ASize: TdxSizeF; const AParameters: TArray<Single>;
  const ALocalPoint: TdxPointF): Single;
var
  AValueContext: TdxEvaluatorContext;
  AValue: Single;
begin
  AValueContext := TdxEvaluatorContext.Create(ASize, ALocalPoint);
  try
    AValue := Evaluate(AValueContext, Value);
    Result := Normalize(ASize, AParameters, AValue);
  finally
    AValueContext.Free;
  end;
end;

function TdxShapeXmlDescription.TParameter.GetPoint(const ASize: TdxSizeF;
  const AParameters: TArray<Single>; AValue: Single): TdxPointF;
var
  AContext: TShapeContext;
begin
  AValue := Normalize(ASize, AParameters, AValue);
  AContext := TShapeContext.Create(ASize, AValue);
  try
    Result := AContext.EvaluatePoint(FPoint);
  finally
    AContext.Free;
  end;
end;

function TdxShapeXmlDescription.TParameter.Normalize(const ASize: TdxSizeF;
  const AParameters: TArray<Single>; AValue: Single): Single;
var
  AContext: TShapeContext;
begin
  AContext := TShapeContext.Create(ASize, AParameters);
  try
    if FMin <> nil then
      AValue := Math.Max(AValue, Evaluate(AContext, FMin));
    if FMax <> nil then
      AValue := Math.Min(AValue, Evaluate(AContext, FMax));
    Result := AValue;
  finally
    AContext.Free;
  end;
end;

{ TdxShapeXmlDescription.TParameters }

class function TdxShapeXmlDescription.TParameters.Load(ANode: TdxXMLNode): TParameters;
var
  AParameterNode: TdxXMLNode;
begin
  Result := nil;
  AParameterNode := ANode.First;
  while AParameterNode <> nil do
  begin
    if not MatchName(AParameterNode, 'Parameter') then
      continue;
    if Result = nil then
      Result := TParameters.Create;
    try
      Result.Add(TParameter.Create(AParameterNode));
    except
      FreeAndNil(Result);
      raise;
    end;
    AParameterNode := AParameterNode.Next;
  end;
end;

{ TdxShapeXmlDescription }

constructor TdxShapeXmlDescription.Create;
begin
  FSegments := TObjectList<TdxShapeSegment>.Create;
end;

destructor TdxShapeXmlDescription.Destroy;
begin
  if FRows <> TUnitCollection.Default then
    FRows.Free;
  if FColumns <> TUnitCollection.Default then
    FColumns.Free;
  FParameters.Free;
  FConnectionPoints.Free;
  FSegments.Free;
  inherited Destroy;
end;

function TdxShapeXmlDescription.CreateViewData(const ASize: TdxSizeF; const AParameters: TArray<Single>): TdxShapeViewData;
var
  AShapeContext: TShapeContext;
  AContext: TShapeLayoutCalculator;
begin
  AShapeContext := TShapeContext.Create(ASize, AParameters);
  try
    AContext := TShapeLayoutCalculator.Create(Rows, Columns, ASize, AShapeContext);
    try
      Result := TShapeBuilder.CreateShape(Segments, AContext);
    finally
      AContext.Free;
    end;
  finally
    AShapeContext.Free;
  end;
end;

function TdxShapeXmlDescription.GetCaption: string;
begin
  Result := FCaption;
  if Result = '' then
    Result := ID;
end;

function TdxShapeXmlDescription.GetDefaultSize(AAttributes: TdxXMLNodeAttributes): TdxSizeF;
var
  AValues: TArray<Single>;
begin
  AValues := TAttributeReader.GetArrayOfDouble(AAttributes, 'DefaultSize', ',');
  if Length(AValues) <> 2 then
    Exit(TdxSizeF.Create(100, 100));
  Result.Width := AValues[0];
  Result.Height := AValues[1];
end;

function TdxShapeXmlDescription.GetNormalizedParameters(const ASize: TdxSizeF; const AParameters: TArray<Single>): TArray<Single>;
var
  I, ACount: Integer;
begin
  if Parameters = nil then
    Exit(nil);
  ACount := Parameters.Count;
  if ACount = 0 then
    Exit(nil);
  SetLength(Result, ACount);
  Move(AParameters[0], Result[0], SizeOf(Single) * ACount);
  for I := 0 to ACount - 1 do
    Result[I] := Parameters[I].Normalize(ASize, Result, AParameters[I]);
end;

function TdxShapeXmlDescription.GetConnectionPoints(const ASize: TdxSizeF;
  const AParameters: TArray<Single>): TArray<TdxPointF>;
var
  AShapeContext: TShapeContext;
  AContext: TShapeLayoutCalculator;
  I: Integer;
begin
  if (ConnectionPoints = nil) or (ConnectionPoints.Count = 0) then
    Exit(nil)
  else
    SetLength(Result, ConnectionPoints.Count);
  AShapeContext := TShapeContext.Create(ASize, AParameters);
  try
    AContext := TShapeLayoutCalculator.Create(Rows, Columns, ASize, AShapeContext);
    try
      for I := 0 to ConnectionPoints.Count - 1 do
        Result[I] := AContext.GetConnectionPoint(ConnectionPoints[I]);
    finally
      AContext.Free;
    end;
  finally
    AShapeContext.Free;
  end;
end;

function TdxShapeXmlDescription.GetParameterDefaultValue(AParameterIndex: Integer): Single;
begin
  Result := Parameters[AParameterIndex].DefaultValue;
end;

function TdxShapeXmlDescription.GetParameterPoint(AParameterIndex: Integer; const ASize: TdxSizeF;
  const AParameters: TArray<Single>; AValue: Single): TdxPointF;
begin
  Result := Parameters[AParameterIndex].GetPoint(ASize, AParameters, AValue);
end;

function TdxShapeXmlDescription.GetParameterValue(AParameterIndex: Integer; const ASize: TdxSizeF;
  const AParameters: TArray<Single>; const ALocalPoint: TdxPointF): Single;
begin
  Result := Parameters[AParameterIndex].GetValue(ASize, AParameters, ALocalPoint);
end;

function TdxShapeXmlDescription.GetParameters(const ASize: TdxSizeF; const AParameters: TArray<Single>): TArray<Single>;
var
  I, ACount: Integer;
begin
  if Parameters = nil then
    Exit(nil);
  ACount := Parameters.Count;
  SetLength(Result, ACount);
  if AParameters = nil then
  begin
    for I := 0 to ACount - 1 do
      Result[I] := Parameters[I].DefaultValue;
    Exit;
  end;
  Move(AParameters[0], Result[0], SizeOf(Single) * ACount);
  for I := 0 to ACount - 1 do
    Result[I] := Parameters[I].Normalize(ASize, Result, AParameters[I]);
end;

procedure TdxShapeXmlDescription.Load(ANode: TdxXmlNode);
var
  I: Integer;
begin
  ID := ANode.Attributes.GetValueAsString('Id');
  for I := 0 to ANode.Count - 1 do
    if not ParseTemplateNode(ANode[I]) then
      Break;
  if FSegments.Count < 2 then
    raise EdxFlowChartShapeParserError.Create('Empty shape description!');
  if FSegments[0].ClassType <> TStart then
    raise EdxFlowChartShapeParserError.Create('Shape description must begins with Start section!');
  DefaultSize := GetDefaultSize(ANode.Attributes);
  UseBackgroundAsForeground := ANode.Attributes.GetValueAsBoolean('UseBackgroundAsForeground', True);
  FRows := TUnitCollection.Parse(ANode.Attributes.GetValueAsString('Rows', '*'));
  FColumns := TUnitCollection.Parse(ANode.Attributes.GetValueAsString('Columns', '*'));
  FCaption := ANode.Attributes.GetValueAsString('Caption', '');
end;

procedure TdxShapeXmlDescription.ParseConnectionPoints(ANode: TdxXMLNode);
begin
  FConnectionPoints := TConnectionPoints.Load(ANode);
end;

procedure TdxShapeXmlDescription.ParseParameters(ANode: TdxXMLNode);
begin
  FParameters := TParameters.Load(ANode);
end;

procedure TdxShapeXmlDescription.ParseSegment(ANode: TdxXmlNode; ASegmentClass: TdxShapeSegmentClass);
var
  ASegment: TdxShapeSegment;
begin
  ASegment := ASegmentClass.Create;
  ASegment.Load(ANode);
  FSegments.Add(ASegment);
end;

function TdxShapeXmlDescription.ParseTemplateNode(ANode: TdxXMLNode): Boolean;
begin
  Result := True;
  if MatchName(ANode, 'Parameters') then
    ParseParameters(ANode)
  else if MatchName(ANode, 'ConnectionPoints') then
    ParseConnectionPoints(ANode)
  else if MatchName(ANode, 'Start') then
    ParseSegment(ANode, TStart)
  else if MatchName(ANode, 'Line') then
    ParseSegment(ANode, TLine)
  else if MatchName(ANode, 'Arc') then
    ParseSegment(ANode, TArc)
  else
    Result := False;
end;

{ TdxTemplateLoader }

procedure TdxTemplateLoader.LoadFromStream(AStream: TStream; const AAddShape: TAddShapeProcedure);
var
  ADoc: TdxXMLDocument;
  ARoot, ANode: TdxXMLNode;
  I: Integer;
begin
  FAddShape := AAddShape;
  ADoc := TdxXMLDocument.Create;
  try
    ADoc.LoadFromStream(AStream);
    ARoot := ADoc.Root;
    for I := 0 to ARoot.Count - 1 do
    begin
      ANode := ARoot[I];
      if MatchName(ANode, 'Shapes') then
        ParseShapes(ANode)
      else if MatchName(ANode, 'ShapeTemplate') then
        ParseShapeTemplate(ANode);
    end;
  finally
    ADoc.Free;
  end;
end;

procedure TdxTemplateLoader.ParseShapes(ANode: TdxXmlNode);
var
  I: Integer;
begin
  for I := 0 to ANode.Count - 1 do
    if MatchName(ANode[I], 'ShapeTemplate') then
      ParseShapeTemplate(ANode[I]);
end;

procedure TdxTemplateLoader.ParseShapeTemplate(ANode: TdxXmlNode);
var
  ATemplate: TdxShapeXmlDescription;
begin
  if not Assigned(FAddShape) then
    Exit;
  ATemplate := TdxShapeXmlDescription.Create;
  try
    ATemplate.Load(ANode);
    FAddShape(ATemplate);
  except
    ATemplate.Free;
    raise;
  end;
end;

end.

