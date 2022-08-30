{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
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

unit dxPSShapes;

interface

{$I cxVer.inc}

uses
  Classes, Windows, Graphics, dxBase, dxPSCore, dxPSFillPatterns,
  dxPSReportRenderCanvas;

type
  TdxReportCellShapeClass = class of TCustomdxReportCellShape;

  TdxPSShapeFactory = class(TdxCustomClassFactory)
  private
    function GetItem(Index: Integer): TdxReportCellShapeClass;
  public
    class function Instance: TdxPSShapeFactory; reintroduce; overload;
    procedure GetList(const AStrings: TStrings);
    property Items[Index: Integer]: TdxReportCellShapeClass read GetItem; default;
  end;

  TCustomdxReportCellShape = class(TAbstractdxReportCellData)
  private
    FContentBkColor: TColor;
    FContentPattern: TdxPSFillPatternClass;
    FShapeBorderColor: TColor;
    FShapeBorderThickness: Integer;
    FShapeColor: TColor;
    function GetShapeTransparent: Boolean;
    procedure SetShapeBorderThickness(Value: Integer);
    procedure SetShapeTransparent(Value: Boolean);
  protected
    function GetContentBkColor: TColor; override;
    function GetContentPattern: TdxPSFillPatternClass; override;
    procedure SetContentBkColor(Value: TColor); override;
    procedure SetContentPattern(Value: TdxPSFillPatternClass); override;

    procedure ReadData(AReader: TdxPSDataReader); override;
    procedure WriteData(AWriter: TdxPSDataWriter); override;

    function GetShapeCenter: TPoint; virtual;
    function GetShapeHeight: Integer; virtual;
    function GetShapeRect: TRect; virtual;
    function GetShapeWidth: Integer; virtual;
  public
    constructor Create(AParent: TdxReportCell); override;
    procedure Assign(Source: TPersistent); override;

    class function Name: string; virtual;
    class procedure RegisterShape; virtual;
    class procedure UnregisterShape; virtual;

    property ShapeBorderColor: TColor read FShapeBorderColor write FShapeBorderColor;
    property ShapeBorderThickness: Integer read FShapeBorderThickness write SetShapeBorderThickness;
    property ShapeCenter: TPoint read GetShapeCenter;
    property ShapeColor: TColor read FShapeColor write FShapeColor;
    property ShapeHeight: Integer read GetShapeHeight;
    property ShapeRect: TRect read GetShapeRect;
    property ShapeTransparent: Boolean read GetShapeTransparent write SetShapeTransparent;
    property ShapeWidth: Integer read GetShapeWidth;
  end;

  TCustomdxReportCellEllipseShape = class(TCustomdxReportCellShape)
  private
    function GetRadiusHorz: Integer;
    function GetRadiusVert: Integer;
  protected
    property RadiusHorz: Integer read GetRadiusHorz;
    property RadiusVert: Integer read GetRadiusVert;
  public
    procedure DrawContent(ACanvas: TdxPSReportRenderCustomCanvas;
      AStage: TdxPSRenderStages); override;
  end;

  TdxReportCellEllipse = class(TCustomdxReportCellEllipseShape)
  public
    class function Name: string; override;
    property RadiusHorz;
    property RadiusVert;
  end;

  TdxReportCellCircle = class(TCustomdxReportCellEllipseShape)
  private
    function GetRadius: Integer;
  protected
    function GetShapeHeight: Integer; override;
    function GetShapeWidth: Integer; override;
  public
    class function Name: string; override;
    property Radius: Integer read GetRadius;
  end;

  TCustomdxReportCellRectangle = class(TCustomdxReportCellShape)
  protected
    function GetShapeRect: TRect; override;
  public
    procedure DrawContent(ACanvas: TdxPSReportRenderCustomCanvas;
      AStage: TdxPSRenderStages); override;
  end;

  TdxReportCellRectangle = class(TCustomdxReportCellRectangle)
  public
    class function Name: string; override;
  end;

  TdxReportCellSquare = class(TCustomdxReportCellRectangle)
  protected
    function GetShapeHeight: Integer; override;
    function GetShapeWidth: Integer; override;
  public
    class function Name: string; override;
  end;

  TdxReportCellRoundRect = class(TCustomdxReportCellRectangle)
  private
    FEllipseHeight: Integer;
    FEllipseWidth: Integer;
    procedure SetEllipseHeight(Value: Integer);
    procedure SetEllipseWidth(Value: Integer);
  protected
    procedure ConvertCoords(APixelsNumerator, APixelsDenominator: Integer); override;
  public
    constructor Create(AParent: TdxReportCell); override;
    procedure Assign(Source: TPersistent); override;

    procedure DrawContent(ACanvas: TdxPSReportRenderCustomCanvas;
      AStage: TdxPSRenderStages); override;
    class function Name: string; override;

    property EllipseHeight: Integer read FEllipseHeight write SetEllipseHeight;
    property EllipseWidth: Integer read FEllipseWidth write SetEllipseWidth;
  end;

  TdxReportCellRoundSquare = class(TdxReportCellRoundRect)
  protected
    function GetShapeHeight: Integer; override;
    function GetShapeWidth: Integer; override;
  public
    class function Name: string; override;
  end;

implementation

uses
 cxClasses, dxPSGlbl, dxPSRes, dxPSUtl, dxCore;

{ TdxPSShapeFactory }

function dxPSShapeFactory: TdxPSShapeFactory;
begin
  Result := TdxPSShapeFactory.Instance;
end;

class function TdxPSShapeFactory.Instance: TdxPSShapeFactory;
begin
  Result := inherited Instance as TdxPSShapeFactory;
end;

procedure TdxPSShapeFactory.GetList(const AStrings: TStrings);
var
  I: Integer;
  Shape: TdxReportCellShapeClass;
begin
  AStrings.BeginUpdate;
  try
    for I := 0 to Count - 1 do
    begin
      Shape := Items[I];
      AStrings.AddObject(Shape.Name, TObject(Shape));
    end;
  finally
    AStrings.EndUpdate;
  end;
end;

function TdxPSShapeFactory.GetItem(Index: Integer): TdxReportCellShapeClass;
begin
  Result := TdxReportCellShapeClass(inherited Items[Index]);
end;

{ TCustomdxReportCellShape }

constructor TCustomdxReportCellShape.Create(AParent: TdxReportCell);
begin
  inherited;
  FShapeBorderColor := clBlack;
  FShapeBorderThickness := 1;
end;

procedure TCustomdxReportCellShape.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TCustomdxReportCellShape then
    with TCustomdxReportCellShape(Source) do
    begin
      Self.ShapeBorderColor := ShapeBorderColor;
      Self.ShapeBorderThickness := ShapeBorderThickness;
      Self.ShapeColor := ShapeColor;
    end;
end;

class function TCustomdxReportCellShape.Name: string;
begin
  Result := '';
end;

class procedure TCustomdxReportCellShape.RegisterShape;
begin
  dxPSShapeFactory.Register(Self);
end;

class procedure TCustomdxReportCellShape.UnregisterShape;
begin
  dxPSShapeFactory.Unregister(Self);
end;

function TCustomdxReportCellShape.GetContentBkColor: TColor;
begin
  Result := FContentBkColor;
end;

function TCustomdxReportCellShape.GetContentPattern: TdxPSFillPatternClass;
begin
  Result := FContentPattern;
end;

procedure TCustomdxReportCellShape.SetContentBkColor(Value: TColor);
begin
  FContentBkColor := Value;
end;

procedure TCustomdxReportCellShape.SetContentPattern(Value: TdxPSFillPatternClass);
begin
  FContentPattern := Value;
end;

procedure TCustomdxReportCellShape.ReadData(AReader: TdxPSDataReader);
begin
  inherited;
  ShapeBorderColor := AReader.ReadInteger;
  ShapeBorderThickness := AReader.ReadInteger;
  ShapeColor := AReader.ReadInteger;
end;

procedure TCustomdxReportCellShape.WriteData(AWriter: TdxPSDataWriter);
begin
  inherited;
  AWriter.WriteInteger(ShapeBorderColor);
  AWriter.WriteInteger(ShapeBorderThickness);
  AWriter.WriteInteger(ShapeColor);
end;

function TCustomdxReportCellShape.GetShapeCenter: TPoint;
begin
  Result.X := Left + Width div 2;
  Result.Y := Top + Height div 2;
end;

function TCustomdxReportCellShape.GetShapeHeight: Integer;
begin
  Result := Height;
end;

function TCustomdxReportCellShape.GetShapeRect: TRect;
var
  SW, SH: Integer;
begin
  SH := ShapeHeight;
  SW := ShapeWidth;
  with ShapeCenter do
    Result := Bounds(X - SW div 2, Y - SH div 2, SW, SH);
end;

function TCustomdxReportCellShape.GetShapeWidth: Integer;
begin
  Result := Width;
end;

function TCustomdxReportCellShape.GetShapeTransparent: Boolean;
begin
  Result := GetFormatBit(dxFormatShapeTransparent);
end;

procedure TCustomdxReportCellShape.SetShapeBorderThickness(Value: Integer);
begin
  if Value < 0 then Value := 0;
  FShapeBorderThickness := Value;
end;

procedure TCustomdxReportCellShape.SetShapeTransparent(Value: Boolean);
begin
  SetFormatBit(dxFormatShapeTransparent, Value);
end;

{ TCustomdxReportCellEllipseShape }

procedure TCustomdxReportCellEllipseShape.DrawContent(
  ACanvas: TdxPSReportRenderCustomCanvas; AStage: TdxPSRenderStages);
begin
  Renderer.DrawEllipse(ACanvas, ShapeRect, ShapeColor, ContentBkColor,
    ContentPattern, ShapeBorderColor, ShapeBorderThickness);
end;

function TCustomdxReportCellEllipseShape.GetRadiusHorz: Integer;
begin
  Result := ShapeWidth div 2;
end;

function TCustomdxReportCellEllipseShape.GetRadiusVert: Integer;
begin
  Result := ShapeHeight div 2;
end;

{ TdxReportCellEllipse }

class function TdxReportCellEllipse.Name: string;
begin
  Result := cxGetResourcestring(@sdxEllipse);
end;

{ TdxReportCellCircle }

class function TdxReportCellCircle.Name: string;
begin
  Result := cxGetResourcestring(@sdxCircle);
end;

function TdxReportCellCircle.GetShapeHeight: Integer;
begin
  if Width > Height then
    Result := inherited GetShapeHeight
  else
    Result := ShapeWidth;
end;

function TdxReportCellCircle.GetShapeWidth: Integer;
begin
  if Width > Height then
    Result := ShapeHeight
  else
    Result := inherited GetShapeWidth;
end;

function TdxReportCellCircle.GetRadius: Integer;
begin
  Result := RadiusHorz;
end;

{ TCustomdxReportCellRectangle }

procedure TCustomdxReportCellRectangle.DrawContent(
  ACanvas: TdxPSReportRenderCustomCanvas; AStage: TdxPSRenderStages);
begin
  Renderer.DrawRectangle(ACanvas, ShapeRect, ShapeColor, ContentBkColor,
    ContentPattern, ShapeBorderColor, ShapeBorderThickness);
end;

function TCustomdxReportCellRectangle.GetShapeRect: TRect;
var
  SH, SW: Integer;
begin
  SH := ShapeHeight;
  SW := ShapeWidth;
  with ShapeCenter do
    Result := Bounds(X - SW div 2, Y - SH div 2, SW, SH);
end;

{ TdxReportCellRectangle }

class function TdxReportCellRectangle.Name: string;
begin
  Result := cxGetResourcestring(@sdxRectangle);
end;

{ TdxReportCellSquare }

class function TdxReportCellSquare.Name: string;
begin
  Result := cxGetResourcestring(@sdxSquare);
end;

function TdxReportCellSquare.GetShapeHeight: Integer;
begin
  if Width > Height then
    Result := inherited GetShapeHeight
  else
    Result := ShapeWidth;
end;

function TdxReportCellSquare.GetShapeWidth: Integer;
begin
  if Width > Height then
    Result := ShapeHeight
  else
    Result := inherited GetShapeWidth;
end;

{ TdxReportCellRoundRect }

constructor TdxReportCellRoundRect.Create(AParent: TdxReportCell);
begin
  inherited;
  FEllipseHeight := 2;
  FEllipseWidth := 2;
end;

procedure TdxReportCellRoundRect.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TdxReportCellRoundRect then
    with TdxReportCellRoundRect(Source) do
    begin
      Self.EllipseHeight := EllipseHeight;
      Self.EllipseWidth := EllipseWidth;
    end;
end;

procedure TdxReportCellRoundRect.DrawContent(
  ACanvas: TdxPSReportRenderCustomCanvas; AStage: TdxPSRenderStages);
begin
  Renderer.DrawRoundRect(ACanvas, ShapeRect, EllipseWidth, EllipseHeight,
    ShapeColor, ContentBkColor, ContentPattern, ShapeBorderColor,
    ShapeBorderThickness);
end;

class function TdxReportCellRoundRect.Name: string;
begin
  Result := cxGetResourcestring(@sdxRoundRect);
end;

procedure TdxReportCellRoundRect.ConvertCoords(APixelsNumerator, APixelsDenominator: Integer);
begin
  inherited;
  FEllipseHeight := MulDiv(FEllipseHeight, APixelsNumerator, APixelsDenominator);
  FEllipseWidth := MulDiv(FEllipseWidth, APixelsNumerator, APixelsDenominator);
end;

procedure TdxReportCellRoundRect.SetEllipseHeight(Value: Integer);
begin
  if Value < 0 then Value := 0;
  FEllipseHeight := Value;
end;

procedure TdxReportCellRoundRect.SetEllipseWidth(Value: Integer);
begin
  if Value < 0 then Value := 0;
  FEllipseWidth := Value;
end;

{ TdxReportCellRoundSquare }

class function TdxReportCellRoundSquare.Name: string;
begin
  Result := cxGetResourcestring(@sdxRoundSquare);
end;

function TdxReportCellRoundSquare.GetShapeHeight: Integer;
begin
  if Width > Height then
    Result := inherited GetShapeHeight
  else
    Result := ShapeWidth;
end;

function TdxReportCellRoundSquare.GetShapeWidth: Integer;
begin
  if Width > Height then
    Result := ShapeHeight
  else
    Result := inherited GetShapeWidth;
end;

procedure RegisterItems;
begin
  TdxReportCellEllipse.Register;
  TdxReportCellCircle.Register;
  TdxReportCellRectangle.Register;
  TdxReportCellSquare.Register;
  TdxReportCellRoundRect.Register;
  TdxReportCellRoundSquare.Register;
end;

procedure RegisterShapes;
begin
  TdxReportCellEllipse.RegisterShape;
  TdxReportCellCircle.RegisterShape;
  TdxReportCellRectangle.RegisterShape;
  TdxReportCellSquare.RegisterShape;
  TdxReportCellRoundRect.RegisterShape;
  TdxReportCellRoundSquare.RegisterShape;
end;

procedure UnregisterItems;
begin
  TdxReportCellRoundSquare.Unregister;
  TdxReportCellRoundRect.Unregister;
  TdxReportCellSquare.Unregister;
  TdxReportCellRectangle.Unregister;
  TdxReportCellCircle.Unregister;
  TdxReportCellEllipse.Unregister;
end;

initialization
  RegisterItems;
  RegisterShapes;

finalization
  UnregisterItems;

end.
