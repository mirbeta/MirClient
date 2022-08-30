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

unit dxCompositeShape;

interface

{$I cxVer.inc}

uses
  Windows, Classes, SysUtils, Graphics, Contnrs,
  dxCore, cxClasses, cxGeometry, dxCoreGraphics, cxGraphics, dxGDIPlusClasses, dxXMLDoc,
  dxShapePrimitives;

type
  { TdxCompositeShape }

  TdxCompositeShape = class(TGraphic)
  private
    FBounds: TdxRectF;
    FNeedNormalize: Boolean;
    FShapes: TdxShapeList;
    FStream: TMemoryStream;

    function GetHeightF: Single;
    function GetWidthF: Single;

    function GetBounds: TdxRectF;
    function GetShape(AIndex: Integer): TdxCustomVectorShape;
    function GetShapeCount: Integer;
    function GetRootShape: TdxCustomVectorShape;
    function NeedDraw: Boolean;

    procedure DoWorldTransform(AGraphics: TdxGPGraphics; const ADestRect: TdxRectF);
    procedure SaveStreamCopy(AStream: TStream);

    property Shapes[Index: Integer]: TdxCustomVectorShape read GetShape;
  protected
    function GetEmpty: Boolean; override;
    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    procedure SetHeight(Value: Integer); override;
    procedure SetWidth(Value: Integer); override;

    procedure Add(AShape: TdxCustomVectorShape);
    procedure LoadFromNode(ANode: TdxXMLNode);
    procedure LoadFromShape(AShape: TdxCompositeShape; const AShapeName: string);
    procedure Draw(ACanvas: TCanvas; const ARect: TRect); override;
    procedure DrawF(AGPGraphics: TdxGPGraphics; const ARect: TdxRectF);
    procedure SetShapesColor(AAlphaColor: TdxAlphaColor; const AShapeName: string);

    property HeightF: Single read GetHeightF;
    property NeedNormalize: Boolean read FNeedNormalize write FNeedNormalize;
    property ShapeCount: Integer read GetShapeCount;
    property WidthF: Single read GetWidthF;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(ASource: TPersistent); override;

    //TGraphic
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle; APalette: HPALETTE); override;
    procedure LoadFromStream(AStream: TStream); override;
    procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle; var APalette: HPALETTE); override;
    procedure SaveToStream(AStream: TStream); override;
  end;

implementation

uses
  Types, Math, dxGDIPlusAPI, dxCustomTree, dxShapeReaders, dxShapeBrushes, dxCoreClasses;

type
  TdxCustomVectorShapeAccess = class(TdxCustomVectorShape);
  TdxShapeCanvasAccess = class(TdxShapeCanvas);

{ TdxCompositeShape }

constructor TdxCompositeShape.Create;
begin
  inherited;
  FShapes := TdxShapeList.Create(True);
  FStream := TMemoryStream.Create;
end;

destructor TdxCompositeShape.Destroy;
begin
  FreeAndNil(FStream);
  FreeAndNil(FShapes);
  inherited;
end;

procedure TdxCompositeShape.Assign(ASource: TPersistent);
begin
  inherited;
  if ASource is TdxCompositeShape then
  begin
    FShapes.Assign(TdxCompositeShape(ASource).FShapes);
    SaveStreamCopy(TdxCompositeShape(ASource).FStream);
  end;
end;

procedure TdxCompositeShape.LoadFromClipboardFormat(AFormat: Word; AData: THandle; APalette: HPALETTE);
begin
//do nothing
end;

procedure TdxCompositeShape.LoadFromStream(AStream: TStream);
var
  AXMLDocument: TdxXMLDocument;
begin
  AXMLDocument := TdxXMLDocument.Create(nil);
  try
    FShapes.Clear;
    SaveStreamCopy(AStream);
    FStream.Position := 0;
    AXMLDocument.LoadFromStream(FStream);
    if (AXMLDocument <> nil) and (AXMLDocument.Root <> nil) and AXMLDocument.Root.HasChildren then
      LoadFromNode(AXMLDocument.Root)
  finally
    FreeAndNil(AXMLDocument);
  end;
end;

procedure TdxCompositeShape.SaveToClipboardFormat(var AFormat: Word; var AData: THandle; var APalette: HPALETTE);
var
  ABitmap: TBitmap;
begin
  ABitmap := TBitmap.Create;
  try
    ABitmap.Width := Width;
    ABitmap.Height := Height;
    ABitmap.Canvas.Draw(0, 0, Self);
    ABitmap.SaveToClipboardFormat(AFormat, AData, APalette);
  finally
    ABitmap.Free;
  end;
end;

procedure TdxCompositeShape.SaveToStream(AStream: TStream);
begin
  if FStream <> nil then
  begin
    FStream.Position := 0;
    AStream.CopyFrom(FStream, 0);
  end;
end;

function TdxCompositeShape.GetEmpty: Boolean;
begin
  Result := FShapes.Count = 0;
end;

function TdxCompositeShape.GetHeight: Integer;
begin
  if Empty then
    Result := 0
  else
    Result := Round(GetHeightF);
end;

function TdxCompositeShape.GetWidth: Integer;
begin
  if Empty then
    Result := 0
  else
    Result := Round(GetWidthF);
end;

procedure TdxCompositeShape.SetHeight(Value: Integer);
begin
//do nothing
end;

procedure TdxCompositeShape.SetWidth(Value: Integer);
begin
//do nothing
end;

procedure TdxCompositeShape.Add(AShape: TdxCustomVectorShape);
begin
  TdxCustomVectorShapeAccess(AShape as TdxShapeCanvas).Reference;
  FShapes.Add(AShape);
  FBounds := GetBounds;
end;

procedure TdxCompositeShape.LoadFromNode(ANode: TdxXMLNode);
var
  ACanvasShape: TdxShapeCanvas;
  ACanvasReader: TdxShapeCanvasReader;
  ACanvasNode: TdxXMLNode;
begin
  if ANode <> nil then
  begin
    ACanvasNode := ANode.First;
    if SameText(dxXMLStringToString(ACanvasNode.Name), TdxShapeCanvas.GetName) then
    begin
      ACanvasShape := TdxShapeCanvas.Create;
      ACanvasReader := TdxShapeCanvasReader.Create;
      try
        ACanvasReader.ReadFromNode(ACanvasShape, ACanvasNode);
        TdxShapeCanvasAccess(ACanvasShape).IsRoot := True;
        Add(ACanvasShape);
      finally
        FreeAndNil(ACanvasReader);
      end;
    end
    else
      raise EdxException.Create('Invalid root node');
  end;
end;

procedure TdxCompositeShape.LoadFromShape(AShape: TdxCompositeShape; const AShapeName: string);
var
  I: Integer;
  ACanvas: TdxShapeCanvas;
begin
  FShapes.Clear;
  if not AShape.Empty then
  begin
    ACanvas := AShape.GetRootShape as TdxShapeCanvas;
    for I := 0 to ACanvas.Count - 1 do
      if SameText(ACanvas.Shapes[I].Name, AShapeName) then
      begin
        FNeedNormalize := True;
        Add(ACanvas.Shapes[I] as TdxShapeCanvas);
        Break;
      end;
  end;
end;

procedure TdxCompositeShape.Draw(ACanvas: TCanvas; const ARect: TRect);
var
  AGPGraphics: TdxGPGraphics;
begin
  AGPGraphics := dxGpBeginPaint(ACanvas.Handle, ARect);
  try
    AGPGraphics.SmoothingMode := smAntiAlias;
    DrawF(AGPGraphics, cxRectF(ARect));
  finally
    dxGpEndPaint(AGPGraphics);
  end;
end;

procedure TdxCompositeShape.DrawF(AGPGraphics: TdxGPGraphics; const ARect: TdxRectF);
var
  I: Integer;
begin
  if NeedDraw then
  begin
    AGPGraphics.SaveWorldTransform;
    try
      DoWorldTransform(AGPGraphics, ARect);
      for I := 0 to FShapes.Count - 1 do
        TdxCustomVectorShapeAccess(Shapes[I]).Draw(AGPGraphics);
    finally
      AGPGraphics.RestoreWorldTransform;
    end;
  end;
end;

procedure TdxCompositeShape.SetShapesColor(AAlphaColor: TdxAlphaColor; const AShapeName: string);
var
  ARootShape: TdxShapeCanvas;
begin
  if not GetEmpty then
  begin
    ARootShape := GetRootShape as TdxShapeCanvas;
    ARootShape.BeginUpdate;
    TdxShapeCanvasAccess(ARootShape).SetShapeColor(AAlphaColor, AShapeName);
    ARootShape.CancelUpdate;
  end;
end;

function TdxCompositeShape.GetHeightF: Single;
begin
  if FNeedNormalize then
    Result := cxRectHeight(FBounds)
  else
    if GetRootShape <> nil then
      Result := GetRootShape.Height
    else
      Result := 0;
end;

function TdxCompositeShape.GetWidthF: Single;
begin
  if FNeedNormalize then
    Result := cxRectWidth(FBounds)
  else
    if GetRootShape <> nil then
      Result := GetRootShape.Width
    else
      Result := 0;
end;

function TdxCompositeShape.GetBounds: TdxRectF;
begin
  if Empty then
    Result := dxRectF(cxNullRect)
  else
    Result := GetRootShape.ContentBounds;
end;

function TdxCompositeShape.GetRootShape: TdxCustomVectorShape;
begin
  if Empty then
    Result := nil
  else
    Result := Shapes[0];
end;

function TdxCompositeShape.GetShape(AIndex: Integer): TdxCustomVectorShape;
begin
  Result := FShapes[AIndex] as TdxCustomVectorShape;
end;

function TdxCompositeShape.GetShapeCount: Integer;
begin
  Result := FShapes.Count;
end;

function TdxCompositeShape.NeedDraw: Boolean;
begin
  Result := (Width <> 0) or (Height <> 0);
end;

procedure TdxCompositeShape.DoWorldTransform(AGraphics: TdxGPGraphics; const ADestRect: TdxRectF);
var
  AScale: TdxPointF;
  ATransform: TdxGPMatrix;
begin
  AScale.X := Max(cxRectWidth(ADestRect) / GetWidthF, 0.00001);
  AScale.Y := Max(cxRectHeight(ADestRect) / GetHeightF, 0.00001);
  ATransform := TdxGPMatrix.Create;
  try
    ATransform.Scale(AScale);
    if FNeedNormalize then
      ATransform.Translate(-FBounds.Left, -FBounds.Top);
    ATransform.Translate(ADestRect.Left / AScale.X, ADestRect.Top / AScale.Y);
    AGraphics.ModifyWorldTransform(ATransform);
  finally
    FreeAndNil(ATransform);
  end;
end;

procedure TdxCompositeShape.SaveStreamCopy(AStream: TStream);
begin
  FStream.Clear;
  FStream.CopyFrom(AStream, AStream.Size - AStream.Position);
end;

end.



