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

unit dxPSGraphicLnk;

interface

{$I cxVer.inc}

uses
  Types, Classes, Windows, Graphics, ExtCtrls, dxCore, dxPSGlbl, dxPSCore, dxBkgnd;

type

  { TCustomdxGraphicReportLink }

  TCustomdxGraphicReportLink = class(TBasedxReportLink)
  private
    FBorderColor: TColor;
    FCenter: Boolean;
    FDrawBorder: Boolean;
    FProportional: Boolean;
    FStretch: Boolean;
    FTransparentColor: TColor;
    procedure SetBorderColor(Value: TColor);
    procedure SetCenter(Value: Boolean);
    procedure SetDrawBorder(Value: Boolean);
    procedure SetProportional(Value: Boolean);
    procedure SetStretch(Value: Boolean);
    procedure SetTransparentColor(Value: TColor);
  protected
    function GetAlwaysBufferedGraphics: Boolean; override;
    function GetCriticalSize(AReportCells: TdxReportCells): Integer; override;
    procedure ConstructReport(AReportCells: TdxReportCells); override;
    procedure InternalRestoreDefaults; override;

    function GetGraphic: TGraphic; virtual;
    function GetGraphicClass: TGraphicClass; virtual;
    function GetGraphicHeight: Integer; virtual;
    function GetGraphicWidth: Integer; virtual;
    function GetDrawMode: TdxGraphicDrawMode; virtual;
    procedure InitializeGraphicItem(AnItem: TdxReportCellGraphic); virtual;

    property BorderColor: TColor read FBorderColor write SetBorderColor default clBlack;
    property Center: Boolean read FCenter write SetCenter default False;
    property DrawBorder: Boolean read FDrawBorder write SetDrawBorder default False;
    property DrawMode: TdxGraphicDrawMode read GetDrawMode;
    property GraphicHeight: Integer read GetGraphicHeight;
    property GraphicWidth: Integer read GetGraphicWidth;
    property Proportional: Boolean read FProportional write SetProportional default False;
    property Stretch: Boolean read FStretch write SetStretch default False;
    property TransparentColor: TColor read FTransparentColor write SetTransparentColor default clWindow;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
  end;

  { TCustomdxPictureReportLink }

  TCustomdxPictureReportLink = class(TCustomdxGraphicReportLink)
  protected
    function GetGraphic: TGraphic; override;
    function GetGraphicHeight: Integer; override;
    function GetGraphicWidth: Integer; override;
    function GetPicture: TPicture; virtual;
    procedure InitializeGraphicItem(AnItem: TdxReportCellGraphic); override;
    function IsPictureEmpty(APicture: TPicture): Boolean;

    property Picture: TPicture read GetPicture;
  public
    function DataProviderPresent: Boolean; override;
  end;

  { TdxPictureReportLink }

  TdxPictureReportLink = class(TCustomdxPictureReportLink)
  private
    FPicture: TPicture;
    procedure SetPicture(Value: TPicture);
    procedure PictureChanged(Sender: TObject);
  protected
    function GetPicture: TPicture; override;
    procedure SetComponent(Value: TComponent); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property BorderColor;
    property DrawBorder;
    property Picture write SetPicture;
    property Transparent;
    property TransparentColor;
  end;

implementation

uses
  SysUtils, cxClasses, dxPSUtl, dxPSRes, cxGeometry, cxGraphics;

const
  CellSidesMap: array[Boolean] of TdxCellSides = ([], csAll);

{ TCustomdxGraphicReportLink }

constructor TCustomdxGraphicReportLink.Create(AOwner: TComponent);
begin
  inherited;
  FBorderColor := dxDefaultGridLineColor;
  FCenter := False;
  FDrawBorder := False;
  FProportional := False;
  FStretch := False;
  FTransparentColor := clWindow;
end;

procedure TCustomdxGraphicReportLink.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TCustomdxGraphicReportLink then
    with TCustomdxGraphicReportLink(Source) do
    begin
      Self.BorderColor := BorderColor;
      Self.Center := Center;
      Self.DrawBorder := DrawBorder;
      Self.Proportional := Proportional;
      Self.Stretch := Stretch;
      Self.TransparentColor := TransparentColor;
    end;
end;

function TCustomdxGraphicReportLink.GetAlwaysBufferedGraphics: Boolean;
begin
  Result := False;
end;

function TCustomdxGraphicReportLink.GetCriticalSize(AReportCells: TdxReportCells): Integer;
begin
  with AReportCells.BoundsRect do
  begin
    Result := Right - Left;
    if Result < Bottom - Top then
      Result := Bottom - Top;
  end;
end;

procedure TCustomdxGraphicReportLink.ConstructReport(AReportCells: TdxReportCells);
var
  Graphic: TGraphic;
  Cell: TdxReportCell;
  Data: TdxReportCellGraphic;
begin
  inherited;
  Graphic := GetGraphic;
  if Graphic <> nil then
  try
    AReportCells.Cells.Color := Color;
    AReportCells.Cells.CellSides := CellSidesMap[DrawBorder];
    AReportCells.BorderColor := BorderColor;

    Cell := TdxReportCell.Create(AReportCells.Cells);
    Cell.CellSides := [];

    Data := TdxReportCellGraphic.Create(Cell);
    Data.BoundsRect := Rect(0, 0, GraphicWidth, GraphicHeight);
    Data.Image := Graphic;
    Data.Stretch := True;
    InitializeGraphicItem(Data);

    Cell.BoundsRect := Data.BoundsRect;
    AReportCells.Cells.BoundsRect := Cell.BoundsRect;
    AReportCells.DoProgress(100);
  finally
    Graphic.Free;
  end;
end;

procedure TCustomdxGraphicReportLink.InternalRestoreDefaults;
begin
  inherited;
  BorderColor := dxDefaultGridLineColor;
  Center := False;
  DrawBorder := False;
  Proportional := False;
  Stretch := False;
  TransparentColor := clWindow;
end;

function TCustomdxGraphicReportLink.GetGraphic: TGraphic;
begin
  Result := nil;
end;

function TCustomdxGraphicReportLink.GetGraphicClass: TGraphicClass;
begin
  Result := nil;
end;

function TCustomdxGraphicReportLink.GetGraphicHeight: Integer;
var
  Graphic: TGraphic;
begin
  Graphic := GetGraphic;
  if Graphic <> nil then
  try
    Result := Graphic.Height;
  finally
    Graphic.Free;
  end
  else
    Result := 0;
end;

function TCustomdxGraphicReportLink.GetGraphicWidth: Integer;
var
  Graphic: TGraphic;
begin
  Graphic := GetGraphic;
  if Graphic <> nil then
    try
      Result := Graphic.Width;
    finally
      Graphic.Free;
    end
  else
    Result := 0;
end;

function TCustomdxGraphicReportLink.GetDrawMode: TdxGraphicDrawMode;
begin
  Result := dxGraphicDrawModeMap[Stretch, Proportional, Center];
end;

procedure TCustomdxGraphicReportLink.InitializeGraphicItem(AnItem: TdxReportCellGraphic);
const
  ImageBufferingMap: array[Boolean] of TdxCellImageBuffering = (cibAlways, cibNone);
begin
  AnItem.Color := Color;
  AnItem.CellSides := CellSidesMap[DrawBorder];
  AnItem.ImageBuffering := ImageBufferingMap[(GetGraphicClass <> nil) and GetGraphicClass.InheritsFrom(TMetafile)];
  AnItem.ImageTransparent := Transparent;
  AnItem.Transparent := True;
  AnItem.ImageTransparentColor := TransparentColor;
end;

procedure TCustomdxGraphicReportLink.SetBorderColor(Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    LinkModified(True);
  end;
end;

procedure TCustomdxGraphicReportLink.SetCenter(Value: Boolean);
begin
  if FCenter <> Value then
  begin
    FCenter := Value;
    LinkModified(True);
  end;
end;

procedure TCustomdxGraphicReportLink.SetDrawBorder(Value: Boolean);
begin
  if FDrawBorder <> Value then
  begin
    FDrawBorder := Value;
    LinkModified(True);
  end;
end;

procedure TCustomdxGraphicReportLink.SetProportional(Value: Boolean);
begin
  if FProportional <> Value then
  begin
    FProportional := Value;
    LinkModified(True);
  end;
end;

procedure TCustomdxGraphicReportLink.SetStretch(Value: Boolean);
begin
  if FStretch <> Value then
  begin
    FStretch := Value;
    LinkModified(True);
  end;
end;

procedure TCustomdxGraphicReportLink.SetTransparentColor(Value: TColor);
begin
  if FTransparentColor <> Value then
  begin
    FTransparentColor := Value;
    LinkModified(True);
  end;
end;

{ TCustomdxPictureReportLink }

function TCustomdxPictureReportLink.DataProviderPresent: Boolean;
begin
  if DataSource = rldsComponent then
    Result := not IsPictureEmpty(Picture)
  else
    Result := inherited DataProviderPresent;
end;

function TCustomdxPictureReportLink.GetGraphic: TGraphic;
begin
  if DataProviderPresent then
  begin
    Result := TGraphicClass(Picture.Graphic.ClassType).Create;
    Result.Assign(Picture.Graphic);
  end
  else
    Result := inherited GetGraphic;
end;

function TCustomdxPictureReportLink.GetGraphicHeight: Integer;
begin
  if Picture <> nil then
    Result := Picture.Height
  else
    Result := inherited GetGraphicHeight;
end;

function TCustomdxPictureReportLink.GetGraphicWidth: Integer;
begin
  if Picture <> nil then
    Result := Picture.Width
  else
    Result := inherited GetGraphicWidth;
end;

function TCustomdxPictureReportLink.GetPicture: TPicture;
begin
  Result := nil;
end;

procedure TCustomdxPictureReportLink.InitializeGraphicItem(AnItem: TdxReportCellGraphic);
begin
  inherited InitializeGraphicItem(AnItem);
  AnItem.Proportional := Proportional;
  AnItem.Stretch := Stretch;
  AnItem.Center := Center;
end;

function TCustomdxPictureReportLink.IsPictureEmpty(APicture: TPicture): Boolean;
begin
  Result := (APicture = nil) or (APicture.Graphic = nil) or APicture.Graphic.Empty;
end;

{ TdxPictureReportLink }

constructor TdxPictureReportLink.Create(AOwner: TComponent);
begin
  inherited;
  ReportDocument.Description := cxGetResourceString(@sdxTPicture);
  Transparent := False;
  FPicture := TPicture.Create;
  FPicture.OnChange := PictureChanged;
  LinkModified(False);
end;

destructor TdxPictureReportLink.Destroy;
begin
  FreeAndNil(FPicture);
  inherited;
end;

procedure TdxPictureReportLink.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TdxPictureReportLink then
    Picture := TdxPictureReportLink(Source).Picture;
end;

function TdxPictureReportLink.GetPicture: TPicture;
begin
  Result := FPicture;
end;

procedure TdxPictureReportLink.SetPicture(Value: TPicture);
begin
  Picture.Assign(Value);
end;

procedure TdxPictureReportLink.SetComponent(Value: TComponent);
begin
end;

procedure TdxPictureReportLink.PictureChanged(Sender: TObject);
begin
  LinkModified(True);
  DesignerModified;
  DesignerUpdate(False);
end;

initialization
  dxPSRegisterReportLink(TdxPictureReportLink, nil, nil);

finalization
  dxPSUnregisterReportLink(TdxPictureReportLink, nil, nil);

end.
