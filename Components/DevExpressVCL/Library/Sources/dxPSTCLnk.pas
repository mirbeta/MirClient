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

unit dxPSTCLnk;

interface

{$I cxVer.inc}

uses
{$IFDEF DELPHI16}
  VCLTee.Chart,
{$ELSE}
  Chart,
{$ENDIF}
  Classes, Graphics, dxPSCore, dxPSGraphicLnk;

type

  TdxPSTeeChartReportLinkGetGraphicClassEvent = procedure(Sender: TBasedxReportLink;
    var AGraphicClass: TGraphicClass) of object;
  TdxPSTeeChartReportLinkCreateGraphicEvent = procedure(Sender: TBasedxReportLink;
    var AGraphic: TGraphic) of object;

  TCustomdxTeeChartReportLink = class(TCustomdxGraphicReportLink)
  private
    FGraphicClass: TGraphicClass;
    FIsGraphicClassAssigned: Boolean;
    FOnCreateGraphic: TdxPSTeeChartReportLinkCreateGraphicEvent;
    FOnGetGraphicClass: TdxPSTeeChartReportLinkGetGraphicClassEvent;

    function CheckBitmapFormat(ABitmap: TBitmap): TBitmap;
    function GetChart: TCustomChart;
    function GetGraphicClassName: string;
    function GetGraphicClassValue: TGraphicClass;
    function IsGraphicClassNameStored: Boolean;
    procedure SetGraphicClassName(const Value: string);
    procedure SetGraphicClassValue(Value: TGraphicClass);
  protected
    procedure ConstructReport(AReportCells: TdxReportCells); override;

    procedure InternalRestoreDefaults; override;

    function GetGraphic: TGraphic; override;
    function GetGraphicClass: TGraphicClass; override;
    function GetGraphicHeight: Integer; override;
    function GetGraphicWidth: Integer; override;

    procedure CreateGraphicStandard(var AGraphic: TGraphic); virtual;
    function DoCreateGraphic(var AGraphic: TGraphic): Boolean; dynamic;
    procedure DoGetGraphicClass(var AGraphicClass: TGraphicClass); dynamic;

    property Chart: TCustomChart read GetChart;
    property GraphicClass: TGraphicClass read GetGraphicClassValue write SetGraphicClassValue;
    property GraphicClassName: string read GetGraphicClassName write SetGraphicClassName stored IsGraphicClassNameStored;
    property OnCreateGraphic: TdxPSTeeChartReportLinkCreateGraphicEvent read FOnCreateGraphic write FOnCreateGraphic;
    property OnGetGraphicClass: TdxPSTeeChartReportLinkGetGraphicClassEvent read FOnGetGraphicClass write FOnGetGraphicClass;
  public
    procedure Assign(Source: TPersistent); override;
    class function Aggregable: Boolean; override;

    function DefaultGraphicClass: TGraphicClass; virtual;
  end;

  TdxTeeChartReportLink = class(TCustomdxTeeChartReportLink)
  private
    function GetChart: TChart;
  public
    property Chart: TChart read GetChart;
    property GraphicClass;
  published
    property GraphicClassName;
    property OnCreateGraphic;
    property OnGetGraphicClass;
  end;

implementation

uses
 {$IFDEF USEJPEGIMAGE}
  Jpeg,
 {$ENDIF}
  SysUtils, dxPSUtl, dxCoreGraphics, cxGraphics;

procedure TCustomdxTeeChartReportLink.Assign(Source: TPersistent);
begin
  if Source is TCustomdxTeeChartReportLink then
    with TCustomdxTeeChartReportLink(Source) do
    begin
      Self.GraphicClass := GraphicClass;
      Self.FIsGraphicClassAssigned := FIsGraphicClassAssigned;
    end;
  inherited;
end;

class function TCustomdxTeeChartReportLink.Aggregable: Boolean;
begin
  Result := True;
end;

function TCustomdxTeeChartReportLink.DefaultGraphicClass: TGraphicClass;
begin
  Result := TBitmap;
end;

procedure TCustomdxTeeChartReportLink.ConstructReport(
  AReportCells: TdxReportCells);
var
  Cell: TdxReportCell;
  ControllerIntf: IdxReportLinkController;
begin
  inherited ConstructReport(AReportCells);
  if (AReportCells.Count > 0) and Aggregable and
    SysUtils.Supports(TObject(Controller), IdxReportLinkController, ControllerIntf) then
  begin
    Cell := AReportCells.Cells[AReportCells.Count - 1];
    if (Cell.DataItemCount > 0) and
       (Cell.DataItems[Cell.DataItemCount - 1] is TdxReportCellGraphic) then
    begin
      TdxReportCellGraphic(Cell.DataItems[Cell.DataItemCount - 1]).DrawMode := gdmStretch;
    end;
  end;
end;

procedure TCustomdxTeeChartReportLink.InternalRestoreDefaults;
begin
  inherited;
  FIsGraphicClassAssigned := False;
end;

function TCustomdxTeeChartReportLink.DoCreateGraphic(var AGraphic: TGraphic): Boolean;
begin
  AGraphic := nil;
  if Assigned(FOnCreateGraphic) then FOnCreateGraphic(Self, AGraphic);
  Result := AGraphic <> nil;
end;

function TCustomdxTeeChartReportLink.GetGraphic: TGraphic;
begin
  if not DoCreateGraphic(Result) then CreateGraphicStandard(Result)
end;

function TCustomdxTeeChartReportLink.GetGraphicClass: TGraphicClass;
begin
  Result := GraphicClass;
  DoGetGraphicClass(Result);
  if Result = nil then
    Result := DefaultGraphicClass;
end;

function TCustomdxTeeChartReportLink.GetGraphicHeight: Integer;
var
  ControllerIntf: IdxReportLinkController;
begin
  if Chart <> nil then
  begin
    Result := Chart.Height;
    if Aggregable and SysUtils.Supports(TObject(Controller), IdxReportLinkController, ControllerIntf) then
    begin
      with ControllerIntf.GetControlSiteBounds(Chart) do
        Result := Bottom - Top - 1
    end;
  end
  else
    Result := inherited GetGraphicHeight;
end;

function TCustomdxTeeChartReportLink.GetGraphicWidth: Integer;
var
  ControllerIntf: IdxReportLinkController;
begin
  if Chart <> nil then
  begin
    Result := Chart.Width;
    if Aggregable and SysUtils.Supports(TObject(Controller), IdxReportLinkController, ControllerIntf) then
    begin
      with ControllerIntf.GetControlSiteBounds(Chart) do
        Result := Right - Left - 1
    end;
  end
  else
    Result := inherited GetGraphicWidth;
end;

procedure TCustomdxTeeChartReportLink.CreateGraphicStandard(var AGraphic: TGraphic);
var
  OwnGraphicClass: TGraphicClass;
 {$IFDEF USEJPEGIMAGE}
  B: TBitmap;
 {$ENDIF}
begin
  OwnGraphicClass := GraphicClass;

  if OwnGraphicClass.InheritsFrom(TBitmap) then
    AGraphic := CheckBitmapFormat(Chart.TeeCreateBitmap(Color, Chart.ClientRect))
  else
   {$IFDEF USEJPEGIMAGE}
    if OwnGraphicClass.InheritsFrom(TJPEGImage) then
    begin
      B := CheckBitmapFormat(Chart.TeeCreateBitmap(Color, Chart.ClientRect));
      try
        AGraphic := TJPEGImage.Create;
        try
          AGraphic.Assign(B);
        except
          FreeAndNil(AGraphic);
          raise;
        end;
      finally
        B.Free;
      end;
    end
    else
   {$ENDIF}
      AGraphic := Chart.TeeCreateMetafile(True, Chart.ClientRect);
end;

procedure TCustomdxTeeChartReportLink.DoGetGraphicClass(var AGraphicClass: TGraphicClass);
begin
  if Assigned(FOnGetGraphicClass) then FOnGetGraphicClass(Self, AGraphicClass);
end;

function TCustomdxTeeChartReportLink.CheckBitmapFormat(ABitmap: TBitmap): TBitmap;
begin
  if cxGetBitmapPixelFormat(ABitmap) < 24 then
  begin
    Result := cxCreateBitmap(ABitmap.Width, ABitmap.Height);
    Result.Canvas.Draw(0, 0, ABitmap);
    ABitmap.Free;
  end
  else
    Result := ABitmap;
end;

function TCustomdxTeeChartReportLink.GetChart: TCustomChart;
begin
  Result := inherited Component as TCustomChart;
end;

function TCustomdxTeeChartReportLink.GetGraphicClassName: string;
begin
  Result := GraphicClass.ClassName;
end;

function TCustomdxTeeChartReportLink.GetGraphicClassValue: TGraphicClass;
begin
  if FIsGraphicClassAssigned then
    Result := FGraphicClass
  else
    Result := DefaultGraphicClass;
end;

function TCustomdxTeeChartReportLink.IsGraphicClassNameStored: Boolean;
begin
  Result := FIsGraphicClassAssigned and (GraphicClass <> DefaultGraphicClass);
end;

procedure TCustomdxTeeChartReportLink.SetGraphicClassName(const Value: string);
var
  ValueClass: TPersistentClass;
begin
  ValueClass := GetClass(Value);
  if (ValueClass <> nil) and ValueClass.InheritsFrom(TGraphic) then
    GraphicClass := TGraphicClass(ValueClass);
end;

procedure TCustomdxTeeChartReportLink.SetGraphicClassValue(Value: TGraphicClass);
begin
  if FGraphicClass <> Value then
  begin
    FGraphicClass := Value;
    FIsGraphicClassAssigned := True;
    LinkModified(True);
  end;
end;

{ TdxTeeChartReportLink }

function TdxTeeChartReportLink.GetChart: TChart;
begin
  Result := inherited Chart as TChart;
end;

initialization
  dxPSRegisterReportLink(TdxTeeChartReportLink, TChart, nil);

finalization
  dxPSUnregisterReportLink(TdxTeeChartReportLink, TChart, nil);

end.
