{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressNavBar                                            }
{                                                                    }
{           Copyright (c) 2002-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSNAVBAR AND ALL ACCOMPANYING    }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
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

unit dxNavBarOffice12Views;

{$I cxVer.inc}

interface

uses
  Windows, cxGraphics, SysUtils, Classes, cxClasses, cxControls, dxNavBar, dxNavBarSkinBasedViews,
  dxSkinsCore, dxSkinInfo, cxScrollBar, cxLookAndFeels, cxLookAndFeelPainters;

type

  { TdxNavBarOffice12PainterHelper }

  TdxNavBarOffice12PainterHelper = class(TdxNavBarSkinBasedPainterHelper)
  protected
    function GetFullSkinName: TdxSkinName; override;
    procedure PopulateSkinNames(AList: TStrings); override;
    procedure SetSkinName(AValue: TdxSkinName); override;
  end;

  { TdxNavBarOffice12NavPanePainter }

  TdxNavBarOffice12NavPanePainter = class(TdxNavBarSkinBasedNavPanePainter)
  protected
    class function GetSkinPainterHelperClass: TdxNavBarSkinBasedPainterHelperClass; override;
    // Scrollbar
    function GetcxScrollBarHelperClass: TcxControlScrollBarHelperClass; override;
    procedure DrawScrollBarPart(ACanvas: TcxCanvas; const R: TRect; APart: TcxScrollBarPart; AState: TcxButtonState);
    function ScrollBarMinimalThumbSize: Integer;

    function GetDefaultColorSchemeName: TdxSkinName; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property ColorSchemeName;
  end;

  { TdxNavBarOffice12ExplorerBarPainter }

  TdxNavBarOffice12ExplorerBarPainter = class(TdxNavBarSkinBasedExplorerBarPainter)
  protected
    class function GetSkinPainterHelperClass: TdxNavBarSkinBasedPainterHelperClass; override;
    // Scrollbar
    function GetcxScrollBarHelperClass: TcxControlScrollBarHelperClass; override;
    procedure DrawScrollBarPart(ACanvas: TcxCanvas; const R: TRect; APart: TcxScrollBarPart; AState: TcxButtonState);
    function ScrollBarMinimalThumbSize: Integer;

    function IsGroupCaptionButtonCompositeDraw(AGroupViewInfo: TdxNavBarGroupViewInfo): Boolean; override;
    function GetDefaultColorSchemeName: TdxSkinName; override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure DrawItemSelection(ALinkViewInfo: TdxNavBarLinkViewInfo); override;
  published
    property ColorSchemeName;
  end;

implementation

{$R *.res}

uses
  dxNavBarBase, dxNavBarViewsFact, dxNavBarConsts;

type

  { TdxNavBarcxOffice12NavPaneScrollBarPainter }

  TdxNavBarcxOffice12NavPaneScrollBarPainter = class(TcxScrollBarPainter)
  protected
    procedure DoDrawScrollBarPart(ACanvas: TcxCanvas; const R: TRect; APart: TcxScrollBarPart; AState: TcxButtonState); override;
    function FadingAvailable: Boolean; override;
    function GetMinThumbnailSize: Integer; override;
  public
    function IsButtonHotTrack: Boolean; override;
  end;

  { TdxNavBarcxOffice12NavPaneScrollBarHelper }

  TdxNavBarcxOffice12NavPaneScrollBarHelper = class(TcxControlScrollBarHelper)
  private
    function GetNavBarPainter: TdxNavBarOffice12NavPanePainter;
  protected
    function GetPainterClass: TcxScrollBarPainterClass; override;
  end;

  TdxNavBarcxOffice12ScrollBarPainter = class(TcxScrollBarPainter)
  protected
    procedure DoDrawScrollBarPart(ACanvas: TcxCanvas; const R: TRect; APart: TcxScrollBarPart; AState: TcxButtonState); override;
    function FadingAvailable: Boolean; override;
    function GetMinThumbnailSize: Integer; override;
  public
    function IsButtonHotTrack: Boolean; override;
  end;

  TdxNavBarcxOffice12ScrollBarHelper = class(TcxControlScrollBarHelper)
  private
    function GetNavBarPainter: TdxNavBarOffice12ExplorerBarPainter;
  protected
    function GetPainterClass: TcxScrollBarPainterClass; override;
  end;

const
  ColorSchemeCount = 3;
  ColorSchemeNames: array [0..ColorSchemeCount - 1] of TdxSkinName = ('Blue', 'Black', 'Silver');

var
  DefaultColorSchemeName: TdxSkinName;

{ TdxNavBarOffice12PainterHelper }

function TdxNavBarOffice12PainterHelper.GetFullSkinName: TdxSkinName;
begin
  Result := 'Office12_' + SkinName;
end;

procedure TdxNavBarOffice12PainterHelper.PopulateSkinNames(AList: TStrings);
var
  I: Integer;
begin
  for I := 0 to ColorSchemeCount - 1 do
    AList.Add(ColorSchemeNames[I]);
end;

procedure TdxNavBarOffice12PainterHelper.SetSkinName(AValue: TdxSkinName);
begin
  inherited;
  DefaultColorSchemeName := AValue;
end;

{ TdxNavBarcxOffice12ScrollBarPainter }

procedure TdxNavBarcxOffice12NavPaneScrollBarPainter.DoDrawScrollBarPart(
  ACanvas: TcxCanvas; const R: TRect; APart: TcxScrollBarPart;
  AState: TcxButtonState);
var
  ANavBarPainter: TdxNavBarOffice12NavPanePainter;
begin
  ANavBarPainter := (ScrollBar as TdxNavBarcxOffice12NavPaneScrollBarHelper).GetNavBarPainter;
  if ANavBarPainter <> nil then
    ANavBarPainter.DrawScrollBarPart(ACanvas, R, APart, AState)
  else
    inherited;
end;

function TdxNavBarcxOffice12NavPaneScrollBarPainter.FadingAvailable: Boolean;
begin
  Result := True;
end;

function TdxNavBarcxOffice12NavPaneScrollBarPainter.GetMinThumbnailSize: Integer;
var
  ANavBarPainter: TdxNavBarOffice12NavPanePainter;
begin
  ANavBarPainter := (ScrollBar as TdxNavBarcxOffice12NavPaneScrollBarHelper).GetNavBarPainter;
  if ANavBarPainter <> nil then
    Result := ANavBarPainter.ScrollBarMinimalThumbSize
  else
    Result := inherited GetMinThumbnailSize;
end;

function TdxNavBarcxOffice12NavPaneScrollBarPainter.IsButtonHotTrack: Boolean;
begin
  Result := True;
end;

{ TdxNavBarcxOffice12NavPaneScrollBarHelper }

function TdxNavBarcxOffice12NavPaneScrollBarHelper.GetNavBarPainter: TdxNavBarOffice12NavPanePainter;
var
  ANavBar: TdxCustomNavBar;
begin
  ANavBar := Owner.GetControl as TdxCustomNavBar;
  Result := ANavBar.Painter as TdxNavBarOffice12NavPanePainter;
end;

function TdxNavBarcxOffice12NavPaneScrollBarHelper.GetPainterClass: TcxScrollBarPainterClass;
begin
   Result := TdxNavBarcxOffice12NavPaneScrollBarPainter;
end;

{ TdxNavBarOffice12NavPanePainter }

procedure TdxNavBarOffice12NavPanePainter.Assign(Source: TPersistent);
begin
  if Source is TdxNavBarOffice12NavPanePainter then
    ColorSchemeName := TdxNavBarOffice12NavPanePainter(Source).ColorSchemeName
  else
    inherited;
end;

class function TdxNavBarOffice12NavPanePainter.GetSkinPainterHelperClass: TdxNavBarSkinBasedPainterHelperClass;
begin
  Result := TdxNavBarOffice12PainterHelper;
end;

procedure TdxNavBarOffice12NavPanePainter.DrawScrollBarPart(ACanvas: TcxCanvas;
  const R: TRect; APart: TcxScrollBarPart; AState: TcxButtonState);
const
  ButtonState2SkinState: array[TcxButtonState] of TdxSkinElementState =
   (esNormal, esNormal, esHot, esPressed, esDisabled);
var
  AInfo: TdxSkinScrollInfo;
begin
  if FSkinBasedPainterHelper = nil then
    Exit;
  AInfo := FSkinBasedPainterHelper.NavBarScrollBarElements(False, APart);
  if (AInfo <> nil) and (AInfo.Element <> nil) then
    AInfo.Element.Draw(ACanvas.Handle, R, ScaleFactor, AInfo.ImageIndex, ButtonState2SkinState[AState]);
end;

function TdxNavBarOffice12NavPanePainter.GetcxScrollBarHelperClass: TcxControlScrollBarHelperClass;
begin
  Result := TdxNavBarcxOffice12NavPaneScrollBarHelper;
end;

function TdxNavBarOffice12NavPanePainter.ScrollBarMinimalThumbSize: Integer;
var
  AInfo: TdxSkinScrollInfo;
begin
  Result := 0;
  if FSkinBasedPainterHelper = nil then
    Exit;
  AInfo := FSkinBasedPainterHelper.NavBarScrollBarElements(False, sbpThumbnail);
  if (AInfo <> nil) and (AInfo.Element <> nil) then
    Result := AInfo.Element.Size.cy;
end;

function TdxNavBarOffice12NavPanePainter.GetDefaultColorSchemeName: TdxSkinName;
begin
  Result := DefaultColorSchemeName;
end;

{ TdxNavBarcxOffice12ScrollBarPainter }

procedure TdxNavBarcxOffice12ScrollBarPainter.DoDrawScrollBarPart(
  ACanvas: TcxCanvas; const R: TRect; APart: TcxScrollBarPart;
  AState: TcxButtonState);
var
  ANavBarPainter: TdxNavBarOffice12ExplorerBarPainter;
begin
  ANavBarPainter := (ScrollBar as TdxNavBarcxOffice12ScrollBarHelper).GetNavBarPainter;
  if ANavBarPainter <> nil then
    ANavBarPainter.DrawScrollBarPart(ACanvas, R, APart, AState)
  else
    inherited;
end;

function TdxNavBarcxOffice12ScrollBarPainter.FadingAvailable: Boolean;
begin
  Result := True;
end;

function TdxNavBarcxOffice12ScrollBarPainter.GetMinThumbnailSize: Integer;
var
  ANavBarPainter: TdxNavBarOffice12ExplorerBarPainter;
begin
  ANavBarPainter := (ScrollBar as TdxNavBarcxOffice12ScrollBarHelper).GetNavBarPainter;
  if ANavBarPainter <> nil then
    Result := ANavBarPainter.ScrollBarMinimalThumbSize
  else
    Result := inherited GetMinThumbnailSize;
end;

function TdxNavBarcxOffice12ScrollBarPainter.IsButtonHotTrack: Boolean;
begin
  Result := True;
end;

{ TdxNavBarcxOffice12ScrollBarHelper }

function TdxNavBarcxOffice12ScrollBarHelper.GetNavBarPainter: TdxNavBarOffice12ExplorerBarPainter;
var
  ANavBar: TdxCustomNavBar;
begin
  ANavBar := Owner.GetControl as TdxCustomNavBar;
  Result := ANavBar.Painter as TdxNavBarOffice12ExplorerBarPainter;
end;

function TdxNavBarcxOffice12ScrollBarHelper.GetPainterClass: TcxScrollBarPainterClass;
begin
   Result := TdxNavBarcxOffice12ScrollBarPainter;
end;

{ TdxNavBarOffice12ExplorerBarPainter }

procedure TdxNavBarOffice12ExplorerBarPainter.Assign(Source: TPersistent);
begin
  if Source is TdxNavBarOffice12ExplorerBarPainter then
    ColorSchemeName := TdxNavBarOffice12ExplorerBarPainter(Source).ColorSchemeName
  else
    inherited;
end;

procedure TdxNavBarOffice12ExplorerBarPainter.DrawItemSelection(ALinkViewInfo: TdxNavBarLinkViewInfo);
begin
end;

class function TdxNavBarOffice12ExplorerBarPainter.GetSkinPainterHelperClass: TdxNavBarSkinBasedPainterHelperClass;
begin
  Result := TdxNavBarOffice12PainterHelper;
end;

function TdxNavBarOffice12ExplorerBarPainter.GetcxScrollBarHelperClass: TcxControlScrollBarHelperClass;
begin
  Result := TdxNavBarcxOffice12ScrollBarHelper;
end;

procedure TdxNavBarOffice12ExplorerBarPainter.DrawScrollBarPart(
  ACanvas: TcxCanvas; const R: TRect; APart: TcxScrollBarPart; AState: TcxButtonState);
const
  ButtonState2SkinState: array[TcxButtonState] of TdxSkinElementState =
   (esNormal, esNormal, esHot, esPressed, esDisabled);
var
  AInfo: TdxSkinScrollInfo;
begin
  if FSkinBasedPainterHelper = nil then
    Exit;
  AInfo := FSkinBasedPainterHelper.NavBarScrollBarElements(False, APart);
  if (AInfo <> nil) and (AInfo.Element <> nil) then
    AInfo.Element.Draw(ACanvas.Handle, R, ScaleFactor, AInfo.ImageIndex, ButtonState2SkinState[AState]);
end;

function TdxNavBarOffice12ExplorerBarPainter.ScrollBarMinimalThumbSize: Integer;
var
  AInfo: TdxSkinScrollInfo;
begin
  Result := 0;
  if FSkinBasedPainterHelper = nil then
    Exit;
  AInfo := FSkinBasedPainterHelper.NavBarScrollBarElements(False, sbpThumbnail);
  if (AInfo <> nil) and (AInfo.Element <> nil) then
    Result := AInfo.Element.Size.cy;
end;

function TdxNavBarOffice12ExplorerBarPainter.IsGroupCaptionButtonCompositeDraw(
  AGroupViewInfo: TdxNavBarGroupViewInfo): Boolean;
begin
  Result := (ViewInfo.GroupCount > 0) and (ViewInfo.Groups[0] = AGroupViewInfo);
end;

function TdxNavBarOffice12ExplorerBarPainter.GetDefaultColorSchemeName: TdxSkinName;
begin
  Result := DefaultColorSchemeName;
end;

initialization
  DefaultColorSchemeName := ColorSchemeNames[0];
  RegisterView(dxNavBarOffice12NavigatorPaneView, 'Office12NavigationPaneView', TdxNavBarOffice12NavPanePainter);
  RegisterView(dxNavBarOffice12ExplorerBarView, 'Office12ExplorerBarView', TdxNavBarOffice12ExplorerBarPainter);

finalization
  UnRegisterView(dxNavBarOffice12NavigatorPaneView);
  UnRegisterView(dxNavBarOffice12ExplorerBarView);
end.

