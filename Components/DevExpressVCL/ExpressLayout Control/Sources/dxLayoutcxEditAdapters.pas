{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressLayoutControl cxEditors adapters                  }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSLAYOUTCONTROL AND ALL          }
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

unit dxLayoutcxEditAdapters;

{$I cxVer.inc}

interface

uses
  Classes, Windows, cxControls, dxLayoutContainer, cxEdit, cxLookAndFeelPainters, cxGraphics, dxColorGallery;

type
  TdxLayoutcxEditAdapter = class(TdxCustomLayoutControlAdapter)
  private
    function GetControl: TcxCustomEdit;
    function GetControlStyle: TcxCustomEditStyle;
  protected
    function AllowCheckAutoSize: Boolean; override;
    procedure CombineRegion(const ARect: TRect); override;
    function GetControlAutoWidth: Boolean; override;
    function GetControlAutoHeight: Boolean; override;
    procedure SetControlAutoWidth(AValue: Boolean); override;
    procedure SetControlAutoHeight(AValue: Boolean); override;
    procedure Init; override;
    procedure InternalSetInitialSettings; override;
    function IsDefaultSkinAssigned: Boolean;

    property Control: TcxCustomEdit read GetControl;
    property ControlStyle: TcxCustomEditStyle read GetControlStyle;
  public
    procedure AfterInitialization; override;
    procedure BeforeInitialization; override;
  end;

  TdxLayoutdxColorGalleryAdapter = class(TdxCustomLayoutControlAdapter)
  private
    function GetControl: TdxColorGallery;
  protected
    function AllowCheckAutoSize: Boolean; override;
    function GetControlAutoWidth: Boolean; override;
    function GetControlAutoHeight: Boolean; override;

    property Control: TdxColorGallery read GetControl;
  end;

implementation

uses
  cxContainer, dxLayoutLookAndFeels, cxLookAndFeels, TypInfo;

type
  TcxCustomEditAccess = class(TcxCustomEdit);
  TcxCustomEditStyleAccess = class(TcxCustomEditStyle);
  TdxCustomLayoutLookAndFeelAccess = class(TdxCustomLayoutLookAndFeel);

{ TdxLayoutcxEditAdapter }

procedure TdxLayoutcxEditAdapter.AfterInitialization;
begin
  TcxCustomEditAccess(Control).EndRefreshContainer;
  inherited;
end;

procedure TdxLayoutcxEditAdapter.BeforeInitialization;
begin
  inherited;
  TcxCustomEditAccess(Control).BeginRefreshContainer;
end;

function TdxLayoutcxEditAdapter.GetControl: TcxCustomEdit;
begin
  Result := inherited Control as TcxCustomEdit;
end;

function TdxLayoutcxEditAdapter.GetControlStyle: TcxCustomEditStyle;
begin
  Result := TcxCustomEditAccess(Control).Style;
end;

function TdxLayoutcxEditAdapter.IsDefaultSkinAssigned: Boolean;
begin
  with ControlStyle.LookAndFeel do
    Result := (MasterLookAndFeel <> nil) and (MasterLookAndFeel.SkinName <> '');
end;

function TdxLayoutcxEditAdapter.AllowCheckAutoSize: Boolean;
begin
  Result := (Control <> nil) and IsPublishedProp(Control, 'AutoSize');
end;

procedure TdxLayoutcxEditAdapter.CombineRegion(const ARect: TRect);
var
  ARegion: TcxRegion;
  ARgn: TcxRegionHandle;
begin
  ARegion := TcxRegion.Create(ARect);
  try
    ARgn := TcxCustomEditAccess(Control).FNewWindowRegion;
    if ARgn <> 0 then
      ARegion.Combine(TcxRegion.Create(ARgn), roIntersect);
    TcxCustomEditAccess(Control).FNewWindowRegion := ARegion.Handle;
  finally
    ARegion.Handle := 0;
    ARegion.Free;
  end;
end;

function TdxLayoutcxEditAdapter.GetControlAutoWidth: Boolean;
begin
  Result := TcxCustomEditAccess(Control).IsAutoWidth;
end;

function TdxLayoutcxEditAdapter.GetControlAutoHeight: Boolean;
begin
  Result := TcxCustomEditAccess(Control).IsAutoHeight;
end;

procedure TdxLayoutcxEditAdapter.SetControlAutoWidth(AValue: Boolean);
begin
  TcxCustomEditAccess(Control).AutoWidth := AValue;
end;

procedure TdxLayoutcxEditAdapter.SetControlAutoHeight(AValue: Boolean);
begin
  TcxCustomEditAccess(Control).AutoHeight := AValue;
end;

procedure TdxLayoutcxEditAdapter.Init;
begin
  inherited;
  TcxCustomEditStyleAccess(ControlStyle).HotTrack := False;
end;

procedure TdxLayoutcxEditAdapter.InternalSetInitialSettings;
const
  BorderStyles: array[TdxLayoutBorderStyle] of TcxEditBorderStyle =
    (ebsNone, ebsSingle, ebsFlat, ebs3D);
  ButtonStyles: array[TdxLayoutBorderStyle] of TcxEditButtonStyle =
    (btsHotFlat, btsHotFlat, btsFlat, bts3D);
  PopupBorderStyles: array[TdxLayoutBorderStyle] of TcxEditPopupBorderStyle =
    (epbsSingle, epbsSingle, epbsFlat, epbsFrame3D);
var
  AControlStyle: TcxCustomEditStyleAccess;
  AItemBorderStyle: TdxLayoutBorderStyle;
begin
  inherited;
  AControlStyle := TcxCustomEditStyleAccess(ControlStyle);
  if TdxCustomLayoutLookAndFeelAccess(LayoutLookAndFeel).DoesCxLookAndFeelHavePriority then
    AControlStyle.AssignedValues := AControlStyle.AssignedValues - [svBorderColor, svBorderStyle, svButtonStyle, svPopupBorderStyle]
  else
  begin
    AItemBorderStyle := LayoutLookAndFeel.ItemOptions.ControlBorderStyle;
    if AControlStyle.DefaultBorderStyle <> cbsNone then
    begin
      AControlStyle.BorderColor := LayoutLookAndFeel.ItemOptions.GetControlBorderColor;
      AControlStyle.BorderStyle := BorderStyles[AItemBorderStyle];
    end;
    AControlStyle.ButtonStyle := ButtonStyles[AItemBorderStyle];
    AControlStyle.PopupBorderStyle := PopupBorderStyles[AItemBorderStyle];
  end;
end;

function TdxLayoutdxColorGalleryAdapter.AllowCheckAutoSize: Boolean;
begin
  Result := (Control <> nil) and IsPublishedProp(Control, 'AutoSizeMode');
end;

function TdxLayoutdxColorGalleryAdapter.GetControlAutoWidth: Boolean;
begin
  Result := Control.AutoSizeMode in [asAutoWidth, asAutoSize];
end;

function TdxLayoutdxColorGalleryAdapter.GetControlAutoHeight: Boolean;
begin
  Result := Control.AutoSizeMode in [asAutoHeight, asAutoSize];
end;

function TdxLayoutdxColorGalleryAdapter.GetControl: TdxColorGallery;
begin
  Result := inherited Control as TdxColorGallery;
end;

initialization
  RegisterClasses([TdxLayoutcxEditAdapter, TdxLayoutdxColorGalleryAdapter]);
  TdxLayoutcxEditAdapter.Register(TcxCustomEdit);
  TdxLayoutdxColorGalleryAdapter.Register(TdxColorGallery);

finalization
  TdxLayoutdxColorGalleryAdapter.Unregister(TdxColorGallery);
  TdxLayoutcxEditAdapter.Unregister(TcxCustomEdit);

end.
