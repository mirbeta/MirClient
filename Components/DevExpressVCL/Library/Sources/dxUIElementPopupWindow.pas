{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEditors                                           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL                }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit dxUIElementPopupWindow;

{$I cxVer.inc}

interface

uses
  Controls, Types, cxControls, cxContainer, dxIncrementalFiltering, cxLookAndFeels;

type
  TdxUIElementPopupWindow = class;

  { IdxUIElementPopupWindowOwner }
  // for internal use
  IdxUIElementPopupWindowOwner = interface
  ['{ACF62D23-6871-4735-A4CE-3B0888DB8FC3}']
    function ClosePopupWhenSetNil: Boolean;
    procedure InitPopup(APopup: TdxUIElementPopupWindow);
    procedure PopupClosed;
  end;

  { TdxUIElementPopupWindowViewInfo }

  TdxUIElementPopupWindowViewInfo = class(TdxCustomIncrementalFilteringContainerViewInfo);

  { TdxUIElementPopupWindow }

  TdxUIElementPopupWindow = class(TdxCustomIncrementalFilteringPopupWindow)
  private
    FOwner: TObject;
    FOwnerIntf: IdxUIElementPopupWindowOwner;

    function GetClientMinWidth: Integer;
    function GetLookAndFeel: TcxLookAndFeel;
    function GetViewInfo: TdxUIElementPopupWindowViewInfo;
    procedure SetLookAndFeel(ALookAndFeel: TcxLookAndFeel);
    procedure SetOwner(AValue: TObject);
  protected
    procedure InitPopup; override;
    procedure OwnerChanged; virtual;
    procedure Paint; override;
    procedure UpdateInnerControlsHeight(var AClientHeight: Integer); virtual;
    procedure VisibleChanged; override;

    property OwnerIntf: IdxUIElementPopupWindowOwner read FOwnerIntf;
  public
    constructor Create(AOwnerControl: TWinControl); override;

    procedure CorrectBoundsWithDesktopWorkArea(var APosition: TPoint; var ASize: TSize); override;
    function GetViewInfoClass: TcxContainerViewInfoClass; override;
    procedure Popup; reintroduce; virtual;

    property BorderWidth;
    property ClientMinWidth: Integer read GetClientMinWidth;
    property LookAndFeel: TcxLookAndFeel read GetLookAndFeel write SetLookAndFeel;
    property Owner: TObject read FOwner write SetOwner;
    property ViewInfo: TdxUIElementPopupWindowViewInfo read GetViewInfo;
  end;

implementation

uses
  SysUtils, Graphics, cxGeometry;

{ TdxUIElementPopupWindow }

constructor TdxUIElementPopupWindow.Create(AOwnerControl: TWinControl);
begin
  inherited Create(AOwnerControl);
  Color := clWindow;
  IsTopMost := True;
end;

procedure TdxUIElementPopupWindow.CorrectBoundsWithDesktopWorkArea(var APosition: TPoint; var ASize: TSize);
var
  ADesktopWorkArea: TRect;
  AHeight, AClientHeight: Integer;
begin
  AHeight := ASize.cy;
  ADesktopWorkArea := GetDesktopWorkArea(APosition);
  with APosition do
  begin
    if Y < ADesktopWorkArea.Top then
    begin
      AHeight := AHeight + Y - ADesktopWorkArea.Top;
      Y := ADesktopWorkArea.Top;
    end;
    if AHeight > ADesktopWorkArea.Bottom - Y then
      AHeight := ADesktopWorkArea.Bottom - Y;
    if AHeight < ASize.cy then
    begin
      AClientHeight := AHeight - NCHeight;
      UpdateInnerControlsHeight(AClientHeight);
      ASize.cy := AClientHeight + NCHeight;
    end;
    if (AHeight <> ASize.cy) and (Y = ADesktopWorkArea.Top) then
      Inc(APosition.Y, AHeight - ASize.cy);
  end;
end;

function TdxUIElementPopupWindow.GetViewInfoClass: TcxContainerViewInfoClass;
begin
  Result := TdxUIElementPopupWindowViewInfo;
end;

procedure TdxUIElementPopupWindow.Popup;
begin
  SetCaptureControl(nil);
  inherited Popup(FindNextControl(nil, True, True, False));
end;

procedure TdxUIElementPopupWindow.InitPopup;
begin
  inherited InitPopup;
  if OwnerIntf <> nil then
    OwnerIntf.InitPopup(Self);
end;

procedure TdxUIElementPopupWindow.OwnerChanged;
begin
  Supports(Owner, IdxUIElementPopupWindowOwner, FOwnerIntf);
end;

procedure TdxUIElementPopupWindow.Paint;
begin
  DrawFrame;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientBounds);
end;

procedure TdxUIElementPopupWindow.UpdateInnerControlsHeight(var AClientHeight: Integer);
begin
//do nothing
end;

procedure TdxUIElementPopupWindow.VisibleChanged;
begin
  inherited VisibleChanged;
  if not Visible and (OwnerIntf <> nil) then
    OwnerIntf.PopupClosed;
end;

function TdxUIElementPopupWindow.GetClientMinWidth: Integer;
begin
  Result := cxRectWidth(OwnerBounds) - NCWidth;
end;

function TdxUIElementPopupWindow.GetLookAndFeel: TcxLookAndFeel;
begin
  Result := Style.LookAndFeel;
end;

function TdxUIElementPopupWindow.GetViewInfo: TdxUIElementPopupWindowViewInfo;
begin
  Result := TdxUIElementPopupWindowViewInfo(inherited ViewInfo);
end;

procedure TdxUIElementPopupWindow.SetLookAndFeel(ALookAndFeel: TcxLookAndFeel);
begin
  Style.LookAndFeel.MasterLookAndFeel := ALookAndFeel;
  ViewInfo.UpdateStyle(Style);
end;

procedure TdxUIElementPopupWindow.SetOwner(AValue: TObject);
begin
  if Owner <> AValue then
  begin
    if (AValue = nil) and Visible and ((OwnerIntf = nil) or OwnerIntf.ClosePopupWhenSetNil) then
      CloseUp;
    FOwner := AValue;
    OwnerChanged;
  end;
end;

end.
