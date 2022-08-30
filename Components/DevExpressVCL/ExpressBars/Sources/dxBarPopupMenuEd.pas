{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressBars PopupMenu editor                             }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSBARS AND ALL ACCOMPANYING VCL  }
{   CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.                  }
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

unit dxBarPopupMenuEd;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, dxCore, dxBar;

type
  TdxBarSubMenuEditor = class(TForm)
    procedure FormDestroy(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FItemLinks: TdxBarItemLinks;
    FItemLinksOwner: TComponent;
    FSubMenuWidth: Integer;
    procedure CorrectBounds;
    function GetSubMenuControl: TdxBarSubMenuControl;
    procedure SetItemLinks(Value: TdxBarItemLinks);
    procedure WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
    procedure WMMove(var Message: TWMMove); message WM_MOVE;
    procedure WMNCLButtonDown(var Message: TWMNCLButtonDown); message WM_NCLBUTTONDOWN;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoShow; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    property SubMenuControl: TdxBarSubMenuControl read GetSubMenuControl;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Release;
    property ItemLinks: TdxBarItemLinks read FItemLinks write SetItemLinks;
  end;

procedure ShowdxBarSubMenuEditor(AItemLinks: TdxBarItemLinks);

function dxBarSubMenuEditor: TdxBarSubMenuEditor;

implementation

{$R *.DFM}

uses
  Types, dxBarCustomCustomizationForm, dxBarStrs, cxClasses, Math, cxControls;

var
  FdxBarSubMenuEditor: TdxBarSubMenuEditor;

procedure ShowdxBarSubMenuEditor(AItemLinks: TdxBarItemLinks);
begin
  if AItemLinks.BarManager = nil then
    Exit;

  AItemLinks.BarManager.Customizing(True);
  Application.ProcessMessages;
  if AItemLinks.BarManager.IsCustomizing then
    dxBarCustomizingForm.SwitchToItemsPage;
  if FdxBarSubMenuEditor = nil then
  begin
    FdxBarSubMenuEditor := TdxBarSubMenuEditor.Create(nil);
    FdxBarSubMenuEditor.Position := poDesigned;
  end;
  with FdxBarSubMenuEditor do
  begin
    ClientHeight := 0;
    ItemLinks := AItemLinks;
    ProcessMouseMessages;
    Show;
  end;
end;

function dxBarSubMenuEditor: TdxBarSubMenuEditor;
begin
  Result := FdxBarSubMenuEditor;
end;

{ TdxBarSubMenuEditor }

constructor TdxBarSubMenuEditor.Create(AOwner: TComponent);
begin
  inherited;
  Caption := cxGetResourceString(@dxSBAR_SUBMENUEDITORCAPTION);
end;

procedure TdxBarSubMenuEditor.Release;
begin
  if not (csDestroying in ComponentState) then
    if HandleAllocated then
      inherited Release
    else
      Free;
end;

procedure TdxBarSubMenuEditor.CorrectBounds;
var
  ARect: TRect;
begin
  ARect := GetDesktopWorkArea(Point(Left, Top));
  Left := Max(Left, ARect.Left);
  Top := Max(Top, ARect.Top);
end;

function TdxBarSubMenuEditor.GetSubMenuControl: TdxBarSubMenuControl;
begin
  if ItemLinks <> nil then
    Result := ItemLinks.BarControl as TdxBarSubMenuControl
  else
    Result := nil;
end;

procedure TdxBarSubMenuEditor.SetItemLinks(Value: TdxBarItemLinks);
begin
  if FItemLinks <> Value then
  begin
    FItemLinks := Value;
    if FItemLinks = nil then
      Release
    else
    begin
      FItemLinksOwner := FItemLinks.Owner;
      FItemLinksOwner.FreeNotification(Self);
    end;
  end;
end;

procedure TdxBarSubMenuEditor.WMGetMinMaxInfo(var Message: TWMGetMinMaxInfo);
begin
  with Message.MinMaxInfo^ do
  begin
    ptMaxSize.Y := Height;
    ptMinTrackSize.Y := Height;
    ptMaxTrackSize.Y := Height;
    if SubMenuControl <> nil then
      FSubMenuWidth := SubMenuControl.Width;
    ptMaxSize.X := FSubMenuWidth;
    ptMinTrackSize.X := FSubMenuWidth;
    ptMaxTrackSize.X := FSubMenuWidth;
  end;
end;

procedure TdxBarSubMenuEditor.WMMove(var Message: TWMMove);
begin
  inherited;
  if SubMenuControl <> nil then
    SetWindowPos(SubMenuControl.Handle, 0, Left, Top + Height, 0, 0,
      SWP_NOZORDER or SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW);
end;

procedure TdxBarSubMenuEditor.WMNCLButtonDown(var Message: TWMNCLButtonDown);
begin
  inherited;
  if Message.HitTest <> HTCLOSE then
    Activate
  else
    if SubMenuControl <> nil then
      SubMenuControl.Hide;
end;

procedure TdxBarSubMenuEditor.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
  begin
    Style := WS_POPUP or WS_CLIPSIBLINGS or WS_SYSMENU or WS_CAPTION;
    ExStyle := WS_EX_DLGMODALFRAME or WS_EX_WINDOWEDGE;
    WindowClass.Style := CS_OWNDC or CS_SAVEBITS;
    WndParent := GetParent(dxBarCustomizingForm.Handle);
  end;
end;

procedure TdxBarSubMenuEditor.DoShow;
begin
  inherited;
  CorrectBounds;
end;

procedure TdxBarSubMenuEditor.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FItemLinksOwner) then
    ItemLinks := nil;
end;

procedure TdxBarSubMenuEditor.FormDestroy(Sender: TObject);
begin
  if SubMenuControl <> nil then
    SubMenuControl.Hide;
  FdxBarSubMenuEditor := nil;
end;

type
  TdxBarManagerAccess = class(TdxBarManager);

procedure TdxBarSubMenuEditor.FormActivate(Sender: TObject);
var
  AErrorText: string;
begin
  with TdxBarManagerAccess(ItemLinks.BarManager) do
    if Dragging and not ItemLinks.CanContainItem(DraggingItem, AErrorText) then Exit;
  if SubMenuControl = nil then
  begin
    ItemLinks.CreateBarControl;
    SubMenuControl.Left := Left;
    SubMenuControl.Top := Top + Height;
    SubMenuControl.OwnerHeight := Height;
    SubMenuControl.Show;
  end;
  Width := SubMenuControl.Width;
end;

procedure TdxBarSubMenuEditor.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

end.
