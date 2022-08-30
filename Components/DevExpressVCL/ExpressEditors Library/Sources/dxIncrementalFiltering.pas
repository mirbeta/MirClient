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

unit dxIncrementalFiltering;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Graphics, Classes, Types, Controls, SysUtils, Windows, Math, Messages,
  cxLookAndFeelPainters, cxGeometry, cxGraphics, cxContainer, cxControls, cxEdit, cxTextEdit;

const
  dxIncrementalFilteringNavigationKeys = [VK_DOWN, VK_UP, VK_PRIOR, VK_NEXT, VK_HOME, VK_END];

type

  { TdxCustomIncrementalFilteringHelper }

  TdxCustomIncrementalFilteringHelper = class
  strict private
    FSearchEdit: TcxCustomTextEdit;
  protected
    procedure CreateSearchEdit; virtual;
    procedure DestroySearchEdit; virtual;

    procedure DrawSearchEditButton(Sender: TcxEditButtonViewInfo; ACanvas: TcxCanvas; var AHandled: Boolean); virtual;
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter; virtual; abstract;
    function GetSearchEditOffsets: TRect; virtual;
    procedure InitSearchEdit; virtual;
    procedure SearchEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual; abstract;
    procedure SearchEditKeyPress(Sender: TObject; var Key: Char); virtual; abstract;
    procedure SearchEditMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean); virtual; abstract;
    procedure SearchEditValueChanged(Sender: TObject); virtual; abstract;
    property SearchEdit: TcxCustomTextEdit read FSearchEdit;
  public
    function GetSearchText: string; virtual;
  end;

  { TdxCustomIncrementalFilteringContainerHelper }

  TdxCustomIncrementalFilteringContainerHelper = class(TdxCustomIncrementalFilteringHelper)
  protected
    function IsActionKey(AKey: Word): Boolean; virtual;
    procedure CheckSearchControl(AParent: TWinControl);
    procedure FocusSearchControl;
    function GetItemCount: Integer; virtual; abstract;
    function GetItemIndex: Integer; virtual; abstract;
    function GetVisibleItemCount: Integer; virtual; abstract;
    procedure ResetSearchText; virtual;
    procedure SearchEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); override;
    procedure SearchEditKeyPress(Sender: TObject; var Key: Char); override;
    procedure SetItemIndex(const Value: Integer); virtual; abstract;

    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
  public
    destructor Destroy; override;
  end;

  { TdxIncrementalFilteringFilterPopupHelper }

  TdxIncrementalFilteringFilterPopupHelper = class(TdxCustomIncrementalFilteringContainerHelper)
  protected
    procedure DoKeyDown(var Key: Word; Shift: TShiftState); virtual;
    function GetMouseWheelHandler: TcxCustomInnerListBox; virtual;

    function IsNavigationKey(AKey: Word; AShift: TShiftState): Boolean; virtual;
    procedure SearchEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); override;

    procedure SearchEditMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean); override;
  end;

  { TdxCustomIncrementalFilteringContainerViewInfo }

  TdxCustomIncrementalFilteringContainerViewInfo = class(TcxContainerViewInfo)
  public
    ScaleFactor: TdxOwnedScaleFactor;
    SearchInfoPanelBounds: TRect;
    SearchInfoPanelHeight: Integer;
    ShowSearchInfoPanel: Boolean;
    UseRightToLeftAlignment: Boolean;

    constructor Create; override;
    destructor Destroy; override;

    procedure DrawSearchInfoPanel(ACanvas: TcxCanvas); virtual;
    function GetSearchInfoPanelDefaultHeight: Integer; virtual;
    function GetSearchInfoPanelHeight: Integer; virtual;
    function GetSearchInfoPanelText: string; virtual;
    function GetSearchInfoPanelTextDrawFlags: Cardinal; virtual;
    function GetSearchInfoPanelTextOffset: Integer; virtual;
    function GetSearchInfoPanelWidth: Integer; virtual;
    function IsSearchInfoPanelVisible: Boolean; virtual;
    procedure Paint(ACanvas: TcxCanvas); override;
  end;

  { TdxCustomIncrementalFilteringPopupWindow }

  TdxCustomIncrementalFilteringPopupWindow = class(TcxCustomPopupWindow)
  strict private
    FShowSearchInfoPanel: Boolean;

    function GetViewInfo: TdxCustomIncrementalFilteringContainerViewInfo;
    procedure SetShowSearchInfoPanel(const Value: Boolean);
  public
    constructor Create(AOwnerControl: TWinControl); override;
    function GetViewInfoClass: TcxContainerViewInfoClass; override;

    property ShowSearchInfoPanel: Boolean read FShowSearchInfoPanel write SetShowSearchInfoPanel;
    property ViewInfo: TdxCustomIncrementalFilteringContainerViewInfo read GetViewInfo;
  end;

implementation

uses
  dxCore, cxEditConsts, cxDrawTextUtils;

type
  TControlAccess = class(TControl);
  TcxCustomTextEditAccess = class(TcxCustomTextEdit);

{ TcxIncrementalFilteringHelper }

function TdxCustomIncrementalFilteringHelper.GetSearchText: string;
begin
  Result := FSearchEdit.Text;
end;

procedure TdxCustomIncrementalFilteringHelper.CreateSearchEdit;
begin
  FSearchEdit := TcxTextEdit.Create(nil);
end;

procedure TdxCustomIncrementalFilteringHelper.DestroySearchEdit;
begin
  FreeAndNil(FSearchEdit);
end;

procedure TdxCustomIncrementalFilteringHelper.DrawSearchEditButton(
  Sender: TcxEditButtonViewInfo; ACanvas: TcxCanvas; var AHandled: Boolean);
begin
  AHandled := True;
  if ACanvas <> nil then
    GetLookAndFeelPainter.DrawScaledSearchEditButtonGlyph(ACanvas, Sender.Bounds, cxbsNormal, Sender.ScaleFactor);
end;

function TdxCustomIncrementalFilteringHelper.GetSearchEditOffsets: TRect;
begin
  Result := cxRect(1, 1, 1, 1);
end;

procedure TdxCustomIncrementalFilteringHelper.InitSearchEdit;
begin
  FSearchEdit.Properties.OnChange := SearchEditValueChanged;
  FSearchEdit.OnKeyDown := SearchEditKeyDown;
  FSearchEdit.OnKeyPress := SearchEditKeyPress;
  FSearchEdit.Properties.Buttons.Add.Kind := bkGlyph;
  FSearchEdit.ViewInfo.OnDrawButton := DrawSearchEditButton;
  TControlAccess(FSearchEdit).OnMouseWheel := SearchEditMouseWheel;
  TcxCustomTextEditAccess(FSearchEdit).BeepOnEnter := False;
end;

{ TdxCustomIncrementalFilteringContainerHelper }

destructor TdxCustomIncrementalFilteringContainerHelper.Destroy;
begin
  DestroySearchEdit;
  inherited Destroy;
end;

function TdxCustomIncrementalFilteringContainerHelper.IsActionKey(AKey: Word): Boolean;
begin
  Result := AKey in [VK_SPACE, VK_RETURN];
end;

procedure TdxCustomIncrementalFilteringContainerHelper.CheckSearchControl(AParent: TWinControl);
begin
  CreateSearchEdit;
  InitSearchEdit;
  SearchEdit.Parent := AParent;
end;

procedure TdxCustomIncrementalFilteringContainerHelper.FocusSearchControl;
begin
  if SearchEdit.CanFocus then
  begin
    SearchEdit.SetFocus;
    SearchEdit.SelStart := Length(GetSearchText);
  end;
end;

procedure TdxCustomIncrementalFilteringContainerHelper.ResetSearchText;
begin
  SearchEdit.Properties.OnChange := nil;
  SearchEdit.Text := '';
  SearchEdit.Properties.OnChange := SearchEditValueChanged;
end;

procedure TdxCustomIncrementalFilteringContainerHelper.SearchEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_DOWN:
      if ItemIndex = -1 then
      begin
        ItemIndex := 0;
        Key := 0;
      end;
    VK_UP, VK_PRIOR:
      if (ItemIndex = 0) and (Key = VK_UP) and (GetSearchText <> '') then
      begin
        ItemIndex := - 1;
        Key := 0;
      end;
    VK_NEXT:
      if ItemIndex = -1 then
      begin
        if ssCtrl in Shift then
          ItemIndex := GetItemCount - 1
        else
          ItemIndex := GetVisibleItemCount - 1;
        Key := 0;
      end;
  end;
end;

procedure TdxCustomIncrementalFilteringContainerHelper.SearchEditKeyPress(Sender: TObject; var Key: Char);
begin
  if IsActionKey(Ord(Key)) and (ItemIndex <> -1) then
    Key := #0;
end;

{ TcxFilterPopupIncrementalFilteringHelper }

procedure TdxIncrementalFilteringFilterPopupHelper.DoKeyDown(var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_DOWN:
      ItemIndex := ItemIndex + 1;
    VK_UP:
      ItemIndex := ItemIndex - 1;
    VK_PRIOR:
      if ssCtrl in Shift then
        ItemIndex := 0
      else
        ItemIndex := Max(ItemIndex - GetVisibleItemCount + 1, 0);
    VK_NEXT:
      if ssCtrl in Shift then
        ItemIndex := GetItemCount - 1
      else
        ItemIndex := Min(ItemIndex + GetVisibleItemCount - 1, GetItemCount - 1);
  end;
end;

function TdxIncrementalFilteringFilterPopupHelper.GetMouseWheelHandler: TcxCustomInnerListBox;
begin
  Result := nil;
end;

function TdxIncrementalFilteringFilterPopupHelper.IsNavigationKey(AKey: Word;
  AShift: TShiftState): Boolean;
begin
  Result := AKey in dxIncrementalFilteringNavigationKeys;
end;

procedure TdxIncrementalFilteringFilterPopupHelper.SearchEditKeyDown(
  Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if IsNavigationKey(Key, Shift) and not (Key in [VK_HOME, VK_END]) or
    (Key = VK_RETURN) or (Key = VK_SPACE) and (ItemIndex <> -1) then
  begin
    DoKeyDown(Key, Shift);
    Key := 0;
  end;
end;

procedure TdxIncrementalFilteringFilterPopupHelper.SearchEditMouseWheel(
  Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
var
  Message: TWMMouseWheel;
begin
  Handled := True;
  Message.Msg := WM_MOUSEWHEEL;
  Message.Keys := ShiftStateToKeys(Shift);
  Message.WheelDelta := WheelDelta;
  Message.Pos := PointToSmallPoint(MousePos);
  if GetMouseWheelHandler <> nil then
  begin
    GetMouseWheelHandler.DefaultHandler(Message);
    GetMouseWheelHandler.SetExternalScrollBarsParameters;
  end;
end;

{ TdxCustomIncrementalFilteringContainerViewInfo }

constructor TdxCustomIncrementalFilteringContainerViewInfo.Create;
begin
  inherited;
  ScaleFactor := TdxOwnedScaleFactor.Create;
end;

destructor TdxCustomIncrementalFilteringContainerViewInfo.Destroy;
begin
  FreeAndNil(ScaleFactor);
  inherited Destroy;
end;

procedure TdxCustomIncrementalFilteringContainerViewInfo.DrawSearchInfoPanel(ACanvas: TcxCanvas);
begin
  Painter.DrawWindowContent(ACanvas, SearchInfoPanelBounds);
  cxTextOut(ACanvas.Canvas, GetSearchInfoPanelText, SearchInfoPanelBounds,
    GetSearchInfoPanelTextDrawFlags, Font, 0, GetSearchInfoPanelTextOffset, 0, Painter.GetWindowContentTextColor);
end;

function TdxCustomIncrementalFilteringContainerViewInfo.GetSearchInfoPanelDefaultHeight: Integer;
begin
  Result := cxTextHeight(Font) + 8;
end;

function TdxCustomIncrementalFilteringContainerViewInfo.GetSearchInfoPanelHeight: Integer;
begin
  Result := SearchInfoPanelHeight;
  if Result = 0 then
    Result := GetSearchInfoPanelDefaultHeight;
end;

function TdxCustomIncrementalFilteringContainerViewInfo.GetSearchInfoPanelText: string;
begin
  Result := cxGetResourceString(@cxSNoMatchesFound);
end;

function TdxCustomIncrementalFilteringContainerViewInfo.GetSearchInfoPanelTextDrawFlags: Cardinal;
begin
  Result := CXTO_CENTER_VERTICALLY or CXTO_SINGLELINE;
  if UseRightToLeftAlignment then
    Result := Result or CXTO_RIGHT or CXTO_RTLREADING;
end;

function TdxCustomIncrementalFilteringContainerViewInfo.GetSearchInfoPanelTextOffset: Integer;
begin
  Result := 4;
end;

function TdxCustomIncrementalFilteringContainerViewInfo.GetSearchInfoPanelWidth: Integer;
begin
  Result := cxTextWidth(Font, GetSearchInfoPanelText) + 2 * GetSearchInfoPanelTextOffset;
end;

function TdxCustomIncrementalFilteringContainerViewInfo.IsSearchInfoPanelVisible: Boolean;
begin
  Result := False;
end;

procedure TdxCustomIncrementalFilteringContainerViewInfo.Paint(ACanvas: TcxCanvas);
begin
  inherited Paint(ACanvas);
  if IsSearchInfoPanelVisible then
    DrawSearchInfoPanel(ACanvas);
end;

{ TdxCustomIncrementalFilteringPopupWindow }

constructor TdxCustomIncrementalFilteringPopupWindow.Create(AOwnerControl: TWinControl);
begin
  inherited;
  ViewInfo.ScaleFactor.Owner := ScaleFactor;
end;

function TdxCustomIncrementalFilteringPopupWindow.GetViewInfoClass: TcxContainerViewInfoClass;
begin
  Result := TdxCustomIncrementalFilteringContainerViewInfo;
end;

function TdxCustomIncrementalFilteringPopupWindow.GetViewInfo: TdxCustomIncrementalFilteringContainerViewInfo;
begin
  Result := inherited ViewInfo as TdxCustomIncrementalFilteringContainerViewInfo;
end;

procedure TdxCustomIncrementalFilteringPopupWindow.SetShowSearchInfoPanel(const Value: Boolean);
begin
  FShowSearchInfoPanel := Value;
  ViewInfo.ShowSearchInfoPanel := Value;
end;

end.
