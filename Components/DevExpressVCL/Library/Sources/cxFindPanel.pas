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

unit cxFindPanel;

interface

uses
  Windows, Classes, Controls, Messages, cxClasses, cxControls, cxDropDownEdit, cxEdit, cxMRUEdit, dxCoreClasses,
  cxStyles, cxLookAndFeels, cxGraphics;

const
  cxFindPanelDefaultMRUItemsListDropDownCount = 8;
  cxFindPanelDefaultMRUItemsListCount = 0;

type
  TcxFindPanelMRUEdit = class;
  TcxCustomFindPanel = class;

  TcxFindPanelDisplayMode = (fpdmNever, fpdmManual, fpdmAlways);
  TcxFindPanelPosition = (fppTop, fppBottom);
  TcxFindPanelFocusedItem = (fpfiNone, fpfiFindEdit, fpfiCloseButton, fpfiFindButton, fpfiClearButton);

  { TcxFindPanelComboBoxInnerEdit }

  TcxFindPanelComboBoxInnerEdit = class(TcxCustomComboBoxInnerEdit)
  strict private
    function GetContainer: TcxFindPanelMRUEdit;
    //
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
  protected
    property Container: TcxFindPanelMRUEdit read GetContainer;
  end;

  { TcxFindPanelMRUEdit }

  TcxFindPanelMRUEdit = class(TcxCustomMRUEdit)
  strict private
    FCachedHeight: Integer;
    FFindPanel: TcxCustomFindPanel;
    FNeedRecalculateHeight: Boolean;

    function GetHeight: Integer;
    procedure SetHeight(Value: Integer);
  protected
    procedure ChangeScaleEx(M: Integer; D: Integer; IsDPIChanged: Boolean); override;
    procedure CreateHandle; override;
    procedure FocusChanged; override;
    function GetInnerEditClass: TControlClass; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure SetInternalEditValue(const Value: Variant); override;
    procedure UndoPerformed; override;

    property FindPanel: TcxCustomFindPanel read FFindPanel;
  public
    constructor Create(AFindPanel: TcxCustomFindPanel); reintroduce; virtual;

    procedure AddItem(const Value: string); override;
    procedure ClearSelection; override;
    procedure CutToClipboard; override;
    procedure Hide;
    procedure PasteFromClipboard; override;
    procedure SetFocus; override;
    procedure SetFocusAndSelectAll;
    procedure Show;
    procedure UpdateLookAndFeel(ALookAndFeel: TcxLookAndFeel);
    procedure UpdateStyles(const AParams, AContentParams: TcxViewParams);

    property Height: Integer read GetHeight write SetHeight;
  end;

  TcxFindPanelOptions = class(TPersistent)
  strict private
    FApplyInputDelay: Integer;
    FClearOnClose: Boolean;
    FDisplayMode: TcxFindPanelDisplayMode;
    FFindPanel: TcxCustomFindPanel;
    FFocusContentOnApply: Boolean;
    FHighlightSearchResults: Boolean;
    FInfoText: string;
    FInfoTextAssigned: Boolean;
    FIsUpdatingMRUItems: Boolean;
    FMRUItems: TStringList;
    FMRUItemsListCount: Integer;
    FMRUItemsListDropDownCount: Integer;
    FPosition: TcxFindPanelPosition;
    FShowClearButton: Boolean;
    FShowCloseButton: Boolean;
    FShowFindButton: Boolean;
    FUseDelayedFind: Boolean;

    function GetMRUItems: TStrings;
    function GetUseExtendedSyntax: Boolean;
    procedure SetApplyInputDelay(AValue: Integer);
    procedure SetClearOnClose(AValue: Boolean);
    procedure SetDisplayMode(AValue: TcxFindPanelDisplayMode);
    procedure SetContentOnApply(AValue: Boolean);
    procedure SetHighlightSearchResults(AValue: Boolean);
    procedure SetMRUItems(AValue: TStrings);
    procedure SetMRUItemsListCount(AValue: Integer);
    procedure SetMRUItemsListDropDownCount(AValue: Integer);
    procedure SetPosition(AValue: TcxFindPanelPosition);
    procedure SetShowClearButton(AValue: Boolean);
    procedure SetShowCloseButton(AValue: Boolean);
    procedure SetShowFindButton(AValue: Boolean);
    procedure SetUseDelayedFind(AValue: Boolean);
    procedure SetUseExtendedSyntax(AValue: Boolean);

    procedure CreateMRUItems;
    procedure DestroyMRUItems;
    procedure MRUItemsChangeHandler(Sender: TObject);

    function GetInfoText: string;
    procedure SetInfoText(AValue: string);
  protected
    procedure Changed;
    procedure CheckMRUItemListCount;

    property FindPanel: TcxCustomFindPanel read FFindPanel;
  public
    constructor Create(AFindPanel: TcxCustomFindPanel); reintroduce; virtual;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    function IsCloseButtonVisible: Boolean;
    procedure UpdateMRUItems;

    property ApplyInputDelay: Integer read FApplyInputDelay write SetApplyInputDelay;
    property ClearOnClose: Boolean read FClearOnClose write SetClearOnClose;
    property DisplayMode: TcxFindPanelDisplayMode read FDisplayMode write SetDisplayMode;
    property FocusContentOnApply: Boolean read FFocusContentOnApply write SetContentOnApply;
    property HighlightSearchResults: Boolean read FHighlightSearchResults write SetHighlightSearchResults;
    property InfoText: string read GetInfoText write SetInfoText;
    property InfoTextAssigned: Boolean read FInfoTextAssigned;
    property MRUItems: TStrings read GetMRUItems write SetMRUItems;
    property MRUItemsListCount: Integer read FMRUItemsListCount write SetMRUItemsListCount;
    property MRUItemsListDropDownCount: Integer read FMRUItemsListDropDownCount write SetMRUItemsListDropDownCount;
    property Position: TcxFindPanelPosition read FPosition write SetPosition;
    property ShowClearButton: Boolean read FShowClearButton write SetShowClearButton;
    property ShowCloseButton: Boolean read FShowCloseButton write SetShowCloseButton;
    property ShowFindButton: Boolean read FShowFindButton write SetShowFindButton;
    property UseDelayedFind: Boolean read FUseDelayedFind write SetUseDelayedFind;
    property UseExtendedSyntax: Boolean read GetUseExtendedSyntax write SetUseExtendedSyntax;
  end;

  { TcxFindPanel }

  TcxCustomFindPanel = class
  strict private
    FDelayedFindTimer: TcxTimer;
    FEdit: TcxFindPanelMRUEdit;
    FFocusedItem: TcxFindPanelFocusedItem;
    FFreeNotificator: TcxFreeNotificator;
    FOptions: TcxFindPanelOptions;
    FVisible: Boolean;

    function GetEditHeight: Integer;
    function GetEditLookupItems: TStrings;
    procedure SetFocusedItem(AValue: TcxFindPanelFocusedItem);
    procedure SetOptions(AValue: TcxFindPanelOptions);
    procedure SetVisible(AValue: Boolean);

    procedure CreateTimer;
    procedure DestroyTimer;
    procedure OnDelayedFindTimer(Sender: TObject);

    procedure FreeNotificationHandler(Sender: TComponent);
  protected
    procedure AddTextInEditMRUItems; virtual;
    function CanHide: Boolean; virtual;
    procedure Clear; virtual;
    procedure FocusedItemChanged; virtual;
    procedure FocusedItemExecute; virtual;
    procedure FocusNextItem; virtual;
    procedure FocusPreviewItem; virtual;
    function GetClearButtonCaption: string; virtual;
    function GetDefaultInfoText: string; virtual;
    function GetDefaultMRUItemsListCount: Integer; virtual;
    function GetDefaultMRUItemsListDropDownCount: Integer; virtual;
    function GetFindButtonCaption: string; virtual;
    function IsDesigning: Boolean;
    function IsDestroying: Boolean;
    procedure StartDelayedFind; virtual;
    procedure UpdateEdit; virtual;
    procedure UpdateEditLookAndFeel; virtual;
    procedure UpdateEditStyles; virtual;
    procedure UpdateEditValue; virtual;
    procedure UpdateStyles; virtual;
    procedure ValidateFocusedItem(AIsFocusedNext: Boolean = True); virtual;
    procedure VisibilityChanged; virtual;

    //abstraction
    procedure ApplyText(const AText: string); virtual; abstract;
    procedure ClearText; virtual; abstract;
    procedure FocusChanged; virtual; abstract;
    procedure FocusControl; virtual; abstract;
    function FocusData: Boolean; virtual; abstract;
    procedure GetContentViewParams(var AParams: TcxViewParams); virtual; abstract;
    function GetEditLookAndFeel: TcxLookAndFeel; virtual; abstract;
    function GetOwner: TComponent; virtual; abstract;
    function GetParent: TWinControl; virtual; abstract;
    function GetText: string; virtual; abstract;
    function GetUseExtendedSyntax: Boolean; virtual; abstract;
    procedure GetViewParams(var AParams: TcxViewParams); virtual; abstract;
    procedure SetUseExtendedSyntax(AValue: Boolean); virtual; abstract;

    function CreateEdit: TcxFindPanelMRUEdit; virtual;
    function CreateOptions: TcxFindPanelOptions; virtual;
    procedure DestroyEdit; virtual;
    procedure DestroyOptions; virtual;

    property Edit: TcxFindPanelMRUEdit read FEdit;
    property EditLookAndFeel: TcxLookAndFeel read GetEditLookAndFeel;
    property Text: string read GetText;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Apply; virtual;
    function CanShow: Boolean; virtual;
    procedure Changed; virtual; abstract;
    procedure ClearButtonExecute; virtual;
    procedure CloseButtonExecute; virtual;
    procedure ControlFocusChanged(AIsFocused: Boolean); virtual;
    procedure DelayedFind; virtual;
    procedure DisplayModeChanged; virtual;
    procedure EditFocusChanged; virtual;
    procedure FindButtonExecute; virtual;
    procedure FindChanged; virtual;
    procedure Hide; virtual;
    procedure HideEdit; virtual;
    function IsFocused: Boolean; virtual;
    function IsEditFocused: Boolean; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); virtual;
    procedure KeyPress(var Key: Char); virtual;
    procedure KeyUp(var Key: Word; Shift: TShiftState); virtual;
    procedure LookAndFeelChanged; virtual;
    procedure Show(AFocusEdit: Boolean = True); virtual;
    procedure ShowEdit; virtual;
    procedure StylesChanged; virtual;
    procedure UpdateEditBounds(const ABounds: TRect); virtual;
    procedure UpdateEditMRUItems; virtual;
    procedure UpdateEditTextHint; virtual;
    procedure UpdateMRUItems; virtual;
    procedure UpdateOptionsMRUItems; virtual;

    property ClearButtonCaption: string read GetClearButtonCaption;
    property DefaultInfoText: string read GetDefaultInfoText;
    property DefaultMRUItemsListCount: Integer read GetDefaultMRUItemsListCount;
    property DefaultMRUItemsListDropDownCount: Integer read GetDefaultMRUItemsListDropDownCount;
    property EditHeight: Integer read GetEditHeight;
    property EditLookupItems: TStrings read GetEditLookupItems;
    property FindButtonCaption: string read GetFindButtonCaption;
    property FocusedItem: TcxFindPanelFocusedItem read FFocusedItem write SetFocusedItem;
    property Options: TcxFindPanelOptions read FOptions write SetOptions;
    property Owner: TComponent read GetOwner;
    property Parent: TWinControl read GetParent;
    property Visible: Boolean read FVisible write SetVisible;
    property UseExtendedSyntax: Boolean read GetUseExtendedSyntax write SetUseExtendedSyntax;
  end;

implementation

uses
  SysUtils, Variants, Math, dxCore;

{ TcxFindPanelComboBoxInnerEdit }

function TcxFindPanelComboBoxInnerEdit.GetContainer: TcxFindPanelMRUEdit;
begin
  Result := TcxFindPanelMRUEdit(inherited Container);
end;

procedure TcxFindPanelComboBoxInnerEdit.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTTAB or DLGC_WANTALLKEYS;
end;

{ TcxFindPanelMRUEdit }

constructor TcxFindPanelMRUEdit.Create(AFindPanel: TcxCustomFindPanel);
begin
  inherited Create(AFindPanel.Owner);
  FFindPanel := AFindPanel;
  AutoSize := False;
  ParentCtl3D := False;
  Ctl3D := False;
  TabStop := False;
  BorderStyle := cxcbsNone;
  DoubleBuffered := False;
  Visible := False;
  FNeedRecalculateHeight := True;
  Properties.ShowEllipsis := False;
end;

procedure TcxFindPanelMRUEdit.AddItem(const Value: string);
begin
  inherited AddItem(Value);
  FindPanel.UpdateOptionsMRUItems;
end;

procedure TcxFindPanelMRUEdit.ClearSelection;
begin
  inherited ClearSelection;
  FindPanel.DelayedFind;
end;

procedure TcxFindPanelMRUEdit.CutToClipboard;
begin
  inherited CutToClipboard;
  FindPanel.DelayedFind;
end;

procedure TcxFindPanelMRUEdit.Hide;
begin
  if IsFocused then
    Windows.SetFocus(Parent.Handle);
  DestroyHandle;
  Parent := nil;
  Visible := False;
end;

procedure TcxFindPanelMRUEdit.PasteFromClipboard;
begin
  inherited PasteFromClipboard;
  FindPanel.DelayedFind;
end;

procedure TcxFindPanelMRUEdit.SetFocus;
begin
  if CanFocusEx then
    inherited SetFocus;
end;

procedure TcxFindPanelMRUEdit.SetFocusAndSelectAll;
begin
  SetFocus;
  SelectAll;
end;

procedure TcxFindPanelMRUEdit.Show;
begin
  Parent := FindPanel.Parent;
  CheckHandle;
  Visible := True;
  if not IsFocused and (FindPanel.FocusedItem = fpfiFindEdit) then
    SetFocusAndSelectAll;
end;

procedure TcxFindPanelMRUEdit.UpdateLookAndFeel(ALookAndFeel: TcxLookAndFeel);
begin
  Style.LookAndFeel.MasterLookAndFeel := ALookAndFeel;
  FNeedRecalculateHeight := True;
end;

procedure TcxFindPanelMRUEdit.UpdateStyles(const AParams, AContentParams: TcxViewParams);
var
  AEditParams: TcxViewParams;
begin
  AEditParams := AParams;
  AEditParams.Color := AContentParams.Color;
  Style.Init(AEditParams);
  FNeedRecalculateHeight := True;
end;

procedure TcxFindPanelMRUEdit.ChangeScaleEx(M, D: Integer; IsDPIChanged: Boolean);
var
  ABounds: TRect;
begin
  ABounds := BoundsRect;
  try
    inherited ChangeScaleEx(M, D, IsDPIChanged);
  finally
    BoundsRect := ABounds;
  end;
end;

procedure TcxFindPanelMRUEdit.CreateHandle;
begin
  inherited CreateHandle;
end;

procedure TcxFindPanelMRUEdit.FocusChanged;
begin
  if csDestroying in Parent.ComponentState then
    Exit;
  inherited FocusChanged;
  FindPanel.EditFocusChanged;
end;

function TcxFindPanelMRUEdit.GetInnerEditClass: TControlClass;
begin
  Result := TcxFindPanelComboBoxInnerEdit;
end;

procedure TcxFindPanelMRUEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  FindPanel.KeyDown(Key, Shift);
  inherited KeyDown(Key, Shift);
end;

procedure TcxFindPanelMRUEdit.SetInternalEditValue(const Value: Variant);
begin
  inherited;
  FindPanel.Apply;
end;

procedure TcxFindPanelMRUEdit.UndoPerformed;
begin
  inherited UndoPerformed;
  FindPanel.DelayedFind;
end;

function TcxFindPanelMRUEdit.GetHeight: Integer;
var
  AEditSizeProperties: TcxEditSizeProperties;
begin
  if FNeedRecalculateHeight then
  begin
    AEditSizeProperties.Height := -1;
    AEditSizeProperties.Width := -1;
    AEditSizeProperties.MaxLineCount := 1;
    FCachedHeight := Properties.GetEditSize(cxScreenCanvas, Style, False, null, AEditSizeProperties).cy;
    Height := FCachedHeight;
    FNeedRecalculateHeight := False;
  end;
  Result := FCachedHeight;
end;

procedure TcxFindPanelMRUEdit.SetHeight(Value: Integer);
begin
  inherited Height := Value;
end;

{ TcxGridFindPanelOptions }

constructor TcxFindPanelOptions.Create(AFindPanel: TcxCustomFindPanel);
begin
  inherited Create;
  CreateMRUItems;
  FApplyInputDelay := 1000;
  FClearOnClose := True;
  FDisplayMode := fpdmNever;
  FFindPanel := AFindPanel;
  FHighlightSearchResults := True;
  FInfoText := FindPanel.DefaultInfoText;
  FMRUItemsListDropDownCount := FindPanel.DefaultMRUItemsListDropDownCount;
  FMRUItemsListCount := FindPanel.DefaultMRUItemsListCount;
  FShowClearButton := True;
  FShowCloseButton := True;
  FShowFindButton := True;
  FUseDelayedFind := True;
end;

destructor TcxFindPanelOptions.Destroy;
begin
  DestroyMRUItems;
  inherited Destroy;
end;

procedure TcxFindPanelOptions.Assign(Source: TPersistent);
var
  AOptions: TcxFindPanelOptions;
begin
  if Source is TcxFindPanelOptions then
  begin
    AOptions := TcxFindPanelOptions(Source);
    DisplayMode := AOptions.DisplayMode;
    ClearOnClose := AOptions.ClearOnClose;
    ApplyInputDelay := AOptions.ApplyInputDelay;
    HighlightSearchResults := AOptions.HighlightSearchResults;
    MRUItems := AOptions.MRUItems;
    MRUItemsListCount := AOptions.MRUItemsListCount;
    MRUItemsListDropDownCount := AOptions.MRUItemsListDropDownCount;
    ShowClearButton := AOptions.ShowClearButton;
    ShowCloseButton := AOptions.ShowCloseButton;
    ShowFindButton := AOptions.ShowFindButton;
    UseDelayedFind := AOptions.UseDelayedFind;
  end;
end;

function TcxFindPanelOptions.IsCloseButtonVisible: Boolean;
begin
  Result := ShowCloseButton and not (DisplayMode = fpdmAlways);
end;

procedure TcxFindPanelOptions.UpdateMRUItems;
begin
  FIsUpdatingMRUItems := True;
  try
    if FindPanel.Visible then
      MRUItems := FindPanel.EditLookupItems
    else
      CheckMRUItemListCount;
  finally
    FIsUpdatingMRUItems := False;
  end;
end;

procedure TcxFindPanelOptions.Changed;
begin
  FindPanel.Changed;
end;

procedure TcxFindPanelOptions.CheckMRUItemListCount;
begin
  if MRUItemsListCount > 0 then
    while MRUItems.Count > MRUItemsListCount do
      MRUItems.Delete(MRUItems.Count - 1);
end;

function TcxFindPanelOptions.GetMRUItems: TStrings;
begin
  Result := FMRUItems;
end;

function TcxFindPanelOptions.GetUseExtendedSyntax: Boolean;
begin
  Result := FindPanel.UseExtendedSyntax;
end;

procedure TcxFindPanelOptions.SetApplyInputDelay(AValue: Integer);
begin
  if ApplyInputDelay <> AValue then
  begin
    FApplyInputDelay := AValue;
    Changed;
  end;
end;

procedure TcxFindPanelOptions.SetClearOnClose(AValue: Boolean);
begin
  if ClearOnClose <> AValue then
  begin
    FClearOnClose := AValue;
    Changed;
  end;
end;

procedure TcxFindPanelOptions.SetDisplayMode(AValue: TcxFindPanelDisplayMode);
begin
  if DisplayMode <> AValue then
  begin
    FDisplayMode := AValue;
    FindPanel.DisplayModeChanged;
  end;
end;

procedure TcxFindPanelOptions.SetContentOnApply(AValue: Boolean);
begin
  if FocusContentOnApply <> AValue then
  begin
    FFocusContentOnApply := AValue;
    Changed;
  end;
end;

procedure TcxFindPanelOptions.SetHighlightSearchResults(AValue: Boolean);
begin
  if HighlightSearchResults <> AValue then
  begin
    FHighlightSearchResults := AValue;
    Changed;
  end;
end;

procedure TcxFindPanelOptions.SetMRUItems(AValue: TStrings);
begin
  MRUItems.Assign(AValue);
end;

procedure TcxFindPanelOptions.SetMRUItemsListCount(AValue: Integer);
begin
  AValue := Max(AValue, 0);
  if MRUItemsListCount <> AValue then
  begin
    FMRUItemsListCount := AValue;
    FindPanel.UpdateMRUItems;
  end;
end;

procedure TcxFindPanelOptions.SetMRUItemsListDropDownCount(AValue: Integer);
begin
  AValue := Max(AValue, 1);
  if MRUItemsListDropDownCount <> AValue then
  begin
    FMRUItemsListDropDownCount := AValue;
    FindPanel.UpdateEditMRUItems;
  end;
end;

procedure TcxFindPanelOptions.SetPosition(AValue: TcxFindPanelPosition);
begin
  if Position <> AValue then
  begin
    FPosition := AValue;
    Changed;
  end;
end;

procedure TcxFindPanelOptions.SetShowClearButton(AValue: Boolean);
begin
  if ShowClearButton <> AValue then
  begin
    FShowClearButton := AValue;
    Changed;
  end;
end;

procedure TcxFindPanelOptions.SetShowCloseButton(AValue: Boolean);
begin
  if ShowCloseButton <> AValue then
  begin
    FShowCloseButton := AValue;
    Changed;
  end;
end;

procedure TcxFindPanelOptions.SetShowFindButton(AValue: Boolean);
begin
  if ShowFindButton <> AValue then
  begin
    FShowFindButton := AValue;
    Changed;
  end;
end;

procedure TcxFindPanelOptions.SetUseDelayedFind(AValue: Boolean);
begin
  if UseDelayedFind <> AValue then
  begin
    FUseDelayedFind := AValue;
    Changed;
  end;
end;

procedure TcxFindPanelOptions.SetUseExtendedSyntax(AValue: Boolean);
begin
  FindPanel.UseExtendedSyntax := AValue;
end;

procedure TcxFindPanelOptions.CreateMRUItems;
begin
  FMRUItems := TStringList.Create;
  FMRUItems.OnChange := MRUItemsChangeHandler;
end;

procedure TcxFindPanelOptions.DestroyMRUItems;
begin
  FreeAndNil(FMRUItems);
end;

procedure TcxFindPanelOptions.MRUItemsChangeHandler(Sender: TObject);
begin
  if not FIsUpdatingMRUItems then
    FindPanel.UpdateMRUItems;
end;

function TcxFindPanelOptions.GetInfoText: string;
begin
  if FInfoTextAssigned then
    Result := FInfoText
  else
    Result := FindPanel.DefaultInfoText;
end;

procedure TcxFindPanelOptions.SetInfoText(AValue: string);
begin
  if FInfoText <> AValue then
  begin
    FInfoText := AValue;
    FInfoTextAssigned := FInfoText <> FindPanel.DefaultInfoText;
    Changed;
  end;
end;

{ TcxFindPanel }

constructor TcxCustomFindPanel.Create;
begin
  inherited Create;
  FOptions := CreateOptions;
  FFreeNotificator := TcxFreeNotificator.Create(nil);
  FFreeNotificator.OnFreeNotification := FreeNotificationHandler;
  FEdit := CreateEdit;
end;

destructor TcxCustomFindPanel.Destroy;
begin
  DestroyTimer;
  FreeAndNil(FFreeNotificator);
  DestroyEdit;
  DestroyOptions;
  inherited Destroy;
end;

procedure TcxCustomFindPanel.ClearButtonExecute;
begin
  Clear;
  Edit.SetFocus;
end;

procedure TcxCustomFindPanel.CloseButtonExecute;
begin
  Hide;
end;

procedure TcxCustomFindPanel.ControlFocusChanged(AIsFocused: Boolean);
begin
  if not Visible then
    Exit;
  case FocusedItem of
    fpfiFindEdit:
      if not Edit.IsFocused then
        Edit.SetFocusAndSelectAll;
    fpfiCloseButton..fpfiClearButton:
      if not AIsFocused then
        FocusedItem := fpfiNone;
  end;
end;

procedure TcxCustomFindPanel.DelayedFind;
begin
  if Options.UseDelayedFind then
    StartDelayedFind;
end;

procedure TcxCustomFindPanel.DisplayModeChanged;
begin
  case Options.DisplayMode of
    fpdmManual:
      if IsDesigning then
        Hide
      else
        if Visible then
          Changed;
    fpdmAlways:
      Show(False);
    else
      Hide
  end;
end;

procedure TcxCustomFindPanel.EditFocusChanged;
begin
  if IsEditFocused then
    FocusedItem := fpfiFindEdit
  else
    if FocusedItem = fpfiFindEdit then
      FocusedItem := fpfiNone;
end;

procedure TcxCustomFindPanel.FindButtonExecute;
begin
  Apply;
  AddTextInEditMRUItems;
  Edit.SetFocusAndSelectAll;
end;

procedure TcxCustomFindPanel.FindChanged;
begin
  DestroyTimer;
  if Visible then
    UpdateEditValue;
  if Options.FocusContentOnApply and FocusData then
  begin
    FocusedItem := fpfiNone;
    FocusControl;
  end;
end;

procedure TcxCustomFindPanel.Hide;
begin
  if CanHide then
    Visible := False;
end;

procedure TcxCustomFindPanel.HideEdit;
begin
  Edit.Hide;
end;

function TcxCustomFindPanel.IsFocused: Boolean;
begin
  Result := FocusedItem <> fpfiNone;
end;

function TcxCustomFindPanel.IsEditFocused: Boolean;
begin
  Result := Edit.IsFocused;
end;

procedure TcxCustomFindPanel.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN:
      begin
        if FocusedItem in [fpfiCloseButton..fpfiClearButton] then
          Key := 0;
        FocusedItemExecute;
      end;
    VK_SPACE:
      if FocusedItem in [fpfiCloseButton..fpfiClearButton] then
        FocusedItemExecute;
    VK_TAB:
      begin
        if Shift = [ssShift] then
          FocusPreviewItem
        else
          FocusNextItem;
        Key := 0;
      end;
    VK_DOWN:
      begin
        if not Edit.HasPopupWindow then
        begin
          if FocusData then
            FocusControl;
          Key := 0;
        end;
      end;
    VK_ESCAPE:
      begin
        if VarIsNull(Edit.EditingValue) or (Edit.EditingValue = '') then
          Hide
        else
          Clear;
        Key := 0;
      end;
  end;
  if not (Key in [0, VK_RETURN]) then
    DelayedFind;
end;

procedure TcxCustomFindPanel.KeyPress(var Key: Char);
begin
// do nothing
end;

procedure TcxCustomFindPanel.KeyUp(var Key: Word; Shift: TShiftState);
begin
//do nothing
end;

procedure TcxCustomFindPanel.LookAndFeelChanged;
begin
  UpdateStyles;
end;

procedure TcxCustomFindPanel.Show(AFocusEdit: Boolean = True);
begin
  if CanShow then
  begin
    Visible := True;
    if AFocusEdit then
      FocusedItem := fpfiFindEdit;
  end;
end;

procedure TcxCustomFindPanel.ShowEdit;
begin
  Edit.Show;
  UpdateStyles;
end;

procedure TcxCustomFindPanel.StylesChanged;
begin
  if Visible then
    UpdateStyles;
end;

procedure TcxCustomFindPanel.UpdateEditBounds(const ABounds: TRect);
begin
  Edit.BoundsRect := ABounds;
end;

procedure TcxCustomFindPanel.UpdateEditMRUItems;
begin
  if Visible then
  begin
    Edit.Properties.LookupItems := Options.MRUItems;
    Edit.Properties.MaxItemCount := Options.MRUItemsListCount;
    Edit.Properties.DropDownRows := Options.MRUItemsListDropDownCount;
  end;
end;

procedure TcxCustomFindPanel.UpdateEditTextHint;
begin
  Edit.TextHint := Options.InfoText;
end;

procedure TcxCustomFindPanel.UpdateMRUItems;
begin
  UpdateEditMRUItems;
  UpdateOptionsMRUItems;
end;

procedure TcxCustomFindPanel.UpdateOptionsMRUItems;
begin
  Options.UpdateMRUItems;
end;

procedure TcxCustomFindPanel.AddTextInEditMRUItems;
begin
  Edit.AddItem(Text);
end;

procedure TcxCustomFindPanel.Apply;
var
  AText: string;
begin
  if not Visible then
    Exit;
  if VarIsNull(Edit.EditingValue) then
    AText := ''
  else
    AText := Edit.EditingValue;
  ApplyText(AText);
end;

function TcxCustomFindPanel.CanShow;
begin
  Result := Options.DisplayMode in [fpdmManual, fpdmAlways];
end;

function TcxCustomFindPanel.CanHide;
begin
  Result := Options.DisplayMode in [fpdmNever, fpdmManual];
end;

procedure TcxCustomFindPanel.Clear;
begin
  if Text = '' then
  begin
    DestroyTimer;
    if Visible then
      UpdateEditValue;
  end
  else
    ClearText;
end;

procedure TcxCustomFindPanel.FocusedItemChanged;
begin
  if IsDestroying then
    Exit;
  if FocusedItem = fpfiNone then
    FocusChanged
  else
    if FocusedItem <> fpfiFindEdit then
      FocusControl
    else
      Edit.SetFocusAndSelectAll;
  Changed;
end;

procedure TcxCustomFindPanel.FocusedItemExecute;
begin
  case FocusedItem of
    fpfiCloseButton:
      CloseButtonExecute;
    fpfiFindButton:
      FindButtonExecute;
    fpfiClearButton:
      ClearButtonExecute;
    fpfiFindEdit:
      Apply;
  end;
end;

procedure TcxCustomFindPanel.FocusNextItem;
begin
  case FocusedItem of
    fpfiNone, fpfiCloseButton:
      FocusedItem := fpfiFindEdit;
    fpfiFindEdit:
      FocusedItem := fpfiFindButton;
    fpfiFindButton:
      FocusedItem := fpfiClearButton;
    fpfiClearButton:
      FocusedItem := fpfiCloseButton;
  end;
  ValidateFocusedItem;
end;

procedure TcxCustomFindPanel.FocusPreviewItem;
begin
  case FocusedItem of
    fpfiCloseButton:
      FocusedItem := fpfiClearButton;
    fpfiFindEdit:
      FocusedItem := fpfiCloseButton;
    fpfiFindButton, fpfiNone:
      FocusedItem := fpfiFindEdit;
    fpfiClearButton:
      FocusedItem := fpfiFindButton;
  end;
  ValidateFocusedItem(False);
end;

function TcxCustomFindPanel.GetClearButtonCaption: string;
begin
  Result := 'Clear';
end;

function TcxCustomFindPanel.GetDefaultInfoText: string;
begin
  Result := 'Enter text to search...';
end;

function TcxCustomFindPanel.GetDefaultMRUItemsListCount: Integer;
begin
  Result := cxFindPanelDefaultMRUItemsListCount;
end;

function TcxCustomFindPanel.GetDefaultMRUItemsListDropDownCount: Integer;
begin
  Result := cxFindPanelDefaultMRUItemsListDropDownCount;
end;

function TcxCustomFindPanel.GetFindButtonCaption: string;
begin
  Result := 'Find';
end;

function TcxCustomFindPanel.IsDesigning: Boolean;
begin
  Result := csDesigning in Owner.ComponentState;
end;

function TcxCustomFindPanel.IsDestroying: Boolean;
begin
  Result := csDestroying in Owner.ComponentState;
end;

procedure TcxCustomFindPanel.StartDelayedFind;
begin
  DestroyTimer;
  CreateTimer;
end;

procedure TcxCustomFindPanel.UpdateEdit;
begin
  UpdateStyles;
  UpdateEditMRUItems;
  UpdateEditValue;
end;

procedure TcxCustomFindPanel.UpdateEditLookAndFeel;
begin
  Edit.UpdateLookAndFeel(EditLookAndFeel);
end;

procedure TcxCustomFindPanel.UpdateEditStyles;
var
  AParams, AContentParams: TcxViewParams;
begin
  GetViewParams(AParams);
  GetContentViewParams(AContentParams);
  Edit.UpdateStyles(AParams, AContentParams);
end;

procedure TcxCustomFindPanel.UpdateEditValue;
var
  AEditValue: TcxEditValue;
begin
  if Text = '' then
    AEditValue := Null
  else
    AEditValue := Text;
  Edit.EditValue := AEditValue;
end;

procedure TcxCustomFindPanel.UpdateStyles;
begin
  UpdateEditLookAndFeel;
  UpdateEditStyles;
end;

procedure TcxCustomFindPanel.ValidateFocusedItem(AIsFocusedNext: Boolean = True);
begin
  if ((FocusedItem = fpfiCloseButton) and not Options.IsCloseButtonVisible) or
    ((FocusedItem = fpfiFindButton) and not Options.ShowFindButton) or
    ((FocusedItem = fpfiClearButton) and not Options.ShowClearButton) then
    if AIsFocusedNext then
      FocusNextItem
    else
      FocusPreviewItem;
end;

procedure TcxCustomFindPanel.VisibilityChanged;
begin
  if Visible then
    UpdateEdit
  else
  begin
    FocusedItem := fpfiNone;
    if Options.ClearOnClose then
      Clear;
  end;
  Changed;
end;

function TcxCustomFindPanel.CreateEdit: TcxFindPanelMRUEdit;
begin
  Result := TcxFindPanelMRUEdit.Create(Self);
  FFreeNotificator.AddSender(Result);
end;

function TcxCustomFindPanel.CreateOptions: TcxFindPanelOptions;
begin
  Result := TcxFindPanelOptions.Create(Self);
end;

procedure TcxCustomFindPanel.DestroyEdit;
begin
  FreeAndNil(FEdit);
end;

procedure TcxCustomFindPanel.DestroyOptions;
begin
  FreeAndNil(FOptions);
end;

function TcxCustomFindPanel.GetEditHeight: Integer;
begin
  Result := Edit.Height;
end;

function TcxCustomFindPanel.GetEditLookupItems: TStrings;
begin
  Result := Edit.Properties.LookupItems;
end;

procedure TcxCustomFindPanel.SetFocusedItem(AValue: TcxFindPanelFocusedItem);
begin
  if FocusedItem <> AValue then
  begin
    FFocusedItem := AValue;
    FocusedItemChanged;
  end;
end;

procedure TcxCustomFindPanel.SetOptions(AValue: TcxFindPanelOptions);
begin
  Options.Assign(AValue);
end;

procedure TcxCustomFindPanel.SetVisible(AValue: Boolean);
begin
  if Visible <> AValue then
  begin
    FVisible := AValue;
    VisibilityChanged;
  end;
end;

procedure TcxCustomFindPanel.CreateTimer;
begin
  FDelayedFindTimer := TcxTimer.Create(nil);
  FDelayedFindTimer.Interval := Options.ApplyInputDelay;
  FDelayedFindTimer.OnTimer := OnDelayedFindTimer;
end;

procedure TcxCustomFindPanel.DestroyTimer;
begin
  FreeAndNil(FDelayedFindTimer);
end;

procedure TcxCustomFindPanel.OnDelayedFindTimer(Sender: TObject);
begin
  DestroyTimer;
  Apply;
end;

procedure TcxCustomFindPanel.FreeNotificationHandler(Sender: TComponent);
begin
  FEdit := nil;
  if not IsDestroying then
  begin
    FEdit := CreateEdit;
    UpdateEdit;
  end;
end;

end.
