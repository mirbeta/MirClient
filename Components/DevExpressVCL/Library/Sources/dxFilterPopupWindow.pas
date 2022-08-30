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

unit dxFilterPopupWindow;

{$I cxVer.inc}

interface

uses
  Classes, Windows, Controls, Messages, cxControls, cxGraphics, cxListBox, cxLookAndFeelPainters,
  dxCore, dxCoreClasses, cxContainer, dxIncrementalFiltering, cxFilter, cxButtons, cxClasses, cxTextEdit,
  dxUIElementPopupWindow, dxFilterValueContainer, Generics.Collections, cxGeometry;

type
  TdxFilterPopupWindowMode = (fpmDefault, fpmClassic, fpmExcel);

const
  dxDefaultFilterPopupWindowMode: TdxFilterPopupWindowMode = fpmClassic;

type
  TdxFilterPopupWindowFilterValueContainer = class;
  TdxFilterPopupWindowExcelFilterValueContainer = class;
  TdxFilterPopupWindow = class;

  TdxFilterPopupWindowActualMode = fpmClassic..fpmExcel; //for internal use
  TdxFilterPopupWindowSysPanelType = (sptNone, sptClearClose, sptClearOkCancel); //for internal use

  { IdxFilterPopupWindowOptions }

  IdxFilterPopupWindowOptions = interface
  ['{EF3A7F20-7A85-4AEB-9FE6-91D3A60CDBC7}']
    function GetApplyMode: TdxFilterApplyChangesMode;
    function GetButtonCaption: string;
    function GetIncrementalFiltering: Boolean;
    function GetIncrementalFilteringOptions: TcxTextEditIncrementalFilteringOptions;
    function GetVisibleItemCount: Integer;
    function GetShowCheckBoxes: Boolean;
    function GetWidth: Integer;
  end;

  { IdxExcelFilterPopupWindowOptions }

  IdxExcelFilterPopupWindowOptions = interface
  ['{BC38A38C-8202-46C6-B5B3-3952577DE96B}']
    function GetApplyChanges: TdxExcelFilterValueContainerApplyChangesMode;
    function GetDateTimeValuesPageType: TdxExcelFilterValueContainerDateTimeValuesPageType;
    function GetDefaultPage: TdxExcelFilterValueContainerDefaultPage;
    function GetNumericValuesPageType: TdxExcelFilterValueContainerNumericValuesPageType;
  end;

  { IdxFilterPopupWindowOwner }

  IdxFilterPopupWindowOwner = interface
  ['{973519A2-4DDB-4DF6-93E9-D23E89675977}']
    function GetLinkComponent: TComponent;
    function GetMode: TdxFilterPopupWindowMode;
    function GetOptions: TObject;
  end;

  { IdxFilterPopupWindowFilterValueContainer }

  IdxFilterPopupWindowFilterValueContainer = interface
  ['{F48DAC4E-2091-4D0A-A596-A19629D08B07}']
    procedure AssignOptions(AOptions: TObject);
    procedure Closed;
    procedure FocusStartControl;
    procedure SysPanelButtonAction(AHitCode: Integer);
    function SysPanelButtonEnabled(AHitCode: Integer): Boolean;
    function SysPanelType: TdxFilterPopupWindowSysPanelType;
  end;

  { TdxFilterPopupWindowFilterValueContainerListBox }

  TdxFilterPopupWindowFilterValueContainerListBox = class(TdxFilterValueContainerListBox)
  strict private
    function GetFilterValueContainer: TdxFilterPopupWindowFilterValueContainer;
  protected
    procedure ApplyCheckAction(AItemIndex: Integer); override;
    procedure ApplyClickAction(AItemIndex: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  public
    property FilterValueContainer: TdxFilterPopupWindowFilterValueContainer read GetFilterValueContainer;
  end;

  { TdxFilterPopupWindowExcelFilterValueContainerListBox }

  TdxFilterPopupWindowExcelFilterValueContainerListBox = class(TdxExcelFilterValueContainerListBox)
  strict private
    function GetFilterValueContainer: TdxFilterPopupWindowExcelFilterValueContainer;
  protected
    procedure ApplyCheckAction(AItemIndex: Integer); override;
  public
    property FilterValueContainer: TdxFilterPopupWindowExcelFilterValueContainer read GetFilterValueContainer;
  end;

  { TdxFilterPopupWindowFilterValueContainer }

  TdxFilterPopupWindowFilterValueContainer = class(TdxFilterValueContainer,
    IdxFilterPopupWindowFilterValueContainer)
  strict private
    function GetListBox: TdxFilterPopupWindowFilterValueContainerListBox;
    function GetPopup: TdxFilterPopupWindow;
    function GetVisibleItemCount: Integer;
    function GetVisibleWidth: Integer;
    procedure SetVisibleItemCount(AValue: Integer);
    procedure SetVisibleWidth(AValue: Integer);
  protected
    //IdxFilterPopupWindowFilterValueContainer
    procedure AssignOptions(AOptions: TObject); virtual;
    procedure Closed; virtual;
    procedure FocusStartControl; virtual;
    procedure SysPanelButtonAction(AHitCode: Integer); virtual;
    function SysPanelButtonEnabled(AHitCode: Integer): Boolean; virtual;
    function SysPanelType: TdxFilterPopupWindowSysPanelType; virtual;

    procedure AdjustSize; override;
    procedure ApplyFilterOnButtonClick; override;
    procedure BoundsChanged; override;
    function CreateListBox: TdxFilterValueContainerListBox; override;
    procedure LayoutChanged; override;

    property ListBox: TdxFilterPopupWindowFilterValueContainerListBox read GetListBox;
    property Popup: TdxFilterPopupWindow read GetPopup;
    property VisibleItemCount: Integer read GetVisibleItemCount write SetVisibleItemCount;
    property VisibleWidth: Integer read GetVisibleWidth write SetVisibleWidth;
  end;

  { TdxFilterPopupWindowFilterValueContainer }

  TdxFilterPopupWindowExcelFilterValueContainer = class(TdxExcelFilterValueContainer,
    IdxFilterPopupWindowFilterValueContainer)
  strict private
    function GetPopup: TdxFilterPopupWindow;
  protected
    //IdxFilterPopupWindowFilterValueContainer
    procedure AssignOptions(AOptions: TObject); virtual;
    procedure Closed; virtual;
    procedure FocusStartControl; virtual;
    procedure SysPanelButtonAction(AHitCode: Integer); virtual;
    function SysPanelButtonEnabled(AHitCode: Integer): Boolean; virtual;
    function SysPanelType: TdxFilterPopupWindowSysPanelType; virtual;

    procedure ApplyModeChanged; override;
    function GetListBoxClass: TdxExcelFilterValueContainerListBoxClass; override;
    procedure ModifiedChanged; override;
    procedure LayoutKeyDown(ASender: TObject; var Key: Word; Shift: TShiftState); override;

    property Popup: TdxFilterPopupWindow read GetPopup;
  end;

  { TdxFilterPopupWindowItemViewInfo }

  TdxFilterPopupWindowCustomItemViewInfo = class(TcxIUnknownObject,
    IcxMouseTrackingCaller,
    IcxMouseTrackingCaller2)
  strict private
    FBounds: TRect;
    FFilterPopup: TdxFilterPopupWindow;
    FState: TcxButtonState;

    function GetPainter: TcxCustomLookAndFeelPainter;
    function GetScaleFactor: TdxScaleFactor;
    function GetState: TcxButtonState;
    procedure SetState(AValue: TcxButtonState);
  protected
    { IcxMouseTrackingCaller }
    procedure MouseLeave;
    { IcxMouseTrackingCaller2 }
    function PtInCaller(const P: TPoint): Boolean;

    function CanChangeState: Boolean; virtual;
    procedure Click; virtual;
    function GetEnabled: Boolean; virtual;
    function GetHeight: Integer; virtual;
    function GetHitCode: Integer; virtual;
    function GetHitTest(const APoint: TPoint): Integer; virtual;
    function GetWidth: Integer; virtual;
    function HasPoint(const APoint: TPoint): Boolean; virtual;
    procedure Invalidate; virtual;
    procedure RightToLeftConversion(ABounds: TRect); virtual;
    procedure StateChanged; virtual;

    property Bounds: TRect read FBounds;
    property FilterPopup: TdxFilterPopupWindow read FFilterPopup;
    property Height: Integer read GetHeight;
    property Painter: TcxCustomLookAndFeelPainter read GetPainter;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property State: TcxButtonState read GetState write SetState;
    property Width: Integer read GetWidth;
  public
    constructor Create(AFilterPopup: TdxFilterPopupWindow); virtual;

    procedure Calculate(ALeftBound, ATopBound: Integer); virtual;
    procedure Paint(ACanvas: TcxCanvas); virtual;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
  end;

  { TdxFilterPopupWindowSysPanelButtonViewInfo }

  TdxFilterPopupWindowSysPanelButtonViewInfo = class(TdxFilterPopupWindowCustomItemViewInfo)
  protected
    function CanChangeState: Boolean; override;
    procedure Click; override;
    function GetEnabled: Boolean; override;
    function GetHorzAlign: TLeftRight; virtual;
    function GetHeight: Integer; override;
    function GetMinHeight: Integer; virtual;
    function GetMinWidth: Integer; virtual;
    function GetText: string; virtual;
    function GetTextOffset: Integer; virtual;
    function GetWidth: Integer; override;

    property Text: string read GetText;
  public
    procedure Paint(ACanvas: TcxCanvas); override;
  end;
  TdxFilterPopupWindowSysPanelButtonViewInfoClass = class of TdxFilterPopupWindowSysPanelButtonViewInfo;

  { TdxFilterPopupWindowSysPanelClearButtonViewInfo }

  TdxFilterPopupWindowSysPanelClearButtonViewInfo = class(TdxFilterPopupWindowSysPanelButtonViewInfo)
  protected
    function GetHitCode: Integer; override;
    function GetHorzAlign: TLeftRight; override;
    function GetText: string; override;
  end;

  { TdxFilterPopupWindowSysPanelCloseButtonViewInfo }

  TdxFilterPopupWindowSysPanelCloseButtonViewInfo = class(TdxFilterPopupWindowSysPanelButtonViewInfo)
  protected
    function GetHitCode: Integer; override;
    function GetText: string; override;
  end;

  { TdxFilterPopupWindowSysPanelCancelButtonViewInfo }

  TdxFilterPopupWindowSysPanelCancelButtonViewInfo = class(TdxFilterPopupWindowSysPanelCloseButtonViewInfo)
  protected
    function GetHitCode: Integer; override;
    function GetText: string; override;
  end;

  { TdxFilterPopupWindowSysPanelOkButtonViewInfo }

  TdxFilterPopupWindowSysPanelOkButtonViewInfo = class(TdxFilterPopupWindowSysPanelButtonViewInfo)
  protected
    function GetHitCode: Integer; override;
    function GetText: string; override;
  end;

  { TdxFilterPopupWindowSysPanelViewInfo }

  TdxFilterPopupWindowSysPanelViewInfo = class(TdxFilterPopupWindowCustomItemViewInfo)
  strict private
    FButtons: TObjectList<TdxFilterPopupWindowSysPanelButtonViewInfo>;
    FSeparatorBounds: TRect;
    FSizeGripBounds: TRect;

    function GetSeparatorSize: Integer;
    function GetSizeGripSize: Integer;
  protected
    procedure AddButton(AClass: TdxFilterPopupWindowSysPanelButtonViewInfoClass);
    procedure CalculateButtons; virtual;
    function CalculateSeparatorBounds: TRect; virtual;
    function CalculateSizeGripBounds: TRect; virtual;
    procedure DrawBackground(ACanvas: TcxCanvas); virtual;
    procedure DrawButtons(ACanvas: TcxCanvas); virtual;
    procedure DrawSeparator(ACanvas: TcxCanvas); virtual;
    procedure DrawSizeGrip(ACanvas: TcxCanvas); virtual;
    function GetButtonIndent: Integer; virtual;
    function GetContentIndent: Integer; virtual;
    function GetHeight: Integer; override;
    function GetHitButton(const APoint: TPoint): TdxFilterPopupWindowSysPanelButtonViewInfo; virtual;
    function GetHitCode: Integer; override;
    function GetHitTest(const APoint: TPoint): Integer; override;
    function GetSizeGripCorner: TdxCorner; virtual;
    function GetWidth: Integer; override;
    function IsCancelButtonVisible: Boolean; virtual;
    function IsClearButtonVisible: Boolean; virtual;
    function IsCloseButtonVisible: Boolean; virtual;
    function IsOkButtonVisible: Boolean; virtual;
    procedure RightToLeftConversion(ABounds: TRect); override;

    property Buttons: TObjectList<TdxFilterPopupWindowSysPanelButtonViewInfo> read FButtons;
    property SeparatorBounds: TRect read FSeparatorBounds;
    property SeparatorSize: Integer read GetSeparatorSize;
    property SizeGripBounds: TRect read FSizeGripBounds;
    property SizeGripSize: Integer read GetSizeGripSize;
  public
    constructor Create(AFilterPopup: TdxFilterPopupWindow); override;
    destructor Destroy; override;

    procedure Calculate(ALeftBound, ATopBound: Integer); override;
    procedure Paint(ACanvas: TcxCanvas); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  end;

  { TdxFilterPopupWindowViewInfo }

  TdxFilterPopupWindowViewInfo = class(TdxUIElementPopupWindowViewInfo);

  { TdxFilterPopupWindow }

  TdxFilterPopupWindow = class(TdxUIElementPopupWindow)
  strict private
    FActualMode: TdxFilterPopupWindowActualMode;
    FCaptureSizeGrip: Boolean;
    FFilterOwnerIntf: IdxFilterPopupWindowOwner;
    FFilterValueContainer: TdxCustomFilterValueContainer;
    FMode: TdxFilterPopupWindowMode;
    FNeedFilteringAppliedOnClose: Boolean;
    FSysPanelViewInfo: TdxFilterPopupWindowSysPanelViewInfo;

    function GetFilterValueContainerIntf: IdxFilterPopupWindowFilterValueContainer;
    function GetLinkComponent: TComponent;
    function GetViewInfo: TdxFilterPopupWindowViewInfo;
    procedure SetActualMode(AValue: TdxFilterPopupWindowActualMode);

    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
  protected
    procedure ActualModeChanged; virtual;
    procedure AdjustBounds; virtual;
    procedure AdjustableChanged; override;
    procedure BordersChanged; override;
    procedure BoundsChanged; virtual;
    function CalculateSize: TSize; override;
    procedure CreateFilterValueContainer; virtual;
    procedure DoShowed; override;
    procedure DrawSysPanel; virtual;
    function CreateSysPanelViewInfo: TdxFilterPopupWindowSysPanelViewInfo; virtual;
    function GetDefaultBounds: TRect; virtual;
    function GetFilterValueContainerClass: TdxCustomFilterValueContainerClass; virtual;
    function GetHitTest(const APoint: TPoint): Integer; virtual;
    function GetMinSize: TSize; virtual;
    function GetSysPanelType: TdxFilterPopupWindowSysPanelType; virtual;
    function HasSysPanel: Boolean; virtual;
    procedure InitFilterValueContainer; virtual;
    procedure InitPopup; override;
    procedure OwnerChanged; override;
    procedure Paint; override;
    procedure RecreateFilterValueContainer;
    procedure RecreateSysPanel;
    procedure ResetFilterValueContainerPosition; virtual;
    procedure Resize; override;
    procedure ResizeByDragging(X, Y: Integer); virtual;
    function ShowSysPanel: Boolean; virtual;
    procedure SetMode(AMode: TdxFilterPopupWindowMode); virtual;
    procedure SysPanelButtonAction(AHitCode: Integer); virtual;
    function SysPanelButtonEnabled(AHitCode: Integer): Boolean; virtual;
    procedure UpdateFilterValueContainerByOwner; virtual;
    procedure UpdateLayout; virtual;

    property ActualMode: TdxFilterPopupWindowActualMode read FActualMode write SetActualMode;
    property CaptureSizeGrip: Boolean read FCaptureSizeGrip;
    property FilterOwnerIntf: IdxFilterPopupWindowOwner read FFilterOwnerIntf;
    property FilterValueContainer: TdxCustomFilterValueContainer read FFilterValueContainer;
    property FilterValueContainerIntf: IdxFilterPopupWindowFilterValueContainer read GetFilterValueContainerIntf;
    property NeedFilteringAppliedOnClose: Boolean read FNeedFilteringAppliedOnClose write FNeedFilteringAppliedOnClose;
    property SysPanelViewInfo: TdxFilterPopupWindowSysPanelViewInfo read FSysPanelViewInfo;
  public
    constructor Create(AOwnerControl: TWinControl); override;
    destructor Destroy; override;

    procedure CloseUp; override;
    function GetViewInfoClass: TcxContainerViewInfoClass; override;
    procedure Popup; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    property LinkComponent: TComponent read GetLinkComponent;
    property Mode: TdxFilterPopupWindowMode read FMode;
    property ViewInfo: TdxFilterPopupWindowViewInfo read GetViewInfo;
  end;

  TdxFilterPopupWindowClass = class of TdxFilterPopupWindow;

  function dxGetFilterPopupActualMode(AMode: TdxFilterPopupWindowMode): TdxFilterPopupWindowActualMode; //for internal use

implementation

uses
  Types, SysUtils, Graphics, Math, Forms, cxDrawTextUtils, cxEdit, cxEditConsts;

const
  htNone      = -1;
  htSysPanel = htNone + 1;
  htSizeGrip  = htSysPanel + 1;
  htClearButton  = htSizeGrip + 1;
  htCloseButton  = htClearButton + 1;
  htCancelButton  = htCloseButton + 1;
  htOkButton = htCancelButton + 1;

  dxFilterPopupWindowSysPanelHeight = 50;
  dxFilterPopupWindowSysPanelSizeGripSize = 20;
  dxFilterPopupWindowSysPanelButtonMinHeight = 26;
  dxFilterPopupWindowSysPanelButtonMinWidht = 88;
  dxFilterPopupWindowSysPanelButtonIndent = 5;
  dxFilterPopupWindowSysPanelContentIndent = 12;
  dxFilterPopupWindowSysPanelButtonTextOffset = 2;

  dxExcelFilterPopupDefaultHeight = 278;
  dxExcelFilterPopupDefaultWidth = 320;

function dxGetFilterPopupActualMode(AMode: TdxFilterPopupWindowMode): TdxFilterPopupWindowActualMode;
begin
  if (AMode = fpmExcel) or (AMode = fpmDefault) and (dxDefaultFilterPopupWindowMode = fpmExcel) then
    Result := fpmExcel
  else
    Result := fpmClassic;
end;

{ TdxFilterPopupWindowFilterValueContainerListBox }

procedure TdxFilterPopupWindowFilterValueContainerListBox.ApplyCheckAction(AItemIndex: Integer);
begin
  inherited ApplyCheckAction(AItemIndex);
  if FilterValueContainer.NeedImmediateApply then
    FilterValueContainer.Popup.NeedFilteringAppliedOnClose := True;
end;

procedure TdxFilterPopupWindowFilterValueContainerListBox.ApplyClickAction(AItemIndex: Integer);
begin
  inherited ApplyClickAction(AItemIndex);
  FilterValueContainer.Popup.CloseUp;
end;

procedure TdxFilterPopupWindowFilterValueContainerListBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Key = VK_ESCAPE then
  begin
    Key := 0;
    FilterValueContainer.Popup.CloseUp;
  end;
end;

function TdxFilterPopupWindowFilterValueContainerListBox.GetFilterValueContainer: TdxFilterPopupWindowFilterValueContainer;
begin
  Result := TdxFilterPopupWindowFilterValueContainer(inherited FilterValueContainer);
end;

{ TdxFilterPopupWindowExcelFilterValueContainerListBox }

procedure TdxFilterPopupWindowExcelFilterValueContainerListBox.ApplyCheckAction(AItemIndex: Integer);
begin
  inherited ApplyCheckAction(AItemIndex);
  if FilterValueContainer.NeedImmediateApply then
    FilterValueContainer.Popup.NeedFilteringAppliedOnClose := True;
end;

function TdxFilterPopupWindowExcelFilterValueContainerListBox.GetFilterValueContainer:
  TdxFilterPopupWindowExcelFilterValueContainer;
begin
  Result := TdxFilterPopupWindowExcelFilterValueContainer(inherited FilterValueContainer);
end;

{ TdxFilterPopupWindowFilterValueContainer }

procedure TdxFilterPopupWindowFilterValueContainer.AdjustSize;
var
  AHeight, AWidth: Integer;
begin
  AHeight := GetBorderExtent.Top + GetBorderExtent.Bottom;
  if HasIncrementalFiltering then
    Inc(AHeight, SearchEditOffsets.Top + SearchEditOffsets.Bottom + SearchEdit.Height);
  if IsSearchInfoPanelVisible then
    Inc(AHeight, ViewInfo.GetSearchInfoPanelHeight)
  else
    Inc(AHeight, ListBox.CalculateAutoHeight);
  if IsButtonVisible then
    Inc(AHeight, Button.GetOptimalSize.cy);
  Height := AHeight;
  AWidth := ListBox.CalculateAutoWidth + GetBorderExtent.Left + GetBorderExtent.Right;
  Width := Max(AWidth, Popup.ClientMinWidth);
end;

procedure TdxFilterPopupWindowFilterValueContainer.ApplyFilterOnButtonClick;
begin
  inherited ApplyFilterOnButtonClick;
  Popup.CloseUp;
end;

procedure TdxFilterPopupWindowFilterValueContainer.BoundsChanged;
begin
  inherited BoundsChanged;
  Popup.AdjustBounds;
end;

function TdxFilterPopupWindowFilterValueContainer.CreateListBox: TdxFilterValueContainerListBox;
begin
  Result := TdxFilterPopupWindowFilterValueContainerListBox.Create(Self);
end;

procedure TdxFilterPopupWindowFilterValueContainer.LayoutChanged;
begin
  if not IsLocked then
    AdjustSize;
end;

procedure TdxFilterPopupWindowFilterValueContainer.AssignOptions(AOptions: TObject);
var
  AOptionsIntf: IdxFilterPopupWindowOptions;
begin
  if Supports(AOptions, IdxFilterPopupWindowOptions, AOptionsIntf) then
  begin
    ApplyMode := AOptionsIntf.GetApplyMode;
    ButtonCaption := AOptionsIntf.GetButtonCaption;
    IncrementalFiltering := AOptionsIntf.GetIncrementalFiltering;
    IncrementalFilteringOptions := AOptionsIntf.GetIncrementalFilteringOptions;
    ShowCheckBoxes := AOptionsIntf.GetShowCheckBoxes;
    VisibleItemCount := AOptionsIntf.GetVisibleItemCount;
    VisibleWidth := AOptionsIntf.GetWidth;
  end;
end;

procedure TdxFilterPopupWindowFilterValueContainer.Closed;
begin
  ResetSearchText;
end;

procedure TdxFilterPopupWindowFilterValueContainer.FocusStartControl;
begin
//do nothing
end;

procedure TdxFilterPopupWindowFilterValueContainer.SysPanelButtonAction(AHitCode: Integer);
begin
//do nothing
end;

function TdxFilterPopupWindowFilterValueContainer.SysPanelButtonEnabled(AHitCode: Integer): Boolean;
begin
  Result := False;
end;

function TdxFilterPopupWindowFilterValueContainer.SysPanelType: TdxFilterPopupWindowSysPanelType;
begin
  Result := sptNone;
end;

function TdxFilterPopupWindowFilterValueContainer.GetListBox: TdxFilterPopupWindowFilterValueContainerListBox;
begin
  Result := TdxFilterPopupWindowFilterValueContainerListBox(inherited ListBox);
end;

function TdxFilterPopupWindowFilterValueContainer.GetPopup: TdxFilterPopupWindow;
begin
  Result := TdxFilterPopupWindow(Owner);
end;

function TdxFilterPopupWindowFilterValueContainer.GetVisibleItemCount: Integer;
begin
  Result := ListBox.VisibleItemCount;
end;

function TdxFilterPopupWindowFilterValueContainer.GetVisibleWidth: Integer;
begin
  Result := ListBox.VisibleWidth;
end;

procedure TdxFilterPopupWindowFilterValueContainer.SetVisibleItemCount(AValue: Integer);
begin
  if VisibleItemCount <> AValue then
  begin
    ListBox.VisibleItemCount := AValue;
    LayoutChanged;
  end;
end;

procedure TdxFilterPopupWindowFilterValueContainer.SetVisibleWidth(AValue: Integer);
begin
  if VisibleWidth <> AValue then
  begin
    ListBox.VisibleWidth := AValue;
    LayoutChanged;
  end;
end;

{ TdxFilterPopupWindowExcelFilterValueContainer }

procedure TdxFilterPopupWindowExcelFilterValueContainer.AssignOptions(AOptions: TObject);
var
  AOptionsIntf: IdxExcelFilterPopupWindowOptions;
begin
  if Supports(AOptions, IdxExcelFilterPopupWindowOptions, AOptionsIntf) then
  begin
    ApplyChanges := AOptionsIntf.GetApplyChanges;
    DateTimeValuesPageType := AOptionsIntf.GetDateTimeValuesPageType;
    DefaultPage := AOptionsIntf.GetDefaultPage;
    NumericValuesPageType := AOptionsIntf.GetNumericValuesPageType;
  end;
end;

procedure TdxFilterPopupWindowExcelFilterValueContainer.Closed;
begin
//do nothing
end;

procedure TdxFilterPopupWindowExcelFilterValueContainer.FocusStartControl;
begin
  FocusLayoutMainItem;
end;

procedure TdxFilterPopupWindowExcelFilterValueContainer.SysPanelButtonAction(AHitCode: Integer);
begin
  case AHitCode of
    htClearButton:
      ResetFilter;
    htOkButton:
      ApplyFilter;
  end;
end;

function TdxFilterPopupWindowExcelFilterValueContainer.SysPanelButtonEnabled(AHitCode: Integer): Boolean;
begin
  Result := True;
  case AHitCode of
    htClearButton:
      Result := Filter.FindItemByItemLink(FilterItemLink) <> nil;
    htOkButton:
      Result := IsModified;
  end;
end;

function TdxFilterPopupWindowExcelFilterValueContainer.SysPanelType: TdxFilterPopupWindowSysPanelType;
begin
  if NeedImmediateApply then
    Result := sptClearClose
  else
    Result := sptClearOkCancel;
end;

procedure TdxFilterPopupWindowExcelFilterValueContainer.ApplyModeChanged;
begin
  inherited ApplyModeChanged;
  Popup.RecreateSysPanel;
  Popup.UpdateLayout;
end;

function TdxFilterPopupWindowExcelFilterValueContainer.GetListBoxClass: TdxExcelFilterValueContainerListBoxClass;
begin
  Result := TdxFilterPopupWindowExcelFilterValueContainerListBox;
end;

procedure TdxFilterPopupWindowExcelFilterValueContainer.ModifiedChanged;
begin
  inherited ModifiedChanged;
  Popup.SysPanelViewInfo.Invalidate;
end;

procedure TdxFilterPopupWindowExcelFilterValueContainer.LayoutKeyDown(ASender: TObject; var Key: Word; Shift: TShiftState);
var
  AEdit: TcxCustomEdit;
begin
  inherited LayoutKeyDown(ASender, Key, Shift);
  AEdit := Safe<TcxCustomEdit>.Cast(ASender);
  if (Key = VK_ESCAPE) and ((AEdit = nil) or not AEdit.ModifiedAfterEnter and not AEdit.HasPopupWindow) then
  begin
    Key := 0;
    Popup.ClosePopup;
  end;
end;

function TdxFilterPopupWindowExcelFilterValueContainer.GetPopup: TdxFilterPopupWindow;
begin
  Result := TdxFilterPopupWindow(Owner);
end;

{ TdxFilterPopupWindowItemViewInfo }

constructor TdxFilterPopupWindowCustomItemViewInfo.Create(AFilterPopup: TdxFilterPopupWindow);
begin
  inherited Create;
  FFilterPopup := AFilterPopup;
  FState := cxbsNormal;
end;

procedure TdxFilterPopupWindowCustomItemViewInfo.Calculate(ALeftBound, ATopBound: Integer);
begin
  FBounds := cxRectSetLeft(cxEmptyRect, ALeftBound, Width);
  FBounds := cxRectSetTop(FBounds, ATopBound, Height);
end;

procedure TdxFilterPopupWindowCustomItemViewInfo.Paint(ACanvas: TcxCanvas);
begin
//do nothing
end;

procedure TdxFilterPopupWindowCustomItemViewInfo.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  State := cxbsPressed;
end;

procedure TdxFilterPopupWindowCustomItemViewInfo.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if State <> cxbsPressed then
    State := cxbsHot;
end;

procedure TdxFilterPopupWindowCustomItemViewInfo.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AIsClick: Boolean;
begin
  AIsClick := (State = cxbsPressed) and (Button = mbLeft);
  State := cxbsNormal;
  if AIsClick then
    Click;
end;

procedure TdxFilterPopupWindowCustomItemViewInfo.MouseLeave;
begin
  State := cxbsNormal;
end;

function TdxFilterPopupWindowCustomItemViewInfo.PtInCaller(const P: TPoint): Boolean;
begin
  Result := HasPoint(P);
end;

function TdxFilterPopupWindowCustomItemViewInfo.CanChangeState: Boolean;
begin
  Result := False;
end;

procedure TdxFilterPopupWindowCustomItemViewInfo.Click;
begin
//do nothing
end;

function TdxFilterPopupWindowCustomItemViewInfo.GetEnabled: Boolean;
begin
  Result := True;
end;

function TdxFilterPopupWindowCustomItemViewInfo.GetHeight: Integer;
begin
  Result := 0;
end;

function TdxFilterPopupWindowCustomItemViewInfo.GetHitCode: Integer;
begin
  Result := htNone;
end;

function TdxFilterPopupWindowCustomItemViewInfo.GetHitTest(const APoint: TPoint): Integer;
begin
  if HasPoint(APoint) then
    Result := GetHitCode
  else
    Result := htNone;
end;

function TdxFilterPopupWindowCustomItemViewInfo.GetWidth: Integer;
begin
  Result := 0;
end;

function TdxFilterPopupWindowCustomItemViewInfo.HasPoint(const APoint: TPoint): Boolean;
begin
  Result := PtInRect(Bounds, APoint);
end;

procedure TdxFilterPopupWindowCustomItemViewInfo.Invalidate;
begin
  cxInvalidateRect(FilterPopup, Bounds);
end;

procedure TdxFilterPopupWindowCustomItemViewInfo.RightToLeftConversion(ABounds: TRect);
begin
  FBounds := TdxRightToLeftLayoutConverter.ConvertRect(Bounds, ABounds);
end;

procedure TdxFilterPopupWindowCustomItemViewInfo.StateChanged;
begin
  Invalidate;
  case State of
    cxbsNormal:
      EndMouseTracking(Self);
    cxbsHot:
      BeginMouseTracking(FilterPopup, Bounds, Self);
  end;
end;

function TdxFilterPopupWindowCustomItemViewInfo.GetPainter: TcxCustomLookAndFeelPainter;
begin
  Result := FilterPopup.LookAndFeel.Painter;
end;

function TdxFilterPopupWindowCustomItemViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := FilterPopup.ScaleFactor;
end;

function TdxFilterPopupWindowCustomItemViewInfo.GetState: TcxButtonState;
begin
  if GetEnabled then
    Result := FState
  else
    Result := cxbsDisabled;
end;

procedure TdxFilterPopupWindowCustomItemViewInfo.SetState(AValue: TcxButtonState);
begin
  if (State <> AValue) and CanChangeState then
  begin
    FState := AValue;
    StateChanged;
  end;
end;

{ TdxFilterPopupWindowSysPanelButtonViewInfo }

procedure TdxFilterPopupWindowSysPanelButtonViewInfo.Paint(ACanvas: TcxCanvas);
begin
  inherited Paint(ACanvas);
  Painter.DrawScaledButton(ACanvas, Bounds, Text, State, ScaleFactor);
end;

function TdxFilterPopupWindowSysPanelButtonViewInfo.CanChangeState: Boolean;
begin
  Result := GetEnabled;
end;

procedure TdxFilterPopupWindowSysPanelButtonViewInfo.Click;
begin
  FilterPopup.SysPanelButtonAction(GetHitCode);
end;

function TdxFilterPopupWindowSysPanelButtonViewInfo.GetEnabled: Boolean;
begin
  Result := FilterPopup.SysPanelButtonEnabled(GetHitCode);
end;

function TdxFilterPopupWindowSysPanelButtonViewInfo.GetHorzAlign: TLeftRight;
begin
  Result := taRightJustify;
end;

function TdxFilterPopupWindowSysPanelButtonViewInfo.GetHeight: Integer;
begin
  Result := cxTextHeight(FilterPopup.Font) + 2 * Painter.ScaledButtonTextOffset(ScaleFactor) + 2 * Painter.ButtonBorderSize;
  Result := Max(Result, GetMinHeight);
end;

function TdxFilterPopupWindowSysPanelButtonViewInfo.GetMinHeight: Integer;
begin
  Result := ScaleFactor.Apply(dxFilterPopupWindowSysPanelButtonMinHeight)
end;

function TdxFilterPopupWindowSysPanelButtonViewInfo.GetMinWidth: Integer;
begin
  Result := ScaleFactor.Apply(dxFilterPopupWindowSysPanelButtonMinWidht);
end;

function TdxFilterPopupWindowSysPanelButtonViewInfo.GetText: string;
begin
  Result := '';
end;

function TdxFilterPopupWindowSysPanelButtonViewInfo.GetTextOffset: Integer;
begin
  Result := ScaleFactor.Apply(dxFilterPopupWindowSysPanelButtonTextOffset);
end;

function TdxFilterPopupWindowSysPanelButtonViewInfo.GetWidth: Integer;
begin
  Result := cxTextWidth(FilterPopup.Font, Text) + 2 * Painter.ButtonBorderSize + 2 * Painter.ScaledButtonTextOffset(ScaleFactor);
  Result := Max(Result, GetMinWidth);
end;

{ TdxFilterPopupWindowSysPanelClearButtonViewInfo }

function TdxFilterPopupWindowSysPanelClearButtonViewInfo.GetHitCode: Integer;
begin
  Result := htClearButton;
end;

function TdxFilterPopupWindowSysPanelClearButtonViewInfo.GetHorzAlign: TLeftRight;
begin
  Result := taLeftJustify;
end;

function TdxFilterPopupWindowSysPanelClearButtonViewInfo.GetText: string;
begin
  Result := cxGetResourceString(@sdxFilterPopupWindowClearButtonCaption);
end;

{ TdxFilterPopupWindowSysPanelCloseButtonViewInfo }

function TdxFilterPopupWindowSysPanelCloseButtonViewInfo.GetHitCode: Integer;
begin
  Result := htCloseButton;
end;

function TdxFilterPopupWindowSysPanelCloseButtonViewInfo.GetText: string;
begin
  Result := cxGetResourceString(@sdxFilterPopupWindowCloseButtonCaption);
end;

{ TdxFilterPopupWindowSysPanelCancelButtonViewInfo }

function TdxFilterPopupWindowSysPanelCancelButtonViewInfo.GetHitCode: Integer;
begin
  Result := htCancelButton;
end;

function TdxFilterPopupWindowSysPanelCancelButtonViewInfo.GetText: string;
begin
  Result := cxGetResourceString(@sdxFilterPopupWindowCancelButtonCaption);
end;

{ TdxFilterPopupWindowSysPanelOkButtonViewInfo }

function TdxFilterPopupWindowSysPanelOkButtonViewInfo.GetHitCode: Integer;
begin
  Result := htOkButton;
end;

function TdxFilterPopupWindowSysPanelOkButtonViewInfo.GetText: string;
begin
  Result := cxGetResourceString(@sdxFilterPopupWindowOKButtonCaption);
end;

{ TdxFilterPopupWindowSysPanelViewInfo }

constructor TdxFilterPopupWindowSysPanelViewInfo.Create(AFilterPopup: TdxFilterPopupWindow);
begin
  inherited Create(AFilterPopup);
  FButtons := TObjectList<TdxFilterPopupWindowSysPanelButtonViewInfo>.Create;
  if IsClearButtonVisible then
    AddButton(TdxFilterPopupWindowSysPanelClearButtonViewInfo);
  if IsCloseButtonVisible then
    AddButton(TdxFilterPopupWindowSysPanelCloseButtonViewInfo);
  if IsCancelButtonVisible then
    AddButton(TdxFilterPopupWindowSysPanelCancelButtonViewInfo);
  if IsOkButtonVisible then
    AddButton(TdxFilterPopupWindowSysPanelOkButtonViewInfo);
end;

destructor TdxFilterPopupWindowSysPanelViewInfo.Destroy;
begin
  FreeAndNil(FButtons);
  inherited Destroy;
end;

procedure TdxFilterPopupWindowSysPanelViewInfo.Calculate(ALeftBound, ATopBound: Integer);
begin
  inherited Calculate(ALeftBound, ATopBound);
  FSeparatorBounds := CalculateSeparatorBounds;
  if Buttons.Count > 0 then
    CalculateButtons;
  FSizeGripBounds := CalculateSizeGripBounds;
end;

procedure TdxFilterPopupWindowSysPanelViewInfo.Paint(ACanvas: TcxCanvas);
begin
  inherited Paint(ACanvas);
  DrawBackground(ACanvas);
  DrawSeparator(ACanvas);
  DrawSizeGrip(ACanvas);
  DrawButtons(ACanvas);
end;

procedure TdxFilterPopupWindowSysPanelViewInfo.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AHitButton: TdxFilterPopupWindowSysPanelButtonViewInfo;
begin
  AHitButton := GetHitButton(cxPoint(X, Y));
  if AHitButton <> nil then
    AHitButton.MouseDown(Button, Shift, X, Y)
  else
    inherited MouseDown(Button, Shift, X, Y);
end;

procedure TdxFilterPopupWindowSysPanelViewInfo.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  AHitButton: TdxFilterPopupWindowSysPanelButtonViewInfo;
begin
  AHitButton := GetHitButton(cxPoint(X, Y));
  if AHitButton <> nil then
    AHitButton.MouseMove(Shift, X, Y)
  else
    inherited MouseMove(Shift, X, Y);
end;

procedure TdxFilterPopupWindowSysPanelViewInfo.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AHitButton: TdxFilterPopupWindowSysPanelButtonViewInfo;
begin
  AHitButton := GetHitButton(cxPoint(X, Y));
  if AHitButton <> nil then
    AHitButton.MouseUp(Button, Shift, X, Y)
  else
    inherited MouseUp(Button, Shift, X, Y);
end;

procedure TdxFilterPopupWindowSysPanelViewInfo.AddButton(AClass: TdxFilterPopupWindowSysPanelButtonViewInfoClass);
begin
  FButtons.Add(AClass.Create(FilterPopup));
end;

procedure TdxFilterPopupWindowSysPanelViewInfo.CalculateButtons;
var
  I: Integer;
  AButtonsPlaceBounds: TRect;
  AButton: TdxFilterPopupWindowSysPanelButtonViewInfo;
begin
  AButtonsPlaceBounds := Bounds;
  AButtonsPlaceBounds.Left := AButtonsPlaceBounds.Left + GetContentIndent;
  AButtonsPlaceBounds.Right := AButtonsPlaceBounds.Right - GetContentIndent;
  AButtonsPlaceBounds := cxRectCenterVertically(AButtonsPlaceBounds, Buttons.First.Height);
  for I := 0 to Buttons.Count - 1 do
  begin
    AButton := Buttons[I];
    if AButton.GetHorzAlign = taLeftJustify then
    begin
      AButton.Calculate(AButtonsPlaceBounds.Left, AButtonsPlaceBounds.Top);
      AButtonsPlaceBounds.Left := AButton.Bounds.Right + GetButtonIndent;
    end
    else
    begin
      AButton.Calculate(AButtonsPlaceBounds.Right - AButton.Width, AButtonsPlaceBounds.Top);
      AButtonsPlaceBounds.Right := AButton.Bounds.Left - GetButtonIndent;
    end;
  end;
end;

function TdxFilterPopupWindowSysPanelViewInfo.CalculateSeparatorBounds: TRect;
begin
  Result := Bounds;
  Result.Bottom := Result.Top + SeparatorSize;
end;

function TdxFilterPopupWindowSysPanelViewInfo.CalculateSizeGripBounds: TRect;
begin
  Result := Bounds;
  Result.Top := Result.Bottom - SizeGripSize;
  Result.Left := Result.Right - SizeGripSize;
end;

procedure TdxFilterPopupWindowSysPanelViewInfo.DrawBackground(ACanvas: TcxCanvas);
begin
  Painter.DrawWindowContent(ACanvas, Bounds);
end;

procedure TdxFilterPopupWindowSysPanelViewInfo.DrawButtons(ACanvas: TcxCanvas);
var
  I: Integer;
begin
  for I := 0 to Buttons.Count - 1 do
    Buttons[I].Paint(ACanvas);
end;

procedure TdxFilterPopupWindowSysPanelViewInfo.DrawSeparator(ACanvas: TcxCanvas);
begin
  Painter.DrawSeparator(ACanvas, SeparatorBounds, False);
end;

procedure TdxFilterPopupWindowSysPanelViewInfo.DrawSizeGrip(ACanvas: TcxCanvas);
begin
  Painter.DrawScaledSizeGrip(ACanvas, SizeGripBounds, ScaleFactor, clDefault, GetSizeGripCorner);
end;

function TdxFilterPopupWindowSysPanelViewInfo.GetButtonIndent: Integer;
begin
  Result := ScaleFactor.Apply(dxFilterPopupWindowSysPanelButtonIndent);
end;

function TdxFilterPopupWindowSysPanelViewInfo.GetContentIndent: Integer;
begin
  Result := ScaleFactor.Apply(dxFilterPopupWindowSysPanelContentIndent);
end;

function TdxFilterPopupWindowSysPanelViewInfo.GetHeight: Integer;
begin
  Result := ScaleFactor.Apply(dxFilterPopupWindowSysPanelHeight);
end;

function TdxFilterPopupWindowSysPanelViewInfo.GetHitButton(const APoint: TPoint): TdxFilterPopupWindowSysPanelButtonViewInfo;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Buttons.Count - 1 do
    if Buttons[I].GetHitTest(APoint) <> htNone then
      Exit(Buttons[I]);
end;

function TdxFilterPopupWindowSysPanelViewInfo.GetHitCode: Integer;
begin
  Result := htSysPanel;
end;

function TdxFilterPopupWindowSysPanelViewInfo.IsCancelButtonVisible: Boolean;
begin
  Result := FilterPopup.GetSysPanelType = sptClearOkCancel;
end;

function TdxFilterPopupWindowSysPanelViewInfo.IsClearButtonVisible: Boolean;
begin
  Result := True;
end;

function TdxFilterPopupWindowSysPanelViewInfo.IsCloseButtonVisible: Boolean;
begin
  Result := FilterPopup.GetSysPanelType = sptClearClose;
end;

function TdxFilterPopupWindowSysPanelViewInfo.IsOkButtonVisible: Boolean;
begin
  Result := FilterPopup.GetSysPanelType = sptClearOkCancel;
end;

procedure TdxFilterPopupWindowSysPanelViewInfo.RightToLeftConversion(ABounds: TRect);
var
  I: Integer;
begin
  inherited RightToLeftConversion(ABounds);
  FSeparatorBounds := TdxRightToLeftLayoutConverter.ConvertRect(SeparatorBounds, ABounds);
  for I := 0 to Buttons.Count - 1 do
    Buttons[I].RightToLeftConversion(ABounds);
  FSizeGripBounds := TdxRightToLeftLayoutConverter.ConvertRect(SizeGripBounds, ABounds);
end;

function TdxFilterPopupWindowSysPanelViewInfo.GetHitTest(const APoint: TPoint): Integer;
var
  I, AHitCode: Integer;
begin
  Result := inherited GetHitTest(APoint);
  if Result = htNone then
    Exit;
  for I := 0 to Buttons.Count - 1 do
  begin
    AHitCode := Buttons[I].GetHitTest(APoint);
    if AHitCode <> htNone then
      Exit(AHitCode);
  end;
  if PtInRect(SizeGripBounds, APoint) then
    Result := htSizeGrip;
end;

function TdxFilterPopupWindowSysPanelViewInfo.GetSizeGripCorner: TdxCorner;
begin
  if FilterPopup.UseRightToLeftAlignment then
    Result := coBottomLeft
  else
    Result := coBottomRight;
end;

function TdxFilterPopupWindowSysPanelViewInfo.GetWidth: Integer;
begin
  Result := cxRectWidth(FilterPopup.ClientBounds);
end;

function TdxFilterPopupWindowSysPanelViewInfo.GetSeparatorSize: Integer;
begin
  Result := Painter.SeparatorSize;
end;

function TdxFilterPopupWindowSysPanelViewInfo.GetSizeGripSize: Integer;
begin
  Result := ScaleFactor.Apply(dxFilterPopupWindowSysPanelSizeGripSize);
end;

{ TdxFilterPopupWindow }

constructor TdxFilterPopupWindow.Create(AOwnerControl: TWinControl);
begin
  inherited Create(AOwnerControl);
  AlignHorz := pahRight;

  CreateFilterValueContainer;
end;

destructor TdxFilterPopupWindow.Destroy;
begin
  FreeAndNil(FSysPanelViewInfo);
  FreeAndNil(FFilterValueContainer);
  inherited Destroy;
end;

procedure TdxFilterPopupWindow.CloseUp;
begin
  inherited CloseUp;
  if FilterValueContainerIntf <> nil then
    FilterValueContainerIntf.Closed;
  if NeedFilteringAppliedOnClose then
  begin
    FilterValueContainer.FilteringApplied;
    NeedFilteringAppliedOnClose := False;
  end;
  FilterValueContainer.LinkComponent := nil;
end;

function TdxFilterPopupWindow.GetViewInfoClass: TcxContainerViewInfoClass;
begin
  Result := TdxFilterPopupWindowViewInfo;
end;

procedure TdxFilterPopupWindow.Popup;
begin
  if FilterOwnerIntf <> nil then
    SetMode(FilterOwnerIntf.GetMode);
  inherited Popup;
end;

procedure TdxFilterPopupWindow.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AHitCode: Integer;
begin
  AHitCode := GetHitTest(cxPoint(X, Y));
  if AHitCode = htNone then
    Exit;
  if (AHitCode = htSizeGrip) and (Button = mbLeft) and (GetCaptureControl = Self) then
    FCaptureSizeGrip := True
  else
    SysPanelViewInfo.MouseDown(Button, Shift, X, Y);
end;

procedure TdxFilterPopupWindow.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if CaptureSizeGrip then
    ResizeByDragging(X, Y)
  else
    if GetHitTest(cxPoint(X, Y)) <> htNone then
      SysPanelViewInfo.MouseMove(Shift, X, Y);
end;

procedure TdxFilterPopupWindow.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if CaptureSizeGrip then
    FCaptureSizeGrip := False
  else
    if GetHitTest(cxPoint(X, Y)) <> htNone then
      SysPanelViewInfo.MouseUp(Button, Shift, X, Y);
end;

procedure TdxFilterPopupWindow.ActualModeChanged;
begin
  RecreateFilterValueContainer;
  RecreateSysPanel;
  Adjustable := ActualMode <> fpmExcel;
  UpdateLayout;
end;

procedure TdxFilterPopupWindow.AdjustBounds;
var
  ASize: TSize;
  APos: TPoint;
begin
  ASize := CalculateSize;
  APos := CalculatePosition(ASize);
  CorrectBoundsWithDesktopWorkArea(APos, ASize);
  SetBounds(APos.X, APos.Y, ASize.cx, ASize.cy);
end;

procedure TdxFilterPopupWindow.AdjustableChanged;
begin
  inherited AdjustableChanged;
  if Adjustable then
    AdjustBounds
  else
    BoundsRect := cxRectSetSize(BoundsRect, cxRectSize(GetDefaultBounds));
end;

procedure TdxFilterPopupWindow.BordersChanged;
begin
  UpdateLayout;
end;

procedure TdxFilterPopupWindow.BoundsChanged;
begin
  UpdateLayout;
end;

function TdxFilterPopupWindow.CalculateSize: TSize;
begin
  if Adjustable then
    ResetFilterValueContainerPosition;
  Result := inherited CalculateSize;
  if Adjustable and HasSysPanel then
    Inc(Result.cy, SysPanelViewInfo.Height);
end;

procedure TdxFilterPopupWindow.CreateFilterValueContainer;
begin
  FFilterValueContainer := GetFilterValueContainerClass.Create(Self);
  InitFilterValueContainer;
end;

procedure TdxFilterPopupWindow.DoShowed;
begin
  inherited DoShowed;
  if FilterValueContainerIntf <> nil then
    FilterValueContainerIntf.FocusStartControl;
end;

procedure TdxFilterPopupWindow.DrawSysPanel;
var
  ABuffer: TcxBitmap32;
  ACanvas: TcxCanvas;
begin
  ABuffer := TcxBitmap32.CreateSize(SysPanelViewInfo.Bounds);
  try
    ACanvas := ABuffer.cxCanvas;
    ACanvas.Font := Canvas.Font;
    ACanvas.WindowOrg := SysPanelViewInfo.Bounds.TopLeft;
    SysPanelViewInfo.Paint(ACanvas);
    ACanvas.WindowOrg := cxNullPoint;
    cxBitBlt(Canvas.Handle, ACanvas.Handle, SysPanelViewInfo.Bounds, cxNullPoint, SRCCOPY);
  finally
    ABuffer.Free;
  end;
end;

function TdxFilterPopupWindow.CreateSysPanelViewInfo: TdxFilterPopupWindowSysPanelViewInfo;
begin
  Result := TdxFilterPopupWindowSysPanelViewInfo.Create(Self);
end;

function TdxFilterPopupWindow.GetDefaultBounds: TRect;
begin
  Result := cxEmptyRect;
  if ActualMode = fpmExcel then
    Result := ScaleFactor.Apply(Rect(0, 0, dxExcelFilterPopupDefaultWidth, dxExcelFilterPopupDefaultHeight));
end;

function TdxFilterPopupWindow.GetFilterValueContainerClass: TdxCustomFilterValueContainerClass;
begin
  if ActualMode = fpmExcel then
    Result := TdxFilterPopupWindowExcelFilterValueContainer
  else
    Result := TdxFilterPopupWindowFilterValueContainer;
end;

function TdxFilterPopupWindow.GetHitTest(const APoint: TPoint): Integer;
begin
  Result := htNone;
  if HasSysPanel then
    Result := SysPanelViewInfo.GetHitTest(APoint);
end;

function TdxFilterPopupWindow.GetMinSize: TSize;
begin
  Result := cxRectSize(GetDefaultBounds);
end;

function TdxFilterPopupWindow.GetSysPanelType: TdxFilterPopupWindowSysPanelType;
begin
  Result := FilterValueContainerIntf.SysPanelType;
end;

function TdxFilterPopupWindow.HasSysPanel: Boolean;
begin
  Result := SysPanelViewInfo <> nil;
end;

procedure TdxFilterPopupWindow.InitFilterValueContainer;
begin
  FilterValueContainer.Style.Edges := [];
  FilterValueContainer.Style.HotTrack := False;
  FilterValueContainer.Style.BorderStyle := cbsNone;
  FilterValueContainer.Style.TransparentBorder := False;
  FilterValueContainer.Style.LookAndFeel.MasterLookAndFeel := LookAndFeel;
  FilterValueContainer.Parent := Self;
end;

procedure TdxFilterPopupWindow.InitPopup;
begin
  inherited InitPopup;
  UpdateFilterValueContainerByOwner;
end;

procedure TdxFilterPopupWindow.OwnerChanged;
begin
  inherited OwnerChanged;
  Supports(Owner, IdxFilterPopupWindowOwner, FFilterOwnerIntf);
end;

procedure TdxFilterPopupWindow.Paint;
begin
  inherited Paint;
  if HasSysPanel then
    DrawSysPanel;
end;

procedure TdxFilterPopupWindow.RecreateFilterValueContainer;
begin
  FreeAndNil(FFilterValueContainer);
  CreateFilterValueContainer;
end;

procedure TdxFilterPopupWindow.RecreateSysPanel;
begin
  FreeAndNil(FSysPanelViewInfo);
  if ShowSysPanel then
    FSysPanelViewInfo := CreateSysPanelViewInfo;
end;

procedure TdxFilterPopupWindow.ResetFilterValueContainerPosition;
begin
  FilterValueContainer.Left := 0;
  FilterValueContainer.Top := 0;
end;

procedure TdxFilterPopupWindow.Resize;
begin
  inherited Resize;
  BoundsChanged;
end;

procedure TdxFilterPopupWindow.ResizeByDragging(X, Y: Integer);
var
  ARect: TRect;
  APoint: TPoint;
  ASize, AMinSize: TSize;
begin
  ARect := BoundsRect;
  APoint := dxMapWindowPoint(Handle, 0, Point(X, Y));
  if UseRightToLeftAlignment then
  begin
    ARect.Left := APoint.X;
    ARect.Bottom := APoint.Y;
  end
  else
    ARect.BottomRight := APoint;
  ASize := cxRectSize(ARect);
  AMinSize := GetMinSize;
  if ASize.cx < AMinSize.cx then
    if UseRightToLeftAlignment then
      ARect.Left := ARect.Right - AMinSize.cx
    else
      ARect.Right := ARect.Left + AMinSize.cx;
  if ASize.cy < AMinSize.cy then
    ARect := cxRectSetHeight(ARect, AMinSize.cy);
  BoundsRect := ARect;
end;

function TdxFilterPopupWindow.ShowSysPanel: Boolean;
begin
  Result := GetSysPanelType <> sptNone;
end;

procedure TdxFilterPopupWindow.SetMode(AMode: TdxFilterPopupWindowMode);
begin
  FMode := AMode;
  ActualMode := dxGetFilterPopupActualMode(Mode);
end;

procedure TdxFilterPopupWindow.SysPanelButtonAction(AHitCode: Integer);
begin
  FilterValueContainerIntf.SysPanelButtonAction(AHitCode);
  ClosePopup;
end;

function TdxFilterPopupWindow.SysPanelButtonEnabled(AHitCode: Integer): Boolean;
begin
  Result := FilterValueContainerIntf.SysPanelButtonEnabled(AHitCode);
end;

procedure TdxFilterPopupWindow.UpdateFilterValueContainerByOwner;
begin
  FilterValueContainer.BeginUpdate;
  try
    if FilterOwnerIntf <> nil then
    begin
      if FilterValueContainerIntf <> nil then
        FilterValueContainerIntf.AssignOptions(FilterOwnerIntf.GetOptions);
      FilterValueContainer.LinkComponent := FilterOwnerIntf.GetLinkComponent;
    end
    else
      FilterValueContainer.LinkComponent := nil;
  finally
    FilterValueContainer.EndUpdate;
  end;
end;

procedure TdxFilterPopupWindow.UpdateLayout;
var
  AClientBounds: TRect;
begin
  AClientBounds := ClientBounds;
  if HasSysPanel then
  begin
    AClientBounds.Bottom := AClientBounds.Bottom - SysPanelViewInfo.Height;
    SysPanelViewInfo.Calculate(AClientBounds.Left, AClientBounds.Bottom);
    if UseRightToLeftAlignment then
      SysPanelViewInfo.RightToLeftConversion(ClientBounds);
  end;
  if not Adjustable then
  begin
    FilterValueContainer.BoundsRect := AClientBounds;
    Repaint;
  end;
end;

function TdxFilterPopupWindow.GetFilterValueContainerIntf: IdxFilterPopupWindowFilterValueContainer;
begin
  Supports(FilterValueContainer, IdxFilterPopupWindowFilterValueContainer, Result);
end;

function TdxFilterPopupWindow.GetLinkComponent: TComponent;
begin
  Result := FilterValueContainer.LinkComponent;
end;

function TdxFilterPopupWindow.GetViewInfo: TdxFilterPopupWindowViewInfo;
begin
  Result := TdxFilterPopupWindowViewInfo(inherited ViewInfo);
end;

procedure TdxFilterPopupWindow.SetActualMode(AValue: TdxFilterPopupWindowActualMode);
begin
  if ActualMode <> AValue then
  begin
    FActualMode := AValue;
    ActualModeChanged;
  end;
end;

procedure TdxFilterPopupWindow.WMSetCursor(var Message: TWMSetCursor);
var
  APoint: TPoint;
  ACursore: TCursor;
  AHitTest: Integer;
begin
  APoint := ScreenToClient(GetMouseCursorPos);
  AHitTest := GetHitTest(APoint);
  if AHitTest = htSizeGrip then
  begin
    if UseRightToLeftAlignment then
      ACursore := crSizeNESW
    else
      ACursore := crSizeNWSE;
    SetCursor(Screen.Cursors[ACursore]);
  end
  else
    inherited;
end;

end.
