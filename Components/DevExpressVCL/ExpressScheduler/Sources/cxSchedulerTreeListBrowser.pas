{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressScheduler                                         }
{                                                                    }
{           Copyright (c) 2003-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSCHEDULER AND ALL ACCOMPANYING }
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

unit cxSchedulerTreeListBrowser;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Variants, Windows, Forms, Messages, Classes, Math, StdCtrls, Graphics, Controls, ExtCtrls,
  Contnrs, SysUtils, Types, RTLConsts, cxInplaceContainer, dxCore, cxControls, cxGraphics,
  cxEdit, cxTextEdit, cxMemo, cxCalendar, cxCheckbox, cxSpinEdit, cxDrawTextUtils,
  cxSchedulerStrs, cxSchedulerCustomControls, cxSchedulerUtils, cxSchedulerStorage, cxClasses,
  cxScrollBar, cxSchedulerTimeGridView, cxTL, cxTLData, cxCustomData, cxVariants, dxMessages, dxCoreClasses;

const
  WM_DROPTOGROUP = WM_DX + 400;
  WM_GROUPUNGROUP = WM_DROPTOGROUP + 1;
  WM_UNLOCK = WM_GROUPUNGROUP + 1;

type
  TcxSchedulerTreeListBrowser = class;
  TcxSchedulerTreeBrowserDataSource = class;
  TcxSchedulerTreeListBrowserColumnDataProvider = class;

  TcxShedulerTreeBrowserStyle = (tbsTree, tbsList);

  { TcxSchedulerTreeListBrowser }

  TcxSchedulerTreeListBrowser = class(TcxVirtualTreeList, IcxSchedulerTreeBrowserControl)
  private
    FDataSource: TcxCustomDataSource;
    FDropGroup: TcxSchedulerEvent;
    FIsLoading: Boolean;
    FStyle: TcxShedulerTreeBrowserStyle;
    FView: TcxSchedulerTimeGridView;
    function GetFocusedEvent: TcxSchedulerEvent;
    function GetStorage: TcxCustomSchedulerStorage;
    procedure SetStyle(AValue: TcxShedulerTreeBrowserStyle);
    procedure WMDROPTOGROUP(var AMessage: TMessage); message WM_DROPTOGROUP;
    procedure WMGROUPUNGROUP(var AMessage: TMessage); message WM_GROUPUNGROUP;
    procedure WMUNLOCK(var AMessage: TMessage); message WM_UNLOCK;
  protected
    function ChangeExpanding(ANode: TcxTreeListNode; ARecursive, AExpanded: Boolean): Boolean;
    procedure CreateAllColumns; virtual;
    procedure DragOverEventHandler(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure DrawIndicator(Sender: TcxCustomTreeList; ACanvas: TcxCanvas;
      AViewInfo: TcxTreeListIndicatorCellViewInfo; var ADone: Boolean); virtual;
    function GetDesignHitTest(X, Y: Integer; Shift: TShiftState): Boolean; override;
    function GetTreeListColumnClass: TcxTreeListColumnClass; override;
    function GetViewInfoClass: TcxCustomControlViewInfoClass; override;
    procedure InitScrollBarsParameters; override;
    function InternalCollapseNode(ANode: TcxTreeListNode; ARecursive: Boolean): Boolean; override;
    function InternalExpandNode(ANode: TcxTreeListNode; ARecursive: Boolean): Boolean; override;
    function IsActionSupported(AButtonIndex: Integer): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure RefreshData;
    procedure Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode;
      var AScrollPos: Integer); override;
    procedure SetContentLineHeight(AHeight: Integer);
    procedure SetHeaderHeight(AHeight: Integer);
    procedure SetMode(AMode: TcxSchedulerTreeBrowserDisplayMode);
    procedure SetTopRecordIndex(AIndex: Integer);
    procedure SetView(AView: TcxSchedulerTimeGridView);
    procedure SynchronizeTopRow;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    procedure TranslationChanged; override;

    property FocusedEvent: TcxSchedulerEvent read GetFocusedEvent;
    property Style: TcxShedulerTreeBrowserStyle read FStyle write SetStyle default tbsTree;
    property Storage: TcxCustomSchedulerStorage read GetStorage;
    property View: TcxSchedulerTimeGridView read FView;
  end;

  { TcxTreeListBrowserColumn }

  TcxTreeListBrowserColumn = class(TcxTreeListColumn)
  protected
    function CanEdit: Boolean; override;
  public
    destructor Destroy; override;
  end;

  { TcxSchedulerTreeBrowserDataSource }

  TcxSchedulerTreeBrowserDataSource = class(TcxTreeListCustomDataSource)
  private
    FBrowser: TcxSchedulerTreeListBrowser;
    FEvents: TdxFastList;
    function GetViewInfo: TcxSchedulerTimeGridViewViewInfo;
  protected
    function GetColumnDataProvider(AItemHandle: TcxDataItemHandle): TcxSchedulerTreeListBrowserColumnDataProvider;
    function GetChildCount(AParentHandle: TcxDataRecordHandle): Integer; override;
    function GetChildRecordHandle(AParentHandle: TcxDataRecordHandle; AChildIndex: Integer): TcxDataRecordHandle; override;
    function GetEventByGroupID(const AGroupID: Variant; AIndex: Integer): TcxSchedulerEvent;
    function GetEventByTaskLink(ALink: TcxSchedulerEventItemLink): TcxSchedulerEvent;
    function GetGroupID(AParentHandle: TcxDataRecordHandle): Variant;
    function GetItemHandle(AItemIndex: Integer): TcxDataItemHandle; override;
    function GetParentRecordHandle(ARecordHandle: TcxDataRecordHandle): TcxDataRecordHandle; override;
    function GetRecordCount: Integer; override;
    function GetRecordHandle(ARecordIndex: Integer): TcxDataRecordHandle; override;
    function GetRootRecordHandle: TcxDataRecordHandle; override;
    function GetValue(ARecordHandle: TcxDataRecordHandle; AItemHandle: TcxDataItemHandle): Variant; override;
    procedure PrepareEventsList;
    procedure SetValue(ARecordHandle: TcxDataRecordHandle; AItemHandle: TcxDataItemHandle; const AValue: Variant); override;

    property Browser: TcxSchedulerTreeListBrowser read FBrowser;
    property Events: TdxFastList read FEvents;
    property ViewInfo: TcxSchedulerTimeGridViewViewInfo read GetViewInfo;
  public
    constructor Create(ABrowser: TcxSchedulerTreeListBrowser); virtual;
    destructor Destroy; override;
    procedure DataChanged; override;
  end;

  { TcxSchedulerTreeListBrowserColumnDataProvider }

  TcxSchedulerTreeListBrowserColumnDataProvider = class
  private
    FBrowser: TcxSchedulerTreeListBrowser;
    FColumn: TcxTreeListColumn;
  protected
    function GetCaption: string; virtual; abstract;
    function GetDefaultColumnWidth: Integer; virtual;
    function GetPropertiesClass: TcxCustomEditPropertiesClass; virtual;
    function GetValue(const AEvent: TcxSchedulerEvent): Variant; virtual;
    procedure InitializeProperties; virtual;
    procedure SetValue(const AEvent: TcxSchedulerEvent; const AValue: Variant); virtual;
    procedure TranslationChanged;
  public
    constructor Create(ABrowser: TcxSchedulerTreeListBrowser; AVisible: Boolean);

    property Browser: TcxSchedulerTreeListBrowser read FBrowser;
    property Column: TcxTreeListColumn read FColumn;
  end;

  TcxSchedulerTreeListBrowserColumnCaptionDataProvider = class(TcxSchedulerTreeListBrowserColumnDataProvider)
  protected
    function GetCaption: string; override;
    function GetDefaultColumnWidth: Integer; override;
    function GetValue(const AEvent: TcxSchedulerEvent): Variant; override;
    procedure SetValue(const AEvent: TcxSchedulerEvent; const AValue: Variant); override;
  end;

  TcxSchedulerTreeListBrowserColumnLocationDataProvider = class(TcxSchedulerTreeListBrowserColumnDataProvider)
  protected
    function GetCaption: string; override;
    function GetValue(const AEvent: TcxSchedulerEvent): Variant; override;
    procedure SetValue(const AEvent: TcxSchedulerEvent; const AValue: Variant); override;
  end;

  TcxSchedulerTreeListBrowserColumnDescriptionDataProvider = class(TcxSchedulerTreeListBrowserColumnDataProvider)
  protected
    function GetCaption: string; override;
    function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    function GetValue(const AEvent: TcxSchedulerEvent): Variant; override;
    procedure SetValue(const AEvent: TcxSchedulerEvent; const AValue: Variant); override;
  end;

  TcxSchedulerTreeListBrowserColumnDateDataProvider = class(TcxSchedulerTreeListBrowserColumnDataProvider)
  protected
    function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    procedure InitializeProperties; override;
  end;

  TcxSchedulerTreeListBrowserColumnStartDataProvider = class(TcxSchedulerTreeListBrowserColumnDateDataProvider)
  protected
    function GetCaption: string; override;
    function GetValue(const AEvent: TcxSchedulerEvent): Variant; override;
    procedure SetValue(const AEvent: TcxSchedulerEvent; const AValue: Variant); override;
  end;

  TcxSchedulerTreeListBrowserColumnFinishDataProvider = class(TcxSchedulerTreeListBrowserColumnDateDataProvider)
  protected
    function GetCaption: string; override;
    function GetValue(const AEvent: TcxSchedulerEvent): Variant; override;
    procedure SetValue(const AEvent: TcxSchedulerEvent; const AValue: Variant); override;
  end;

  TcxSchedulerTreeListBrowserColumnReminderDataProvider = class(TcxSchedulerTreeListBrowserColumnDataProvider)
  protected
    function GetCaption: string; override;
    function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    function GetValue(const AEvent: TcxSchedulerEvent): Variant; override;
    procedure SetValue(const AEvent: TcxSchedulerEvent; const AValue: Variant); override;
  end;

  TcxSchedulerTreeListBrowserColumnProgressDataProvider = class(TcxSchedulerTreeListBrowserColumnDataProvider)
  protected
    function GetCaption: string; override;
    function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    function GetValue(const AEvent: TcxSchedulerEvent): Variant; override;
    procedure InitializeProperties; override;
    procedure SetValue(const AEvent: TcxSchedulerEvent; const AValue: Variant); override;
  end;

  { TcxSchedulerTreeListBrowserViewInfo }

  TcxSchedulerTreeListBrowserViewInfo = class(TcxTreeListViewInfo)
  private
    function GetBrowser: TcxSchedulerTreeListBrowser;
    function GetView: TcxSchedulerTimeGridView;
    function GetView_ViewInfo: TcxSchedulerTimeGridViewViewInfo;
  protected
    procedure CalculateDefaultHeights; override;

    property Browser: TcxSchedulerTreeListBrowser read GetBrowser;
    property View: TcxSchedulerTimeGridView read GetView;
    property View_ViewInfo: TcxSchedulerTimeGridViewViewInfo read GetView_ViewInfo;
  end;

implementation

type
  TcxSchedulerControlEventAccess = class(TcxSchedulerControlEvent);
  TcxSchedulerViewAccess = class(TcxSchedulerTimeGridView);
  TcxSchedulerView_ViewInfoAccess = class(TcxSchedulerTimeGridViewViewInfo);
  TcxTreeListNodeAccess = class(TcxTreeListNode);
  TcxTreeListIndicatorAccess = class(TcxTreeListIndicatorCellViewInfo);
  TcxSchedulerEventAccess = class(TcxSchedulerEvent);

const
  ssAltShift = [ssAlt, ssShift];

function dxVarToDateTime(const AValue: Variant; const ADefaultValue: TDateTime): TDateTime;
begin
  try
    Result := VarToDateTime(AValue);
  except
    on EVariantTypeCastError do
      Result := ADefaultValue;
  end;
end;

{ TcxSchedulerTreeBrowserDataSource }

constructor TcxSchedulerTreeBrowserDataSource.Create(
  ABrowser: TcxSchedulerTreeListBrowser);
begin
  FBrowser := ABrowser;
  FEvents := TdxFastList.Create;
end;

destructor TcxSchedulerTreeBrowserDataSource.Destroy;
begin
  FreeAndNil(FEvents);
  inherited Destroy;
end;

procedure TcxSchedulerTreeBrowserDataSource.DataChanged;
begin
  PrepareEventsList;
  inherited DataChanged;
end;
function TcxSchedulerTreeBrowserDataSource.GetColumnDataProvider(
  AItemHandle: TcxDataItemHandle): TcxSchedulerTreeListBrowserColumnDataProvider;
begin
  Result := TcxSchedulerTreeListBrowserColumnDataProvider(TcxTreeListColumn(AItemHandle).Data);
end;

function TcxSchedulerTreeBrowserDataSource.GetChildCount(
  AParentHandle: TcxDataRecordHandle): Integer;
var
  I: Integer;
begin
  Result := 0;
  if Browser.Style = tbsList then
  begin
    if AParentHandle = nil then
      Result := Events.Count;
  end
  else
    if (AParentHandle = nil) or TcxSchedulerEvent(AParentHandle).IsGroup then
    begin
      for I := 0 to Events.Count - 1 do
        if VarEquals(TcxSchedulerEvent(Events[I]).GroupID, GetGroupID(AParentHandle)) then
          Inc(Result);
    end;
end;

function TcxSchedulerTreeBrowserDataSource.GetChildRecordHandle(
  AParentHandle: TcxDataRecordHandle; AChildIndex: Integer): TcxDataRecordHandle;
begin
  Result := nil;
  if Browser.Style = tbsList then
  begin
    if AParentHandle = nil then
      Result := Events[AChildIndex];
  end
  else
    if (AParentHandle = nil) or TcxSchedulerEvent(AParentHandle).IsGroup then
      Result := GetEventByGroupID(GetGroupID(AParentHandle), AChildIndex)
end;

function TcxSchedulerTreeBrowserDataSource.GetEventByGroupID(
  const AGroupID: Variant; AIndex: Integer): TcxSchedulerEvent;
var
  I: Integer;
  AEvent: TcxSchedulerEvent;
begin
  Result := nil;
  if Events = nil then Exit;
  for I := 0 to Events.Count - 1 do
  begin
    AEvent := Events[I];
    if VarEquals(AEvent.GroupID, AGroupID) then
    begin
      if AIndex = 0 then
      begin
        Result := AEvent;
        Break;
      end
      else
        Dec(AIndex);
    end;
  end;
end;

function TcxSchedulerTreeBrowserDataSource.GetEventByTaskLink(
  ALink: TcxSchedulerEventItemLink): TcxSchedulerEvent;
var
  I: Integer;
  AEvent: TcxSchedulerControlEvent;
begin
  Result := nil;
  for I := 0 to Events.Count - 1 do
  begin
    AEvent := TcxSchedulerControlEvent(Events[I]);
    if (AEvent.Source = ALink.Link) and (AEvent.RecurrenceIndex = ALink.LinkRecurrenceIndex) then
    begin
      Result := AEvent;
      Break;
    end;
  end;
end;

function TcxSchedulerTreeBrowserDataSource.GetGroupID(
  AParentHandle: TcxDataRecordHandle): Variant;
begin
  Result := Null;
  if AParentHandle <> nil then
    Result := TcxSchedulerEvent(AParentHandle).ID;
end;

function TcxSchedulerTreeBrowserDataSource.GetItemHandle(AItemIndex: Integer): TcxDataItemHandle;
begin
  Result := Browser.Columns[AItemIndex];
end;

function TcxSchedulerTreeBrowserDataSource.GetParentRecordHandle(ARecordHandle: TcxDataRecordHandle): TcxDataRecordHandle;
var
  I: Integer;
begin
  if not VarIsNull(TcxSchedulerEvent(ARecordHandle).GroupID) then
  begin
    for I := 0 to Events.Count - 1 do
      if VarEquals(TcxSchedulerEvent(ARecordHandle).GroupID, TcxSchedulerEvent(Events[I]).ID) then
      begin
        Result := Events[I];
        Exit;
      end;
  end;
  Result := nil;
end;

function TcxSchedulerTreeBrowserDataSource.GetRecordCount: Integer;
begin
  Result := Events.Count;
end;

function TcxSchedulerTreeBrowserDataSource.GetRecordHandle(ARecordIndex: Integer): TcxDataRecordHandle;
begin
  Result := Events[ARecordIndex];
end;

function TcxSchedulerTreeBrowserDataSource.GetRootRecordHandle: TcxDataRecordHandle;
begin
  Result := nil;
end;

function TcxSchedulerTreeBrowserDataSource.GetValue(
  ARecordHandle: TcxDataRecordHandle; AItemHandle: TcxDataItemHandle): Variant;
begin
  if ARecordHandle = nil then Exit;
  Result := GetColumnDataProvider(AItemHandle).GetValue(TcxSchedulerEvent(ARecordHandle));
end;

function cxCompareBrowserEvents(AEvent1, AEvent2: TcxSchedulerControlEventAccess): Integer;
begin
  if (AEvent1.VisibleIndex < 0) and (AEvent2.VisibleIndex >= 0) then
    Result := 1
  else
    if (AEvent1.VisibleIndex >= 0) and (AEvent2.VisibleIndex < 0) then
      Result := -1
    else
      Result := AEvent1.VisibleIndex - AEvent2.VisibleIndex;
end;

procedure TcxSchedulerTreeBrowserDataSource.PrepareEventsList;
var
  I, C: Integer;
  AEvent: TcxSchedulerControlEventAccess;
begin
  C := 0;
  FEvents.Assign(TcxSchedulerViewAccess(Browser.View).EventList.List);
  FEvents.Assign(TcxSchedulerViewAccess(Browser.View).EventList.Clones.List, laOr);
  for I := 0 to FEvents.Count - 1 do
    if TcxSchedulerControlEventAccess(FEvents[I]).VisibleIndex < 0 then Inc(C);
  FEvents.Sort(@cxCompareBrowserEvents);
  FEvents.Count := FEvents.Count - C;
  I := 0;
  while I < FEvents.Count do
  begin
    AEvent := TcxSchedulerControlEventAccess(FEvents[I]);
    Inc(I);
    if AEvent.EventType <> etNone then
      while (I < FEvents.Count) and (TcxSchedulerControlEventAccess(FEvents[I]).Pattern = AEvent.Pattern) do
        FEvents.Delete(I);
  end;
end;

procedure TcxSchedulerTreeBrowserDataSource.SetValue(
  ARecordHandle: TcxDataRecordHandle; AItemHandle: TcxDataItemHandle; const AValue: Variant);
begin
  if ARecordHandle = nil then Exit;
  GetColumnDataProvider(AItemHandle).SetValue(TcxSchedulerControlEvent(ARecordHandle).Source, AValue);
end;

function TcxSchedulerTreeBrowserDataSource.GetViewInfo: TcxSchedulerTimeGridViewViewInfo;
begin
  Result := TcxSchedulerViewAccess(Browser.View).ViewInfo;
end;

{ TcxSchedulerTreeListBrowserColumnDataProvider }

constructor TcxSchedulerTreeListBrowserColumnDataProvider.Create(
  ABrowser: TcxSchedulerTreeListBrowser; AVisible: Boolean);
begin
  FBrowser := ABrowser;
  Browser.BeginUpdate;
  FColumn := Browser.CreateColumn(Browser.Bands.LastVisible);
  FColumn.Caption.Text := GetCaption;
  FColumn.Caption.AlignVert := vaCenter;
  FColumn.Options.Sorting := False;
  FColumn.Data := Self;
  FColumn.PropertiesClass := GetPropertiesClass;
  FColumn.Width := GetDefaultColumnWidth;
  InitializeProperties;
  FColumn.Visible := AVisible;
  Browser.EndUpdate;
end;

function TcxSchedulerTreeListBrowserColumnDataProvider.GetDefaultColumnWidth: Integer;
begin
  Result := 50;
end;

function TcxSchedulerTreeListBrowserColumnDataProvider.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxTextEditProperties;
end;

function TcxSchedulerTreeListBrowserColumnDataProvider.GetValue(
  const AEvent: TcxSchedulerEvent): Variant;
begin
  Result := Null;
end;

procedure TcxSchedulerTreeListBrowserColumnDataProvider.InitializeProperties;
begin
  Column.Properties.Alignment.Vert := taVCenter;
  Column.Properties.ImmediatePost := True;
end;

procedure TcxSchedulerTreeListBrowserColumnDataProvider.SetValue(
  const AEvent: TcxSchedulerEvent; const AValue: Variant);
var
  ADuration: TDateTime;
  AStart: TDateTime;
begin
  AStart := TcxSchedulerEventAccess(AEvent).Start;
  ADuration := TcxSchedulerEventAccess(AEvent).Duration;
  AEvent.Start := AStart;
  AEvent.Duration := ADuration;
end;

procedure TcxSchedulerTreeListBrowserColumnDataProvider.TranslationChanged;
begin
  Column.Caption.Text := GetCaption;
end;

{ TcxSchedulerTreeListBrowserColumnCaptionDataProvider }

function TcxSchedulerTreeListBrowserColumnCaptionDataProvider.GetCaption: string;
begin
  Result := cxGetResourceString(@scxCaptionField);
end;

function TcxSchedulerTreeListBrowserColumnCaptionDataProvider.GetDefaultColumnWidth: Integer;
begin
  Result := 200;
end;

function TcxSchedulerTreeListBrowserColumnCaptionDataProvider.GetValue(
  const AEvent: TcxSchedulerEvent): Variant;
begin
  Result := AEvent.Caption;
end;

procedure TcxSchedulerTreeListBrowserColumnCaptionDataProvider.SetValue(
  const AEvent: TcxSchedulerEvent; const AValue: Variant);
begin
  inherited SetValue(AEvent, AValue);
  AEvent.Caption := VarToStr(AValue);
end;

{ TcxSchedulerTreeListBrowserColumnLocationDataProvider }

function TcxSchedulerTreeListBrowserColumnLocationDataProvider.GetCaption: string;
begin
  Result := cxGetResourceString(@scxLocationField);
end;

function TcxSchedulerTreeListBrowserColumnLocationDataProvider.GetValue(
  const AEvent: TcxSchedulerEvent): Variant;
begin
  Result := AEvent.Location;
end;

procedure TcxSchedulerTreeListBrowserColumnLocationDataProvider.SetValue(
  const AEvent: TcxSchedulerEvent; const AValue: Variant);
begin
  inherited SetValue(AEvent, AValue);
  AEvent.Location := VarToStr(AValue);
end;

{ TcxSchedulerTreeListBrowserColumnDescriptionDataProvider }

function TcxSchedulerTreeListBrowserColumnDescriptionDataProvider.GetCaption: string;
begin
  Result := cxGetResourceString(@scxMessageField);
end;

function TcxSchedulerTreeListBrowserColumnDescriptionDataProvider.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxMemoProperties;
end;

function TcxSchedulerTreeListBrowserColumnDescriptionDataProvider.GetValue(
  const AEvent: TcxSchedulerEvent): Variant;
begin
  Result := AEvent.Message;
end;

procedure TcxSchedulerTreeListBrowserColumnDescriptionDataProvider.SetValue(
  const AEvent: TcxSchedulerEvent; const AValue: Variant);
begin
  inherited SetValue(AEvent, AValue);
  AEvent.Message := VarToStr(AValue);
end;

{ TcxSchedulerTreeListBrowserColumnDateDataProvider }

function TcxSchedulerTreeListBrowserColumnDateDataProvider.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxDateEditProperties;
end;

procedure TcxSchedulerTreeListBrowserColumnDateDataProvider.InitializeProperties;
begin
  inherited InitializeProperties;
  TcxDateEditProperties(Column.Properties).Kind := ckDateTime;
  TcxDateEditProperties(Column.Properties).DateButtons :=
    TcxDateEditProperties(Column.Properties).DateButtons - [btnClear];
end;

{ TcxSchedulerTreeListBrowserColumnStartDataProvider }

function TcxSchedulerTreeListBrowserColumnStartDataProvider.GetCaption: string;
begin
  Result := cxGetResourceString(@scxStartField);
end;

function TcxSchedulerTreeListBrowserColumnStartDataProvider.GetValue(
  const AEvent: TcxSchedulerEvent): Variant;
begin
  Result := AEvent.Start;
end;

procedure TcxSchedulerTreeListBrowserColumnStartDataProvider.SetValue(
  const AEvent: TcxSchedulerEvent; const AValue: Variant);
begin
  inherited SetValue(AEvent, AValue);
  AEvent.Finish := AEvent.Finish;
  AEvent.Start := dxVarToDateTime(AValue, AEvent.Start);
end;

{ TcxSchedulerTreeListBrowserColumnFinishDataProvider }

function TcxSchedulerTreeListBrowserColumnFinishDataProvider.GetCaption: string;
begin
  Result := cxGetResourceString(@scxFinishField)
end;

function TcxSchedulerTreeListBrowserColumnFinishDataProvider.GetValue(
  const AEvent: TcxSchedulerEvent): Variant;
begin
  Result := AEvent.Finish;
end;

procedure TcxSchedulerTreeListBrowserColumnFinishDataProvider.SetValue(
  const AEvent: TcxSchedulerEvent; const AValue: Variant);
begin
  AEvent.Start := AEvent.Start;
  AEvent.Finish := dxVarToDateTime(AValue, AEvent.Finish);
  inherited SetValue(AEvent, AValue);
end;

{ TcxSchedulerTreeListBrowserColumnReminderDataProvider }

function TcxSchedulerTreeListBrowserColumnReminderDataProvider.GetCaption: string;
begin
  Result := cxGetResourceString(@scxReminderField);
end;

function TcxSchedulerTreeListBrowserColumnReminderDataProvider.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCheckBoxProperties;
end;

function TcxSchedulerTreeListBrowserColumnReminderDataProvider.GetValue(
  const AEvent: TcxSchedulerEvent): Variant;
begin
  Result := AEvent.Reminder;
end;

procedure TcxSchedulerTreeListBrowserColumnReminderDataProvider.SetValue(
  const AEvent: TcxSchedulerEvent; const AValue: Variant);
begin
  inherited SetValue(AEvent, AValue);
  AEvent.Reminder := AValue;
end;

{ TcxSchedulerTreeListBrowserColumnProgressDataProvider }

function TcxSchedulerTreeListBrowserColumnProgressDataProvider.GetCaption: string;
begin
  Result := cxGetResourceString(@scxTaskCompleteField)
end;

function TcxSchedulerTreeListBrowserColumnProgressDataProvider.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxSpinEditProperties;
end;

function TcxSchedulerTreeListBrowserColumnProgressDataProvider.GetValue(
  const AEvent: TcxSchedulerEvent): Variant;
begin
  Result := AEvent.TaskComplete;
  if VarIsNumeric(Result) then
    Result := Min(100, Max(0, Result))
  else
    Result := 0;
end;

procedure TcxSchedulerTreeListBrowserColumnProgressDataProvider.InitializeProperties;
begin
  inherited InitializeProperties;
  with TcxSpinEditProperties(Column.Properties) do
  begin
    DisplayFormat := cxGetResourceString(@scxCompleteDisplayFormat);
    MaxValue := 100;
    MinValue := 0;
    ImmediatePost := False;
  end;
end;

procedure TcxSchedulerTreeListBrowserColumnProgressDataProvider.SetValue(
  const AEvent: TcxSchedulerEvent; const AValue: Variant);
begin
  inherited SetValue(AEvent, AValue);
  if VarIsNull(AValue) then
    AEvent.TaskComplete := 0
  else
    AEvent.TaskComplete := AValue;
end;

{ TcxSchedulerTreeListBrowser }

constructor TcxSchedulerTreeListBrowser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csNoDesignVisible];
  BorderStyle := cxcbsNone;
  OptionsView.Indicator := True;
  OptionsView.IndicatorWidth := 20;
  OnCustomDrawIndicatorCell := DrawIndicator;
  OptionsData.Deleting := False;
  OptionsData.Inserting := False;
  OptionsView.Bands := True;
  OptionsView.GridLines := tlglBoth;
  FDataSource := TcxSchedulerTreeBrowserDataSource.Create(Self);
  DragMode := dmAutomatic;
  OnDragOver := DragOverEventHandler;
  CreateAllColumns;
end;

destructor TcxSchedulerTreeListBrowser.Destroy;
begin
  CustomDataSource := nil;
  FDataSource.Free;
  inherited Destroy;
end;

procedure TcxSchedulerTreeListBrowser.DragDrop(Source: TObject; X, Y: Integer);
begin
  PostMessage(Handle, WM_DROPTOGROUP, wParam(FocusedEvent), lParam(FDropGroup));
end;

procedure TcxSchedulerTreeListBrowser.TranslationChanged;
var
  I: Integer;
begin
  for I := 0 to ColumnCount - 1 do
    TcxSchedulerTreeListBrowserColumnDataProvider(Columns[I].Data).TranslationChanged;
end;

function TcxSchedulerTreeListBrowser.ChangeExpanding(ANode: TcxTreeListNode; ARecursive, AExpanded: Boolean): Boolean;

  procedure ChangeGroupExpanding(AGroup: TcxSchedulerEventGroupItems);
  var
    AEvent: TcxSchedulerEvent;
    ALink: TcxDoublyLinkedEventListData;
  begin
    AGroup.Expanded := AExpanded;
    if not AGroup.HasChildren or not ARecursive then Exit;
    ALink := AGroup.First;
    while ALink <> nil do
    begin
      AEvent := ALink.Event;
      ChangeGroupExpanding(AEvent.GroupItems);
      ALink := ALink.Next;
    end;
  end;

begin
  Result := (ANode <> Root) or ARecursive;
  if not Result then Exit;
  Storage.BeginUpdate;
  try
    if ANode = Root then
    begin
      ANode := ANode.getFirstChild;
      while ANode <> nil do
      begin
        ChangeGroupExpanding(TcxSchedulerControlEvent(TcxVirtualTreeListNode(ANode).RecordHandle).Source.GroupItems);
        ANode := ANode.getNextSibling;
      end
    end
    else
      ChangeGroupExpanding(TcxSchedulerControlEvent(TcxVirtualTreeListNode(ANode).RecordHandle).Source.GroupItems);
  finally
    PostMessage(Handle, WM_UNLOCK, 0, 0);
  end;
end;

procedure TcxSchedulerTreeListBrowser.CreateAllColumns;
begin
  BeginUpdate;
  try
    Bands.Add;
    TcxSchedulerTreeListBrowserColumnCaptionDataProvider.Create(Self, True);
    TcxSchedulerTreeListBrowserColumnProgressDataProvider.Create(Self, True);
    TcxSchedulerTreeListBrowserColumnLocationDataProvider.Create(Self, False);
    TcxSchedulerTreeListBrowserColumnDescriptionDataProvider.Create(Self, False);
    TcxSchedulerTreeListBrowserColumnStartDataProvider.Create(Self, True);
    TcxSchedulerTreeListBrowserColumnFinishDataProvider.Create(Self, True);
    TcxSchedulerTreeListBrowserColumnReminderDataProvider.Create(Self, False);
  finally
    EndUpdate;
  end;
end;

function TcxSchedulerTreeListBrowser.GetDesignHitTest(
  X, Y: Integer; Shift: TShiftState): Boolean;
begin
  Result := False;
end;

function TcxSchedulerTreeListBrowser.GetTreeListColumnClass: TcxTreeListColumnClass;
begin
  Result := TcxTreeListBrowserColumn;
end;

function TcxSchedulerTreeListBrowser.GetViewInfoClass: TcxCustomControlViewInfoClass;
begin
  Result := TcxSchedulerTreeListBrowserViewInfo;
end;

procedure TcxSchedulerTreeListBrowser.DragOverEventHandler(
  Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
const
  ADragCursors: array[Boolean] of TCursor = (crNoDrop, crDrag);
begin
  Accept := (Source = Sender) and (FDropGroup <> nil);
  if (State in [dsDragMove, dsDragEnter]) and (FocusedNode <> nil) then
  begin
    HitTest.HitPoint := Point(X, Y);
    Accept := HitTest.HitAtNode;
    if Accept then
    begin
      FDropGroup := TcxSchedulerControlEvent(TcxVirtualTreeListNode(HitTest.HitNode).RecordHandle).Source;
      Accept := FDropGroup.IsGroup;
      if not Accept then
        FDropGroup := nil
      else
        Accept := not VarEquals(FDropGroup.ID, FocusedEvent.GroupID) and not VarEquals(FDropGroup.ID, FocusedEvent.ID);
    end;
  end;
  DragCursor := ADragCursors[Accept];
end;

procedure TcxSchedulerTreeListBrowser.DrawIndicator(Sender: TcxCustomTreeList; ACanvas: TcxCanvas;
  AViewInfo: TcxTreeListIndicatorCellViewInfo; var ADone: Boolean);
var
  R: TRect;
begin
  ADone := (AViewInfo.Node <> nil) and not (AViewInfo.Node.Focused or AViewInfo.Node.Selected);
  if ADone then
  begin
    AViewInfo.Painter.DrawScaledIndicatorItem(ACanvas, AViewInfo.BoundsRect, AViewInfo.Kind,
      AViewInfo.ViewParams.Color, ScaleFactor, TcxTreeListIndicatorAccess(AViewInfo).DrawBackgroundHandler);
    R := TcxTreeListIndicatorAccess(AViewInfo).TextBounds;
    cxTextOut(ACanvas.Canvas, IntToStr(AViewInfo.Node.AbsoluteIndex), R,
      cxMakeFormat(taCenterX, taCenterY), 0, 0, AViewInfo.ViewParams.Font, clNone, clNone);
    ACanvas.ExcludeClipRect(TcxTreeListIndicatorAccess(AViewInfo).ClipRect);
  end;
end;

procedure TcxSchedulerTreeListBrowser.KeyDown(var Key: Word; Shift: TShiftState);
const
  ANextCellKey: array[Boolean] of Word = (VK_RIGHT, VK_LEFT);
var
  ATopNode: TcxTreeListNode;
begin
  ATopNode := TopVisibleNode;
  if ((Key = VK_LEFT) or (KEY = VK_RIGHT)) and (Shift * ssAltShift = ssAltShift) then
  begin
    if HandleAllocated then
      PostMessage(Handle, WM_GROUPUNGROUP, wParam(KEY = ANextCellKey[UseRightToLeftAlignment]), 0);
    Key := 0;
  end;
  inherited;
  if ATopNode <> TopVisibleNode then
    SynchronizeTopRow;
end;

procedure TcxSchedulerTreeListBrowser.RefreshData;

   procedure ProcessNode(ANode: TcxTreeListNode);
   var
     AEvent: TcxSchedulerControlEvent;
   begin
     if ANode is TcxVirtualTreeListNode then
     begin
       AEvent := TcxSchedulerControlEvent(TcxVirtualTreeListNode(ANode).RecordHandle);
       ANode.HasChildren := AEvent.IsGroup;
       if ANode.HasChildren then
       begin
         ANode.Expanded := AEvent.GroupItems.Expanded;
         if not ANode.Expanded then
           ANode.HasChildren := True;
       end;
     end;
     ANode := ANode.getFirstChild;
     while ANode <> nil do
     begin
       ProcessNode(ANode);
       ANode := ANode.getNextSibling;
     end;
   end;

var
  AIndex: Integer;
begin
  AIndex := -1;
  if FocusedNode <> nil then
    AIndex := FocusedNode.AbsoluteIndex;
  FIsLoading := True;
  BeginUpdate;
  try
    if FView <> nil then
      FDataSource.DataChanged;
    ProcessNode(Root);
    if AIndex <> -1 then
    begin
      AIndex := Min(AIndex, AbsoluteCount - 1);
      if AIndex >= 0 then
        AbsoluteItems[AIndex].Focused := True
    end;
    if (FocusedNode = nil) and (TopVisibleNode <> nil) then
      TopVisibleNode.Focused := True;
  finally
    EndUpdate;
    FIsLoading := False;
  end;
end;

procedure TcxSchedulerTreeListBrowser.InitScrollBarsParameters;
begin
  inherited;
  if VScrollBar <> nil then
    GetScrollBar(sbVertical).Data.Visible := False;
end;

function TcxSchedulerTreeListBrowser.InternalCollapseNode(ANode: TcxTreeListNode; ARecursive: Boolean): Boolean;
begin
  Result := ChangeExpanding(ANode, ARecursive, False);
end;

function TcxSchedulerTreeListBrowser.InternalExpandNode(ANode: TcxTreeListNode; ARecursive: Boolean): Boolean;
begin
  Result := ChangeExpanding(ANode, ARecursive, True);
end;

function TcxSchedulerTreeListBrowser.IsActionSupported(AButtonIndex: Integer): Boolean;
begin
  Result := (Storage <> nil) and Storage.IsActive and inherited IsActionSupported(AButtonIndex);
end;

procedure TcxSchedulerTreeListBrowser.Scroll(
  AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode;
    var AScrollPos: Integer);
begin
  inherited Scroll(AScrollBarKind, AScrollCode, AScrollPos);
  SynchronizeTopRow;
end;

procedure TcxSchedulerTreeListBrowser.SetContentLineHeight(AHeight: Integer);
begin
  ViewInfo.Calculate;
  DefaultRowHeight := AHeight - Byte(OptionsView.GridLines in [tlglBoth, tlglHorz]);
end;

procedure TcxSchedulerTreeListBrowser.SetHeaderHeight(AHeight: Integer);
begin
  ViewInfo.Calculate;
  OptionsView.BandLineHeight := AHeight - ViewInfo.HeadersHeight;
end;

procedure TcxSchedulerTreeListBrowser.SetMode(AMode: TcxSchedulerTreeBrowserDisplayMode);
begin
end;

function TcxSchedulerTreeListBrowser.GetFocusedEvent: TcxSchedulerEvent;
begin
  Result := TcxSchedulerControlEvent(TcxVirtualTreeListNode(FocusedNode).RecordHandle).Source;
end;

function TcxSchedulerTreeListBrowser.GetStorage: TcxCustomSchedulerStorage;
begin
  Result := TcxCustomScheduler(Owner).Storage;
end;

procedure TcxSchedulerTreeListBrowser.SetStyle(
  AValue: TcxShedulerTreeBrowserStyle);
begin
  if AValue <> FStyle then
  begin
    FStyle := AValue;
    OptionsView.ShowRoot := Style = tbsTree;
    FullRefresh;
  end;
end;

procedure TcxSchedulerTreeListBrowser.WMDROPTOGROUP(var AMessage: TMessage);
begin
  TcxSchedulerEvent(AMessage.WParam).ParentGroup := TcxSchedulerEvent(AMessage.LParam);
end;

procedure TcxSchedulerTreeListBrowser.WMUNLOCK(var AMessage: TMessage);
begin
  if Storage <> nil then
    TcxCustomScheduler(Owner).Storage.EndUpdate;
end;

procedure TcxSchedulerTreeListBrowser.WMGROUPUNGROUP(var AMessage: TMessage);
var
  AMakeGroup: Boolean;
  AEvent, AGroupEvent: TcxSchedulerEvent;
  APrevNode: TcxTreeListNode;
begin
  AMakeGroup := Boolean(AMessage.WParam);
  AEvent := FocusedEvent;
  APrevNode := FocusedNode.getPrev;
  if (APrevNode = nil) or (not AMakeGroup and VarIsNull(AEvent.GroupID)) then Exit;
  AGroupEvent := TcxSchedulerControlEvent(TcxVirtualTreeListNode(APrevNode).RecordHandle).Source;
  Storage.BeginUpdate;
  try
    if AMakeGroup then
      AEvent.ParentGroup := AGroupEvent
    else
    begin
      while AGroupEvent.GroupItems.Count > 0 do
        AGroupEvent.GroupItems.Last.Event.ParentGroup := nil;
      AGroupEvent.IsGroup := False;
    end;
  finally
    Storage.EndUpdate;
  end;
end;

procedure TcxSchedulerTreeListBrowser.SetTopRecordIndex(AIndex: Integer);
begin
  AIndex := Max(0, Min(AbsoluteVisibleCount - 1, AIndex * -1));
  if AIndex < AbsoluteVisibleCount then
    TopVisibleNode := AbsoluteVisibleItems[AIndex];
end;

procedure TcxSchedulerTreeListBrowser.SetView(
  AView: TcxSchedulerTimeGridView);
begin
  FView := AView;
  if View = nil then
    CustomDataSource := nil
  else
    CustomDataSource := FDataSource as TcxTreeListCustomDataSource;
end;

procedure TcxSchedulerTreeListBrowser.SynchronizeTopRow;
begin
  with TcxSchedulerViewAccess(View).ViewInfo do
  begin
    if (ResourceViewShiftList.Count > 0) and (TopVisibleNode <> nil) then
      if ResourceViewShift[0].Shift <> -TopVisibleNode.VisibleIndex then
      begin
         ResourceViewShift[0].Shift := -TopVisibleNode.VisibleIndex;
         View.Refresh;
      end;
  end;
end;

{ TcxTreeListBrowserColumn }

destructor TcxTreeListBrowserColumn.Destroy;
begin
  TObject(TcxTreeListColumn(Self).Data).Free;
  inherited Destroy;
end;

function TcxTreeListBrowserColumn.CanEdit: Boolean;
begin
  Result := inherited CanEdit;
  if Result and (TreeList.FocusedNode <> nil) then
    Result := not TcxSchedulerEvent(TcxVirtualTreeListNode(TreeList.FocusedNode).RecordHandle).IsGroup;
end;

{ TcxSchedulerTreeListBrowserViewInfo }

procedure TcxSchedulerTreeListBrowserViewInfo.CalculateDefaultHeights;
begin
  inherited;
  with TcxSchedulerView_ViewInfoAccess(View_ViewInfo) do
  begin
    FBandHeaderLineHeight := FMajorScaleHeight;
    FHeaderLineHeight := FMinorScaleHeight + FSelectionBarHeight;
  end;
end;

function TcxSchedulerTreeListBrowserViewInfo.GetBrowser: TcxSchedulerTreeListBrowser;
begin
  Result := TcxSchedulerTreeListBrowser(TreeList);
end;

function TcxSchedulerTreeListBrowserViewInfo.GetView: TcxSchedulerTimeGridView;
begin
  Result := Browser.View;
end;

function TcxSchedulerTreeListBrowserViewInfo.GetView_ViewInfo: TcxSchedulerTimeGridViewViewInfo;
begin
  Result := TcxSchedulerViewAccess(View).ViewInfo;
end;

initialization
  SchedulerTreeBrowserClass := TcxSchedulerTreeListBrowser;

finalization
  SchedulerTreeBrowserClass := nil;

end.
