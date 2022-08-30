{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumGrid Utils                                 }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSQUANTUMGRID AND ALL            }
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

unit cxGridUIHelper;

{$I cxVer.inc}

interface

uses
  Classes, SysUtils, cxGrid, cxGridCustomView;

const
  GROP_FIRST  = 0;
  GROP_LAST   = 1;
  GROP_PREV   = 2;
  GROP_NEXT   = 3;
  GROP_INSERT = 4;
  GROP_DELETE = 5;
  GROP_COPYTOCLIPBOARD = 6;

  GROP_SHOWCOLUMNCUSTOMIZING = 100;
  GROP_SHOWGROUPINGPANEL = 101;
  GROP_SHOWINDICATOR = 102;
  GROP_INVERTSELECT = 103;
  GROP_SHOWHEADERS = 104;
  GROP_SHOWSUMMARYFOOTER = 105;
  GROP_SHOWBANDS = 106;
  GROP_SHOWGRID = 107;
  GROP_COLUMNAUTOWIDTH = 108;
  GROP_SHOWPREVIEW = 109;
  GROP_SHOWEDITBUTTONS = 110;
  GROP_LAYOUTDIRECTION = 111;

type
  TcxCustomGridOperationHelperParameters = class
  private
    FView: TcxCustomGridView;
    FIsPerform: Boolean;
  public
    constructor Create(AView: TcxCustomGridView; AIsPerform: Boolean = True); virtual;
    property View: TcxCustomGridView read FView;
    property IsPerform: Boolean read FIsPerform;
  end;

  TcxShowingGridOperationHelperParameters = class(TcxCustomGridOperationHelperParameters)
  private
    FShowing: Boolean;
  public
    property Showing: Boolean read FShowing write FShowing;
  end;

  TcxGridOperationHelperProc = procedure(const AParameter: TcxCustomGridOperationHelperParameters) of object;

  TcxCustomGridViewOperationHelper = class
  private
    FList: TList;
  protected
    procedure RegisterOperation(AOperationIndex: Integer; const AProc: TcxGridOperationHelperProc);
    procedure RegisterOperations; virtual;
    function GetShowProperty(const AParameter: TcxCustomGridOperationHelperParameters): Boolean;
    procedure SetShowProperty(const AParameter: TcxCustomGridOperationHelperParameters; AShow: Boolean);

    procedure DoFirst(const AParameter: TcxCustomGridOperationHelperParameters);
    procedure DoLast(const AParameter: TcxCustomGridOperationHelperParameters);
    procedure DoNext(const AParameter: TcxCustomGridOperationHelperParameters);
    procedure DoPrev(const AParameter: TcxCustomGridOperationHelperParameters);
    procedure DoInsert(const AParameter: TcxCustomGridOperationHelperParameters);
    procedure DoDelete(const AParameter: TcxCustomGridOperationHelperParameters);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    class function GetViewClass: TcxCustomGridViewClass; virtual;
    function IsOperationAccessible(AOperationIndex: Integer): Boolean; virtual;
    function IsOperationEnabled(AView: TcxCustomGridView; AOperationIndex: Integer): Boolean; virtual;
    procedure PerformOperation(AOperationIndex: Integer; const AParameter: TcxCustomGridOperationHelperParameters); virtual;
  end;

  TcxCustomGridViewOperationHelperClass = class of TcxCustomGridViewOperationHelper;

  TcxCustomGridOperationHelper = class(TComponent)
  private
    FFocusedViewChangedNotification: TcxCustomGridNotification;
    FGrid: TcxGrid;
    FOnUpdateOperations: TNotifyEvent;
    FOnFocusedRecordChanged: TNotifyEvent;
    FOnCustomizationFormVisibleChanged: TNotifyEvent;

    function GetIsOperationAccessible(AOperationIndex: Integer): Boolean;
    function GetIsOperationEnabled(AOperationIndex: Integer): Boolean;
    function GetIsOperationShowing(AOperationIndex: Integer): Boolean;
    procedure SetGrid(const Value: TcxGrid);

    //TODO Sender should be TObject??
    procedure DoFocusedViewChanged;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoUpdateOperations; dynamic;
    procedure DoFocusedRecordChanged; dynamic;
    procedure DoCustomizationFormVisibleChanged; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function FocusedView: TcxCustomGridView;
    function FocusedViewClass: TcxCustomGridViewClass;

    procedure PerformOperation(AOperationIndex: Integer; const AParameters: TcxCustomGridOperationHelperParameters);
    procedure PerformStandardOperation(AOperationIndex: Integer);
    procedure PerformShowingOperation(AOperationIndex: Integer; AShow: Boolean);

    property IsOperationAccessible[AOperationIndex: Integer]: Boolean read GetIsOperationAccessible;
    property IsOperationEnabled[AOperationIndex: Integer]: Boolean read GetIsOperationEnabled;
    property IsOperationShowing[AOperationIndex: Integer]: Boolean read GetIsOperationShowing;
  public
    procedure DoFirst;
    procedure DoLast;
    procedure DoNext;
    procedure DoPrev;
    procedure DoInsert;
    procedure DoDelete;
    procedure CopyToClipboard;

    procedure DoShowColumnCustomizing(AShow: Boolean);
    procedure DoShowGroupingPanel(AShow: Boolean);
    procedure DoShowHeaders(AShow: Boolean);
    procedure DoShowBands(AShow: Boolean);
    procedure DoShowSummaryFooter(AShow: Boolean);
    procedure DoShowGrid(AShow: Boolean);
    procedure DoColumnAutoWidth(AShow: Boolean);
    procedure DoShowPreview(AShow: Boolean);
    procedure DoShowEditButtons(AShow: Boolean);
    procedure DoLayoutDirection(AShow: Boolean);
    procedure DoInvertSelect(AShow: Boolean);
    procedure DoShowIndicator(AShow: Boolean);

    function IsColumnsCustomizingShowing: Boolean;
    function IsGroupingPanelShowing: Boolean;
    function IsHeadersShowing: Boolean;
    function IsBandsShowing: Boolean;
    function IsSummaryFooterShowing: Boolean;
    function IsGridShowing: Boolean;
    function IsColumnAutoWidth: Boolean;
    function IsShowPreview: Boolean;
    function IsShowEditButtons: Boolean;
    function IsVertLayoutDirection: Boolean;
    function IsInvertSelect: Boolean;
    function IsShowIndicator: Boolean;

    property Grid: TcxGrid read FGrid write SetGrid;
    property OnUpdateOperations: TNotifyEvent read FOnUpdateOperations write FOnUpdateOperations;
    property onFocusedRecordChanged: TNotifyEvent read FOnFocusedRecordChanged write FOnFocusedRecordChanged;
    property OnCustomizationFormVisibleChanged: TNotifyEvent read FOnCustomizationFormVisibleChanged
                write FOnCustomizationFormVisibleChanged;
  end;

  TcxGridOperationHelper = class(TcxCustomGridOperationHelper)
  published
    property Grid;
    property OnUpdateOperations;
  end;

  procedure RegisterGridViewOperationHelper(AGridViewOperationHelperClass: TcxCustomGridViewOperationHelperClass);
  procedure UnregisterGridViewOperationHelper(AGridViewOperationHelperClass: TcxCustomGridViewOperationHelperClass);
  function GetGridViewOperationHelperByGridViewClass(const AGridViewClass: TcxCustomGridViewClass): TcxCustomGridViewOperationHelper;

implementation

uses
  cxCustomData, cxGridCustomTableView;

type
  TcxCustomGridViewOperationHelperRec = record
    Index: Integer;
    Proc: TcxGridOperationHelperProc;
  end;

  PcxCustomGridViewOperationHelperRec = ^TcxCustomGridViewOperationHelperRec;

  TcxGridNotifications = class(TcxCustomGridNotification)
  private
    FGridOperationHelper: TcxCustomGridOperationHelper;
  protected
    procedure Notify(AKind: TcxGridNotificationKind; AData: TObject; var AHandled: Boolean); override;
    function NotificationKinds: TcxGridNotificationKinds; override;
  end;

  procedure TcxGridNotifications.Notify(AKind: TcxGridNotificationKind; AData: TObject;
    var AHandled: Boolean);
  begin
    case AKind of
      gnkFocusedViewChanged:
        FGridOperationHelper.DoFocusedViewChanged;
      gnkFocusedRecordChanged, gnkRecordCountChanged:
        FGridOperationHelper.DoFocusedRecordChanged;
      gnkCustomization:
        FGridOperationHelper.DoCustomizationFormVisibleChanged;
    end;
  end;

  function TcxGridNotifications.NotificationKinds: TcxGridNotificationKinds;
  begin
    Result := [gnkFocusedViewChanged, gnkFocusedRecordChanged, gnkRecordCountChanged, gnkCustomization];
  end;

{ TcxCustomGridOperationHelperParameters }
constructor TcxCustomGridOperationHelperParameters.Create(
  AView: TcxCustomGridView; AIsPerform: Boolean = True);
begin
  inherited Create;
  FView := AView;
  FIsPerform := AIsPerform;
end;

{ TcxCustomGridViewOperationHelper }
constructor TcxCustomGridViewOperationHelper.Create;
begin
  inherited Create;
  FList := TList.Create;
  RegisterOperations;
end;

destructor TcxCustomGridViewOperationHelper.Destroy;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    Dispose(PcxCustomGridViewOperationHelperRec(FList[I]));
  FList.Free;
  inherited Destroy;
end;

class function TcxCustomGridViewOperationHelper.GetViewClass: TcxCustomGridViewClass;
begin
  Result := TcxCustomGridView;
end;

procedure TcxCustomGridViewOperationHelper.RegisterOperation(AOperationIndex: Integer; const AProc: TcxGridOperationHelperProc);
var
  ARec: PcxCustomGridViewOperationHelperRec;
begin
  New(ARec);
  ARec.Index := AOperationIndex;
  ARec.Proc := AProc;
  FList.Add(ARec);
end;

procedure TcxCustomGridViewOperationHelper.RegisterOperations;
begin
  RegisterOperation(GROP_FIRST, DoFirst);
  RegisterOperation(GROP_LAST, DoLast);
  RegisterOperation(GROP_NEXT, DoNext);
  RegisterOperation(GROP_PREV, DoPrev);
  RegisterOperation(GROP_INSERT, DoInsert);
  RegisterOperation(GROP_DELETE, DoDelete);
end;

function TcxCustomGridViewOperationHelper.IsOperationAccessible(AOperationIndex: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to FList.Count - 1 do
    if PcxCustomGridViewOperationHelperRec(FList[I]).Index = AOperationIndex then
    begin
      Result := True;
      break;
    end;
end;

function TcxCustomGridViewOperationHelper.IsOperationEnabled(AView: TcxCustomGridView; AOperationIndex: Integer): Boolean;

  function CanDelete: Boolean;
  begin
    Result := dceoDelete in AView.DataController.EditOperations;
    if Result and (AView is TcxCustomGridTableView) then
      Result := TcxCustomGridTableView(AView).OptionsData.Deleting;
  end;

  function CanInsert: Boolean;
  begin
    Result := dceoInsert in AView.DataController.EditOperations;
    if Result and (AView is TcxCustomGridTableView) then
      Result := TcxCustomGridTableView(AView).OptionsData.Inserting;
  end;

begin
  Result := True;
  case AOperationIndex of
    GROP_FIRST, GROP_PREV:
      Result := AView.DataController.Active and not AView.DataController.IsBOF;
    GROP_LAST, GROP_NEXT:
      Result := AView.DataController.Active and not AView.DataController.IsEOF;
    GROP_INSERT:
      Result := AView.DataController.Active  and CanInsert;
    GROP_DELETE:
      Result := AView.DataController.Active and CanDelete;
  end;
end;

procedure TcxCustomGridViewOperationHelper.PerformOperation(AOperationIndex: Integer; const AParameter: TcxCustomGridOperationHelperParameters);
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    if PcxCustomGridViewOperationHelperRec(FList[I]).Index = AOperationIndex then
    begin
      PcxCustomGridViewOperationHelperRec(FList[I]).Proc(AParameter);
      break;
    end;
end;

function TcxCustomGridViewOperationHelper.GetShowProperty(
  const AParameter: TcxCustomGridOperationHelperParameters): Boolean;
begin
  if AParameter is TcxShowingGridOperationHelperParameters then
    Result := (AParameter as TcxShowingGridOperationHelperParameters).Showing
  else Result := False;
end;

procedure TcxCustomGridViewOperationHelper.SetShowProperty(
  const AParameter: TcxCustomGridOperationHelperParameters; AShow: Boolean);
begin
  if AParameter is TcxShowingGridOperationHelperParameters then
    (AParameter as TcxShowingGridOperationHelperParameters).Showing := AShow;
end;

procedure TcxCustomGridViewOperationHelper.DoFirst(const AParameter: TcxCustomGridOperationHelperParameters);
begin
  AParameter.View.DataController.GotoFirst;
end;

procedure TcxCustomGridViewOperationHelper.DoLast(const AParameter: TcxCustomGridOperationHelperParameters);
begin
  AParameter.View.DataController.GotoLast;
end;

procedure TcxCustomGridViewOperationHelper.DoNext(const AParameter: TcxCustomGridOperationHelperParameters);
begin
  AParameter.View.DataController.GotoNext;
end;

procedure TcxCustomGridViewOperationHelper.DoPrev(const AParameter: TcxCustomGridOperationHelperParameters);
begin
  AParameter.View.DataController.GotoPrev;
end;

procedure TcxCustomGridViewOperationHelper.DoInsert(const AParameter: TcxCustomGridOperationHelperParameters);
begin
  AParameter.View.DataController.Insert;
end;

procedure TcxCustomGridViewOperationHelper.DoDelete(const AParameter: TcxCustomGridOperationHelperParameters);
begin
  if AParameter.View.DataController.GetSelectedCount > 0 then
    AParameter.View.DataController.DeleteSelection
  else AParameter.View.DataController.DeleteFocused;
end;

{ TcxCustomGridToolBarHelper }
constructor TcxCustomGridOperationHelper.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFocusedViewChangedNotification := TcxGridNotifications.Create;
  TcxGridNotifications(FFocusedViewChangedNotification).FGridOperationHelper := Self;
end;

destructor TcxCustomGridOperationHelper.Destroy;
begin
  Grid := nil;
  FreeAndNil(FFocusedViewChangedNotification);
  inherited Destroy;
end;

procedure TcxCustomGridOperationHelper.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Grid) then
    FGrid := nil;
end;

procedure TcxCustomGridOperationHelper.DoUpdateOperations;
begin
  if Assigned(FOnUpdateOperations) then
    FOnUpdateOperations(self);
end;

procedure TcxCustomGridOperationHelper.DoFocusedRecordChanged;
begin
  if Assigned(FOnFocusedRecordChanged) then
    FOnFocusedRecordChanged(self);
end;

procedure TcxCustomGridOperationHelper.DoCustomizationFormVisibleChanged;
begin
  if Assigned(FOnCustomizationFormVisibleChanged) then
    FOnCustomizationFormVisibleChanged(self);
end;

function TcxCustomGridOperationHelper.FocusedView: TcxCustomGridView;
begin
  if Grid <> nil then
    Result := Grid.FocusedView
  else Result := nil;
end;

function TcxCustomGridOperationHelper.FocusedViewClass: TcxCustomGridViewClass;
begin
  if FocusedView <> nil then
    Result := TcxCustomGridViewClass(FocusedView.ClassType)
  else Result := nil;
end;

procedure TcxCustomGridOperationHelper.PerformOperation(AOperationIndex: Integer;
        const AParameters: TcxCustomGridOperationHelperParameters);
begin
  if IsOperationEnabled[AOperationIndex] then
    GetGridViewOperationHelperByGridViewClass(
      FocusedViewClass).PerformOperation(AOperationIndex, AParameters);
end;

function TcxCustomGridOperationHelper.GetIsOperationAccessible(AOperationIndex: Integer): Boolean;
var
  AGridViewHelper: TcxCustomGridViewOperationHelper;
begin
  if FocusedView <> nil then
  begin
    AGridViewHelper := GetGridViewOperationHelperByGridViewClass(FocusedViewClass);
    Result := (AGridViewHelper <> nil) and AGridViewHelper.IsOperationAccessible(AOperationIndex);
  end else Result := False;
end;

function TcxCustomGridOperationHelper.GetIsOperationEnabled(AOperationIndex: Integer): Boolean;
begin
  Result := IsOperationAccessible[AOperationIndex] and
    GetGridViewOperationHelperByGridViewClass(
      FocusedViewClass).IsOperationEnabled(FocusedView, AOperationIndex);
end;

procedure TcxCustomGridOperationHelper.DoFocusedViewChanged;
begin
  if not (csDestroying in ComponentState) and not (csDesigning in ComponentState) and
   (Grid <> nil) and not (csDestroying in Grid.ComponentState) then
    DoUpdateOperations;
end;

procedure TcxCustomGridOperationHelper.SetGrid(const Value: TcxGrid);
begin
  if FGrid <> Value then
  begin
    if (FGrid <> nil) and not (csDestroying in FGrid.ComponentState) then
    begin
      FGrid.RemoveFreeNotification(self);
      FGrid.UnregisterNotification(FFocusedViewChangedNotification);
    end;
    FGrid := Value;
    if FGrid <> nil then
    begin
      FGrid.FreeNotification(self);
      FGrid.RegisterNotification(FFocusedViewChangedNotification);
    end;
  end;
end;

procedure TcxCustomGridOperationHelper.PerformStandardOperation(AOperationIndex: Integer);
var
  AParameters: TcxCustomGridOperationHelperParameters;
begin
  AParameters := TcxCustomGridOperationHelperParameters.Create(FocusedView);
  try
    PerformOperation(AOperationIndex, AParameters);
  finally
    AParameters.Free;
  end;
end;

procedure TcxCustomGridOperationHelper.PerformShowingOperation(AOperationIndex: Integer; AShow: Boolean);
var
  AParameters: TcxShowingGridOperationHelperParameters;
begin
  AParameters := TcxShowingGridOperationHelperParameters.Create(FocusedView);
  AParameters.Showing := AShow;
  try
    PerformOperation(AOperationIndex, AParameters);
  finally
    AParameters.Free;
  end;
end;

function TcxCustomGridOperationHelper.GetIsOperationShowing(AOperationIndex: Integer): Boolean;
var
  AParameters: TcxShowingGridOperationHelperParameters;
begin
  AParameters := TcxShowingGridOperationHelperParameters.Create(FocusedView, False);
  try
    PerformOperation(AOperationIndex, AParameters);
    Result := AParameters.Showing;
  finally
    AParameters.Free;
  end;
end;

procedure TcxCustomGridOperationHelper.DoFirst;
begin
  PerformStandardOperation(GROP_FIRST);
end;

procedure TcxCustomGridOperationHelper.DoLast;
begin
  PerformStandardOperation(GROP_LAST);
end;

procedure TcxCustomGridOperationHelper.DoNext;
begin
  PerformStandardOperation(GROP_NEXT);
end;

procedure TcxCustomGridOperationHelper.DoPrev;
begin
  PerformStandardOperation(GROP_PREV);
end;

procedure TcxCustomGridOperationHelper.DoInsert;
begin
  PerformStandardOperation(GROP_INSERT);
end;

procedure TcxCustomGridOperationHelper.DoDelete;
begin
  PerformStandardOperation(GROP_DELETE);
end;

procedure TcxCustomGridOperationHelper.CopyToClipboard;
begin
  PerformStandardOperation(GROP_COPYTOCLIPBOARD);
end;

procedure TcxCustomGridOperationHelper.DoShowColumnCustomizing(AShow: Boolean);
begin
  PerformShowingOperation(GROP_SHOWCOLUMNCUSTOMIZING, AShow);
end;

procedure TcxCustomGridOperationHelper.DoShowGroupingPanel(AShow: Boolean);
begin
  PerformShowingOperation(GROP_SHOWGROUPINGPANEL, AShow);
end;

procedure TcxCustomGridOperationHelper.DoShowHeaders(AShow: Boolean);
begin
  PerformShowingOperation(GROP_SHOWHEADERS, AShow);
end;

procedure TcxCustomGridOperationHelper.DoShowBands(AShow: Boolean);
begin
  PerformShowingOperation(GROP_SHOWBANDS, AShow);
end;

procedure TcxCustomGridOperationHelper.DoShowSummaryFooter(AShow: Boolean);
begin
  PerformShowingOperation(GROP_SHOWSUMMARYFOOTER, AShow);
end;

procedure TcxCustomGridOperationHelper.DoShowGrid(AShow: Boolean);
begin
  PerformShowingOperation(GROP_SHOWGRID, AShow);
end;

procedure TcxCustomGridOperationHelper.DoColumnAutoWidth(AShow: Boolean);
begin
  PerformShowingOperation(GROP_COLUMNAUTOWIDTH, AShow);
end;

procedure TcxCustomGridOperationHelper.DoShowPreview(AShow: Boolean);
begin
  PerformShowingOperation(GROP_SHOWPREVIEW, AShow);
end;

procedure TcxCustomGridOperationHelper.DoShowEditButtons(AShow: Boolean);
begin
  PerformShowingOperation(GROP_SHOWEDITBUTTONS, AShow);
end;

procedure TcxCustomGridOperationHelper.DoLayoutDirection(AShow: Boolean);
begin
  PerformShowingOperation(GROP_LAYOUTDIRECTION, AShow);
end;

procedure TcxCustomGridOperationHelper.DoInvertSelect(AShow: Boolean);
begin
  PerformShowingOperation(GROP_INVERTSELECT, AShow);
end;

procedure TcxCustomGridOperationHelper.DoShowIndicator(AShow: Boolean);
begin
  PerformShowingOperation(GROP_SHOWINDICATOR, AShow);
end;

function TcxCustomGridOperationHelper.IsColumnsCustomizingShowing: Boolean;
begin
  Result := IsOperationShowing[GROP_SHOWCOLUMNCUSTOMIZING];
end;

function TcxCustomGridOperationHelper.IsGroupingPanelShowing: Boolean;
begin
  Result := IsOperationShowing[GROP_SHOWGROUPINGPANEL];
end;

function TcxCustomGridOperationHelper.IsHeadersShowing: Boolean;
begin
  Result := IsOperationShowing[GROP_SHOWHEADERS];
end;

function TcxCustomGridOperationHelper.IsBandsShowing: Boolean;
begin
  Result := IsOperationShowing[GROP_SHOWBANDS];
end;

function TcxCustomGridOperationHelper.IsSummaryFooterShowing: Boolean;
begin
  Result := IsOperationShowing[GROP_SHOWSUMMARYFOOTER];
end;

function TcxCustomGridOperationHelper.IsGridShowing: Boolean;
begin
  Result := IsOperationShowing[GROP_SHOWGRID];
end;

function TcxCustomGridOperationHelper.IsColumnAutoWidth: Boolean;
begin
  Result := IsOperationShowing[GROP_COLUMNAUTOWIDTH];
end;

function TcxCustomGridOperationHelper.IsShowPreview: Boolean;
begin
  Result := IsOperationShowing[GROP_SHOWPREVIEW];
end;

function TcxCustomGridOperationHelper.IsShowEditButtons: Boolean;
begin
  Result := IsOperationShowing[GROP_SHOWEDITBUTTONS];
end;

function TcxCustomGridOperationHelper.IsVertLayoutDirection: Boolean;
begin
  Result := IsOperationShowing[GROP_LAYOUTDIRECTION];
end;

function TcxCustomGridOperationHelper.IsInvertSelect: Boolean;
begin
  Result := IsOperationShowing[GROP_INVERTSELECT];
end;

function TcxCustomGridOperationHelper.IsShowIndicator: Boolean;
begin
  Result := IsOperationShowing[GROP_SHOWINDICATOR];
end;

var
  FGridViewOperationHelperList: TList = nil;

procedure RegisterGridViewOperationHelper(AGridViewOperationHelperClass: TcxCustomGridViewOperationHelperClass);
begin
  FGridViewOperationHelperList.Add(AGridViewOperationHelperClass.Create);
end;

procedure UnregisterGridViewOperationHelper(AGridViewOperationHelperClass: TcxCustomGridViewOperationHelperClass);
var
  I: Integer;
begin
  for I := 0 to FGridViewOperationHelperList.Count - 1 do
    if TcxCustomGridViewOperationHelper(FGridViewOperationHelperList[I]).ClassType = AGridViewOperationHelperClass then
    begin
      TcxCustomGridViewOperationHelper(FGridViewOperationHelperList[I]).Free;
      FGridViewOperationHelperList.Delete(I);
    end;
end;

function GetGridViewOperationHelperByGridViewClass(const AGridViewClass: TcxCustomGridViewClass): TcxCustomGridViewOperationHelper;
var
  AItem: TcxCustomGridViewOperationHelper;
  I: Integer;
begin
  Result := nil;
  for I := 0 to FGridViewOperationHelperList.Count - 1 do
  begin
    AItem := TcxCustomGridViewOperationHelper(FGridViewOperationHelperList[I]);
    if AItem.GetViewClass = AGridViewClass then
    begin
      Result := AItem;
      break;
    end;
    if AGridViewClass.InheritsFrom(AItem.GetViewClass) then
      if (Result = nil) or not Result.InheritsFrom(AItem.GetViewClass) then
        Result := AItem;
  end;
end;

procedure ClearGridViewOperationHelperList;
var
  I: Integer;
begin
  for I := 0 to FGridViewOperationHelperList.Count - 1 do
    TcxCustomGridViewOperationHelper(FGridViewOperationHelperList[I]).Free;
  FGridViewOperationHelperList.Clear;
end;

initialization
  FGridViewOperationHelperList := TList.Create;
  RegisterGridViewOperationHelper(TcxCustomGridViewOperationHelper);

finalization
  ClearGridViewOperationHelperList;
  FGridViewOperationHelperList.Free;
  FGridViewOperationHelperList := nil;
end.
