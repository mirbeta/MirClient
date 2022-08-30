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
unit cxDBNavigator;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Classes, DB, dxCore, cxDBFilterControl, cxNavigator;

type
  TcxCustomDBNavigator = class;

  TcxDBNavigatorDataLink = class(TDataLink)
  private
    FNavigator: TcxCustomDBNavigator;
  protected
    procedure EditingChanged; override;
    procedure DataSetChanged; override;
    procedure ActiveChanged; override;
  public
    constructor Create(ANavigator: TcxCustomDBNavigator);
    destructor Destroy; override;
  end;

  TcxDBNavigatorButtonsFilterEvent = procedure(Sender: TObject;
    AFilterControl: TcxDBFilterControl) of object;
  TcxDBNavigatorButtonsGetDataLink = function: TDataLink of object;

  TcxDBNavigatorButtons = class(TcxCustomNavigatorButtons)
  private
    FBookmark: TBookmark;
    FFilterCriteria: TMemoryStream;
    FFilterSetting: Boolean;
    FPageSize: Integer;
    FOnApplyFilter: TcxDBNavigatorButtonsFilterEvent;
    FOnGetDataLink: TcxDBNavigatorButtonsGetDataLink;
    FOnShowFilterDialog: TcxDBNavigatorButtonsFilterEvent;

    procedure DoApplyFilter(Sender: TObject);
    procedure DoShowFilterDialog(Sender: TObject);
    function GetDataLink: TDataLink;
    function GetDataSet: TDataSet;
    function GetInternalPageSize: Integer;
    procedure SetBookmark(Value: TBookmark);
    procedure SetPageSize(Value: Integer);
    procedure ShowFilter;
  protected
    procedure DoButtonClick(ADefaultIndex: Integer); override;
    function GetButtonEnabled(ADefaultIndex: Integer): Boolean; override;
    procedure CheckBookmark;
    procedure ClearFilterCriteria;
    property Bookmark: TBookmark read FBookmark write SetBookmark;
    property DataLink: TDataLink read GetDataLink;
    property DataSet: TDataSet read GetDataSet;
    property InternalPageSize: Integer read GetInternalPageSize;
    property OnGetDataLink: TcxDBNavigatorButtonsGetDataLink
      read FOnGetDataLink write FOnGetDataLink;
  public
    constructor Create(ANavigator: IcxNavigatorOwner); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure ReleaseBookmark; override;
  published
    property ConfirmDelete;
    property CustomButtons;
    property Images;
    property PageSize: Integer read FPageSize write SetPageSize default 0;
    property OnShowFilterDialog: TcxDBNavigatorButtonsFilterEvent
      read FOnShowFilterDialog write FOnShowFilterDialog;
    property OnApplyFilter: TcxDBNavigatorButtonsFilterEvent
      read FOnApplyFilter write FOnApplyFilter;
    property First;
    property PriorPage;
    property Prior;
    property Next;
    property NextPage;
    property Last;
    property Insert;
    property Append;
    property Delete;
    property Edit;
    property Post;
    property Cancel;
    property Refresh;
    property SaveBookmark;
    property GotoBookmark;
    property Filter;
  end;

  TcxDBNavigatorFilterEvent = procedure(ANavigator: TcxCustomDBNavigator;
    AFilterControl: TcxDBFilterControl) of object;

  TcxCustomDBNavigator = class(TcxCustomNavigator,
    IcxNavigatorRecordPosition)
  private
    FDataLink: TcxDBNavigatorDataLink;
    FOnApplyFilter: TcxDBNavigatorFilterEvent;
    FOnShowFilterDialog: TcxDBNavigatorFilterEvent;

    procedure DoApplyFilter(Sender: TObject; AFilterControl: TcxDBFilterControl);
    procedure DoShowFilterDialog(Sender: TObject; AFilterControl: TcxDBFilterControl);
    function GetButtons: TcxDBNavigatorButtons;
    function GetDataLink: TDataLink;
    function GetDataSource: TDataSource;
    function GetDataSet: TDataSet;
    function GetIRecordPosition: IcxNavigatorRecordPosition;
    function GetInfoPanel: TcxNavigatorControlInfoPanel;
    procedure SetButtons(Value: TcxDBNavigatorButtons);
    procedure SetDataSource(Value: TDataSource);
    procedure SetOnApplyFilter(Value: TcxDBNavigatorFilterEvent);
    procedure SetOnShowFilterDialog(Value: TcxDBNavigatorFilterEvent);
    procedure SetInfoPanel(Value: TcxNavigatorControlInfoPanel);
  protected
    //IcxNavigatorRecordPosition
    function GetRecordCount: Integer;
    function GetRecordIndex: Integer;

    function CreateButtons: TcxCustomNavigatorButtons; override;
    function CreateInfoPanel: TcxCustomNavigatorInfoPanel; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure DataSetActiveChanged; virtual;
    procedure DataSetDataChanged; virtual;

    property Buttons: TcxDBNavigatorButtons read GetButtons write SetButtons;
    property DataLink: TcxDBNavigatorDataLink read FDataLink;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property InfoPanel: TcxNavigatorControlInfoPanel read GetInfoPanel write SetInfoPanel;
    property OnApplyFilter: TcxDBNavigatorFilterEvent read FOnApplyFilter
      write SetOnApplyFilter;
    property OnShowFilterDialog: TcxDBNavigatorFilterEvent read FOnShowFilterDialog
      write SetOnShowFilterDialog;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property DataSet: TDataSet read GetDataSet;
  end;

  TcxDBNavigator = class(TcxCustomDBNavigator)
  published
    property BorderStyle;
    property Buttons;
    property DataSource;
    property InfoPanel;
    property LookAndFeel;
    property OnApplyFilter;
    property OnShowFilterDialog;
    property Align;
    property Anchors;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Ctl3D;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses
  Windows, SysUtils, Controls, DBCtrls, Dialogs, cxClasses,
  cxFilterControlDialog, cxEditConsts;

type
  TDataSetAccess = class(TDataSet);

{ TcxDBNavigatorDataLink }

constructor TcxDBNavigatorDataLink.Create(ANavigator: TcxCustomDBNavigator);
begin
  inherited Create;
  FNavigator := ANavigator;
  VisualControl := True;
end;

destructor TcxDBNavigatorDataLink.Destroy;
begin
  FNavigator := nil;
  inherited Destroy;
end;

procedure TcxDBNavigatorDataLink.EditingChanged;
begin
  if FNavigator <> nil then
    FNavigator.NavigatorStateChanged;
end;

procedure TcxDBNavigatorDataLink.DataSetChanged;
begin
  if FNavigator <> nil then
  begin
    FNavigator.DataSetDataChanged;
    FNavigator.NavigatorStateChanged;
  end;
end;

procedure TcxDBNavigatorDataLink.ActiveChanged;
begin
  if FNavigator <> nil then
  begin
    FNavigator.DataSetActiveChanged;
    FNavigator.NavigatorStateChanged;
  end;
end;

{ TcxDBNavigatorButtons }

constructor TcxDBNavigatorButtons.Create(ANavigator: IcxNavigatorOwner);
begin
  inherited Create(ANavigator);
  FPageSize := 0;
  FFilterCriteria := TMemoryStream.Create;
end;

destructor TcxDBNavigatorButtons.Destroy;
begin
  FreeAndNil(FFilterCriteria);
  inherited Destroy;
end;

procedure TcxDBNavigatorButtons.Assign(Source: TPersistent);
begin
  if Source is TcxDBNavigatorButtons then
    with TcxDBNavigatorButtons(Source) do
    begin
      Self.PageSize := PageSize;
      Self.OnApplyFilter := OnApplyFilter;
      Self.OnShowFilterDialog := OnShowFilterDialog;
    end
  else
    inherited Assign(Source);
end;

procedure TcxDBNavigatorButtons.ReleaseBookmark;
begin
  Bookmark := nil;
end;

procedure TcxDBNavigatorButtons.DoApplyFilter(Sender: TObject);
begin
  ClearFilterCriteria;
  TcxDBFilterControl(Sender).SaveToStream(FFilterCriteria);
  FFilterSetting := True;
  try
    if Assigned(FOnApplyFilter) then
      FOnApplyFilter(Self, TcxDBFilterControl(Sender))
    else
    begin
      DataSet.Filter := TcxDBFilterControl(Sender).GetFilterTextEx('[', ']');
      DataSet.Filtered := True;
    end;
  finally
    FFilterSetting := False;
  end;
end;

procedure TcxDBNavigatorButtons.DoShowFilterDialog(Sender: TObject);
var
  AFilterControl: TcxDBFilterControl;
begin
  AFilterControl := TfmFilterControlDialog(Sender).FilterControl as TcxDBFilterControl;
  if FFilterCriteria.Size > 0 then
  begin
    FFilterCriteria.Position := 0;
    AFilterControl.LoadFromStream(FFilterCriteria);
  end;
  if Assigned(OnShowFilterDialog) then
    OnShowFilterDialog(Self, AFilterControl);
  if not Assigned(FOnApplyFilter) then
    with AFilterControl.FilterOptions do
    begin
      SupportedBetween := False;
      SupportedIn := False;
      SupportedLike := False;
    end;
end;

function TcxDBNavigatorButtons.GetDataLink: TDataLink;
begin
  if Assigned(FOnGetDataLink) then
    Result := FOnGetDataLink
  else Result := nil;
end;

function TcxDBNavigatorButtons.GetDataSet: TDataSet;
begin
  if DataLink <> nil then
    Result := DataLink.DataSet
  else Result := nil;
end;

function TcxDBNavigatorButtons.GetInternalPageSize: Integer;
begin
  if PageSize > 0 then
    Result := PageSize
  else
    if (DataSet <> nil) then
      Result := TDataSetAccess(DataSet).BufferCount
    else
      Result := 0;
end;

procedure TcxDBNavigatorButtons.SetBookmark(Value: TBookmark);
begin
  if Value <> FBookmark then
  begin
    if FBookmark <> nil then
      if DataSet <> nil then
        DataSet.FreeBookmark(FBookmark);
    FBookmark := Value;
    Navigator.NavigatorStateChanged;
  end;
end;

procedure TcxDBNavigatorButtons.SetPageSize(Value: Integer);
begin
  if Value > - 1 then
    FPageSize := Value;
end;

procedure TcxDBNavigatorButtons.ShowFilter;
var
  AShowFilterEvent: TNotifyEvent;
begin
  AShowFilterEvent := DoShowFilterDialog;
  ExecuteDBFilterControlDialog(DataSet, Navigator.GetNavigatorLookAndFeel,
    DoApplyFilter, AShowFilterEvent);
end;

procedure TcxDBNavigatorButtons.DoButtonClick(ADefaultIndex: Integer);
begin
  if (DataLink = nil) or not DataLink.Active then
    Exit;
  case ADefaultIndex of
    NBDI_FIRST: DataSet.First;
    NBDI_PRIORPAGE: DataSet.MoveBy(-InternalPageSize);
    NBDI_PRIOR: DataSet.Prior;
    NBDI_LAST: DataSet.Last;
    NBDI_NEXTPAGE: DataSet.MoveBy(InternalPageSize);
    NBDI_NEXT: DataSet.Next;
    NBDI_INSERT: DataSet.Insert;
    NBDI_APPEND: DataSet.Append;
    NBDI_DELETE:
      if not ConfirmDelete or
        (MessageDlg(cxGetResourceString(@cxNavigator_DeleteRecordQuestion),
        mtConfirmation, mbOKCancel, 0) = ID_OK) then
          DataSet.Delete;
    NBDI_EDIT: DataSet.Edit;
    NBDI_POST: DataSet.Post;
    NBDI_CANCEL: DataSet.Cancel;
    NBDI_SAVEBOOKMARK: Bookmark := DataSet.GetBookmark;
    NBDI_GOTOBOOKMARK: DataSet.GotoBookmark(Bookmark);
    NBDI_REFRESH: DataSet.Refresh;
    NBDI_FILTER: ShowFilter;
  end;
end;

function TcxDBNavigatorButtons.GetButtonEnabled(ADefaultIndex: Integer): Boolean;
begin
  Result := IsNavigatorEnabled and (DataLink <> nil) and DataLink.Active;
  if Result then
  begin
    case ADefaultIndex of
      NBDI_FIRST, NBDI_PRIOR, NBDI_PRIORPAGE: Result := not DataSet.Bof;
      NBDI_LAST, NBDI_NEXT, NBDI_NEXTPAGE: Result := not DataSet.Eof;
      NBDI_INSERT, NBDI_APPEND: Result := DataSet.CanModify;
      NBDI_DELETE: Result := DataSet.CanModify and
                  not (DataSet.BOF and DataSet.EOF);
      NBDI_EDIT: Result := DataSet.CanModify and not DataLink.Editing;
      NBDI_POST, NBDI_CANCEL:  Result := DataSet.CanModify and DataLink.Editing;
      NBDI_GOTOBOOKMARK: Result := Bookmark <> nil;
      NBDI_SAVEBOOKMARK: Result := TDataSetAccess(DataSet).BookmarkAvailable;
    end;
  end;
end;

procedure TcxDBNavigatorButtons.CheckBookmark;
begin
  if (Bookmark <> nil) and not DataSet.BookmarkValid(Bookmark) then
    ReleaseBookmark;
end;

procedure TcxDBNavigatorButtons.ClearFilterCriteria;
begin
  if not FFilterSetting then
    FFilterCriteria.Clear;
end;

{ TcxCustomDBNavigator }

constructor TcxCustomDBNavigator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TcxDBNavigatorDataLink.Create(Self);
end;

destructor TcxCustomDBNavigator.Destroy;
begin
  FreeAndNil(FDataLink);
  inherited Destroy;
end;

procedure TcxCustomDBNavigator.DoApplyFilter(Sender: TObject;
  AFilterControl: TcxDBFilterControl);
begin
  if Assigned(FOnApplyFilter) then
    FOnApplyFilter(Self, AFilterControl);
end;

procedure TcxCustomDBNavigator.DoShowFilterDialog(Sender: TObject;
  AFilterControl: TcxDBFilterControl);
begin
  if Assigned(FOnShowFilterDialog) then
    FOnShowFilterDialog(Self, AFilterControl);
end;

function TcxCustomDBNavigator.GetButtons: TcxDBNavigatorButtons;
begin
  Result := TcxDBNavigatorButtons(CustomButtons);
end;

function TcxCustomDBNavigator.GetDataLink: TDataLink;
begin
  Result := FDataLink;
end;

function TcxCustomDBNavigator.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TcxCustomDBNavigator.GetInfoPanel: TcxNavigatorControlInfoPanel;
begin
  Result := TcxNavigatorControlInfoPanel(CustomInfoPanel);
end;

function TcxCustomDBNavigator.GetDataSet: TDataSet;
begin
  Result := FDataLink.DataSet;
end;

function TcxCustomDBNavigator.GetIRecordPosition: IcxNavigatorRecordPosition;
begin
  Supports(Self, IcxNavigatorRecordPosition, Result)
end;

procedure TcxCustomDBNavigator.SetButtons(Value: TcxDBNavigatorButtons);
begin
  CustomButtons.Assign(Value);
end;

procedure TcxCustomDBNavigator.SetDataSource(Value: TDataSource);
begin
  if DataSource <> Value then
  begin
    if (DataSource <> nil) and not (csDestroying in DataSource.ComponentState) then
      DataSource.RemoveFreeNotification(Self);
    FDataLink.DataSource := Value;
    if (DataSource <> nil) then
      DataSource.FreeNotification(Self);
  end;
end;

procedure TcxCustomDBNavigator.SetInfoPanel(Value: TcxNavigatorControlInfoPanel);
begin
  CustomInfoPanel.Assign(Value);
end;

procedure TcxCustomDBNavigator.SetOnApplyFilter(Value: TcxDBNavigatorFilterEvent);
begin
  FOnApplyFilter := Value;
  if Assigned(FOnApplyFilter) then
    Buttons.OnApplyFilter := DoApplyFilter
  else
    Buttons.OnApplyFilter := nil;
end;

procedure TcxCustomDBNavigator.SetOnShowFilterDialog(Value: TcxDBNavigatorFilterEvent);
begin
  FOnShowFilterDialog := Value;
  if Assigned(FOnShowFilterDialog) then
    Buttons.OnShowFilterDialog := DoShowFilterDialog
  else
    Buttons.OnShowFilterDialog := nil;
end;

function TcxCustomDBNavigator.GetRecordCount: Integer;
begin
  if (DataSet = nil) or not DataSet.Active then
    Result := 0
  else
    Result := DataSet.RecordCount;
end;

function TcxCustomDBNavigator.GetRecordIndex: Integer;
begin
  if (DataSet = nil) or not DataSet.Active then
    Result := 0
  else
    Result := DataSet.RecNo;
  Dec(Result);
end;

function TcxCustomDBNavigator.CreateButtons: TcxCustomNavigatorButtons;
begin
  Result := TcxDBNavigatorButtons.Create(Self);
  TcxDBNavigatorButtons(Result).OnGetDataLink := GetDataLink;
end;

function TcxCustomDBNavigator.CreateInfoPanel: TcxCustomNavigatorInfoPanel;
begin
  Result := TcxNavigatorControlInfoPanel.Create(Self);
  Result.OnGetIRecordPosition := GetIRecordPosition;
end;

procedure TcxCustomDBNavigator.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil)
    and (AComponent = DataSource) then
    DataSource := nil;
end;

procedure TcxCustomDBNavigator.DataSetActiveChanged;
begin
  Buttons.ClearFilterCriteria;
  Buttons.ReleaseBookmark;
end;

procedure TcxCustomDBNavigator.DataSetDataChanged;
begin
  Buttons.CheckBookmark;
end;

end.
