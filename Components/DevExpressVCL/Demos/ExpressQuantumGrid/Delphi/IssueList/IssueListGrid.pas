unit IssueListGrid;

{$I cxVer.inc}

interface

uses
  Variants, Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, StdCtrls, Mask, ExtCtrls, cxGridLevel,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxControls,
  cxGridCustomView, cxGrid, cxGridCustomPopupMenu, cxGridPopupMenu, cxStyles,
  Menus, cxCustomData, cxGraphics, cxFilter, cxData, cxEdit, DB, cxDBData,
  cxClasses, cxGridBandedTableView, cxGridDBBandedTableView, cxLookAndFeels,
  cxDataStorage, cxDBLookupComboBox, cxCalendar, cxBlobEdit,
  cxLookAndFeelPainters;

type
  TIssueListGridForm = class(TForm)
    cxGrid: TcxGrid;
    tvItems: TcxGridDBTableView;
    tvItemsNAME: TcxGridDBColumn;
    tvItemsPROJECTID: TcxGridDBColumn;
    tvItemsTYPE: TcxGridDBColumn;
    tvItemsOWNERID: TcxGridDBColumn;
    tvItemsCREATEDDATE: TcxGridDBColumn;
    tvItemsSTATUS: TcxGridDBColumn;
    tvItemsPRIORITY: TcxGridDBColumn;
    tvItemsCREATORID: TcxGridDBColumn;
    tvItemsLASTMODIFIEDDATE: TcxGridDBColumn;
    tvItemsFIXEDDATE: TcxGridDBColumn;
    tvItemsRESOLUTION: TcxGridDBColumn;
    tvItemsDESCRIPTION: TcxGridDBColumn;
    tvProjects: TcxGridDBTableView;
    tvProjectsNAME: TcxGridDBColumn;
    tvProjectsMANAGERID: TcxGridDBColumn;
    tvUsers: TcxGridDBTableView;
    tvUsersFNAME: TcxGridDBColumn;
    tvUsersMNAME: TcxGridDBColumn;
    tvUsersLNAME: TcxGridDBColumn;
    tvUsersEMAIL: TcxGridDBColumn;
    tvUsersPHONE: TcxGridDBColumn;
    tvUsersDEPARTMENTID: TcxGridDBColumn;
    tvTeams: TcxGridDBTableView;
    tvTeamsPROJECTID: TcxGridDBColumn;
    tvTeamsUSERID: TcxGridDBColumn;
    tvTeamsFUNCTION: TcxGridDBColumn;
    tvDepartments: TcxGridDBTableView;
    tvDepartmentsNAME: TcxGridDBColumn;
    lvProjects: TcxGridLevel;
    lvProjectItems: TcxGridLevel;
    lvItems: TcxGridLevel;
    lvDepartments: TcxGridLevel;
    lvDepartmentUsers: TcxGridLevel;
    lvTeam: TcxGridLevel;
    lvUsers: TcxGridLevel;
    pnlForm: TPanel;
    Splitter1: TSplitter;
    cxGridPopupMenu1: TcxGridPopupMenu;
    lvSchedule: TcxGridLevel;
    btnSchedule: TcxGridDBBandedTableView;
    btnScheduleID: TcxGridDBBandedColumn;
    btnSchedulePROJECTID: TcxGridDBBandedColumn;
    btnScheduleUSERID: TcxGridDBBandedColumn;
    btnScheduleSUNDAY: TcxGridDBBandedColumn;
    btnScheduleMONDAY: TcxGridDBBandedColumn;
    btnScheduleTUESDAY: TcxGridDBBandedColumn;
    btnScheduleWEDNESDAY: TcxGridDBBandedColumn;
    btnScheduleTHURSDAY: TcxGridDBBandedColumn;
    btnScheduleFRIDAY: TcxGridDBBandedColumn;
    btnScheduleSATURDAY: TcxGridDBBandedColumn;
    btnScheduleRowSum: TcxGridDBBandedColumn;
    btnScheduleRowAvg: TcxGridDBBandedColumn;
    lbDesciption: TLabel;
    procedure cxGridFocusedViewChanged(Sender: TcxCustomGrid;
      APrevFocusedView, AFocusedView: TcxCustomGridView);
    procedure FormCreate(Sender: TObject);
    procedure cxGridActiveTabChanged(Sender: TcxCustomGrid;
      ALevel: TcxGridLevel);
    procedure cxGridRootLevelStylesGetTabStyle(Sender,
      ATabLevel: TcxGridLevel; var AStyle: TcxStyle);
    procedure FormActivate(Sender: TObject);
    procedure tvItemsStylesGetContentStyle(Sender: TcxCustomGridTableView;
      ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem;
      var AStyle: TcxStyle);
  private
    FIsDependsOnData: boolean;

    procedure GoToForm(AID: Integer);
    function GetFormIDByDataSetName(ADataSetName: string): Integer;
    procedure ChangeUsersLevel(ALevel: TcxGridLevel);
    procedure ChangeItemsLevel(ALevel: TcxGridLevel);

    procedure ChangeDescription(AIndex: Integer);
    function GetGridLevelByTag(ATag: Integer): TcxGridLevel;

    function GetFocusedView: TcxGridTableView;
  public
    // operations
    procedure DoGoProject(AIndex: integer);

    procedure DoSetShowPictures(Value: boolean);
    procedure DoSetShowDescription(Value: boolean);
    procedure DoSetShowDependsOnData(Value: boolean);
    procedure DoSetShowNewItemRow(Value: boolean);

    procedure DoSetLookAndFeelKind(AKind: TcxLookAndFeelKind);
    procedure DoSetEditorsShadow(Value: boolean);

    procedure DoSetShowIndicator(Value: boolean);
    procedure DoSetShowGrouping(Value: boolean);
    procedure DoSetShowHeader(Value: boolean);
    procedure DoSetShowFooter(Value: boolean);
    procedure DoSetAutoWidth(Value: boolean);
    procedure DoSetInvertSelected(Value: boolean);
    procedure DoSetAutoPreview(Value: boolean);
    procedure DoSetShowEditButtons(Value: boolean);
    procedure DoSetShowGridLines(Value: boolean);

    procedure DoFullCollapse;
    procedure DoFullExpand;
    procedure DoColumnsCustomization;
    procedure DoBestFit;

    property FocusedView: TcxGridTableView read GetFocusedView;
    property IsDependsOnData: boolean read FIsDependsOnData write FIsDependsOnData;
  end;

implementation

uses
  cxStyleSheetEditor,
  IssueListData, IssueListForms, IssueListForm, cxGridDBDataDefinitions,
  IssueListConst, IssueListMain, cxEditRepositoryItems;

{$R *.dfm}

procedure TIssueListGridForm.GoToForm(AID: Integer);
begin
   FormManager.ShowForm(AID, pnlForm);
end;


procedure TIssueListGridForm.cxGridFocusedViewChanged(
  Sender: TcxCustomGrid; APrevFocusedView,
  AFocusedView: TcxCustomGridView);
var
   AID: Integer;
   ADataController: TcxGridDBDataController;
begin
  if AFocusedView <> nil then
  begin
    AFocusedView.BeginUpdate;
    try
      ADataController := TcxGridDBDataController(GetGridViewDataController(AFocusedView));
      if ADataController.DataSet <> nil then
      begin
         AID := GetFormIDByDataSetName(ADataController.DataSet.Name);
         GoToForm(AID);
         if ADataController.RecordCount = 0 then
           ADataController.Append;
       end;
      (AFocusedView.Controller as TcxGridTableController).Customization := False;
    finally
      AFocusedView.EndUpdate;
    end;
  end;
end;

procedure TIssueListGridForm.FormCreate(Sender: TObject);
begin
  GoToForm(ProjectsFormID);
  FIsDependsOnData := True;
end;

procedure TIssueListGridForm.cxGridActiveTabChanged(
  Sender: TcxCustomGrid; ALevel: TcxGridLevel);
begin
  ChangeDescription(ALevel.Tag);
  if (ALevel = lvDepartments) or (ALevel = lvUsers) then
    ChangeUsersLevel(ALevel);
  if (ALevel = lvProjects) or (ALevel = lvItems) then
    ChangeItemsLevel(ALevel);
end;

function TIssueListGridForm.GetFormIDByDataSetName(
  ADataSetName: string): Integer;
begin
  Result := 0;
  if ADataSetName = 'cdsItems' then
     Result := ItemsFormID;
  if ADataSetName = 'cdsProjects' then
     Result := ProjectsFormID;
  if ADataSetName = 'cdsUsers' then
     Result := UsersFormID;
  if ADataSetName = 'cdsTeam' then
     Result := TeamsFormID;
  if ADataSetName = 'cdsDepartments' then
     Result := DepartmentsFormID;
  if ADataSetName = 'cdsScheduler' then
     Result := ScheduleFormID;
end;

function TIssueListGridForm.GetGridLevelByTag(ATag: Integer): TcxGridLevel;
var
  I: Integer;
begin
  for I := 0 to cxGrid.Levels.Count - 1 do
  begin
    Result := cxGrid.Levels[I];
    if cxGrid.Levels[I].Tag = ATag then Exit;
  end;
  Result := nil;
end;

procedure TIssueListGridForm.ChangeDescription(AIndex: Integer);
const
  ADescription: array[1..6] of string =
   ('A project list showing project managers and the issues for each project',
    'A list of issues with problem descriptions and classifications ',
    'A list of departments displaying employee details',
    'A list of employees working on a project showing their work status',
    'A list of employees displaying personal data',
    'An operating schedule of users working on a project during a week');
begin
  lbDesciption.Caption := ADescription[AIndex];
end;

procedure TIssueListGridForm.DoSetShowGrouping(Value: boolean);
begin
  FocusedView.OptionsView.GroupByBox := Value;
end;

procedure TIssueListGridForm.DoSetShowIndicator(Value: boolean);
begin
  FocusedView.OptionsView.Indicator := Value;
end;

procedure TIssueListGridForm.DoGoProject(AIndex: integer);
var
  ALevel: TcxGridLevel;
  AInfo: TcxGridTopDetailsSiteViewInfo;
begin
  ALevel := GetGridLevelByTag(AIndex);
  AInfo := TcxGridTopDetailsSiteViewInfo(cxGrid.ViewInfo.DetailsSiteViewInfo);
  if AInfo <> nil then
    AInfo.ChangeActiveTab(ALevel);
end;

procedure TIssueListGridForm.DoColumnsCustomization;
begin
  TcxGridTableController(cxGrid.FocusedView.Controller).Customization := True;
end;

procedure TIssueListGridForm.DoSetShowPictures(Value: boolean);
var
  I: Integer;
  AImageList: TImageList;
begin
  if Value then
     AImageList := dmMain.imStat
  else AImageList := nil;

  with dmMain.edrepMain do
  for I := 0 to Count -1 do
    if (Items[I] is TcxEditRepositoryImageComboBoxItem) then
      TcxEditRepositoryImageComboBoxItem(Items[I]).Properties.Images := AImageList;
end;

procedure TIssueListGridForm.DoSetLookAndFeelKind(AKind: TcxLookAndFeelKind);
begin
  cxGrid.LookAndFeel.Kind := AKind;
end;

procedure TIssueListGridForm.DoSetShowDescription(Value: boolean);
begin
  lbDesciption.Visible := Value;
end;

procedure TIssueListGridForm.DoSetShowDependsOnData(Value: boolean);
begin
  FIsDependsOnData := Value;
  tvItems.LayoutChanged;
  tvProjects.DataController.ClearDetails;
end;

procedure TIssueListGridForm.DoSetShowHeader(Value: boolean);
begin
  FocusedView.OptionsView.Header := Value;
end;

procedure TIssueListGridForm.DoSetShowFooter(Value: boolean);
begin
  FocusedView.OptionsView.Footer := Value;
end;

procedure TIssueListGridForm.DoSetAutoWidth(Value: boolean);
begin
  FocusedView.OptionsView.ColumnAutoWidth := Value;
end;

procedure TIssueListGridForm.cxGridRootLevelStylesGetTabStyle(Sender,
  ATabLevel: TcxGridLevel; var AStyle: TcxStyle);
begin
  if TcxGridLevel(ATabLevel).GridView = cxGrid.ActiveView then
    AStyle := dmMain.stSelected;
end;

procedure TIssueListGridForm.DoSetInvertSelected(Value: boolean);
begin
  FocusedView.OptionsSelection.InvertSelect := Value;
end;

procedure TIssueListGridForm.ChangeItemsLevel(ALevel: TcxGridLevel);
begin
  tvItems.BeginUpdate;
  try
    lvProjectItems.Visible :=  ALevel = lvProjects;
    tvItemsPROJECTID.Visible := False;
    if ALevel = lvProjects then
    begin
     tvItemsPROJECTID.GroupIndex := -1;
     lvProjectItems.GridView := tvItems;
     cxGrid.FocusedView := tvProjects;
    end
    else
    begin
      tvItemsPROJECTID.GroupIndex := 0;
      lvItems.GridView := tvItems;
      cxGrid.FocusedView := tvItems;
    end;
 finally
  tvItems.EndUpdate;
 end;
end;

procedure TIssueListGridForm.ChangeUsersLevel(ALevel: TcxGridLevel);
begin
  tvUsers.BeginUpdate;
  try
    lvDepartmentUsers.Visible :=  ALevel = lvDepartments;
    tvUsersDEPARTMENTID.Visible := False;
    if ALevel = lvDepartments then
    begin
      tvUsersDEPARTMENTID.GroupIndex := -1;
      lvDepartmentUsers.GridView := tvUsers;
      cxGrid.FocusedView := tvDepartments;
    end
    else
    begin
     tvUsersDEPARTMENTID.GroupIndex := 0;
     lvUsers.GridView := tvUsers;
     cxGrid.FocusedView := tvUsers;
    end;
  finally
    tvUsers.EndUpdate;
  end;
end;

function TIssueListGridForm.GetFocusedView: TcxGridTableView;
begin
  Result := TcxGridTableView(cxGrid.FocusedView);
end;

procedure TIssueListGridForm.DoBestFit;
begin
  FocusedView.ApplyBestFit;
end;

procedure TIssueListGridForm.DoFullCollapse;
begin
  FocusedView.ViewData.Collapse(True);
end;

procedure TIssueListGridForm.DoFullExpand;
begin
  FocusedView.ViewData.Expand(True);
end;

procedure TIssueListGridForm.DoSetShowEditButtons(Value: boolean);
begin
  if Value then
    FocusedView.OptionsView.ShowEditButtons := gsebForFocusedRecord
  else FocusedView.OptionsView.ShowEditButtons := gsebNever;
end;

procedure TIssueListGridForm.DoSetShowGridLines(Value: boolean);
begin
  if Value then
    FocusedView.OptionsView.GridLines := glBoth
  else FocusedView.OptionsView.GridLines := glNone;
end;

procedure TIssueListGridForm.DoSetAutoPreview(Value: boolean);
begin
  FocusedView.Preview.Visible := Value;
end;

procedure TIssueListGridForm.DoSetShowNewItemRow(Value: boolean);
begin
  FocusedView.OptionsView.NewItemRow := Value;
end;

procedure TIssueListGridForm.FormActivate(Sender: TObject);
begin
  ChangeDescription(1);
end;

procedure TIssueListGridForm.tvItemsStylesGetContentStyle(
  Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord;
  AItem: TcxCustomGridTableItem; var AStyle: TcxStyle);
var
  AStatus: Integer;
begin
  if not IsDependsOnData then
   Exit;
  AStatus := -1;
  if (ARecord <> nil) and   (AItem <> nil ) then
    if not VarIsNull(TcxCustomGridRecord(ARecord).Values[tvItemsSTATUS.Index]) then
    AStatus := VarAsType(TcxCustomGridRecord(ARecord).Values[tvItemsSTATUS.Index], varInteger);

  case AStatus of
    1: AStyle := dmMain.stNew;
    2: AStyle := dmMain.stPostponed;
    3: AStyle := dmMain.stFixed;
    4: AStyle := dmMain.stRejected;
  end;
end;

procedure TIssueListGridForm.DoSetEditorsShadow(Value: boolean);
begin
  dmMain.edstcMain.Style.Shadow := Value;
end;

end.
