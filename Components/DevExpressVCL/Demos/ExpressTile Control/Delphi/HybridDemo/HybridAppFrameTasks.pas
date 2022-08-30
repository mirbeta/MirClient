unit HybridAppFrameTasks;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, HybridAppBaseFrame, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit,
  dxLayoutcxEditAdapters, ImgList, dxLayoutContainer, cxClasses, dxGDIPlusClasses, cxImage, cxTextEdit, cxMaskEdit,
  cxButtonEdit, dxLayoutControl, dxCustomTileControl, dxTileControl, cxGridLevel, cxGridCustomView, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxGrid, cxStyles, cxCustomData, cxFilter, cxData, cxDataStorage, cxNavigator, DB,
  cxDBData, cxDBLookupComboBox, cxImageComboBox, cxProgressBar, dxLayoutControlAdapters, Menus, StdCtrls, cxButtons,
  HybridAppDM, fmTaskEditUnit, cxGroupBox,
  cxImageList, dxCore, dxDateRanges;

type
  TfrmTasks = class(TfrmBase)
    ilTasks: TcxImageList;
    tcTasksState: TdxTileControl;
    dxLayoutItem2: TdxLayoutItem;
    tcTasksStateGroup: TdxTileControlGroup;
    tiAllTasks: TdxTileControlItem;
    tiInProgress: TdxTileControlItem;
    tiNotStarted: TdxTileControlItem;
    tiDeferred: TdxTileControlItem;
    tiCompleted: TdxTileControlItem;
    tiHighPriority: TdxTileControlItem;
    tiUrgent: TdxTileControlItem;
    tiNeedAssist: TdxTileControlItem;
    dxLayoutItem3: TdxLayoutItem;
    cxGridTasks: TcxGrid;
    gvTasks: TcxGridDBTableView;
    colTask_AssignedTo: TcxGridDBColumn;
    colTask_OwnedBy: TcxGridDBColumn;
    colTasks_Subject: TcxGridDBColumn;
    colTasks_Priority: TcxGridDBColumn;
    colTasks_Due_Date: TcxGridDBColumn;
    colTasks_Complete: TcxGridDBColumn;
    gvEmployees: TcxGridDBTableView;
    gvEmployeesFullName: TcxGridDBColumn;
    gvEmployeesDepartment: TcxGridDBColumn;
    gvEmployeesTitle: TcxGridDBColumn;
    gvEmployeesStatus: TcxGridDBColumn;
    gvEmployeesPersonalProfile: TcxGridDBColumn;
    lvTasks: TcxGridLevel;
    lvEmployees: TcxGridLevel;
    dxLayoutItem1: TdxLayoutItem;
    btnEdit: TcxButton;
    dxLayoutItem7: TdxLayoutItem;
    btnDelete: TcxButton;
    dxLayoutSeparatorItem2: TdxLayoutSeparatorItem;
    dxLayoutItem8: TdxLayoutItem;
    btnPrint: TcxButton;
    procedure gvTasksKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure tcTasksStateItemFocusChange(Sender: TdxCustomTileControl; AFocusedItem,
      ANewFocusedItem: TdxTileControlItem);
    procedure gvTasksTcxGridDBDataControllerTcxDataSummaryFooterSummaryItems0GetText(Sender: TcxDataSummaryItem;
      const AValue: Variant; AIsFooter: Boolean; var AText: string);
    procedure btnEditClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnPrintClick(Sender: TObject);
    procedure colTasks_SubjectGetFilterValues(Sender: TcxCustomGridTableItem; AValueList: TcxDataFilterValueList);
    procedure TranslateFilters;
  protected
    procedure DoAfterActivate; override;

    procedure RefreshInfo; override;
    procedure Translate; override;
  end;

implementation

{$R *.dfm}

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  MainUnit, LocalizationStrs, dxBarStrs;

procedure TfrmTasks.DoAfterActivate;
begin
  inherited DoAfterActivate;
  if ActivatingCount = 1 then
    gvTasks.Controller.GoToFirst;
  if tcTasksState.Controller.FocusedItem = nil then
    tcTasksState.Controller.FocusedItem := tiAllTasks;
end;

procedure TfrmTasks.gvTasksKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DELETE then
  begin
    btnDelete.Click;
    Key := 0;
  end;
end;

procedure TfrmTasks.gvTasksTcxGridDBDataControllerTcxDataSummaryFooterSummaryItems0GetText(Sender: TcxDataSummaryItem;
  const AValue: Variant; AIsFooter: Boolean; var AText: string);
begin
  AText := cxGetResourceString(@sTotalOfTasksColumn) + #13 + AText;
end;

procedure TfrmTasks.colTasks_SubjectGetFilterValues(Sender: TcxCustomGridTableItem; AValueList: TcxDataFilterValueList);
begin
  AssignGridFilterBoxFont(Sender, AValueList);
end;

procedure TfrmTasks.TranslateFilters;
var
  I: Integer;
  AText: string;
begin
  DM.LocalizePriorities(colTasks_Priority.Properties as TcxImageComboBoxProperties);

  tcTasksState.Title.Text := cxGetResourceString(@sFilterTaskCaption);
  for I := 0 to tcTasksState.Items.Count - 1 do
  begin
    AText := DM.GetTaskStatusName(TdxTaskStatus(tcTasksState.Items[I].Tag));
    if AText = '' then
      case tcTasksState.Items[I].Tag of
        cAllCountTag:
          AText := cxGetResourceString(@sFilterAllCaption);
        cHighPriorityTaskCountTag:
          AText := DM.GetTaskPriorityTypeName(tpHigh);
        cUrgentPriorityTaskCountTag:
          AText := DM.GetTaskPriorityTypeName(tpUrgent);
      else
        AText := tcTasksState.Items[I].Text3.Value;
      end;
    tcTasksState.Items[I].Text3.Value := AText;
  end;
end;

procedure TfrmTasks.btnDeleteClick(Sender: TObject);
begin
  if MessageDlg(cxGetResourceString(@sDeleteConfirmation), mtConfirmation, [mbYes, mbNo], 0) = IDYES then
  begin
    gvTasks.Controller.DeleteSelection;
    DM.RecalculateTasksCount;
    RefreshInfo;
  end;
end;

procedure TfrmTasks.btnEditClick(Sender: TObject);
var
  fmTaskEdit: TfmTaskEdit;
begin
  fmTaskEdit := TfmTaskEdit.Create(Self);
  try
    fmTaskEdit.edComplete.Position := gvTasks.DataController.DataSource.DataSet.FieldByName('Completion').AsInteger;
    fmTaskEdit.ShowModal;
    RefreshInfo;
  finally
    fmTaskEdit.Free;
  end;
end;

procedure TfrmTasks.btnPrintClick(Sender: TObject);
begin
  ShowEditPage(MainForm.tbiTasksPrint);
end;

procedure TfrmTasks.tcTasksStateItemFocusChange(Sender: TdxCustomTileControl; AFocusedItem,
  ANewFocusedItem: TdxTileControlItem);
begin
  if ANewFocusedItem <> nil then
    DM.ApplyTaskStatusFilter(ANewFocusedItem.Tag);
end;

procedure TfrmTasks.RefreshInfo;
begin
  tcTasksStateItemFocusChange(tcTasksState, nil, tcTasksState.Controller.FocusedItem);
  DM.RecalculateTasksCount;

  tiAllTasks.Tag := cAllCountTag;
  tiHighPriority.Tag := cHighPriorityTaskCountTag;
  tiUrgent.Tag := cUrgentPriorityTaskCountTag;
  tiNotStarted.Tag := Integer(tsNotStarted);
  tiCompleted.Tag := Integer(tsCompleted);
  tiDeferred.Tag := Integer(tsDeferred);
  tiInProgress.Tag := Integer(tsInProgress);
  tiNeedAssist.Tag := Integer(tsNeedAssist);

  tiAllTasks.Text2.Value := IntToStr(DM.TaskStatusCount[cAllCountTag]);
  tiNotStarted.Text2.Value := IntToStr(DM.TaskStatusCount[tiNotStarted.Tag]);
  tiCompleted.Text2.Value := IntToStr(DM.TaskStatusCount[tiCompleted.Tag]);
  tiInProgress.Text2.Value := IntToStr(DM.TaskStatusCount[tiInProgress.Tag]);
  tiNeedAssist.Text2.Value := IntToStr(DM.TaskStatusCount[tiNeedAssist.Tag]);
  tiDeferred.Text2.Value := IntToStr(DM.TaskStatusCount[tiDeferred.Tag]);
  tiHighPriority.Text2.Value := IntToStr(DM.TaskStatusCount[tiHighPriority.Tag]);
  tiUrgent.Text2.Value := IntToStr(DM.TaskStatusCount[tiUrgent.Tag]);
end;

procedure TfrmTasks.Translate;
begin
  inherited Translate;

  TranslateFilters;

  colTask_AssignedTo.Caption := cxGetResourceString(@sAssignedToColumn);
  colTask_OwnedBy.Caption := cxGetResourceString(@sOwnedByColumn);
  colTasks_Subject.Caption := cxGetResourceString(@sSubjectColumn);
  colTasks_Complete.Caption := cxGetResourceString(@sCompleteColumn);
  colTasks_Priority.Caption := cxGetResourceString(@sPriorityColumn);
  colTasks_Due_Date.Caption := cxGetResourceString(@sDueDateColumn);

  lvEmployees.Caption := cxGetResourceString(@sAssignedEmployeesTab);
  gvEmployeesFullName.Caption := cxGetResourceString(@sFullNameColumn);
  gvEmployeesDepartment.Caption := cxGetResourceString(@sDepartmentColumn);
  gvEmployeesTitle.Caption := cxGetResourceString(@sTitleColumn);
  gvEmployeesStatus.Caption := cxGetResourceString(@sStatusColumn);
  gvEmployeesPersonalProfile.Caption := cxGetResourceString(@sPersonalProfile);

  btnEdit.Caption := cxGetResourceString(@sEditButton);
  btnDelete.Caption := cxGetResourceString(@dxSBAR_DELETE);
  btnPrint.Caption := cxGetResourceString(@sPrintButton);
end;

initialization
  RegisterFrame(IDTasks, TfrmTasks);

end.
