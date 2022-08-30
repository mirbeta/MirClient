unit HybridAppFrameEmployeeEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, HybridAppBaseFrame, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit,
  dxLayoutcxEditAdapters, dxLayoutContainer, cxClasses, cxTextEdit, cxMaskEdit, cxButtonEdit, dxLayoutControl,
  dxLayoutControlAdapters, Menus, StdCtrls, cxButtons, cxImage, dxGDIPlusClasses, cxDBEdit, cxDropDownEdit,
  cxLookupEdit, cxDBLookupEdit, cxDBLookupComboBox, cxMemo, cxRichEdit, cxDBRichEdit, cxCalendar, cxStyles,
  cxCustomData, cxFilter, cxData, cxDataStorage, cxNavigator, DB, cxDBData, cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, cxGridLevel, cxGridCustomView, cxGrid, cxImageComboBox, cxProgressBar, dxCustomTileControl,
  HybridAppDM, cxGroupBox, dxSkinscxPCPainter,
  cxDataControllerConditionalFormattingRulesManagerDialog;

type
  TfrmEmployeeEdit = class(TfrmBase)
    dxLayoutGroup5: TdxLayoutGroup;
    dxLayoutGroup6: TdxLayoutGroup;
    edPhoto: TcxDBImage;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutGroup7: TdxLayoutGroup;
    edFirstName: TcxDBTextEdit;
    liFirstName: TdxLayoutItem;
    edLastName: TcxDBTextEdit;
    liLastName: TdxLayoutItem;
    edPrefix: TcxDBLookupComboBox;
    liPrefix: TdxLayoutItem;
    edTitle: TcxDBTextEdit;
    liTitle: TdxLayoutItem;
    edAddress: TcxDBTextEdit;
    liAddress: TdxLayoutItem;
    edCity: TcxDBTextEdit;
    liCity: TdxLayoutItem;
    edState: TcxDBLookupComboBox;
    liState: TdxLayoutItem;
    edZipCode: TcxDBTextEdit;
    liZipCode: TdxLayoutItem;
    dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem;
    edMobilePhone: TcxDBTextEdit;
    liMobilePhone: TdxLayoutItem;
    edHomePhone: TcxDBTextEdit;
    liHomePhone: TdxLayoutItem;
    dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup;
    edEmail: TcxDBTextEdit;
    liEmail: TdxLayoutItem;
    edSkype: TcxDBTextEdit;
    liSkype: TdxLayoutItem;
    edDepartment: TcxDBLookupComboBox;
    liDepartment: TdxLayoutItem;
    edStatus: TcxDBLookupComboBox;
    liStatus: TdxLayoutItem;
    edHireDate: TcxDBDateEdit;
    liHireDate: TdxLayoutItem;
    edBirthDate: TcxDBDateEdit;
    liDOB: TdxLayoutItem;
    dxLayoutSplitterItem1: TdxLayoutSplitterItem;
    edProfile: TcxDBRichEdit;
    dxLayoutItem22: TdxLayoutItem;
    lgTasks: TdxLayoutGroup;
    gvTasks: TcxGridDBTableView;
    lvTasks: TcxGridLevel;
    cxGridTasks: TcxGrid;
    dxLayoutItem23: TdxLayoutItem;
    colTask_AssignedTo: TcxGridDBColumn;
    colTask_OwnedBy: TcxGridDBColumn;
    colTasksTask_Subject: TcxGridDBColumn;
    colTasksTask_Priority: TcxGridDBColumn;
    colTasksTask_Due_Date: TcxGridDBColumn;
    colTasks_Complete: TcxGridDBColumn;
    lvEmployees: TcxGridLevel;
    gvEmployees: TcxGridDBTableView;
    gvEmployeesFullName: TcxGridDBColumn;
    gvEmployeesDepartment: TcxGridDBColumn;
    gvEmployeesTitle: TcxGridDBColumn;
    gvEmployeesStatus: TcxGridDBColumn;
    gvEmployeesPersonalProfile: TcxGridDBColumn;
    lgNotes: TdxLayoutGroup;
    gvNotes: TcxGridDBTableView;
    cxGridNotesLevel1: TcxGridLevel;
    cxGridNotes: TcxGrid;
    dxLayoutItem24: TdxLayoutItem;
    colNotesCreated_On: TcxGridDBColumn;
    colNotesCreated_By: TcxGridDBColumn;
    colNotesSubject: TcxGridDBColumn;
    dxLayoutItem1: TdxLayoutItem;
    btnSave: TcxButton;
    dxLayoutItem4: TdxLayoutItem;
    btnCancel: TcxButton;
    procedure btnSaveClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure colTasksTask_SubjectGetFilterValues(Sender: TcxCustomGridTableItem; AValueList: TcxDataFilterValueList);
  protected
    function GetDataSet: TDataSet; override;
    function GetParentFrameTileItem: TdxTileControlItem; override;
    procedure DoOnBackButtonClick; override;
    procedure Translate; override;
  end;

implementation

{$R *.dfm}

uses
  dxCore, MainUnit, LocalizationStrs;

procedure TfrmEmployeeEdit.btnSaveClick(Sender: TObject);
begin
  SaveData;
  ReturnToParentFrame;
end;

procedure TfrmEmployeeEdit.colTasksTask_SubjectGetFilterValues(Sender: TcxCustomGridTableItem; AValueList: TcxDataFilterValueList);
begin
  AssignGridFilterBoxFont(Sender, AValueList);
end;

procedure TfrmEmployeeEdit.btnCancelClick(Sender: TObject);
begin
  if not CanDeactivate then
    Exit;
  DataSet.Cancel;
  ReturnToParentFrame;
end;

function TfrmEmployeeEdit.GetDataSet: TDataSet;
begin
  Result := DM.dsEmployees.DataSet;
end;

function TfrmEmployeeEdit.GetParentFrameTileItem: TdxTileControlItem;
begin
  Result := MainForm.tbiEmployees;
end;

procedure TfrmEmployeeEdit.DoOnBackButtonClick;
begin
  btnCancel.Click;
end;

procedure TfrmEmployeeEdit.Translate;
begin
  inherited Translate;
  btnSave.Caption := cxGetResourceString(@sSaveButton);
  btnCancel.Caption := cxGetResourceString(@sCancelButton);

  liFirstName.Caption := cxGetResourceString(@sFirstNameLabel);
  liLastName.Caption := cxGetResourceString(@sLastNameLabel);
  liPrefix.Caption := cxGetResourceString(@sPrefixLabel);
  liTitle.Caption := cxGetResourceString(@sTitleLabel);
  liAddress.Caption := cxGetResourceString(@sAddressLabel);
  liCity.Caption := cxGetResourceString(@sCityLabel);
  liState.Caption := cxGetResourceString(@sStateLabel);
  liZipCode.Caption := cxGetResourceString(@sZipCodeLabel);
  liMobilePhone.Caption := cxGetResourceString(@sMobilePhoneLabel);
  liHomePhone.Caption := cxGetResourceString(@sHomePhoneLabel);
  liEmail.Caption := cxGetResourceString(@sEmailLabel);
  liSkype.Caption := cxGetResourceString(@sSkypeLabel);
  liDepartment.Caption := cxGetResourceString(@sDepartmentLabel);
  liStatus.Caption := cxGetResourceString(@sStatusLabel);
  liHireDate.Caption := cxGetResourceString(@sHireDateLabel);
  liDOB.Caption := cxGetResourceString(@sDOBLabel);

  lgTasks.Caption := cxGetResourceString(@sTasksLabel);
  colTask_AssignedTo.Caption := cxGetResourceString(@sAssignedToColumn);
  colTasksTask_Subject.Caption := cxGetResourceString(@sSubjectColumn);
  colTasksTask_Priority.Caption := cxGetResourceString(@sPriorityColumn);
  colTasksTask_Due_Date.Caption := cxGetResourceString(@sDueDateColumn);
  colTasks_Complete.Caption := cxGetResourceString(@sCompleteColumn);
  colTask_OwnedBy.Caption := cxGetResourceString(@sOwnedByColumn);

  lvEmployees.Caption := cxGetResourceString(@sAssignedEmployeesTab);
  gvEmployeesFullName.Caption := cxGetResourceString(@sFullNameColumn);
  gvEmployeesDepartment.Caption := cxGetResourceString(@sDepartmentColumn);
  gvEmployeesTitle.Caption := cxGetResourceString(@sTitleColumn);
  gvEmployeesStatus.Caption := cxGetResourceString(@sStatusColumn);
  gvEmployeesPersonalProfile.Caption := cxGetResourceString(@sPersonalProfile);

  lgNotes.Caption := cxGetResourceString(@sNotesLabel);
  colNotesCreated_On.Caption := cxGetResourceString(@sCreatedOnColumn);
  colNotesCreated_By.Caption := cxGetResourceString(@sCreatedByColumn);
  colNotesSubject.Caption := cxGetResourceString(@sSubjectColumn);
end;

initialization
  RegisterFrame(IDEmployeeEdit, TfrmEmployeeEdit);

end.
