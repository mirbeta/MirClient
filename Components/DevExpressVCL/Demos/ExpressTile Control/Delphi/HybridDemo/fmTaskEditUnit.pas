unit fmTaskEditUnit;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxClasses, dxLayoutContainer, dxLayoutControl,
  HybridAppDM, cxContainer, cxEdit, Menus, dxLayoutcxEditAdapters, dxLayoutControlAdapters, StdCtrls, cxButtons,
  cxDropDownEdit, cxCalendar, cxDBEdit, cxMaskEdit, cxLookupEdit, cxDBLookupEdit, cxDBLookupComboBox, cxTextEdit,
  cxMemo, cxRichEdit, cxDBRichEdit, cxImageComboBox, cxTrackBar, cxDBTrackBar, DB, dxForms, dxCore;

type
  TfmTaskEdit = class(TdxForm, IdxLocalizerListener)
    dxLayoutControl1: TdxLayoutControl;
    edProfile: TcxDBRichEdit;
    edHomePhone: TcxDBTextEdit;
    edStatus: TcxDBLookupComboBox;
    edStartDate: TcxDBDateEdit;
    edAssigned: TcxDBLookupComboBox;
    btnSave: TcxButton;
    btnCancel: TcxButton;
    edOwner: TcxDBLookupComboBox;
    dxLayoutControl1Group_Root: TdxLayoutGroup;
    dxLayoutControl1Group1: TdxLayoutGroup;
    liDescription: TdxLayoutItem;
    liSubject: TdxLayoutItem;
    liStatus: TdxLayoutItem;
    liStartDate: TdxLayoutItem;
    liAssignedTo: TdxLayoutItem;
    dxLayoutItem8: TdxLayoutItem;
    dxLayoutItem10: TdxLayoutItem;
    dxLayoutEmptySpaceItem2: TdxLayoutEmptySpaceItem;
    liOwner: TdxLayoutItem;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup;
    dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem;
    edDueDate: TcxDBDateEdit;
    liDueDate: TdxLayoutItem;
    dxLayoutEmptySpaceItem3: TdxLayoutEmptySpaceItem;
    edPriority: TcxDBImageComboBox;
    liPriority: TdxLayoutItem;
    dxLayoutAutoCreatedGroup3: TdxLayoutAutoCreatedGroup;
    edComplete: TcxTrackBar;
    liComplete: TdxLayoutItem;
    dxLayoutAutoCreatedGroup4: TdxLayoutAutoCreatedGroup;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    FStartPosition: Integer;
    function DataSet: TDataSet;
    function IsDataChanged: Boolean;
  protected
    procedure PostData; virtual;

    procedure Translate;
    procedure TranslationChanged;
  end;

implementation

{$R *.dfm}

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Math, LocalizationStrs;

function TfmTaskEdit.DataSet: TDataSet;
begin
  Result := edOwner.DataBinding.DataSource.DataSet;
end;

procedure TfmTaskEdit.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  AResult: Integer;
begin
  if IsDataChanged then
    if ModalResult <> mrCancel then
      PostData
    else
    begin
      AResult := MessageDlg(cxGetResourceString(@sSaveChangesConfirmation), mtConfirmation, [mbYes, mbNo, mbCancel], 0);
      CanClose := AResult <> IDCANCEL;
      if CanClose then
        if AResult = IDYES then
          PostData
        else
          DataSet.Cancel;
    end;
end;

procedure TfmTaskEdit.FormCreate(Sender: TObject);
begin
  dxResourceStringsRepository.AddListener(Self);
  Translate;
end;

procedure TfmTaskEdit.FormDestroy(Sender: TObject);
begin
  dxResourceStringsRepository.RemoveListener(Self);
end;

procedure TfmTaskEdit.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if key = #27 then
    Close;
end;

procedure TfmTaskEdit.FormShow(Sender: TObject);
var
  dH, dW: Integer;
begin
  FStartPosition := edComplete.Position;
  dH := Max(0, dxLayoutControl1.OccupiedClientHeight - ClientHeight);
  dW := Max(0, dxLayoutControl1.OccupiedClientWidth - ClientWidth);
  Width := Width + dW;
  Height := Height + dH;
end;

function TfmTaskEdit.IsDataChanged: Boolean;
begin
  Result := (DataSet.State = dsEdit) or (FStartPosition <> edComplete.Position);
end;

procedure TfmTaskEdit.PostData;
begin
  if DataSet.State <> dsEdit then
    DataSet.Edit;
  DataSet.FieldByName('Completion').AsInteger := edComplete.Position;
  DataSet.Post;
end;

procedure TfmTaskEdit.Translate;
begin
  Caption := cxGetResourceString(@sEditTaskFormCaption);
  liOwner.Caption := cxGetResourceString(@sOwnerLabel);
  liAssignedTo.Caption := cxGetResourceString(@sAssignedToLabel);
  liStartDate.Caption := cxGetResourceString(@sStartDateLabel);
  liDueDate.Caption := cxGetResourceString(@sDueDateLabel);
  liStatus.Caption := cxGetResourceString(@sStatusLabel);
  liPriority.Caption := cxGetResourceString(@sPriorityLabel);
  liSubject.Caption := cxGetResourceString(@sSubjectLabel);
  liDescription.Caption := cxGetResourceString(@sDescriptionLabel);
  liComplete.Caption := cxGetResourceString(@sCompleteLabel);
  btnSave.Caption := cxGetResourceString(@sSaveButton);
  btnCancel.Caption := cxGetResourceString(@sCancelButton);

  DM.LocalizePriorities(edPriority.Properties);
end;

procedure TfmTaskEdit.TranslationChanged;
begin
  Translate;
end;

end.
