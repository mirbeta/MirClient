unit FilterControlDemoMain;

{$I cxVer.inc}

interface

uses
  SysUtils, Classes, Controls, Forms, Menus, DB, StdCtrls, ComCtrls, DBClient,
  dxCore, cxStyles, cxCustomData, cxGraphics, cxFilter, cxData, cxDataStorage,
  cxEdit, cxClasses, cxControls, cxEditRepositoryItems, cxLookAndFeels, cxLookAndFeelPainters,
  cxNavigator, cxContainer, cxGroupBox, ActnList, cxCheckBox, cxLabel,
  cxTextEdit, cxMaskEdit, cxSpinEdit, cxDropDownEdit, cxImageComboBox, cxDBData,
  cxGridDBTableView, XPMan, DemoBasicMain, MidasLib, Dialogs, ImgList, cxTL,
  cxTLdxBarBuiltInMenu, cxDataControllerConditionalFormattingRulesManagerDialog,
  cxInplaceContainer, cxDBTL, cxTLData, dxmdaset, cxFilterControl, cxButtons,
  ExtCtrls;

type
  TfrmMain = class(TDemoBasicMainForm)
    erMain: TcxEditRepository;
    erMainFlag: TcxEditRepositoryImageItem;
    dsEmployeesGroups: TDataSource;
    mdEmployeesGroups: TdxMemData;
    mdEmployeesGroupsId: TStringField;
    mdEmployeesGroupsParentId: TStringField;
    mdEmployeesGroupsJobTitle: TStringField;
    mdEmployeesGroupsFirstName: TStringField;
    mdEmployeesGroupsLastName: TStringField;
    mdEmployeesGroupsCity: TStringField;
    mdEmployeesGroupsStateProvinceName: TStringField;
    mdEmployeesGroupsPhone: TStringField;
    mdEmployeesGroupsEmailAddress: TStringField;
    mdEmployeesGroupsAddressLine1: TStringField;
    mdEmployeesGroupsPostalCode: TStringField;
    TreeList: TcxDBTreeList;
    tlDBRecId: TcxDBTreeListColumn;
    tlDBId: TcxDBTreeListColumn;
    tlDBParentId: TcxDBTreeListColumn;
    tlDBJobTitle: TcxDBTreeListColumn;
    tlDBFirstName: TcxDBTreeListColumn;
    tlDBLastName: TcxDBTreeListColumn;
    tlDBCity: TcxDBTreeListColumn;
    tlDBStateProvinceName: TcxDBTreeListColumn;
    tlDBPhone: TcxDBTreeListColumn;
    tlDBEmailAddress: TcxDBTreeListColumn;
    tlDBAddressLine1: TcxDBTreeListColumn;
    tlDBPostalCode: TcxDBTreeListColumn;
    FilterControl: TcxFilterControl;
    Panel1: TPanel;
    btnApply: TcxButton;
    btnReset: TcxButton;
    btOpen: TcxButton;
    btSave: TcxButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure btOpenClick(Sender: TObject);
    procedure btSaveClick(Sender: TObject);
  end;

var
  frmMain: TfrmMain;

implementation

uses
  cxFilterControlStrs, Variants, AboutDemoForm;

{$R *.dfm}

procedure TfrmMain.btnApplyClick(Sender: TObject);
begin
  FilterControl.ApplyFilter;
end;

procedure TfrmMain.btnResetClick(Sender: TObject);
begin
  FilterControl.UpdateFilter;
end;

procedure TfrmMain.btOpenClick(Sender: TObject);
begin
  OpenDialog.FileName := '';
  if OpenDialog.Execute then
    FilterControl.LoadFromFile(OpenDialog.FileName);
end;

procedure TfrmMain.btSaveClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    FilterControl.SaveToFile(SaveDialog.FileName);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  APath: string;
begin
  APath := ExtractFilePath(Application.ExeName) + '..\..\Data\';
  mdEmployeesGroups.LoadFromBinaryFile(APath + 'EmployeesGroups.dat');
  mdEmployeesGroups.Open;
  TreeList.FullExpand;
  OpenDialog.Title := cxGetResourceString(@cxSFilterControlDialogOpenDialogCaption);
  OpenDialog.DefaultExt := cxGetResourceString(@cxSFilterControlDialogFileExt);
  OpenDialog.Filter := cxGetResourceString(@cxSFilterControlDialogFileFilter);
  SaveDialog.Title := cxGetResourceString(@cxSFilterControlDialogSaveDialogCaption);
  SaveDialog.DefaultExt := OpenDialog.DefaultExt;
  SaveDialog.Filter := cxGetResourceString(@cxSFilterControlDialogFileFilter);
  TreeList.Filter.Active := False;
  TreeList.Filter.AddItem(nil, tlDBJobTitle, foContains, 'Manager', 'Manager');
  TreeList.Filter.Active := True;
  FilterControl.UpdateFilter;
end;

end.
