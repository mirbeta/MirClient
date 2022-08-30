unit UnboundExternalDataDemoMain;

interface

uses

  Windows,Forms, Dialogs, SysUtils, cxStyles, Classes, ActnList, ImgList, Controls, Menus,
  ComCtrls, StdCtrls, cxGridLevel, cxClasses, cxControls, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGrid, cxDataStorage,
  cxCustomData, cxGraphics, cxFilter, cxData, cxEdit, cxLookAndFeels,
  cxLookAndFeelPainters, BaseForm, cxGridCardView;

type
  TUnboundExternalDataDemoMainForm = class(TfmBaseForm)
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    cxGrid: TcxGrid;
    tvSections: TcxGridTableView;
    tvValues: TcxGridTableView;
    SectionLevel: TcxGridLevel;
    DetailLevel: TcxGridLevel;
    N1: TMenuItem;
    miOpen: TMenuItem;
    miSave: TMenuItem;
    miSaveAs: TMenuItem;
    Edit1: TMenuItem;
    miInsertSection: TMenuItem;
    miDeleteSection: TMenuItem;
    procedure FormDestroy(Sender: TObject);
    procedure miOpenClick(Sender: TObject);
    procedure miSaveClick(Sender: TObject);
    procedure miSaveAsClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure miInsertSectionClick(Sender: TObject);
    procedure miDeleteSectionClick(Sender: TObject);
  private
    FChangesCount: Integer;
    procedure CustomizeGrid;
    procedure DoSmthOnModify(Sender: TObject);
    procedure DoStatusBarResize(Sender: TObject);
    procedure GenerateColumns;
    procedure Load(const AFileName: string);
    procedure LoadData;
    procedure ResetChanges;
    procedure SetChangesCount(AValue: Integer);
    procedure UpdateFileInfo(const AFileName: string);
    //
    property ChangesCount: Integer read FChangesCount write SetChangesCount;
  protected
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel;
      AChangedValues: TcxLookAndFeelValues); override;
  public
    procedure AfterConstruction; override;
  end;

var
  UnboundExternalDataDemoMainForm: TUnboundExternalDataDemoMainForm;

implementation

{$R *.dfm}

uses
  UnboundExternalDataDemoClasses, AboutDemoForm;

resourcestring
  rsIniFileName = 'odbcinst_test.ini';

var
  IniFile: TUserIniFile;
  UserDataSource: TUserDataSource;
  UserDetailDataSource: TUserDetailDataSource;

procedure TUnboundExternalDataDemoMainForm.AfterConstruction;
begin
  inherited AfterConstruction;
  IniFile := TUserIniFile.Create(ExtractFilePath(Application.ExeName) + rsIniFileName);
  UpdateFileInfo(IniFile.FileName);
  IniFile.OnModify := DoSmthOnModify;
  UserDataSource := TUserDataSource.Create(IniFile);
  UserDetailDataSource := TUserDetailDataSource.Create(UserDataSource);
  StatusBar.OnResize := DoStatusBarResize;
  CustomizeGrid;
end;

procedure TUnboundExternalDataDemoMainForm.CustomizeGrid;
begin
  GenerateColumns;
  LoadData;
end;

procedure TUnboundExternalDataDemoMainForm.DoSmthOnModify(Sender: TObject);
begin
  ChangesCount := ChangesCount + 1;
end;

procedure TUnboundExternalDataDemoMainForm.GenerateColumns;
begin
  with tvSections do
  begin
    with CreateColumn as TcxGridColumn do
    begin
      Caption := 'Section Name';
      DataBinding.ValueTypeClass := TcxStringValueType;
      Width := 600;
    end;
    DataController.CustomDataSource := UserDataSource;
  end;

  with tvValues do
  begin
    with CreateColumn as TcxGridColumn do
    begin
      Caption := 'Parameter Name';
      DataBinding.ValueTypeClass := TcxStringValueType;
      DataBinding.Data := Pointer(0);
      Width := 300;
    end;
    with CreateColumn as TcxGridColumn do
    begin
      Caption := 'Parameter Value';
      DataBinding.ValueTypeClass := TcxStringValueType;
      DataBinding.Data := Pointer(1);
      Width := 300;
    end;
    DataController.CustomDataSource := UserDetailDataSource;
  end;
end;

procedure TUnboundExternalDataDemoMainForm.Load(const AFileName: string);
begin
  IniFile.Rename(AFileName, True);
  UserDataSource.DataChanged;
end;

procedure TUnboundExternalDataDemoMainForm.LoadData;
begin
  Load(IniFile.FileName);
end;

procedure TUnboundExternalDataDemoMainForm.LookAndFeelChanged(
  Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  inherited LookAndFeelChanged(Sender, AChangedValues);
  UpdateTableViewStyleSheet(tvSections);
  UpdateTableViewStyleSheet(tvValues);
end;

procedure TUnboundExternalDataDemoMainForm.SetChangesCount(AValue: Integer);
begin
  FChangesCount := AValue;
  StatusBar.Panels[2].Text := 'Changes Count: ' + IntToStr(ChangesCount);
end;

procedure TUnboundExternalDataDemoMainForm.UpdateFileInfo(const AFileName: string);
begin
  StatusBar.Panels[1].Text := ExtractFileName(AFileName);
end;

procedure TUnboundExternalDataDemoMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(UserDetailDataSource);
  FreeAndNil(UserDataSource);
  FreeAndNil(IniFile);
end;

procedure TUnboundExternalDataDemoMainForm.miOpenClick(Sender: TObject);
var
  I: Integer;
begin
  if IniFile.Modified then
  begin
    I := MessageDlg('Do you want to save the changes ?', mtConfirmation, [mbYes, mbNo, mbCancel], 0);
    case I of
      mrYes:
        IniFile.SaveValues;
      mrCancel:
        Exit;
    end;
  end;

  OpenDialog.InitialDir := ExtractFilePath(Application.ExeName);
  with OpenDialog do
    if Execute then
    begin
      Load(FileName);
      UpdateFileInfo(FileName);
      ResetChanges;
    end;
end;

procedure TUnboundExternalDataDemoMainForm.miSaveClick(Sender: TObject);
begin
  IniFile.SaveValues;
  ResetChanges;
end;

procedure TUnboundExternalDataDemoMainForm.miSaveAsClick(Sender: TObject);
begin
  with SaveDialog do
    if Execute then
    begin
      IniFile.SaveAs(FileName);
      UpdateFileInfo(FileName);
      ResetChanges;
    end;
end;

procedure TUnboundExternalDataDemoMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  I: Integer;
begin
  I := -1;
  if IniFile.Modified then
  I := MessageDlg('Do you want to save the changes ?', mtConfirmation, [mbYes, mbNo, mbCancel], 0);
  case I of
    mrYes:
      IniFile.SaveValues;
    mrCancel:
      CanClose := False;
  end;
end;

procedure TUnboundExternalDataDemoMainForm.miInsertSectionClick(Sender: TObject);
begin
  cxGrid.FocusedView.DataController.Insert;
end;

procedure TUnboundExternalDataDemoMainForm.miDeleteSectionClick(Sender: TObject );
begin
  cxGrid.FocusedView.DataController.DeleteFocused;
end;

procedure TUnboundExternalDataDemoMainForm.DoStatusBarResize(Sender: TObject);
begin
  with Statusbar do
    Panels[0].Width := Width - (Panels[1].Width + Panels[2].Width)
end;

procedure TUnboundExternalDataDemoMainForm.ResetChanges;
begin
  ChangesCount := 0;
end;

end.  
