unit InplaceEditFormMain;

interface

{$I cxVer.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ActnList,
  Dialogs, cxStyles, cxCustomData, cxGraphics, cxFilter, cxData, cxDataStorage,
  cxEdit, DB, cxDBData, cxControls, cxGridCustomView, cxGridCustomTableView,
  cxClasses, cxGridLevel, cxGrid, StdCtrls, Menus, cxMemo, cxImage, cxCurrencyEdit,
  cxHyperLinkEdit, cxTextEdit, cxEditRepositoryItems, cxLookAndFeels, cxLookAndFeelPainters,
  dxLayoutContainer, cxGridLayoutView, cxGridDBLayoutView, cxGridCustomLayoutView,
  cxContainer, cxGroupBox, dxLayoutLookAndFeels, ExtCtrls, cxButtons, 
  dxmdaset, BaseForm, cxGridTableView, cxRadioGroup, cxCheckBox, cxGridCardView,
  ComCtrls, ImgList, cxLabel, cxMaskEdit, cxDropDownEdit, cxNavigator, cxGridDBTableView,
  cxSpinEdit, cxGridViewLayoutContainer, cxGridInplaceEditForm, CarsDataForGrid;

type
  TfrmMain = class(TfmBaseForm)
    Grid: TcxGrid;
    EditRepository: TcxEditRepository;
    EditRepositoryImage: TcxEditRepositoryImageItem;
    EditRepositoryMemo: TcxEditRepositoryMemoItem;
    EditRepositoryHyperLink: TcxEditRepositoryHyperLinkItem;
    EditRepositoryPrice: TcxEditRepositoryCurrencyItem;
    EditRepositoryAutomatic: TcxEditRepositoryCheckBoxItem;
    miCustomize: TMenuItem;
    GridLevel1: TcxGridLevel;
    stValues: TcxStyle;
    stItems: TcxStyle;
    stHeader: TcxStyle;
    stRecordCaption: TcxStyle;
    Images: TcxImageList;
    stRecordSelected: TcxStyle;
    TableView: TcxGridDBTableView;
    TableViewRecId: TcxGridDBColumn;
    TableViewID: TcxGridDBColumn;
    TableViewTrademark: TcxGridDBColumn;
    TableViewModel: TcxGridDBColumn;
    TableViewHP: TcxGridDBColumn;
    TableViewCyl: TcxGridDBColumn;
    TableViewTransmissSpeedCount: TcxGridDBColumn;
    TableViewTransmissAutomatic: TcxGridDBColumn;
    TableViewMPG_City: TcxGridDBColumn;
    TableViewMPG_Highway: TcxGridDBColumn;
    TableViewCategory: TcxGridDBColumn;
    TableViewDescription: TcxGridDBColumn;
    TableViewHyperlink: TcxGridDBColumn;
    TableViewPicture: TcxGridDBColumn;
    TableViewPrice: TcxGridDBColumn;
    miEditMode: TMenuItem;
    miInplace: TMenuItem;
    miInplaceEditForm: TMenuItem;
    miInplaceEditFormHideCurrentRow: TMenuItem;
    alAction: TActionList;
    actCustomizeEditForm: TAction;
    gbOptions: TcxGroupBox;
    btnCustomizeEditForm: TcxButton;
    actInplace: TAction;
    actInplaceEditForm: TAction;
    actInplaceEditFormHCR: TAction;
    cxGroupBox1: TcxGroupBox;
    rbInplace: TcxRadioButton;
    rbInplaceEditForm: TcxRadioButton;
    rbInplaceEditFormHideCurrentRow: TcxRadioButton;
    miHotTrack: TMenuItem;
    actHotTrack: TAction;
    procedure actCustomizeEditFormExecute(Sender: TObject);
    procedure actEditModeChange(Sender: TObject);
    procedure actHotTrackExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.actCustomizeEditFormExecute(Sender: TObject);
begin
  inherited;
  TableView.Controller.ShowEditFormCustomizationDialog;
end;

procedure TfrmMain.actEditModeChange(Sender: TObject);
begin
  inherited;
  if (Sender = actInplace) then
  begin
    TableView.OptionsBehavior.EditMode := emInplace;
    actCustomizeEditForm.Enabled := False;
  end
  else
  begin
    if (Sender = actInplaceEditForm) then
      TableView.OptionsBehavior.EditMode := emInplaceEditForm
    else
      TableView.OptionsBehavior.EditMode := emInplaceEditFormHideCurrentRow;
    actCustomizeEditForm.Enabled := True;
  end;
end;

procedure TfrmMain.actHotTrackExecute(Sender: TObject);
begin
  inherited;
  TableView.EditForm.ItemHotTrack := actHotTrack.Checked;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  TableView.Controller.FocusedRowIndex := 0;
  TableView.Controller.FocusedRow.Expand(False);
  TableView.Controller.FocusedRowIndex := 1;
  (TableView.Controller.FocusedRow as TcxGridDataRow).EditFormVisible := True;
end;

end.
