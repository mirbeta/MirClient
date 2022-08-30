unit ViewTableSimpleDemoMain;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  cxGridLevel, cxControls, cxGridCustomView, cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, cxGrid, ExtCtrls, Grids, DBGrids, cxStyles, Menus, ActnList,
  ImgList, cxLookAndFeels, ComCtrls, cxCustomData, cxGraphics, cxFilter, cxData,
  cxEdit, DB, cxDBData, cxClasses, cxDataStorage, cxImage, cxTextEdit, cxMemo,
  cxCheckBox, cxBlobEdit, cxHyperLinkEdit, cxLookAndFeelPainters, BaseForm,
  cxGridCardView;

type
  TViewTableSimpleDemoMainForm = class(TfmBaseForm)
    cxgFilms: TcxGrid;
    cxgFilmsDBTableView: TcxGridDBTableView;
    cxgFilmsDBTableViewCAPTION: TcxGridDBColumn;
    cxgFilmsDBTableViewCOLOR: TcxGridDBColumn;
    cxgFilmsDBTableViewICON: TcxGridDBColumn;
    cxgFilmsDBTableViewPHOTO: TcxGridDBColumn;
    cxgFilmsDBTableViewPLOTOUTLINE: TcxGridDBColumn;
    cxgFilmsDBTableViewRUNTIME: TcxGridDBColumn;
    cxgFilmsDBTableViewTAGLINE: TcxGridDBColumn;
    cxgFilmsDBTableViewWEBSITE: TcxGridDBColumn;
    cxgFilmsDBTableViewYEAR: TcxGridDBColumn;
    cxgFilmsLevel: TcxGridLevel;
    cxgGenras: TcxGrid;
    cxgGenrasDBTableView: TcxGridDBTableView;
    cxgGenrasDBTableViewNAME: TcxGridDBColumn;
    cxgGenrasLevel: TcxGridLevel;
    miFocusCellOnTab: TMenuItem;
    miImmediateEditor: TMenuItem;
    miIncSearch: TMenuItem;
    miMultiSelect: TMenuItem;
    miOptions: TMenuItem;
    miSeparator5: TMenuItem;
    miShowIndicator: TMenuItem;
    miShowNavigator: TMenuItem;
    miShowPreviewRow: TMenuItem;
    pnFilms: TPanel;
    pnFilmsCaption: TPanel;
    pnGenres: TPanel;
    pnGenresCaption: TPanel;
    Splitter: TSplitter;
    procedure FormShow(Sender: TObject);
    procedure miAboutClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miFocusCellOnTabClick(Sender: TObject);
    procedure miImmediateEditorClick(Sender: TObject);
    procedure miIncSearchClick(Sender: TObject);
    procedure miMultiSelectClick(Sender: TObject);
    procedure miShowIndicatorClick(Sender: TObject);
    procedure miShowNavigatorClick(Sender: TObject);
    procedure miShowPreviewRowClick(Sender: TObject);
  private
    procedure FocusRomanceCategory;
  protected
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel;
      AChangedValues: TcxLookAndFeelValues); override;
  end;

var
  ViewTableSimpleDemoMainForm: TViewTableSimpleDemoMainForm;

implementation

{$R *.dfm}

uses
{$IFDEF CLR}
  Variants,
{$ENDIF}
  AboutDemoForm, ViewTableSimpleDemoData;

procedure TViewTableSimpleDemoMainForm.miExitClick(Sender: TObject);
begin
  Close;
end;

procedure TViewTableSimpleDemoMainForm.FocusRomanceCategory;
var
  I: Integer;
begin
  for I:=0 to cxgGenrasDBTableView.ViewInfo.RecordsViewInfo.Count - 1 do
    if cxgGenrasDBTableView.ViewData.Records[i].Values[0] = 'Romance' then
    begin
      cxgGenrasDBTableView.ViewData.Records[i].Focused := True;
      Exit;
    end;
end;

procedure TViewTableSimpleDemoMainForm.FormShow(Sender: TObject);
begin
  cxgFilms.FocusedView.DataController.Groups.FullExpand;
  FocusRomanceCategory;
end;

procedure TViewTableSimpleDemoMainForm.LookAndFeelChanged(
  Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  inherited LookAndFeelChanged(Sender, AChangedValues);
  UpdateTableViewStyleSheet(cxgGenrasDBTableView);
end;

procedure TViewTableSimpleDemoMainForm.miAboutClick(Sender: TObject);
begin
  ShowAboutDemoForm;
end;

procedure TViewTableSimpleDemoMainForm.miShowIndicatorClick(Sender: TObject);
begin
  cxgFilmsDBTableView.OptionsView.Indicator := GetMenuItemChecked(Sender);
end;

procedure TViewTableSimpleDemoMainForm.miShowPreviewRowClick(Sender: TObject);
begin
  cxgFilmsDBTableViewTAGLINE.IsPreview := GetMenuItemChecked(Sender);
end;

procedure TViewTableSimpleDemoMainForm.miFocusCellOnTabClick(Sender: TObject);
begin
  cxgFilmsDBTableView.OptionsBehavior.FocusCellOnTab := GetMenuItemChecked(Sender);
end;

procedure TViewTableSimpleDemoMainForm.miIncSearchClick(Sender: TObject);
begin
  cxgFilmsDBTableView.OptionsBehavior.IncSearch := GetMenuItemChecked(Sender);
end;

procedure TViewTableSimpleDemoMainForm.miImmediateEditorClick(Sender: TObject);
begin
  cxgFilmsDBTableView.OptionsBehavior.ImmediateEditor := GetMenuItemChecked(Sender);
end;

procedure TViewTableSimpleDemoMainForm.miMultiSelectClick(Sender: TObject);
begin
  cxgFilmsDBTableView.OptionsSelection.MultiSelect := GetMenuItemChecked(Sender);
end;

procedure TViewTableSimpleDemoMainForm.miShowNavigatorClick(Sender: TObject);
begin
  cxgFilmsDBTableView.Navigator.Visible := GetMenuItemChecked(Sender);
end;

end.
