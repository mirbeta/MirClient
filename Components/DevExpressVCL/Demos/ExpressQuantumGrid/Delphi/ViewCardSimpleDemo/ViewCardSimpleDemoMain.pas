unit ViewCardSimpleDemoMain;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs, cxControls,
  cxLookupGrid, cxLookupDBGrid, cxGridCustomTableView, cxGridLevel, cxGrid,
  cxGridCustomView, cxGridCardView, cxGridDBCardView, cxMaskEdit, StdCtrls,
  ExtCtrls, cxContainer, cxEdit, cxTextEdit, cxDropDownEdit, cxDBEdit, cxStyles,
  ViewCardSimpleDemoData, Menus, ActnList, ImgList, ComCtrls, cxLookAndFeels,
  cxCustomData, cxGraphics, cxFilter, cxData, cxDBData, cxGridTableView,
  cxGridDBTableView, DB, cxDataStorage, cxHyperLinkEdit, cxLookAndFeelPainters,
  cxClasses, cxGridCustomLayoutView, BaseForm;

type
  TViewCardSimpleDemoMainForm = class(TfmBaseForm)
    cxGridDepartments: TcxGrid;
    cxGridDepartmentsDBTableView: TcxGridDBTableView;
    cxGridDepartmentsDBTableViewNAME: TcxGridDBColumn;
    cxGridDepartmentsLevel: TcxGridLevel;
    cxGridUsers: TcxGrid;
    cxGridUsersDBCardView: TcxGridDBCardView;
    cxGridUsersDBCardViewADDRESS: TcxGridDBCardViewRow;
    cxGridUsersDBCardViewCITY: TcxGridDBCardViewRow;
    cxGridUsersDBCardViewCOUNTRY: TcxGridDBCardViewRow;
    cxGridUsersDBCardViewEMAIL: TcxGridDBCardViewRow;
    cxGridUsersDBCardViewFAX: TcxGridDBCardViewRow;
    cxGridUsersDBCardViewFNAME: TcxGridDBCardViewRow;
    cxGridUsersDBCardViewHOMEPAGE: TcxGridDBCardViewRow;
    cxGridUsersDBCardViewLNAME: TcxGridDBCardViewRow;
    cxGridUsersDBCardViewMNAME: TcxGridDBCardViewRow;
    cxGridUsersDBCardViewPHONE: TcxGridDBCardViewRow;
    cxGridUsersDBCardViewPOSTALCODE: TcxGridDBCardViewRow;
    cxGridUsersLevel: TcxGridLevel;
    miCellSelect: TMenuItem;
    miHideFocusRect: TMenuItem;
    miInvertSelect: TMenuItem;
    miMultiSelect: TMenuItem;
    miOptions: TMenuItem;
    miShowNavigator: TMenuItem;
    pnDepartments: TPanel;
    pnDepartmentsCaption: TPanel;
    pnUsers: TPanel;
    pnUsersCaption: TPanel;
    Splitter: TSplitter;
    procedure miCellSelectClick(Sender: TObject);
    procedure miHideFocusRectClick(Sender: TObject);
    procedure miInvertSelectClick(Sender: TObject);
    procedure miMultiSelectClick(Sender: TObject);
    procedure miShowNavigatorClick(Sender: TObject);
  end;

var
  ViewCardSimpleDemoMainForm: TViewCardSimpleDemoMainForm;

implementation

{$R *.dfm}

uses
  AboutDemoForm;

procedure TViewCardSimpleDemoMainForm.miInvertSelectClick(Sender: TObject);
begin
  cxGridUsersDBCardView.OptionsSelection.InvertSelect := GetMenuItemChecked(Sender);
end;

procedure TViewCardSimpleDemoMainForm.miCellSelectClick(Sender: TObject);
begin
  cxGridUsersDBCardView.OptionsSelection.CellSelect := GetMenuItemChecked(Sender);
end;

procedure TViewCardSimpleDemoMainForm.miHideFocusRectClick(Sender: TObject);
begin
  cxGridUsersDBCardView.OptionsSelection.HideFocusRect := GetMenuItemChecked(Sender);
end;

procedure TViewCardSimpleDemoMainForm.miMultiSelectClick(Sender: TObject);
begin
  cxGridUsersDBCardView.OptionsSelection.MultiSelect := GetMenuItemChecked(Sender);
end;

procedure TViewCardSimpleDemoMainForm.miShowNavigatorClick(Sender: TObject);
begin
  cxGridUsersDBCardView.Navigator.Visible := GetMenuItemChecked(Sender);
end;

end.  
