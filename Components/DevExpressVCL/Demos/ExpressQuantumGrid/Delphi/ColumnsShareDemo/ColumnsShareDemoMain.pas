unit ColumnsShareDemoMain;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, cxControls, cxLookupGrid, cxLookupDBGrid, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxGridLevel, cxGridCustomView, cxGrid,
  cxGridCardView, cxGridDBCardView, cxMaskEdit, StdCtrls, ExtCtrls,
  cxContainer, cxEdit, cxTextEdit, cxDropDownEdit, cxDBEdit, cxStyles,
  Menus, ActnList, ImgList, ComCtrls, cxLookAndFeels, DB,
  cxCustomData, cxGraphics, cxFilter, cxData, cxDBData, cxClasses,
  cxDBEditRepository, cxGridCustomPopupMenu, cxGridPopupMenu,
  cxDBExtLookupComboBox, cxDataStorage, cxImageComboBox,
  cxDBLookupComboBox, cxHyperLinkEdit, cxLookAndFeelPainters,
  cxGridCustomLayoutView, BaseForm;

type
  TColumnsShareDemoMainForm = class(TfmBaseForm)
    miOptions: TMenuItem;
    Grid: TcxGrid;
    tvProjects: TcxGridDBTableView;
    tvProjectsNAME: TcxGridDBColumn;
    tvProjectsMANAGERID: TcxGridDBColumn;
    tvItems: TcxGridDBTableView;
    tvItemsNAME: TcxGridDBColumn;
    tvItemsTYPE: TcxGridDBColumn;
    tvItemsPROJECTID: TcxGridDBColumn;
    tvItemsPRIORITY: TcxGridDBColumn;
    tvItemsSTATUS: TcxGridDBColumn;
    tvItemsCREATORID: TcxGridDBColumn;
    tvItemsCREATEDDATE: TcxGridDBColumn;
    tvItemsOWNERID: TcxGridDBColumn;
    tvItemsLASTMODIFIEDDATE: TcxGridDBColumn;
    tvItemsFIXEDDATE: TcxGridDBColumn;
    tvItemsDESCRIPTION: TcxGridDBColumn;
    cvUsers: TcxGridDBCardView;
    cvUsersFNAME: TcxGridDBCardViewRow;
    cvUsersMNAME: TcxGridDBCardViewRow;
    cvUsersLNAME: TcxGridDBCardViewRow;
    cvUsersCOUNTRY: TcxGridDBCardViewRow;
    cvUsersPOSTALCODE: TcxGridDBCardViewRow;
    cvUsersCITY: TcxGridDBCardViewRow;
    cvUsersADDRESS: TcxGridDBCardViewRow;
    cvUsersPHONE: TcxGridDBCardViewRow;
    cvUsersFAX: TcxGridDBCardViewRow;
    cvUsersEMAIL: TcxGridDBCardViewRow;
    cvUsersHOMEPAGE: TcxGridDBCardViewRow;
    tvTeam: TcxGridDBTableView;
    tvTeamUSERID: TcxGridDBColumn;
    tvTeamFUNCTION: TcxGridDBColumn;
    lvItems: TcxGridLevel;
    lvProjects: TcxGridLevel;
    lvUsers: TcxGridLevel;
    cxEditRepository: TcxEditRepository;
    eriLookupComboUsers: TcxEditRepositoryLookupComboBoxItem;
    lvTeam: TcxGridLevel;
    cxGridPopupMenu1: TcxGridPopupMenu;
    miCustomizePersonsLookupCombobox: TMenuItem;
    cvUsersDepartment: TcxGridDBCardViewRow;
    cxGridViewRepository: TcxGridViewRepository;
    cxGridViewRepositoryDBTableView: TcxGridDBTableView;
    cxGridViewRepositoryDBTableViewID: TcxGridDBColumn;
    cxGridViewRepositoryDBTableViewUserName: TcxGridDBColumn;
    cxGridViewRepositoryDBTableViewCOUNTRY: TcxGridDBColumn;
    cxGridViewRepositoryDBTableViewPOSTALCODE: TcxGridDBColumn;
    cxGridViewRepositoryDBTableViewCITY: TcxGridDBColumn;
    cxGridViewRepositoryDBTableViewADDRESS: TcxGridDBColumn;
    cxGridViewRepositoryDBTableViewPHONE: TcxGridDBColumn;
    cxGridViewRepositoryDBTableViewFAX: TcxGridDBColumn;
    cxGridViewRepositoryDBTableViewEMAIL: TcxGridDBColumn;
    cxGridViewRepositoryDBTableViewHOMEPAGE: TcxGridDBColumn;
    cxGridViewRepositoryDBTableViewDepartment: TcxGridDBColumn;
    erExtLookupComboBoxItem: TcxEditRepositoryExtLookupComboBoxItem;
    miPersonEditor: TMenuItem;
    miExtLookUpEditor: TMenuItem;
    miLookupEditor: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure miCustomizePersonsLookupComboboxClick(Sender: TObject);
    procedure miLookUpEditorClick(Sender: TObject);
    procedure miExtLookUpEditorClick(Sender: TObject);
  protected
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
  end;

var
  ColumnsShareDemoMainForm: TColumnsShareDemoMainForm;

implementation

uses
  ColumnsShareDemoData, ColumnsShareDemoLookupCustomize, AboutDemoForm;

{$R *.dfm}

procedure TColumnsShareDemoMainForm.FormShow(Sender: TObject);
begin
  miCustomizePersonsLookupComboboxClick(nil);
end;

procedure TColumnsShareDemoMainForm.LookAndFeelChanged(
  Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  inherited LookAndFeelChanged(Sender, AChangedValues);
  UpdateTableViewStyleSheet(tvProjects);
  UpdateTableViewStyleSheet(tvItems);
  UpdateTableViewStyleSheet(tvTeam);
  UpdateCardViewStyleSheet(cvUsers);
end;

procedure TColumnsShareDemoMainForm.miLookUpEditorClick(Sender: TObject);
begin
  if GetMenuItemChecked(Sender) then
  begin
    tvItemsCREATORID.RepositoryItem := cxEditRepository[1];
    tvItemsOWNERID.RepositoryItem := cxEditRepository[1];
    tvProjectsMANAGERID.RepositoryItem := cxEditRepository[1];
    tvTeamUSERID.RepositoryItem := cxEditRepository[1];
    tvProjects.DataController.ClearDetails;
    MenuItemSetEnabled('miCustomizePersonsLookupCombobox', True);
  end;
end;

procedure TColumnsShareDemoMainForm.miExtLookUpEditorClick(Sender: TObject);
begin
  if GetMenuItemChecked(Sender) then
  begin
    tvItemsCREATORID.RepositoryItem := cxEditRepository[0];
    tvItemsOWNERID.RepositoryItem := cxEditRepository[0];
    tvProjectsMANAGERID.RepositoryItem := cxEditRepository[0];
    tvTeamUSERID.RepositoryItem := cxEditRepository[0];
    tvProjects.DataController.ClearDetails;

    if (ColumnsShareDemoLookupCustomizeForm <> nil) and
      ColumnsShareDemoLookupCustomizeForm.Showing
    then
      ColumnsShareDemoLookupCustomizeForm.Hide;
    MenuItemSetEnabled('miCustomizePersonsLookupCombobox', False);
  end;
end;

procedure TColumnsShareDemoMainForm.miCustomizePersonsLookupComboboxClick(Sender: TObject);
begin
  if not Assigned(ColumnsShareDemoLookupCustomizeForm) then
    ColumnsShareDemoLookupCustomizeForm := TColumnsShareDemoLookupCustomizeForm.Create(Self);
  ColumnsShareDemoLookupCustomizeForm.Show;
end;

end.
