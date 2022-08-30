unit MasterDetailMultiDemoMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, cxControls, cxLookupGrid, cxLookupDBGrid, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxGridLevel, cxGridCustomView, cxGrid,
  cxGridCardView, cxGridDBCardView, cxMaskEdit, StdCtrls, ExtCtrls,
  cxContainer, cxEdit, cxTextEdit, cxDropDownEdit, cxDBEdit, cxStyles,
  Menus, ActnList, ImgList, ComCtrls, cxLookAndFeels, DB,
  cxCustomData, cxGraphics, cxFilter, cxData, cxDBData, cxClasses,
  cxGridBandedTableView, cxGridDBBandedTableView, cxDataStorage,
  cxDBLookupComboBox, cxBlobEdit, cxImage, cxMemo, cxHyperLinkEdit,
  cxLookAndFeelPainters, cxGridCustomLayoutView, BaseForm;

type
  TMasterDetailMultiDemoMainForm = class(TfmBaseForm)
    miView: TMenuItem;
    miGenreTabPosition: TMenuItem;
    Grid: TcxGrid;
    cvPeople: TcxGridDBCardView;
    cvPeopleName: TcxGridDBCardViewRow;
    cvPeoplePersonLineID: TcxGridDBCardViewRow;
    cvPeopleFIRSTNAME: TcxGridDBCardViewRow;
    cvPeopleSECONDNAME: TcxGridDBCardViewRow;
    cvPeopleNICKNAME: TcxGridDBCardViewRow;
    cvPeopleBIRTHNAME: TcxGridDBCardViewRow;
    cvPeopleDATEOFBIRTH: TcxGridDBCardViewRow;
    cvPeopleLOCATIONOFBIRTH: TcxGridDBCardViewRow;
    cvPeopleBIOGRAPHY: TcxGridDBCardViewRow;
    cvPeopleHOMEPAGE: TcxGridDBCardViewRow;
    cvPeopleID: TcxGridDBCardViewRow;
    cvPeopleFilmID: TcxGridDBCardViewRow;
    cvPeopleBIRTHCOUNTRY: TcxGridDBCardViewRow;
    cvPeopleGender: TcxGridDBCardViewRow;
    tvCompanies: TcxGridDBTableView;
    tvCompaniesName: TcxGridDBColumn;
    tvCompaniesType: TcxGridDBColumn;
    tvCompaniesCountry: TcxGridDBColumn;
    tvCompaniesWebSite: TcxGridDBColumn;
    tvCompaniesID: TcxGridDBColumn;
    tvCompaniesFILMID: TcxGridDBColumn;
    cvPhotos: TcxGridDBCardView;
    cvPhotosID: TcxGridDBCardViewRow;
    cvPhotosFILMID: TcxGridDBCardViewRow;
    cvPhotosSCREEN: TcxGridDBCardViewRow;
    cvPhotosICON: TcxGridDBCardViewRow;
    bvFilms: TcxGridDBBandedTableView;
    bvFilmsCAPTION: TcxGridDBBandedColumn;
    bvFilmsYEAR: TcxGridDBBandedColumn;
    bvFilmsPLOTOUTLINE: TcxGridDBBandedColumn;
    bvFilmsRUNTIME: TcxGridDBBandedColumn;
    bvFilmsPHOTO: TcxGridDBBandedColumn;
    bvFilmsWEBSITE: TcxGridDBBandedColumn;
    miTabPositionLeft: TMenuItem;
    miTabPositionTop: TMenuItem;
    miTabPositionNone: TMenuItem;
    miTabsForEmptyDetails: TMenuItem;
    miTabStyle: TMenuItem;
    miTabCaptionAlignment: TMenuItem;
    miTabCaptionAlignmentLeft: TMenuItem;
    miTabCaptionAlignmentRight: TMenuItem;
    miTabCaptionAlignmentCenter: TMenuItem;
    procedure GridActiveTabChanged(Sender: TcxCustomGrid; ALevel: TcxGridLevel);
    procedure miTabPositionClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure miTabsForEmptyDetailsClick(Sender: TObject);
    procedure miTabStyleClick(Sender: TObject);
    procedure miTabCaptionAlignmentClick(Sender: TObject);
  private
    procedure CreateLevels;
    procedure CreateTabStyleMenu;
    procedure UpdateMenu;
  public
    procedure AfterConstruction; override;
  end;

var
  MasterDetailMultiDemoMainForm: TMasterDetailMultiDemoMainForm;

implementation

{$R *.dfm}

uses
  cxPCPaintersFactory, FilmsDemoData, AboutDemoForm, cxPC;

procedure TMasterDetailMultiDemoMainForm.AfterConstruction;
begin
  inherited AfterConstruction;
  UpdateMenu;
end;

procedure TMasterDetailMultiDemoMainForm.CreateLevels;
begin
  with FilmsDemoDM do
  begin
    cdsGenres.First;
    while not cdsGenres.Eof do
    begin
      with Grid.Levels.Add do
      begin
        Caption := cdsGenresName.AsString;
        MaxDetailHeight := 250;
        Options.DetailTabsPosition := dtpTop;
        Options.TabsForEmptyDetails := False;
        Add.Caption := 'Companies';
        Add.Caption := 'People';
        Add.Caption := 'Photos';
        Tag := cdsGenresID.AsInteger;
      end;
      cdsGenres.Next;
    end;
  end;
end;

procedure TMasterDetailMultiDemoMainForm.CreateTabStyleMenu;

  procedure AddTabStyleMenuItem(AStyleID: TcxPCStyleID; const AStyleName: string);
  var
    AMenuItem: TMenuItem;
  begin
    AMenuItem := TMenuItem.Create(Self);
    AMenuItem.Tag := AStyleID;
    AMenuItem.Caption := AStyleName;
    AMenuItem.GroupIndex := 1;
    AMenuItem.RadioItem := True;
    AMenuItem.OnClick := miTabStyleClick;
    miTabStyle.Add(AMenuItem);
  end;

var
  I: Integer;
  APainterClass: TcxPCPainterClass;
begin
  AddTabStyleMenuItem(cxPCDefaultStyle, cxPCDefaultStyleName);
  for I := 0 to PaintersFactory.PainterClassCount - 1 do
  begin
    APainterClass := PaintersFactory.PainterClasses[I];
    AddTabStyleMenuItem(APainterClass.GetStyleID, APainterClass.GetStyleName);
  end;
end;

procedure TMasterDetailMultiDemoMainForm.UpdateMenu;
begin
  MenuItemSetChecked('miTabsForEmptyDetails', Grid.Levels[0].Options.TabsForEmptyDetails);
  MenuItemCheckSubItemWithTag('miGenreTabPosition', Ord(Grid.RootLevelOptions.DetailTabsPosition));
  MenuItemCheckSubItemWithTag('miTabCaptionAlignment', Ord(Grid.LevelTabs.CaptionAlignment));
  MenuItemCheckSubItemWithTag('miTabStyle', Grid.LevelTabs.Style);
end;

procedure TMasterDetailMultiDemoMainForm.FormCreate(Sender: TObject);
begin
  CreateLevels;
  GridActiveTabChanged(Grid, Grid.Levels[0]);
  CreateTabStyleMenu;
end;

procedure TMasterDetailMultiDemoMainForm.GridActiveTabChanged(
  Sender: TcxCustomGrid; ALevel: TcxGridLevel);
begin
  if not ALevel.IsMaster then Exit;
  Grid.BeginUpdate;
  try
    FilmsDemoDM.cdsGenres.Locate('ID', ALevel.Tag, []);
    ALevel.GridView := bvFilms;
    ALevel.Items[0].GridView := tvCompanies;
    ALevel.Items[1].GridView := cvPeople;
    ALevel.Items[2].GridView := cvPhotos;
  finally
    Grid.EndUpdate;
  end;
end;

procedure TMasterDetailMultiDemoMainForm.miTabCaptionAlignmentClick(Sender: TObject);
begin
  Grid.LevelTabs.CaptionAlignment := TAlignment(TComponent(Sender).Tag);
  UpdateMenu;
end;

procedure TMasterDetailMultiDemoMainForm.miTabPositionClick(Sender: TObject);
begin
  Grid.RootLevelOptions.DetailTabsPosition := TcxGridDetailTabsPosition(TComponent(Sender).Tag);
  UpdateMenu;
end;

procedure TMasterDetailMultiDemoMainForm.miTabsForEmptyDetailsClick(Sender: TObject);
var
  I: Integer;
begin
  Grid.BeginUpdate;
  try
    for I := 0 to Grid.Levels.Count - 1 do
      with Grid.Levels[I].Options do
        TabsForEmptyDetails := not TabsForEmptyDetails;
  finally
    Grid.EndUpdate;
    UpdateMenu;
  end;
end;

procedure TMasterDetailMultiDemoMainForm.miTabStyleClick(Sender: TObject);
begin
  Grid.LevelTabs.Style := TcxPCStyleID(TComponent(Sender).Tag);
  UpdateMenu;
end;

end.
