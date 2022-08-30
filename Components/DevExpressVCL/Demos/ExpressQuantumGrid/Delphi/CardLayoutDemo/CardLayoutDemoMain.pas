unit CardLayoutDemoMain;

interface

{$I cxVer.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, cxStyles, cxCustomData, cxGraphics, cxFilter, cxData, cxDataStorage,
  cxEdit, DB, cxDBData, cxControls, cxGridCustomView, cxGridCustomTableView,
  cxGridCardView, cxGridDBCardView, cxClasses, cxGridLevel, cxGrid, StdCtrls,
  Menus, cxMemo, cxImage, cxCurrencyEdit, cxHyperLinkEdit, cxTextEdit,
  cxEditRepositoryItems, BaseForm, cxLookAndFeels, cxLookAndFeelPainters,
  cxGridTableView, cxGridCustomLayoutView, ComCtrls, DBClient, CarsDataForGrid, cxNavigator;

type
  TfrmMain = class(TfmBaseForm)
    GridLevel1: TcxGridLevel;
    Grid: TcxGrid;
    cvHorizontal: TcxGridDBCardView;
    GridLevel2: TcxGridLevel;
    cvVertical: TcxGridDBCardView;
    miView: TMenuItem;
    cvHorizontalTrademark: TcxGridDBCardViewRow;
    cvHorizontalModel: TcxGridDBCardViewRow;
    cvHorizontalHP: TcxGridDBCardViewRow;
    cvHorizontalTorque: TcxGridDBCardViewRow;
    cvHorizontalCyl: TcxGridDBCardViewRow;
    cvHorizontalTransmissSpeedCount: TcxGridDBCardViewRow;
    cvHorizontalTransmissAutomatic: TcxGridDBCardViewRow;
    cvHorizontalMPG_City: TcxGridDBCardViewRow;
    cvHorizontalMPG_Highway: TcxGridDBCardViewRow;
    cvHorizontalCategory: TcxGridDBCardViewRow;
    cvHorizontalDescription: TcxGridDBCardViewRow;
    cvHorizontalHyperlink: TcxGridDBCardViewRow;
    cvHorizontalPicture: TcxGridDBCardViewRow;
    cvHorizontalPrice: TcxGridDBCardViewRow;
    cvHorizontalRow1: TcxGridDBCardViewRow;
    styleSelection: TcxStyle;
    styleCardHeader: TcxStyle;
    styleCardBorder: TcxStyle;
    styleBackground: TcxStyle;
    styleCategoryRow: TcxStyle;
    cvHorizontalRow2: TcxGridDBCardViewRow;
    stylePrice: TcxStyle;
    cvHorizontalRow3: TcxGridDBCardViewRow;
    cvHorizontalRow4: TcxGridDBCardViewRow;
    cvVerticalTrademark: TcxGridDBCardViewRow;
    cvVerticalModel: TcxGridDBCardViewRow;
    cvVerticalHP: TcxGridDBCardViewRow;
    cvVerticalLiter: TcxGridDBCardViewRow;
    cvVerticalCyl: TcxGridDBCardViewRow;
    cvVerticalTransmissSpeedCount: TcxGridDBCardViewRow;
    cvVerticalTransmissAutomatic: TcxGridDBCardViewRow;
    cvVerticalMPG_City: TcxGridDBCardViewRow;
    cvVerticalMPG_Highway: TcxGridDBCardViewRow;
    cvVerticalCategory: TcxGridDBCardViewRow;
    cvVerticalDescription: TcxGridDBCardViewRow;
    cvVerticalHyperlink: TcxGridDBCardViewRow;
    cvVerticalPicture: TcxGridDBCardViewRow;
    cvVerticalPrice: TcxGridDBCardViewRow;
    cvVerticalFuelEconomy: TcxGridDBCardViewRow;
    CardsStyleSheet: TcxGridCardViewStyleSheet;
    EditRepository: TcxEditRepository;
    EditRepositoryImage: TcxEditRepositoryImageItem;
    EditRepositoryMemo: TcxEditRepositoryMemoItem;
    EditRepositoryHyperLink: TcxEditRepositoryHyperLinkItem;
    EditRepositoryPrice: TcxEditRepositoryCurrencyItem;
    cvVerticalRow1: TcxGridDBCardViewRow;
    cvVerticalRow2: TcxGridDBCardViewRow;
    EditRepositoryFuelEconomy: TcxEditRepositoryTextItem;
    cvVerticalRow3: TcxGridDBCardViewRow;
    EditRepositoryAutomatic: TcxEditRepositoryCheckBoxItem;
    miCardAutoWidth: TMenuItem;
    miCustomize: TMenuItem;
    miCellSelection: TMenuItem;
    N1: TMenuItem;
    procedure miCardAutoWidthClick(Sender: TObject);
    procedure miCellSelectionClick(Sender: TObject);
    procedure miCustomizeClick(Sender: TObject);
    procedure GridActiveTabChanged(Sender: TcxCustomGrid; ALevel: TcxGridLevel);
  private
    { Private declarations }
  protected
    procedure UpdateMenuValues;
  public
    procedure AfterConstruction; override;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  AboutDemoForm;

procedure TfrmMain.UpdateMenuValues;
begin
  with Grid.ActiveView as TcxGridCardView do
  begin
    MenuItemSetChecked('miCardAutoWidth', OptionsView.CardAutoWidth);
    MenuItemSetChecked('miCellSelection', OptionsSelection.CellSelect);
  end;
end;

procedure TfrmMain.miCardAutoWidthClick(Sender: TObject);
begin
  with (Grid.ActiveView as TcxGridCardView).OptionsView do
    CardAutoWidth := not CardAutoWidth;
  UpdateMenuValues;
end;

procedure TfrmMain.miCellSelectionClick(Sender: TObject);
begin
  with (Grid.ActiveView as TcxGridCardView).OptionsSelection do
    CellSelect := not CellSelect;
  UpdateMenuValues;
end;

procedure TfrmMain.miCustomizeClick(Sender: TObject);
begin
  (Grid.ActiveView as TcxGridCardView).Controller.Customization := True;
end;

procedure TfrmMain.AfterConstruction;
begin
  inherited AfterConstruction;
  UpdateMenuValues;
end;

procedure TfrmMain.GridActiveTabChanged(Sender: TcxCustomGrid; ALevel: TcxGridLevel);
begin
  UpdateMenuValues;
end;

end.
