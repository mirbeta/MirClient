unit CustomRowHeightDemoMain;

{$I cxVer.inc}

interface

uses
  Windows, Forms, Messages, SysUtils, Classes, ActnList, ImgList, Controls, Menus,
  StdCtrls, cxButtons, cxCheckBox, cxContainer, cxEdit, cxTextEdit,
  cxMaskEdit, cxSpinEdit, ExtCtrls, cxGridLevel, cxGridCustomTableView,
  cxGridCardView, cxGridDBCardView, cxClasses, cxControls,
  cxGridCustomView, cxGrid, ComCtrls, cxStyles, cxCustomData, cxGraphics,
  cxFilter, cxData, DB, cxDBData, cxDataStorage, cxLookAndFeelPainters,
  cxLookAndFeels, cxHyperLinkEdit, cxImageComboBox, cxDBLookupComboBox,
  cxMemo, cxImage, cxGridTableView, cxGridDBTableView, BaseForm;

type
  TCustomRowHeightDemoMainForm = class(TfmBaseForm)
    miOptions: TMenuItem;
    Grid: TcxGrid;
    tvFilms: TcxGridDBTableView;
    tvFilmsCAPTION: TcxGridDBColumn;
    tvFilmsPLOTOUTLINE: TcxGridDBColumn;
    tvFilmsPHOTO: TcxGridDBColumn;
    lvFilms: TcxGridLevel;
    miPictureZoom: TMenuItem;
    miZoom100perc: TMenuItem;
    miZoom75perc: TMenuItem;
    miZoom50perc: TMenuItem;
    miZoom25perc: TMenuItem;
    procedure miZoomClick(Sender: TObject);
    procedure tvFilmsGetCellHeight(Sender: TcxCustomGridTableView;
      ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem;
      ACellViewInfo: TcxGridTableDataCellViewInfo; var AHeight: Integer);
  private
    FCurrentZoom: Integer;
    function GetZoomByMenuItem(AMenuItemIndex: Integer): Integer;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  CustomRowHeightDemoMainForm: TCustomRowHeightDemoMainForm;

implementation

{$R *.dfm}

uses
  Variants, Dialogs, FilmsDemoData, AboutDemoForm, Graphics, cxVariants;

constructor TCustomRowHeightDemoMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCurrentZoom := 50;
  FilmsDemoDM.FilmsFiltered := False;
end;

procedure TCustomRowHeightDemoMainForm.miZoomClick(Sender: TObject);
begin
  MenuItemSetChecked(Sender, True);
  FCurrentZoom := GetZoomByMenuItem(TComponent(Sender).Tag);
  tvFilms.SizeChanged;
end;

procedure TCustomRowHeightDemoMainForm.tvFilmsGetCellHeight(
  Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord;
  AItem: TcxCustomGridTableItem;
  ACellViewInfo: TcxGridTableDataCellViewInfo; var AHeight: Integer);
var
  AEditValue: Variant;
  APicture: TPicture;
begin
  if AItem <> tvFilmsPHOTO then
  begin
    AHeight := 0;
    Exit;
  end;
  AEditValue := ARecord.Values[tvFilmsPHOTO.Index];
  if VarIsStr(AEditValue) or VarIsArray(AEditValue) then
  begin
    APicture := TPicture.Create;
    try
      LoadPicture(APicture,
        TcxImageProperties(tvFilmsPHOTO.Properties).GraphicClass, AEditValue);
      AHeight := APicture.Height;
      AHeight := AHeight * FCurrentZoom div 100;
    finally
      APicture.Free;
    end;
  end;
end;

function TCustomRowHeightDemoMainForm.GetZoomByMenuItem(AMenuItemIndex: Integer): Integer;
const
  Zoom: array[0..3] of Integer = (100, 75, 50, 25);
begin
  Result := Zoom[AMenuItemIndex];
end;

end.
