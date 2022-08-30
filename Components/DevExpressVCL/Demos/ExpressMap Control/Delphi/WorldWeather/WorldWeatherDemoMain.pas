unit WorldWeatherDemoMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BasicDemoMain, dxCoreClasses, dxCore, cxGraphics, cxControls, StrUtils,
  RTLConsts, Types, Generics.Collections,
  cxLookAndFeels, cxLookAndFeelPainters, dxGdiPlusClasses, dxMapControlTypes, dxMapItem,
  dxMapControlBingMapImageryDataProvider, Menus, dxCustomMapItemLayer,
  dxMapItemLayer, cxClasses, dxMapLayer, dxMapImageTileLayer, dxMapControl,
  cxContainer, cxEdit, cxCheckBox, StdCtrls, cxRadioGroup, cxTextEdit,
  cxMaskEdit, cxDropDownEdit, cxCheckComboBox, cxButtons, cxLabel, cxImage,
  cxGroupBox, cxCheckListBox, dxMapControlHttpRequest, dxXmlDoc,
  dxMapControlOpenStreetMapImageryDataProvider, dxScreenTip, dxCustomHint,
  cxHint, cxGeometry, dxRESTService, OpenWeatherMapService;

type
  TWorldWeatherDemoMainForm = class(TfrmBasicDemoMain)
    dxMapControl1ImageTileLayer1: TdxMapImageTileLayer;
    dxMapControl1ItemLayer1: TdxMapItemLayer;
    dxMapControl1ImageTileLayer2: TdxMapImageTileLayer;
    Addcity1: TMenuItem;
    N1: TMenuItem;
    Additionallayer1: TMenuItem;
    Settings1: TMenuItem;
    emperaturemeasure1: TMenuItem;
    Celsius1: TMenuItem;
    Fahrenheit1: TMenuItem;
    None1: TMenuItem;
    Clouds1: TMenuItem;
    emperature1: TMenuItem;
    Precipitation1: TMenuItem;
    Pressure1: TMenuItem;
    Wind1: TMenuItem;
    Snow1: TMenuItem;
    cxHintStyleController1: TcxHintStyleController;
    dxScreenTipRepository1: TdxScreenTipRepository;
    dxScreenTipRepository1ScreenTip1: TdxScreenTip;
    PopupMenu1: TPopupMenu;
    ShowWeatherforGeoPoint: TMenuItem;
    ListBox1: TListBox;
    Hidecity1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cxButton1Click(Sender: TObject);
    procedure cxRadioButton1Click(Sender: TObject);
    procedure Settings1Click(Sender: TObject);
    procedure None1Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure ShowWeatherforGeoPointClick(Sender: TObject);
    procedure Hidecity1Click(Sender: TObject);
  private
    FCelsius: Boolean;
    FWeatherInfos: TWeatherInfos;
    FHotCityName: string;
    FNewGeoPoint: TdxMapControlGeoPoint;
    function GetHottrackedMapItem: TdxMapCustomElement;
    procedure WeatherInfoAdded(ASender: TObject; AInfo: TWeatherInfo);
  end;

var
  WorldWeatherDemoMainForm: TWorldWeatherDemoMainForm;

implementation

{$R *.dfm}

uses
  WorldWeatherDemoAddCityDialog, WorldWeatherChangeVisibilityDialog;

procedure TWorldWeatherDemoMainForm.cxButton1Click(Sender: TObject);
var
  AWeatherInfo: TWeatherInfo;
begin
  if WorldWeatherDemoAddCityDialogForm.Execute then
    if not FWeatherInfos.IsInfoExists(WorldWeatherDemoAddCityDialogForm.CityName, AWeatherInfo) then
      if FWeatherInfos.GetInfo(WorldWeatherDemoAddCityDialogForm.CityName, AWeatherInfo) then
      begin
        FWeatherInfos.AddInfo(AWeatherInfo);
        AWeatherInfo.MapItem.Selected := True;
        dxMapControl1.CenterPoint := AWeatherInfo.MapItem.Location;
      end
      else
        ShowMessage(Format('Cannot find weather info for ''%s''', [WorldWeatherDemoAddCityDialogForm.CityName]))
    else
      AWeatherInfo.Visible := True;
end;

procedure TWorldWeatherDemoMainForm.cxRadioButton1Click(Sender: TObject);
begin
  FCelsius := (Sender as TComponent).Tag = 0;
  dxMapControl1ItemLayer1.MapItems.BeginUpdate;
  FWeatherInfos.UpdateItemText(FCelsius);
  dxMapControl1ItemLayer1.MapItems.EndUpdate;
end;

procedure TWorldWeatherDemoMainForm.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  inherited;
  FCelsius := True;
  FWeatherInfos := TWeatherInfos.Create;
  FWeatherInfos.OnItemAdded := WeatherInfoAdded;
  for I := 0 to ListBox1.Count - 1 do
    FWeatherInfos.LoadInfoAsync(ListBox1.Items[I]);
end;

procedure TWorldWeatherDemoMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FWeatherInfos);
  inherited;
end;

function TWorldWeatherDemoMainForm.GetHottrackedMapItem: TdxMapCustomElement;
begin
  Result := nil;
  if (dxMapControl1.HitTest.HitObject <> nil) and
    (dxMapControl1.HitTest.HitObject is TdxMapCustomElementViewInfo) then
    Result := TdxMapCustomElementViewInfo(dxMapControl1.HitTest.HitObject).Item as TdxMapCustomElement;
end;

procedure TWorldWeatherDemoMainForm.Hidecity1Click(Sender: TObject);
begin
  FWeatherInfos.HideInfo(FHotCityName);
end;

procedure TWorldWeatherDemoMainForm.WeatherInfoAdded(ASender: TObject;
  AInfo: TWeatherInfo);
var
  AItem: TdxMapCustomElement;
  AScreenTip: TdxScreenTip;
  AText: string;
begin
  dxMapControl1ItemLayer1.MapItems.BeginUpdate;
  try
    AItem := dxMapControl1ItemLayer1.AddItem(TdxMapCustomElement) as TdxMapCustomElement;
    AScreenTip := dxScreenTipRepository1.Items.Add;
    AItem.ScreenTip := AScreenTip;
    AInfo.MapItem := AItem;
    AItem.Location.GeoPoint := AInfo.GeoPoint;
    AInfo.UpdateItemText(FCelsius);
    with AScreenTip do
    begin
      Header.Text := AInfo.FullCityName;
      Description.GlyphFixedWidth := False;
      Description.PlainText := False;
      AText := dxScreenTipRepository1ScreenTip1.Description.Text;
      AText := StringReplace(AText, '%Pressure%', AInfo.Pressure, []);
      AText := StringReplace(AText, '%Wind%', AInfo.Wind, []);
      AText := StringReplace(AText, '%Humidity%', AInfo.Humidity, []);
      Description.Text := AText;
    end;
    AInfo.UpdateItemImage;
  finally
    dxMapControl1ItemLayer1.MapItems.EndUpdate;
  end;
end;

procedure TWorldWeatherDemoMainForm.None1Click(Sender: TObject);
var
  ATag: Integer;
begin
  ATag := (Sender as TComponent).Tag;
  dxMapControl1ImageTileLayer2.Visible := ATag > 0;
  if ATag > 0 then
  begin
    (dxMapControl1ImageTileLayer2.Provider as TdxMapControlOpenStreetMapImageryDataProvider).UrlTemplate :=
      Format(AdditionalLayerUri, [AdditionalLayerSubUri[ATag - 1], GetKey]);
    dxMapControl1ImageTileLayer2.Visible := True;
  end;
end;

procedure TWorldWeatherDemoMainForm.PopupMenu1Popup(Sender: TObject);
var
  AMapItem: TdxMapCustomElement;
begin
  inherited;
  ShowWeatherforGeoPoint.Visible := not dxMapControl1.HitTest.HitAtNavigationPanel;
  if ShowWeatherforGeoPoint.Visible then
  begin
    FNewGeoPoint := dxMapControl1ImageTileLayer1.ScreenPointToGeoPoint(dxPointDouble(dxMapControl1.HitTest.HitPoint));
    ShowWeatherforGeoPoint.Caption := Format('Show weather info for (%.6f, %.6f)',
      [FNewGeoPoint.Latitude, FNewGeoPoint.Longitude]);
  end;
  AMapItem := GetHottrackedMapItem;
  Hidecity1.Visible := AMapItem <> nil;
  if Hidecity1.Visible then
  begin
    FHotCityName := FWeatherInfos.GetInfoByMapItem(AMapItem).FullCityName;
    Hidecity1.Caption := Format('Hide weather info for %s', [FHotCityName]);
  end;
end;

procedure TWorldWeatherDemoMainForm.Settings1Click(Sender: TObject);
var
  I: Integer;
  AWeatherInfo: TWeatherInfo;
  ACheckItem: TcxCheckListBoxItem;
begin
  WorldWeatherChangeVisibilityDialogForm.cxCheckListBox1.Items.Clear;
  for I := 0 to FWeatherInfos.Items.Count - 1 do
  begin
    AWeatherInfo := FWeatherInfos.Items[I];
    ACheckItem := WorldWeatherChangeVisibilityDialogForm.cxCheckListBox1.Items.Add;
    ACheckItem.Text := AWeatherInfo.FullCityName;
    if AWeatherInfo.Visible then
      ACheckItem.State := cbsChecked
    else
      ACheckItem.State := cbsUnchecked;
  end;
  if WorldWeatherChangeVisibilityDialogForm.ShowModal = mrOk then
    for I := 0 to WorldWeatherChangeVisibilityDialogForm.cxCheckListBox1.Items.Count - 1 do
      if FWeatherInfos.IsInfoExists(WorldWeatherChangeVisibilityDialogForm.cxCheckListBox1.Items[I].Text, AWeatherInfo) then
        AWeatherInfo.Visible := (WorldWeatherChangeVisibilityDialogForm.cxCheckListBox1.Items[I].State = cbsChecked);
end;

procedure TWorldWeatherDemoMainForm.ShowWeatherforGeoPointClick(
  Sender: TObject);
var
  AWeatherInfo, AExistingWeatherInfo: TWeatherInfo;
begin
  if FWeatherInfos.GetInfo(FNewGeoPoint, AWeatherInfo) then
    if FWeatherInfos.IsInfoExists(AWeatherInfo.CityId, AExistingWeatherInfo) then
    begin
      AWeatherInfo.Free;
      AExistingWeatherInfo.Visible := True;
    end
    else
    begin
      FWeatherInfos.AddInfo(AWeatherInfo);
      dxMapControl1.Select(AWeatherInfo.MapItem);
    end;
end;

end.

