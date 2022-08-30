unit DataProvidersDemoMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, BasicDemoMain, dxCore, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, dxMapControlTypes,
  dxMapControlBingMapImageryDataProvider, Menus, dxCustomMapItemLayer,
  dxMapItemLayer, cxClasses, dxMapLayer, dxMapImageTileLayer, dxMapControl,
  dxMapControlOpenStreetMapImageryDataProvider, dxMapItem;
type
  TDataProvidersDemoMainForm = class(TfrmBasicDemoMain)
    OpenStreetMapProvider1: TMenuItem;
    miRoad: TMenuItem;
    miArea: TMenuItem;
    miHybrid: TMenuItem;
    dxMapControl1ImageTileLayer1: TdxMapImageTileLayer;
    dxMapControl1ItemLayer1: TdxMapItemLayer;
    dxMapControl1ItemLayer1CustomItem1: TdxMapCustomElement;
    procedure MapKindClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FMapKind: TdxBingMapKind;
    FProviderId: Integer;
    procedure UpdateMapKind;
    procedure UpdateProvider;
  end;

var
  DataProvidersDemoMainForm: TDataProvidersDemoMainForm;

implementation

{$R *.dfm}

procedure TDataProvidersDemoMainForm.FormCreate(Sender: TObject);
begin
  inherited;
  dxMapControl1.CenterPoint := dxMapControl1ItemLayer1CustomItem1.Location;
  dxMapControl1.ZoomLevel := 10;
  dxMapControl1ItemLayer1CustomItem1.Selected := True;
  FProviderId := 1;
  FMapKind := bmkHybrid;
  UpdateProvider;
end;

procedure TDataProvidersDemoMainForm.MapKindClick(Sender: TObject);
begin
  if (Sender as TComponent).Tag > 0 then
  begin
    FProviderId := 1;
    FMapKind := TdxBingMapKind((Sender as TComponent).Tag - 1);
  end
  else
    FProviderId := 0;
  UpdateProvider;
end;

procedure TDataProvidersDemoMainForm.UpdateMapKind;
begin
  if dxMapControl1ImageTileLayer1.Provider is TdxMapControlBingMapImageryDataProvider then
    TdxMapControlBingMapImageryDataProvider(dxMapControl1ImageTileLayer1.Provider).Kind := FMapKind;
end;

procedure TDataProvidersDemoMainForm.UpdateProvider;
begin
  dxMapControl1.BeginUpdate;
  try
    if FProviderId = 1 then
    begin
      dxMapControl1ImageTileLayer1.ProviderClass := TdxMapControlBingMapImageryDataProvider;
      (dxMapControl1ImageTileLayer1.Provider as TdxMapControlBingMapImageryDataProvider).BingKey := DXBingKey;
      UpdateMapKind;
    end
    else
      dxMapControl1ImageTileLayer1.ProviderClass := TdxMapControlOpenStreetMapImageryDataProvider;
    dxMapControl1ImageTileLayer1.Provider.MaxParallelConnectionCount := 30;
  finally
    dxMapControl1.EndUpdate;
  end;
end;

end.
