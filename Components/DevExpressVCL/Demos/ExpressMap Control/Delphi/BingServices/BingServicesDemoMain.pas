unit BingServicesDemoMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, BasicDemoMain, cxGraphics, cxControls, Types, Math,
  cxLookAndFeels, cxLookAndFeelPainters, dxMapControlTypes, Menus, Generics.Collections,
  cxClasses, dxMapControl, dxMapControlBingMapImageryDataProvider, dxMapLayer,
  dxMapImageTileLayer, ActnList, dxCustomMapItemLayer, StrUtils,
  dxMapItemLayer, dxMapItem, cxGeometry, dxBingMapRouteDataService, dxRestService,
  dxBingMapRESTService, dxBingMapLocationDataService, dxBingMapRESTServiceStrs,
  ExtCtrls, dxScreenTip, dxCustomHint, cxHint, cxContainer, cxEdit,
  cxTextEdit, cxMaskEdit, cxDropDownEdit, StdCtrls, cxGroupBox, cxListBox, dxCore,
  cxButtons, dxMapControlBingMapInformationProviders, dxMapControlInformationProvider,
  dxCoreGraphics, cxSplitter, cxCheckListBox, dxGDIPlusClasses, cxImage;

type
  TBingServicesDemoMainForm = class(TfrmBasicDemoMain)
    PopupMenu1: TPopupMenu;
    ActionList1: TActionList;
    actAddStartPoint: TAction;
    actAddEndPoint: TAction;
    actDeletePoint: TAction;
    Setstartpoint1: TMenuItem;
    Addroutepoint1: TMenuItem;
    Deletepoint1: TMenuItem;
    actChangeStartPoint: TAction;
    Changestartpoint1: TMenuItem;
    actClear: TAction;
    dxScreenTipRepository1: TdxScreenTipRepository;
    cxHintStyleController1: TcxHintStyleController;
    dxScreenTipRepository1ScreenTip1: TdxScreenTip;
    cxTextEdit1: TcxTextEdit;
    actSetAsStartPoint: TAction;
    actSetAsEndPoint: TAction;
    Setasstartpoint1: TMenuItem;
    Setasroutepoint1: TMenuItem;
    cxGroupBox1: TcxGroupBox;
    dxMapControl1ImageTileLayer1: TdxMapImageTileLayer;
    dxMapControl1ItemLayer1: TdxMapItemLayer;
    cxListBox1: TcxListBox;
    miManeuverPoint: TdxMapDot;
    cxButton1: TcxButton;
    cxButton2: TcxButton;
    dxMapControl1BingMapGeoCodingDataProvider1: TdxMapControlBingMapGeoCodingDataProvider;
    dxMapControl1BingMapReverseGeoCodingDataProvider1: TdxMapControlBingMapReverseGeoCodingDataProvider;
    dxMapControl1BingMapRouteDataProvider1: TdxMapControlBingMapRouteDataProvider;
    actRouteFromMajorRoads: TAction;
    Showroutefrommajorroads1: TMenuItem;
    dxMapControl1BingMapMajorRoadRouteDataProvider1: TdxMapControlBingMapMajorRoadRouteDataProvider;
    miNewPointPointer: TdxMapDot;
    cxSplitter1: TcxSplitter;
    cxComboBox1: TcxComboBox;
    cxImage1: TcxImage;
    procedure actAddStartPointExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actAddEndPointExecute(Sender: TObject);
    procedure actDeletePointExecute(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure actChangeStartPointExecute(Sender: TObject);
    procedure actClearExecute(Sender: TObject);
    procedure cxTextEdit1PropertiesChange(Sender: TObject);
    procedure actSetAsStartPointExecute(Sender: TObject);
    procedure actSetAsEndPointExecute(Sender: TObject);
    procedure cxListBox1Click(Sender: TObject);
    procedure cxButton1Click(Sender: TObject);
    procedure dxMapControl1BingMapGeoCodingDataProvider1Response(
      Sender: TObject; AResponse: TdxBingMapLocationDataServiceResponse;
      var ADestroyResponse: Boolean);
    procedure dxMapControl1BingMapRouteDataProvider1Response(Sender: TObject;
      AResponse: TdxBingMapRouteDataServiceResponse;
      var ADestroyResponse: Boolean);
    procedure dxMapControl1BingMapReverseGeoCodingDataProvider1Response(
      Sender: TObject; AResponse: TdxBingMapLocationDataServiceResponse;
      var ADestroyResponse: Boolean);
    procedure actRouteFromMajorRoadsExecute(Sender: TObject);
    procedure dxMapControl1BingMapMajorRoadRouteDataProvider1Response(
      Sender: TObject; AResponse: TdxBingMapRouteDataServiceResponse;
      var ADestroyResponse: Boolean);
    procedure dxMapControl1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure cxComboBox1PropertiesEditValueChanged(Sender: TObject);
    procedure dxMapControl1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FAddedItemAddress: string;
    FCurrentCursorPos: TPoint;
    FHotPushpin: TdxMapPushpin;
    FRouteDataServiceResponse: TdxBingMapRouteDataServiceResponse;
    FRouteFromMajorRoadsDataServiceResponse: TdxBingMapRouteDataServiceResponse;
    FIsMajorRoadMode: Boolean;
    FRoutePins: TList<TdxMapPushpin>;
    FMajorRoadPins: TList<TdxMapPushpin>;
    FRoutes: TList<TdxMapPolyline>;
    FSearchText: string;
    FSearchPins: TList<TdxMapPushpin>;
    FSearchEditBackgroundColor: TdxAlphaColor;
    FWndProcLinkedObj: TcxWindowProcLinkedObject;
    function CreatePushpin: TdxMapPushpin;
    function GetCurrentCursorGeoPoint: TdxMapControlGeoPoint;
    function GetPinLetter(ANumber: Integer): string;
    function GetTravelDistanceStr(ADistanceInKilometers: Double): string;
    function GetTravelDurationStr(ATimeInSeconds: Double): string;
    procedure CalculateRoute;
    procedure CalculateRouteFromMajorRoads;
    procedure ClearMapItems<T:TdxMapItem>(AItems: TList<T>);
    function CheckResponse(AResponse: TdxBingMapResponse): Boolean;
    procedure CheckWaypointTexts(AItems: TList<TdxMapPushpin>);
    procedure ClearRoutePath;
    procedure ShowFullRoute(const ABoundingBox: TdxMapControlGeoRect);
    procedure TravelModeChanged(ASender: TObject);
    procedure CreateRouteLine(ARoute: TdxBingMapRouteInfo);
    procedure ClearAllPins;
    procedure ClearAllRouteInfo;
    procedure StopRouteProviders;
    procedure MapControlWndProc(var Message: TMessage);
  public
    property CurrentCursorGeoPoint: TdxMapControlGeoPoint read GetCurrentCursorGeoPoint;
  end;

var
  BingServicesDemoMainForm: TBingServicesDemoMainForm;

implementation

{$R *.dfm}

procedure TBingServicesDemoMainForm.actChangeStartPointExecute(Sender: TObject);
begin
  StopRouteProviders;
  FRoutePins[0].Location.GeoPoint := CurrentCursorGeoPoint;
  dxMapControl1BingMapReverseGeoCodingDataProvider1.Search(FRoutePins[0].Location.GeoPoint);
  FRoutePins[0].Hint := FAddedItemAddress;
  CalculateRoute;
end;

procedure TBingServicesDemoMainForm.actClearExecute(Sender: TObject);
begin
  ClearAllRouteInfo;
end;

procedure TBingServicesDemoMainForm.actDeletePointExecute(Sender: TObject);
begin
  if FHotPushpin <> nil then
  begin
    StopRouteProviders;
    FMajorRoadPins.Remove(FHotPushpin);
    FRoutePins.Remove(FHotPushpin);
    FSearchPins.Remove(FHotPushpin);
    dxMapControl1ItemLayer1.MapItems.Remove(FHotPushpin);
    CheckWaypointTexts(FRoutePins);
    CalculateRoute;
  end;
end;

procedure TBingServicesDemoMainForm.actRouteFromMajorRoadsExecute(
  Sender: TObject);
var
  APushpin: TdxMapPushpin;
begin
  StopRouteProviders;
  ClearAllRouteInfo;
  APushpin := CreatePushpin;
  APushpin.Location.GeoPoint := CurrentCursorGeoPoint;
  FMajorRoadPins.Add(APushpin);
  dxMapControl1BingMapReverseGeoCodingDataProvider1.Search(APushpin.Location.GeoPoint);
  APushpin.Hint := FAddedItemAddress;
  CheckWaypointTexts(FMajorRoadPins);
  CalculateRouteFromMajorRoads;
end;

procedure TBingServicesDemoMainForm.actAddEndPointExecute(Sender: TObject);
var
  APushpin: TdxMapPushpin;
begin
  StopRouteProviders;
  APushpin := CreatePushpin;
  APushpin.Location.GeoPoint := CurrentCursorGeoPoint;
  FRoutePins.Add(APushpin);
  CheckWaypointTexts(FRoutePins);
  dxMapControl1BingMapReverseGeoCodingDataProvider1.Search(APushpin.Location.GeoPoint);
  APushpin.Hint := FAddedItemAddress;
  CalculateRoute;
end;

procedure TBingServicesDemoMainForm.actSetAsEndPointExecute(Sender: TObject);
begin
  if FHotPushpin <> nil then
  begin
    StopRouteProviders;
    FSearchPins.Remove(FHotPushpin);
    FRoutePins.Remove(FHotPushpin);
    FRoutePins.Add(FHotPushpin);
    CheckWaypointTexts(FRoutePins);
    CalculateRoute;
  end;
end;

procedure TBingServicesDemoMainForm.actSetAsStartPointExecute(Sender: TObject);
begin
  if FHotPushpin <> nil then
  begin
    StopRouteProviders;
    FSearchPins.Remove(FHotPushpin);
    FMajorRoadPins.Remove(FHotPushpin);
    FRoutePins.Remove(FHotPushpin);
    FRoutePins.Insert(0, FHotPushpin);
    ClearMapItems<TdxMapPushpin>(FMajorRoadPins);
    CheckWaypointTexts(FRoutePins);
    CalculateRoute;
  end;
end;

procedure TBingServicesDemoMainForm.actAddStartPointExecute(Sender: TObject);
var
  APushpin: TdxMapPushpin;
begin
  ClearAllRouteInfo;
  APushpin := CreatePushpin;
  APushpin.Location.GeoPoint := CurrentCursorGeoPoint;
  FRoutePins.Insert(0, APushpin);
  CheckWaypointTexts(FRoutePins);
  dxMapControl1BingMapReverseGeoCodingDataProvider1.Search(APushpin.Location.GeoPoint);
  APushpin.Hint := FAddedItemAddress;
end;

function TBingServicesDemoMainForm.CreatePushpin: TdxMapPushpin;
begin
  Result := dxMapControl1ItemLayer1.MapItems.Add(TdxMapPushpin) as TdxMapPushpin;
end;

procedure TBingServicesDemoMainForm.cxButton1Click(Sender: TObject);

  function MapRectUnion(const R1, R2: TdxMapControlGeoRect): TdxMapControlGeoRect;
  begin
    Result := R1;
    if (R2.Right - R2.Left <= 0) or (R2.Top - R2.Bottom <= 0) then Exit;
    if R2.Left < R1.Left then
      Result.Left := R2.Left;
    if R2.Top > R1.Top then
      Result.Top := R2.Top;
    if R2.Right > R1.Right then
      Result.Right := R2.Right;
    if R2.Bottom < R1.Bottom then
      Result.Bottom := R2.Bottom;
  end;

var
  I: Integer;
  ABoundingBox: TdxMapControlGeoRect;
begin
  cxListBox1.ItemIndex := -1;
  if (FRouteDataServiceResponse <> nil) and (FRouteDataServiceResponse.Routes.Count > 0) then
    ShowFullRoute(FRouteDataServiceResponse.Routes[0].BoundingBox)
  else
    if (FRouteFromMajorRoadsDataServiceResponse <> nil) and
      (FRouteFromMajorRoadsDataServiceResponse.Routes.Count > 0) then
    begin
      ABoundingBox := FRouteFromMajorRoadsDataServiceResponse.Routes[0].BoundingBox;
      for I := 1 to FRouteFromMajorRoadsDataServiceResponse.Routes.Count - 1 do
        ABoundingBox := MapRectUnion(ABoundingBox,
          FRouteFromMajorRoadsDataServiceResponse.Routes[I].BoundingBox);
      ShowFullRoute(ABoundingBox);
    end;
end;

procedure TBingServicesDemoMainForm.cxComboBox1PropertiesEditValueChanged(
  Sender: TObject);
var
  I, J: Integer;
  ARoute: TdxBingMapRouteInfo;
  AItineraryItem: TdxBingMapItineraryItem;
begin
  if cxComboBox1.ItemIndex = -1 then
    Exit;
  ARoute := cxComboBox1.ItemObject as TdxBingMapRouteInfo;
  cxListBox1.Clear;
  if ARoute <> nil then
  begin
    for I := 0 to ARoute.RouteLegs.Count - 1 do
    begin
      for J := 0 to ARoute.RouteLegs[I].ItineraryItems.Count - 1 do
      begin
        AItineraryItem := ARoute.RouteLegs[I].ItineraryItems[J];
        cxListBox1.AddItem(AItineraryItem.Instruction.Description, AItineraryItem);
      end;
    end;
    dxMapControl1ItemLayer1.MapItems.BeginUpdate;
    for I := 0 to FRoutes.Count - 1 do
      FRoutes[I].Visible := FRoutes[I].Tag = TdxNativeInt(ARoute);
    dxMapControl1ItemLayer1.MapItems.EndUpdate;
  end;
end;

procedure TBingServicesDemoMainForm.cxListBox1Click(Sender: TObject);
var
  AItineraryItem: TdxBingMapItineraryItem;
begin
  if cxListBox1.ItemIndex = -1 then
    Exit;

  AItineraryItem := cxListBox1.Items.Objects[cxListBox1.ItemIndex] as TdxBingMapItineraryItem;
  miManeuverPoint.Location.GeoPoint := AItineraryItem.ManeuverPoint;
  dxMapControl1.BeginUpdate;
  dxMapControl1.CenterPoint := miManeuverPoint.Location;
  dxMapControl1.ZoomLevel := 17;
  dxMapControl1.EndUpdate;
  miManeuverPoint.Visible := True;
end;

procedure TBingServicesDemoMainForm.cxTextEdit1PropertiesChange(
  Sender: TObject);
begin
  if FSearchText <> cxTextEdit1.EditingText then
  begin
    FSearchText := cxTextEdit1.EditingText;
    dxMapControl1BingMapGeoCodingDataProvider1.CancelRequests;
    ClearMapItems<TdxMapPushpin>(FSearchPins);
    if FSearchText <> '' then
      dxMapControl1BingMapGeoCodingDataProvider1.SearchAsync(FSearchText);
  end;
end;

procedure TBingServicesDemoMainForm.dxMapControl1BingMapGeoCodingDataProvider1Response(
  Sender: TObject; AResponse: TdxBingMapLocationDataServiceResponse;
  var ADestroyResponse: Boolean);
var
  APushpin: TdxMapPushpin;
  ALocationInfo: TdxBingMapLocationInfo;
begin
  if CheckResponse(AResponse) and (AResponse.Locations.Count > 0) then
  begin
    ALocationInfo := nil;
    for ALocationInfo in AResponse.Locations do
    begin
      APushpin := CreatePushpin;
      APushpin.Location.GeoPoint := ALocationInfo.Point;
      APushpin.Hint := ALocationInfo.GetDisplayText;
      FSearchPins.Add(APushpin);
    end;
    dxMapControl1.CenterPoint.GeoPoint := AResponse.Locations[0].Point;
    dxMapControl1.ZoomToGeoRect(ALocationInfo.BoundingBox);
  end;
end;

procedure TBingServicesDemoMainForm.dxMapControl1BingMapMajorRoadRouteDataProvider1Response(
  Sender: TObject; AResponse: TdxBingMapRouteDataServiceResponse;
  var ADestroyResponse: Boolean);
var
  ARoute: TdxBingMapRouteInfo;
  APushpin: TdxMapPushpin;
  ARouteLeg: TdxBingMapRouteLeg;
  ARouteIndex: Integer;
begin
  if CheckResponse(AResponse) then
  begin
    cxListBox1.Clear;
    cxComboBox1.Properties.Items.Clear;
    FreeAndNil(FRouteFromMajorRoadsDataServiceResponse);
    FRouteFromMajorRoadsDataServiceResponse := AResponse;
    for ARouteIndex := 0 to AResponse.Routes.Count - 1 do
    begin
      ARoute := AResponse.Routes[ARouteIndex];
      CreateRouteLine(ARoute);
      cxComboBox1.Properties.Items.AddObject('Route ' + GetPinLetter(ARouteIndex + 1) + '-A', ARoute);
      if ARoute.RouteLegs.Count > 0 then
      begin
        ARouteLeg := ARoute.RouteLegs[0];
        APushpin := CreatePushpin;
        FMajorRoadPins.Add(APushpin);
        APushpin.Location.GeoPoint := ARouteLeg.ActualStart;
        if ARouteLeg.StartLocation <> nil then
          APushpin.Hint := ARouteLeg.StartLocation.GetDisplayText;
      end;
    end;
    CheckWaypointTexts(FMajorRoadPins);
    if cxComboBox1.Properties.Items.Count > 0 then
      cxComboBox1.ItemIndex := 0;
    ADestroyResponse := False;
  end;
end;

procedure TBingServicesDemoMainForm.dxMapControl1BingMapReverseGeoCodingDataProvider1Response(
  Sender: TObject; AResponse: TdxBingMapLocationDataServiceResponse;
  var ADestroyResponse: Boolean);
begin
  if CheckResponse(AResponse) and (AResponse.Locations.Count > 0) then
    FAddedItemAddress := AResponse.Locations[0].GetDisplayText
  else
    FAddedItemAddress := '';
end;

procedure TBingServicesDemoMainForm.dxMapControl1BingMapRouteDataProvider1Response(
  Sender: TObject; AResponse: TdxBingMapRouteDataServiceResponse;
  var ADestroyResponse: Boolean);
var
  ARoute: TdxBingMapRouteInfo;
  ARouteIndex: Integer;
  I: Integer;
begin
  if CheckResponse(AResponse) then
  begin
    cxComboBox1.Properties.Items.Clear;
    cxListBox1.Clear;
    FreeAndNil(FRouteDataServiceResponse);
    FRouteDataServiceResponse := AResponse;
    for ARouteIndex := 0 to AResponse.Routes.Count - 1 do
    begin
      ARoute := AResponse.Routes[ARouteIndex];
      CreateRouteLine(ARoute);
      cxComboBox1.Properties.Items.AddObject('Route ' + IntToStr(ARouteIndex + 1), ARoute);
      for I := 0 to ARoute.RouteLegs.Count - 1 do
        FRoutePins[I].Location.GeoPoint := ARoute.RouteLegs[I].ActualStart;
      FRoutePins.Last.Location.GeoPoint := ARoute.RouteLegs.Last.ActualEnd;
    end;
    if cxComboBox1.Properties.Items.Count > 0 then
      cxComboBox1.ItemIndex := 0;
    ADestroyResponse := False;
  end;
end;

procedure TBingServicesDemoMainForm.dxMapControl1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FCurrentCursorPos := Point(X, Y);
end;

procedure TBingServicesDemoMainForm.dxMapControl1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  miNewPointPointer.Visible := False;
end;

procedure TBingServicesDemoMainForm.FormCreate(Sender: TObject);
var
  I: Integer;
  AMenuItem: TMenuItem;
begin
  (dxMapControl1ImageTileLayer1.Provider as TdxMapControlBingMapImageryDataProvider).BingKey := DXBingKey;
  FMajorRoadPins := TList<TdxMapPushpin>.Create;
  FRoutePins := TList<TdxMapPushpin>.Create;
  FSearchPins := TList<TdxMapPushpin>.Create;
  FRoutes := TList<TdxMapPolyline>.Create;
  dxMapControl1BingMapGeoCodingDataProvider1.BingKey := DXBingKey;
  dxMapControl1BingMapReverseGeoCodingDataProvider1.BingKey := DXBingKey;
  dxMapControl1BingMapRouteDataProvider1.BingKey := DXBingKey;
  dxMapControl1BingMapMajorRoadRouteDataProvider1.BingKey := DXBingKey;

  for I := 0 to High(dxBingTravelMode) do
  begin
    AMenuItem := TMenuItem.Create(miOptions);
    AMenuItem.Caption := dxBingTravelMode[I];
    AMenuItem.RadioItem := True;
    AMenuItem.GroupIndex := 1;
    AMenuItem.AutoCheck := True;
    AMenuItem.Tag := I;
    if I = 0 then
      AMenuItem.Checked := True;
    AMenuItem.OnClick := TravelModeChanged;
    miOptions.Add(AMenuItem);
  end;
  FWndProcLinkedObj := cxWindowProcController.Add(dxMapControl1, MapControlWndProc);
end;

procedure TBingServicesDemoMainForm.FormDestroy(Sender: TObject);
begin
  cxWindowProcController.Remove(FWndProcLinkedObj);
  FreeAndNil(FRouteDataServiceResponse);
  FreeAndNil(FRouteFromMajorRoadsDataServiceResponse);
  FreeAndNil(FRoutes);
  FreeAndNil(FSearchPins);
  FreeAndNil(FRoutePins);
  FreeAndNil(FMajorRoadPins);
end;

function TBingServicesDemoMainForm.GetCurrentCursorGeoPoint: TdxMapControlGeoPoint;
begin
  Result := dxMapControl1ImageTileLayer1.ScreenPointToGeoPoint(dxPointDouble(FCurrentCursorPos));
end;

function TBingServicesDemoMainForm.GetPinLetter(ANumber: Integer): string;
begin
  Result := Chr(Ord('A') + ANumber);
end;

function TBingServicesDemoMainForm.GetTravelDistanceStr(
  ADistanceInKilometers: Double): string;
begin
  Result := Format('%f km', [ADistanceInKilometers]);
end;

function TBingServicesDemoMainForm.GetTravelDurationStr(ATimeInSeconds: Double): string;
var
  ATimeInt: Cardinal;
  AHour: Cardinal;
  AMinute: Word;
begin
  Result := '';
  ATimeInt := Round(ATimeInSeconds);
  AHour := ATimeInt div 3600;
  AMinute := (ATimeInt mod 3600) div 60;
  Result := IfThen(AHour > 0, IntToStr(AHour) + ' h ');
  if AMinute > 0 then
    Result := Result + IntToStr(AMinute) + ' min'
  else
    if AHour = 0 then
      Result := Result + '< 1' + ' min';
end;

procedure TBingServicesDemoMainForm.MapControlWndProc(var Message: TMessage);
var
  AImage: TdxSmartImage;
  AGpImageCanvas: TdxGPCanvas;
begin
  FWndProcLinkedObj.DefaultProc(Message);
  if Message.Msg = WM_PAINT then
  begin
    if dxMapControl1.LookAndFeel.Painter.MapControlPanelBackColor <> FSearchEditBackgroundColor then
    begin
      FSearchEditBackgroundColor := dxMapControl1.LookAndFeel.Painter.MapControlPanelBackColor;
      AImage := TdxSmartImage.CreateSize(cxSize(182, 40));
      AGpImageCanvas := AImage.CreateCanvas;
      try
        AGpImageCanvas.Rectangle(AImage.ClientRect, 0, FSearchEditBackgroundColor);
      finally
        AGpImageCanvas.Free;
      end;
      cxImage1.Picture.Graphic := AImage;
    end
    else
      cxImage1.Invalidate;
  end;
end;

procedure TBingServicesDemoMainForm.CalculateRoute;
var
  ARouteWaypoints: TdxBingMapRouteWaypoints;
  I: Integer;
begin
  FIsMajorRoadMode := False;
  ClearRoutePath;
  if FRoutePins.Count > 1 then
  begin
    ARouteWaypoints := TdxBingMapRouteWaypoints.Create;
    try
      for I := 0 to FRoutePins.Count - 1 do
        ARouteWaypoints.Add(FRoutePins[I].Location.GeoPoint);
      dxMapControl1BingMapRouteDataProvider1.CalculateAsync(ARouteWaypoints);
    finally
      ARouteWaypoints.Free;
    end;
  end;
end;

procedure TBingServicesDemoMainForm.CalculateRouteFromMajorRoads;
var
  AWayPoint: TdxBingMapRouteWaypoint;
begin
  FIsMajorRoadMode := True;
  AWayPoint := TdxBingMapRouteWaypoint.Create(FMajorRoadPins[0].Location.GeoPoint);
  try
    dxMapControl1BingMapMajorRoadRouteDataProvider1.CalculateAsync(AWayPoint);
  finally
    AWayPoint.Free;
  end;
end;

procedure TBingServicesDemoMainForm.ClearMapItems<T>(AItems: TList<T>);
var
  I: Integer;
  AMapItem: T;
begin
  for I := AItems.Count - 1 downto 0 do
  begin
    AMapItem := AItems[I];
    AItems.Delete(I);
    dxMapControl1ItemLayer1.MapItems.Remove(AMapItem);
  end;
end;

function TBingServicesDemoMainForm.CheckResponse(
  AResponse: TdxBingMapResponse): Boolean;
begin
  Result := False;
  if AResponse <> nil then
    if AResponse.IsSuccess then
      Result := True
    else
      if AResponse.ErrorInfo <> nil then
        ShowMessage(AResponse.ErrorInfo.ErrorDetails);
end;

procedure TBingServicesDemoMainForm.CheckWaypointTexts(AItems: TList<TdxMapPushpin>);
var
  I: Integer;
begin
  for I := 0 to AItems.Count - 1 do
    AItems[I].Text := GetPinLetter(I);
end;

procedure TBingServicesDemoMainForm.ClearRoutePath;
begin
  miManeuverPoint.Visible := False;
  ClearMapItems<TdxMapPolyline>(FRoutes);
  cxListBox1.Clear;
  cxComboBox1.Properties.Items.Clear;
  FreeAndNil(FRouteDataServiceResponse);
  FreeAndNil(FRouteFromMajorRoadsDataServiceResponse);
end;

procedure TBingServicesDemoMainForm.ShowFullRoute(const ABoundingBox: TdxMapControlGeoRect);
begin
  dxMapControl1.BeginUpdate;
  miManeuverPoint.Visible := False;
  dxMapControl1.ZoomToGeoRect(ABoundingBox);
  dxMapControl1.ZoomOut;
  dxMapControl1.EndUpdate;
end;

procedure TBingServicesDemoMainForm.StopRouteProviders;
begin
  dxMapControl1BingMapRouteDataProvider1.CancelRequests;
  dxMapControl1BingMapMajorRoadRouteDataProvider1.CancelRequests;
end;

procedure TBingServicesDemoMainForm.TravelModeChanged(ASender: TObject);
var
  AOldTravelMode, ANewTravelMode: TdxBingMapTravelMode;
begin
  AOldTravelMode := dxMapControl1BingMapRouteDataProvider1.TravelMode;
  ANewTravelMode := TdxBingMapTravelMode((ASender as TComponent).Tag);
  if ANewTravelMode <> AOldTravelMode then
  begin
    dxMapControl1BingMapRouteDataProvider1.TravelMode := ANewTravelMode;
    dxMapControl1BingMapRouteDataProvider1.TimeType := bmttDeparture;
    dxMapControl1BingMapRouteDataProvider1.DateTime := Now;
    if FRoutePins.Count > 0 then
    begin
      StopRouteProviders;
      CalculateRoute;
    end;
  end;
end;

procedure TBingServicesDemoMainForm.CreateRouteLine(ARoute: TdxBingMapRouteInfo);
var
  APolyline: TdxMapPolyline;
begin
  APolyline := dxMapControl1ItemLayer1.AddItem(TdxMapPolyline) as TdxMapPolyline;
  APolyline.GeoPoints.AddRange(ARoute.Path);
  APolyline.Style.BorderColor := $9F0000FF;
  APolyline.Style.BorderWidth := 4;
  APolyline.Hint := Format('Distance: %s, Duration: %s', [GetTravelDistanceStr(ARoute.TravelDistance),
    GetTravelDurationStr(ARoute.TravelDuration)]);
  APolyline.Tag := TdxNativeInt(ARoute);
  FRoutes.Add(APolyline);
end;

procedure TBingServicesDemoMainForm.ClearAllPins;
begin
  ClearMapItems<TdxMapPushpin>(FRoutePins);
  ClearMapItems<TdxMapPushpin>(FSearchPins);
  ClearMapItems<TdxMapPushpin>(FMajorRoadPins);
end;

procedure TBingServicesDemoMainForm.ClearAllRouteInfo;
begin
  ClearAllPins;
  ClearRoutePath;
end;

procedure TBingServicesDemoMainForm.PopupMenu1Popup(Sender: TObject);
begin
  if dxMapControl1.HitTest.HitObject is TdxMapPointerViewInfo then
    FHotPushpin := TdxMapPointerViewInfo(dxMapControl1.HitTest.HitObject).Item as TdxMapPushpin
  else
    FHotPushpin := nil;
  miNewPointPointer.Location.GeoPoint := GetCurrentCursorGeoPoint;
  miNewPointPointer.Visible := FHotPushpin = nil;
  actDeletePoint.Enabled := (FHotPushpin <> nil) and (FMajorRoadPins.IndexOf(FHotPushpin) = -1);
  actAddEndPoint.Enabled := (FHotPushpin = nil) and (FRoutePins.Count > 0);
  actAddStartPoint.Visible := (FHotPushpin = nil) and (FRoutePins.Count = 0);
  actChangeStartPoint.Visible := (FHotPushpin = nil) and (FRoutePins.Count > 0);
  actSetAsEndPoint.Visible := (FHotPushpin <> nil) and (FRoutePins.Count > 0) and
   (FRoutePins[FRoutePins.Count - 1] <> FHotPushpin);
  actSetAsStartPoint.Visible := (FHotPushpin <> nil) and
   ((FRoutePins.Count = 0) or (FRoutePins[0] <> FHotPushpin));
end;

end.
